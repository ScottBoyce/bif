!
! CODE DEVELOPED BY SCOTT E BOYCE
!                   CONTACT <seboyce@usgs.gov> or <Boyce@engineer.com>
!
!
!
!   GENERIC_OUTPUT_FILE_INSTRUCTION
!                           DATA TYPE
!                                    GENERIC_OUTPUT_FILE
!                           SUBROUTINES
!                                    FL%OPEN
!                                    FL%SET_HEADER
!                                    FL%SIZE_CHECK --NOTE THIS MAY CHANGE FI%IU --DUE TO COMPILER BUG
!                                    FL%MOVE
!                                    FL%CLOSE
!                                    FL%WRITE
!
MODULE GENERIC_OUTPUT_FILE_INSTRUCTION!, ONLY: GENERIC_OUTPUT_FILE
  !OPENS AN FILE FOR WRITING. PROVIDES GENERIC INTERFACING FOR WRITTING TO FILE. IF OPEN/CLOSE AUOTMATICALLY CLOSES FILES
  !
  USE, INTRINSIC:: ISO_FORTRAN_ENV, ONLY: REAL32, REAL64, REAL128
  !
  USE CONSTANTS
  USE PARSE_WORD_INTERFACE,   ONLY: PARSE_WORD, GET_WORD
  USE NUM2STR_INTERFACE,      ONLY: NUM2STR
  USE POST_KEY_SUB,           ONLY: CHECK_FOR_POST_KEY
  USE ERROR_INTERFACE,        ONLY: FILE_IO_ERROR, STOP_ERROR, GET_WARN, WARNING_MESSAGE
  USE GENERIC_OPEN_INTERFACE, ONLY: GENERIC_OPEN, NULL_FILE
  USE FILE_IO_INTERFACE,      ONLY: DATAFILE_UNIT_NUMBER, GET_FILE_NAME
  USE STRINGS,                ONLY: UPPER, GET_INTEGER
  USE FILE_INCREMENTER_INTERFACE, ONLY: FILE_INCREMENTER
  USE PATH_INTERFACE,         ONLY: GET_CWD
  !
  IMPLICIT NONE
  !
  PRIVATE
  PUBLIC:: GENERIC_OUTPUT_FILE, GENERIC_OUTPUT_FILE_DEPOINT   ! XX_DEPOINT only necessary as workaround for gfortran compiler error
  !
  TYPE GENERIC_OUTPUT_FILE
      INTEGER:: IU          = Z
      INTEGER:: IOUT        = Z
      LOGICAL:: IS_OPEN     = FALSE
      LOGICAL:: OPENCLOSE   = FALSE
      LOGICAL:: ERROR       = FALSE
      LOGICAL:: BINARY      = FALSE
      LOGICAL:: NULL_FILE   = FALSE
      CHARACTER(:),ALLOCATABLE:: FNAME
      CHARACTER(:),ALLOCATABLE:: EXTRA
      CHARACTER(:),ALLOCATABLE:: FMT
      !
      TYPE(FILE_INCREMENTER),ALLOCATABLE:: FI
      !
      CONTAINS
      !
      PROCEDURE, PASS(FL):: OPEN        => OPEN_GENERIC_OUTPUT_FILE !(LN,[LLOC,OUTPUT,INFILE,NOSTOP,REQKEY,IU,BINARY,BUFFER,SPLIT_SIZE,NOBINARY, SPLITMAXCOUNT, NO_INTERNAL, NO_ONLY_UNIT, SAVE_FNAME, KEY,DIM])
      PROCEDURE, PASS(FL):: SET_HEADER  => GENERIC_OUTPUT_SET_HEADER!(HEADER)
      PROCEDURE, PASS(FL):: SIZE_CHECK  => GENERIC_OUTPUT_SIZE_CHECK
      PROCEDURE, PASS(FL):: REWIND      => REWIND_GENERIC_OUTPUT_FILE
      PROCEDURE, PASS(FL):: MOVE        => MOVE_GENERIC_OUTPUT_FILE !(FL_NEW)
      PROCEDURE, PASS(FL):: CLOSE       => CLOSE_GENERIC_OUTPUT_FILE
      PROCEDURE, PASS(FL):: SET_FNAME   => SET_FILE_NAME_GENERIC_OUTPUT_FILE
      GENERIC            :: WRITE       => GENERIC_OUTPUT_FILE_WRITE_SINGLE, GENERIC_OUTPUT_FILE_WRITE_VECTOR, GENERIC_OUTPUT_FILE_WRITE_ARRAY
      !
      PROCEDURE, PASS(FL), PRIVATE:: GENERIC_OUTPUT_FILE_WRITE_SINGLE
      PROCEDURE, PASS(FL), PRIVATE:: GENERIC_OUTPUT_FILE_WRITE_VECTOR
      PROCEDURE, PASS(FL), PRIVATE:: GENERIC_OUTPUT_FILE_WRITE_ARRAY
      !
      FINAL::                       FINAL_CLOSE_GENERIC_OUTPUT_FILE
  END TYPE
  !
  INTERFACE GENERIC_OUTPUT_FILE_DEPOINT
    MODULE PROCEDURE GENERIC_OUTPUT_FILE_DEPOINT_DIM0
    MODULE PROCEDURE GENERIC_OUTPUT_FILE_DEPOINT_DIM1
  END INTERFACE
  !
  CONTAINS
  !
  SUBROUTINE RESET_GENERIC_OUTPUT_FILE(FL)
    CLASS(GENERIC_OUTPUT_FILE), INTENT(INOUT):: FL
    INTEGER:: IERR
    !
    IF(FL%OPENCLOSE) CLOSE(FL%IU, IOSTAT=IERR)
    !
    FL%IU          = Z
    FL%IOUT        = Z
    FL%IS_OPEN     = FALSE
    FL%OPENCLOSE   = FALSE
    FL%ERROR       = FALSE
    FL%BINARY      = FALSE
    FL%NULL_FILE   = FALSE
    !
    IF(ALLOCATED(FL%FNAME)) DEALLOCATE(FL%FNAME)
    IF(ALLOCATED(FL%EXTRA)) DEALLOCATE(FL%EXTRA)
    IF(ALLOCATED(FL%FI   )) DEALLOCATE(FL%FI   )
    !
    IF(ALLOCATED(FL%FMT  )) DEALLOCATE(FL%FMT  )
    !
  END SUBROUTINE
  !
  SUBROUTINE OPEN_GENERIC_OUTPUT_FILE(FL, LN, LLOC, OUTPUT, INFILE, NOSTOP, REQKEY, IU, BINARY, BUFFER, SPLIT_SIZE, NOBINARY, SPLITMAXCOUNT, NO_WARN, NO_INTERNAL, NO_ONLY_UNIT, SAVE_FNAME, KEY, DIM, PREPOST, STREAM_TEXT, NOPOSTKEY, FORCE_DATAFILE, MSG)
    ! ATTEMPTS TO READ KEYWORDS AND CREATE A FILE FOR WRITTING TO. IF IT EXISTS IT IS REPLACED
    ! SETS ERROR=TRUE IF FILE FAILED TO IDENTIFY OR OPEN A FILE/UNIT.
    !
    ! THE ORDER THAT THE FILE IS ATTEMPTED TO BE IDENTIFIED IS:
    ! 1) READ SINGLE UNIT NUMBER
    ! 2) CHECK FOR OPTIONAL KEYWORD "BINARY" --ADDED TO OPEN FILE
    ! 4) CHECK FOR KEYWORD "INTERNAL", "LIST", OR THE REST OF HTE LINE IS BLANK TO INDICATE THAT OUTPUT IS WRITTEN TO LIST FILE
    ! 5) CHECK FOR KEYWORD EXTERNAL FOLLOWED BY UNIT NUMBER
    ! 6) CHECK FOR KEYWORD OPEN/CLOSE AND THEN OPENS FILE SPECIFIED
    ! 7) CHEKS IF THE LINE CONTAINS A FILE THAT CAN BE OPENED (SAME AS IF OPEN/CLOSE WAS NOT PRESENT)
    !
    ! LLOC   is the starting location of the line to look for KEYWORD/NAME
    ! LN     is the line to process the KEYWORDS/UNIT/NAME
    ! OUTPUT is where to write error messages too
    ! INFILE is the input file that LN originated from
    ! NOSTOP optional, when present and is true will prevent the program from stopping if the file fails to open. ERROR will be set to TRUE
    ! REQKEY optional, when present and is true indicates that a keyword is required to open file (viz. no reading a single number or just a file name)
    ! IU     optional, when present is the unit number used when a file is opened by OPEN/CLOSE or by NAME
    ! BINARY optional, when present sets file to be opened as a binary file or formatted.
    ! BUFFER optional, when present sets the buffer size in KB. --131072 = 128KB is the default and 1048576 = 1MB  --BUFFER USES TWO THREADS SO ACTUAL BUFFER IS TWICE THE VALUE (eg. 256KB)
    ! SPLIT_SIZE  optional, when present turns on the bility to split the file every SPLIT_SIZE megabytes into a new file
    ! NOBINARY    optional, when present and set to true idicates an error is raised when file is opened as a binary file.
    ! 
    CLASS(GENERIC_OUTPUT_FILE), INTENT(INOUT):: FL
    CHARACTER(*),               INTENT(IN   ):: LN
    INTEGER,      OPTIONAL,     INTENT(INOUT):: LLOC
    INTEGER,      OPTIONAL,     INTENT(IN   ):: OUTPUT, INFILE
    LOGICAL,      OPTIONAL,     INTENT(IN   ):: NOSTOP
    LOGICAL,      OPTIONAL,     INTENT(IN   ):: REQKEY
    INTEGER,      OPTIONAL,     INTENT(IN   ):: IU
    LOGICAL,      OPTIONAL,     INTENT(IN   ):: BINARY
    INTEGER,      OPTIONAL,     INTENT(IN   ):: BUFFER
    INTEGER,      OPTIONAL,     INTENT(IN   ):: SPLIT_SIZE
    LOGICAL,      OPTIONAL,     INTENT(IN   ):: NOBINARY
    INTEGER,      OPTIONAL,     INTENT(IN   ):: SPLITMAXCOUNT
    LOGICAL,      OPTIONAL,     INTENT(IN   ):: NO_WARN         ! If true, warnings are supressed
    LOGICAL,      OPTIONAL,     INTENT(IN   ):: NO_INTERNAL
    LOGICAL,      OPTIONAL,     INTENT(IN   ):: NO_ONLY_UNIT
    LOGICAL,      OPTIONAL,     INTENT(IN   ):: SAVE_FNAME
    CHARACTER(*), OPTIONAL,     INTENT(  OUT):: KEY               ! SHOULD BE CHARACTER(10)
    INTEGER,      OPTIONAL,     INTENT(INOUT):: DIM
    INTEGER,      OPTIONAL,     INTENT(  OUT):: PREPOST           ! Holds location before running post-keycheck
    LOGICAL,      OPTIONAL,     INTENT(IN   ):: STREAM_TEXT
    LOGICAL,      OPTIONAL,     INTENT(IN   ):: NOPOSTKEY
    LOGICAL,      OPTIONAL,     INTENT(IN   ):: FORCE_DATAFILE
    CHARACTER(*), OPTIONAL,     INTENT(IN   ):: MSG
    LOGICAL:: ISOPEN, ALLOW_ERROR, NOREQKEY, FOUND_KEY, DATAFILE
    LOGICAL:: GO_TO_TOP, SAVE_FN, CHECK_POST, CHECK_ONLY_UNIT, NOT_UNIQUE
    !
    CHARACTER(12):: EXT, FORM_CHK, ACCESS_TXT
    CHARACTER(:), ALLOCATABLE:: FNAME, ERR_MSG
    !
    INTEGER:: I, IIN, LL, ISTART, ISTOP, LLOC_BAK
    INTEGER:: IERR, BUF, SPLIT, IU_READ
    INTEGER:: MXCNT
    !
    CALL RESET_GENERIC_OUTPUT_FILE(FL)
    !
    IERR         = Z
    IU_READ      = Z
    SPLIT        = Z
    DATAFILE     = FALSE
    FOUND_KEY    = FALSE
    NOT_UNIQUE   = FALSE
    GO_TO_TOP    = FALSE
    IF(ALLOCATED(ERR_MSG)) DEALLOCATE(ERR_MSG)
    IF(ALLOCATED(FNAME  )) DEALLOCATE(FNAME)
    !
    IF(PRESENT(LLOC)) THEN
        LL = LLOC
    ELSE
        LL = ONE
    END IF
    LLOC_BAK  = LL
    !
    IF(PRESENT(OUTPUT)) THEN
        FL%IOUT = OUTPUT
    ELSE
        FL%IOUT = Z
    END IF
    !
    IF(PRESENT(INFILE)) THEN
        IIN = INFILE
    ELSE
        IIN = Z
    END IF
    !
    IF(PRESENT(REQKEY)) THEN
        NOREQKEY = .NOT. REQKEY
    ELSE
        NOREQKEY = TRUE
    END IF
    !
    IF(PRESENT(NOSTOP)) THEN
        ALLOW_ERROR = .NOT. NOSTOP
    ELSE
        ALLOW_ERROR = TRUE
    END IF
    !
    IF(PRESENT(NOPOSTKEY)) THEN
        CHECK_POST = .NOT. NOPOSTKEY
    ELSE
        CHECK_POST = TRUE
    END IF
    !
    IF(PRESENT(NO_ONLY_UNIT)) THEN
        CHECK_ONLY_UNIT = .not. NO_ONLY_UNIT
    ELSE
        CHECK_ONLY_UNIT = TRUE
    END IF
    !
    IF(PRESENT(SAVE_FNAME)) THEN
        SAVE_FN = SAVE_FNAME
    ELSE
        SAVE_FN = FALSE
    END IF
    !
    ISOPEN = FALSE
    IF(PRESENT(STREAM_TEXT)) ISOPEN = STREAM_TEXT
    IF(ISOPEN) THEN
        ACCESS_TXT = 'STREAM'
    ELSE
        ACCESS_TXT = 'SEQUENTIAL'
    END IF
    !
    IF(PRESENT(BINARY)) FL%BINARY = BINARY
    !
    BUF = 16384 != 16KB x2 = 32KB  --1048576 = 1MB   --NOTE THAT TWO THREADS ARE USED PER BUFFER SO ACTUAL SPACE IS TWICE AS BIG
    IF(PRESENT(BUFFER)) BUF = BUFFER
    !
    MXCNT = 10
    IF(PRESENT(SPLITMAXCOUNT)) MXCNT = SPLITMAXCOUNT
    !
    IF(PRESENT(SPLIT_SIZE)) SPLIT = SPLIT_SIZE
    !
    ISOPEN = FALSE
    IF(PRESENT(IU)) THEN
        IF(IU /= Z) THEN
                        INQUIRE(IU, OPENED=ISOPEN)
                        IF(ISOPEN) FL%IU = IU
        END IF
    END IF
    !
    IF(.NOT. ISOPEN) THEN
            I = LL
            CALL GET_WORD(LN,LL,ISTART,ISTOP,EXT)
            !
            IF (ISTART > ISTOP) THEN
                IERR     = ONE
                FL%ERROR = TRUE
                CALL ADD_MSG(ERR_MSG, &
                        'Failed to successfully parse the line to find a directive keyword or file to open.'//NL// &
                        'The line may be empty, or is empty at the starting point of parsing.'//NL// &
                        'The line passed to the routine is:'//NL// &
                        '"'//TRIM(LN)//'"'//NL// &
                        'And the portion of the line that is being parsed is:'//NL// &
                        '"'//TRIM(LN(I:))//'"')
            ELSEIF (NOREQKEY .AND. CHECK_ONLY_UNIT .AND. LN(ISTART:ISTOP) /= BLNK) THEN
                              READ(LN(ISTART:ISTOP),*,IOSTAT=IERR) IU_READ
            ELSE
                IERR=69
            END IF
            !
            IF (IERR .EQ. Z) THEN
                !
                FL%IU = IU_READ 
                IF(CHECK_POST) CALL CHECK_FOR_POST_KEY(LL, LN, IIN, FL%IOUT, BUF, SPLIT, DIM=DIM, FMT=FL%FMT, OLDLOC=PREPOST, NO_WARN=NO_WARN, MSG=MSG)
                !
                IF (FL%IU == Z) THEN
                    CALL CLOSE_GENERIC_OUTPUT_FILE(FL)
                    CALL WARNING_MESSAGE(LN,IIN,FL%IOUT,MSG='GENERIC_OUTPUT_FILE_INSTRUCTION: SUCESSFULLY LOADED A UNIT NUMBER ON LINE,'//NL//'BUT THE UNIT NUMBER WAS ZERO, SO IT IS ASSUMED THAT NO OUTPUT IS MEANT TO BE WRITTEN.')
                    IF(PRESENT(KEY)) KEY = 'NOKEY'
                    IF(SAVE_FN) FL%FNAME = 'ERROR - ¿¿¿UNKOWN FILE???'
                    RETURN
                END IF
            ELSE
                IERR= Z
                !
                EXT = LN(ISTART:ISTOP)
                CALL UPPER(EXT)
                !
                IF(EXT == 'BINARY') THEN
                                        FL%BINARY = TRUE
                                        CALL PARSE_WORD(LN,LL,ISTART,ISTOP)
                                        EXT = LN(ISTART:ISTOP)
                                        CALL UPPER(EXT)
                END IF
                !
                IF    (EXT == 'NOPRINT' .OR. EXT == 'SKIP' .OR. EXT == 'NUL' .OR. EXT == 'NULL') THEN
                                                 IF(CHECK_POST) CALL CHECK_FOR_POST_KEY(LL, LN, IIN, FL%IOUT, BUF, SPLIT, DIM=DIM, FMT=FL%FMT, OLDLOC=PREPOST, NO_WARN=NO_WARN, MSG=MSG)
                                                 CALL CLOSE_GENERIC_OUTPUT_FILE(FL)
                                                 FL%IU = NULL_FILE%GET(LN,IIN,FL%IOUT)
                                                 FL%NULL_FILE = TRUE
                                                 IF(PRESENT(KEY)) KEY = ADJUSTL(EXT)
                                                 IF(SAVE_FN) FL%FNAME = TRIM(EXT)
                                                 RETURN
                ELSEIF(EXT == 'INTERNAL'.OR. EXT == 'LIST' .OR. EXT==BLNK) THEN
                                                 FL%IU = FL%IOUT
                                                 IF(EXT /= BLNK) FOUND_KEY=TRUE
                ELSEIF(EXT == 'EXTERNAL' .OR. EXT=='DATAUNIT') THEN
                                                 CALL GET_INTEGER(LN,LL,ISTART,ISTOP,FL%IOUT,IIN,FL%IU,HAS_ERROR=FL%ERROR)
                                                 !
                                                 IF(FL%ERROR) THEN
                                                     FNAME = LN(ISTART:ISTOP)
                                                     CALL DATAFILE_UNIT_NUMBER%CHECK_BASE(FNAME,FL%IU,NOT_UNIQUE)
                                                     FL%ERROR = FL%IU /= Z .AND. NOT_UNIQUE                 !Found IU and its basename is Unique
                                                 END IF
                                                 !CALL GET_INTEGER(LN,LL,ISTART,ISTOP,FL%IOUT,IIN,FL%IU,MSG='GENERIC_OUTPUT_FILE_INSTRUCTION ERROR: FROUND KEYWORD "'//TRIM(EXT)//'" WHICH SHOULD BE FOLLOWED BY AN INTEGER REPRESENTING THE UNIT NUMBER TO USE.')
                                                 IF(.NOT. FL%ERROR .AND. CHECK_POST) CALL CHECK_FOR_POST_KEY(LL, LN, IIN, FL%IOUT, BUF, SPLIT, DIM=DIM, FMT=FL%FMT, OLDLOC=PREPOST, NO_WARN=NO_WARN, MSG=MSG)
                                                 FOUND_KEY=TRUE
                                                 !
                                                 IF(FL%ERROR) THEN
                                                     FL%IU = Z
                                                     CALL ADD_MSG(ERR_MSG, &
                                                             'Found '//TRIM(EXT)//' directive, but failed to read the unit number or file base name after the directive.'//NL// &
                                                             'The following is what found: "'//FNAME//'"'//BLN//                                                                &
                                                             'Typically, the external unit input is defined as:'//NL//                                                          &
                                                             ' EXTERNAL  56        # for a file that is already opened using unit 56'//NL//                                     &
                                                             ' EXTERNAL  base.txt  # for a file that is already opened and located at some/path/to/base.txt'//BLN//             &
                                                             ' DATAUNIT  56        # for a file that is already opened using unit 56'//NL//                                     &
                                                             ' DATAUNIT  base.txt  # for a file that is already opened and located at some/path/to/base.txt'//BLN//             &
                                                             'Note a base name must match exactly to one file that is already opened and the match is case sensitive.'//BLN//   &
                                                             'The following are a list of unit numbers that were registered with the program'//NL//                             &
                                                             '  and can be used to look up by basename.'//NL//                                                                  &
                                                             'Other unit numbers may be opened, but these are what was registered '//NL//                                       &
                                                             '  and can be looked with a base name.'//BLN//DATAFILE_UNIT_NUMBER%PRINT_STR())
                                                 END IF
                                                 IF(ALLOCATED(FNAME)) DEALLOCATE(FNAME)
                ELSE
                      IF (EXT == 'OPEN/CLOSE' .OR. EXT=='DATAFILE') THEN  ! OPEN/CLOSE KEYWORD
                                                 DATAFILE = EXT=='DATAFILE'
                                                 CALL PARSE_WORD(LN,LL,ISTART,ISTOP)   !MOVE TO NEXT WORD WHICH IS THE FILE NAME
                                                 FL%OPENCLOSE = TRUE
                                                 FOUND_KEY    = TRUE
                      ELSEIF(NOREQKEY) THEN
                                                 FL%OPENCLOSE = TRUE
                      END IF
                      !
                      IF(PRESENT(FORCE_DATAFILE) .AND. FL%OPENCLOSE .AND. .NOT. DATAFILE)  DATAFILE = FORCE_DATAFILE
                      !
                      ALLOCATE( FNAME, SOURCE = LN(ISTART:ISTOP) )
                      !
                      IF(CHECK_POST) CALL CHECK_FOR_POST_KEY(LL, LN, IIN, FL%IOUT, BUF, SPLIT, BINARY=FL%BINARY, DIM=DIM, FMT=FL%FMT, OLDLOC=PREPOST, NO_WARN=NO_WARN, MSG=MSG)  !FL%BINARY IS ONLY SET TO TRUE IF BINARY FLAG FOUND, OTHERWISE IGNORED
                      !
                      INQUIRE(FILE=FNAME, NUMBER=I, OPENED=ISOPEN)
                      !
                      IF(ISOPEN .AND. PRESENT(IU)) THEN
                                     IF (I /= IU) ISOPEN = FALSE
                      END IF
                      !
                      ! OVERKILL CHECK
                      !IF( DATAFILE .AND. .NOT.ISOPEN .AND. .NOT. PRESENT(IU)) CALL DATAFILE_UNIT_NUMBER%CHECK_NAME(FNAME,I,ISOPEN)
                      !
                      IF( DATAFILE .AND. ISOPEN) THEN
                                                 FL%IU = I
                                                 FL%OPENCLOSE = FALSE
                      ELSEIF(FL%OPENCLOSE) THEN
                                                 INQUIRE(FILE=FNAME,NUMBER=FL%IU,OPENED=ISOPEN)
                                                 IF(ISOPEN) THEN
                                                     CALL WARNING_MESSAGE(LN,IIN,FL%IOUT,MSG='GENERIC_OUTPUT FILE OPEN: OPEN/CLOSE FILE WITH FILENAME:'//NL//TRIM(FNAME)//NL//'HAS ALREADY BEEN OPENED/ASSOCIATED WITH A FORTRAN UNIT NUMBER.'//NL//'OUTPUT TO THIS FILE MAY CONTAIN INFORMATION FROM MULTIPLE SOURCES.'//NL//'(THIS IS JUST A WARNING AS YOU MAY BE INTENTIONALY DOING THIS.)')
                                                     FL%OPENCLOSE = FALSE
                                                 ELSE
                                                     FL%IU = Z
                                                     IF(PRESENT(IU)) FL%IU = IU
                                                     !
                                                     IF(FL%BINARY) THEN
                                                          CALL GENERIC_OPEN(FNAME, FL%IU, FL%IOUT, ACTION='WRITE', FORM='UNFORMATTED', ACCESS='STREAM',   STATUS='REPLACE', ASYNC='NO', BUFFER_BLOCKSIZE=BUF, BUFFER_COUNT=2, LINE=LN, INFILE=IIN, ERROR=FL%ERROR)
                                                     ELSE
                                                          CALL GENERIC_OPEN(FNAME, FL%IU, FL%IOUT, ACTION='WRITE', FORM=  'FORMATTED', ACCESS=ACCESS_TXT, STATUS='REPLACE', ASYNC='NO', BUFFER_BLOCKSIZE=BUF, BUFFER_COUNT=2, LINE=LN, INFILE=IIN, ERROR=FL%ERROR)
                                                     END IF
                                                     !
                                                     IF     (FL%ERROR) THEN
                                                                           FL%OPENCLOSE = FALSE
                                                     ELSEIF (DATAFILE) THEN
                                                                           FL%OPENCLOSE = FALSE
                                                                           CALL DATAFILE_UNIT_NUMBER%ADD(FL%IU)
                                                     END IF
                                                 END IF
                      ELSE
                                                 FL%ERROR = TRUE  !DID NOT FIND OPEN/CLOSE KEYWORD AND REQKEY=TRUE
                      END IF
                END IF
            END IF
    END IF
    !
    IF(.NOT. FL%ERROR) THEN
                           IF( FL%IU /= Z) THEN
                                                 INQUIRE(FL%IU,FORM=FORM_CHK, OPENED=ISOPEN) 
                                                 FL%BINARY = FORM_CHK /= 'FORMATTED'
                           ELSE
                                                 ISOPEN = FALSE
                           END IF
                           !
                           IF(.NOT. ISOPEN) FL%ERROR =TRUE !FILE IS NOT INTERNAL, NOR UNIT WAS ASSOCIATED WITH A FILE, NOR WAS IT SUCCESFULLY OPENED.
    END IF
    !
    IF(SPLIT>Z) THEN
                    ALLOCATE(FL%FI)
                    CALL FL%FI%INIT(SPLIT, FL%IU, BUF, MAXCOUNT=MXCNT)
    END IF
    !
    IF(PRESENT(NOBINARY) .AND. FL%BINARY) THEN
        IF(NOBINARY) THEN
            ERR_MSG = 'GENERIC_OUTPUT_FILE_INSTRUCTION ERROR: CONNECTED FILE IS SELECTED FOR BINARY OUTPUT, BUT ONLY FORMATTED OUTPUT IS ALLOWED. PLEASE REMOVE "BINARY" KEYWORD OR MAKE SURE TO USE IN THE NAME FILE THE KEYWORD "DATA" INSTEAD OF DATA(BINARY) FOR THIS FILE.'
            IF(PRESENT(MSG))  ERR_MSG = ERR_MSG//BLN//'THE FOLLOWING MESSAGE WAS PASSED TO GENERIC_OUTPUT:'//BLN//MSG
            CALL FILE_IO_ERROR(Z,IIN,LINE=LN,OUTPUT=FL%IOUT,MSG=ERR_MSG)
        END IF
    END IF
    !
    IF(PRESENT(NO_INTERNAL) .AND. (FL%IU == Z .OR. FL%IU == FL%IOUT).AND. .NOT. FL%ERROR) THEN
            !
            IF(NO_INTERNAL) FL%ERROR = TRUE
            !
            IF(NO_INTERNAL .AND. ALLOW_ERROR) THEN
                  !
                  IF(PRESENT(MSG))  THEN
                                    ERR_MSG = BLN//'THE FOLLOWING MESSAGE WAS PASSED TO GENERIC_OUTPUT:'//BLN//MSG
                  ELSE
                                    ERR_MSG = ""
                  END IF
                  IF( EXT=='INTERNAL') CALL FILE_IO_ERROR(Z,IIN,LINE=LN,OUTPUT=FL%IOUT,MSG='GENERIC_INPUT_FILE ERROR: FOUND KEYWORD "INTERNAL" BUT THIS OUTPUT DATA ITEM DOES NOT ALLOW FOR INTERNAL KEYWORD.'//BLN//'PLEASE WRITE OUTPUT TO SEPARATE FILE AND USE EITHER OPEN/CLOSE, DATAFILE, DATAUNIT, OR EXTERNAL.'//ERR_MSG)
                  IF( EXT=='LIST')     CALL FILE_IO_ERROR(Z,IIN,LINE=LN,OUTPUT=FL%IOUT,MSG='GENERIC_INPUT_FILE ERROR: FOUND KEYWORD "LIST" BUT THIS OUTPUT DATA ITEM DOES NOT ALLOW FOR INTERNAL KEYWORD.'//BLN//'PLEASE WRITE OUTPUT TO SEPARATE FILE AND USE EITHER OPEN/CLOSE, DATAFILE, DATAUNIT, OR EXTERNAL.'//ERR_MSG)
                  IF( FL%IU == Z .OR. EXT==BLNK) CALL FILE_IO_ERROR(Z,IIN,LINE=LN,OUTPUT=FL%IOUT,MSG='GENERIC_INPUT_FILE ERROR: THIS OUTPUT DATA ITEM DOES NOT ALLOW WRITING TO LIST FILE (NO KEYWORD FOUND, SO ASSUMING YOU MEANT THE LIST FILE).'//NL//'PLEASE WRITE OUTPUT TO SEPARATE FILE AND USE EITHER OPEN/CLOSE, DATAFILE, DATAUNIT, OR EXTERNAL.'//ERR_MSG)
            END IF
    END IF
    !
    IF (FL%ERROR .AND. ALLOW_ERROR) THEN
        IF(EXT == 'INTERNAL'.OR. EXT == 'LIST' .OR. EXT==BLNK) THEN
            IF(EXT == 'INTERNAL') THEN
                ERR_MSG = NL//'FOUND KEYWORD "INTERNAL",'
            ELSEIF( EXT == 'LIST') THEN
                ERR_MSG = NL//'FOUND KEYWORD "LIST",'
            ELSE
                ERR_MSG = NL//'NO KEYWORD FOUND, ASSUMING THAT OUTPUT IS WRITTEN TO LIST.'
            END IF
            ERR_MSG = ERR_MSG//NL//'HOWEVER THE LIST FILE UNIT WAS NOT PASSED TO SUBROUTINE. THIS INDICATES THAT LIST WRITING IS NOT SUPPORTED FOR THIS OneWater OUTPUT OPTION.'//NL//'PLEASE CHOSE A DIFFERENT OUTPUT LOCATION (eg OPEN/CLOSE OR EXTERNAL).'
        ELSEIF( ALLOCATED(FNAME) .AND. EXT /= 'EXTERNAL' .AND. EXT /= 'DATAUNIT' ) THEN
            IF(EXT == 'OPEN/CLOSE') THEN
                ERR_MSG = NL//'FOUND KEYWORD "OPEN/CLOSE",'
            ELSEIF( EXT == 'DATAFILE') THEN
                ERR_MSG = NL//'FOUND KEYWORD "DATAFILE",'
            ELSE
                ERR_MSG = NL//'NO KEYWORD FOUND, ASSUMING FILE NAME IS JUST SPECIFIED ON CURRENT LINE,'
            END IF
            ERR_MSG = ERR_MSG//NL//'BUT FAILED TO OPEN THE FOLLOWING FILE FOR WRITING:'//BLN//'"'//FNAME//'"'//BLN// &
                                   'PLEASE CHECK TO SEE IF THE PATH AND FILE NAME CORRECT.'//BLN//                   &
                                   ' ***NOTE THAT THE "/" WORKS FOR BOTH WINDOWS AND LINUX,' //NL//                  &
                                   '    BUT THE "\" ONLY WORKS ON WINDOWS.'
        ELSE
            IF(EXT == 'EXTERNAL') THEN
                ERR_MSG = NL//'FOUND KEYWORD "EXTERNAL",'
            ELSEIF( EXT == 'DATAUNIT') THEN
                ERR_MSG = NL//'FOUND KEYWORD "DATAUNIT",'
            ELSE
                ERR_MSG = NL//'NO KEYWORD FOUND AND A UNIT NUMBER WAS SUCCESSFULLY READ,'//NL//'SO IT WAS ASSUMED TO BE A UNIT SPECIFIED IN THE NAME FILE OR ALREADY OPEN,'
            END IF
            ERR_MSG = ERR_MSG//BLN//'BUT UNIT NUMBER WAS NOT ASSOCIATED WITH ANY FILE (NOT OPEN) FOR WRITING.'//NL// &
                                    'THIS PROBABLY IS BECAUSE IT WAS NOT SPECIFIED IN THE NAME FILE,'//NL//          &
                                    'FILE WAS CLOSED AT SOME POINT,'//NL//                                           &
                                    'OR EVEN NEVER SUCESSFULLY OPENED.'//BLN
            IF( ALLOCATED(FNAME) ) THEN 
                ERR_MSG = ERR_MSG//'THE FOLLOWING IS THE UNIT NUMBER OR BASENAME OF A FILE THAT WAS SEARCHED FOR: "'//FNAME//'"'//BLN
                !
                IF(NOT_UNIQUE) ERR_MSG = ERR_MSG//'-> BUT THE BASENAME WAS NOT UNIQUE WITHIN THE LIST OF CURRENTLY OPENED FILES.'//BLN
                !
                ERR_MSG = ERR_MSG//                                                                                      &
                                   'NOTE THAT A BASENAME MUST MATCH EXACTLY TO ONE FILE THAT IS ALREADY OPENED.'//NL//   &
                                   '                                             --That is, its Case Sensitive.'//NL//   &
                                   'FOR EXAMPLE, IF THE NAME FILE HAS:'//BLN//                                           &
                                   'DATA   23    ./Dir1/Dir2/MyFile.txt'//BLN//                                          &
                                   'THEN ONLY ONE OF THE FOLLOWING WILL FIND THE FILE:'//BLN//                           &
                                   TRIM(EXT)//' 23'//NL//                                                                &
                                   TRIM(EXT)//' MyFile.txt'//BLN
            ELSE
                ERR_MSG = ERR_MSG//'THE FOLLOWING IS THE UNIT NUMBER SEARCHED FOR: "'//NUM2STR(FL%IU)//'"'//BLN
            END IF
            !
            ERR_MSG = ERR_MSG//'PLEASE CHECK TO SEE IF UNIT IS SPECIFIED IN THE NAME FILE WITH DATA OR DATA(BINARY) KEYWORDS.'//BLN// &
                               'SEE ABOVE--IN THE LIST/WARN FILE--FOR A TRANSCRIPT OF ALL THE CURRENTLY OPEN DATAFILES AND THEIR UNIT NUMBERS.'//BLN
            !
            CALL DATAFILE_UNIT_NUMBER%PRINT(FL%IOUT)
            CALL DATAFILE_UNIT_NUMBER%PRINT(GET_WARN())
            !    
        END IF
        !
        ERR_MSG = 'FAILED TO OPEN FILE WITH GENERIC_OUTPUT_FILE_INSTRUCTION.'//BLN//ERR_MSG
        !
        IF(PRESENT(MSG))  ERR_MSG = ERR_MSG//BLN//'THE FOLLOWING MESSAGE WAS PASSED TO GENERIC_OUTPUT:'//BLN//MSG
        !
        CALL FILE_IO_ERROR(IERR,IIN,LINE=LN,OUTPUT=FL%IOUT,MSG=ERR_MSG)
    END IF
    IF (FL%ERROR) FL%IU = Z
    !
    IF(PRESENT(KEY)) THEN
          IF(FOUND_KEY) THEN
                             KEY = ADJUSTL(EXT)
          ELSE
                             KEY = 'NOKEY'
          END IF
    END IF
    !
    IF(PRESENT(LLOC)) LLOC = LL
    !
    IF(FL%IU /= Z) FL%IS_OPEN = TRUE
    !
    IF(SAVE_FN) THEN
       IF(ALLOCATED(FNAME)) THEN
                 IF(FNAME == BLNK) DEALLOCATE(FNAME)
       END IF
       !
       IF(ALLOCATED(FNAME)) THEN
                 FL%FNAME = FNAME
       ELSEIF(FL%IS_OPEN) THEN
                 CALL SET_FILE_NAME_GENERIC_OUTPUT_FILE(FL)
       ELSE
                 FL%FNAME = 'ERROR - ¿¿¿UNKOWN FILE???'
       END IF
    END IF
    !
    CONTAINS
       !
       PURE SUBROUTINE ADD_MSG(TXT, MSG)
         CHARACTER(:), ALLOCATABLE, INTENT(INOUT):: TXT
         CHARACTER(*),              INTENT(IN   ):: MSG
         IF(ALLOCATED(TXT)) THEN
                      TXT = TXT//BLN//MSG
         ELSE
                      TXT = MSG
         END IF
       END SUBROUTINE
       !
       PURE SUBROUTINE HED_MSG(TXT, MSG)
         CHARACTER(:), ALLOCATABLE, INTENT(INOUT):: TXT
         CHARACTER(*),              INTENT(IN   ):: MSG
         IF(ALLOCATED(TXT)) THEN
                      TXT = MSG//BLN//TXT
         ELSE
                      TXT = MSG
         END IF
       END SUBROUTINE
       !
  END SUBROUTINE
  !
  SUBROUTINE GENERIC_OUTPUT_SET_HEADER(FL, HEADER)
    CLASS(GENERIC_OUTPUT_FILE), INTENT(INOUT):: FL
    CHARACTER(*),               INTENT(IN   ):: HEADER
    !
    IF(FL%IS_OPEN) THEN
        IF(FL%BINARY) THEN
            WRITE(FL%IU) HEADER
        ELSE
            WRITE(FL%IU,'(A)') HEADER
        END IF
    END IF
    !
    IF(ALLOCATED(FL%FI)) CALL FL%FI%SET_HEADER(HEADER, TRUE)
    !
  END SUBROUTINE
  !
  SUBROUTINE GENERIC_OUTPUT_SIZE_CHECK(FL,HEADER)
    CLASS(GENERIC_OUTPUT_FILE), INTENT(INOUT):: FL
    CHARACTER(*), OPTIONAL,     INTENT(IN   ):: HEADER
    !
    IF(ALLOCATED(FL%FI)) THEN
        IF(PRESENT(HEADER)) THEN
            CALL FL%FI%SIZE_CHECK(HEADER, TRUE)
        ELSE
            CALL FL%FI%SIZE_CHECK()
        END IF
        IF(FL%IU /= FL%FI%IU) FL%IU=FL%FI%IU
    END IF
    !
  END SUBROUTINE
  !
  PURE SUBROUTINE MOVE_GENERIC_OUTPUT_FILE(FL, FL_NEW)
    CLASS(GENERIC_OUTPUT_FILE),  INTENT(INOUT):: FL
    CLASS(GENERIC_OUTPUT_FILE),  INTENT(INOUT):: FL_NEW
    !
    IF( ALLOCATED(FL%FI)) CALL MOVE_ALLOC(FL%FI, FL_NEW%FI)
    FL_NEW%IU         =  FL%IU
    FL_NEW%OPENCLOSE  =  FL%OPENCLOSE
    FL_NEW%ERROR      =  FL%ERROR
    FL_NEW%IOUT       =  FL%IOUT
    FL_NEW%BINARY     =  FL%BINARY
    FL_NEW%IS_OPEN    =  FL%IS_OPEN
    !
    FL%OPENCLOSE  =FALSE
    FL%IS_OPEN    =FALSE
    !
    IF(ALLOCATED(FL%FNAME)) THEN
        CALL MOVE_ALLOC(FL%FNAME,FL_NEW%FNAME)
    END IF
    !
    IF(ALLOCATED(FL%EXTRA)) THEN
        CALL MOVE_ALLOC(FL%EXTRA,FL_NEW%EXTRA)
    END IF
    !
  END SUBROUTINE
  !
  IMPURE ELEMENTAL SUBROUTINE REWIND_GENERIC_OUTPUT_FILE(FL)
    CLASS(GENERIC_OUTPUT_FILE), INTENT(INOUT):: FL
    !
    IF(FL%IU /= Z) REWIND(FL%IU)
    !
  END SUBROUTINE
    !
    !    CLASS(GENERIC_OUTPUT_FILE), INTENT(INOUT):: FL
    !    INTEGER,                    INTENT(INOUT):: LLOC
    !    CHARACTER(*),               INTENT(IN   ):: LN
    !    INTEGER,                    INTENT(IN   ):: IOUT, IN
    !    INTEGER, OPTIONAL,          INTENT(IN   ):: IU
    !    LOGICAL, OPTIONAL,          INTENT(IN   ):: BINARY
    !    INTEGER, OPTIONAL,          INTENT(IN   ):: BUFFER
    !    CHARACTER(8):: EXT
    !    INTEGER:: I,Z,ISTART,ISTOP,IERR, BUF
    !    REAL:: R
    !    !
    !    Z = 0
    !    FL%IN  = IN
    !    FL%IOUT= IOUT
    !    FL%BINARY = FALSE
    !    !
    !    IF(FL%OPENCLOSE) CLOSE(FL%IU)
    !    FL%OPENCLOSE = FALSE
    !    !
    !    FL%IU = Z
    !    IF(PRESENT(IU)) FL%IU = IU
    !    !
    !    IF(PRESENT(BINARY)) FL%BINARY = BINARY
    !    BUF = 131072 != 128KB  --1048576 = 1MB   --NOTE THAT TWO THREADS ARE USED PER BUFFER SO ACTUAL SPACE IS TWICE AS BIG
    !    IF(PRESENT(BUFFER)) BUF = BUFFER
    !    !
    !    IF (FL%IU == Z) THEN
    !       CALL URWORD(LN,LLOC,ISTART,ISTOP,Z,I,R,Z,Z)
    !       READ(LN(ISTART:ISTOP),*,IOSTAT=IERR) FL%IU
    !       !
    !       IF (IERR /= Z) THEN
    !             !
    !             EXT = LN(ISTART:ISTOP)
    !             CALL UPPER(EXT)
    !             !
    !             IF    (EXT == 'EXTERNAL') THEN
    !                   CALL URWORD(LN,LLOC,ISTART,ISTOP,2,FL%IU,R,FL%IOUT,FL%IN)
    !             ELSEIF(EXT == 'LIST' .OR. EXT=='') THEN
    !                   FL%IU = IOUT
    !             ELSE
    !                   IF(EXT == 'OPEN/CLO' ) CALL URWORD(LN,LLOC,ISTART,ISTOP,Z,I,R,Z,Z)   !MOVE TO NEXT WORD WHICH IS THE FILE NAME
    !                   !
    !                   FL%OPENCLOSE = TRUE
    !                   FL%IU = Z
    !                   !
    !                   IF(FL%BINARY) THEN
    !                        CALL GENERIC_OPEN(LN(ISTART:ISTOP), FL%IU, FL%IOUT, ACTION='WRITE', FORM=FORM, ACCESS=ACCESS, STATUS='REPLACE', ASYNC='NO', BUFFER_BLOCKSIZE=BUF, BUFFER_COUNT=2, LINE=LN, INFILE=IN)
    !                   ELSE
    !                        CALL GENERIC_OPEN(LN(ISTART:ISTOP), FL%IU, FL%IOUT, ACTION='WRITE', FORM='FORMATTED', ACCESS='SEQUENTIAL', STATUS='REPLACE', ASYNC='NO', BUFFER_BLOCKSIZE=BUF, BUFFER_COUNT=2, LINE=LN, INFILE=IN)
    !                   END IF
    !             END IF
    !       END IF
    !       !
    !    END IF
    !    !
    !    INQUIRE(FL%IU,FORM=FORM)
    !    IF(FORM == 'FORMATTED') THEN
    !        FL%BINARY = FALSE
    !    ELSE
    !        FL%BINARY = TRUE
    !    END IF
    !    !
    !  END SUBROUTINE
  !
  SUBROUTINE GENERIC_OUTPUT_FILE_WRITE_LINE(FL,LINE)
    CLASS(GENERIC_OUTPUT_FILE), INTENT(INOUT):: FL
    CHARACTER(*), INTENT(IN):: LINE
    !
    IF(FL%IS_OPEN) THEN
        IF(FL%BINARY) THEN
            WRITE(FL%IU) LINE
        ELSE
            WRITE(FL%IU,'(A)') TRIM(LINE)
        END IF
    END IF
    !
  END SUBROUTINE
  !
  SUBROUTINE GENERIC_OUTPUT_FILE_WRITE_SINGLE(FL,VAL,WIDTH,ADVANCE)
    CLASS(GENERIC_OUTPUT_FILE), INTENT(INOUT):: FL
    CLASS(*),          INTENT(IN):: VAL
    INTEGER, OPTIONAL, INTENT(IN):: WIDTH
    LOGICAL, OPTIONAL, INTENT(IN):: ADVANCE
    CHARACTER(3 ):: ADV
    CHARACTER(30):: FMT
    CHARACTER(4):: DEC
    !
    IF(FL%IS_OPEN) THEN
        !
        IF(FL%BINARY) THEN
            SELECT TYPE (VAL)
            TYPE IS (REAL(REAL64))
                WRITE(FL%IU) VAL
            TYPE IS (INTEGER)
                WRITE(FL%IU) VAL
            TYPE IS (REAL(REAL32))
                WRITE(FL%IU) VAL
            TYPE IS (CHARACTER(*))
                WRITE(FL%IU) VAL
            TYPE IS (REAL(REAL128))
                WRITE(FL%IU) VAL
            END SELECT
            RETURN
        END IF
        !
        FMT=BLNK
        DEC=BLNK
        IF(PRESENT(WIDTH)) THEN
            WRITE(FMT,'(I30)') WIDTH - 1  !FORCE 1 blank between numbers
            FMT = ADJUSTL(FMT)
            WRITE(DEC,'(I4)') WIDTH - 8  !WIDTH  - 7 - 1
            DEC = ADJUSTL(DEC)
        END IF
        !
        ADV='YES'
        IF(PRESENT(ADVANCE)) THEN
            IF (.NOT. ADVANCE) ADV='NO'
        END IF
        !
        SELECT TYPE (VAL)
        TYPE IS (REAL(REAL64))
            IF(FMT==BLNK) THEN
                FMT = '(1x ES19.11)'
            ELSE
                FMT = '(1x ES'//TRIM(FMT)//'.'//DEC//')'
            END IF
            !
            WRITE(FL%IU,FMT, ADVANCE=ADV) VAL
        TYPE IS (REAL(REAL32))
            IF(FMT==BLNK) THEN
                FMT = '(1x ES19.11)'
            ELSE
                FMT = '(1x ES'//TRIM(FMT)//'.'//DEC//')'
            END IF
            !
            WRITE(FL%IU,FMT, ADVANCE=ADV) VAL
        TYPE IS (INTEGER)
            IF(FMT==BLNK) THEN
                FMT = '(1x I19)'
            ELSE
                FMT = '(1x I'//TRIM(FMT)//')'
            END IF
            !
            WRITE(FL%IU,FMT, ADVANCE=ADV) VAL
        TYPE IS (CHARACTER(*))
            IF(FMT==BLNK) THEN
                FMT = '(A)'
            ELSE
                FMT = '(1x A'//TRIM(FMT)//')'
            END IF
            !
            WRITE(FL%IU,FMT, ADVANCE=ADV) TRIM(VAL)
        TYPE IS (REAL(REAL128))
            IF(FMT==BLNK) THEN
                FMT = '(1x ES19.11)'
            ELSE
                FMT = '(1x ES'//TRIM(FMT)//'.'//DEC//')'
            END IF
            !
            WRITE(FL%IU,FMT, ADVANCE=ADV) VAL
        END SELECT
    END IF
    !
  END SUBROUTINE
  !
  SUBROUTINE GENERIC_OUTPUT_FILE_WRITE_VECTOR(FL,VAL,WIDTH,ADVANCE)
    CLASS(GENERIC_OUTPUT_FILE), INTENT(INOUT):: FL
    CLASS(*),DIMENSION(:),INTENT(IN):: VAL
    INTEGER, OPTIONAL,    INTENT(IN):: WIDTH
    LOGICAL, OPTIONAL, INTENT(IN):: ADVANCE
    CHARACTER(3 ):: ADV
    CHARACTER(30):: FMT
    CHARACTER(4):: DEC
    INTEGER:: I
    IF(FL%IS_OPEN) THEN
        !
        IF(FL%BINARY) THEN
            SELECT TYPE (VAL)
            TYPE IS (REAL(REAL64))
                WRITE(FL%IU) VAL
            TYPE IS (INTEGER)
                WRITE(FL%IU) VAL
            TYPE IS (REAL(REAL32))
                WRITE(FL%IU) VAL
            TYPE IS (CHARACTER(*))
                WRITE(FL%IU) VAL
            TYPE IS (REAL(REAL128))
                WRITE(FL%IU) VAL
            END SELECT
            RETURN
        END IF
        !
        FMT=BLNK
        DEC=BLNK
        IF(PRESENT(WIDTH)) THEN
            WRITE(FMT,'(I30)') WIDTH - 1  !FORCE 1 blank between numbers
            FMT = ADJUSTL(FMT)
            WRITE(DEC,'(I4)') WIDTH - 8  !WIDTH  - 7 - 1
            DEC = ADJUSTL(DEC)
        END IF
        !
        ADV='YES'
        IF(PRESENT(ADVANCE)) THEN
            IF (.NOT. ADVANCE) ADV='NO'
        END IF
        !
        SELECT TYPE (VAL)
        TYPE IS (REAL(REAL64))
            IF(FMT==BLNK) THEN
                FMT = '(*(1x ES19.11))'
            ELSE
                FMT = '(*(1x ES'//TRIM(FMT)//'.'//DEC//'))'
            END IF
            !
            WRITE(FL%IU,FMT, ADVANCE=ADV) VAL
        TYPE IS (REAL(REAL32))
            IF(FMT==BLNK) THEN
                FMT = '(*(1x ES19.11))'
            ELSE
                FMT = '(*(1x ES'//TRIM(FMT)//'.'//DEC//'))'
            END IF
            !
            WRITE(FL%IU,FMT, ADVANCE=ADV) VAL
        TYPE IS (INTEGER)
            IF(FMT==BLNK) THEN
                FMT = '(*(1x I19))'
            ELSE
                FMT = '(*(1x I'//TRIM(FMT)//'))'
            END IF
            !
            WRITE(FL%IU,FMT, ADVANCE=ADV) VAL
        TYPE IS (CHARACTER(*))
            IF(FMT==BLNK) THEN
                FMT = '(*(A))'
            ELSE
                FMT = '(*(1x A'//TRIM(FMT)//'))'
            END IF
            !
            DO I=ONE,SIZE(VAL)
                WRITE(FL%IU,FMT) TRIM(VAL(I))
            END DO
        TYPE IS (REAL(REAL128))
            IF(FMT==BLNK) THEN
                FMT = '(*(1x ES19.11))'
            ELSE
                FMT = '(*(1x ES'//TRIM(FMT)//'.'//DEC//'))'
            END IF
            !
            WRITE(FL%IU,FMT, ADVANCE=ADV) VAL
        END SELECT
    END IF
    !
  END SUBROUTINE
  !
  SUBROUTINE GENERIC_OUTPUT_FILE_WRITE_ARRAY(FL,VAL,WIDTH,ADVANCE)
    CLASS(GENERIC_OUTPUT_FILE), INTENT(INOUT):: FL
    CLASS(*),DIMENSION(:,:),INTENT(IN):: VAL
    INTEGER, OPTIONAL,    INTENT(IN):: WIDTH
    LOGICAL, OPTIONAL, INTENT(IN):: ADVANCE
    CHARACTER(3 ):: ADV
    CHARACTER(30):: FMT
    CHARACTER(4):: DEC
    INTEGER:: I
    IF(FL%IS_OPEN) THEN
        !
        IF(FL%BINARY) THEN
            SELECT TYPE (VAL)
            TYPE IS (REAL(REAL64))
                WRITE(FL%IU) VAL
            TYPE IS (INTEGER)
                WRITE(FL%IU) VAL
            TYPE IS (REAL(REAL32))
                WRITE(FL%IU) VAL
            TYPE IS (CHARACTER(*))
                WRITE(FL%IU) VAL
            TYPE IS (REAL(REAL128))
                WRITE(FL%IU) VAL
            END SELECT
            RETURN
        END IF
        !
        FMT=BLNK
        DEC=BLNK
        IF(PRESENT(WIDTH)) THEN
            WRITE(FMT,'(I30)') WIDTH - 1  !FORCE 1 blank between numbers
            FMT = ADJUSTL(FMT)
            WRITE(DEC,'(I4)') WIDTH - 8  !WIDTH  - 7 - 1
            DEC = ADJUSTL(DEC)
        END IF
        !
        ADV='YES'
        IF(PRESENT(ADVANCE)) THEN
            IF (.NOT. ADVANCE) ADV='NO'
        END IF
        !
        SELECT TYPE (VAL)
        TYPE IS (REAL(REAL64))
            IF(FMT==BLNK) THEN
                FMT = '(*(1x ES19.11))'
            ELSE
                FMT = '(*(1x ES'//TRIM(FMT)//'.'//DEC//'))'
            END IF
            !
            DO I=ONE, UBOUND(VAL,2)
                WRITE(FL%IU,FMT, ADVANCE=ADV) VAL(:,I)
            END DO
        TYPE IS (REAL(REAL32))
            IF(FMT==BLNK) THEN
                FMT = '(*(1x ES19.11))'
            ELSE
                FMT = '(*(1x ES'//TRIM(FMT)//'.'//DEC//'))'
            END IF
            !
            DO I=ONE, UBOUND(VAL,2)
                WRITE(FL%IU,FMT, ADVANCE=ADV) VAL(:,I)
            END DO
        TYPE IS (INTEGER)
            IF(FMT==BLNK) THEN
                FMT = '(*(1x I19))'
            ELSE
                FMT = '(*(1x I'//TRIM(FMT)//'))'
            END IF
            !
            DO I=ONE, UBOUND(VAL,2)
                WRITE(FL%IU,FMT, ADVANCE=ADV) VAL(:,I)
            END DO
        TYPE IS (CHARACTER(*))
            IF(FMT==BLNK) THEN
                FMT = '(*(A))'
            ELSE
                FMT = '(*(1x A'//TRIM(FMT)//'))'
            END IF
            !
            DO I=ONE, UBOUND(VAL,2)
                WRITE(FL%IU,FMT, ADVANCE=ADV) VAL(:,I)
            END DO
        TYPE IS (REAL(REAL128))
            IF(FMT==BLNK) THEN
                FMT = '(*(1x ES19.11))'
            ELSE
                FMT = '(*(1x ES'//TRIM(FMT)//'.'//DEC//'))'
            END IF
            !
            DO I=ONE, UBOUND(VAL,2)
                WRITE(FL%IU,FMT, ADVANCE=ADV) VAL(:,I)
            END DO
        END SELECT
    END IF
    !
  END SUBROUTINE
  !
  IMPURE ELEMENTAL SUBROUTINE SET_FILE_NAME_GENERIC_OUTPUT_FILE(FL)
    CLASS(GENERIC_OUTPUT_FILE), INTENT(INOUT):: FL
    LOGICAL:: EXIST
    !
    IF(FL%IU /= Z) THEN
        IF(.NOT. ALLOCATED(FL%FNAME)) THEN
            CALL GET_FILE_NAME(FL%IU,FL%FNAME,EXIST,FL%IOUT,Z,MSG='GENERIC_OUTPUT_FILE ERROR: FROUND KEYWORD "EXTERNAL" OR "DATAUNIT", BUT FAILED TO IDENTIFY THE FILE (IN PARTICULAR ITS NAME) THAT IS ASSOCAITED WITH IT.')
        END IF
    ELSEIF(ALLOCATED(FL%FNAME)) THEN
        DEALLOCATE(FL%FNAME)
    END IF
    !
  END SUBROUTINE
  !
  ELEMENTAL IMPURE SUBROUTINE CLOSE_GENERIC_OUTPUT_FILE(FL)
    CLASS(GENERIC_OUTPUT_FILE), INTENT(INOUT):: FL
    INTEGER:: IERR
    !
    IF    (FL%OPENCLOSE) THEN
        CLOSE(FL%IU, IOSTAT=IERR)
    ELSEIF(FL%IU /= Z) THEN
        FLUSH(FL%IU, IOSTAT=IERR)
    END IF
    !
    IF(ALLOCATED(FL%FI))    DEALLOCATE(FL%FI)
    IF(ALLOCATED(FL%FNAME)) DEALLOCATE(FL%FNAME)
    IF(ALLOCATED(FL%EXTRA)) DEALLOCATE(FL%EXTRA)
    !
    FL%IU        = Z
    FL%OPENCLOSE = FALSE
    FL%BINARY    = FALSE
    FL%NULL_FILE = FALSE
    FL%IS_OPEN   = FALSE
    !
  END SUBROUTINE
  !
  ELEMENTAL IMPURE SUBROUTINE FINAL_CLOSE_GENERIC_OUTPUT_FILE(FL)
    TYPE(GENERIC_OUTPUT_FILE), INTENT(INOUT):: FL
    !
    CALL CLOSE_GENERIC_OUTPUT_FILE(FL)
    !
  END SUBROUTINE
  !
  !!!PURE SUBROUTINE GENERIC_OUTPUT_FILE_DEALLOCATE(FL)
  !!!  TYPE(GENERIC_OUTPUT_FILE),ALLOCATABLE,INTENT(INOUT):: FL
  !!!  INTEGER:: I
  !!!  !
  !!!  DEALLOCATE(FL, STAT=I)
  !!!  !
  !!!END SUBROUTINE
  !!!!
  PURE SUBROUTINE GENERIC_OUTPUT_FILE_DEPOINT_DIM0(FL)
    TYPE(GENERIC_OUTPUT_FILE),POINTER,INTENT(INOUT):: FL
    INTEGER:: I
    !
    DEALLOCATE(FL, STAT=I)
    NULLIFY(FL)
    !
  END SUBROUTINE
  !
  PURE SUBROUTINE GENERIC_OUTPUT_FILE_DEPOINT_DIM1(FL)
    TYPE(GENERIC_OUTPUT_FILE),POINTER,DIMENSION(:), INTENT(INOUT):: FL
    INTEGER:: I
    !
    DEALLOCATE(FL, STAT=I)
    NULLIFY(FL)
    !
  END SUBROUTINE
  !
END MODULE
!
