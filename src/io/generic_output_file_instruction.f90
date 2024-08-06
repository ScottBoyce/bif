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
  USE PATH_INTERFACE,         ONLY: GET_CWD
  !
  USE FILE_INCREMENTER_INTERFACE, ONLY: FILE_INCREMENTER
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
      LOGICAL:: STREAM      = FALSE
      LOGICAL:: NULL_FILE   = FALSE
      LOGICAL:: IS_INTERNAL = FALSE
      !
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
    FL%STREAM      = FALSE
    FL%NULL_FILE   = FALSE
    FL%IS_INTERNAL = FALSE
    !
    IF(ALLOCATED(FL%FNAME)) DEALLOCATE(FL%FNAME)
    IF(ALLOCATED(FL%EXTRA)) DEALLOCATE(FL%EXTRA)
    IF(ALLOCATED(FL%FMT  )) DEALLOCATE(FL%FMT  )
    IF(ALLOCATED(FL%FI   )) DEALLOCATE(FL%FI   )
    !
  END SUBROUTINE
  !
  SUBROUTINE OPEN_GENERIC_OUTPUT_FILE(FL, LN, LLOC, OUTPUT, INFILE, REQKEY, NOSTOP, KEY_FAIL_STOP, KEY, KEY_FOUND, FORCE_DATAFILE, ALLOW_ONLY_UNIT, NO_POSTKEY_CHECK, NO_WARN, NO_INTERNAL, NO_CONSTANT, NO_BINARY, SAVE_FNAME, STREAM_TEXT, IU, INTERNAL_IU, DIM, PREPOST, BINARY, BUFFER, REWIND, APPEND, SPLIT_SIZE, SPLITMAXCOUNT, MSG)
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
    CLASS(GENERIC_OUTPUT_FILE), INTENT(INOUT):: FL
    CHARACTER(*),               INTENT(IN   ):: LN              ! Optional Directive plus either file name or unit plus optional post keywords.
    INTEGER,      OPTIONAL,     INTENT(INOUT):: LLOC            ! Position to start parsing line, set to 1 if not present
    INTEGER,      OPTIONAL,     INTENT(IN   ):: OUTPUT, INFILE  ! Output file to write error msg too, infile to report error from.
    LOGICAL,      OPTIONAL,     INTENT(IN   ):: REQKEY          ! If true, then requires specifying a directive keyword (viz OPEN/CLOSE, EXTERNAL, DATAFILE, DATAUNIT, CONSTANT, NULL)
    LOGICAL,      OPTIONAL,     INTENT(IN   ):: NOSTOP          ! If true, then indicates the subroutine wil return if an error occurs, otherwise the program terminates with an error message
    LOGICAL,      OPTIONAL,     INTENT(IN   ):: KEY_FAIL_STOP   ! If true, then always raises an error if a directive keyword is found but there was an error perfomating what it requested, this supercedes NOSTOP=TRUE
    CHARACTER(*), OPTIONAL,     INTENT(INOUT):: KEY             ! Set to the directive that is used.
    LOGICAL,      OPTIONAL,     INTENT(INOUT):: KEY_FOUND       ! Set to true if a directive is found
    LOGICAL,      OPTIONAL,     INTENT(IN   ):: FORCE_DATAFILE  ! If true, then the file is always opened as a DATAFILE (ie its unit number does not close till program ends)
    LOGICAL,      OPTIONAL,     INTENT(IN   ):: ALLOW_ONLY_UNIT ! If true and REQKEY=FALSE, then the allows checking for a unit number without the EXTERNAL or DATAUNIT directives (ie, a single integer is not assumed to be a unit number)
    LOGICAL,      OPTIONAL,     INTENT(IN   ):: NO_POSTKEY_CHECK !If true, then no post keywords are checked for.
    LOGICAL,      OPTIONAL,     INTENT(IN   ):: NO_WARN         ! If true, warnings are supressed
    LOGICAL,      OPTIONAL,     INTENT(IN   ):: NO_INTERNAL     ! If true, the INTERNAL directive is not allowed
    LOGICAL,      OPTIONAL,     INTENT(IN   ):: NO_CONSTANT     ! If true, the CONSTANT directive is not allowed
    LOGICAL,      OPTIONAL,     INTENT(IN   ):: NO_BINARY       ! If true, the BINARY keyword     is not allowed
    LOGICAL,      OPTIONAL,     INTENT(IN   ):: SAVE_FNAME      ! If true, then store the file name under FL%FNAME
    LOGICAL,      OPTIONAL,     INTENT(IN   ):: STREAM_TEXT     ! If true, then the file is always opend with ACCESS = "STREAM"
    INTEGER,      OPTIONAL,     INTENT(IN   ):: IU              ! When present is the unit number used when a file is opened by OPEN/CLOSE or by NAME
    INTEGER,      OPTIONAL,     INTENT(IN   ):: INTERNAL_IU     ! Is the unit to write to if INTERNAL directive is found, otherwise uses FL%IOUT
    INTEGER,      OPTIONAL,     INTENT(INOUT):: DIM             ! Value set by post-keyword DIM or DIMENSION followed by an INT.
    INTEGER,      OPTIONAL,     INTENT(INOUT):: PREPOST         ! Holds location before running post-keycheck
    LOGICAL,      OPTIONAL,     INTENT(IN   ):: BINARY          ! If true, then the file is opened as unformated binary.
    INTEGER,      OPTIONAL,     INTENT(IN   ):: BUFFER          ! Sets the file buffer in Kilobyes
    LOGICAL,      OPTIONAL,     INTENT(IN   ):: REWIND          ! Rewind file before exiting routing
    LOGICAL,      OPTIONAL,     INTENT(IN   ):: APPEND          ! If true, then file is opened with append (at end of file)
    INTEGER,      OPTIONAL,     INTENT(IN   ):: SPLIT_SIZE      ! Specifies the size, in MB, of when a file is spit into a new file (default is 0, or never split)
    INTEGER,      OPTIONAL,     INTENT(IN   ):: SPLITMAXCOUNT   ! Specifies the interval that the split size is checked (default is 10)
    CHARACTER(*), OPTIONAL,     INTENT(IN   ):: MSG             ! Message to pass to stop routing if progarm terminates.
    !
    LOGICAL:: ISOPEN, ALLOW_ERROR, NOREQKEY, FOUND_KEY, DATAFILE
    LOGICAL:: GO_TO_TOP, SAVE_FN, CHECK_POST, CHECK_ONLY_UNIT, NOT_UNIQUE
    LOGICAL:: IS_EXTERNAL, NO_BINARY_FLAG
    !
    CHARACTER(12):: STAT_TXT
    CHARACTER(12):: EXT, FORM_CHK, ACCESS_TXT, POS_TXT
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
    IS_EXTERNAL  = FALSE
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
    IF(PRESENT(ALLOW_ONLY_UNIT)) THEN
        CHECK_ONLY_UNIT = ALLOW_ONLY_UNIT .and. NOREQKEY
    ELSE
        CHECK_ONLY_UNIT = FALSE
    END IF
    !
    IF(PRESENT(NO_POSTKEY_CHECK)) THEN
        CHECK_POST = .NOT. NO_POSTKEY_CHECK
    ELSE
        CHECK_POST = TRUE
    END IF
    !
    IF(PRESENT(NO_BINARY)) THEN
        NO_BINARY_FLAG = NO_BINARY
    ELSE
        NO_BINARY_FLAG = FALSE
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
    BUF = 16384 ! 16KB x2 = 32KB  --1048576 = 1MB   --Note that two threads are used per buffer so actual space is twice as big
    IF(PRESENT(BUFFER)) THEN
        BUF = BUFFER
        IF( BUF > SEV   ) THEN
            BUF = 4096*(BUF/EIGHT)  !Make sure it is a multiple of 8  -- 4096=512*8
        ELSEIF( BUF > Z ) THEN
            BUF = 512*BUF  ! Multiply by 0.5KB
        END IF
    END IF
    !
    STAT_TXT= 'REPLACE'
    POS_TXT = 'REWIND'
    IF(PRESENT(APPEND)) THEN
            IF(APPEND) THEN
                       POS_TXT = 'APPEND'
                       STAT_TXT= 'UNKNOWN'
            END IF
    END IF
    !
    MXCNT = 10
    IF(PRESENT(SPLITMAXCOUNT)) MXCNT = SPLITMAXCOUNT
    !
    IF(PRESENT(SPLIT_SIZE)) SPLIT = SPLIT_SIZE
    !
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
    ELSE IF (CHECK_ONLY_UNIT .AND. LN(ISTART:ISTOP) /= BLNK) THEN
                      READ(LN(ISTART:ISTOP),*,IOSTAT=IERR) IU_READ
    ELSE
        IERR=69
    END IF
    !
    IF (IERR == Z) THEN               ! FOUND UNIT NUMBER--CHECK FOR POST KEYS
          FL%IU       = IU_READ
          EXT         ='IMPLIED_UNIT'
          IS_EXTERNAL = TRUE
          IF(CHECK_POST) CALL CHECK_FOR_POST_KEY(LL, LN, IIN, FL%IOUT, BUF, SPLIT, BINARY=FL%BINARY, GO_TO_TOP=GO_TO_TOP, DIM=DIM, FMT=FL%FMT, OLDLOC=PREPOST, NO_WARN=NO_WARN, MSG=MSG)
    ELSE IF(.not. FL%ERROR) THEN
          IERR = Z
          !
          IF(EXT == 'BINARY') THEN
                              FL%BINARY = TRUE
                              CALL GET_WORD(LN,LL,ISTART,ISTOP,EXT)
          END IF
          !
          IF(EXT == 'EXTERNAL' .OR. EXT =='DATAUNIT') THEN  ! Quick grab of unit number if directive found.
                                           FOUND_KEY   = TRUE
                                           IS_EXTERNAL = TRUE
                                           CALL GET_INTEGER(LN,LL,ISTART,ISTOP,FL%IOUT,IIN,FL%IU,HAS_ERROR=FL%ERROR)
                                           !
                                           IF(FL%ERROR) THEN
                                               FNAME = LN(ISTART:ISTOP)
                                               CALL DATAFILE_UNIT_NUMBER%CHECK_BASE(FNAME,FL%IU,NOT_UNIQUE)
                                               FL%ERROR = FL%IU /= Z .AND. NOT_UNIQUE                 ! Found IU and its basename is Unique
                                           END IF
                                           IF(CHECK_POST) CALL CHECK_FOR_POST_KEY(LL, LN, IIN, FL%IOUT, BUF, SPLIT, BINARY=FL%BINARY, GO_TO_TOP=GO_TO_TOP, DIM=DIM, FMT=FL%FMT, OLDLOC=PREPOST, NO_WARN=NO_WARN, MSG=MSG)
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
          END IF
          !
          IF(EXT == 'LIST')  EXT = 'INTERNAL'
          !
          IF(EXT == 'INTERNAL') THEN
                                           FOUND_KEY      = TRUE
                                           FL%IS_INTERNAL = TRUE
                                           FL%IU = FL%IOUT
                                           !
                                           IF( PRESENT(INTERNAL_IU) ) THEN
                                                    IF(INTERNAL_IU /= Z) FL%IU = INTERNAL_IU
                                           END IF
                                           !
                                           IF(CHECK_POST) CALL CHECK_FOR_POST_KEY(LL, LN, IIN, FL%IOUT, BUF, SPLIT, BINARY=FL%BINARY, GO_TO_TOP=GO_TO_TOP, DIM=DIM, FMT=FL%FMT, OLDLOC=PREPOST, NO_WARN=NO_WARN, MSG=MSG)
                                           !
                                           IF(FL%IU == Z) CALL ADD_MSG(ERR_MSG, 'Found INTERNAL directive keyword, but the code does not know what the internal unit number is.'//NL//'That is, the code is not setup to handel the INTERNAL directive'//NL//' This is either a code bug or there will be another message stating INTERNAL is not supported.')
                                           !
                                           IF( PRESENT(NO_INTERNAL) ) THEN
                                                    IF(NO_INTERNAL) THEN
                                                        FL%ERROR = TRUE
                                                        CALL ADD_MSG(ERR_MSG, 'Found INTERNAL directive keyword, but this input data item does not allow it.'//NL//'Please change input to specify a separate file with the OPEN/CLOSE, DATAFILE, or EXTERNAL directives.')
                                                    END IF
                                           END IF
                                           !
          ELSEIF(EXT == 'EXTERNAL' .OR. EXT =='DATAUNIT') THEN  ! Already read unit number
                                           CONTINUE
          ELSEIF(EXT =='NULL' .OR. EXT =='NUL' .OR. EXT == 'SKIP' .OR. EXT == 'NOPRINT') THEN
                                           FOUND_KEY = TRUE
                                           IF(CHECK_POST) CALL CHECK_FOR_POST_KEY(LL, LN, IIN, FL%IOUT, BUF, SPLIT, BINARY=FL%BINARY, GO_TO_TOP=GO_TO_TOP, DIM=DIM, FMT=FL%FMT, OLDLOC=PREPOST, NO_WARN=NO_WARN, MSG=MSG)
                                           CALL FL%CLOSE()
                                           FL%NULL_FILE = TRUE
                                           FNAME = "NULL"
                                           !
                                           FL%IU = NULL_FILE%GET(LN,IIN,FL%IOUT)
                                           !
                                           IF(PRESENT(LLOC)) LLOC = LL
                                           IF(PRESENT(KEY_FOUND)) KEY_FOUND = TRUE
                                           IF(PRESENT(KEY)) KEY = FNAME
                                           IF(SAVE_FN) FL%FNAME = FNAME
          ELSEIF(.not. FL%ERROR) THEN
                IF (EXT == 'OPEN/CLOSE' .OR. EXT == 'OPENCLOSE' .OR. EXT == 'DATAFILE') THEN  !OPEN/CLOSE FILE
                                           FOUND_KEY    = TRUE
                                           FL%OPENCLOSE = TRUE
                                           !
                                           DATAFILE  = EXT == 'DATAFILE'
                                           !
                                           CALL PARSE_WORD(LN,LL,ISTART,ISTOP)   ! MOVE TO NEXT WORD WHICH IS THE FILE NAME
                ELSEIF(NOREQKEY) THEN
                                           FL%OPENCLOSE = TRUE
                                           EXT='IMPLIED_FILE'
                ELSE
                                           EXT='IMPLIED_LINE' ! Failed to open a file, set flag to indicate that input is either bad or located along line (Implied Internal)
                                           FL%IU = Z
                                           FL%ERROR = TRUE
                END IF
                !
                IF(PRESENT(FORCE_DATAFILE) .AND. FL%OPENCLOSE .AND. .NOT. DATAFILE) DATAFILE = FORCE_DATAFILE
                !
                IF(ISTART > ISTOP) THEN               ! Error parsing file name (can only be true if OPEN/CLOSE does not have a file after it)
                    FL%ERROR = TRUE
                    CALL ADD_MSG(ERR_MSG, &
                            'Found '//TRIM(EXT)//' directive,'//NL// &
                            '  which should be followed by a file name to open,'//NL// &
                            '  but nothing was found after it.')
                ELSE IF ( .not. FL%ERROR) THEN
                    ALLOCATE( FNAME, SOURCE = LN(ISTART:ISTOP) )
                    !
                    INQUIRE(FILE=FNAME, NUMBER=I, OPENED=ISOPEN)
                    !
                    IF(.not. ISOPEN .and. DATAFILE) THEN  ! Check for BaseName
                       CALL DATAFILE_UNIT_NUMBER%CHECK_BASE(FNAME,I,NOT_UNIQUE)
                       ISOPEN = I /= Z .and. .not. NOT_UNIQUE                 ! Found IU and its basename is Unique
                       IF(ISOPEN) THEN
                           BLOCK
                               CHARACTER(12):: ACTION_TXT
                               INQUIRE(UNIT=I, ACTION=ACTION_TXT, OPENED=ISOPEN)
                               IF( ACTION_TXT /= 'WRITE' .and. ACTION_TXT /= 'READWRITE' ) ISOPEN = FALSE
                           END BLOCK
                       END IF
                    END IF
                    !
                END IF
                !
                IF(FL%ERROR) THEN
                    FL%IU    = Z
                    FL%ERROR = TRUE
                ELSE
                    IF(CHECK_POST) CALL CHECK_FOR_POST_KEY(LL, LN, IIN, FL%IOUT, BUF, SPLIT, BINARY=FL%BINARY, GO_TO_TOP=GO_TO_TOP, DIM=DIM, FMT=FL%FMT, OLDLOC=PREPOST, NO_WARN=NO_WARN, MSG=MSG)
                    !
                    IF( DATAFILE .AND. ISOPEN) THEN
                                               FL%IU = I
                                               FL%OPENCLOSE = FALSE
                    ELSEIF(FL%OPENCLOSE .AND. ISOPEN) THEN
                                               FL%IU = I
                                               FL%OPENCLOSE = FALSE
                                               CALL WARNING_MESSAGE(LN,IIN,FL%IOUT,MSG='GENERIC_OUTPUT_FILE: OPEN/CLOSE directive attempted to open the file:'//NL//TRIM(FNAME)//NL//'Has already been opened/associated with a fortran unit number.'//NL//'That is, it has been opened somewhere else for writing, so now two or more different code features will attempt to write to the same file.'//NL//'If you want to supress this warning, just change the directive keyword from OPEN/CLOSE to DATAFILE to indicate you do mean to reuse the same file.')
                    ELSEIF(FL%OPENCLOSE) THEN
                                               FL%IU = Z
                                               IF(PRESENT(IU)) FL%IU = IU
                                               !
                                               IF(FL%BINARY) THEN
                                                    CALL GENERIC_OPEN(FNAME, FL%IU, FL%IOUT, ACTION='WRITE', FORM='UNFORMATTED', ACCESS='STREAM',   POSITION=POS_TXT, STATUS=STAT_TXT, ASYNC='NO', BUFFER_BLOCKSIZE=BUF, BUFFER_COUNT=2, LINE=LN, INFILE=IIN, ERROR=FL%ERROR)
                                               ELSE
                                                    CALL GENERIC_OPEN(FNAME, FL%IU, FL%IOUT, ACTION='WRITE', FORM=  'FORMATTED', ACCESS=ACCESS_TXT, POSITION=POS_TXT, STATUS=STAT_TXT, ASYNC='NO', BUFFER_BLOCKSIZE=BUF, BUFFER_COUNT=2, LINE=LN, INFILE=IIN, ERROR=FL%ERROR)
                                               END IF
                                               !
                                               IF     (FL%ERROR) THEN
                                                                     FL%OPENCLOSE = FALSE
                                                                     CALL ADD_MSG(ERR_MSG, &
                                                                             'Found '//TRIM(EXT)//' directive,'//NL//        &
                                                                             '  but failed to open the file:'//NL//          &
                                                                             '     "'//FNAME//'"'//NL//                      &
                                                                             'The reason for the failure is unknown.'//NL//  &
                                                                             'Perhaps the path to the file is bad or '//NL// &
                                                                             '  the file is locked by another program'//NL// &
                                                                             '  preventing the write only file from being created or appended to.'//BLN// &
                                                                             'Please check to see if the path and file name are correct'//NL// &
                                                                             '  and that either the file does not exist (so it can be created)'//NL// &
                                                                             '  or is editable so that it can be overwritten or appended to).'//NL// &
                                                                             'If you are are using a relative path to specify the file (such as: ../dir/file.txt),'//NL// &
                                                                             '  then the current working director (point for relative paths) is:'//NL// &
                                                                             '     "'//GET_CWD()//'"'//BLN//                                    &
                                                                             '  ***Note: in a path to a file'//NL//                             &
                                                                             '     the the "/" works for both Windows and Linux,' //NL//        &
                                                                             '     but the "\" only works on Windows.')
                                                                     IF(EXT == 'IMPLIED_FILE') &
                                                                         CALL ADD_MSG(ERR_MSG, &
                                                                             'The IMPLIED_FILE directive is a place holder for when no directive is found.'//NL//  &
                                                                             'The given input line assumes that it contains a file name that should be opened.'//NL//  &
                                                                             'That is: "path/to/myFile.txt" is marked as an IMPLIED_FILE and'//NL//  &
                                                                             '         treated as equivalent to "OPEN/CLOSE  path/to/myFile.txt"')
                                               ELSEIF (DATAFILE) THEN
                                                                     FL%OPENCLOSE = FALSE
                                                                     CALL DATAFILE_UNIT_NUMBER%ADD(FL%IU, FALSE)
                                               END IF
                    ELSE
                                               FL%ERROR = TRUE  ! Did not find OPEN/CLOSE keyword and REQKEY=TRUE - Error message written later if not "IMPLIED_LINE"
                    END IF
                END IF
          END IF
    END IF
    !
    IF(PRESENT(KEY_FAIL_STOP)) ALLOW_ERROR = ALLOW_ERROR .or. ( KEY_FAIL_STOP .and. FOUND_KEY )
    !
    IF(ALLOW_ERROR .AND. FL%IU /= Z) THEN
      IF( EXT == 'EXTERNAL' .or. EXT == 'DATAUNIT' .or. EXT == 'IMPLIED_UNIT' ) THEN
        I = FL%IU
        BLOCK 
            CHARACTER(12):: ACTION_TXT
            INQUIRE(UNIT=I, ACTION=ACTION_TXT, OPENED=ISOPEN)
            IF( ACTION_TXT /= 'WRITE' .and. ACTION_TXT /= 'READWRITE' ) ISOPEN = FALSE
        END BLOCK
        !
        IF(.NOT. ISOPEN) FL%ERROR = TRUE ! Unit associated with no file or not opened with correct permissions
        !
        IF(FL%ERROR) THEN
           CALL ADD_MSG(ERR_MSG, &
                   'Found '//TRIM(EXT)//' directive,'//NL//                         &
                   '  but the unit number specified, '//NUM2STR(FL%IU)//','//NL//   &
                   ' is not associated with an opened file or'//NL//                &
                   ' is not associated with a file opened with a "WRITE" permission.')
           IF(EXT == 'IMPLIED_UNIT') &
               CALL ADD_MSG(ERR_MSG, &
                   'The IMPLIED_UNIT directive is a place holder for when no directive is found'//NL// &
                   '  but a unit number was sucessfully read in.'//BLN//                               &
                   'This input line assumes that if it contains an integer,'//NL//                     &
                   '  then it is a unit number associated with a file.'//NL//                          &
                   'That is: "56" is marked as an IMPLIED_UNIT and'//NL//                              &
                   '         treated as equivalent to "EXTERNAL  56"'//NL//                            &
                   'Note, IMPLIED_UNIT is typically not allowed unless'//NL//                          &
                   '  the code is set up to handle it.'//NL//  &
                   '  (in this case this input does allow for it).')
           
        END IF
      END IF
    END IF
    !
    IF(FL%ERROR) THEN
            FL%IU     = Z
            FL%BINARY = FALSE
            FL%STREAM = FALSE
    ELSE
        I = FL%IU
        IF( I /= Z) THEN
            INQUIRE(UNIT=I, FORM=FORM_CHK, ACCESS=ACCESS_TXT, OPENED=ISOPEN)
            !
            IF(FL%BINARY .and. .not. FORM_CHK /= 'FORMATTED') THEN  ! BINARY keyword found, but file was not opened as a binary file.  FORM_CHK /= 'FORMATTED' -> Binary
                 FL%ERROR = TRUE
                 CALL ADD_MSG(ERR_MSG, &
                         'Found BINARY keyword,'//NL// &
                         '  but the file was not opened as a stream unformatted (BINARY) file.'//NL// &
                         'This typically occurs if the file was opened elsewhere, but without the BINARY flag.')
            END IF
            !
            FL%BINARY = FORM_CHK /= 'FORMATTED'
            FL%STREAM = ACCESS_TXT == 'STREAM'
            IF(.NOT. ISOPEN) FL%ERROR = TRUE ! File unit was not successifully connected to a file.
        END IF
    END IF
    !
    IF(.NOT. FL%ERROR .and. PRESENT(IU)) THEN
       IF (FL%IU /= IU .and. FL%IU /= Z .and. IU /= Z) THEN
          FL%ERROR = TRUE
          IF(IS_EXTERNAL) THEN
              CALL ADD_MSG(ERR_MSG, 'Possible Code issues because OPEN method was told to use a specific unit number, "'//NUM2STR(IU)//'", but instead the input read requested a different unit number, "'//NUM2STR(FL%IU)//'"')
          ELSE IF(.not. FL%IS_INTERNAL .AND. .not. FL%NULL_FILE) THEN
              CALL ADD_MSG(ERR_MSG, 'Possible Code issues because OPEN method was told to use a specific unit number, "'//NUM2STR(IU)//'", but the input specified a file that is already opened under a differnet unit number "'//NUM2STR(FL%IU)//'"'//NL//'That is, the file was opened or in use somewhere else and not closed.')
          END IF
       END IF
    END IF
    !
    IF(FL%BINARY .and. PRESENT(NO_BINARY)) THEN
      IF(NO_BINARY) THEN
          FL%ERROR = TRUE
         CALL ADD_MSG(ERR_MSG, 'For the given input the file opened is a stream unformatted (BINARY) file,'//NL//'but the file open method (this input specifying the file to open) is set to only allow for a formatted text (ASCII) file.'//NL//'Please remove the "BINARY" keyword or check input to ensure it is setup for binary output.')
      END IF
    END IF
    !
    IF(.NOT. FL%ERROR .AND. SPLIT > Z) THEN
                           ALLOCATE(FL%FI)
                           CALL FL%FI%INIT(SPLIT, FL%IU, BUF, MAXCOUNT=MXCNT)
    END IF
    !
    ! Check if ERROR occured and either stopping is allowed or that a keyword was found so it can not be an implied internal
    !
    IF(ALLOW_ERROR .and. FL%ERROR .and. .not. FL%NULL_FILE) THEN
      IF(.not. NOREQKEY .AND.  .not. FOUND_KEY) THEN  ! Require keword, but none found
          CALL HED_MSG(ERR_MSG, &
                  'Failed to identify a directive keyword while processing the'//NL//    &
                  '   line that specified the input file to make a connection to (ie open the file).'//NL// &
                  'However, the input for opening this file requires that the directive keyword'//NL//      &
                  '   be specified (no implied opening of a file by name).'//BLN//                          &
                  'This input expects one of the following directive keywords:'//NL//                       &
                  '   INTERNAL'  //NL//                                                                     &
                  '   OPEN/CLOSE'//NL//                                                                     &
                  '   OPENCLOSE' //NL//                                                                     &
                  '   DATAFILE'  //NL//                                                                     &
                  '   DATAUNIT'  //NL//                                                                     &
                  '   EXTERNAL'  //NL//                                                                     &
                  '   NULL'      //NL//                                                                     &
                  '   and SKIP')
      END IF
      !
      CALL HED_MSG(ERR_MSG,'FAILED TO OPEN FILE WITH GENERIC_OUTPUT_FILE.')
      !
      CALL FILE_IO_ERROR(LINE=LN, INFILE=IIN, OUTPUT=FL%IOUT, MSG=ERR_MSG, MSG2=MSG)
      !
    END IF
    !
    IF (FL%ERROR) THEN
                       FL%IU = Z
                       LL = LLOC_BAK
    END IF
    !
    FL%IS_OPEN = FL%IU /= Z
    !
    IF(PRESENT(KEY)) KEY = ADJUSTL(EXT)
    IF(PRESENT(KEY_FOUND)) KEY_FOUND = FOUND_KEY
    !
    IF(PRESENT(REWIND) .AND. .NOT. GO_TO_TOP) GO_TO_TOP = REWIND
    !
    IF(GO_TO_TOP) CALL REWIND_GENERIC_OUTPUT_FILE(FL)
    !
    IF(PRESENT(LLOC)) LLOC = LL
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
  SUBROUTINE MOVE_GENERIC_OUTPUT_FILE(FL, FL_NEW)
    CLASS(GENERIC_OUTPUT_FILE),  INTENT(INOUT):: FL
    CLASS(GENERIC_OUTPUT_FILE),  INTENT(INOUT):: FL_NEW
    !
    FL_NEW%IU          = FL%IU
    FL_NEW%IOUT        = FL%IOUT
    FL_NEW%IS_OPEN     = FL%IS_OPEN
    FL_NEW%OPENCLOSE   = FL%OPENCLOSE
    FL_NEW%ERROR       = FL%ERROR
    FL_NEW%BINARY      = FL%BINARY
    FL_NEW%STREAM      = FL%STREAM
    FL_NEW%NULL_FILE   = FL%NULL_FILE  
    FL_NEW%IS_INTERNAL = FL%IS_INTERNAL
    !
    IF(ALLOCATED(FL%FNAME)) CALL MOVE_ALLOC(FL%FNAME,FL_NEW%FNAME)
    IF(ALLOCATED(FL%EXTRA)) CALL MOVE_ALLOC(FL%EXTRA,FL_NEW%EXTRA)
    IF(ALLOCATED(FL%FMT))   CALL MOVE_ALLOC(FL%FMT,  FL_NEW%FMT)
    IF( ALLOCATED(FL%FI))   CALL MOVE_ALLOC(FL%FI,   FL_NEW%FI)
    !
    FL%OPENCLOSE = FALSE
    FL%IS_OPEN   = FALSE
    !
  END SUBROUTINE
  !
  IMPURE ELEMENTAL SUBROUTINE REWIND_GENERIC_OUTPUT_FILE(FL)
    CLASS(GENERIC_OUTPUT_FILE), INTENT(INOUT):: FL
    !
    IF(FL%IS_OPEN) REWIND(FL%IU)
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
  SUBROUTINE GENERIC_OUTPUT_FILE_DEPOINT_DIM0(FL)
    TYPE(GENERIC_OUTPUT_FILE),POINTER,INTENT(INOUT):: FL
    INTEGER:: I
    !
    DEALLOCATE(FL, STAT=I)
    NULLIFY(FL)
    !
  END SUBROUTINE
  !
  SUBROUTINE GENERIC_OUTPUT_FILE_DEPOINT_DIM1(FL)
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
