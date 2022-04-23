!--------------------------------------------------------------------------------------------------------
!
! GENERIC_INPUT_FILE_INSTRUCTION MODULE
!
! Read-Only File Object/Data Type.
!
! Code developed by Scott E Boyce
!                   Contact <seboyce@usgs.gov> or <Boyce@engineer.com>
!--------------------------------------------------------------------------------------------------------
!
! Module provides a data type with a generic interface for opening a read only file. 
!
! Given a line sent to open statement it will:
! 
! first  checks for unit number. if there then it will use that.
! second checks for "external"   keyword and then obtain unit number after it
! third  checks for "open/close" keyword and then opens file and sets unit number
! fourth attempts to open file specified within line. raises an error if it cannot be opened. error can be overrided with nostop
!
!--------------------------------------------------------------------------------------------------------
!
! Example Use
!
!  PROGRAM EXAMPLE
!    !
!    USE GENERIC_INPUT_FILE_INSTRUCTION, ONLY: GENERIC_INPUT_FILE
!    !
!    TYPE(GENERIC_INPUT_FILE):: FL
!    CHARACTER(128):: LINE
!    !
!    ! Example 1 open a file
!    !
!    FL%OPEN('Input_File.txt')
!    !
!    ! File is opened on unit stored on FL%IU
!    READ(FL%IU, '(A)') LINE
!    !
!    ! Or use built in read function
!    CALL FL%READ(LINE)
!    !
!    ! Move back 1 line
!    CALL FL%BACKSPACE()
!    !
!    ! Move back to first line
!    CALL FL%REWIND()
!    !
!    ! Close file   - Note automatically done if FL is deallocated.
!    CALL FL%CLOSE()
!    !
!    END PROGRAM
!
!--------------------------------------------------------------------------------------------------------
!
!   GENERIC_INPUT_FILE_INSTRUCTION
!                           DATA TYPE
!                                    GENERIC_INPUT_FILE
!                           SUBROUTINES
!                                    FL%OPEN   
!                                    FL%READ   
!                                    FL%REWIND 
!                                    FL%MOVE   
!                                    FL%CLOSE
!                                    FL%COUNT_LINES
!       
MODULE GENERIC_INPUT_FILE_INSTRUCTION!, ONLY: GENERIC_INPUT_FILE                        --side note, reposition STREAM with READ(IU,'()', ADVANCE='NO', POS=P)
  ! OPENS AN FILE FOR READING. FOR A GIVEN LINE SENT TO OPEN STATEMENT IT WILL:
  ! FIRST  CHECKS FOR UNIT NUMBER. IF THERE THEN IT WILL USE THAT.
  ! SECOND CHECKS FOR "EXTERNAL"   KEYWORD AND THEN OBTAIN UNIT NUMBER AFTER IT
  ! THIRD  CHECKS FOR "OPEN/CLOSE" KEYWORD AND THEN OPENS FILE AND SETS UNIT NUMBER
  ! FOURTH ATTEMPTS TO OPEN FILE SPECIFIED WITHIN LINE. RAISES AN ERROR IF IT CANNOT BE OPENED. ERROR CAN BE OVERRIDED WITH NOSTOP
  !
  USE, INTRINSIC:: ISO_FORTRAN_ENV, ONLY: REAL32, REAL64, REAL128
  !
  USE CONSTANTS
  USE PARSE_WORD_INTERFACE,   ONLY: PARSE_WORD, GET_WORD
  USE NUM2STR_INTERFACE,      ONLY: NUM2STR
  USE POST_KEY_SUB,           ONLY: CHECK_FOR_POST_KEY
  USE ERROR_INTERFACE,        ONLY: FILE_IO_ERROR, STOP_ERROR, GET_WARN, WARNING_MESSAGE
  USE GENERIC_OPEN_INTERFACE, ONLY: GENERIC_OPEN, UTF8_BOM_OFFSET_REWIND
  USE FILE_IO_INTERFACE,      ONLY: DATAFILE_UNIT_NUMBER, GET_FILE_NAME,        &
                                    READ_TO_DATA, MAX_LINE_LENGTH, COMMENT_INDEX
  USE STRINGS,                ONLY: UPPER, GET_INTEGER, GET_NUMBER, SPECIAL_BLANK_STRIP
  USE PATH_INTERFACE,         ONLY: GET_CWD
  !
  IMPLICIT NONE
  !
  PRIVATE
  PUBLIC:: GENERIC_INPUT_FILE
  !
  TYPE GENERIC_INPUT_FILE
      INTEGER:: IU          = Z
      INTEGER:: IOUT        = Z
      LOGICAL:: OPENCLOSE   = FALSE
      LOGICAL:: ERROR       = FALSE   !IS TRUE IF NO FLAG IS FOUND AND FAILED TO OPEN FILE
      LOGICAL:: SKIP        = FALSE   !IS TRUE IF FILE WAS SET WITH SKIP OR NULL
      LOGICAL:: IS_CONSTANT = FALSE   !IS TRUE IF CONSTANT KEYWORD IS FOUND
      LOGICAL:: BINARY      = FALSE
      LOGICAL:: STREAM      = FALSE
      LOGICAL:: IS_BOM      = FALSE
      LOGICAL:: IS_EXTERNAL = FALSE
      CHARACTER(:),ALLOCATABLE:: FNAME
      !
      REAL(REAL64):: SCALE = UNO
      !
      REAL(REAL64), ALLOCATABLE:: CONST  !Only allocated if IS_CONSTANT is TRUE
      !
      CONTAINS
      !
      PROCEDURE, PASS(FL):: OPEN   => OPEN_GENERIC_INPUT_FILE !(LN,[LLOC,OUTPUT,INFILE,NOSTOP,REQKEY,NOSFAC,IU,BINARY,BUFFER,EOL,NO_INTERNAL,NO_ONLY_UNIT,NO_CONSTANT,NEW_UNIT,SAVE_FNAME,KEY,DIM,PREPOST,STREAM_TEXT,NOPOSTKEY])
      GENERIC::             READ   => READ_GENERIC_INPUT_FILE_LINE, READ_GENERIC_INPUT_FILE_VECTOR,READ_GENERIC_INPUT_FILE_ARRAY
      PROCEDURE, PASS(FL):: REWIND => REWIND_GENERIC_INPUT_FILE
      PROCEDURE, PASS(FL):: BACK   => BACKSPACE_GENERIC_INPUT_FILE !([N])
      PROCEDURE, PASS(FL):: MOVE   => MOVE_GENERIC_INPUT_FILE
      PROCEDURE, PASS(FL):: CLOSE  => CLOSE_GENERIC_INPUT_FILE
      PROCEDURE, PASS(FL):: NOCLOSE=> NOCLOSE_GENERIC_INPUT_FILE
      PROCEDURE, PASS(FL):: SET_FNAME   => SET_FILE_NAME_GENERIC_INPUT_FILE
      PROCEDURE, PASS(FL):: COUNT_LINES => COUNT_UNCOMMENTED_LINES_GENERIC_INPUT_FILE
      PROCEDURE, PASS(FL):: BACKSPACE   => BACKSPACE_GENERIC_INPUT_FILE 
      PROCEDURE, PASS(FL):: GETPOS      => GET_FILE_POSITION !MUST BE OPENED WITH STREAM
      PROCEDURE, PASS(FL):: POS         => GOTO_FILE_POSITION
      !
      GENERIC            :: ASSIGNMENT(=) => COPY_GENERIC_INPUT_FILE
      !
      PROCEDURE, PASS(FL), PRIVATE:: READ_GENERIC_INPUT_FILE_LINE
      PROCEDURE, PASS(FL), PRIVATE:: READ_GENERIC_INPUT_FILE_VECTOR
      PROCEDURE, PASS(FL), PRIVATE:: READ_GENERIC_INPUT_FILE_ARRAY
      PROCEDURE, PASS(FL), PRIVATE:: BACKSPACE_GENERIC_INPUT_FILE
      PROCEDURE,           PRIVATE:: COPY_GENERIC_INPUT_FILE
      !
      FINAL::                       FINAL_CLOSE_GENERIC_INPUT_FILE
  END TYPE
  !
  CONTAINS
  !
  SUBROUTINE RESET_GENERIC_INPUT_FILE(FL)
    CLASS(GENERIC_INPUT_FILE), INTENT(INOUT):: FL
    INTEGER:: IERR
    !
    IF(FL%OPENCLOSE) CLOSE(FL%IU, IOSTAT=IERR)
    !
    FL%IU          = Z
    FL%IOUT        = Z
    FL%OPENCLOSE   = FALSE
    FL%ERROR       = FALSE
    FL%BINARY      = FALSE
    FL%STREAM      = FALSE
    FL%SKIP        = FALSE
    FL%IS_EXTERNAL = FALSE
    !
    FL%IS_BOM      = FALSE
    FL%IS_CONSTANT = FALSE
    FL%SCALE       = UNO
    !
    IF(ALLOCATED(FL%FNAME)) DEALLOCATE(FL%FNAME)
    IF(ALLOCATED(FL%CONST)) DEALLOCATE(FL%CONST)
    !
  END SUBROUTINE
  !
  RECURSIVE SUBROUTINE OPEN_GENERIC_INPUT_FILE(FL, LN, LLOC, OUTPUT, INFILE, NOSTOP, REQKEY, NOSFAC, IU, BINARY, BUFFER, EOL, NO_WARN, NO_INTERNAL, NO_ONLY_UNIT, NO_CONSTANT, NEW_UNIT, SAVE_FNAME, KEY, DIM, PREPOST, STREAM_TEXT, NOPOSTKEY, FORCE_DATAFILE, MSG)
    ! ATTEMPTS TO READ KEYWORDS AND OPEN AN EXISTING INTPUT FILE
    ! SETS ERROR=TRUE IF FILE FAILED TO IDENTIFY OR OPEN A FILE/UNIT.
    !
    ! THE ORDER THAT THE FILE IS ATTEMPTED TO BE IDENTIFIED IS:
    ! 1) READ SINGLE UNIT NUMBER
    ! 2) CHECK FOR OPTIONAL KEYWORD "BINARY" --ADDED TO OPEN FILE
    ! 4) CHECK FOR KEYWORD INTERNAL IF FOUND THEN RETURNS IU=0
    ! 5) CHECK FOR KEYWORD EXTERNAL FOLLOWED BY UNIT NUMBER
    ! 6) CHECK FOR KEYWORD OPEN/CLOSE AND THEN OPENS FILE SPECIFIED
    ! 7) CHEKS IF THE LINE CONTAINS A FILE THAT CAN BE OPENED (SAME AS IF OPEN/CLOSE WAS NOT PRESENT)
    !
    ! LLOC   is the starting location of the line to look for KEYWORD/NAME
    ! LN     is the line to process the KEYWORDS/UNIT/NAME
    ! IOUT   is where to write error messages too
    ! IN     is the input file that LN originated from
    ! NOSTOP optional, when present and is true will prevent the program from stopping if the file fails to open. ERROR will be set to TRUE
    ! REQKEY optional, when present and is true indicates that a keyword is required to open file (viz. no reading a single number or just a file name)
    ! IU     optional, when present is the unit number used when a file is opened by OPEN/CLOSE or by NAME
    ! BINARY optional, when present is sets file to be opened as a binary file or formatted.
    ! BUFFER optional, when present is sets the buffer size in KB. --131072 = 128KB is the default and 1048576 = 1MB  --BUFFER USES TWO THREADS SO ACTUAL BUFFER IS TWICE THE VALUE (eg. 256KB)
    ! NOSFAC optional, when present and set to true indicates that file does not support scale factors
    ! 
    CLASS(GENERIC_INPUT_FILE),  INTENT(INOUT):: FL
    CHARACTER(*),               INTENT(IN   ):: LN
    INTEGER,      OPTIONAL,     INTENT(INOUT):: LLOC
    INTEGER,      OPTIONAL,     INTENT(IN   ):: OUTPUT, INFILE ! Output file to write error msg too, infile to report error from.
    LOGICAL,      OPTIONAL,     INTENT(IN   ):: NOSTOP
    LOGICAL,      OPTIONAL,     INTENT(IN   ):: REQKEY
    INTEGER,      OPTIONAL,     INTENT(IN   ):: IU
    LOGICAL,      OPTIONAL,     INTENT(IN   ):: BINARY
    INTEGER,      OPTIONAL,     INTENT(IN   ):: BUFFER
    INTEGER,      OPTIONAL,     INTENT(  OUT):: EOL            ! Max line length for file
    LOGICAL,      OPTIONAL,     INTENT(  OUT):: NOSFAC
    LOGICAL,      OPTIONAL,     INTENT(IN   ):: NO_WARN         ! If true, warnings are supressed
    LOGICAL,      OPTIONAL,     INTENT(IN   ):: NO_INTERNAL
    LOGICAL,      OPTIONAL,     INTENT(IN   ):: NO_ONLY_UNIT
    LOGICAL,      OPTIONAL,     INTENT(IN   ):: NO_CONSTANT
    LOGICAL,      OPTIONAL,     INTENT(IN   ):: NEW_UNIT
    LOGICAL,      OPTIONAL,     INTENT(IN   ):: SAVE_FNAME
    CHARACTER(*), OPTIONAL,     INTENT(  OUT):: KEY            ! SHOULD BE CHARACTER(>10)
    INTEGER,      OPTIONAL,     INTENT(INOUT):: DIM
    INTEGER,      OPTIONAL,     INTENT(  OUT):: PREPOST        ! Holds location before running post-keycheck
    LOGICAL,      OPTIONAL,     INTENT(IN   ):: STREAM_TEXT
    LOGICAL,      OPTIONAL,     INTENT(IN   ):: NOPOSTKEY
    LOGICAL,      OPTIONAL,     INTENT(IN   ):: FORCE_DATAFILE
    CHARACTER(*), OPTIONAL,     INTENT(IN   ):: MSG
    !
    LOGICAL:: ISOPEN, ALLOW_ERROR, NOREQKEY, FOUND_KEY, DATAFILE
    LOGICAL:: GO_TO_TOP, SAVE_FN, CHECK_POST, CHECK_ONLY_UNIT, NOT_UNIQUE
    LOGICAL:: EXIST, REQ_NEW_UNIT
    !
    CHARACTER(12):: EXT, FORM_CHK, ACCESS_TXT
    CHARACTER(:), ALLOCATABLE:: FNAME, ERR_MSG
    !
    INTEGER:: I, IIN, LL, ISTART, ISTOP, LLOC_BAK
    INTEGER:: IERR, BUF, SPLIT, IU_READ
    !
    CALL RESET_GENERIC_INPUT_FILE(FL)
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
    IF(PRESENT(BINARY)) FL%BINARY = BINARY
    !
    BUF = 16384 ! 16KB x2 = 32KB  --1048576 = 1MB   --NOTE THAT TWO THREADS ARE USED PER BUFFER SO ACTUAL SPACE IS TWICE AS BIG
    IF(PRESENT(BUFFER)) BUF = BUFFER
    !
    ISOPEN = FALSE
    IF(PRESENT(STREAM_TEXT)) ISOPEN = STREAM_TEXT
    IF(ISOPEN) THEN
        ACCESS_TXT = 'STREAM'
    ELSE
        ACCESS_TXT = 'SEQUENTIAL'
    END IF
    !
    REQ_NEW_UNIT = FALSE
    IF(PRESENT(NEW_UNIT)) REQ_NEW_UNIT = NEW_UNIT
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
    ELSE IF (NOREQKEY .AND. CHECK_ONLY_UNIT .AND. LN(ISTART:ISTOP) /= BLNK) THEN
                      READ(LN(ISTART:ISTOP),*,IOSTAT=IERR) FL%IU
    ELSE
        IERR=69
    END IF
    !
    IF (IERR /= Z) THEN
          !
          IERR = Z
          !
          IF(EXT == 'BINARY') THEN
                                  FL%BINARY = TRUE
                                  CALL PARSE_WORD(LN,LL,ISTART,ISTOP)
                                  EXT = LN(ISTART:ISTOP)
                                  CALL UPPER(EXT)
          END IF
          !
          IF(REQ_NEW_UNIT) THEN
              IF(EXT == 'EXTERNAL' .OR. EXT =='DATAUNIT') EXT = 'NO_EXTERN'
          END IF
          !
          IF(EXT == 'INTERNAL') THEN
                                           FOUND_KEY = TRUE
                                           FL%IU = Z
                                           IF(CHECK_POST) CALL CHECK_FOR_POST_KEY(LL, LN, IIN, FL%IOUT, BUF, SPLIT, FL%SCALE, GO_TO_TOP=GO_TO_TOP, DIM=DIM, OLDLOC=PREPOST, NO_WARN=NO_WARN, MSG=MSG)
                                           !
                                           ! IF(IIN == Z) CALL ADD_MSG(ERR_MSG, 'Found INTERNAL directive keyword, but the code does not know what the internal unit number is.'//NL//'That is, the code is not setup to handel the INTERNAL directive'//NL//' This is either a code bug or there will be another message stating INTERNAL is not supported.')
                                           IF( PRESENT(NO_INTERNAL) ) THEN
                                                    IF(NO_INTERNAL) THEN
                                                        FL%ERROR = TRUE
                                                        CALL ADD_MSG(ERR_MSG, 'Found INTERNAL directive keyword, but this input data item does not allow it.'//NL//'Please change input to specify a separate file with the OPEN/CLOSE, DATAFILE, or EXTERNAL directives.')
                                                    END IF
                                           END IF
          ELSEIF(EXT == 'EXTERNAL' .OR. EXT =='DATAUNIT') THEN
                                           FOUND_KEY      = TRUE
                                           FL%IS_EXTERNAL = TRUE
                                           !CALL GET_INTEGER(LN,LL,ISTART,ISTOP,FL%IOUT,IIN,FL%IU,MSG='GENERIC_INPUT_FILE_INSTRUCTION ERROR: FROUND KEYWORD "'//TRIM(EXT)//'" WHICH SHOULD BE FOLLOWED BY AN INTEGER REPRESENTING THE UNIT NUMBER TO USE.')
                                           CALL GET_INTEGER(LN,LL,ISTART,ISTOP,FL%IOUT,IIN,FL%IU,HAS_ERROR=FL%ERROR)
                                           !
                                           IF(FL%ERROR) THEN
                                               FNAME = LN(ISTART:ISTOP)
                                               CALL DATAFILE_UNIT_NUMBER%CHECK_BASE(FNAME,FL%IU,NOT_UNIQUE)
                                               FL%ERROR = FL%IU /= Z .AND. NOT_UNIQUE                 !Found IU and its basename is Unique
                                           END IF
                                           IF(CHECK_POST) CALL CHECK_FOR_POST_KEY(LL, LN, IIN, FL%IOUT, BUF, SPLIT, FL%SCALE, GO_TO_TOP=GO_TO_TOP, DIM=DIM, OLDLOC=PREPOST, NO_WARN=NO_WARN, MSG=MSG)
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
          ELSEIF(EXT == 'SKIP' .OR. EXT =='NAN' .OR. EXT =='NULL' .OR. EXT =='NUL') THEN
                                           FOUND_KEY = TRUE
                                           FL%IU = Z
                                           FL%ERROR  = TRUE
                                           FL%SKIP   = TRUE
                                           IF(CHECK_POST) CALL CHECK_FOR_POST_KEY(LL, LN, IIN, FL%IOUT, BUF, SPLIT, FL%SCALE, DIM=DIM, OLDLOC=PREPOST, NO_WARN=NO_WARN, MSG=MSG)
          ELSEIF(EXT == 'CONSTANT' ) THEN
                                           FOUND_KEY      = TRUE
                                           FL%IS_CONSTANT = TRUE
                                           FL%IU = Z
                                           !
                                           IF(.NOT. ALLOCATED(FL%CONST)) ALLOCATE(FL%CONST)
                                           !
                                           IF(.NOT.PRESENT(KEY)) KEY = EXT
                                           !
                                           CALL GET_NUMBER(LN,LL,ISTART,ISTOP,FL%IOUT,IIN,FL%CONST,HAS_ERROR=FL%ERROR)
                                           !
                                           IF(CHECK_POST) CALL CHECK_FOR_POST_KEY(LL, LN, IIN, FL%IOUT, BUF, SPLIT, FL%SCALE, DIM=DIM, OLDLOC=PREPOST, NO_WARN=NO_WARN, MSG=MSG)
                                           !
                                           IF(FL%ERROR) THEN
                                               CALL ADD_MSG(ERR_MSG, &
                                                       'Found CONSTANT directive, but failed to sucessifully load the constant value (CONST). '//NL//  &   ! Temp use of vairable to hold error message
                                                       'The following is what found: "'//LN(ISTART:ISTOP)//'"'//BLN//                                  &
                                                       'An example input would be:'//NL//                                                              &
                                                       ' CONSTANT  2.5   # to use a constant value of 2.5 for input')
                                           END IF
                                           !
                                           IF(PRESENT(NO_CONSTANT)) THEN
                                                   IF(NO_CONSTANT) THEN
                                                       FL%ERROR = TRUE
                                                       CALL ADD_MSG(ERR_MSG, &
                                                              'Found CONSTANT directive, but this '//NL//                                               &
                                                              '   specific input that is opening a file for input reading'//NL//                 &
                                                              '   does not allow using the CONSTANT directive.'//BLN//                           &
                                                              'Or at least GENERIC_INPUT_FILE came across "CONSTANT"'//NL//                      &
                                                              'and it was not passed the fortran arguments to handel a constant keyword,'//NL//  &
                                                              'so you may have it in the wrong input location.')
                                                   END IF
                                           END IF
          ELSE
                IF (EXT == 'OPEN/CLOSE' .OR. EXT == 'DATAFILE' .or. EXT == 'OPENCLOSE') THEN  !OPEN/CLOSE FILE
                                           FOUND_KEY = TRUE
                                           !
                                           DATAFILE  = EXT == 'DATAFILE'
                                           IF(DATAFILE .AND. REQ_NEW_UNIT) DATAFILE = FALSE
                                           !
                                           !IF(DATAFILE .AND. PRESENT(NO_DATAFILE)) DATAFILE  = .NOT. NO_DATAFILE
                                           !
                                           CALL PARSE_WORD(LN,LL,ISTART,ISTOP)   !MOVE TO NEXT WORD WHICH IS THE FILE NAME
                                           FL%OPENCLOSE = TRUE
                ELSEIF(EXT == 'NO_EXTERN') THEN
                                           FOUND_KEY    = TRUE  ! OPEN A CLONE OF FILE --EXT == 'NO_EXTERN' IF EXT == EXTERNAL AND REQ_NEW_UNIT = TRUE
                                           FL%OPENCLOSE = TRUE
                ELSEIF(NOREQKEY) THEN
                                           FL%OPENCLOSE = TRUE
                                           EXT='IMPLIED_FILE'
                ELSE
                                           EXT='IMPLIED_LINE' ! Failed to open a file, set flag to indicate that input is either bad or located along line (Implied Internal)
                END IF
                !
                IF(PRESENT(FORCE_DATAFILE) .AND. FL%OPENCLOSE .AND. .NOT. DATAFILE .AND. .NOT. REQ_NEW_UNIT) DATAFILE = FORCE_DATAFILE
                !
                IF(ISTART > ISTOP) THEN
                    ALLOCATE( FNAME, SOURCE = BLNK )
                    I      = Z
                    EXIST  = FALSE
                    ISOPEN = FALSE
                    CALL ADD_MSG(ERR_MSG, &
                            'Found '//TRIM(EXT)//' directive,'//NL// &
                            '  which should be followed by a file name to open,'//NL// &
                            '  but nothing was found after it.')
                ELSEIF(EXT == 'NO_EXTERN') THEN
                    !
                    CALL GET_INTEGER(LN,LL,ISTART,ISTOP,FL%IOUT,IIN,I,MSG='GENERIC_INPUT_FILE_INSTRUCTION ERROR: FROUND KEYWORD "EXTERNAL" OR "DATAUNIT" WHICH SHOULD BE FOLLOWED BY AN INTEGER REPRESENTING THE UNIT NUMBER TO USE.')
                    !                 
                    CALL GET_FILE_NAME(I,FNAME,EXIST,FL%IOUT,IIN,HAS_ERROR=FL%ERROR)
                    IF(FL%ERROR) THEN
                        CALL ADD_MSG(ERR_MSG, &
                                'Found EXTERNAL OR DATAUNIT directive,'//NL//                        &
                                '   but failed to identify an open file associated with the unit number: '//NUM2STR(FL%IU)//NL// &
                                'This probably is because it was either closed at some point,'//NL// &
                                '   or never sucessfully opened.')
                    END IF
                    I = Z
                    ISOPEN = FALSE
                ELSE
                    ALLOCATE( FNAME, SOURCE = LN(ISTART:ISTOP) )
                    !
                    INQUIRE(FILE=FNAME, NUMBER=I, OPENED=ISOPEN, EXIST=EXIST)
                    !
                    IF(.not. EXIST) THEN
                        CALL ADD_MSG(ERR_MSG, &
                                'Found '//TRIM(EXT)//' directive,'//NL//  &
                                '  but the file specified to open:'//NL// &
                                '     "'//FNAME//'"'//NL//                &
                                '  was not found.'//NL//                  &
                                'Please check to see if the path and file name are correct.'//NL// &
                                'If you are are using a relative path to specify the file (such as: ../dir/file.txt),'//NL// &
                                '  then the current working director (point for  relative paths) is:'//NL// &
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
                    END IF
                    !
                    ! OVERKILL CHECK
                    !IF(DATAFILE .AND. .NOT.ISOPEN .AND. EXIST)  CALL DATAFILE_UNIT_NUMBER%CHECK_NAME(FNAME,I,ISOPEN)
                    !
                END IF
                !
                IF(EXIST) THEN
                    !
                    IF(CHECK_POST) CALL CHECK_FOR_POST_KEY(LL, LN, IIN, FL%IOUT, BUF, SPLIT, FL%SCALE, BINARY=FL%BINARY, GO_TO_TOP=GO_TO_TOP, DIM=DIM, OLDLOC=PREPOST, NO_WARN=NO_WARN, MSG=MSG) !FL%BINARY ONLY SET TO TRUE IF BINARY FLAG FOUND, OTHERWISE IGNORED
                    !
                    IF(ISOPEN .AND. PRESENT(IU)) THEN; IF (I /= IU) ISOPEN = FALSE
                    END IF
                    !
                    IF( DATAFILE .AND. ISOPEN) THEN
                                               FL%IU = I
                                               FL%OPENCLOSE = FALSE
                                               FL%IS_BOM    = DATAFILE_UNIT_NUMBER%UNIT_IS_BOM(FL%IU)
                    ELSEIF(FL%OPENCLOSE) THEN
                                               FL%IU = Z
                                               IF(PRESENT(IU)) FL%IU = IU
                                               !
                                               IF(FL%BINARY) THEN
                                                    CALL GENERIC_OPEN(FNAME, FL%IU, FL%IOUT, ACTION='READ', FORM='UNFORMATTED', ACCESS='STREAM',   STATUS='OLD', ASYNC='NO', BUFFER_BLOCKSIZE=BUF, BUFFER_COUNT=2, LINE=LN, INFILE=IIN, ERROR=FL%ERROR, IS_BOM=FL%IS_BOM)
                                               ELSE
                                                    CALL GENERIC_OPEN(FNAME, FL%IU, FL%IOUT, ACTION='READ', FORM=  'FORMATTED', ACCESS=ACCESS_TXT, STATUS='OLD', ASYNC='NO', BUFFER_BLOCKSIZE=BUF, BUFFER_COUNT=2, LINE=LN, INFILE=IIN, ERROR=FL%ERROR, IS_BOM=FL%IS_BOM)
                                               END IF
                                               !
                                               IF     (FL%ERROR) THEN
                                                                     FL%OPENCLOSE = FALSE
                                                                     CALL ADD_MSG(ERR_MSG, &
                                                                             'Found '//TRIM(EXT)//' directive,'//NL//  &
                                                                             '  but failed to open the file:'//NL// &
                                                                             '     "'//FNAME//'"'//NL//                &
                                                                             'The reason for the failure is unknown.'//NL//                  &
                                                                             'The path to the file seems to be correct.'//NL// &
                                                                             '  That is, the file was found, but failed to establish a connection and open the file.'//NL// &
                                                                             '    (Perhaps the file is locked by another program?)')
                                                                     IF(EXT == 'IMPLIED_FILE') &
                                                                         CALL ADD_MSG(ERR_MSG, &
                                                                             'The IMPLIED_FILE directive is a place holder for when no directive is found.'//NL//  &
                                                                             'The given input line assumes that it contains a file name that should be opened.'//NL//  &
                                                                             'That is: "path/to/myFile.txt" is marked as an IMPLIED_FILE and'//NL//  &
                                                                             '         treated as equivalent to "OPEN/CLOSE  path/to/myFile.txt"')
                                               ELSEIF (DATAFILE) THEN
                                                                     FL%OPENCLOSE = FALSE
                                                                     CALL DATAFILE_UNIT_NUMBER%ADD(FL%IU, FL%IS_BOM)
                                               END IF
                    ELSE
                                               FL%ERROR = TRUE  ! Did not find OPEN/CLOSE keyword and REQKEY=TRUE - Error message written later if not "IMPLIED_LINE"
                    END IF
                ELSE
                                               FL%IU    = Z
                                               FL%ERROR = TRUE  !FILE DOES NOT EXIST, MAYBE IMPLIED INTERNAL
                END IF
          END IF
    ELSE  !FOUND UNIT NUMBER--CHECK FOR POST KEYS
          EXT ='IMPLIED_UNIT'
          FL%IS_EXTERNAL = TRUE
          IF(CHECK_POST) CALL CHECK_FOR_POST_KEY(LL, LN, IIN, FL%IOUT, BUF, SPLIT, FL%SCALE, GO_TO_TOP=GO_TO_TOP, DIM=DIM, OLDLOC=PREPOST, NO_WARN=NO_WARN, MSG=MSG)
    END IF
    !
    IF(.NOT. FL%ERROR) THEN
                           IF( FL%IU /= Z) THEN
                                                 INQUIRE(FL%IU,FORM=FORM_CHK, ACCESS=ACCESS_TXT, OPENED=ISOPEN)
                                                 FL%BINARY = FORM_CHK /= 'FORMATTED'
                                                 FL%STREAM = ACCESS_TXT == 'STREAM'
                           ELSE
                                                 FL%BINARY = FALSE
                                                 FL%STREAM = FALSE
                                                 ISOPEN    = TRUE !ASSUMED INTERNAL FILE SO IT IS ALREADY OPEN
                           END IF
                           !
                           IF(.NOT. ISOPEN) FL%ERROR = TRUE !FILE IS NOT INTERNAL, NOR UNIT WAS ASSOCIATED WITH A FILE, NOR WAS IT SUCCESFULLY OPENED.
    END IF
    !
    IF(ALLOW_ERROR .AND. FL%IU /= Z) THEN
      IF( EXT == 'EXTERNAL' .or. EXT == 'DATAUNIT' .or. EXT == 'IMPLIED_UNIT' ) THEN
        I = FL%IU
        BLOCK 
            CHARACTER(12):: ACTION_TXT
            INQUIRE(UNIT=I, ACTION=ACTION_TXT, OPENED=ISOPEN)
            IF( ACTION_TXT /= 'READ' .and. ACTION_TXT /= 'READWRITE' ) ISOPEN = FALSE
        END BLOCK
        !
        IF(.NOT. ISOPEN) FL%ERROR = TRUE ! Unit associated with no file or not opened with correct permissions
        !
        IF(FL%ERROR) THEN
           CALL ADD_MSG(ERR_MSG, &
                   'Found '//TRIM(EXT)//' directive,'//NL//                         &
                   '  but the unit number specified, '//NUM2STR(FL%IU)//','//NL//   &
                   ' is not associated with an opened file or'//NL//                &
                   ' is not associated with a file opened with a "READ" permission.')
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
    ! ERROR OCCURED AND EITHER STOPPING IS ALLOWED OR THAT A KEYWORD WAS FOUND SO IT CAN NOT BE AN IMPLIED INTERNAL
    IF (FL%ERROR .AND. (ALLOW_ERROR .OR. FOUND_KEY) .AND. .NOT. FL%SKIP) THEN
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
      CALL HED_MSG(ERR_MSG,'FAILED TO OPEN FILE WITH GENERIC_INPUT_FILE.')
      !
      CALL FILE_IO_ERROR(LINE=LN, INFILE=IIN, OUTPUT=FL%IOUT, MSG=ERR_MSG, MSG2=MSG)
      !
    END IF
    !
    IF(PRESENT(NO_INTERNAL) .AND. FL%IU == Z .AND. .NOT. FL%ERROR) THEN
            !
            IF(NO_INTERNAL) FL%ERROR = TRUE
            !
            IF(NO_INTERNAL .AND. ALLOW_ERROR) THEN
                  !
                  IF( EXT=='INTERNAL') THEN
                      ERR_MSG = 'GENERIC_INPUT_FILE ERROR: FOUND KEYWORD "INTERNAL" BUT THIS INPUT DATA ITEM DOES NOT ALLOW FOR INTERNAL KEYWORD.'//BLN//'PLEASE MOVE INPUT TO SEPARATE FILE AND USE EITHER OPEN/CLOSE OR EXTERNAL.'
                      IF(PRESENT(MSG))  ERR_MSG = ERR_MSG//BLN//'THE FOLLOWING MESSAGE WAS PASSED TO GENERIC_INPUT:'//BLN//MSG
                      !
                      CALL FILE_IO_ERROR(Z,IIN,LINE=LN,OUTPUT=FL%IOUT,MSG=ERR_MSG)
                  END IF
                  !
                  IF( FL%IU == Z     ) THEN
                      ERR_MSG = 'GENERIC_INPUT_FILE ERROR: THIS INPUT DATA ITEM DOES NOT ALLOW IMPLIED INTERNAL LOADING (LOADING ON SAME LINE).'//NL//'EITHER THIS WAS WHAT WAS ATTEMPED AND NOT ALLOWED OR PROGRAM FAILED TO IDENTIFY KEYWORD USED (OPEN/CLOSE, EXTERNAL, CONSTANT, REPEAT, SKIP), OPEN THE SPECIFIED FILE, OR IDENTIFY UNIT NUMBER DECLARED.'//BLN//'PLEASE CHECK INPUT.'
                      IF(PRESENT(MSG))  ERR_MSG = ERR_MSG//BLN//'THE FOLLOWING MESSAGE WAS PASSED TO GENERIC_INPUT:'//BLN//MSG
                      !
                      CALL FILE_IO_ERROR(Z,IIN,LINE=LN,OUTPUT=FL%IOUT,MSG=ERR_MSG)
                  END IF
            END IF
    END IF
    !
    IF(PRESENT(NOSFAC) .AND. ALLOW_ERROR) THEN
            IF(NOSFAC  .AND. FL%SCALE /= UNO) THEN
                ERR_MSG = 'GENERIC_INPUT_FILE_INSTRUCTION FOUND KEYWORD "SF" OR "SCALE",'//NL//'BUT THIS MODEL FEATURE DOES NOT ALLOW SCALE FACTORS.'//NL//'PLEASE REMOVE KEYWORD SF OR SCALE'//NL//'("SCALE" IS A NUMBER LOCATED TO THE RIGHT OF THE FILE NAME THAT IS LOADED AS A SCALE FACTOR, PLEASE REMOVE OR PLACE A # TO COMMENT IT OUT).'
                IF(PRESENT(MSG))  ERR_MSG = ERR_MSG//BLN//'THE FOLLOWING MESSAGE WAS PASSED TO GENERIC_INPUT:'//BLN//MSG
                CALL FILE_IO_ERROR(IERR,IIN,LINE=LN,OUTPUT=FL%IOUT,MSG=ERR_MSG)
            END IF
    END IF
    !
    IF(PRESENT(EOL)) THEN
         IF (.NOT. FL%ERROR .AND. .NOT. FL%BINARY .AND. FL%IU /= Z) THEN
             EOL = MAX_LINE_LENGTH(FL%IU)
         ELSE
             EOL = LEN(LN)
         END IF
    END IF
    !
    IF (FL%ERROR .AND. .NOT. FL%SKIP) THEN
                       FL%IU = Z
                       LL = LLOC_BAK
    END IF
    !
    IF(PRESENT(KEY)) THEN
          IF(FOUND_KEY) THEN
                             KEY = ADJUSTL(EXT)
          ELSE
                             KEY = 'NOKEY'
          END IF
    END IF
    !
    IF(GO_TO_TOP .AND. FL%IU /= Z) THEN
                       IF(FL%IS_EXTERNAL) THEN
                                       CALL UTF8_BOM_OFFSET_REWIND(FL%IU, FL%IS_BOM)  !EXTERNAL and DATAUNIT have no clue if file is UTF8 or UTF8_BOM - This rewinds appropiately
                       ELSE
                                       CALL REWIND_GENERIC_INPUT_FILE(FL)   !File includes flag for IS_BOM
                       END IF
    END IF
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
       ELSEIF(FL%IU /= Z) THEN
                 CALL SET_FILE_NAME_GENERIC_INPUT_FILE(FL)
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
  SUBROUTINE READ_GENERIC_INPUT_FILE_LINE(FL, LINE, CNT, EOL, EOF, NOSHIFT, READ_COM)
    CLASS(GENERIC_INPUT_FILE), INTENT(IN ):: FL
    CHARACTER(*),              INTENT(OUT):: LINE
    INTEGER,OPTIONAL,          INTENT(OUT):: CNT     !RETURNS A COUNT OF HOW MANY LINES WERE LOADED
    INTEGER,OPTIONAL,          INTENT(OUT):: EOL     !LOCATIONS OF WHERE THE END OF LINE IS OR ONE SPACE BEFORE #
    LOGICAL,OPTIONAL,          INTENT(OUT):: EOF     !SET TO TRUE IF END OF FILE IS REACHED
    LOGICAL,OPTIONAL,          INTENT(IN ):: NOSHIFT !SET TO TRUE TO NOT TO USE ADJUSTL
    LOGICAL,OPTIONAL,          INTENT(IN ):: READ_COM !IF TRUE, THEN COMMENTED LINES ARE NOT BYPASSED (default is False)
    INTEGER:: IERR
    LOGICAL:: EEOF
    !
    IERR = Z
    IF(PRESENT(READ_COM)) THEN; IF(READ_COM) IERR = ONE
    END IF
    !
    IF(FL%BINARY) THEN
        READ(FL%IU, IOSTAT=IERR) LINE
        IF    (IERR > Z) CALL FILE_IO_ERROR(IERR,FL%IU,LINE=LINE,INFILE=FL%IU,OUTPUT=FL%IOUT, MSG='ERROR WHILE READING A GENERIC_INPUT FILE')
        EEOF = IERR < Z
        IF(PRESENT(CNT)) CNT = ONE
        IF(PRESENT(EOL)) EOL = LEN(LINE)
    ELSEIF(IERR==Z) THEN
        CALL READ_TO_DATA(LINE,FL%IU,FL%IOUT,Z,CNT,EOL,EEOF,NOSHIFT)
    ELSE
          READ(FL%IU,'(A)',IOSTAT=IERR) LINE
          IF    (IERR > Z) THEN                                   !LINE FAILED TO READ, THIS IS MOST LIKELY DUE TO END OF FILE LINE,INFILE,OUTPUT,MSG
                               CALL FILE_IO_ERROR(IERR,FL%IU,LINE=LINE,INFILE=FL%IU,OUTPUT=FL%IOUT, MSG='ERROR WHILE READING A GENERIC_INPUT FILE')
          ELSEIF(IERR < Z) THEN !EOF
                               LINE=BLNK
                               EEOF=TRUE
                               BACKSPACE(FL%IU) !NOTE THAT EOF COUNTS OF 1 READ, BUT MULTIPLE READS TO EOF WILL NOT MOVE ANY FURTHER, SO REPOSITION TO THE END OF THE FILE, BUT NOT ONE BEYOND TO KEEP N (THE READ COUNT) CORRET 
          ELSE
                               CALL SPECIAL_BLANK_STRIP(LINE)   !FORTRAN TREATES TAB AS IF IT WAS CHARACTER--MAKE SPACES TO IMPROVE SEARCH
                               !
                               EEOF = TRUE
                               IF(PRESENT(NOSHIFT)) EEOF = .NOT. NOSHIFT
                               IF(LINE(1:1) == BLNK .AND. EEOF) LINE = ADJUSTL(LINE)
                               !
                               EEOF=FALSE
          END IF
          IF(PRESENT(CNT)) CNT = ONE
          IF(PRESENT(EOL)) EOL = COMMENT_INDEX(LINE)
          !
    END IF
    !
    IF(PRESENT(EOF)) EOF = EEOF
    !
  END SUBROUTINE
  !
  SUBROUTINE READ_GENERIC_INPUT_FILE_VECTOR(FL, VEC)
    CLASS(GENERIC_INPUT_FILE), INTENT(INOUT):: FL
    REAL(REAL64),DIMENSION(:), INTENT(OUT  ):: VEC
    INTEGER:: IERR
    CHARACTER(10):: LN
    !
    IF(FL%BINARY) THEN
        READ(FL%IU, IOSTAT=IERR) VEC
    ELSE 
        CALL READ_TO_DATA(LN,FL%IU,FL%IOUT)
        BACKSPACE(FL%IU)
        READ(FL%IU, *, IOSTAT=IERR) VEC
    END IF
    FL%ERROR = IERR /= Z
    !
  END SUBROUTINE
  !
  SUBROUTINE READ_GENERIC_INPUT_FILE_ARRAY(FL, ARR)
    CLASS(GENERIC_INPUT_FILE),      INTENT(INOUT):: FL
    REAL(REAL64),   DIMENSION(:,:), INTENT(  OUT):: ARR
    INTEGER:: IERR, I
    CHARACTER(10):: LN
    !
    IF(FL%BINARY) THEN
        READ(FL%IU, IOSTAT=IERR) ARR
    ELSE
        CALL READ_TO_DATA(LN,FL%IU,FL%IOUT)
        BACKSPACE(FL%IU)
        DO I=ONE, UBOUND(ARR,2)
            READ(FL%IU, *, IOSTAT=IERR) ARR(:,I)
            IF(IERR /= Z) EXIT
        END DO
        !
    END IF
    FL%ERROR = IERR /= Z
    !
  END SUBROUTINE
  !
  IMPURE ELEMENTAL SUBROUTINE REWIND_GENERIC_INPUT_FILE(FL)
    CLASS(GENERIC_INPUT_FILE), INTENT(IN):: FL
    !
    IF(FL%IU /= Z) THEN
        !
        IF(FL%BINARY) THEN
            !
            REWIND(FL%IU)
            !
        ELSEIF(FL%IS_EXTERNAL) THEN  !fix for -> Potential error is UTF8 BOM file that is opened in the Name File and calling REWIND_GENERIC_INPUT_FILE(FL) outside of the OPEN_GENERIC_INPUT_FILE routiune
            !
            CALL UTF8_BOM_OFFSET_REWIND(FL%IU)
            !
        ELSE
            REWIND(FL%IU)
            !
            IF(FL%IS_BOM) THEN
                          BLOCK
                              CHARACTER(THREE):: BOM
                              INTEGER:: IERR
                              READ(FL%IU, '(A)', ADVANCE='NO', IOSTAT=IERR) BOM
                          END BLOCK
            END IF
        END IF
    END IF
    !
  END SUBROUTINE
  !
  IMPURE ELEMENTAL SUBROUTINE BACKSPACE_GENERIC_INPUT_FILE(FL,N)
    CLASS(GENERIC_INPUT_FILE), INTENT(INOUT):: FL
    INTEGER, OPTIONAL,         INTENT(IN   ):: N
    INTEGER:: I
    !
    IF(FL%IU /= Z) THEN
        IF(PRESENT(N)) THEN
            DO I=1, N
                  BACKSPACE(FL%IU) ! Potential error is UTF8 BOM file that is opened in the Name File and calling REWIND_GENERIC_INPUT_FILE(FL) outside of the OPEN_GENERIC_INPUT_FILE routiune
            END DO
        ELSE
                  BACKSPACE(FL%IU)
        END IF
    END IF
    !
  END SUBROUTINE
  !
  IMPURE ELEMENTAL SUBROUTINE SET_FILE_NAME_GENERIC_INPUT_FILE(FL)
    CLASS(GENERIC_INPUT_FILE), INTENT(INOUT):: FL
    LOGICAL:: EXIST
    !
    IF(FL%IU /= Z) THEN
        IF(.NOT. ALLOCATED(FL%FNAME)) THEN
              CALL GET_FILE_NAME(FL%IU,FL%FNAME,EXIST,FL%IOUT,Z,MSG='GENERIC_INPUT_FILE_INSTRUCTION ERROR: FROUND KEYWORD "EXTERNAL" OR "DATAUNIT", BUT FAILED TO IDENTIFY THE FILE (IN PARTICULAR ITS NAME) THAT IS ASSOCAITED WITH IT.')
        END IF
    ELSEIF(ALLOCATED(FL%FNAME)) THEN
        DEALLOCATE(FL%FNAME)
    END IF
    !
  END SUBROUTINE
  !
  SUBROUTINE COUNT_UNCOMMENTED_LINES_GENERIC_INPUT_FILE(FL, CNT)
    CLASS(GENERIC_INPUT_FILE), INTENT(INOUT):: FL
    INTEGER,                   INTENT(OUT  ):: CNT
    LOGICAL:: EOF
    CHARACTER(120):: LN
    !
    CNT = Z
    IF    (FL%IS_CONSTANT         ) THEN; CONTINUE
    ELSEIF(FL%BINARY .OR. FL%IU==Z) THEN
        CNT = NEG
        FL%ERROR = TRUE
    ELSE
        CALL REWIND_GENERIC_INPUT_FILE(FL)
        DO
           CALL READ_TO_DATA(LN,FL%IU,EOF=EOF)
           IF (EOF) EXIT
           CNT = CNT + ONE
        END DO
        CALL REWIND_GENERIC_INPUT_FILE(FL)
    END IF
    !
  END SUBROUTINE
  !
  FUNCTION GET_FILE_POSITION(FL) RESULT(POS)
    CLASS(GENERIC_INPUT_FILE), INTENT(IN):: FL
    INTEGER:: POS
    !
    IF(FL%IU /= Z .AND. FL%STREAM) THEN
        INQUIRE(FL%IU, POS=POS)
    ELSE
        POS = ONE
    END IF
    !
  END FUNCTION
  !
  SUBROUTINE GOTO_FILE_POSITION(FL,POS)
    CLASS(GENERIC_INPUT_FILE), INTENT(INOUT):: FL
    INTEGER,                  INTENT(IN   ):: POS
    !
    IF(FL%IU /= Z .AND. FL%STREAM) THEN
        READ(FL%IU,'(A)', POS=POS, ADVANCE='NO')
    ELSEIF(FL%IU /= Z) THEN
        CALL REWIND_GENERIC_INPUT_FILE(FL)
    END IF
    !
  END SUBROUTINE
  !
  PURE ELEMENTAL SUBROUTINE MOVE_GENERIC_INPUT_FILE(FL,FL_NEW)
    CLASS(GENERIC_INPUT_FILE), INTENT(INOUT):: FL
    CLASS(GENERIC_INPUT_FILE), INTENT(INOUT):: FL_NEW
    !
    FL_NEW%IU        = FL%IU 
    FL_NEW%IOUT      = FL%IOUT
    FL_NEW%OPENCLOSE = FL%OPENCLOSE
    FL_NEW%ERROR     = FL%ERROR
    FL_NEW%BINARY    = FL%BINARY
    FL_NEW%SKIP      = FL%SKIP
    FL_NEW%SCALE     = FL%SCALE
    FL_NEW%IS_CONSTANT = FL%IS_CONSTANT
    FL_NEW%IS_BOM      = FL%IS_BOM      
    FL_NEW%IS_EXTERNAL = FL%IS_EXTERNAL 
    IF(ALLOCATED(FL%FNAME)) THEN
        CALL MOVE_ALLOC(FL%FNAME,FL_NEW%FNAME)
    END IF
    !
    FL%OPENCLOSE = FALSE
    !
  END SUBROUTINE
  !
  PURE ELEMENTAL SUBROUTINE COPY_GENERIC_INPUT_FILE(FL_COPY,FL)
    CLASS(GENERIC_INPUT_FILE), INTENT(INOUT):: FL_COPY
    CLASS(GENERIC_INPUT_FILE), INTENT(IN):: FL
    !
    FL_COPY%IU        = FL%IU 
    FL_COPY%IOUT      = FL%IOUT
    FL_COPY%OPENCLOSE = FL%OPENCLOSE
    FL_COPY%ERROR     = FL%ERROR
    FL_COPY%BINARY    = FL%BINARY
    FL_COPY%SKIP      = FL%SKIP
    FL_COPY%SCALE     = FL%SCALE
    FL_COPY%IS_CONSTANT = FL%IS_CONSTANT
    FL_COPY%IS_BOM      = FL%IS_BOM      
    FL_COPY%IS_EXTERNAL = FL%IS_EXTERNAL 
    IF(ALLOCATED(FL%FNAME)) THEN
        ALLOCATE(FL_COPY%FNAME, SOURCE=FL%FNAME)
    END IF
    !
    FL_COPY%OPENCLOSE = FL%OPENCLOSE
    !
  END SUBROUTINE
  !
  IMPURE ELEMENTAL SUBROUTINE CLOSE_GENERIC_INPUT_FILE(FL)
    CLASS(GENERIC_INPUT_FILE), INTENT(INOUT):: FL
    INTEGER:: IERR
    !
    IF(FL%OPENCLOSE) CLOSE(FL%IU,IOSTAT=IERR)
    !
    IF(ALLOCATED(FL%FNAME)) DEALLOCATE(FL%FNAME)
    !
    FL%IU  = Z
    FL%OPENCLOSE  = FALSE
    FL%BINARY     = FALSE
    FL%SKIP       = FALSE
    FL%IS_CONSTANT= FALSE
    FL%IS_BOM     = FALSE 
    FL%IS_EXTERNAL= FALSE 
    FL%SCALE      = UNO
    !
  END SUBROUTINE
  !
  IMPURE ELEMENTAL SUBROUTINE NOCLOSE_GENERIC_INPUT_FILE(FL)
    CLASS(GENERIC_INPUT_FILE), INTENT(INOUT):: FL
    !
    FL%OPENCLOSE = FALSE
    !
  END SUBROUTINE
  !
  SUBROUTINE FINAL_CLOSE_GENERIC_INPUT_FILE(FL)
    TYPE(GENERIC_INPUT_FILE), INTENT(INOUT):: FL
    !
    CALL CLOSE_GENERIC_INPUT_FILE(FL)
    !
  END SUBROUTINE
  !
  !!!PURE SUBROUTINE GENERIC_INPUT_FILE_DEALLOCATE(FL)
  !!!  TYPE(GENERIC_INPUT_FILE),ALLOCATABLE,INTENT(INOUT):: FL
  !!!  INTEGER:: I
  !!!  !
  !!!  DEALLOCATE(FL, STAT=I)
  !!!  !
  !!!END SUBROUTINE
  !!!!
  !!!PURE SUBROUTINE GENERIC_INPUT_FILE_DEPOINT(FL)
  !!!  TYPE(GENERIC_INPUT_FILE),POINTER,INTENT(INOUT):: FL
  !!!  INTEGER:: I
  !!!  !
  !!!  DEALLOCATE(FL, STAT=I)
  !!!  !
  !!!END SUBROUTINE
  !
END MODULE
!
!