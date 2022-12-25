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
  !
  USE, INTRINSIC:: ISO_FORTRAN_ENV, ONLY: REAL32, REAL64, REAL128
  !
  USE CONSTANTS
  USE PARSE_WORD_INTERFACE,   ONLY: PARSE_WORD, GET_WORD
  USE NUM2STR_INTERFACE,      ONLY: NUM2STR
  USE POST_KEY_SUB,           ONLY: CHECK_FOR_POST_KEY
  USE ERROR_INTERFACE,        ONLY: FILE_IO_ERROR, STOP_ERROR, GET_WARN, WARNING_MESSAGE
  USE GENERIC_OPEN_INTERFACE, ONLY: GENERIC_OPEN, UNIT_IS_BOM, UTF8_BOM_OFFSET_REWIND
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
      LOGICAL:: IS_OPEN     = FALSE
      LOGICAL:: OPENCLOSE   = FALSE
      LOGICAL:: ERROR       = FALSE
      LOGICAL:: BINARY      = FALSE
      LOGICAL:: STREAM      = FALSE
      LOGICAL:: NULL_FILE   = FALSE
      LOGICAL:: IS_INTERNAL = FALSE
      LOGICAL:: IS_BOM      = FALSE
      LOGICAL:: IS_CONSTANT = FALSE
      !
      CHARACTER(:),ALLOCATABLE:: FNAME
      !
      REAL(REAL64):: SCALE = UNO
      !
      REAL(REAL64), ALLOCATABLE:: CONST
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
    FL%IS_OPEN     = FALSE
    FL%OPENCLOSE   = FALSE
    FL%ERROR       = FALSE
    FL%BINARY      = FALSE
    FL%STREAM      = FALSE
    FL%NULL_FILE   = FALSE
    FL%IS_INTERNAL = FALSE
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
  RECURSIVE SUBROUTINE OPEN_GENERIC_INPUT_FILE(FL, LN, LLOC, OUTPUT, INFILE, REQKEY, NOSTOP, KEY_FAIL_STOP, KEY, KEY_FOUND, FORCE_DATAFILE, ALLOW_ONLY_UNIT, NO_POSTKEY_CHECK, NO_WARN, NO_INTERNAL, NO_CONSTANT, NO_BINARY, SAVE_FNAME, STREAM_TEXT, IU, INTERNAL_IU, DIM, PREPOST, BINARY, BUFFER, REWIND, APPEND, NO_SCALE, EOL, NEW_UNIT, MSG)
    ! ATTEMPTS TO READ KEYWORDS AND OPEN AN EXISTING INTPUT FILE
    ! SETS ERROR=TRUE IF FILE FAILED TO IDENTIFY OR OPEN A FILE/UNIT.
    !
    ! THE ORDER THAT THE FILE IS ATTEMPTED TO BE IDENTIFIED IS:
    ! 1) Read single unit number (only if ALLOW_ONLY_UNIT=TRUE)
    ! 2) Check for optional keyword "binary" --added to open file
    ! 4) Check for keyword INTERNAL if found then returns FL%IU=0
    ! 5) Check for keyword EXTERNAL followed by unit number
    ! 6) Check for keyword open/close and then opens file specified
    ! 7) Cheks if the line contains a file that can be opened (same as if open/close was not present)
    !
    CLASS(GENERIC_INPUT_FILE),  INTENT(INOUT):: FL
    CHARACTER(*),               INTENT(IN   ):: LN              ! Optional Directive plus either file name or unit plus optional post keywords.
    INTEGER,      OPTIONAL,     INTENT(INOUT):: LLOC            ! Position to start parsing line, set to 1 if not present
    INTEGER,      OPTIONAL,     INTENT(IN   ):: OUTPUT, INFILE  ! Output file to write error msg too, infile to report error from.
    LOGICAL,      OPTIONAL,     INTENT(IN   ):: REQKEY          ! If true, then requires specifying a directive keyword (viz OPEN/CLOSE, EXTERNAL, DATAFILE, DATAUNIT, CONSTANT, NULL)
    LOGICAL,      OPTIONAL,     INTENT(IN   ):: NOSTOP          ! If true, then indicates the subroutine wil return if an error occurs, otherwise the program terminates with an error message
    LOGICAL,      OPTIONAL,     INTENT(IN   ):: KEY_FAIL_STOP   ! If true, then always raises an error if a directive keyword is found but there was an error perfomating what it requested, this supercedes NOSTOP=TRUE
    CHARACTER(*), OPTIONAL,     INTENT(INOUT):: KEY             ! Set to the directive that is used.
    LOGICAL,      OPTIONAL,     INTENT(INOUT):: KEY_FOUND       ! Set to true if a directive is found
    LOGICAL,      OPTIONAL,     INTENT(IN   ):: FORCE_DATAFILE  ! If true, then the file is always opened as a DATAFILE (ie its unit number does not close till program ends)
    LOGICAL,      OPTIONAL,     INTENT(IN   ):: ALLOW_ONLY_UNIT ! If true and REQKEY=FALSE, then the allows checking for a unite number without the EXTERNAL or DATAUNIT directives (ie, a single integer is not assumed to be a unit number)
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
    LOGICAL,      OPTIONAL,     INTENT(IN   ):: NO_SCALE        ! Raises error if scale factor is found
    INTEGER,      OPTIONAL,     INTENT(  OUT):: EOL             ! Max line length for file
    LOGICAL,      OPTIONAL,     INTENT(IN   ):: NEW_UNIT        ! If true, then a new unit number is forced even if opened with EXTERNAL unit.
    CHARACTER(*), OPTIONAL,     INTENT(IN   ):: MSG             ! Message to pass to stop routing if progarm terminates.
    !
    LOGICAL:: ISOPEN, ALLOW_ERROR, NOREQKEY, FOUND_KEY, DATAFILE
    LOGICAL:: GO_TO_TOP, SAVE_FN, CHECK_POST, CHECK_ONLY_UNIT, NOT_UNIQUE
    LOGICAL:: IS_EXTERNAL, NO_BINARY_FLAG
    LOGICAL:: EXIST, REQ_NEW_UNIT
    !
    CHARACTER(12):: EXT, FORM_CHK, ACCESS_TXT, POS_TXT
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
    POS_TXT = 'REWIND'
    IF(PRESENT(APPEND)) THEN
            IF(APPEND) POS_TXT = 'APPEND'
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
          IF(CHECK_POST) CALL CHECK_FOR_POST_KEY(LL, LN, IIN, FL%IOUT, BUF, SPLIT, FL%SCALE, BINARY=FL%BINARY, GO_TO_TOP=GO_TO_TOP, DIM=DIM, OLDLOC=PREPOST, NO_WARN=NO_WARN, MSG=MSG)
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
                                           IF(CHECK_POST) CALL CHECK_FOR_POST_KEY(LL, LN, IIN, FL%IOUT, BUF, SPLIT, FL%SCALE, BINARY=FL%BINARY, GO_TO_TOP=GO_TO_TOP, DIM=DIM, OLDLOC=PREPOST, NO_WARN=NO_WARN, MSG=MSG)
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
                                           ELSEIF(REQ_NEW_UNIT) THEN
                                               EXT = 'NO_EXTERN'
                                           END IF
                                           IF(ALLOCATED(FNAME)) DEALLOCATE(FNAME)
          END IF
          !
          IF(EXT == 'INTERNAL') THEN
                                           FOUND_KEY      = TRUE
                                           FL%IS_INTERNAL = TRUE
                                           FL%IU = IIN
                                           !
                                           IF( PRESENT(INTERNAL_IU) ) THEN
                                                    IF(INTERNAL_IU /= Z) FL%IU = INTERNAL_IU
                                           END IF
                                           !
                                           IF(CHECK_POST) CALL CHECK_FOR_POST_KEY(LL, LN, IIN, FL%IOUT, BUF, SPLIT, FL%SCALE, BINARY=FL%BINARY, GO_TO_TOP=GO_TO_TOP, DIM=DIM, OLDLOC=PREPOST, NO_WARN=NO_WARN, MSG=MSG)
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
          ELSEIF(EXT =='NULL' .OR. EXT =='NUL' .OR. EXT == 'SKIP') THEN
                                           FOUND_KEY = TRUE
                                           IF(CHECK_POST) CALL CHECK_FOR_POST_KEY(LL, LN, IIN, FL%IOUT, BUF, SPLIT, FL%SCALE, BINARY=FL%BINARY, DIM=DIM, OLDLOC=PREPOST, NO_WARN=NO_WARN, MSG=MSG)
                                           CALL FL%CLOSE()
                                           FL%NULL_FILE = TRUE
                                           FNAME = "NULL"
                                           !
                                           IF(PRESENT(LLOC)) LLOC = LL
                                           IF(PRESENT(KEY_FOUND)) KEY_FOUND = TRUE
                                           IF(PRESENT(KEY)) KEY = FNAME
                                           IF(SAVE_FN) FL%FNAME = FNAME
          ELSEIF(EXT == 'CONSTANT' ) THEN
                                           FOUND_KEY      = TRUE
                                           FL%IS_CONSTANT = TRUE
                                           FL%IU = Z
                                           !
                                           IF(.NOT. ALLOCATED(FL%CONST)) ALLOCATE(FL%CONST)
                                           !
                                           CALL GET_NUMBER(LN,LL,ISTART,ISTOP,FL%IOUT,IIN,FL%CONST,HAS_ERROR=FL%ERROR)
                                           !
                                           IF(CHECK_POST) CALL CHECK_FOR_POST_KEY(LL, LN, IIN, FL%IOUT, BUF, SPLIT, FL%SCALE, BINARY=FL%BINARY, DIM=DIM, OLDLOC=PREPOST, NO_WARN=NO_WARN, MSG=MSG)
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
          ELSEIF(.not. FL%ERROR) THEN
                IF (EXT == 'OPEN/CLOSE' .OR. EXT == 'OPENCLOSE' .OR. EXT == 'DATAFILE') THEN  !OPEN/CLOSE FILE
                                           FOUND_KEY    = TRUE
                                           FL%OPENCLOSE = TRUE
                                           !
                                           DATAFILE  = EXT == 'DATAFILE'
                                           IF(DATAFILE .AND. REQ_NEW_UNIT) DATAFILE = FALSE
                                           !
                                           CALL PARSE_WORD(LN,LL,ISTART,ISTOP)   ! MOVE TO NEXT WORD WHICH IS THE FILE NAME
                ELSEIF(EXT == 'NO_EXTERN') THEN
                                           FOUND_KEY    = TRUE  ! OPEN A CLONE OF FILE --EXT == 'NO_EXTERN' IF EXT == EXTERNAL AND REQ_NEW_UNIT = TRUE
                                           FL%OPENCLOSE = TRUE
                ELSEIF(NOREQKEY) THEN
                                           FL%OPENCLOSE = TRUE
                                           EXT='IMPLIED_FILE'
                ELSE
                                           EXT='IMPLIED_LINE' ! Failed to open a file, set flag to indicate that input is either bad or located along line (Implied Internal)
                                           FL%IU = Z
                                           FL%ERROR = TRUE
                END IF
                !
                IF(PRESENT(FORCE_DATAFILE) .AND. FL%OPENCLOSE .AND. .NOT. DATAFILE .AND. .NOT. REQ_NEW_UNIT) DATAFILE = FORCE_DATAFILE
                !
                IF(ISTART > ISTOP) THEN               ! Error parsing file name (can only be true if OPEN/CLOSE does not have a file after it)
                    FL%ERROR = TRUE
                    CALL ADD_MSG(ERR_MSG, &
                            'Found '//TRIM(EXT)//' directive,'//NL// &
                            '  which should be followed by a file name to open,'//NL// &
                            '  but nothing was found after it.')
                ELSEIF(EXT == 'NO_EXTERN') THEN  ! Found EXTERNAL but requires new unit
                    !
                    I = FL%IU
                    CALL GET_FILE_NAME(I,FNAME,EXIST,FL%IOUT,IIN,HAS_ERROR=FL%ERROR)
                    IF(FL%ERROR) THEN
                        CALL ADD_MSG(ERR_MSG, &
                                'Found EXTERNAL OR DATAUNIT directive,'//NL//                        &
                                '   but failed to identify an open file associated with the unit number: '//NUM2STR(FL%IU)//NL// &
                                'This probably is because it was either closed at some point,'//NL// &
                                '   or never sucessfully opened.')
                    END IF
                    FL%IU  = Z
                    I      = Z
                    ISOPEN = FALSE
                ELSE IF ( .not. FL%ERROR) THEN
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
                    IF(.not. ISOPEN .and. DATAFILE) THEN  ! Check for BaseName
                       CALL DATAFILE_UNIT_NUMBER%CHECK_BASE(FNAME,I,NOT_UNIQUE)
                       ISOPEN = I /= Z .and. .not. NOT_UNIQUE                 ! Found IU and its basename is Unique
                    END IF
                    !
                END IF
                !
                IF(FL%ERROR .or. .not. EXIST) THEN
                    FL%IU    = Z
                    FL%ERROR = TRUE
                ELSE
                    IF(CHECK_POST) CALL CHECK_FOR_POST_KEY(LL, LN, IIN, FL%IOUT, BUF, SPLIT, FL%SCALE, BINARY=FL%BINARY, GO_TO_TOP=GO_TO_TOP, DIM=DIM, OLDLOC=PREPOST, NO_WARN=NO_WARN, MSG=MSG) !FL%BINARY ONLY SET TO TRUE IF BINARY FLAG FOUND, OTHERWISE IGNORED
                    !
                    IF(ISOPEN .AND. PRESENT(IU)) THEN
                                    IF (I /= IU .and. IU /= Z) ISOPEN = FALSE
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
                                                    CALL GENERIC_OPEN(FNAME, FL%IU, FL%IOUT, ACTION='READ', FORM='UNFORMATTED', ACCESS='STREAM',   POSITION=POS_TXT, STATUS='OLD', ASYNC='NO', BUFFER_BLOCKSIZE=BUF, BUFFER_COUNT=2, LINE=LN, INFILE=IIN, ERROR=FL%ERROR, IS_BOM=FL%IS_BOM)
                                               ELSE
                                                    CALL GENERIC_OPEN(FNAME, FL%IU, FL%IOUT, ACTION='READ', FORM=  'FORMATTED', ACCESS=ACCESS_TXT, POSITION=POS_TXT, STATUS='OLD', ASYNC='NO', BUFFER_BLOCKSIZE=BUF, BUFFER_COUNT=2, LINE=LN, INFILE=IIN, ERROR=FL%ERROR, IS_BOM=FL%IS_BOM)
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
              CALL ADD_MSG(ERR_MSG, 'Possible Code issues because OPEN method was told to use a specific unit number, "'//NUM2STR(IU)//'", but the input parced specified a file that is already opened under a differnet unit number "'//NUM2STR(FL%IU)//'"'//NL//'That is, the file was opened or in use somewhere else and not closed.')
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
    IF(.NOT. FL%ERROR .and. IS_EXTERNAL) FL%IS_BOM = UNIT_IS_BOM(FL%IU)
    !
    IF(PRESENT(NO_SCALE)) THEN
            IF(NO_SCALE  .AND. FL%SCALE /= UNO) THEN
                FL%ERROR = TRUE
                IF(ALLOW_ERROR)  CALL ADD_MSG(ERR_MSG, &
                                         'Found a scale factor that is applied to input.'//NL// &
                                         'However this input does not support scale factors.'//NL// &
                                         'Scale factors are a number located after either the unit number or file name,'//NL// &
                                         '  or explicity defined with the post-keyword "SF" followed by a scale factor.'//NL// &
                                         '  (Note, a post-keyword appears after the unit number or file name.)'//NL// &
                                         'Please remove the scale factor and/or post-keyword SF to continue.'//NL// &
                                         '  (note you can place a # before it to comment it out).')
            END IF
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
      CALL HED_MSG(ERR_MSG,'FAILED TO OPEN FILE WITH GENERIC_INPUT_FILE.')
      !
      CALL FILE_IO_ERROR(LINE=LN, INFILE=IIN, OUTPUT=FL%IOUT, MSG=ERR_MSG, MSG2=MSG)
      !
    END IF
    !
    IF(PRESENT(EOL)) THEN
         IF (FL%ERROR .or. FL%BINARY .or. FL%IS_CONSTANT .or. FL%IU == Z) THEN
             EOL = NEG
         ELSE
             EOL = MAX_LINE_LENGTH(FL%IU)   ! File assocaited with a unit, check its size
         END IF
    END IF
    !
    IF (FL%ERROR .AND. .NOT. FL%NULL_FILE) THEN
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
    IF(GO_TO_TOP) CALL REWIND_GENERIC_INPUT_FILE(FL)
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
       ELSEIF(FL%IS_OPEN .or. FL%IS_CONSTANT) THEN
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
    CLASS(GENERIC_INPUT_FILE), INTENT(INOUT):: FL
    CHARACTER(*),              INTENT(OUT  ):: LINE
    INTEGER,OPTIONAL,          INTENT(OUT  ):: CNT     !RETURNS A COUNT OF HOW MANY LINES WERE LOADED
    INTEGER,OPTIONAL,          INTENT(OUT  ):: EOL     !LOCATIONS OF WHERE THE END OF LINE IS OR ONE SPACE BEFORE #
    LOGICAL,OPTIONAL,          INTENT(OUT  ):: EOF     !SET TO TRUE IF END OF FILE IS REACHED
    LOGICAL,OPTIONAL,          INTENT(IN   ):: NOSHIFT !SET TO TRUE TO NOT TO USE ADJUSTL
    LOGICAL,OPTIONAL,          INTENT(IN   ):: READ_COM !IF TRUE, THEN COMMENTED LINES ARE NOT BYPASSED (default is False)
    INTEGER:: IERR
    LOGICAL:: EEOF
    !
    IERR = Z
    IF(PRESENT(READ_COM)) THEN; IF(READ_COM) IERR = ONE
    END IF
    !
    IF(FL%BINARY) THEN
        READ(FL%IU, IOSTAT=IERR) LINE
        FL%ERROR = IERR /= Z
        EEOF = IERR < Z
        IF(PRESENT(CNT)) CNT = ONE
        IF(PRESENT(EOL)) EOL = LEN(LINE)
    ELSEIF(IERR==Z) THEN
        CALL READ_TO_DATA(LINE,FL%IU,FL%IOUT,Z,CNT,EOL,EEOF,NOSHIFT)
        FL%ERROR = EEOF
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
    CLASS(GENERIC_INPUT_FILE),      INTENT(INOUT):: FL
    REAL(REAL64),DIMENSION(:),  INTENT(OUT  ):: VEC
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
    CHARACTER(3):: BOM
    INTEGER:: IERR
    !
    IF(FL%IS_INTERNAL) THEN
                   CALL UTF8_BOM_OFFSET_REWIND(FL%IU)
    ELSEIF(FL%IS_OPEN) THEN
                   REWIND(FL%IU)
                   !
                   IF(FL%IS_BOM) READ(FL%IU, '(A)', ADVANCE='NO', IOSTAT=IERR) BOM
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
              CALL GET_FILE_NAME(FL%IU,FL%FNAME,EXIST,FL%IOUT,Z,MSG='GENERIC_INPUT_FILE ERROR: FROUND KEYWORD "EXTERNAL" OR "DATAUNIT", BUT FAILED TO IDENTIFY THE FILE (IN PARTICULAR ITS NAME) THAT IS ASSOCAITED WITH IT.')
        END IF
    ELSEIF(FL%IS_CONSTANT) THEN
              FL%FNAME = 'CONSTANT '//NUM2STR(FL%CONST)
    ELSEIF(FL%NULL_FILE) THEN
              FL%FNAME = "NULL"
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
    ELSEIF(FL%BINARY .OR. FL%IU==Z .or. FL%IS_INTERNAL) THEN
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
  IMPURE ELEMENTAL SUBROUTINE MOVE_GENERIC_INPUT_FILE(FL,FL_NEW)
    CLASS(GENERIC_INPUT_FILE), INTENT(INOUT):: FL
    CLASS(GENERIC_INPUT_FILE), INTENT(INOUT):: FL_NEW
    !
    CALL COPY_GENERIC_INPUT_FILE(FL_NEW, FL)
    !
    FL_NEW%OPENCLOSE = FL%OPENCLOSE ! Transefer ownership of the file
    FL%OPENCLOSE     = FALSE
    CALL FL%CLOSE()                 ! Clean out old file object
    !
  END SUBROUTINE
  !
  IMPURE ELEMENTAL SUBROUTINE COPY_GENERIC_INPUT_FILE(FL_COPY,FL)
    CLASS(GENERIC_INPUT_FILE), INTENT(INOUT):: FL_COPY
    CLASS(GENERIC_INPUT_FILE), INTENT(IN):: FL
    !
    CALL FL_COPY%CLOSE()
    !
    FL_COPY%OPENCLOSE   = FALSE      ! Original retains the right to close file, not the copy
    !
    FL_COPY%IU          = FL%IU 
    FL_COPY%IOUT        = FL%IOUT
    FL_COPY%IS_OPEN     = FL%IS_OPEN
    FL_COPY%ERROR       = FL%ERROR
    FL_COPY%BINARY      = FL%BINARY
    FL_COPY%STREAM      = FL%STREAM
    FL_COPY%NULL_FILE   = FL%NULL_FILE
    FL_COPY%IS_INTERNAL = FL%IS_INTERNAL
    FL_COPY%IS_BOM      = FL%IS_BOM      
    FL_COPY%IS_CONSTANT = FL%IS_CONSTANT
    FL_COPY%SCALE       = FL%SCALE
    IF(ALLOCATED(FL%FNAME)) ALLOCATE(FL_COPY%FNAME, SOURCE=FL%FNAME)
    IF(ALLOCATED(FL%CONST)) ALLOCATE(FL_COPY%CONST, SOURCE=FL%CONST)
    !
  END SUBROUTINE
  !
  IMPURE ELEMENTAL SUBROUTINE CLOSE_GENERIC_INPUT_FILE(FL, ONLY_CLOSE)
    CLASS(GENERIC_INPUT_FILE), INTENT(INOUT):: FL
    LOGICAL,         OPTIONAL, INTENT(IN   ):: ONLY_CLOSE
    INTEGER:: IERR
    LOGICAL:: RESET
    RESET = TRUE
    IF(PRESENT(ONLY_CLOSE)) RESET = .not. ONLY_CLOSE
    !
    IF(FL%OPENCLOSE) CLOSE(FL%IU, IOSTAT=IERR)
    !
    FL%IU        = Z
    FL%OPENCLOSE = FALSE
    FL%IS_OPEN   = FALSE
    !
    IF(RESET) THEN
       FL%IOUT        = Z
       FL%ERROR       = FALSE
       FL%BINARY      = FALSE
       FL%STREAM      = FALSE
       FL%NULL_FILE   = FALSE
       FL%IS_INTERNAL = FALSE
       FL%IS_BOM      = FALSE 
       FL%IS_CONSTANT = FALSE
       FL%SCALE       = UNO
       !
       IF(ALLOCATED(FL%FNAME)) DEALLOCATE(FL%FNAME)
       IF(ALLOCATED(FL%CONST)) DEALLOCATE(FL%CONST)
    END IF
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
    FL%IOUT = Z
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