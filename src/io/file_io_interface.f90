!
! CODE DEVELOPED BY SCOTT E BOYCE 
!                   CONTACT <seboyce@usgs.gov> or <Boyce@engineer.com>
!
! Holds a list of file unit numbers files and provides a convenient method of closing the files.
!    
! MODULE LISTING:
!   FILE_IO_INTERFACE
!                           Global Variable
!                                    TYPE(UNIT_ARRAY_BUILDER), SAVE:: DATAFILE_UNIT_NUMBER --> Any unit that remains open for entire program should be added - Note units opened with Generic_Input or Generic_Output with DATAFILE are automatically added. 
!
!                           TYPE(UNIT_ARRAY_BUILDER):: DF
!                                    CALL DF%ADD        (IU, [IS_BOM])
!                                    CALL DF%CHECK_NAME (FNAM, IU, [ISOPEN], [IS_BOM])
!                                    CALL DF%CHECK_BASE (BASENAME, IU, [NOT_UNIQUE], [IS_BOM])
!                                    CALL DF%UNIT_IS_BOM(IU)
!                                    CALL DF%PRINT      (IOUT)
!                                    CALL DF%DESTROY    ()
!
!                           Functions
!                                    COMMENT_INDEX           (LINE, [SYMBOL])       RESULT(IDX)
!                                    MAX_LINE_LENGTH         (FNAME, [INC]        ) RESULT(MXLEN)
!                                    MAX_LINE_LENGTH         (IU,    [INC], [LINE]) RESULT(MXLEN)
!                                    MAX_UNCOMMENTED_LINE_LEN
!                                    MAX_UNCOMMENTED_LINE_LEN
!
!                           Subroutines
!                                    CLOSE_GERNIC_INPUT_OUTPUT_DATAFILES    
!                                    
!                                    GET_FILE_NAME       (IU,FNAME,[EXIST],[IOUT],[IN],[MSG],[HAS_ERROR]) 
!                                    FILENAME_TO_UNIT    (FNAME,IU) 
!                                    FILENAME_TO_FULLNAME(FNAME,FULL,ISNEW) 
!    
!                                    MOVE_TO_DATA(LINE,INFILE,ERROR,OUTPUT,CNT,EOL,EOF)
!                                    READ_TO_DATA(LINE,INFILE,ERROR,OUTPUT,CNT,EOL,EOF,NOSHIFT,BACK_UP)
!
!                                    WRITE_DATA(VAR, IOUT, [NAM], [HEADER], [FMT], [TRANSPOSE], [NO_LAY_HEAD])   -- VAR can be dim = 1, 2, or 3 dim -- NO_LAY_HEAD only for dim=3
!
!
!
!
!
!
MODULE FILE_IO_INTERFACE !, ONLY: CLOSE_GERNIC_INPUT_OUTPUT_DATAFILES, DATAFILE_UNIT_NUMBER
!MODULE GENERIC_INPUT_OUTPUT_DATAFILES!, ONLY: CLOSE_GERNIC_INPUT_OUTPUT_DATAFILES, DATAFILE_UNIT_NUMBER
  !SHOULD CALL "CLOSE_GERNIC_INPUT_OUTPUT_DATAFILES" WHEN YOU WANT TO CLOSE ALL THE OPENED DATAFILES, OTHERWISE THEY WILL BE AUTOMATICALLY CLOSED WHEN SIMULATION ENDS
  USE CONSTANTS,              ONLY: NEG, Z, ONE, TWO, FOUR, SEV, EIGHT, THOU, QUIN, TRUE, FALSE, BLNK, TAB, NL, BLN, CR, LF, DZ, UNO, NO
  USE ARRAY_DATA_TYPES,       ONLY: CHARACTER_ARRAY, INTEGER_VECTOR, LOGICAL_VECTOR, CHARACTER_TYPE_ARRAY  
  USE GENERIC_OPEN_INTERFACE, ONLY: GENERIC_OPEN, FORCE_UNIT_CLOSE
  USE ERROR_INTERFACE,        ONLY: STOP_ERROR, WARNING_MESSAGE, FILE_IO_ERROR
  USE PARSE_WORD_INTERFACE,   ONLY: PARSE_WORD, PARSE_WORD_UP
  !
  USE, INTRINSIC:: ISO_FORTRAN_ENV, ONLY: REAL32, REAL64, REAL128, INT8, INT16, INT32, INT64
  !
  IMPLICIT NONE
  PRIVATE
  !
  !
  PUBLIC:: READ_TO_DATA, MOVE_TO_DATA, COMMENT_INDEX
  !
  PUBLIC:: DATAFILE_UNIT_NUMBER, UNIT_ARRAY_BUILDER
  !
  PUBLIC:: CLOSE_GERNIC_INPUT_OUTPUT_DATAFILES ! closes all files added to DATAFILE_UNIT_NUMBER 
  !
  PUBLIC:: GET_FILE_NAME
  PUBLIC:: FILENAME_TO_UNIT, FILENAME_TO_FULLNAME
  !
  PUBLIC:: MAX_LINE_LENGTH, MAX_UNCOMMENTED_LINE_LEN
  !
  PUBLIC:: WRITE_DATA
  !
  INTERFACE MOVE_TO_DATA
    MODULE PROCEDURE MOVE_TO_DATA_LINE   ! MOVE_TO_DATA(LINE,INFILE,[ERROR],[EOF])
    MODULE PROCEDURE MOVE_TO_DATA_NOLINE ! MOVE_TO_DATA(     INFILE,[ERROR],[EOF])     !Assumes that # is within the first 128 columns to skip empty line -- CHAR(128):: LINE
  END INTERFACE
  !
  INTERFACE COMMENT_INDEX
    MODULE PROCEDURE COMMENT_INDEX_POUND
    MODULE PROCEDURE COMMENT_INDEX_PASS
  END INTERFACE
  !
  INTERFACE MAX_LINE_LENGTH
    MODULE PROCEDURE MAX_LINE_LENGTH_FILE   !(FNAME, [INC]        ) RESULT(MXLEN)
    MODULE PROCEDURE MAX_LINE_LENGTH_UNIT   !(IU,    [INC], [LINE]) RESULT(MXLEN)
  END INTERFACE
  !
  !
  INTERFACE MAX_UNCOMMENTED_LINE_LEN
    MODULE PROCEDURE MAX_UNCOMMENTED_LINE_LEN_FILE   !(FNAME     ) RESULT(MXLEN)
    MODULE PROCEDURE MAX_UNCOMMENTED_LINE_LEN_UNIT   !(IU, [LINE]) RESULT(MXLEN)
  END INTERFACE
  !
  INTERFACE WRITE_DATA
    MODULE PROCEDURE WRITE_DATA_DIM1_DBL!(VAR, IOUT, NAM, HEADER, FMT, TRANSPOSE)
    MODULE PROCEDURE WRITE_DATA_DIM2_DBL!(VAR, IOUT, NAM, HEADER, FMT, TRANSPOSE)
    MODULE PROCEDURE WRITE_DATA_DIM3_DBL!(VAR, IOUT, NAM, HEADER, FMT, TRANSPOSE, NO_LAY_HEAD)
  END INTERFACE
  !
  TYPE UNIT_ARRAY_BUILDER
      TYPE(INTEGER_VECTOR ):: IU
      TYPE(LOGICAL_VECTOR ):: IS_BOM
      TYPE(CHARACTER_ARRAY):: FNAM
      TYPE(CHARACTER_TYPE_ARRAY):: BASE
      CONTAINS
      PROCEDURE, PASS(DF):: ADD         => ADD_UNIT_ARRAY_ENTRY !(IU, [IS_BOM])
      PROCEDURE, PASS(DF):: CHECK_NAME  => CHECK_NAME_UNIT_ARRAY!(FNAM,IU,ISOPEN)
      PROCEDURE, PASS(DF):: CHECK_BASE  => CHECK_BASE_UNIT_ARRAY!(BASENAME,IU,ISOPEN)
      PROCEDURE, PASS(DF):: UNIT_IS_BOM => GET_IS_BOM_UNIT_ARRAY!(IU)
      PROCEDURE, PASS(DF):: PRINT       => PRINT_NAME_UNIT_ARRAY!(IOUT)
      PROCEDURE, PASS(DF):: PRINT_STR   => GET_NAME_UNIT_ARRAY  ! Same as print, but returns a str
      PROCEDURE, PASS(DF):: DESTROY     => DESTROY_UNIT_ARRAY_ENTRY
      FINAL:: FINAL_CLOSE_GERNIC_INPUT_OUTPUT_DATAFILES
  END TYPE
  !
  TYPE(UNIT_ARRAY_BUILDER), SAVE:: DATAFILE_UNIT_NUMBER
  !
  CONTAINS
  !
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !  
  !  Read Line routines
  !
  SUBROUTINE MOVE_TO_DATA_LINE(LINE,INFILE,ERROR,EOF)                          !Moves to start of first uncommented line
    CHARACTER(*),    INTENT(INOUT):: LINE    !LINE TO LOAD DATA TOO
    INTEGER,         INTENT(IN   ):: INFILE  !UNIT OF FILE TO LOAD LINE FROM
    INTEGER,OPTIONAL,INTENT(IN   ):: ERROR   !UNIT TO WRITE ERROR MESSAGE TOO
    LOGICAL,OPTIONAL,INTENT(OUT  ):: EOF     !SET TO TRUE IF END OF FILE IS REACHED
    !                            OUTPUT=Z
    CALL READ_TO_DATA(LINE,INFILE,ERROR,EOF=EOF,NOSHIFT=TRUE,BACK_UP=TRUE)
    !
  END SUBROUTINE
  !
  SUBROUTINE MOVE_TO_DATA_NOLINE(INFILE,ERROR,EOF)                          !Moves to start of first uncommented line
    INTEGER,         INTENT(IN   ):: INFILE  !UNIT OF FILE TO LOAD LINE FROM
    INTEGER,OPTIONAL,INTENT(IN   ):: ERROR   !UNIT TO WRITE ERROR MESSAGE TOO
    LOGICAL,OPTIONAL,INTENT(OUT  ):: EOF     !SET TO TRUE IF END OF FILE IS REACHED
    CHARACTER(128):: LINE
    !                            OUTPUT=Z
    CALL READ_TO_DATA(LINE,INFILE,ERROR,EOF=EOF,NOSHIFT=TRUE,BACK_UP=TRUE)
    !
  END SUBROUTINE
  !
  !#########################################################################################################################
  !
  SUBROUTINE READ_TO_DATA(LINE,INFILE,ERROR,OUTPUT,CNT,EOL,EOF,NOSHIFT,BACK_UP,HED,COM)
    CHARACTER(*),         INTENT(INOUT):: LINE    !LINE TO LOAD DATA TOO
    INTEGER,              INTENT(IN   ):: INFILE  !UNIT OF FILE TO LOAD LINE FROM
    INTEGER,     OPTIONAL,INTENT(IN   ):: ERROR   !UNIT TO WRITE ERROR MESSAGE TOO
    INTEGER,     OPTIONAL,INTENT(IN   ):: OUTPUT  !UNIT TO WRITE TRANSCRIPT OF WHAT IS LOADED TOO
    INTEGER,     OPTIONAL,INTENT(OUT  ):: CNT     !RETURNS A COUNT OF HOW MANY LINES WERE LOADED
    INTEGER,     OPTIONAL,INTENT(OUT  ):: EOL     !LOCATIONS OF WHERE THE END OF LINE IS OR ONE SPACE BEFORE #
    LOGICAL,     OPTIONAL,INTENT(OUT  ):: EOF     !SET TO TRUE IF END OF FILE IS REACHED
    LOGICAL,     OPTIONAL,INTENT(IN   ):: NOSHIFT !SET TO TRUE TO NOT TO USE ADJUSTL
    LOGICAL,     OPTIONAL,INTENT(IN   ):: BACK_UP !SET TO TRUE TO BACKSPACE 1 LINE AFTER READING LINK
    CHARACTER(*),OPTIONAL,INTENT(IN   ):: HED     !IF OUTPUT IS SPECIFIED, THEN WRITE HEADER BEFORE ECHOING COMMENTS OUT.
    CHARACTER(*),OPTIONAL,INTENT(IN   ):: COM     !STRING OF ACCEPTED COMMENT SYMBOLS (1 byte/Char per symbol). 
                                                  !  Set to "" to only include empty lines as comments
                                                  !  -- DO NOT INCLUDE BLANK SPACES WITH OTHER SYMBOLS --
                                                  !  ---- FOR EXAMPLE THESE ARE WRONG:  "# !" or " #!" or "#! "  ----
                                                  !  Default when not present is COM = #
    INTEGER:: I, C, ERR, N, IOUT, IERR
    LOGICAL:: FIRST, TRANSCRIBE, SHIFT, POUND_COM, NO_COM
    !
    SHIFT = TRUE
    FIRST = TRUE
    !
    IF(PRESENT(ERROR))THEN
        IERR=ERROR
    ELSE
        IERR=Z
    END IF
    !
    IF(PRESENT(OUTPUT))THEN
        IOUT=OUTPUT
    ELSE
        IOUT=Z
    END IF
    !
    IF(PRESENT(EOF)) EOF=FALSE
    !
    IF(PRESENT(NOSHIFT)) SHIFT = .NOT. NOSHIFT
    !
       POUND_COM = .NOT. PRESENT(COM)
    IF(POUND_COM) THEN
                  NO_COM = FALSE
    ELSE
                  NO_COM = COM == ""
    END IF
    !
    TRANSCRIBE = IOUT /= Z
    N=Z
    READ_LOOP: DO
          READ(INFILE,'(A)',IOSTAT=ERR) LINE
          IF    (ERR > Z) THEN                                   !LINE FAILED TO READ, THIS IS MOST LIKELY DUE TO END OF FILE LINE,INFILE,OUTPUT,MSG
                               CALL FILE_IO_ERROR(ERR,INFILE,INFILE=INFILE,OUTPUT=IERR, MSG='"SUBROUTINE READ_TO_DATA" FATAL ERROR'//NL//'RECIEVED A FORTRAN ERROR GREATER THAN ZERO (IOSTAT>0),'//NL//'WHILE READING IN THE NEXT LINE IN THE FILE.'//BLN//'THIS IS NOT AN END OF FILE ERROR,'//NL//'BUT A RATHER A FAILURE TO READ THE NEXT LINE FROM THE FILE ITSELF.'//BLN//'THE GUESSED PREVIOUS LINE THAT WAS SUCESSFULLY READ IS:'//NL//'"'//TRIM(LINE)//'"')
          ELSEIF(ERR < Z) THEN !EOF
                               LINE=BLNK
                               C=ONE
                               IF(PRESENT(EOF)) EOF=TRUE
                               BACKSPACE(INFILE) !NOTE THAT EOF COUNTS OF 1 READ, BUT MULTIPLE READS TO EOF WILL NOT MOVE ANY FURTHER, SO REPOSITION TO THE END OF THE FILE, BUT NOT ONE BEYOND TO KEEP N (THE READ COUNT) CORRET 
                               EXIT READ_LOOP
          END IF
          !
          N = N + ONE  ! Sucessfully read of line, keep track of total line count
          !----------------------------------------------------------------------------------------------------------
          DO CONCURRENT (I=1:LEN_TRIM(LINE), LINE(I:I)==TAB .OR. LINE(I:I)==CR .OR. LINE(I:I)==LF)  !TAB = CHAR(9) -> Fortran treates TAB as if it was character--make spaces to improve search --also remove dangling CR or LF  --> Note that this is identicaly to "CALL SPECIAL_BLANK_STRIP(LINE)"   
              LINE(I:I)=BLNK
          END DO
          !----------------------------------------------------------------------------------------------------------
          IF(LINE(1:1) == BLNK .AND. SHIFT) LINE = ADJUSTL(LINE)
          !----------------------------------------------------------------------------------------------------------
          !
          IF (POUND_COM) THEN
                            C=INDEX(LINE,'#')-ONE      ! Check for comment symbol  --> Same as C = COMMENT_INDEX(LINE)
          ELSEIF(NO_COM) THEN
                            C = NEG
          ELSE
                            C=SCAN(LINE, COM)-ONE
          END IF
          !
          IF (C < Z) THEN
              C=LEN_TRIM(LINE)      ! NO # FOUND, SO USE ENTIRE STRING
              IF (C == Z) C=ONE     ! LINE IS BLANK, SET TO 1
          END IF
          IF (C == Z) C=ONE         !# IS ON FIRST COLUMN OR LINE IS BLANK, SET TO 1
          !----------------------------------------------------------------------------------------------------------
          !
          IF (POUND_COM) THEN
                            IF(LINE(C:C) /= '#' .AND. LINE(1:C) /= " ") EXIT READ_LOOP   !Start of line is not COM and all char before COM are blank
          ELSEIF(NO_COM) THEN
                            IF(LINE(1:C) /= " ") EXIT READ_LOOP
          ELSE
                            IF(INDEX(COM, LINE(C:C)) == Z .AND. LINE(1:C) /= " ") EXIT READ_LOOP  
          END IF
          !
          IF(TRANSCRIBE) THEN
                IF(FIRST) THEN
                              IF( PRESENT(HED) ) WRITE(IOUT,'(/ A)') HED
                              WRITE(IOUT,'(/ A)') TRIM(LINE)
                              FIRST = FALSE
                ELSE
                              WRITE(IOUT,'(  A)') TRIM(LINE)
                END IF
          END IF
          !
    END DO READ_LOOP
    !
    IF(.NOT. FIRST) WRITE(IOUT,'(A)')  ! Implies TRANSCRIBE is true
    !
    IF(PRESENT(CNT)) CNT=N
    !
    IF(PRESENT(EOL)) EOL = C
    !
    IF(PRESENT(BACK_UP)) THEN; IF(BACK_UP.AND.ERR==Z) BACKSPACE(INFILE)
    END IF
    !
  END SUBROUTINE
  !
  !#########################################################################################################################
  !
  PURE FUNCTION COMMENT_INDEX_POUND(LINE) RESULT(IDX)
    CHARACTER(*),         INTENT(IN):: LINE
    INTEGER:: IDX
    !
    IDX = INDEX(LINE,'#')-ONE
    !
    IF (IDX  < Z) THEN
        IDX=LEN_TRIM(LINE)      ! NO # FOUND, SO USE ENTIRE STRING
        IF (IDX == Z) IDX=ONE   ! LINE IS BLANK, SET TO 1
    END IF
    IF (IDX == Z) IDX=ONE                !# IS ON FIRST COLUMN OR LINE IS BLANK, SET TO 1
    !
    !IF (IDX > ONE) THEN
    !                   IF(LINE(IDX:IDX) == CR .OR. LINE(IDX:IDX) == LF) IDX=IDX-ONE  !HAS MIXED WINDOWS AND UNIX STYLE ENDING 
    !END IF
    !
  END FUNCTION
  !
  !#########################################################################################################################
  !
  PURE FUNCTION COMMENT_INDEX_PASS(LINE, COM) RESULT(IDX)
    CHARACTER(*),INTENT(IN):: LINE
    CHARACTER,   INTENT(IN):: COM     !STRING OF ACCEPTED COMMENT SYMBOLS (1 byte/Char per symbol). 
                                      !  Set to "" to only include empty lines as comments
                                      !  -- DO NOT INCLUDE BLANK SPACES WITH OTHER SYMBOLS --
                                      !  ---- FOR EXAMPLE THESE ARE WRONG:  "# !" or " #!" or "#! "  ----
                                      !  Default when not present is COM = #
    INTEGER:: IDX
    !
    IF(COM == "") THEN
                  IDX = NEG
    ELSE         
                  IDX = SCAN(LINE, COM)-ONE
    END IF
    !
    IF (IDX  < Z) THEN
        IDX=LEN_TRIM(LINE)      ! NO # FOUND, SO USE ENTIRE STRING
        IF (IDX == Z) IDX=ONE   ! LINE IS BLANK, SET TO 1
    END IF
    IF (IDX == Z) IDX=ONE                !# IS ON FIRST COLUMN OR LINE IS BLANK, SET TO 1
    !
    !IF (IDX > ONE) THEN
    !                   IF(LINE(IDX:IDX) == CR .OR. LINE(IDX:IDX) == LF) IDX=IDX-ONE  !HAS MIXED WINDOWS AND UNIX STYLE ENDING 
    !END IF
    !
  END FUNCTION
  !
  !#########################################################################################################################
  !
  RECURSIVE FUNCTION MAX_LINE_LENGTH_UNIT(IU, INC, LINE) RESULT(MXLEN)
    INTEGER,               INTENT(IN   ):: IU
    LOGICAL, OPTIONAL, INTENT(IN):: INC   !IF TRUE, THEN CHECK FOR KEYWORD "BLOCK_INCLUDE" AND SEARCH THE FILE THAT IT WOULD OPEN, ALSO CHECK WITHIN SUBFILES FOR "BLOCK_INCLUDE".
    CHARACTER(*), OPTIONAL,INTENT(INOUT):: LINE !SCRATCH SPACE TO USE FOR FINDING THE FILE NAME
    INTEGER:: MXLEN
    CHARACTER(:),ALLOCATABLE:: FNAME
    !
    IF(IU /= Z) THEN
       IF(PRESENT(LINE)) THEN
           INQUIRE(IU, NAME=LINE)
           MXLEN=MAX_LINE_LENGTH_FILE(LINE,INC)
       ELSE
           CALL GET_FILE_NAME(IU,FNAME) 
           MXLEN=MAX_LINE_LENGTH_FILE(FNAME,INC)
           DEALLOCATE(FNAME)
       END IF
    ELSE
        MXLEN = Z
    END IF
    !
  END FUNCTION
  !
  !#########################################################################################################################
  !
  RECURSIVE FUNCTION MAX_LINE_LENGTH_FILE(FNAME, INC) RESULT(MXLEN)
    !USE GENERIC_OPEN_INTERFACE, ONLY: GENERIC_OPEN
    CHARACTER(*),      INTENT(IN   ):: FNAME
    LOGICAL, OPTIONAL, INTENT(IN):: INC   !IF TRUE, THEN CHECK FOR KEYWORD "BLOCK_INCLUDE" AND SEARCH THE FILE THAT IT WOULD OPEN, ALSO CHECK WITHIN SUBFILES FOR "BLOCK_INCLUDE".
    INTEGER:: MXLEN
    INTEGER:: IU, WID, IERR, IBLNK
    CHARACTER:: TXT
    !NL = NEW_LINE(TXT)
    !
    MXLEN = Z
    !
    IF(PRESENT(INC)) THEN  !Check for BLOCK_INCLUDE WITHIN FILE, IF FOUND MAKE RECURSIVE CALL.
        IF(INC) THEN
                IU = Z
                CALL GENERIC_OPEN(FNAME, IU, Z, ACTION='READ', FORM='FORMATTED', ACCESS='SEQUENTIAL', STATUS='OLD', BUFFER_BLOCKSIZE=65536) !, WARN=GET_WARN()
                BLOCK
                    INTEGER:: LLOC, I, J, MXLEN2
                    CHARACTER(512):: LINE
                    LOGICAL::EOF
                    DO
                        CALL READ_TO_DATA(LINE,IU,EOF=EOF)
                        IF(EOF) EXIT
                        !
                        LLOC = ONE
                        CALL PARSE_WORD_UP(LINE,LLOC,I,J)
                        !
                        IF(LINE(I:J)=='BLOCK_INCLUDE') THEN              ! .OR. LINE(I:J)=='INCLUDE'
                           CALL PARSE_WORD(LINE,LLOC,I,J)
                           !
                           MXLEN2 = MAX_LINE_LENGTH_FILE(LINE(I:J), INC)
                           !
                           IF(MXLEN < MXLEN2 ) MXLEN = MXLEN2
                           !
                        END IF
                    END DO
                END BLOCK
                !
                CLOSE(IU)
        END IF
    END IF
    !
    ! Now start actual line width check
    !
    IU = Z
    CALL GENERIC_OPEN(FNAME, IU, Z, ACTION='READ', FORM='UNFORMATTED', ACCESS='STREAM', STATUS='OLD', BUFFER_BLOCKSIZE=65536)   !, WARN=GET_WARN()
    !
    IBLNK = Z  ! Keep track of trailing blank spaces (dont include them)
    WID = Z
    DO
        READ(IU,IOSTAT=IERR) TXT
        IF(IERR /= Z) EXIT
        !
        IF(TXT==NL) THEN
            !
            IF(MXLEN < WID) MXLEN = WID
            WID   = Z
            IBLNK = Z
            !
        ELSEIF(TXT==BLNK .OR. TXT==TAB) THEN
            !
            IBLNK = IBLNK + ONE
            !
        ELSEIF(TXT /= CR) THEN  !Is not a DOS Carriage Return, not a blank, and not a LineFeed (NL)
            WID = WID + IBLNK + ONE
            IBLNK = Z
        END IF
    END DO
    !
    IF(MXLEN < WID) MXLEN = WID
    !
    CLOSE(IU)
    !
  END FUNCTION
  !RECURSIVE FUNCTION MAX_LINE_LENGTH_FILE(FNAME, INC) RESULT(MXLEN)
  !  !USE GENERIC_OPEN_INTERFACE, ONLY: GENERIC_OPEN
  !  CHARACTER(*),      INTENT(IN):: FNAME
  !  LOGICAL, OPTIONAL, INTENT(IN):: INC   !IF TRUE, THEN CHECK FILES LOADED WITH BLOCK_INCLUDE OR INCLUDE
  !  INTEGER:: MXLEN
  !  INTEGER:: IU, WID, IERR, ISTOP, ISTART
  !  CHARACTER:: TXT
  !  !NL = NEW_LINE(TXT)
  !  !
  !  MXLEN = Z
  !  !
  !  IF(PRESENT(INC)) THEN
  !      IF(INC) THEN
  !              IU = Z
  !              CALL GENERIC_OPEN(FNAME, IU, Z, ACTION='READ', FORM='FORMATTED', ACCESS='SEQUENTIAL', STATUS='OLD', BUFFER_BLOCKSIZE=65536, WARN=GET_WARN())
  !              BLOCK
  !                  INTEGER:: LLOC, I, J, MXLEN2
  !                  CHARACTER(512):: LINE
  !                  LOGICAL::EOF
  !                  DO
  !                      CALL READ_TO_DATA(LINE,IU,EOF=EOF)
  !                      IF(EOF) EXIT
  !                      !
  !                      LLOC = ONE
  !                      CALL PARSE_WORD_UP(LINE,LLOC,I,J)
  !                      !
  !                      IF(LINE(I:J)=='BLOCK_INCLUDE') THEN              ! .OR. LINE(I:J)=='INCLUDE'
  !                         CALL PARSE_WORD(LINE,LLOC,I,J)
  !                         !
  !                         MXLEN2 = MAX_LINE_LENGTH_FILE(LINE(I:J), INC)
  !                         !
  !                         IF(MXLEN < MXLEN2 ) MXLEN = MXLEN2
  !                         !
  !                      END IF
  !                  END DO
  !              END BLOCK
  !              !
  !              CLOSE(IU)
  !      END IF
  !  END IF
  !  !
  !  IU = Z
  !  CALL GENERIC_OPEN(FNAME, IU, Z, ACTION='READ', FORM='UNFORMATTED', ACCESS='STREAM', STATUS='OLD', BUFFER_BLOCKSIZE=65536, WARN=GET_WARN())
  !  !
  !  INQUIRE(IU,POS=ISTART)
  !  DO
  !      READ(IU,IOSTAT=IERR) TXT
  !      IF(IERR /= Z) EXIT
  !      !
  !      IF(TXT==NL) THEN
  !          INQUIRE(IU,POS=ISTOP)
  !          WID = ISTOP-ISTART+ONE        !Note that unix format will have the correct length, but windows will be LEN+1
  !          IF(MXLEN < WID) MXLEN = WID
  !          ISTART=ISTOP+ONE
  !      END IF
  !  END DO
  !  !
  !  INQUIRE(IU,POS=ISTOP)
  !  WID = ISTOP-ISTART+ONE
  !  IF(MXLEN < WID) MXLEN = WID
  !  !
  !  CLOSE(IU)
  !  !
  !END FUNCTION
  !
  !######################################################################    
  !
  !!!RECURSIVE SUBROUTINE MAX_LINE_LENGTH_KEYWORD(IU, KEY, LINE, MXLEN) --INCOMPLETE/BAD CODE seb
  !!!  INTEGER,      INTENT(IN   ):: IU
  !!!  CHARACTER(*), INTENT(IN   ):: KEY  !KEYWORD TO INDICATE FILE SHOULD BE OPENED
  !!!  CHARACTER(*), INTENT(INOUT):: LINE !SCRATCH SPACE TO USE FOR FINDING THE FILE NAME
  !!!  INTEGER,      INTENT(INOUT):: MXLEN
  !!!  INTEGER:: ITMP, LLOC, I, J
  !!!  INTEGER:: WID, IERR
  !!!  CHARACTER:: TXT
  !!!  LOGICAL:: EOF
  !!!  !NL = NEW_LINE(TXT)
  !!!  !
  !!!  MXLEN = Z
  !!!  !
  !!!  REWIND(IU) 
  !!!  DO
  !!!      CALL READ_TO_DATA(LINE,IU,EOF=EOF)
  !!!      IF(EOF) EXIT
  !!!      !
  !!!      LLOC = ONE
  !!!      CALL PARSE_WORD_UP(LINE,LLOC,I,J)
  !!!      !
  !!!      IF(LINE(I:J)==KEY) THEN              ! .OR. LINE(I:J)=='INCLUDE'
  !!!         CALL PARSE_WORD(LINE,LLOC,I,J)
  !!!         ITMP = Z
  !!!         CALL GENERIC_OPEN(LINE(I:J), ITMP, Z, ACTION='READ', FORM='FORMATTED', ACCESS='SEQUENTIAL', STATUS='OLD', BUFFER_BLOCKSIZE=65536, WARN=GET_WARN())
  !!!         !
  !!!         CALL MAX_LINE_LENGTH_KEYWORD(ITMP, KEY, LINE, WID)
  !!!         !
  !!!         IF(MXLEN < WID) MXLEN = WID
  !!!         !
  !!!         CLOSE(ITMP)
  !!!         !
  !!!      END IF
  !!!  END DO
  !!!  !
  !!!  REWIND(IU)
  !!!  !
  !!!  INQUIRE(IU,POS=I)
  !!!  DO
  !!!      READ(IU,IOSTAT=IERR) TXT
  !!!      IF(IERR /= Z) EXIT
  !!!      !
  !!!      IF(TXT==NL) THEN
  !!!          INQUIRE(IU,POS=J)
  !!!          WID = J-I+ONE        !Note that unix format will have the correct length, but windows will be LEN+1
  !!!          IF(MXLEN < WID) MXLEN = WID
  !!!          I=J+ONE
  !!!      END IF
  !!!  END DO
  !!!  !
  !!!  INQUIRE(IU,POS=J)
  !!!  WID = J-I+ONE
  !!!  IF(MXLEN < WID) MXLEN = WID
  !!!  !
  !!!  REWIND(IU)
  !!!  !
  !!!END SUBROUTINE
  !
  !#########################################################################################################################
  !
  RECURSIVE FUNCTION MAX_UNCOMMENTED_LINE_LEN_UNIT(IU,LINE) RESULT(MXLEN)
    INTEGER,               INTENT(IN   ):: IU
    CHARACTER(*), OPTIONAL,INTENT(INOUT):: LINE !SCRATCH SPACE TO USE FOR FINDING THE FILE NAME
    INTEGER:: MXLEN
    CHARACTER(:),ALLOCATABLE:: FNAME
    !
    IF(IU /= Z) THEN
       IF(PRESENT(LINE)) THEN
           INQUIRE(IU, NAME=LINE)
           MXLEN=MAX_UNCOMMENTED_LINE_LEN(LINE)
       ELSE
           CALL GET_FILE_NAME(IU,FNAME) 
           MXLEN=MAX_UNCOMMENTED_LINE_LEN(FNAME)
           DEALLOCATE(FNAME)
       END IF
    ELSE
        MXLEN = Z
    END IF
    !
  END FUNCTION
  !
  !#########################################################################################################################
  !
  RECURSIVE FUNCTION MAX_UNCOMMENTED_LINE_LEN_FILE(FNAME, COM) RESULT(MXLEN)
    !USE GENERIC_OPEN_INTERFACE, ONLY: GENERIC_OPEN
    CHARACTER(*),         INTENT(IN   ):: FNAME
    CHARACTER(*),OPTIONAL,INTENT(IN   ):: COM     !STRING OF ACCEPTED COMMENT SYMBOLS (1 byte/Char per symbol). 
                                                  !  Set to "" to only include empty lines as comments
                                                  !  -- DO NOT INCLUDE BLANK SPACES WITH OTHER SYMBOLS --
                                                  !  ---- FOR EXAMPLE THESE ARE WRONG:  "# !" or " #!" or "#! "  ----
                                                  !  Default when not present is COM = #
    INTEGER:: MXLEN
    INTEGER:: IU, WID, IERR, IBLNK
    CHARACTER:: TXT
    LOGICAL:: POUND_COM, HAS_COM, GET_NL
    !
       POUND_COM = .NOT. PRESENT(COM)
    IF(POUND_COM) THEN
                  HAS_COM = TRUE
    ELSE
                  HAS_COM = COM /= ""
    END IF
    !
    MXLEN = Z
    !
    IU = Z
    CALL GENERIC_OPEN(FNAME, IU, Z, ACTION='READ', FORM='UNFORMATTED', ACCESS='STREAM', STATUS='OLD', BUFFER_BLOCKSIZE=65536)  !, WARN=GET_WARN()
    !
    IBLNK = Z
    WID = Z
    DO
        READ(IU,IOSTAT=IERR) TXT
        IF(IERR /= Z) EXIT
        !
        IF(HAS_COM) THEN
                    IF(POUND_COM) THEN
                               GET_NL = TXT=='#'
                    ELSE       
                               GET_NL = INDEX(COM, TXT) > Z
                    END IF
                    IF(GET_NL) THEN
                               DO  WHILE(IERR==Z .AND. TXT /= NL); READ(IU,IOSTAT=IERR) TXT
                               END DO
                        
                    END IF
        END IF
        !
        IF(TXT==NL) THEN
            !
            IF(MXLEN < WID) MXLEN = WID
            WID   = Z
            IBLNK = Z
            !
        ELSEIF(TXT==BLNK .OR. TXT==TAB) THEN
            !
            IBLNK = IBLNK + ONE
            !
        ELSEIF(TXT /= CR) THEN  !Is not a DOS Carriage Return, not a blank, and not a LineFeed (NL)
            WID = WID + IBLNK + ONE
            IBLNK = Z
        END IF
    END DO
    !
    IF(MXLEN < WID) MXLEN = WID
    !
    CLOSE(IU)
    !
  END FUNCTION
  !
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !
  ! File Name Routines
  !
  ! Perfoms same function as ERROR_INTERFACE, ONLY: GET_FILE_NAME(IU, FNAME, SET_FNAME)
  !   But has more options and can stop program execution due to an error.
  !
  SUBROUTINE GET_FILE_NAME(IU,FNAME,EXIST,IOUT,IN,MSG,HAS_ERROR, LINE) 
    INTEGER,                 INTENT(IN   ):: IU                ! UNIT NUMBER TO LOOK FILE NAME UP FROM
    CHARACTER(:),ALLOCATABLE,INTENT(OUT  ):: FNAME             ! FILE NAME ASSOCIATED WITH UNIT NUMBER
    LOGICAL, OPTIONAL,       INTENT(OUT  ):: EXIST             ! RETURN TRUE IF UNIT NUMBER IS ATTACHED TO A FILE THAT EXISTS
    INTEGER, OPTIONAL,       INTENT(IN   ):: IOUT, IN          ! ERROR UNIT TO WRITE TO, FILE UNIT THAT LN ORIGINATED FROM -- FOR ERROR REPORTING
    CHARACTER(*), OPTIONAL,  INTENT(IN   ):: MSG               ! ERROR MESSAGE PRINTED WHEN FAIL TO LOAD NUMBER AND STOP PROGRAM, IF "NOSTOP" THEN VAL IS SET TO NaN
    LOGICAL,      OPTIONAL,  INTENT(  OUT):: HAS_ERROR         ! SET TO TRUE IF THERE IS AN ERROR
    CHARACTER(*), OPTIONAL,  INTENT(IN   ):: LINE              ! CURRENT INPUT LINE FOR ERROR MSG
    CHARACTER(:), ALLOCATABLE:: FNAM
    INTEGER:: I, SIZ
    LOGICAL:: CHECK
	CHARACTER(16):: CIU
    !
    IF(PRESENT(HAS_ERROR)) HAS_ERROR = FALSE
    !
    IF(IU == Z) THEN
        FNAME = '"INTERNAL FILE" with zero unit number. Did you use "INTERNAL" when you should specify a file name?'
        IF(PRESENT(EXIST)) EXIST = FALSE
        RETURN
    END IF
    !
    ALLOCATE(CHARACTER(256):: FNAM)
    INQUIRE(IU, NAME=FNAM, EXIST=CHECK)
    !
    IF(CHECK) THEN
          INQUIRE(FILE=FNAM, EXIST=CHECK)  !CHECK IF FILE NAME SIZE IS BIG ENOUGH
          IF(.NOT. CHECK) THEN
                DO I=ONE, 15
                  IF(CHECK) THEN
                                EXIT
                  ELSE
                      SIZ = 600 * I
                      DEALLOCATE(FNAM)
                      ALLOCATE(CHARACTER(SIZ):: FNAM)
                      INQUIRE(IU, NAME=FNAM)
                      INQUIRE(FILE=FNAM, EXIST=CHECK)  !CHECK IF FILE NAME SIZE IS BIG ENOUGH
                  END IF
                END DO
          END IF
          !
          IF(.NOT. CHECK) THEN
                CHECK = TRUE          !IF STAYS TRUE THEN STOP EXECUTION
                IF(PRESENT(MSG)) THEN
                     !
                     IF (MSG=='NOSTOP') CHECK = FALSE
                     IF(PRESENT(HAS_ERROR)) HAS_ERROR = TRUE
                ELSE IF(PRESENT(HAS_ERROR)) THEN
                     !
                     HAS_ERROR = TRUE
                     CHECK  = FALSE
                END IF
                !
	            WRITE(CIU, "(I16)") IU
	            !
                IF(CHECK) THEN
                    FNAM = 'GET_FILE_NAME READ UTILITY ERROR: FAILED TO IDENFITY FILE NAME FROM PROVIDED UNIT NUMBER.'//BLN//'THE UNIT NUMBER THAT IS BEING LOOKED UP IS '//TRIM(ADJUSTL(CIU))
                    IF(PRESENT(MSG)) FNAM = FNAM//BLN//'THE FOLLOWING IS AN ADDITIONAL COMMENT PASSED TO GET_FILE_NAME:'//BLN//TRIM(MSG)
                    !
                    CALL STOP_ERROR(LINE, IN, IOUT, FNAM )
                ELSE
                    CHECK = FALSE
                    FNAM = 'GET_FILE_NAME ERROR: Failed to identy file name from unit number '//TRIM(ADJUSTL(CIU))
                END IF
          END IF
          !
          I = LEN_TRIM(FNAM)
          ALLOCATE(FNAME, SOURCE=FNAM(ONE:I))
    ELSE
	    WRITE(CIU, "(I16)") IU
        FNAME = '"UNKNOWN FILE" Failed to identy file name from unit number '//TRIM(ADJUSTL(CIU))
    END IF
    !
    IF(PRESENT(EXIST)) EXIST = CHECK
    !
  END SUBROUTINE
  !
  !#############################################################################################################################################################
  !
  SUBROUTINE FILENAME_TO_UNIT(FNAME,IU) 
    CHARACTER(*),            INTENT(IN   ):: FNAME 
    INTEGER,                 INTENT(INOUT):: IU
    !
    CHARACTER(:),ALLOCATABLE:: FN
    LOGICAL::ISOPEN, EXIST
    !
    INQUIRE(FILE=FNAME,NUMBER=IU,OPENED=ISOPEN, EXIST=EXIST)
    !
    IF(.NOT. ISOPEN) THEN
        IU = Z
        IF(EXIST) THEN
            !
            CALL GENERIC_OPEN(FNAME, IU, ACTION='READ', FORM='FORMATTED', ACCESS='SEQUENTIAL', STATUS='OLD', ASYNC=NO, BUFFER_BLOCKSIZE=0, BUFFER_COUNT=0, ERROR=EXIST)
            !
            IF(EXIST) THEN !ERROR FOUND
                IU = Z
            ELSE
                CALL GET_FILE_NAME(IU,FN,HAS_ERROR=EXIST) 
                IF(EXIST) THEN !ERROR FOUND
                    IU = Z
                ELSE
                    CALL FORCE_UNIT_CLOSE(IU)
                    !
                    INQUIRE(FILE=FN,NUMBER=IU,OPENED=ISOPEN) !COULD BE A CHANCE THAT IT DETECTS THE FILE FROM GENERIC_OPEN
                    IF(.NOT. ISOPEN) IU = Z
                END IF
            END IF
        END IF
    END IF
    !
  END SUBROUTINE
  !
  !#############################################################################################################################################################
  !
  SUBROUTINE FILENAME_TO_FULLNAME(FNAME,FULL,ISNEW) 
    CHARACTER(*),            INTENT(IN   ):: FNAME
    CHARACTER(:),ALLOCATABLE,INTENT(OUT  ):: FULL
    LOGICAL,        OPTIONAL,INTENT(OUT  ):: ISNEW
    !
    INTEGER:: IU
    LOGICAL:: EXIST
    !
    INQUIRE(FILE=FNAME, EXIST=EXIST)
    !
    IF(EXIST) THEN
        !
        IU = Z
        CALL GENERIC_OPEN(FNAME, IU, ACTION='READ', FORM='FORMATTED', ACCESS='SEQUENTIAL', STATUS='OLD', ASYNC=NO, BUFFER_BLOCKSIZE=0, BUFFER_COUNT=0, ERROR=EXIST)
        !
        IF(EXIST) THEN !ERROR FOUND
            FULL = FNAME
        ELSE
            CALL GET_FILE_NAME(IU,FULL,HAS_ERROR=EXIST)
            !
            IF(EXIST) FULL = FNAME
        END IF
    ELSE
        FULL = FNAME
    END IF
    !
    IF(PRESENT(ISNEW)) ISNEW = FULL /= FNAME
    !
  END SUBROUTINE
  !
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !
  SUBROUTINE ADD_UNIT_ARRAY_ENTRY(DF,IU,IS_BOM)
     CLASS(UNIT_ARRAY_BUILDER), INTENT(INOUT):: DF
     INTEGER,                   INTENT(IN   ):: IU
     LOGICAL, OPTIONAL,         INTENT(IN   ):: IS_BOM
     CHARACTER(:),ALLOCATABLE:: FNAM
     INTEGER:: I, ISTR, ISTP
     !
     CALL GET_FILE_NAME(IU,FNAM)
     !
     CALL DF%IU  %ADD(IU)
     CALL DF%FNAM%ADD(FNAM)
     !
     ISTR = ONE
     ISTP = LEN_TRIM(FNAM)
     DO I=ONE, ISTP-ONE
         IF(FNAM(I:I)=='\' .OR. FNAM(I:I)=='/') ISTR = I + ONE 
     END DO
     !
     CALL DF%BASE%ADD(FNAM(ISTR:ISTP))
     !
     IF(PRESENT(IS_BOM)) THEN
                          CALL DF%IS_BOM%ADD(IS_BOM)
     ELSE
                          CALL DF%IS_BOM%ADD(FALSE)
     END IF
     !
  END SUBROUTINE
  !
  SUBROUTINE CHECK_NAME_UNIT_ARRAY(DF,FNAM,IU,ISOPEN,IS_BOM)
     CLASS(UNIT_ARRAY_BUILDER), INTENT(INOUT):: DF
     CHARACTER(*),              INTENT(IN   ):: FNAM
     INTEGER,                   INTENT(  OUT):: IU
     LOGICAL,         OPTIONAL, INTENT(  OUT):: ISOPEN, IS_BOM
     !
     CALL FILENAME_TO_UNIT(FNAM,IU)
     !
     IF(IU /= Z) THEN
                   IF(DF%IU%IS_UNIQUE(IU)) IU = Z  !FILE HAS YET TO BE OPEN
     END IF
     !
     IF(PRESENT(ISOPEN)) ISOPEN = IU /= Z
     !
     IF(PRESENT(IS_BOM)) THEN
                         IF(IU /= Z) THEN
                                     IS_BOM = DF%IS_BOM%VEC( DF%IU%FIND(IU) )
                         ELSE
                                     IS_BOM = FALSE
                         END IF
     END IF
     !
  END SUBROUTINE
  !
  SUBROUTINE CHECK_BASE_UNIT_ARRAY(DF,BASENAME,IU,NOT_UNIQUE,IS_BOM)
     CLASS(UNIT_ARRAY_BUILDER), INTENT(INOUT):: DF
     CHARACTER(*),              INTENT(IN   ):: BASENAME
     INTEGER,                   INTENT(  OUT):: IU
     LOGICAL,         OPTIONAL, INTENT(  OUT):: NOT_UNIQUE, IS_BOM
     INTEGER:: P
     !
     IU = Z
     P  = DF%BASE%FIND(BASENAME)  !Set to zero if not found
     !
     IF ( P > Z ) THEN
          IU = DF%IU%VEC(P)
          IF(PRESENT(IS_BOM    )) IS_BOM = DF%IS_BOM%VEC( P )
          IF(PRESENT(NOT_UNIQUE)) NOT_UNIQUE = DF%BASE%COUNT(BASENAME) > ONE
     ELSE 
         IF(PRESENT(NOT_UNIQUE)) NOT_UNIQUE = FALSE
         IF(PRESENT(IS_BOM    )) IS_BOM     = FALSE
     END IF
     !
  END SUBROUTINE
  !
  PURE FUNCTION GET_IS_BOM_UNIT_ARRAY(DF, IU) RESULT(IS_BOM)
     CLASS(UNIT_ARRAY_BUILDER), INTENT(IN):: DF
     INTEGER,                   INTENT(IN):: IU
     INTEGER:: I
     LOGICAL:: IS_BOM
     !
     IS_BOM = FALSE
     !
     IF(IU /= Z) THEN
                 DO I=ONE, DF%IU%N
                           IF(IU == DF%IU%VEC(I)) THEN
                                                  IS_BOM = DF%IS_BOM%VEC(I)
                                                  EXIT
                           END IF
                 END DO
     END IF
     !
  END FUNCTION
  !
  SUBROUTINE PRINT_NAME_UNIT_ARRAY(DF,IOUT)
     CLASS(UNIT_ARRAY_BUILDER), INTENT(IN):: DF
     INTEGER,                   INTENT(IN):: IOUT
     CHARACTER(64):: LN
     INTEGER:: I, MX
     !
     IF(IOUT /= Z) THEN
        WRITE(IOUT,'(//A)') "LISTING OF DATAFILES CURRENTLY OPEN"
        !
        LN = BLNK
        MX = DF%BASE%MAX_LEN() + 11
        !
        LN          = "    UNIT  BASENAME"
        LN(MX+TWO:) = "FILE"
        WRITE(IOUT,'(//A)') TRIM(LN)
        !
        DO I=ONE, DF%IU%N
            WRITE(LN(1:8),'(I8)') DF%IU%VEC(I)
            LN(11:) = DF%BASE%VEC(I)%STR
            !
            WRITE(IOUT,'(A, 1x, A)') LN(1:MX), TRIM(DF%FNAM%STR(I))
        END DO
     END IF
     !
     WRITE(IOUT,"(A/)")
     !
  END SUBROUTINE
  !
  PURE FUNCTION GET_NAME_UNIT_ARRAY(DF) RESULT(NAMES)
     CLASS(UNIT_ARRAY_BUILDER), INTENT(IN):: DF
     CHARACTER(:), ALLOCATABLE:: NAMES
     CHARACTER(256):: LN
     INTEGER:: I, MX
     !
     NAMES = "LISTING OF DATAFILES CURRENTLY OPEN"//BLN
     LN = BLNK
     MX = DF%BASE%MAX_LEN() + 11
     !
     LN          = "    UNIT  BASENAME"
     LN(MX+TWO:) = "FILE"
     NAMES = NAMES//TRIM(LN)//NL
     !
     DO I=ONE, DF%IU%N
         WRITE(LN(1:8),'(I8)') DF%IU%VEC(I)
         LN(11:) = DF%BASE%VEC(I)%STR
         !
         NAMES = NAMES//LN(1:MX)//" "//TRIM(DF%FNAM%STR(I))//NL
     END DO
     !
  END FUNCTION
  !
  SUBROUTINE DESTROY_UNIT_ARRAY_ENTRY(DF)
     CLASS(UNIT_ARRAY_BUILDER), INTENT(INOUT):: DF
     INTEGER:: I, IERR
     !
     IF(DF%IU%N > Z) THEN
         !
         DO I=1,   DF%IU%N
             CLOSE(DF%IU%VEC(I), IOSTAT=IERR)
         END DO
         !
         CALL DF%IU    %DESTROY()
         CALL DF%IS_BOM%DESTROY()
         CALL DF%FNAM  %DESTROY()
         CALL DF%BASE  %DESTROY()
         !
     END IF
     !
  END SUBROUTINE
  !
  SUBROUTINE FINAL_CLOSE_GERNIC_INPUT_OUTPUT_DATAFILES(DF)
    TYPE(UNIT_ARRAY_BUILDER):: DF
    !
    CALL DESTROY_UNIT_ARRAY_ENTRY(DF)
    !
  END SUBROUTINE
  !
  SUBROUTINE CLOSE_GERNIC_INPUT_OUTPUT_DATAFILES()
    !
    CALL DESTROY_UNIT_ARRAY_ENTRY(DATAFILE_UNIT_NUMBER)
    !
  END SUBROUTINE
  !
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !  
  !  Write Data routines
  !
  SUBROUTINE WRITE_DATA_DIM1_DBL(VAR, IOUT, NAM, HEADER, FMT, TRANSPOSE)
    REAL(REAL64), DIMENSION(:), INTENT(IN):: VAR
    INTEGER,                    INTENT(IN):: IOUT
    CHARACTER(*), OPTIONAL,     INTENT(IN):: NAM
    LOGICAL,      OPTIONAL,     INTENT(IN):: TRANSPOSE, HEADER
    CHARACTER(*), OPTIONAL,     INTENT(IN):: FMT
    INTEGER:: DIM1, I, J, ISTP
    LOGICAL:: TS, HED
    CHARACTER(32):: TXT 
    CHARACTER(:), ALLOCATABLE:: FT, FMT_HED
    !
    TS = FALSE
    IF(PRESENT(TRANSPOSE)) TS = TRANSPOSE
    !
    HED = FALSE
    IF(PRESENT(HEADER)) HED = HEADER
    !
    IF(PRESENT(FMT)) THEN
        FT = '(*(1x '//TRIM(ADJUSTL(FMT))//'))'
        !
        IF(HED)  THEN
            WRITE(TXT, FT) VAR(1)
            J = LEN(TRIM(ADJUSTL(TXT)))
            WRITE(TXT, '(I32)') J
            FMT_HED = '(*(1x '//TRIM(ADJUSTL(TXT))//'))'
        END IF
    ELSE
        FT = '(*(1x ES13.6E2))'
        IF(HED) FMT_HED = '(*(1x I12))'
    END IF
    !
    DIM1 = SIZE(VAR, 1)
    !
    IF(HED) THEN
       IF(TS) THEN                 !Find formt of header
           WRITE(TXT, '(I32)') ONE
       ELSE
           WRITE(TXT, '(I32)') DIM1
       END IF
       !
       ISTP = LEN(TRIM(ADJUSTL(TXT)))
       !
       IF(PRESENT(NAM)) THEN
           I = LEN(NAM)
           IF(ISTP<I) ISTP = I
       END IF
    ELSEIF(PRESENT(NAM)) THEN
                         IF(NAM /= "") WRITE(IOUT,'(A)') NAM
    END IF
    !
    IF(TS) THEN
        IF(HED) THEN
            IF(PRESENT(NAM)) WRITE(IOUT,'(A)', ADVANCE=NO) NAM
            WRITE(IOUT, FMT_HED) ONE
        END IF
        !
        DO I=ONE, DIM1
            IF(HED) THEN
                WRITE(TXT, '(I32)') I
                TXT = ADJUSTL(TXT)
                WRITE(IOUT, '(A)', ADVANCE=NO) TXT(:ISTP)
            END IF
            !
            WRITE(IOUT, FT) VAR(I)
        END DO
    ELSE
        IF(HED) THEN
            IF(PRESENT(NAM)) WRITE(IOUT,'(A)', ADVANCE=NO) NAM
            WRITE(IOUT, FMT_HED) (I, I=ONE, DIM1)
            !
            WRITE(TXT, '(I32)') ONE
            TXT = ADJUSTL(TXT)
            WRITE(IOUT, '(A)', ADVANCE=NO) TXT(:ISTP)
        END IF
        !
        WRITE(IOUT, FT) VAR(:)
    END IF
    !
  END SUBROUTINE
  !
  !#########################################################################################################################
  !
  SUBROUTINE WRITE_DATA_DIM2_DBL(VAR, IOUT, NAM, HEADER, FMT, TRANSPOSE)
    REAL(REAL64), DIMENSION(:,:), INTENT(IN):: VAR
    INTEGER,                      INTENT(IN):: IOUT
    CHARACTER(*), OPTIONAL,       INTENT(IN):: NAM
    LOGICAL,      OPTIONAL,       INTENT(IN):: TRANSPOSE, HEADER
    CHARACTER(*), OPTIONAL,       INTENT(IN):: FMT
    INTEGER:: DIM1, DIM2, I, J, ISTP
    LOGICAL:: TS, HED
    CHARACTER(32):: TXT 
    CHARACTER(:), ALLOCATABLE:: FT, FMT_HED
    !
    TS = FALSE
    IF(PRESENT(TRANSPOSE)) TS = TRANSPOSE
    !
    HED = FALSE
    IF(PRESENT(HEADER)) HED = HEADER
    !
    IF(PRESENT(FMT)) THEN
        FT = '(*(1x '//TRIM(ADJUSTL(FMT))//'))'
        !
        IF(HED)  THEN
            WRITE(TXT, FT) VAR(1,1)
            J = LEN(TRIM(ADJUSTL(TXT)))
            WRITE(TXT, '(I32)') J
            FMT_HED = '(*(1x '//TRIM(ADJUSTL(TXT))//'))'
        END IF
    ELSE
        FT = '(*(1x ES13.6E2))'
        IF(HED) FMT_HED = '(*(1x I12))'
    END IF
    !
    DIM1 = SIZE(VAR, 1)
    DIM2 = SIZE(VAR, 2)
    !
    IF(HED) THEN
       IF(TS) THEN                 !Find formt of header
           WRITE(TXT, '(I32)') DIM2
       ELSE
           WRITE(TXT, '(I32)') DIM1
       END IF
       !
       ISTP = LEN(TRIM(ADJUSTL(TXT)))
       !
       IF(PRESENT(NAM)) THEN
           I = LEN(NAM)
           IF(ISTP<I) ISTP = I
       END IF
    ELSEIF(PRESENT(NAM)) THEN
                         IF(NAM /= "") WRITE(IOUT,'(A)') NAM
    END IF
    !
    IF(TS) THEN
        IF(HED) THEN
            IF(PRESENT(NAM)) WRITE(IOUT,'(A)', ADVANCE=NO) NAM
            WRITE(IOUT, FMT_HED) (I, I=ONE, DIM2)
        END IF
        !
        DO I=ONE, DIM1
            IF(HED) THEN
                WRITE(TXT, '(I32)') I
                TXT = ADJUSTL(TXT)
                WRITE(IOUT, '(A)', ADVANCE=NO) TXT(:ISTP)
            END IF
            !
            WRITE(IOUT, FT) (VAR(I,J), J=ONE, DIM2)
        END DO
    ELSE
        IF(HED) THEN
            IF(PRESENT(NAM)) WRITE(IOUT,'(A)', ADVANCE=NO) NAM
            WRITE(IOUT, FMT_HED) (I, I=ONE, DIM1)
        END IF
        !
        DO J=ONE, DIM2
            IF(HED) THEN
                WRITE(TXT, '(I32)') J
                TXT = ADJUSTL(TXT)
                WRITE(IOUT, '(A)', ADVANCE=NO) TXT(:ISTP)
            END IF
            !
            WRITE(IOUT, FT) VAR(:,J)
        END DO
    END IF
    !
  END SUBROUTINE
  !
  !#########################################################################################################################
  !
  SUBROUTINE WRITE_DATA_DIM3_DBL(VAR, IOUT, NAM, HEADER, FMT, TRANSPOSE, NO_LAY_HEAD)
    REAL(REAL64), DIMENSION(:,:,:), INTENT(IN):: VAR
    INTEGER,                      INTENT(IN):: IOUT
    CHARACTER(*), OPTIONAL,       INTENT(IN):: NAM
    LOGICAL,      OPTIONAL,       INTENT(IN):: TRANSPOSE, HEADER, NO_LAY_HEAD
    CHARACTER(*), OPTIONAL,       INTENT(IN):: FMT
    INTEGER:: DIM3, K, ISTP
    LOGICAL:: HED, LAY_HED
    CHARACTER(32):: TXT 
    !
    DIM3 = SIZE(VAR, 3)
    !
    LAY_HED = TRUE
    IF(PRESENT(NO_LAY_HEAD)) LAY_HED = .NOT. NO_LAY_HEAD
    !
    HED = FALSE
    IF(PRESENT(HEADER)) HED = HEADER
    !
    IF(PRESENT(NAM)) THEN
                     IF(NAM /= "") WRITE(IOUT,'(A)') NAM
    END IF
    !
    IF(.NOT. LAY_HED) THEN
        DO K=ONE, DIM3
            CALL WRITE_DATA_DIM2_DBL(VAR(:,:,K), IOUT, "", HEADER, FMT, TRANSPOSE)
        END DO
    ELSE
        WRITE(TXT, '(I32)') DIM3
        !
        ISTP = LEN(TRIM(ADJUSTL(TXT))) + FOUR !"LAY_"
        !
        DO K=ONE, DIM3
            WRITE(TXT, '(I32)') K
            TXT = "LAY_"//ADJUSTL(TXT)
            CALL WRITE_DATA_DIM2_DBL(VAR(:,:,K), IOUT, TXT(:ISTP), HEADER, FMT, TRANSPOSE)
        END DO
    END IF
    !
  END SUBROUTINE
END MODULE
!