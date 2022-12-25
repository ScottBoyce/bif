!
! CODE DEVELOPED BY SCOTT E BOYCE 
!                   CONTACT <seboyce@usgs.gov> or <Boyce@engineer.com>
!
! MODULE TIME_SERIES_INSTRUCTION   ==>  USE TIME_SERIES_FILE
!
MODULE TIME_SERIES_INSTRUCTION!, ONLY: TIME_SERIES_FILE
  USE CONSTANTS
  USE DATE_OPERATOR_INSTRUCTION,        ONLY: DATE_OPERATOR
  USE POST_KEY_SUB,                     ONLY: CHECK_FOR_POST_KEY
  USE FILE_IO_INTERFACE,                ONLY: READ_TO_DATA
  USE PARSE_WORD_INTERFACE,             ONLY: PARSE_WORD
  USE STRINGS,                          ONLY: GET_WORD, GET_DATE, GET_NUMBER, GET_INTEGER
  USE ERROR_INTERFACE,                  ONLY: STOP_ERROR
  USE WARNING_TYPE_INSTRUCTION,         ONLY: WARNING_TYPE
  USE GENERIC_INPUT_FILE_INSTRUCTION,   ONLY: GENERIC_INPUT_FILE
  USE GENERIC_BLOCK_READER_INSTRUCTION, ONLY: GENERIC_BLOCK_READER
  IMPLICIT NONE(TYPE, EXTERNAL)
  PRIVATE
  PUBLIC:: TIME_SERIES_FILE, TIME_SERIES_FILE_GROUP, LOAD_TIME_SERIES_BLOCK, LOAD_TIME_SERIES_BLOCK_POINTER
  !
  TYPE TSF_DATA
    DOUBLE PRECISION   :: TIM=DZ, DAT=DZ
    TYPE(DATE_OPERATOR):: DATE
  END TYPE
  !
  TYPE TIME_SERIES_FILE
      INTEGER:: P         = Z     !CURRENT LINE, IS SET TO ZERO IF WHEN NOT INITIALIZED
      LOGICAL:: BINARY    = FALSE
      LOGICAL:: AT_NEXT   = FALSE
      LOGICAL:: MONTHDAY  = FALSE
      INTEGER:: MD_FLAG   = Z        ! =0 Within File, 1 PREV-CUR stradles EOF, 2 CUR-NEXT stradles EOF
      INTEGER:: OPT= Z    ! OPT => 0=INTERP; 1=STEP, 2=NEAR
      INTEGER:: N  = Z    ! SIZE OF FILE
      INTEGER:: IOUT = Z
      CHARACTER(:), ALLOCATABLE:: NAM
      CHARACTER(75):: LINE
      !
      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE:: T500
      INTEGER:: N500 = Z
      !
      !INTEGER::          CHK     = inf_I ! INDEX TO CHECK IF FASTER TO MOVE TO START OF FILE
      !DOUBLE PRECISION:: CHK_TIM = ninf  !TIME TO CHECK AGAINST IF FASTER TO MOVE TO FRONT OF FILE
      !
      TYPE(GENERIC_INPUT_FILE):: FL
      !TYPE(DATE_OPERATOR):: DATE  !SCRATCH SPACE FOR LOADING TIME VALUE --Made availible to each location
      !
      !DOUBLE PRECISION:: SFAC = UNO
      TYPE(TSF_DATA), POINTER:: PREV => NULL()
      TYPE(TSF_DATA), POINTER:: CUR  => NULL()
      TYPE(TSF_DATA), POINTER:: NEXT => NULL()
      !
      TYPE(TSF_DATA), POINTER:: TMP => NULL()
      !
      CONTAINS
      !
      GENERIC::              INIT     => INITIALIZE_TIME_SERIES_FILE, INITIALIZE_TIME_SERIES_FILE_LINE, INITIALIZE_TIME_SERIES_FILE_VALUE!(LINE,LLOC,IOUT,IN,READ_NAME,[DEFAULT_OPT]) or (LINE,[READ_NAME],[DEFAULT_OPT]) or (VALUE)
      PROCEDURE, PASS(TSF):: OPENED   => TIME_SERIES_FILE_IN_USE
      PROCEDURE, PASS(TSF):: MOVE     => MOVE_TIME_SERIES_TOO
      GENERIC::              GET      => LOOKUP_TIME_SERIES_VALUE_DATE, LOOKUP_TIME_SERIES_VALUE_DYEAR, LOOKUP_TIME_SERIES_VALUE_DATE_DATE
      PROCEDURE, PASS(TSF):: IS_INIT
      PROCEDURE, PASS(TSF):: TIME_MEAN   !(TIME0, TIME, VALUE)
      PROCEDURE, PASS(TSF):: TIME_SUM    !(TIME0, TIME, VAL, DELT)
      PROCEDURE, PASS(TSF):: TIME_MAX    !(TIME0, TIME, VALUE)
      PROCEDURE, PASS(TSF):: TIME_MIN    !(TIME0, TIME, VALUE)
      PROCEDURE, PASS(TSF):: INTERVAL_SUM!(TIME0, TIME, VALUE)
      PROCEDURE, PASS(TSF):: SET_OPTION  !(OPT)
      PROCEDURE, PASS(TSF):: GET_N_SET_OPTION !(LINE, LLOC, ISTART, ISTOP, [CONST], [DEFAULT_OPT])
      PROCEDURE, PASS(TSF):: GET_OPTION       !(LINE, LLOC, ISTART, ISTOP, [CONST])
      PROCEDURE, PASS(TSF):: PRINT_OPTION !([OPT])
      !
      PROCEDURE, PASS(TSF),PRIVATE::     LOOKUP_TIME_SERIES_VALUE_DATE_DATE !(DATE, VALUE, DATE0)
      PROCEDURE, PASS(TSF),PRIVATE::     LOOKUP_TIME_SERIES_VALUE_DATE      !(DATE, VALUE)
      PROCEDURE, PASS(TSF),PRIVATE::     LOOKUP_TIME_SERIES_VALUE_DYEAR     !(TIME, VALUE)
      PROCEDURE, PASS(TSF),PRIVATE::     INITIALIZE_TIME_SERIES_FILE        !(LINE,LLOC,IOUT,IN,READ_NAME,[VALUE],[DEFAULT_OPT],[NAME])
      PROCEDURE, PASS(TSF),PRIVATE::     INITIALIZE_TIME_SERIES_FILE_LINE   !(LINE,[READ_NAME],[DEFAULT_OPT],[NAME])
      PROCEDURE, PASS(TSF),PRIVATE::     INITIALIZE_TIME_SERIES_FILE_VALUE  !(VALUE)
      !
      PROCEDURE, PASS(TSF):: DESTROY  => DEALLOCATE_TIME_SERIES_FILE
      FINAL:: FINAL_DEALLOCATE_TIME_SERIES_FILE
      !
  END TYPE
  !
  TYPE TIME_SERIES_FILE_GROUP
      INTEGER:: NFIL = Z
      TYPE(TIME_SERIES_FILE),DIMENSION(:),ALLOCATABLE:: TSF
      INTEGER,               DIMENSION(:),ALLOCATABLE:: ID
      !
      CONTAINS
      !
      PROCEDURE, PASS(TSG):: INIT    => INITIALIZE_TIME_SERIES_GROUP
      PROCEDURE, PASS(TSG):: DESTROY => DEALLOCATE_TIME_SERIES_GROUP
      FINAL::                FINAL_DEALLOCATE_TIME_SERIES_GROUP
  END TYPE
  !  
  CONTAINS 
  !
  PURE FUNCTION IS_INIT(TSF)
    CLASS(TIME_SERIES_FILE),  INTENT(IN):: TSF
    LOGICAL:: IS_INIT
    !
    IS_INIT = TSF%P > Z
    !
  END FUNCTION
  !
  SUBROUTINE INITIALIZE_TIME_SERIES_GROUP(TSG, BL, READ_ID, READ_NAME)
    CLASS(TIME_SERIES_FILE_GROUP), INTENT(INOUT):: TSG
    CLASS(GENERIC_BLOCK_READER),   INTENT(INOUT):: BL
    LOGICAL, OPTIONAL,             INTENT(IN   ):: READ_ID, READ_NAME
    !
    INTEGER:: I, LLOC,ISTART,ISTOP
    LOGICAL:: GET_ID
    !
    GET_ID = FALSE
    IF(PRESENT(READ_ID)) GET_ID = READ_ID
    !
    CALL DEALLOCATE_TIME_SERIES_GROUP(TSG)
    !
    TSG%NFIL = BL%NLINE
    !
    IF(TSG%NFIL > Z) THEN
       !
       ALLOCATE(TSG%TSF(TSG%NFIL))
       !
       IF(GET_ID) ALLOCATE(TSG%ID(TSG%NFIL))
       !
       CALL BL%START()
       !
       DO I = ONE, TSG%NFIL
                       !
                       LLOC=ONE
                       IF(GET_ID) CALL GET_INTEGER(BL%LINE,LLOC,ISTART,ISTOP,BL%IOUT,BL%IU,TSG%ID(I),MSG='TIME SERIES FILE FAILED TO LOAD INTEGER ID BEFORE TIME SERIES FILE ITSELF. THIS INPUT REQUIRES YOU SPECIFY AN INTEGER FIRST, THEN THE ACTUAL TIME SERIES FILE.')
                       !
                       CALL TSG%TSF(I)%INIT(BL%LINE,LLOC,BL%IOUT,BL%IU,READ_NAME=READ_NAME)
                       !
                       CALL BL%NEXT()    !NOVE TO NEXT TIME SERIES FILE
       END DO
    END IF
    !
  END SUBROUTINE
  !
  IMPURE ELEMENTAL SUBROUTINE DEALLOCATE_TIME_SERIES_GROUP(TSG)
    CLASS(TIME_SERIES_FILE_GROUP), INTENT(INOUT):: TSG
    TSG%NFIL = Z
    IF(ALLOCATED(TSG%TSF)) DEALLOCATE(TSG%TSF)
    IF(ALLOCATED(TSG%ID )) DEALLOCATE(TSG%ID)
  END SUBROUTINE
  !
  IMPURE ELEMENTAL SUBROUTINE FINAL_DEALLOCATE_TIME_SERIES_GROUP(TSG)
    TYPE(TIME_SERIES_FILE_GROUP), INTENT(INOUT):: TSG
    CALL DEALLOCATE_TIME_SERIES_GROUP(TSG)
  END SUBROUTINE
  !
  SUBROUTINE LOAD_TIME_SERIES_BLOCK(BL,TSF, NFIL, ID, READ_NAME)
    CLASS(GENERIC_BLOCK_READER),                     INTENT(INOUT):: BL
    TYPE(TIME_SERIES_FILE),DIMENSION(:),ALLOCATABLE, INTENT(INOUT):: TSF
    INTEGER,                                         INTENT(  OUT):: NFIL
    INTEGER, OPTIONAL,     DIMENSION(:),ALLOCATABLE, INTENT(  OUT):: ID
    LOGICAL, OPTIONAL,                               INTENT(IN   ):: READ_NAME
    !
    INTEGER:: I, LLOC,ISTART,ISTOP
    !
    NFIL = BL%NLINE
    !
    IF(NFIL > Z) THEN
       IF(ALLOCATED(TSF)) DEALLOCATE(TSF)
       !
       ALLOCATE(TSF(NFIL))
       IF(PRESENT(ID)) THEN
           IF(ALLOCATED(ID)) DEALLOCATE(ID)
           ALLOCATE(ID(NFIL))
       END IF
       !
       CALL BL%START()
       !
       DO I = ONE, NFIL
                       !
                       LLOC=ONE
                       IF(PRESENT(ID)) CALL GET_INTEGER(BL%LINE,LLOC,ISTART,ISTOP,BL%IOUT,BL%IU,ID(I),MSG='TIME SERIES FILE FAILED TO LOAD INTEGER ID BEFORE TIME SERIES FILE ITSELF. THIS INPUT REQUIRES YOU SPECIFY AN INTEGER FIRST, THEN THE ACTUAL TIME SERIES FILE.')
                       !
                       CALL TSF(I)%INIT(BL%LINE,LLOC,BL%IOUT,BL%IU,READ_NAME=READ_NAME)
                       !
                       CALL BL%NEXT()    !NOVE TO NEXT TIME SERIES FILE
       END DO
    ELSE
       NFIL = Z
       IF(ALLOCATED(TSF)) DEALLOCATE(TSF)
       IF(PRESENT(ID)) THEN; IF(ALLOCATED(ID)) DEALLOCATE(ID)
       END IF
    END IF
    !
  END SUBROUTINE
  !
  SUBROUTINE LOAD_TIME_SERIES_BLOCK_POINTER(BL,TSF, NFIL, ID, READ_NAME)
    CLASS(GENERIC_BLOCK_READER),                             INTENT(INOUT):: BL
    TYPE(TIME_SERIES_FILE),DIMENSION(:),POINTER, CONTIGUOUS, INTENT(INOUT):: TSF
    INTEGER,                                                 INTENT(  OUT):: NFIL
    INTEGER, OPTIONAL,     DIMENSION(:),POINTER, CONTIGUOUS, INTENT(  OUT):: ID
    LOGICAL, OPTIONAL,                                       INTENT(IN   ):: READ_NAME
    !
    INTEGER:: I, LLOC,ISTART,ISTOP
    !
    NFIL = BL%NLINE
    !
    IF(NFIL > Z) THEN
       IF(ASSOCIATED(TSF)) DEALLOCATE(TSF,STAT=I)
       !
       ALLOCATE(TSF(NFIL))
       IF(PRESENT(ID)) THEN
           IF(ASSOCIATED(ID)) DEALLOCATE(ID,STAT=I)
           ALLOCATE(ID(NFIL))
       END IF
       !
       CALL BL%START()
       !
       DO I = ONE, NFIL
                       !
                       LLOC=ONE
                       IF(PRESENT(ID)) CALL GET_INTEGER(BL%LINE,LLOC,ISTART,ISTOP,BL%IOUT,BL%IU,ID(I),MSG='TIME SERIES FILE FAILED TO LOAD INTEGER ID BEFORE TIME SERIES FILE ITSELF. THIS INPUT REQUIRES YOU SPECIFY AN INTEGER FIRST, THEN THE ACTUAL TIME SERIES FILE.')
                       !
                       CALL TSF(I)%INIT(BL%LINE,LLOC,BL%IOUT,BL%IU,READ_NAME=READ_NAME)
                       !
                       CALL BL%NEXT()    !NOVE TO NEXT TIME SERIES FILE
       END DO
    ELSE
       NFIL = Z
       IF(ASSOCIATED(TSF)) DEALLOCATE(TSF,STAT=I)
       IF(PRESENT(ID)) THEN; IF(ASSOCIATED(ID)) DEALLOCATE(ID,STAT=I)
       END IF
    END IF
    !
  END SUBROUTINE
  !  
  IMPURE ELEMENTAL SUBROUTINE INITIALIZE_TIME_SERIES_FILE_VALUE(TSF,VALUE)
    CLASS(TIME_SERIES_FILE),  INTENT(INOUT):: TSF
    DOUBLE PRECISION,OPTIONAL,INTENT(IN   ):: VALUE
    INTEGER:: LLOC
    !
    LLOC = ONE
    CALL INITIALIZE_TIME_SERIES_FILE(TSF,'SKIP',LLOC,Z,Z,FALSE,VALUE)
    !
  END SUBROUTINE
  !  
  IMPURE ELEMENTAL SUBROUTINE INITIALIZE_TIME_SERIES_FILE_LINE(TSF,LINE,READ_NAME,DEFAULT_OPT,NAME)
    CLASS(TIME_SERIES_FILE),  INTENT(INOUT):: TSF
    CHARACTER(*),             INTENT(IN   ):: LINE
    LOGICAL     ,    OPTIONAL,INTENT(IN   ):: READ_NAME
    CHARACTER(*),    OPTIONAL,INTENT(IN   ):: DEFAULT_OPT !IF NO KEYWORD OPTION IS FOUND THIS STRING WILL BE PARSED
    CHARACTER(*),    OPTIONAL,INTENT(IN   ):: NAME        !IF PRESENT SETS NAME TO THIS STRING
    LOGICAL:: REED_NAME
    INTEGER:: LLOC
    !
    IF(PRESENT(READ_NAME)) THEN
        REED_NAME = READ_NAME
    ELSE
        REED_NAME = FALSE
    END IF
    !
    LLOC = ONE
    CALL INITIALIZE_TIME_SERIES_FILE(TSF,LINE,LLOC,Z,Z,REED_NAME, DEFAULT_OPT=DEFAULT_OPT, NAME=NAME)
    !
  END SUBROUTINE
  !  
  IMPURE ELEMENTAL SUBROUTINE INITIALIZE_TIME_SERIES_FILE(TSF,LINE,LLOC,IOUT,IN,READ_NAME,VALUE,DEFAULT_OPT,NAME)
    CLASS(TIME_SERIES_FILE),  INTENT(INOUT):: TSF
    CHARACTER(*),             INTENT(IN   ):: LINE
    INTEGER,                  INTENT(INOUT):: LLOC
    INTEGER,                  INTENT(IN   ):: IOUT,IN  !Output file to write error msg too, infile to report error from. IN not used other then error msg
    !
    LOGICAL,         OPTIONAL,INTENT(IN   ):: READ_NAME
    DOUBLE PRECISION,OPTIONAL,INTENT(IN   ):: VALUE      !IF PRESENT THEN TIME SERIES ONLY SEES THIS VALUE
    CHARACTER(*),    OPTIONAL,INTENT(IN   ):: DEFAULT_OPT!IF NO KEYWORD OPTION IS FOUND THIS STRING WILL BE PARSED
    CHARACTER(*),    OPTIONAL,INTENT(IN   ):: NAME       !IF PRESENT SETS NAME TO THIS STRING
    !
    INTEGER:: I, K, ISTART,ISTOP
    DOUBLE PRECISION:: CONST
    LOGICAL:: EOF
    !
    CALL DEALLOCATE_TIME_SERIES_FILE(TSF)
    !
    ALLOCATE(TSF%PREV, TSF%CUR, TSF%NEXT)
    TSF%IOUT = IOUT
    !
    IF(PRESENT(VALUE)) THEN
        !
        CALL SET_CONSTANT_VALUE(TSF, VALUE)
        !
        IF(PRESENT(NAME)) THEN
                       ALLOCATE(TSF%NAM, SOURCE = NAME)
        ELSE
                       ALLOCATE(TSF%NAM, SOURCE = 'n')
        END IF
        RETURN
    END IF!-----------------------------------------------------------------------------------
    !
    EOF = FALSE
    IF(PRESENT(READ_NAME)) EOF = READ_NAME
    IF(EOF) THEN
                      CALL GET_WORD(LINE,LLOC,ISTART,ISTOP,TSF%NAM,IS_ALLOC=TRUE,COM_STOP=TRUE)
    ELSE
                      ALLOCATE(TSF%NAM, SOURCE = 'n')
    END IF
    !
    IF(PRESENT(NAME)) THEN
                   DEALLOCATE(TSF%NAM)
                   ALLOCATE(TSF%NAM, SOURCE = NAME)
    END IF
    !
    CALL GET_N_SET_OPTION(TSF, LINE, LLOC, ISTART, ISTOP, IN, CONST, DEFAULT_OPT)
    !
    IF(TSF%OPT > NEG) THEN
        !
        CALL TSF%FL%OPEN(LINE, LLOC, IOUT, IN, NO_INTERNAL=TRUE, NEW_UNIT=TRUE, MSG='ATTEMPED TO OPEN AND LOAD A TIME_SERIES_FILE') !IF DATAFILE OR EXTERNAL REOPEN FILE TO CREATE NEW UNIT NUMBER
        CALL TSF%FL%COUNT_LINES(TSF%N)
        !
        IF(TSF%FL%IS_CONSTANT) THEN               !Should never be true cause it is caught by call GET_N_SET_OPTION
                               TSF%OPT = NINER
                               CONST = TSF%FL%CONST
        END IF
        !
    ELSEIF(TSF%OPT == NINER) THEN
        !
        TSF%FL%NULL_FILE = FALSE
        CALL CHECK_FOR_POST_KEY(LLOC,LINE,IN,IOUT,I,K,TSF%FL%SCALE) !CHECK FOR SCALE FACTORS AFTER CONSTANT
    ELSE
        TSF%FL%NULL_FILE = TRUE !TSF%OPT = NEG SO ITS A SKIPPED FILE
    END IF
    !
    IF(TSF%FL%NULL_FILE) THEN
        !
        CALL SET_CONSTANT_VALUE(TSF, DZ)
        TSF%OPT = NEG
        !
    ELSEIF(TSF%OPT == NINER) THEN
        !
        CALL SET_CONSTANT_VALUE(TSF, CONST)
        TSF%FL%NULL_FILE = TRUE
        !
    ELSEIF(TSF%N == ONE) THEN !ONLY ONE LINE SO TREAT AS CONSTANT
        !
        CALL TSF%FL%READ(TSF%LINE) !MOVE TO FIRST LOCATION
        LLOC = ONE
        CALL GET_DATE  (TSF%LINE,LLOC,ISTART,ISTOP,TSF%IOUT,TSF%FL%IU,TSF%PREV%DATE, ONLY_DYEAR=TRUE ,MSG='FAILED TO LOAD TIME PART (FIRST NUMBER) OF THE TIME SERIES INPUT FILE.')
        CALL GET_NUMBER(TSF%LINE,LLOC,ISTART,ISTOP,TSF%IOUT,TSF%FL%IU,        CONST,                  MSG='FAILED TO LOAD DATA/VALUE PART (SECOND NUMBER) OF THE TIME SERIES INPUT FILE.')
        CALL SET_CONSTANT_VALUE(TSF, CONST)
        CALL TSF%FL%CLOSE()
        !
    ELSEIF(TSF%N < ONE) THEN
                            CALL STOP_ERROR( LINE=LINE, INFILE=IN, OUTPUT=IOUT, MSG= 'TIME SERIES INPUT ERROR: THE TIME SERIES FILE APPEARS TO BE EMPTY? OR THE FILE WAS OPENED WITH BINARY FORMAT.') 
    ELSE
        TSF%N500 = TSF%N / QUIN
        !
        IF(TSF%N500 > Z)  ALLOCATE(TSF%T500(TSF%N500+ONE), SOURCE=inf)
        !
        !ALLOCATE(CHARACTER(75)::TSF%LINE)
        !
        CALL TSF%FL%READ(TSF%LINE) !MOVE TO FIRST LOCATION
        !
        !CHECK IF MONTH/DAY TIME INPUT
        !
        LLOC = ONE
        CALL GET_DATE(TSF%LINE,LLOC,ISTART,ISTOP,TSF%IOUT,TSF%FL%IU,TSF%PREV%DATE, ONLY_DYEAR=TRUE, MSG='FAILED TO LOAD TIME PART (FIRST NUMBER) OF THE TIME SERIES INPUT FILE.')
        TSF%MONTHDAY = TSF%PREV%DATE%MONTH_DAY
        !
        ! CHECK FOR CONTIGUOUS INPUT
        !
        DO ISTART=TWO, TSF%N !ALREADY LOADED FIRST LINE
            CALL TSF%FL%READ(TSF%LINE,CNT=ISTOP,EOF=EOF,NOSHIFT=TRUE)
            !CALL READ_TO_DATA(TSF%LINE,TSF%FL%IU,IOUT,CNT=ISTOP,EOF=EOF,NOSHIFT=TRUE)
            IF(EOF .OR. ISTOP > ONE) THEN
                CALL STOP_ERROR( LINE=TSF%LINE, INFILE=TSF%FL%IU, OUTPUT=IOUT, &
                                           MSG= 'TIME SERIES INPUT ERROR: NO COMMENTS OR BLANK LINES ARE ALLOWED WITHIN THE TIME SERIES DATA.'//NL//      &
                                                'THEY ARE ALLOWED AT THE TOP OF A TIME SERIES FILE BEFORE THE FIRST DATA ENTRY (e.g. TIME DATA),'//NL//   &
                                                'AFTER THE LAST DATA ENTRY AND TO THE RIGHT OF A DATA ENTRY.'//BLN//                                      &
                                                'THE FOLLOWING IS AN AN EXAMPLE OF WHAT IS ALLOWED:'//BLN//REPEAT('-',50)//NL//                         &
                                                '# COMMENT '//NL//'   '//NL//'  # START OF DATA'//NL//'12/31/1999  1.0'//NL//                             &
                                                '1/1/2000  2.0   # Y2K'//NL//'1/5/2000  2.00   # Comment'//NL//'  '//NL//                                 &
                                                '# end of data input'//NL//' '//NL//'#comment'//NL//REPEAT('-',50)//BLN//                                                     &
                                                'THE FOLLOWING IS AN EXAMPLE OF WHAT WILL NOT WORK DUE TO BAD COMMENT AND BAD BLANK LINE LOCATION:'//BLN//REPEAT('-',50)//NL//  &
                                                '# START OF DATA'//NL//'12/31/1999  1.0'//NL//'# BAD COMMENT LOCATION'//NL//'2000  2.0   # Y2K'//NL//'  '//NL//'1/5/2000  2.00'//NL//REPEAT('-',50) ) 
            END IF
        END DO
        !
        ! CHECK FOR DATES IN PROPERT SEQUENCE
        !
        CALL TSF%FL%REWIND()
        !
        ! CHECK FOR CONTIGUOUS INPUT AND SET UP N500 POINTERS
        !
        CALL READ_TO_DATA(TSF%LINE,TSF%FL%IU,IOUT)
        LLOC = ONE
        CALL GET_DATE(TSF%LINE,LLOC,ISTART,ISTOP,TSF%IOUT,TSF%FL%IU,TSF%PREV%DATE, ONLY_DYEAR=TRUE, MSG='FAILED TO LOAD TIME PART (FIRST NUMBER) OF THE TIME SERIES INPUT FILE.')
        !
        K = ONE
        DO I = TWO, TSF%N !ALREADY LOADED FIRST LINE
            CALL READ_TO_DATA(TSF%LINE,TSF%FL%IU,IOUT)
            LLOC = ONE
            CALL GET_DATE(TSF%LINE,LLOC,ISTART,ISTOP,TSF%IOUT,TSF%FL%IU,TSF%CUR%DATE, ONLY_DYEAR=TRUE, MSG='FAILED TO LOAD THE TIME PART (FIRST NUMBER) OF THE TIME SERIES INPUT FILE ON THE AFORMENTIONED LINE. PLEASE CHECK THE DATE FORMAT.')
            !
            IF(TSF%CUR%DATE < TSF%PREV%DATE) THEN
                CALL STOP_ERROR( LINE=TSF%LINE, INFILE=TSF%FL%IU, OUTPUT=IOUT, MSG= 'TIME SERIES INPUT ERROR: THE TIME COLUMN (FIRST COLUMN) MUST BE IN CHRONOLOGICAL ORDER.'//NL//'THE PRESENTED ERROR LINE CONTAINS A DATE THAT IS EARLIAR THEN THE PREVIOUS LINE.'//NL//'IF ONLY USING MONTH AND DAY OR DECIMAL YEAR, THEN THOSE MUST BE IN CHRONOLOGICAL ORDER TOO!.') 
            END IF
            !
            IF(I > 499) THEN
                !
                IF(MOD(I, QUIN) == Z) THEN
                      TSF%T500(K) = TSF%CUR%DATE%DYEAR
                      K = K + ONE
                END IF
            END IF
            !
            TSF%TMP  => TSF%PREV
            !
            TSF%PREV => TSF%CUR
            TSF%CUR  => TSF%TMP
            !
            TSF%TMP  => NULL()
            !
        END DO
        !
        CALL MOVE_TO_START_OF_FILE(TSF)
        !
    END IF
    !
  END SUBROUTINE
  !
  PURE SUBROUTINE SET_CONSTANT_VALUE(TSF, VALUE)
    CLASS(TIME_SERIES_FILE),  INTENT(INOUT):: TSF
    DOUBLE PRECISION, INTENT(IN):: VALUE
    !
    TSF%OPT  = NINER   !CONSTANT VALUE Option
    TSF%N    = THREE
    TSF%P    = THREE
    TSF%N500 = Z
    !
    TSF%MONTHDAY = FALSE
    TSF%AT_NEXT  = TRUE
    !
    TSF%PREV%TIM = ninf * HALF
    TSF%CUR %TIM =  inf * HALF
    TSF%NEXT%TIM =  inf * HALF
    !
    TSF%PREV%DAT = VALUE
    TSF%CUR %DAT = VALUE
    TSF%NEXT%DAT = VALUE
    !
  END SUBROUTINE
  !
  PURE SUBROUTINE SET_OPTION(TSF, OPT)
    CLASS(TIME_SERIES_FILE),  INTENT(INOUT):: TSF
    INTEGER, INTENT(IN):: OPT
    IF(TSF%OPT > Z) TSF%OPT = OPT  !FILE IS CONSTANT VALUE SO CANNOT OFFER OTHER OPTIONS
  END SUBROUTINE
  !
  SUBROUTINE GET_N_SET_OPTION(TSF, LINE, LLOC, ISTART, ISTOP, IN, CONST, DEFAULT_OPT)
    CLASS(TIME_SERIES_FILE),  INTENT(INOUT):: TSF
    CHARACTER(*),             INTENT(IN   ):: LINE
    INTEGER,                  INTENT(INOUT):: LLOC, ISTART, ISTOP
    INTEGER,         OPTIONAL,INTENT(IN   ):: IN
    DOUBLE PRECISION,OPTIONAL,INTENT(INOUT):: CONST
    CHARACTER(*),    OPTIONAL,INTENT(IN   ):: DEFAULT_OPT !IF NO KEYWORD OPTION IS FOUND THIS STRING WILL BE PARSED
    INTEGER:: N
    !
    CALL GET_OPTION(TSF, LINE, LLOC, ISTART, ISTOP, TSF%OPT, CONST)
    !CALL SET_OPTION(TSF, OPT)
    !
    IF(TSF%OPT == -2) CALL STOP_ERROR( LINE=LINE, INFILE=IN, OUTPUT=TSF%IOUT, MSG= 'TIME SERIES INPUT ERROR: FOUND KEYWORD "CONSTANT" FOR TIME SERIES FILE,'//NL//'BUT THIS INPUT THAT USES TIME SERIES FILES DOES NOT SUPPORT THE COSTANT OPTION.'//NL//'PLEASE CHOOSE ONE OF THE FOLLOWING KEYWORDS: '//NL//'INTERPOLATE'//NL//'STEP_FUNCTION'//NL//'NEXT_VALUE'//NL//'NEAREST'//NL//'TIME_MEAN'//NL//'MAX'//NL//'MIN'//NL//'SUM'//NL//'DAY_SUM'//NL//'TIME_MEAN_LOOK_BACK'//NL//'SKIP'//NL//'BEFORE SPECIFYING THE FILE LOCATION/NAME.') 
    !
    IF(TSF%OPT == -3) THEN     !OPTION NOT RECOGNIZED CHECK IF DEFAULT_OPT IS PROVIDED
        TSF%OPT = -2
        N = ONE
        IF(PRESENT(DEFAULT_OPT)) CALL GET_OPTION(TSF, DEFAULT_OPT, N, ISTART, ISTOP, TSF%OPT)
    END IF
    !
    IF(TSF%OPT == -2) CALL STOP_ERROR( LINE=LINE, INFILE=IN, OUTPUT=TSF%IOUT, MSG= 'TIME SERIES INPUT ERROR: FAILED TO IDENTIFY KEYWORD THAT DETERMINES HOW TO INTERPRET TIME SERIES DATA.'//NL//'THE FOLLOWING ARE ACCEPTIBLE KEYWORDS THAT MUST APPEAR BEFORE THE GENERIC_INPUT_FILE LOCATION:'//NL//'INTERPOLATE'//NL//'STEP_FUNCTION'//NL//'NEXT_VALUE'//NL//'NEAREST'//NL//'TIME_MEAN'//NL//'MAX'//NL//'MIN'//NL//'SUM'//NL//'DAY_SUM'//NL//'TIME_MEAN_LOOK_BACK'//NL//'SKIP'//NL//'and the keyword CONSTANT followed single number') 
    !
  END SUBROUTINE
  !
  SUBROUTINE GET_OPTION(TSF, LINE, LLOC, ISTART, ISTOP, OPT, CONST)
    CLASS(TIME_SERIES_FILE),  INTENT(IN   ):: TSF
    CHARACTER(*),             INTENT(IN   ):: LINE
    INTEGER,                  INTENT(INOUT):: LLOC, ISTART, ISTOP
    INTEGER,         OPTIONAL,INTENT(INOUT):: OPT
    DOUBLE PRECISION,OPTIONAL,INTENT(INOUT):: CONST
    CHARACTER(:), ALLOCATABLE:: WORD
    INTEGER:: N
    !
    N = LLOC
    CALL GET_WORD(LINE,LLOC,ISTART,ISTOP,WORD,IS_ALLOC=TRUE,COM_STOP=TRUE)
    !
    SELECT CASE(WORD)
    CASE('INTERP','INTERPOLATE'   ); OPT = Z
    CASE('STEP','STEP_FUNCTION'   ); OPT = ONE
    CASE('NEXT','NEXT_VALUE'      ); OPT = TWO
    CASE('NEAR','NEAREST'         ); OPT = THREE
    CASE('TIME_MEAN'              ); OPT = TEN
    CASE('MAX'                    ); OPT = 11
    CASE('MIN'                    ); OPT = 12
    CASE('SUM'                    ); OPT = 13
    CASE('DAY_SUM'                ); OPT = 14
    CASE('TIME_MEAN_LOOK_BACK'    ); OPT = 15  !NOT COMPLETE
    CASE('SKIP','NAN','NULL','NUL'); OPT = NEG
    CASE('CONSTANT')
                                     OPT = NINER
                                     IF(PRESENT(CONST)) THEN
                                         CALL GET_NUMBER(LINE,LLOC,ISTART,ISTOP,TSF%IOUT,Z,CONST,MSG='FAILED TO LOAD CONSTANT VALUE AFTER KEYWORD "CONSTANT".')
                                     ELSE
                                         OPT = -2
                                     END IF
    CASE DEFAULT
                                     OPT  = -3  !DID NOT IDENTIFY NAME
                                     LLOC = N
    END SELECT 
    !
    DEALLOCATE(WORD)
  END SUBROUTINE
  !
  PURE FUNCTION PRINT_OPTION(TSF,OPT) RESULT (WORD)
    CLASS(TIME_SERIES_FILE),  INTENT(IN):: TSF
    INTEGER,         OPTIONAL,INTENT(IN):: OPT
    CHARACTER(:), ALLOCATABLE:: WORD
    INTEGER:: OP
    !
    IF(PRESENT(OPT)) THEN
        OP = OPT
    ELSE
        OP = TSF%OPT
    END IF
    !
    SELECT CASE(OP)
    CASE(Z    ); WORD = 'INTERPOLATE'   
    CASE(ONE  ); WORD = 'STEP_FUNCTION' 
    CASE(TWO  ); WORD = 'NEXT_VALUE'    
    CASE(THREE); WORD = 'NEAREST'       
    CASE(TEN  ); WORD = 'TIME_MEAN'   
    CASE(11   ); WORD = 'MAX'           
    CASE(12   ); WORD = 'MIN'           
    CASE(13   ); WORD = 'SUM'
    CASE(14   ); WORD = 'DAY_SUM'
    CASE(15   ); WORD = 'TIME_MEAN_LOOK_BACK'  !NOT COMPLETE
    CASE(NINER); WORD = 'CONSTANT'
    CASE(NEG  ); WORD = 'SKIP'         
    CASE DEFAULT
                WORD = 'OPT_ERROR'
    END SELECT
    !
  END FUNCTION
  !
  SUBROUTINE LOOKUP_TIME_SERIES_VALUE_DATE(TSF, DATE, VALUE)
  CLASS(TIME_SERIES_FILE),      INTENT(INOUT):: TSF
  TYPE(DATE_OPERATOR),          INTENT(IN   ):: DATE
  DOUBLE PRECISION,             INTENT(  OUT):: VALUE
  !
  CALL LOOKUP_TIME_SERIES_VALUE_DYEAR(TSF, DATE%DYEAR, VALUE)
  !
  END SUBROUTINE
  !
  SUBROUTINE LOOKUP_TIME_SERIES_VALUE_DATE_DATE(TSF, DATE, VALUE, DATE0)
  CLASS(TIME_SERIES_FILE),      INTENT(INOUT):: TSF
  TYPE(DATE_OPERATOR),          INTENT(IN   ):: DATE
  DOUBLE PRECISION,             INTENT(  OUT):: VALUE
  TYPE(DATE_OPERATOR),          INTENT(IN   ):: DATE0
  !
  CALL LOOKUP_TIME_SERIES_VALUE_DYEAR(TSF, DATE%DYEAR, VALUE, DATE0%DYEAR)
  !
  END SUBROUTINE
  !
  SUBROUTINE LOOKUP_TIME_SERIES_VALUE_DYEAR(TSF, TIME, VALUE, TIME0)
  CLASS(TIME_SERIES_FILE),  INTENT(INOUT):: TSF
  DOUBLE PRECISION,         INTENT(IN   ):: TIME
  DOUBLE PRECISION,         INTENT(  OUT):: VALUE
  DOUBLE PRECISION,OPTIONAL,INTENT(IN   ):: TIME0
  INTEGER:: OPT
  !
  IF(TSF%OPT < Z) THEN !CONSTANT VALUE
      !
      VALUE = TSF%PREV%DAT
  ELSE
      OPT = TSF%OPT
      IF(OPT >= TEN .AND. .NOT. PRESENT(TIME0))  OPT = ONE
      IF(OPT >= TEN) THEN
          IF(ABS(TIME - TIME0) < YEARTOL_5    )  OPT = ONE !WITHIN 5 SECONDS OF EACH OTHER
      END IF
      !
      
      IF(OPT < TEN) THEN
          ! MOVE WINDOW TO CORRECT LOCATION
          IF(TSF%MONTHDAY) THEN
              CALL MOVE_MONTHDAY_WINDOW_TO(TSF, TIME) 
          ELSE
              CALL MOVE_TIME_WINDOW_TO(TSF, TIME) 
          END IF
          !
          IF    (ABS(TIME - TSF%PREV%TIM) < YEARTOL_5) THEN; VALUE = TSF%PREV%DAT  !WITHIN 5 SECONDS OF REQUESTED TIME
          ELSEIF(ABS(TIME - TSF%CUR%TIM ) < YEARTOL_5) THEN; VALUE = TSF%CUR %DAT
          ELSEIF(ABS(TIME - TSF%NEXT%TIM) < YEARTOL_5) THEN; VALUE = TSF%NEXT%DAT
          ELSEIF(TIME < TSF%CUR%TIM ) THEN
                                       CALL INTERP(TSF%OPT, TSF%PREV%TIM, TSF%PREV%DAT, TSF%CUR%TIM, TSF%CUR%DAT,  TIME, VALUE)
          ELSE
                                       CALL INTERP(TSF%OPT, TSF%CUR%TIM,  TSF%CUR%DAT, TSF%NEXT%TIM, TSF%NEXT%DAT, TIME, VALUE)
          END IF
      ELSE
          SELECT CASE(OPT)
          CASE(TEN);       CALL TIME_MEAN(   TSF, TIME0, TIME, VALUE)
          CASE(11);        CALL TIME_MAX(    TSF, TIME0, TIME, VALUE)
          CASE(12);        CALL TIME_MIN(    TSF, TIME0, TIME, VALUE)
          CASE(13);        CALL INTERVAL_SUM(TSF, TIME0, TIME, VALUE)
          CASE(14);        CALL TIME_SUM(    TSF, TIME0, TIME, VALUE)
          END SELECT
      END IF
  END IF
  !
  IF(TSF%FL%SCALE.NE.UNO) VALUE = VALUE*TSF%FL%SCALE
  !
  END SUBROUTINE
  !
  PURE SUBROUTINE INTERP(OPT, TIM1, VAL1, TIM2, VAL2, TIM, VAL)
    INTEGER,          INTENT(IN   ):: OPT
    DOUBLE PRECISION, INTENT(IN   ):: TIM1, VAL1, TIM2, VAL2, TIM
    DOUBLE PRECISION, INTENT(  OUT):: VAL
    !
    ! OPT = 0 INTERPOLATE
    ! OPT = 1 STEP_FUNCTION
    ! OPT = 2 NEAREST VALUE
    !
    IF(OPT==Z) THEN !INTERPOLATE
        !
        VAL = VAL1 + (TIM - TIM1) * (VAL2-VAL1) / (TIM2-TIM1)
        !
    ELSEIF(TIM <= TIM1) THEN
        !
        VAL = VAL1
        !
    ELSEIF( TIM >= TIM2 ) THEN
        !
        VAL = VAL2
        !
    ELSEIF(OPT==ONE) THEN !STEP_FUNCTION
        !
        VAL = VAL1
        !
    ELSEIF(OPT==TWO) THEN !NEXT_VALUE
        !
        VAL = VAL2
        !
    ELSEIF(OPT==THREE) THEN !NEAREST DATUE
        !
        IF( TIM - TIM1 > TIM2 - TIM) THEN  !CLOSE RTO TIM2 IF TIM2-TIM IS SMALLER
            VAL = VAL2
        ELSE
            VAL = VAL1
        END IF
        !
    END IF
    !
  END SUBROUTINE
  !
  PURE SUBROUTINE SET_MONTHDAY(TSF, YR)
    CLASS(TIME_SERIES_FILE), INTENT(INOUT):: TSF
    INTEGER,        INTENT(IN   ):: YR
    !
    !
    SELECT CASE(TSF%MD_FLAG)
    CASE(Z)                                 !Within File, Normal Operation
          CALL TSF%PREV%DATE%SET_YEAR(YR)
          CALL TSF%CUR %DATE%SET_YEAR(YR)
          CALL TSF%NEXT%DATE%SET_YEAR(YR)
    CASE(ONE)                               ! PREV-CUR stradles EOF
          CALL TSF%PREV%DATE%SET_YEAR(YR    )
          CALL TSF%CUR %DATE%SET_YEAR(YR+ONE)
          CALL TSF%NEXT%DATE%SET_YEAR(YR+ONE)
    CASE(TWO)                               ! CUR-NEXT stradles EOF
          CALL TSF%PREV%DATE%SET_YEAR(YR    )
          CALL TSF%CUR %DATE%SET_YEAR(YR    )
          CALL TSF%NEXT%DATE%SET_YEAR(YR+ONE)
    END SELECT
    !
    TSF%CUR%TIM = TSF%CUR%DATE%DYEAR 
    TSF%PREV%TIM = TSF%PREV%DATE%DYEAR 
    TSF%NEXT%TIM = TSF%NEXT%DATE%DYEAR
    !
  END SUBROUTINE
  !
  SUBROUTINE TIME_MEAN(TSF, TIME0, TIME1, VAL)
    CLASS(TIME_SERIES_FILE), INTENT(INOUT):: TSF
    DOUBLE PRECISION,TARGET, INTENT(IN   ):: TIME0, TIME1
    DOUBLE PRECISION,        INTENT(  OUT):: VAL
    DOUBLE PRECISION:: DELT, DT
    DOUBLE PRECISION, POINTER:: TIM0, TIM1
    INTEGER:: YR
    LOGICAL:: EOF
    !
    IF (TIME0 > TIME1) THEN
        TIM0  => TIME1
        TIM1  => TIME0
    ELSE
        TIM0  => TIME0
        TIM1  => TIME1
    END IF
    !
    IF(INT(TIM0) .NE. INT(TIM1) ) THEN  !TIME FRAME SPANS MULTIPLE YEARS SO NEED TO TAKE INTO ACCOUNT LEAP YEARS --NOTE DELT IS IN DAYS
          !
          CALL TIME_SUM(TSF, TIM0, TIM1, VAL, DELT)
          !
    ELSE
          ! USE DECIMAL YEARS AS WEIGHTING FUNCTION SINCE TIME FRAME IS WITHIN THE SAME YEAR
          !
          ! MOVE WINDOW TO CORRECT LOCATION
          !
          IF(TSF%MONTHDAY) THEN
              CALL MOVE_MONTHDAY_WINDOW_TO(TSF, TIM0)
              YR = INT(TSF%NEXT%TIM)
          ELSE
              CALL MOVE_TIME_WINDOW_TO(TSF, TIM0) 
          END IF
          !
          DELT = DZ
          VAL  = DZ
          IF    (TIM1 < TSF%CUR%TIM) THEN
                                     DT   = TIM1 - TIM0
                                     VAL  = VAL + (TSF%PREV%DAT*DT)
                                     DELT = DELT + DT
                                     EOF  = TRUE
          ELSEIF(TIM0 < TSF%CUR%TIM) THEN
                                     DT   = TSF%CUR%TIM - TIM0
                                     VAL  = VAL + (TSF%PREV%DAT*DT)
                                     DELT = DELT + DT
                                     !
                                     IF(TIM1 < TSF%NEXT%TIM) THEN
                                         DT   = TIM1 - TSF%CUR%TIM
                                         VAL  = VAL + (TSF%CUR%DAT*DT)
                                         DELT = DELT + DT
                                         EOF  = TRUE
                                     ELSE
                                         EOF = FALSE
                                     END IF
          ELSEIF(TIM1 < TSF%NEXT%TIM) THEN
                                     DT   = TIM1 - TIM0
                                     VAL  = VAL + (TSF%CUR%DAT*DT)
                                     DELT = DELT + DT
                                     EOF  = TRUE
          ELSE
                                     DT   = TSF%NEXT%TIM - TIM0
                                     VAL  = VAL + (TSF%CUR%DAT*DT)
                                     DELT = DELT + DT
                                     IF(TSF%MONTHDAY) THEN
                                         CALL READ_NEXT_ROW_MONTHDAY(TSF)
                                     ELSE
                                         CALL READ_NEXT_ROW(TSF, EOF)
                                     END IF
                                     EOF  = FALSE
          END IF
          !
          DO WHILE (.NOT. EOF) 
                                     IF(TIM1 < TSF%NEXT%TIM) THEN
                                         DT   = TIM1 - TSF%CUR%TIM
                                         VAL  = VAL + (TSF%CUR%DAT*DT)
                                         DELT = DELT + DT
                                         EOF  = TRUE
                                     ELSE
                                         DT   = TSF%NEXT%TIM - TSF%CUR%TIM
                                         VAL  = VAL + (TSF%CUR%DAT*DT)
                                         DELT = DELT + DT
                                         IF(TSF%MONTHDAY) THEN
                                             CALL READ_NEXT_ROW_MONTHDAY(TSF)
                                         ELSE
                                             CALL READ_NEXT_ROW(TSF, EOF)
                                         END IF
                                     END IF
          END DO
          !
          IF(TIM1 > TSF%NEXT%TIM) THEN
              IF(.NOT. TSF%MONTHDAY) THEN
                                         DT   = TIM1 - TSF%NEXT%TIM
                                         VAL  = VAL + (TSF%NEXT%DAT*DT)
                                         DELT = DELT + DT
              ELSE
                    DO WHILE(TIM1 > TSF%NEXT%TIM)
                        YR = YR + ONE
                        CALL MOVE_TO_START_OF_FILE(TSF)
                        CALL SET_MONTHDAY(TSF, YR) !SET PREV, CUR, NEXT TO MONTH DAY YEAR
                        !
                        IF(TIM1 < TSF%CUR%TIM) THEN
                                     DT   = TIM1 - TSF%PREV%TIM
                                     VAL  = VAL + (TSF%PREV%DAT*DT)
                                     DELT = DELT + DT
                                     EXIT
                        ELSE
                                     DT   = TSF%CUR%TIM - TSF%PREV%TIM
                                     VAL  = VAL + (TSF%PREV%DAT*DT)
                                     DELT = DELT + DT
                        END IF
                        !
                        IF(TIM1 < TSF%NEXT%TIM) THEN
                            DT   = TIM1 - TSF%CUR%TIM
                            VAL  = VAL + (TSF%CUR%DAT*DT)
                            DELT = DELT + DT
                            EXIT
                        ELSE
                            EOF = FALSE
                            DO WHILE (.NOT. EOF) 
                                     IF(TIM1 < TSF%NEXT%TIM) THEN
                                         DT   = TIM1 - TSF%CUR%TIM
                                         VAL  = VAL + (TSF%CUR%DAT*DT)
                                         DELT = DELT + DT
                                         EOF  = TRUE
                                     ELSE
                                         DT   = TSF%NEXT%TIM - TSF%CUR%TIM
                                         VAL  = VAL + (TSF%CUR%DAT*DT)
                                         DELT = DELT + DT
                                         CALL READ_NEXT_ROW_MONTHDAY(TSF)
                                     END IF
                            END DO
                        END IF
                    END DO
              END IF
          END IF
    END IF
    !
    IF (DELT > NEARZERO_29) THEN
        VAL = VAL/DELT
    ELSE
        VAL = DZ
    END IF
    !
    NULLIFY(TIM0, TIM1)
    !
  END SUBROUTINE
  !
  SUBROUTINE INTERVAL_SUM(TSF, TIME0, TIME1, VAL)
    CLASS(TIME_SERIES_FILE), INTENT(INOUT):: TSF
    DOUBLE PRECISION,TARGET, INTENT(IN   ):: TIME0, TIME1
    DOUBLE PRECISION,        INTENT(  OUT):: VAL
    DOUBLE PRECISION, POINTER:: TIM0, TIM1
    INTEGER:: YR
    LOGICAL:: EOF
    !
    IF (TIME0 > TIME1) THEN
        TIM0  => TIME1
        TIM1  => TIME0
    ELSE
        TIM0  => TIME0
        TIM1  => TIME1
    END IF
    !
    ! MOVE WINDOW TO CORRECT LOCATION
    !
    IF(TSF%MONTHDAY) THEN
        CALL MOVE_MONTHDAY_WINDOW_TO(TSF, TIM0)
        YR = INT(TSF%NEXT%TIM)
    ELSE
        CALL MOVE_TIME_WINDOW_TO(TSF, TIM0) 
    END IF
    !
    VAL  = DZ
    IF    (TIM1 < TSF%CUR%TIM) THEN
                               VAL  = VAL + TSF%PREV%DAT
                               EOF  = TRUE
    ELSEIF(TIM0 < TSF%CUR%TIM) THEN
                               VAL  = VAL + TSF%PREV%DAT
                               !
                               IF(TIM1 < TSF%NEXT%TIM) THEN
                                   VAL  = VAL + TSF%CUR%DAT
                                   EOF  = TRUE
                               ELSE
                                   EOF = FALSE
                               END IF
    ELSEIF(TIM1 < TSF%NEXT%TIM) THEN
                               VAL  = VAL + TSF%CUR%DAT
                               EOF  = TRUE
    ELSE
                               VAL  = VAL + TSF%CUR%DAT
                               IF(TSF%MONTHDAY) THEN
                                   CALL READ_NEXT_ROW_MONTHDAY(TSF)
                               ELSE
                                   CALL READ_NEXT_ROW(TSF, EOF)
                               END IF
                               EOF  = FALSE
    END IF
    !
    DO WHILE (.NOT. EOF) 
                               IF(TIM1 < TSF%NEXT%TIM) THEN
                                   VAL  = VAL + TSF%CUR%DAT
                                   EOF  = TRUE
                               ELSE
                                   VAL  = VAL + TSF%CUR%DAT
                                   IF(TSF%MONTHDAY) THEN
                                       CALL READ_NEXT_ROW_MONTHDAY(TSF)
                                   ELSE
                                       CALL READ_NEXT_ROW(TSF, EOF)
                                   END IF
                               END IF
    END DO
    !
    IF(TIM1 > TSF%NEXT%TIM) THEN
        IF(.NOT. TSF%MONTHDAY) THEN
                                   VAL  = VAL + TSF%NEXT%DAT
        ELSE
              DO WHILE(TIM1 > TSF%NEXT%TIM)
                  YR = YR + ONE
                  CALL MOVE_TO_START_OF_FILE(TSF)
                  CALL SET_MONTHDAY(TSF, YR) !SET PREV, CUR, NEXT TO MONTH DAY YEAR
                  !
                  IF(TIM1 < TSF%CUR%TIM) THEN
                               VAL  = VAL + TSF%PREV%DAT
                               EXIT
                  ELSE
                               VAL  = VAL + TSF%PREV%DAT
                  END IF
                  !
                  VAL  = VAL + TSF%CUR%DAT
                  IF(TIM1 < TSF%NEXT%TIM) THEN
                      EXIT
                  ELSE
                      CALL READ_NEXT_ROW_MONTHDAY(TSF)
                      DO WHILE (.NOT. EOF) 
                               VAL  = VAL + TSF%CUR%DAT
                               IF(TIM1 < TSF%NEXT%TIM) THEN
                                   EOF  = TRUE
                               ELSE
                                   CALL READ_NEXT_ROW_MONTHDAY(TSF)
                               END IF
                      END DO
                  END IF
              END DO
        END IF
    END IF
    !
    NULLIFY(TIM0, TIM1)
    !
  END SUBROUTINE
  !
  SUBROUTINE TIME_SUM(TSF, TIME0, TIME1, VAL, DAY_DELT)
    CLASS(TIME_SERIES_FILE),    INTENT(INOUT):: TSF
    DOUBLE PRECISION, TARGET,   INTENT(IN   ):: TIME0, TIME1
    DOUBLE PRECISION,           INTENT(  OUT):: VAL
    DOUBLE PRECISION, OPTIONAL, INTENT(  OUT):: DAY_DELT
    DOUBLE PRECISION, POINTER:: TIM0, TIM1
    DOUBLE PRECISION:: DELT, DT
    TYPE(DATE_OPERATOR):: DT0,DT1
    INTEGER:: YR
    LOGICAL:: EOF
    !
    IF (TIME0 > TIME1) THEN
        TIM0  => TIME1
        TIM1  => TIME0
    ELSE
        TIM0  => TIME0
        TIM1  => TIME1
    END IF
    !
    ! MOVE WINDOW TO CORRECT LOCATION
    !
    IF(TSF%MONTHDAY) THEN
        CALL MOVE_MONTHDAY_WINDOW_TO(TSF, TIM0)
        YR = INT(TSF%NEXT%TIM)
    ELSE
        CALL MOVE_TIME_WINDOW_TO(TSF, TIM0) 
        !
        CALL TSF%CUR %DATE%DYEAR_MAKE_DATE() !IN CASE DATE WAS NOT STORED FOR SPEED PURPOSES
        CALL TSF%NEXT%DATE%DYEAR_MAKE_DATE()
    END IF
    !
    CALL DT0%INIT_DYEAR(TIM0)
    CALL DT1%INIT_DYEAR(TIM1)
    !
    DELT = DZ
    VAL  = DZ
    IF    (TIM1 < TSF%CUR%TIM) THEN
                               DT   = DT1 - DT0
                               VAL  = VAL + (TSF%PREV%DAT*DT)
                               DELT = DELT + DT
                               EOF  = TRUE
    ELSEIF(TIM0 < TSF%CUR%TIM) THEN
                               DT   = TSF%CUR%DATE - DT0
                               VAL  = VAL + (TSF%PREV%DAT*DT)
                               DELT = DELT + DT
                               !
                               IF(TIM1 < TSF%NEXT%TIM) THEN
                                   DT   = DT1 - TSF%CUR%DATE
                                   VAL  = VAL + (TSF%CUR%DAT*DT)
                                   DELT = DELT + DT
                                   EOF  = TRUE
                               ELSE
                                   EOF = FALSE
                               END IF
    ELSEIF(TIM1 < TSF%NEXT%TIM) THEN
                               DT   = DT1 - DT0
                               VAL  = VAL + (TSF%CUR%DAT*DT)
                               DELT = DELT + DT
                               EOF  = TRUE
    ELSE
                               DT   = TSF%NEXT%DATE - DT0
                               VAL  = VAL + (TSF%CUR%DAT*DT)
                               DELT = DELT + DT
                               IF(TSF%MONTHDAY) THEN
                                   CALL READ_NEXT_ROW_MONTHDAY(TSF)
                               ELSE
                                   CALL READ_NEXT_ROW(TSF, EOF)
                                   CALL TSF%NEXT%DATE%DYEAR_MAKE_DATE()
                               END IF
                               EOF  = FALSE
    END IF
    !
    DO WHILE (.NOT. EOF) 
                               IF(TIM1 < TSF%NEXT%TIM) THEN
                                   DT   = DT1 - TSF%CUR%DATE
                                   VAL  = VAL + (TSF%CUR%DAT*DT)
                                   DELT = DELT + DT
                                   EOF  = TRUE
                               ELSE
                                   DT   = TSF%NEXT%DATE - TSF%CUR%DATE
                                   VAL  = VAL + (TSF%CUR%DAT*DT)
                                   DELT = DELT + DT
                                   IF(TSF%MONTHDAY) THEN
                                       CALL READ_NEXT_ROW_MONTHDAY(TSF)
                                   ELSE
                                       CALL READ_NEXT_ROW(TSF, EOF)
                                       CALL TSF%NEXT%DATE%DYEAR_MAKE_DATE()
                                   END IF
                               END IF
    END DO
    !
    IF(TIM1 > TSF%NEXT%TIM) THEN
        IF(.NOT. TSF%MONTHDAY) THEN
                                   DT   = DT1 - TSF%NEXT%DATE
                                   VAL  = VAL + (TSF%NEXT%DAT*DT)
                                   DELT = DELT + DT
        ELSE
              DO WHILE(TIM1 > TSF%NEXT%TIM)
                  YR = YR + ONE
                  CALL MOVE_TO_START_OF_FILE(TSF)
                  CALL SET_MONTHDAY(TSF, YR) !SET PREV, CUR, NEXT TO MONTH DAY YEAR
                  CALL TSF%CUR %DATE%DYEAR_MAKE_DATE() !IN CASE DATE WAS NOT STORED FOR SPEED PURPOSES
                  CALL TSF%NEXT%DATE%DYEAR_MAKE_DATE()
                  !
                  IF(TIM1 < TSF%CUR%TIM) THEN
                               DT   = DT1 - TSF%PREV%DATE
                               VAL  = VAL + (TSF%PREV%DAT*DT)
                               DELT = DELT + DT
                               EXIT
                  ELSE
                               DT   = TSF%CUR%DATE - TSF%PREV%DATE
                               VAL  = VAL + (TSF%PREV%DAT*DT)
                               DELT = DELT + DT
                  END IF
                  !
                  IF(TIM1 < TSF%NEXT%TIM) THEN
                      DT   = DT1 - TSF%CUR%DATE
                      VAL  = VAL + (TSF%CUR%DAT*DT)
                      DELT = DELT + DT
                      EXIT
                  ELSE
                      EOF = FALSE
                      DO WHILE (.NOT. EOF) 
                               IF(TIM1 < TSF%NEXT%TIM) THEN
                                   DT   = DT1 - TSF%CUR%DATE
                                   VAL  = VAL + (TSF%CUR%DAT*DT)
                                   DELT = DELT + DT
                                   EOF  = TRUE
                               ELSE
                                   DT   = TSF%NEXT%DATE - TSF%CUR%DATE
                                   VAL  = VAL + (TSF%CUR%DAT*DT)
                                   DELT = DELT + DT
                                   CALL READ_NEXT_ROW_MONTHDAY(TSF)
                               END IF
                      END DO
                  END IF
              END DO
        END IF
    END IF
    !
    IF(PRESENT(DAY_DELT)) DAY_DELT = DELT
    !
    NULLIFY(TIM0, TIM1)
    !
  END SUBROUTINE
  !
  !SUBROUTINE TIME_SUM(TSF, TIM0, TIM1, VAL, TMULT)
  !  CLASS(TIME_SERIES_FILE), INTENT(INOUT):: TSF
  !  DOUBLE PRECISION,        INTENT(IN   ):: TIM0, TIM1
  !  DOUBLE PRECISION,        INTENT(  OUT):: VAL
  !  LOGICAL,                 INTENT(IN   ):: TMULT
  !  DOUBLE PRECISION:: DT
  !  LOGICAL:: EOF
  !  !
  !  ! MOVE WINDOW TO CORRECT LOCATION
  !  !
  !  CALL MOVE_TIME_WINDOW_TO(TSF, TIM0)
  !  !
  !  DT   = UNO
  !  VAL  = DZ
  !  IF(TIM0 < TSF%CUR%TIM) THEN
  !                             IF(TMULT) DT = TSF%CUR%TIM - TIM0
  !                             VAL  = VAL + (TSF%PREV%DAT*DT)
  !                             !
  !                             IF(TIM1 < TSF%NEXT%TIM) THEN
  !                                 IF(TMULT) DT = TIM1 - TSF%CUR%TIM
  !                                 VAL  = VAL + (TSF%CUR%DAT*DT)
  !                                 EOF  = TRUE
  !                             ELSE
  !                                 IF(TMULT) DT = TSF%NEXT%TIM - TSF%CUR%TIM
  !                                 VAL  = VAL + (TSF%CUR%DAT*DT)
  !                                 CALL READ_NEXT_ROW(TSF, EOF)
  !                             END IF
  !  ELSE
  !        EOF  = FALSE
  !  END IF
  !  !    
  !  DO WHILE (.NOT. EOF) 
  !                             IF(TIM1 < TSF%NEXT%TIM) THEN
  !                                 IF(TMULT) DT = TIM1 - TSF%CUR%TIM
  !                                 VAL  = VAL + (TSF%CUR%DAT*DT)
  !                                 EOF  = TRUE
  !                             ELSE
  !                                 IF(TMULT) DT = TSF%NEXT%TIM - TSF%CUR%TIM
  !                                 VAL  = VAL + (TSF%CUR%DAT*DT)
  !                                 CALL READ_NEXT_ROW(TSF, EOF)
  !                             END IF
  !  END DO
  !  !
  !  IF(TIM1 > TSF%NEXT%TIM) THEN
  !                                 IF(TMULT) DT = TIM1 - TSF%NEXT%TIM
  !                                 VAL  = VAL + (TSF%NEXT%DAT*DT)
  !  END IF
  !  !
  !END SUBROUTINE
  !
  SUBROUTINE TIME_MAX(TSF, TIM0, TIM1, VAL)
    CLASS(TIME_SERIES_FILE), INTENT(INOUT):: TSF
    DOUBLE PRECISION,        INTENT(IN   ):: TIM0, TIM1
    DOUBLE PRECISION,        INTENT(  OUT):: VAL
    !
    CALL TIME_MAXMIN(TSF, TIM0, TIM1, VAL, TRUE)
    !
  END SUBROUTINE
  !
  SUBROUTINE TIME_MIN(TSF, TIM0, TIM1, VAL)
    CLASS(TIME_SERIES_FILE), INTENT(INOUT):: TSF
    DOUBLE PRECISION,        INTENT(IN   ):: TIM0, TIM1
    DOUBLE PRECISION,        INTENT(  OUT):: VAL
    !
    CALL TIME_MAXMIN(TSF, TIM0, TIM1, VAL, FALSE)
    !
  END SUBROUTINE
  !
  SUBROUTINE TIME_MAXMIN(TSF, TIME0, TIME1, VAL, ISMAX)
    CLASS(TIME_SERIES_FILE), INTENT(INOUT):: TSF
    DOUBLE PRECISION,TARGET, INTENT(IN   ):: TIME0, TIME1
    DOUBLE PRECISION,        INTENT(  OUT):: VAL
    LOGICAL,                 INTENT(IN   ):: ISMAX
    DOUBLE PRECISION, POINTER:: TIM0, TIM1
    INTEGER:: YR
    LOGICAL:: EOF
    !
    IF (TIME0 > TIME1) THEN
        TIM0  => TIME1
        TIM1  => TIME0
    ELSE
        TIM0  => TIME0
        TIM1  => TIME1
    END IF
    !
    ! MOVE WINDOW TO CORRECT LOCATION
    !
    IF(TSF%MONTHDAY) THEN
        CALL MOVE_MONTHDAY_WINDOW_TO(TSF, TIM0)
        YR = INT(TSF%NEXT%TIM)
    ELSE
        CALL MOVE_TIME_WINDOW_TO(TSF, TIM0) 
    END IF
    !
    !IF(ISMAX) THEN
    !              VAL  = ninf
    !ELSE
    !              VAL  = inf
    !END IF
    !
    IF    (TIM1 < TSF%CUR%TIM) THEN
                               VAL  = TSF%PREV%DAT
                               EOF  = TRUE
    ELSEIF(TIM0 < TSF%CUR%TIM) THEN
                               VAL  = TSF%PREV%DAT
                               !
                               IF(TIM1 < TSF%NEXT%TIM) THEN
                                   IF(ISMAX) THEN
                                                 IF(VAL < TSF%CUR%DAT ) VAL = TSF%CUR%DAT
                                   ELSE
                                                 IF(VAL > TSF%CUR%DAT ) VAL = TSF%CUR%DAT
                                   END IF
                                   EOF  = TRUE
                               ELSE
                                   EOF = FALSE
                               END IF
    ELSEIF(TIM1 < TSF%NEXT%TIM) THEN
                               VAL  = TSF%CUR%DAT
                               EOF  = TRUE
    ELSE
                               VAL  = TSF%CUR%DAT
                               IF(TSF%MONTHDAY) THEN
                                   CALL READ_NEXT_ROW_MONTHDAY(TSF)
                               ELSE
                                   CALL READ_NEXT_ROW(TSF, EOF)
                               END IF
                               EOF  = FALSE
    END IF
    !
    DO WHILE (.NOT. EOF)
                               IF(ISMAX) THEN
                                             IF(VAL < TSF%CUR%DAT ) VAL = TSF%CUR%DAT
                               ELSE
                                             IF(VAL > TSF%CUR%DAT ) VAL = TSF%CUR%DAT
                               END IF 
                               !
                               IF(TIM1 < TSF%NEXT%TIM) THEN
                                   EOF  = TRUE
                               ELSE
                                   IF(TSF%MONTHDAY) THEN
                                       CALL READ_NEXT_ROW_MONTHDAY(TSF)
                                   ELSE
                                       CALL READ_NEXT_ROW(TSF, EOF)
                                   END IF
                               END IF
    END DO
    !
    IF(TIM1 > TSF%NEXT%TIM) THEN
        IF(.NOT. TSF%MONTHDAY) THEN
                               IF(ISMAX) THEN
                                             IF(VAL < TSF%NEXT%DAT ) VAL = TSF%NEXT%DAT
                               ELSE
                                             IF(VAL > TSF%NEXT%DAT ) VAL = TSF%NEXT%DAT
                               END IF 
        ELSE
              DO WHILE(TIM1 > TSF%NEXT%TIM)
                  YR = YR + ONE
                  CALL MOVE_TO_START_OF_FILE(TSF)
                  CALL SET_MONTHDAY(TSF, YR) !SET PREV, CUR, NEXT TO MONTH DAY YEAR
                  !
                  IF(ISMAX) THEN
                                IF(VAL < TSF%PREV%DAT ) VAL = TSF%PREV%DAT
                  ELSE
                                IF(VAL > TSF%PREV%DAT ) VAL = TSF%PREV%DAT
                  END IF
                  !
                  IF(TIM1 < TSF%CUR%TIM)  EXIT
                  !
                  IF(ISMAX) THEN
                                IF(VAL < TSF%CUR%DAT ) VAL = TSF%CUR%DAT
                  ELSE
                                IF(VAL > TSF%CUR%DAT ) VAL = TSF%CUR%DAT
                  END IF 
                  !
                  IF(TIM1 < TSF%NEXT%TIM) THEN
                      EXIT
                  ELSE
                      CALL READ_NEXT_ROW_MONTHDAY(TSF)
                      DO WHILE (.NOT. EOF)
                               IF(ISMAX) THEN
                                             IF(VAL < TSF%CUR%DAT ) VAL = TSF%CUR%DAT
                               ELSE
                                             IF(VAL > TSF%CUR%DAT ) VAL = TSF%CUR%DAT
                               END IF  
                               IF(TIM1 < TSF%NEXT%TIM) THEN
                                   EOF  = TRUE
                               ELSE
                                   CALL READ_NEXT_ROW_MONTHDAY(TSF)
                               END IF
                      END DO
                  END IF
              END DO
        END IF
    END IF
    !
    NULLIFY(TIM0, TIM1)
    !
  END SUBROUTINE
    
!  SUBROUTINE TIME_MAXMIN(TSF, TIM0, TIM1, VAL, ISMAX)
!    CLASS(TIME_SERIES_FILE), INTENT(INOUT):: TSF
!    DOUBLE PRECISION,        INTENT(IN   ):: TIM0, TIM1
!    DOUBLE PRECISION,        INTENT(  OUT):: VAL
!    LOGICAL,                 INTENT(IN   ):: ISMAX
!    INTEGER:: YR
!    LOGICAL:: EOF
!    !
!    ! MOVE WINDOW TO CORRECT LOCATION
!    !
!    IF(TSF%MONTHDAY) THEN
!        CALL MOVE_MONTHDAY_WINDOW_TO(TSF, TIM0) 
!    ELSE
!        CALL MOVE_TIME_WINDOW_TO(TSF, TIM0) 
!    END IF
!    !
!    IF(ISMAX) THEN
!                  VAL  = ninf
!    ELSE
!                  VAL  = inf
!    END IF
!    IF(TIM0 < TSF%CUR%TIM) THEN
!                               IF(ISMAX) THEN
!                                             IF(VAL < TSF%PREV%DAT) VAL = TSF%PREV%DAT
!                                             IF(VAL < TSF%CUR%DAT ) VAL = TSF%CUR%DAT
!                               ELSE
!                                             IF(VAL > TSF%PREV%DAT) VAL = TSF%PREV%DAT
!                                             IF(VAL > TSF%CUR%DAT ) VAL = TSF%CUR%DAT
!                               END IF
!                               !
!                               IF(TIM1 < TSF%NEXT%TIM) THEN
!                                   EOF  = TRUE
!                               ELSEIF(TSF%MONTHDAY) THEN
!                                       YR = INT(TSF%NEXT%TIM)
!                                       CALL READ_NEXT_ROW_MONTHDAY(TSF)
!                               ELSE
!                                       CALL READ_NEXT_ROW(TSF, EOF)
!                               END IF
!    ELSE
!                               EOF  = FALSE
!    END IF
!    !    
!    IF(TSF%MONTHDAY) YR = INT(TSF%NEXT%TIM)
!    DO WHILE (.NOT. EOF) 
!                               IF(ISMAX) THEN
!                                             IF(VAL < TSF%CUR%DAT ) VAL = TSF%CUR%DAT
!                               ELSE
!                                             IF(VAL > TSF%CUR%DAT ) VAL = TSF%CUR%DAT
!                               END IF
!                               !
!                               IF(TIM1 < TSF%NEXT%TIM) THEN
!                                   EOF  = TRUE
!                               ELSEIF(TSF%MONTHDAY) THEN
!                                       CALL READ_NEXT_ROW_MONTHDAY(TSF)
!                               ELSE
!                                       CALL READ_NEXT_ROW(TSF, EOF)
!                               END IF
!    END DO
!    !
!    IF(TIM1 > TSF%NEXT%TIM) THEN
!                               IF(ISMAX) THEN
!                                             IF(VAL < TSF%NEXT%DAT ) VAL = TSF%NEXT%DAT
!                               ELSE
!                                             IF(VAL > TSF%NEXT%DAT ) VAL = TSF%NEXT%DAT
!                               END IF
!    END IF
!    !
!  END SUBROUTINE
  !
  SUBROUTINE MOVE_TO_START_OF_FILE(TSF)
    CLASS(TIME_SERIES_FILE),  INTENT(INOUT):: TSF
    INTEGER:: LLOC, ISTART, ISTOP
    LOGICAL:: EOF
    !
    IF(TSF%N == Z) RETURN
    !
    CALL TSF%FL%REWIND()
    CALL READ_TO_DATA(TSF%LINE,TSF%FL%IU,TSF%IOUT)
    !
    ! GET FIRST ROW
    LLOC = ONE
    CALL GET_DATE(TSF%LINE,LLOC,ISTART,ISTOP,TSF%IOUT,TSF%FL%IU,TSF%PREV%DATE, ONLY_DYEAR=TRUE ,MSG='FAILED TO LOAD TIME PART (FIRST NUMBER) OF THE TIME SERIES INPUT FILE.')
    TSF%PREV%TIM = TSF%PREV%DATE%DYEAR
    !
    CALL GET_NUMBER(TSF%LINE,LLOC,ISTART,ISTOP,TSF%IOUT,TSF%FL%IU,TSF%PREV%DAT,MSG='FAILED TO LOAD DATA/VALUE PART (SECOND NUMBER) OF THE TIME SERIES INPUT FILE.')
    !
    CALL READ_TO_DATA(TSF%LINE,TSF%FL%IU,TSF%IOUT,EOF=EOF)
    !
    ! GET SECOND ROW
    IF(EOF) THEN
        TSF%CUR%TIM  = TSF%PREV%TIM
        TSF%CUR%DAT  = TSF%PREV%DAT
    ELSE
        LLOC = ONE
        CALL GET_DATE(TSF%LINE,LLOC,ISTART,ISTOP,TSF%IOUT,TSF%FL%IU,TSF%CUR%DATE, ONLY_DYEAR=TRUE ,MSG='FAILED TO LOAD TIME PART (FIRST NUMBER) OF THE TIME SERIES INPUT FILE.')
        TSF%CUR%TIM = TSF%CUR%DATE%DYEAR
        !
        CALL GET_NUMBER(TSF%LINE,LLOC,ISTART,ISTOP,TSF%IOUT,TSF%FL%IU,TSF%CUR%DAT,MSG='FAILED TO LOAD DATA/VALUE PART (SECOND NUMBER) OF THE TIME SERIES INPUT FILE.')
        !
        CALL READ_TO_DATA(TSF%LINE,TSF%FL%IU,TSF%IOUT,EOF=EOF)
    END IF
    !
    ! GET THIRD ROW
    IF(EOF) THEN
        TSF%NEXT%TIM = TSF%CUR%TIM
        TSF%NEXT%DAT = TSF%CUR%DAT
    ELSE
        LLOC = ONE
        CALL GET_DATE(TSF%LINE,LLOC,ISTART,ISTOP,TSF%IOUT,TSF%FL%IU,TSF%NEXT%DATE, ONLY_DYEAR=TRUE ,MSG='FAILED TO LOAD TIME PART (FIRST NUMBER) OF THE TIME SERIES INPUT FILE.')
        TSF%NEXT%TIM = TSF%NEXT%DATE%DYEAR
        !
        CALL GET_NUMBER(TSF%LINE,LLOC,ISTART,ISTOP,TSF%IOUT,TSF%FL%IU,TSF%NEXT%DAT,MSG='FAILED TO LOAD DATA/VALUE PART (SECOND NUMBER) OF THE TIME SERIES INPUT FILE.')
    END IF
    !
    TSF%AT_NEXT   = TRUE
    TSF%P         = THREE              ! POSITION LOCATED AFTER ROW 3
    IF(EOF) TSF%N = THREE              ! NOW HAVE THREE DATA POINTS
    !
    IF(TSF%MONTHDAY) TSF%MD_FLAG = Z   !Change flag to internal
    !
    !!!BACKSPACE(TSF%FL%IU) !POSITIONED AT FIRST LINE OF INPUT DATA
    !!!!
    !!!TSF%AT_NEXT = TRUE  !NOW LOAD FIRST THREE VALUES
    !!!!
    !!!IF(TSF%N > TWO) THEN            !LOAD FIRST THREE VALUES
    !!!    CALL READ_NEXT_ROW(TSF, EOF)
    !!!    CALL READ_NEXT_ROW(TSF, EOF)
    !!!    CALL READ_NEXT_ROW(TSF, EOF)
    !!!ELSEIF(TSF%N == TWO) THEN
    !!!    TSF%P = NINER               !JUST TO GET PAST END OF FILE CHECKS
    !!!    CALL READ_NEXT_ROW(TSF, EOF)
    !!!    CALL READ_NEXT_ROW(TSF, EOF)
    !!!    TSF%PREV%TIM = TSF%CUR%TIM
    !!!    TSF%PREV%DAT = TSF%CUR%DAT
    !!!    TSF%CUR %TIM = TSF%NEXT%TIM
    !!!    TSF%CUR %DAT = TSF%NEXT%DAT
    !!!    TSF%N = THREE              ! NOW HAVE THREE DATA POINTS
    !!!    TSF%P = THREE
    !!!ELSE!IF(TSF%N == ONE) THEN
    !!!    !
    !!!    TSF%P = NINER               ! JUST TO GET PAST END OF FILE CHECKS
    !!!    CALL READ_NEXT_ROW(TSF, EOF)
    !!!    TSF%PREV%TIM = TSF%NEXT%TIM
    !!!    TSF%PREV%DAT = TSF%NEXT%DAT
    !!!    TSF%CUR %TIM = TSF%NEXT%TIM
    !!!    TSF%CUR %DAT = TSF%NEXT%DAT
    !!!    TSF%N = THREE              ! NOW HAVE THREE DATA POINTS
    !!!    TSF%P = THREE
    !!!END IF
  END SUBROUTINE
  !
  SUBROUTINE MOVE_TIME_WINDOW_TO(TSF, TIME)
    CLASS(TIME_SERIES_FILE), INTENT(INOUT):: TSF
    DOUBLE PRECISION,        INTENT(IN   ):: TIME
    INTEGER:: POS, I
    LOGICAL:: EOF
    !
    EOF = FALSE
    IF(TSF%PREV%TIM <= TIME .AND. TIME <= TSF%NEXT%TIM) THEN
                                                             CONTINUE !DO NOTHING CAUSE WINDOW IS WHERE IT NEEDS TO BE
    ELSEIF(TSF%N500 < TWO) THEN
        IF    (TIME < TSF%PREV%TIM) THEN
                                        DO WHILE(.NOT. EOF .AND. TIME <= TSF%PREV%TIM)
                                                                                    CALL READ_PREV_ROW(TSF, EOF)
                                        END DO
        ELSEIF(TIME > TSF%NEXT%TIM ) THEN
                                        DO WHILE(.NOT. EOF .AND. TIME >= TSF%NEXT%TIM)
                                                                                    CALL READ_NEXT_ROW(TSF, EOF)
                                        END DO
        END IF
    ELSEIF(TIME <= TSF%T500(TWO)) THEN
        !
        IF(TSF%P > HUND) CALL MOVE_TO_START_OF_FILE(TSF)
        !
        IF    (TIME < TSF%PREV%TIM) THEN
                                        DO WHILE(.NOT. EOF .AND. TIME <= TSF%PREV%TIM)
                                                                                    CALL READ_PREV_ROW(TSF, EOF)
                                        END DO
        ELSEIF(TIME > TSF%NEXT%TIM ) THEN
                                        DO WHILE(.NOT. EOF .AND. TIME >= TSF%NEXT%TIM)
                                                                                    CALL READ_NEXT_ROW(TSF, EOF)
                                        END DO
        END IF
    ELSE
        IF    (TIME < TSF%PREV%TIM) THEN
                                        POS = TSF%P / QUIN
                                        IF(POS < ONE) POS = ONE
                                        !
                                        IF(TIME < TSF%T500(POS) .OR. POS > QUIN) THEN  !MORE THAN 500 AWAY FOR AT LEAST LINE 250,000
                                            !
                                            CALL MOVE_TO_START_OF_FILE(TSF)
                                            !
                                            POS = Z
                                            DO I=TWO, TSF%N500+ONE
                                                IF(TIME < TSF%T500(I)) THEN
                                                    POS = (I - ONE)*QUIN - TEN
                                                    EXIT
                                                END IF
                                            END DO
                                            IF(POS > Z .AND. TSF%P < POS) THEN !P = 5   POS =8
                                                DO I = TSF%P + ONE, POS
                                                                       READ(TSF%FL%IU, *)
                                                END DO
                                                TSF%P = POS
                                                CALL READ_NEXT_ROW(TSF, EOF)  !LOUD INTO MEMORY THE NEXT THREE LINES
                                                CALL READ_NEXT_ROW(TSF, EOF)
                                                CALL READ_NEXT_ROW(TSF, EOF)
                                            END IF
                                            !
                                            DO WHILE(.NOT. EOF .AND. TIME >= TSF%NEXT%TIM)
                                                                                        CALL READ_NEXT_ROW(TSF, EOF)
                                            END DO
                                        ELSE
                                            DO WHILE(.NOT. EOF .AND. TIME <= TSF%PREV%TIM)
                                                                                        CALL READ_PREV_ROW(TSF, EOF)
                                            END DO
                                        END IF
        ELSEIF(TIME > TSF%NEXT%TIM ) THEN
                                        IF(.NOT. TSF%AT_NEXT) THEN
                                            READ(TSF%FL%IU, *)
                                            READ(TSF%FL%IU, *)
                                            !CALL READ_TO_DATA(TSF%LINE,TSF%FL%IU,TSF%IOUT)
                                            !CALL READ_TO_DATA(TSF%LINE,TSF%FL%IU,TSF%IOUT)
                                            TSF%AT_NEXT = TRUE
                                            TSF%P = TSF%P + TWO 
                                        END IF
                                        !
                                        POS = TSF%P / QUIN + ONE
                                        IF(TIME > TSF%T500(POS)) THEN  !TIME < TSF%T500(TSF%N500) .AND. 
                                            POS = Z
                                            DO I=TWO, TSF%N500+ONE
                                                IF(TIME < TSF%T500(I)) THEN
                                                    POS = (I - ONE)*QUIN - TEN
                                                    EXIT
                                                END IF
                                            END DO
                                            IF(POS > Z .AND. TSF%P < POS) THEN !P = 5   POS =8
                                                DO I = TSF%P + ONE, POS
                                                                       READ(TSF%FL%IU, *)
                                                END DO
                                                TSF%P = POS
                                                CALL READ_NEXT_ROW(TSF, EOF)  !LOUD INTO MEMORY THE NEXT THREE LINES
                                                CALL READ_NEXT_ROW(TSF, EOF)
                                                CALL READ_NEXT_ROW(TSF, EOF)
                                            END IF
                                        END IF
                                        !
                                        DO WHILE(.NOT. EOF .AND. TIME >= TSF%NEXT%TIM)
                                                                                    CALL READ_NEXT_ROW(TSF, EOF)
                                        END DO
        END IF
    END IF
    !
  END SUBROUTINE
  !
  SUBROUTINE MOVE_MONTHDAY_WINDOW_TO(TSF, TIME)
    CLASS(TIME_SERIES_FILE), INTENT(INOUT):: TSF
    DOUBLE PRECISION,        INTENT(IN   ):: TIME
    INTEGER:: YR
    !
    YR = INT(TIME)
    !
    CALL SET_MONTHDAY(TSF, YR) !SET PREV, CUR, NEXT TO MONTH DAY YEAR
    !
    IF(TSF%PREV%TIM <= TIME .AND. TIME <= TSF%NEXT%TIM) THEN
                                                             CONTINUE !DO NOTHING CAUSE WINDOW IS WHERE IT NEEDS TO BE
    ELSE
        IF(TIME < TSF%PREV%TIM) THEN
                                CALL MOVE_TO_START_OF_FILE(TSF)
                                CALL SET_MONTHDAY(TSF, YR) 
        END IF
        !
        IF(TIME > TSF%NEXT%TIM ) THEN
                                        DO WHILE(TIME >= TSF%NEXT%TIM)
                                                                  CALL READ_NEXT_ROW_MONTHDAY(TSF)
                                        END DO
        END IF
    END IF
    !
  END SUBROUTINE
  !
  !SUBROUTINE MOVE_MONTHDAY_WINDOW_TO(TSF, TIME)
  !  CLASS(TIME_SERIES_FILE), INTENT(INOUT):: TSF
  !  DOUBLE PRECISION,        INTENT(IN   ):: TIME
  !  INTEGER:: POS, I, J, K, YR
  !  TYPE(DATE_OPERATOR), ALLOCATABLE:: DT
  !  LOGICAL:: EOF
  !  !
  !  YR = INT(TIME)
  !  !
  !  CALL SET_MONTHDAY(TSF, YR) !SET PREV, CUR, NEXT TO MONTH DAY YEAR
  !  !
  !  EOF = FALSE
  !  IF(TSF%PREV%TIM <= TIME .AND. TIME <= TSF%NEXT%TIM) THEN
  !                                                           CONTINUE !DO NOTHING CAUSE WINDOW IS WHERE IT NEEDS TO BE
  !  ELSEIF(TSF%N <= HUND .OR. TSF%N500 == Z) THEN
  !      IF    (TIME < TSF%PREV%TIM) THEN
  !                                      DO WHILE(.NOT. EOF .AND. TIME <= TSF%PREV%TIM)
  !                                                                                  CALL READ_PREV_ROW_MONTHDAY(TSF, EOF, YR)
  !                                      END DO
  !      ELSEIF(TIME > TSF%NEXT%TIM ) THEN
  !                                      DO WHILE(.NOT. EOF .AND. TIME >= TSF%NEXT%TIM)
  !                                                                                  CALL READ_NEXT_ROW_MONTHDAY(TSF)
  !                                      END DO
  !      END IF
  !  ELSE
  !      IF( YR .NE. INT(TSF%T500(ONE)) ) THEN
  !         !
  !         ALLOCATE(DT)
  !         DO I = ONE, TSF%N500
  !                            CALL DT%INIT(TSF%T500(I))
  !                            CALL DT%SET_YEAR(YR)
  !                            TSF%T500(I) = DT%DYEAR
  !         END DO
  !         DEALLOCATE(DT)
  !      END IF
  !      !
  !      IF(TIME <= TSF%T500(TWO)) THEN
  !          !
  !          IF(TSF%P > HUND) THEN
  !              CALL MOVE_TO_START_OF_FILE(TSF)
  !              CALL SET_MONTHDAY(TSF, YR) !SET PREV, CUR, NEXT TO MONTH DAY YEAR
  !          END IF
  !          !
  !          IF    (TIME < TSF%PREV%TIM) THEN
  !                                          DO WHILE(.NOT. EOF .AND. TIME <= TSF%PREV%TIM)
  !                                                                                      CALL READ_PREV_ROW_MONTHDAY(TSF, EOF, YR)
  !                                          END DO
  !          ELSEIF(TIME > TSF%NEXT%TIM ) THEN
  !                                          DO WHILE(.NOT. EOF .AND. TIME >= TSF%NEXT%TIM)
  !                                                                                      CALL READ_NEXT_ROW_MONTHDAY(TSF)
  !                                          END DO
  !          END IF
  !      ELSE
  !          IF    (TIME < TSF%PREV%TIM) THEN
  !                                          POS = TSF%P / QUIN
  !                                          IF(POS < ONE) POS = ONE
  !                                          !
  !                                          IF(TIME < TSF%T500(POS) .OR. POS > QUIN) THEN  !MORE THAN 500 AWAY OR BEYOND LINE 250,000
  !                                              !
  !                                              CALL MOVE_TO_START_OF_FILE(TSF)
  !                                              CALL SET_MONTHDAY(TSF, YR) !SET PREV, CUR, NEXT TO MONTH DAY YEAR
  !                                              !
  !                                              POS = Z
  !                                              DO I=TWO, TSF%N500+ONE
  !                                                  IF(TIME < TSF%T500(I)) THEN
  !                                                      POS = (I - ONE)*QUIN - TEN
  !                                                      EXIT
  !                                                  END IF
  !                                              END DO
  !                                              IF(POS > Z .AND. TSF%P < POS) THEN !P = 5   POS =8
  !                                                  DO I = TSF%P + ONE, POS
  !                                                                         READ(TSF%FL%IU, *)
  !                                                  END DO
  !                                                  TSF%P = POS
  !                                                  CALL READ_NEXT_ROW_MONTHDAY(TSF)  !LOUD INTO MEMORY THE NEXT THREE LINES
  !                                                  CALL READ_NEXT_ROW_MONTHDAY(TSF)
  !                                                  CALL READ_NEXT_ROW_MONTHDAY(TSF)
  !                                              END IF
  !                                              !
  !                                              DO WHILE(.NOT. EOF .AND. TIME >= TSF%NEXT%TIM)
  !                                                                                          CALL READ_NEXT_ROW_MONTHDAY(TSF)
  !                                              END DO
  !                                          ELSE
  !                                              DO WHILE(.NOT. EOF .AND. TIME <= TSF%PREV%TIM)
  !                                                                                          CALL READ_PREV_ROW_MONTHDAY(TSF, EOF, YR)
  !                                              END DO
  !                                          END IF
  !          ELSEIF(TIME > TSF%NEXT%TIM ) THEN
  !                                          IF(.NOT. TSF%AT_NEXT) THEN
  !                                              READ(TSF%FL%IU, *)
  !                                              READ(TSF%FL%IU, *)
  !                                              !CALL READ_TO_DATA(TSF%LINE,TSF%FL%IU,TSF%IOUT)
  !                                              !CALL READ_TO_DATA(TSF%LINE,TSF%FL%IU,TSF%IOUT)
  !                                              TSF%AT_NEXT = TRUE
  !                                              TSF%P = TSF%P + TWO 
  !                                          END IF
  !                                          !
  !                                          POS = TSF%P / QUIN + ONE
  !                                          IF(TIME > TSF%T500(POS)) THEN  !TIME < TSF%T500(TSF%N500) .AND. 
  !                                              POS = Z
  !                                              DO I=TWO, TSF%N500+ONE
  !                                                  IF(TIME < TSF%T500(I)) THEN
  !                                                      POS = (I - ONE)*QUIN - TEN
  !                                                      EXIT
  !                                                  END IF
  !                                              END DO
  !                                              IF(POS > Z .AND. TSF%P < POS) THEN !P = 5   POS =8
  !                                                  DO I = TSF%P + ONE, POS
  !                                                                         READ(TSF%FL%IU, *)
  !                                                  END DO
  !                                                  TSF%P = POS
  !                                                  CALL READ_NEXT_ROW_MONTHDAY(TSF)  !LOUD INTO MEMORY THE NEXT THREE LINES
  !                                                  CALL READ_NEXT_ROW_MONTHDAY(TSF)
  !                                                  CALL READ_NEXT_ROW_MONTHDAY(TSF)
  !                                              END IF
  !                                          END IF
  !                                          !
  !                                          DO WHILE(.NOT. EOF .AND. TIME >= TSF%NEXT%TIM)
  !                                                                                      CALL READ_NEXT_ROW_MONTHDAY(TSF)
  !                                          END DO
  !          END IF
  !      END IF
  !  END IF
  !  !
  !END SUBROUTINE
  !
  SUBROUTINE GO_TO_TIME_SERIES_POSITION(TSF, POS)
  CLASS(TIME_SERIES_FILE),  INTENT(INOUT):: TSF
  INTEGER,                  INTENT(IN   ):: POS
  INTEGER:: P
  LOGICAL:: EOF
  !
  P = POS
  IF(P > TSF%N  ) P = TSF%N
  IF(P < THREE  ) P = THREE
  !
  CALL MOVE_TO_START_OF_FILE(TSF)
  !
  DO WHILE (TSF%P < P .OR. TSF%P .NE. TSF%N)
        CALL READ_NEXT_ROW(TSF, EOF)
  END DO
  !
  END SUBROUTINE
  !
  SUBROUTINE READ_NEXT_ROW(TSF, EOF)
  CLASS(TIME_SERIES_FILE), INTENT(INOUT):: TSF
  LOGICAL,                 INTENT(OUT  ):: EOF
  INTEGER:: LLOC, ISTART, ISTOP
  !
  IF(TSF%P == TSF%N .OR. (TSF%P == TSF%N - TWO .AND. .NOT. TSF%AT_NEXT) .OR. TSF%N == Z) THEN
      EOF = TRUE
  ELSE
      EOF = FALSE
      IF(.NOT. TSF%AT_NEXT) THEN
          CALL READ_TO_DATA(TSF%LINE,TSF%FL%IU,TSF%IOUT)
          CALL READ_TO_DATA(TSF%LINE,TSF%FL%IU,TSF%IOUT)
          TSF%AT_NEXT = TRUE
          TSF%P = TSF%P + TWO 
      END IF
      !
      CALL READ_TO_DATA(TSF%LINE,TSF%FL%IU,TSF%IOUT)
      !
      TSF%P = TSF%P + ONE 
      !
      CALL SHIFT_FORWARD(TSF)
      !
      LLOC = ONE
      CALL GET_DATE(TSF%LINE,LLOC,ISTART,ISTOP,TSF%IOUT,TSF%FL%IU,TSF%NEXT%DATE, ONLY_DYEAR=TRUE ,MSG='FAILED TO LOAD TIME PART (FIRST NUMBER) OF THE TIME SERIES INPUT FILE.')
      TSF%NEXT%TIM = TSF%NEXT%DATE%DYEAR
      !
      CALL GET_NUMBER(TSF%LINE,LLOC,ISTART,ISTOP,TSF%IOUT,TSF%FL%IU,TSF%NEXT%DAT,MSG='FAILED TO LOAD DATA/VALUE PART (SECOND NUMBER) OF THE TIME SERIES INPUT FILE.')
      TSF%NEXT%DAT = TSF%NEXT%DAT
      !
  END IF
  !
  END SUBROUTINE
  !
  SUBROUTINE READ_PREV_ROW(TSF, EOF)
  CLASS(TIME_SERIES_FILE), INTENT(INOUT):: TSF
  LOGICAL,                 INTENT(OUT  ):: EOF
  INTEGER:: LLOC, ISTART, ISTOP
  !
  IF(TSF%P == ONE .OR. (TSF%P == THREE .AND. TSF%AT_NEXT) .OR. TSF%N == Z) THEN
      EOF = TRUE
  ELSE
      EOF = FALSE
      IF(TSF%AT_NEXT) THEN
          BACKSPACE(TSF%FL%IU)
          BACKSPACE(TSF%FL%IU)
          TSF%AT_NEXT = FALSE
          TSF%P = TSF%P - TWO 
      END IF
      !
      BACKSPACE(TSF%FL%IU)
      BACKSPACE(TSF%FL%IU)
      CALL READ_TO_DATA(TSF%LINE,TSF%FL%IU,TSF%IOUT)
      !
      TSF%P = TSF%P - ONE 
      !
      CALL SHIFT_BACKWARD(TSF)
      !
      LLOC = ONE
      CALL GET_DATE(TSF%LINE,LLOC,ISTART,ISTOP,TSF%IOUT,TSF%FL%IU,TSF%PREV%DATE, ONLY_DYEAR=TRUE ,MSG='FAILED TO LOAD TIME PART (FIRST NUMBER) OF THE TIME SERIES INPUT FILE.')
      TSF%PREV%TIM = TSF%PREV%DATE%DYEAR
      !
      CALL GET_NUMBER(TSF%LINE,LLOC,ISTART,ISTOP,TSF%IOUT,TSF%FL%IU,TSF%PREV%DAT,MSG='FAILED TO LOAD DATA/VALUE PART (SECOND NUMBER) OF THE TIME SERIES INPUT FILE.')
      TSF%PREV%DAT = TSF%PREV%DAT
  END IF
  !
  END SUBROUTINE
  !
  SUBROUTINE SHIFT_FORWARD(TSF)
      CLASS(TIME_SERIES_FILE), INTENT(INOUT):: TSF
      !
      TSF%TMP => TSF%PREV
      !
      TSF%PREV => TSF%CUR
      TSF%CUR  => TSF%NEXT
      TSF%NEXT => TSF%TMP
      !
      TSF%TMP  => NULL()
      !
  END SUBROUTINE
  !
  SUBROUTINE SHIFT_BACKWARD(TSF)
      CLASS(TIME_SERIES_FILE), INTENT(INOUT):: TSF
      !
      TSF%TMP => TSF%NEXT
      !
      TSF%NEXT => TSF%CUR
      TSF%CUR  => TSF%PREV
      TSF%PREV => TSF%TMP
      !
      TSF%TMP  => NULL()
      !
  END SUBROUTINE
  !
  SUBROUTINE READ_NEXT_ROW_MONTHDAY(TSF)
  CLASS(TIME_SERIES_FILE), INTENT(INOUT):: TSF
  INTEGER:: LLOC, ISTART, ISTOP, YR
  !
  IF(TSF%P == TSF%N .OR. (TSF%P == TSF%N - TWO .AND. .NOT. TSF%AT_NEXT)) THEN  ! .OR. TSF%N == Z SCOTT ERROR?!?!
      TSF%MD_FLAG = TWO
      TSF%P = Z
      !
      CALL TSF%FL%REWIND()
      !
  ELSEIF(TSF%MD_FLAG == TWO) THEN
      TSF%MD_FLAG = ONE
  ELSE
      TSF%MD_FLAG = Z
      !
  END IF
  !
  IF(.NOT. TSF%AT_NEXT .AND. TSF%MD_FLAG == Z) THEN
      CALL READ_TO_DATA(TSF%LINE,TSF%FL%IU,TSF%IOUT)
      CALL READ_TO_DATA(TSF%LINE,TSF%FL%IU,TSF%IOUT)
      TSF%AT_NEXT = TRUE
      TSF%P = TSF%P + TWO 
  END IF
  !
  CALL READ_TO_DATA(TSF%LINE,TSF%FL%IU,TSF%IOUT)
  !
  TSF%P = TSF%P + ONE 
  !
  YR = TSF%NEXT%DATE%YEAR  !Get files currently assinged year
  !
  IF(TSF%MD_FLAG == TWO) YR = YR + ONE !Reached end of file, increment the year.
  !
  CALL SHIFT_FORWARD(TSF)
  !
  LLOC = ONE
  CALL GET_DATE(TSF%LINE,LLOC,ISTART,ISTOP,TSF%IOUT,TSF%FL%IU,TSF%NEXT%DATE ,MSG='FAILED TO LOAD TIME PART (FIRST NUMBER) OF THE TIME SERIES INPUT FILE.')
  !
  CALL TSF%NEXT%DATE%SET_YEAR( YR )
  !
  TSF%NEXT%TIM = TSF%NEXT%DATE%DYEAR
  !
  CALL GET_NUMBER(TSF%LINE,LLOC,ISTART,ISTOP,TSF%IOUT,TSF%FL%IU,TSF%NEXT%DAT,MSG='FAILED TO LOAD DATA/VALUE PART (SECOND NUMBER) OF THE TIME SERIES INPUT FILE.')
  !
  END SUBROUTINE
  !
  SUBROUTINE READ_PREV_ROW_MONTHDAY(TSF, EOF, YR)
  CLASS(TIME_SERIES_FILE), INTENT(INOUT):: TSF
  LOGICAL,                 INTENT(OUT  ):: EOF
  INTEGER,                 INTENT(IN   ):: YR
  INTEGER:: LLOC, ISTART, ISTOP
  !
  IF(TSF%P == ONE .OR. (TSF%P == THREE .AND. TSF%AT_NEXT) .OR. TSF%N == Z) THEN
      EOF = TRUE
  ELSE
      EOF = FALSE
      IF(TSF%AT_NEXT) THEN
          BACKSPACE(TSF%FL%IU)
          BACKSPACE(TSF%FL%IU)
          TSF%AT_NEXT = FALSE
          TSF%P = TSF%P - TWO 
      END IF
      !
      BACKSPACE(TSF%FL%IU)
      BACKSPACE(TSF%FL%IU)
      CALL READ_TO_DATA(TSF%LINE,TSF%FL%IU,TSF%IOUT)
      !
      TSF%P = TSF%P - ONE 
      !
      TSF%TMP => TSF%NEXT
      !
      TSF%NEXT => TSF%CUR
      TSF%CUR  => TSF%PREV
      TSF%PREV => TSF%TMP
      !
      TSF%TMP  => NULL()
      !
      LLOC = ONE
      CALL GET_DATE(TSF%LINE,LLOC,ISTART,ISTOP,TSF%IOUT,TSF%FL%IU,TSF%PREV%DATE, ONLY_DYEAR=TRUE ,MSG='FAILED TO LOAD TIME PART (FIRST NUMBER) OF THE TIME SERIES INPUT FILE.')
      !
      CALL TSF%PREV%DATE%SET_YEAR( YR )
      TSF%PREV%TIM = TSF%PREV%DATE%DYEAR
      !
      CALL GET_NUMBER(TSF%LINE,LLOC,ISTART,ISTOP,TSF%IOUT,TSF%FL%IU,TSF%PREV%DAT,MSG='FAILED TO LOAD DATA/VALUE PART (SECOND NUMBER) OF THE TIME SERIES INPUT FILE.')
      TSF%PREV%DAT = TSF%PREV%DAT
  END IF
  !
  END SUBROUTINE
  !  
  PURE ELEMENTAL FUNCTION TIME_SERIES_FILE_IN_USE(TSF) RESULT(ANS)
  CLASS(TIME_SERIES_FILE),  INTENT(IN):: TSF
  LOGICAL:: ANS
  !
  ANS = TSF%N > Z
  !
  END FUNCTION
  !
  IMPURE ELEMENTAL SUBROUTINE MOVE_TIME_SERIES_TOO(TSF, TSF2)
    CLASS(TIME_SERIES_FILE), INTENT(INOUT):: TSF
    CLASS(TIME_SERIES_FILE), INTENT(  OUT):: TSF2
    !
    CALL DEALLOCATE_TIME_SERIES_FILE(TSF2)
    !
    ALLOCATE(TSF2%PREV, TSF2%CUR, TSF2%NEXT)
    TSF2%IOUT = TSF2%IOUT
    IF(ALLOCATED(TSF%NAM)) THEN
        ALLOCATE(TSF2%NAM, SOURCE = TSF%NAM)
    ELSE
        ALLOCATE(TSF2%NAM, SOURCE = 'n')
    END IF
    !
    TSF2%OPT      = TSF%OPT
    TSF2%MONTHDAY = TSF%MONTHDAY
    TSF2%N        = TSF%N
    TSF2%IOUT     = TSF%IOUT
    !TSF2%SFAC     = TSF%SFAC
    TSF2%OPT      = TSF%OPT
    TSF2%P        = TSF%P
    TSF2%N500     = TSF%N500
    !
    IF(TSF2%N500 > Z) ALLOCATE(TSF2%T500, SOURCE=TSF%T500)
    !
    IF(TSF%FL%NULL_FILE) THEN
        TSF2%FL%NULL_FILE = TRUE
        TSF2%AT_NEXT = TRUE
        !
        TSF2%PREV%TIM = ninf  
        TSF2%PREV%DAT = TSF%PREV%DAT
        TSF2%CUR %TIM = ninf
        TSF2%CUR %DAT = TSF%CUR %DAT
        TSF2%NEXT%TIM = ninf 
        TSF2%NEXT%DAT = TSF%NEXT%DAT
    ELSE
        CALL TSF%FL%MOVE( TSF2%FL )
        !
        CALL MOVE_TO_START_OF_FILE(TSF2)
    END IF
    !
    CALL DEALLOCATE_TIME_SERIES_FILE(TSF)
    !
  END SUBROUTINE
  !  
  IMPURE ELEMENTAL SUBROUTINE DEALLOCATE_TIME_SERIES_FILE(TSF)
    CLASS(TIME_SERIES_FILE), INTENT(INOUT):: TSF
    !
    IF(ASSOCIATED(TSF%PREV)) DEALLOCATE(TSF%PREV)
    IF(ASSOCIATED(TSF%CUR )) DEALLOCATE(TSF%CUR )
    IF(ASSOCIATED(TSF%NEXT)) DEALLOCATE(TSF%NEXT)
    TSF%PREV => NULL()
    TSF%CUR  => NULL()
    TSF%NEXT => NULL()
    TSF%TMP  => NULL()
    !
    IF(ALLOCATED (TSF%NAM )) DEALLOCATE(TSF%NAM )
    !
    CALL TSF%FL%CLOSE()
    !
    IF(ALLOCATED(TSF%T500)) DEALLOCATE(TSF%T500)
    TSF%N500 = Z
    !
    TSF%AT_NEXT  = FALSE
    TSF%MONTHDAY = FALSE
    TSF%BINARY   = FALSE
    TSF%IOUT = Z
    !TSF%SFAC = UNO
    TSF%N    = Z
    TSF%OPT  = Z
    TSF%P    = Z
    !TSF%CHK  = inf_I   
    !TSF%CHK_TIM = ninf
    !
  END SUBROUTINE
  !
  IMPURE ELEMENTAL SUBROUTINE FINAL_DEALLOCATE_TIME_SERIES_FILE(TSF)
    TYPE(TIME_SERIES_FILE), INTENT(INOUT):: TSF
    CALL DEALLOCATE_TIME_SERIES_FILE(TSF)
  END SUBROUTINE
END MODULE