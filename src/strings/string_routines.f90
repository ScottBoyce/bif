!
! CODE DEVELOPED BY SCOTT E BOYCE
!                   CONTACT <seboyce@usgs.gov> or <Boyce@engineer.com>
!
! MODULE LISTING:
!   STRINGS
!
!                           SUBROUTINES
!                                    GET                   -> Wrapper for all other GET routines
!                                    GET_NUMBER
!                                    GET_INTEGER
!                                    GET_WORD or RET_WORD
!                                    GET_DATE
!                                    GET_DOUBLE_DATE
!                                    GET_REAL_DATE
!                                    UPPER
!                                    COMMA_STRIP
!                                    TAB_STRIP
!                                    SPECIAL_BLANK_STRIP
!                                    NAME_LIST_DUBLICATE_CHECK
!                                    POST_CHECK_WORD    (WORD,POST,ALLOC)
!                                    PRE_CHECK_WORD     (WORD,PRE,ALLOC)
!
!                           FUNCTIONS
!                                    JOIN_TXT
!                                    IS_IN_STR
!                                    GO_UP
!                                    FPOST_CHECK_WORD   (WORD,POST)
!                                    FPRE_CHECK_WORD    (WORD,PRE)
!
!
!
!
!
MODULE STRINGS
  !
  USE CONSTANTS,                 ONLY: Z, ONE, TWO, inf_I, inf_R, ninf_R, DNEG, DZ, UNO, INF, NINF, BLNK, NL, BLN, TAB, LF, CR, lowerCHAR, upperCHAR, TRUE, FALSE
  USE PARSE_WORD_INTERFACE,      ONLY: PARSE_WORD
  USE DATE_OPERATOR_INSTRUCTION, ONLY: DATE_OPERATOR
  USE NUM2STR_INTERFACE,         ONLY: NUM2STR
  USE ERROR_INTERFACE,           ONLY: STOP_ERROR, WARNING_MESSAGE
  USE FILE_IO_INTERFACE,         ONLY: COMMENT_INDEX
  !
  USE, INTRINSIC:: IEEE_ARITHMETIC, ONLY: IEEE_VALUE, IEEE_QUIET_NAN, IEEE_IS_NAN, IEEE_POSITIVE_INF, IEEE_NEGATIVE_INF
  USE, INTRINSIC:: ISO_FORTRAN_ENV, ONLY: REAL32, REAL64, REAL128, INT8, INT16, INT32, INT64
  !
  IMPLICIT NONE
  !
  PRIVATE
  PUBLIC:: GET
  PUBLIC:: GET_NUMBER, GET_INTEGER, GET_DATE, GET_DOUBLE_DATE, GET_WORD
  PUBLIC:: UPPER, GO_UP
  PUBLIC:: IS_IN_STR, JOIN_TXT
  PUBLIC:: NAME_LIST_DUBLICATE_CHECK
  PUBLIC:: SPECIAL_BLANK_STRIP, COMMA_STRIP, TAB_STRIP
  PUBLIC:: PRE_CHECK_WORD, FPRE_CHECK_WORD, POST_CHECK_WORD, FPOST_CHECK_WORD
  !
  !---------------------------------------------------------------------------------------------------------------------
  ! Caution with using GET_WORD as it is also defined in PARSE_WORD_INTERFACE - Just dont double import the subroutine through USE statments
  !
  INTERFACE GET_WORD
    MODULE PROCEDURE GET_WORD_ASSUM        !GET_WORD(WORD,LN,LOC,         [OLD_LOC],[COM_STOP],[NO_UPCASE])
    MODULE PROCEDURE GET_WORD_ALLOC        !GET_WORD(WORD,LN,LOC,IS_ALLOC,[OLD_LOC],[COM_STOP],[NO_UPCASE])
    MODULE PROCEDURE GET_WORD_ASSUM_ISTART !GET_WORD(LN,LOC,ISTART,ISTOP,WORD,         [OLD_LOC],[COM_STOP],[NO_UPCASE])
    MODULE PROCEDURE GET_WORD_ALLOC_ISTART !GET_WORD(LN,LOC,ISTART,ISTOP,WORD,IS_ALLOC,[OLD_LOC],[COM_STOP],[NO_UPCASE])
  END INTERFACE
  !
  !---------------------------------------------------------------------------------------------------------------------
  !
  INTERFACE GET_NUMBER
    MODULE PROCEDURE GET_DOUBLE_VEC        !GET_NUMBER(LN,LOC,ISTART,ISTOP,IOUT,IN,VAL,                [MSG],[ERROR_VAL],[HAS_ERROR], [IOSTAT])
    MODULE PROCEDURE GET_DOUBLE_VAL        !GET_NUMBER(LN,LOC,ISTART,ISTOP,IOUT,IN,VAL,[NO_PARSE_WORD],[MSG],[ERROR_VAL],[HAS_ERROR], [IOSTAT])
    MODULE PROCEDURE GET_REAL_VEC          !GET_NUMBER(LN,LOC,ISTART,ISTOP,IOUT,IN,VAL,[NO_PARSE_WORD],[MSG],[ERROR_VAL],[HAS_ERROR], [IOSTAT])
    MODULE PROCEDURE GET_REAL_VAL          !GET_NUMBER(LN,LOC,ISTART,ISTOP,IOUT,IN,VAL,[NO_PARSE_WORD],[MSG],[ERROR_VAL],[HAS_ERROR], [IOSTAT])
    MODULE PROCEDURE GET_QUAD_VEC          !GET_NUMBER(LN,LOC,ISTART,ISTOP,IOUT,IN,VAL,                [MSG],[ERROR_VAL],[HAS_ERROR], [IOSTAT])
    MODULE PROCEDURE GET_QUAD_VAL          !GET_NUMBER(LN,LOC,ISTART,ISTOP,IOUT,IN,VAL,[NO_PARSE_WORD],[MSG],[ERROR_VAL],[HAS_ERROR], [IOSTAT])
  END INTERFACE
  !
  !INTERFACE GET_DOUBLE
  !  MODULE PROCEDURE GET_DOUBLE_VEC        !(LN,LOC,ISTART,ISTOP,IOUT,IN,VAL,                [MSG],[ERROR_VAL],[HAS_ERROR])
  !  MODULE PROCEDURE GET_DOUBLE_VAL        !(LN,LOC,ISTART,ISTOP,IOUT,IN,VAL,[NO_PARSE_WORD],[MSG],[ERROR_VAL],[HAS_ERROR])
  !END INTERFACE
  !
  INTERFACE GET_INTEGER
    MODULE PROCEDURE GET_INTEGER_VEC       !GET_INTEGER(LN,LOC,ISTART,ISTOP,IOUT,IN,VAL,                [MSG],[ERROR_VAL],[HAS_ERROR], [IOSTAT]
    MODULE PROCEDURE GET_INTEGER_VAL       !GET_INTEGER(LN,LOC,ISTART,ISTOP,IOUT,IN,VAL,[NO_PARSE_WORD],[MSG],[ERROR_VAL],[HAS_ERROR], [IOSTAT]
  END INTERFACE
  !
  INTERFACE GET_DATE
    MODULE PROCEDURE GET_DATE_VAL          !(LN,LOC,ISTART,ISTOP,IOUT,IN,    DATE,[NO_PARSE_WORD],[MSG],[HAS_ERROR],[FOUND_DATE],[ONLY_DYEAR],[NO_DATE],[TIME_SPACE], [IOSTAT])
    MODULE PROCEDURE GET_DOUBLE_DATE       !(LN,LOC,ISTART,ISTOP,IOUT,IN,VAL,DATE,[NO_PARSE_WORD],[MSG],[HAS_ERROR],[FOUND_DATE],[ONLY_DYEAR],[NO_DATE],[TIME_SPACE], [IOSTAT])
    MODULE PROCEDURE GET_REAL_DATE         !(LN,LOC,ISTART,ISTOP,IOUT,IN,VAL,DATE,[NO_PARSE_WORD],[MSG],[HAS_ERROR],[FOUND_DATE],[ONLY_DYEAR],[NO_DATE],[TIME_SPACE], [IOSTAT])
  END INTERFACE
  !GET_DATE_VAL(LN,LOC,ISTART,ISTOP,IOUT,IN,DATE,NO_PARSE_WORD,MSG, HAS_ERROR, FOUND_DATE,ONLY_DYEAR, NO_DATE, TIME_SPACE, IOSTAT)
  INTERFACE GET
    MODULE PROCEDURE PARSE_WORD            !(LN,LOC,ISTART,ISTOP,                 [COM_STOP], [FIND_NEXT], [OLD_LOC], [EOL])
    MODULE PROCEDURE GET_DATE_VAL          !(LN,LOC,ISTART,ISTOP,IOUT,IN,    DATE,[NO_PARSE_WORD],[MSG], [HAS_ERROR],[FOUND_DATE],[TIME_SPACE], [IOSTAT])
    MODULE PROCEDURE GET_DOUBLE_DATE       !(LN,LOC,ISTART,ISTOP,IOUT,IN,VAL,DATE,[NO_PARSE_WORD],[MSG], [HAS_ERROR],[FOUND_DATE],[TIME_SPACE], [IOSTAT])
    MODULE PROCEDURE GET_REAL_DATE         !(LN,LOC,ISTART,ISTOP,IOUT,IN,VAL,DATE,[NO_PARSE_WORD],[MSG], [HAS_ERROR],[FOUND_DATE],[TIME_SPACE], [IOSTAT])
    MODULE PROCEDURE GET_DOUBLE_VEC        !(LN,LOC,ISTART,ISTOP,IOUT,IN,VAL,                [MSG],[ERROR_VAL],[HAS_ERROR], [IOSTAT])
    MODULE PROCEDURE GET_DOUBLE_VAL        !(LN,LOC,ISTART,ISTOP,IOUT,IN,VAL,[NO_PARSE_WORD],[MSG],[ERROR_VAL],[HAS_ERROR], [IOSTAT])
    MODULE PROCEDURE GET_INTEGER_VEC       !(LN,LOC,ISTART,ISTOP,IOUT,IN,VAL,                [MSG],[ERROR_VAL],[HAS_ERROR], [IOSTAT])
    MODULE PROCEDURE GET_INTEGER_VAL       !(LN,LOC,ISTART,ISTOP,IOUT,IN,VAL,[NO_PARSE_WORD],[MSG],[ERROR_VAL],[HAS_ERROR], [IOSTAT])
    MODULE PROCEDURE GET_REAL_VEC          !(LN,LOC,ISTART,ISTOP,IOUT,IN,VAL,[NO_PARSE_WORD],[MSG],[ERROR_VAL],[HAS_ERROR], [IOSTAT])
    MODULE PROCEDURE GET_REAL_VAL          !(LN,LOC,ISTART,ISTOP,IOUT,IN,VAL,[NO_PARSE_WORD],[MSG],[ERROR_VAL],[HAS_ERROR], [IOSTAT])
    MODULE PROCEDURE GET_QUAD_VEC          !(LN,LOC,ISTART,ISTOP,IOUT,IN,VAL,                [MSG],[ERROR_VAL],[HAS_ERROR], [IOSTAT])
    MODULE PROCEDURE GET_QUAD_VAL          !(LN,LOC,ISTART,ISTOP,IOUT,IN,VAL,[NO_PARSE_WORD],[MSG],[ERROR_VAL],[HAS_ERROR], [IOSTAT])
    MODULE PROCEDURE GET_WORD_ASSUM_ISTART !(LN,LOC,ISTART,ISTOP,WORD,         [OLD_LOC],[COM_STOP],[NO_UPCASE])
    MODULE PROCEDURE GET_WORD_ALLOC_ISTART !(LN,LOC,ISTART,ISTOP,WORD,IS_ALLOC,[OLD_LOC],[COM_STOP],[NO_UPCASE])
    MODULE PROCEDURE GET_WORD_ASSUM        !(WORD,LN,LOC,         [OLD_LOC],[COM_STOP],[NO_UPCASE])
    MODULE PROCEDURE GET_WORD_ALLOC        !(WORD,LN,LOC,IS_ALLOC,[OLD_LOC],[COM_STOP],[NO_UPCASE])
  END INTERFACE
  !
  !
  CONTAINS
  !
  PURE FUNCTION GO_UP(LINE,NOTRIM) RESULT(UPLINE)
    CHARACTER(*),      INTENT(IN):: LINE
    LOGICAL, OPTIONAL, INTENT(IN):: NOTRIM
    CHARACTER(:), ALLOCATABLE:: UPLINE
    !
    IF(PRESENT(NOTRIM)) THEN
        IF(NOTRIM) THEN
                       ALLOCATE(UPLINE, SOURCE=LINE)
        ELSE
                       ALLOCATE(UPLINE, SOURCE=TRIM(LINE))
        END IF
    ELSE
                       ALLOCATE(UPLINE, SOURCE=TRIM(LINE))
    END IF
    !
    CALL UPPER(UPLINE)
    !
  END FUNCTION
  !
  !#########################################################################################################################
  !
  PURE ELEMENTAL SUBROUTINE UPPER(LN)
    CHARACTER(*), INTENT(INOUT):: LN
    INTEGER:: I, N
    !
    DO I=1, LEN_TRIM(LN)
        N = INDEX( lowerCHAR, LN(I:I))
        !
        IF(N > 0) LN(I:I) = upperCHAR(N:N)
    END DO
  END SUBROUTINE
  !!!PURE ELEMENTAL SUBROUTINE UPPER(LN)
  !!!  CHARACTER(*),INTENT(INOUT):: LN
  !!!  INTEGER::I
  !!!  !
  !!!  DO CONCURRENT(I=1:LEN(LN), LN(I:I).GE.'a' .AND. LN(I:I).LE.'z');  LN(I:I)=CHAR(ICHAR(LN(I:I))+Lower_to_Upper)
  !!!  END DO
  !!!  !
  !!!END SUBROUTINE
  !
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !
  !  PRE/POST WORD APPED ROUTINES routines
  !
  !#############################################################################################################################################################
  !
  PURE FUNCTION FPOST_CHECK_WORD(WORD,POST) RESULT(PWORD) ! CHECKS IF POST IS AT END OF WORD, IF NOT, THEN ADDS IT. IF WORD IS EMPTY THEN RETURNS TRIM(WORD)
    CHARACTER(*),  INTENT(IN):: WORD        ! LINE THAT IS CHECKED FOR POST
    CHARACTER(*),  INTENT(IN):: POST        ! IF NOT AT END OF WORD, THEN ADDED
    CHARACTER(:), ALLOCATABLE:: PWORD       ! RETURN WORD WITH POST
    INTEGER:: N, M, P
    N = LEN(WORD)
    M = LEN(POST)
    P = N - M + ONE
    !
    IF(N < ONE) THEN
        PWORD = ''
    ELSEIF(WORD == BLNK)THEN
        PWORD = ''
    ELSEIF(P < ONE)THEN
        PWORD = WORD//POST
    ELSEIF(WORD(P:N) /= POST)THEN
        PWORD = WORD//POST
    ELSE
        PWORD = WORD
    END IF
    !
  END FUNCTION
  !
  !#############################################################################################################################################################
  !
  PURE SUBROUTINE POST_CHECK_WORD(WORD,POST,ALLOC)         ! CHECKS IF POST IS AT END OF WORD, IF NOT, THEN ADDS IT. IF WORD IS EMPTY THEN RETURNS TRIM(WORD)
    CHARACTER(:), ALLOCATABLE, INTENT(INOUT):: WORD        ! LINE THAT IS CHECKED FOR POST
    CHARACTER(*),              INTENT(IN   ):: POST        ! IF NOT AT END OF WORD, THEN ADDED
    LOGICAL,         OPTIONAL, INTENT(IN   ):: ALLOC       ! IF SET and WORD is not allocated, the allocate it to POST
    INTEGER:: N, M, P
    LOGICAL:: CHECK
    !
    IF(.NOT. ALLOCATED(WORD)) THEN
        !
        CHECK = PRESENT(ALLOC)
        IF(CHECK) CHECK = ALLOC
        IF(CHECK) WORD  = POST
    ELSE
        N = LEN(WORD)
        M = LEN(POST)
        P = N - M + ONE
        !
        IF(N > Z         ) THEN
        IF(WORD /= BLNK) THEN
            IF(P < ONE)THEN
                WORD = WORD//POST
            ELSEIF(WORD(P:N) /= POST)THEN
                WORD = WORD//POST
            END IF
        END IF
        END IF
    END IF
    !
  END SUBROUTINE
  !
  !#############################################################################################################################################################
  !
  PURE FUNCTION FPRE_CHECK_WORD(WORD,PRE) RESULT(PWORD) ! CHECKS IF PRE IS AT END OF WORD, IF NOT, THEN ADDS IT. IF WORD IS EMPTY THEN RETURNS TRIM(WORD)
    CHARACTER(*),  INTENT(IN):: WORD        ! LINE THAT IS CHECKED FOR PRE
    CHARACTER(*),  INTENT(IN):: PRE         ! IF NOT AT END OF WORD, THEN ADDED
    CHARACTER(:), ALLOCATABLE:: PWORD       ! RETURN WORD WITH PRE
    INTEGER:: N, M
    N = LEN(WORD)
    M = LEN(PRE)
    !
    IF(N < ONE) THEN
        PWORD = ''
    ELSEIF(WORD == BLNK)THEN
        PWORD = ''
    ELSEIF(N < M)THEN
        PWORD = PRE//WORD
    ELSEIF(WORD(1:M) /= PRE)THEN
        PWORD = PRE//WORD
    ELSE
        PWORD = WORD
    END IF
    !
  END FUNCTION
  !
  !#############################################################################################################################################################
  !
  PURE SUBROUTINE PRE_CHECK_WORD(WORD,PRE,ALLOC) ! CHECKS IF PRE IS AT END OF WORD, IF NOT, THEN ADDS IT. IF WORD IS EMPTY THEN RETURNS TRIM(WORD)
    CHARACTER(:), ALLOCATABLE, INTENT(INOUT):: WORD        ! LINE THAT IS CHECKED FOR PRE
    CHARACTER(*),              INTENT(IN   ):: PRE         ! IF NOT AT END OF WORD, THEN ADDED
    LOGICAL,         OPTIONAL, INTENT(IN   ):: ALLOC       ! IF SET and WORD is not allocated, the allocate it to PRE
    INTEGER:: N, M
    LOGICAL:: CHECK
    !
    IF(.NOT. ALLOCATED(WORD)) THEN
        !
        CHECK = PRESENT(ALLOC)
        IF(CHECK) CHECK = ALLOC
        IF(CHECK) WORD  = PRE
    ELSE
        N = LEN(WORD)
        M = LEN(PRE)
        !
        IF(N > Z         ) THEN
        IF(WORD /= BLNK) THEN
            IF(N < M)THEN
                WORD = PRE//WORD
            ELSEIF(WORD(1:M) /= PRE)THEN
                WORD = PRE//WORD
            END IF
        END IF
        END IF
    END IF
    !
  END SUBROUTINE
  !
  !#########################################################################################################################
  !
  SUBROUTINE NAME_LIST_DUBLICATE_CHECK(NAMES,OUTPUT)
    CHARACTER(*),DIMENSION(:),CONTIGUOUS,INTENT(IN)::NAMES
    INTEGER,OPTIONAL:: OUTPUT
    !
    INTEGER:: I, IOUT
    !
    IF(PRESENT(OUTPUT)) THEN
        IOUT = OUTPUT
    ELSE
        IOUT = Z
    END IF
    !
    DO I=2, SIZE(NAMES)
        IF( ANY( NAMES(I-1) == NAMES(I:) ) )  CALL STOP_ERROR(OUTPUT=IOUT, MSG='UNIQUE NAME IDENTIFIER IS NOT UNIQUE. FOUND DUBLICATE IDENTIFIER FOR NAME: "'//TRIM(NAMES(I-1))//'"')
    END DO
    !
  END SUBROUTINE
  !
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !
  !  GET routines
  !
  SUBROUTINE GET_DATE_VAL(LN,LOC,ISTART,ISTOP,IOUT,IN,DATE,NO_PARSE_WORD,MSG, HAS_ERROR, FOUND_DATE,ONLY_DYEAR, NO_DATE, TIME_SPACE, IOSTAT)  !IF NO_PARSE_WORD=TRUE, THEN ASSUMES ISTART AND ISTOP ARE ALREADY SET TO CORRECT LOCATION
    CHARACTER(*),           INTENT(IN   ):: LN                ! LINE TO PARSE DOUBLE FROM
    INTEGER,                INTENT(INOUT):: LOC,ISTART,ISTOP  ! LOC => STARTING LOCATION TO FIND NUMBER, NUMBER AT EXIT IS LOCATED AT LN(ISTART:ISTOP) AND LOC = ISTOP+1
    INTEGER,                INTENT(IN   ):: IOUT, IN          ! ERROR UNIT TO WRITE TO, FILE UNIT THAT LN ORIGINATED FROM
    CLASS(DATE_OPERATOR),   INTENT(OUT  ):: DATE
    LOGICAL,      OPTIONAL, INTENT(IN   ):: NO_PARSE_WORD     ! INDCIATES THAT ISTART AND ISTOP ALREADY POINT TO NUMBER
    CHARACTER(*), OPTIONAL, INTENT(IN   ):: MSG               ! ERROR MESSAGE PRINTED WHEN FAIL TO LOAD NUMBER AND STOP PROGRAM, IF "NOSTOP" THEN VAL IS SET TO NaN
    LOGICAL,      OPTIONAL, INTENT(  OUT):: HAS_ERROR         ! SET TO TRUE IF THERE IS AN ERROR
    LOGICAL,      OPTIONAL, INTENT(  OUT):: FOUND_DATE        ! SET TO TRUE IF THE DATE WAS FOUND AND NOT THE DOUBLE
    LOGICAL,      OPTIONAL, INTENT(IN   ):: ONLY_DYEAR        ! IF SET TO TRUE THEN ONLY THE DYEAR IS CALCLATED
    LOGICAL,      OPTIONAL, INTENT(IN   ):: NO_DATE           ! IF SET TO TRUE THEN AN ERROR IS *NOT* RAISED IF INPUT SPECIFED 'NO_DATE'
    LOGICAL,      OPTIONAL, INTENT(IN   ):: TIME_SPACE        ! IF TRUE then allow blank spaces/tabs to separte date and time in addition to Tt
    INTEGER,      OPTIONAL, INTENT(  OUT):: IOSTAT            ! SET to zero if there is no error, otherwise non-zero error code
    LOGICAL:: CHECK, TSPACE
    CHARACTER(:), ALLOCATABLE:: ERRMSG
    !
    IF(PRESENT(HAS_ERROR)) HAS_ERROR = FALSE
    !
    IF(PRESENT(NO_PARSE_WORD)) THEN
        CHECK = .NOT. NO_PARSE_WORD
    ELSE
        CHECK = TRUE
    END IF
    !
    IF(PRESENT(TIME_SPACE)) THEN
        TSPACE = TIME_SPACE
    ELSE
        TSPACE = FALSE
    END IF
    !
    IF(CHECK) CALL PARSE_WORD(LN,LOC,ISTART,ISTOP)
    !
    IF(TSPACE .AND. CHECK) THEN  !CHECK IF PARSE_WORD NEEDS TO BE ADJUSTED TO ACCOUNT FOR ANY BLANK SPACES BETWEEN DATE AND CLOCK
          BLOCK
               INTEGER:: I,J
               I = ISTART
               J = ISTOP
               CALL PARSE_WORD(LN,LOC,ISTART,ISTOP)
               !
               IF(INDEX(LN(ISTART:ISTOP),':') > Z) THEN
                   ISTART = I
               ELSE
                   ISTART = I
                   ISTOP  = J
               END IF
          END BLOCK
    END IF
    !
    CHECK = TRUE
    IF(PRESENT(NO_DATE)) THEN
       IF(NO_DATE) THEN
                   ALLOCATE(ERRMSG, SOURCE=LN(ISTART:ISTOP))
                   CALL UPPER(ERRMSG)
                   IF(ERRMSG == 'NO_DATE' .OR. ERRMSG == 'NODATE' .OR. ERRMSG == 'NAN' .OR. ERRMSG == 'SKIP') THEN
                       CALL DATE%INIT( 'NO_DATE', FOUND_DATE = FOUND_DATE )
                       RETURN !------------------------------------------^
                   ELSE
                       DEALLOCATE(ERRMSG)
                   END IF
       END IF
    END IF
    !
    CALL DATE%INIT( LN(ISTART:ISTOP), FOUND_DATE = FOUND_DATE, ONLY_DYEAR = ONLY_DYEAR )
    !
    IF( DATE%NOT_SET() ) THEN
          CHECK = TRUE
          IF(PRESENT(MSG)) THEN
               !
               IF (MSG=='NOSTOP') THEN
                   CHECK = FALSE
               END IF
               IF(PRESENT(HAS_ERROR)) HAS_ERROR = TRUE
          ELSE IF(PRESENT(IOSTAT)) THEN
                          IOSTAT = 163
                          CHECK  = FALSE
               IF(PRESENT(HAS_ERROR)) HAS_ERROR = TRUE
          ELSE IF(PRESENT(HAS_ERROR)) THEN
               !
               HAS_ERROR = TRUE
               CHECK  = FALSE
          END IF
          !
          IF(CHECK) THEN
              ERRMSG = 'GET_DATE READ UTILITY ERROR: FAILED TO CONVERT TEXT TO A CALENDAR DATE/DECIMAL YEAR.'//BLN//'THE FOLLOWING IS THE TEXT ATTEMPTED TO BE CONVERTED "'//LN(ISTART:ISTOP)//'".'//BLN//'THE ACCEPTED DATE FORMATS ARE "mm/dd/YYYY" OR "YYYY-mm-dd" AND THE FOLLOWING SETS THE DAY AUTOMATICALLY TO ONE: "mm/YYYY" OR "YYYY-mm"'//NL//'IT IS ALSO POSSIBLE TO WRITE OUT THE NAME OF THE MONTH WHERE mmm REFERS TO THE THREE LETTER MONTH ABBREVIATION OR FULL MONTH NAME: "mmm/dd/yyyy" OR "mmm-dd-yyyy"'//BLN//'FOR SOME SELECT INPUTS YOU MAY ALSO SPECIFY ONLY THE MONTH AND DAY, WHICH THEN AUTOMATICALLY APPENDS THE YEAR (OR IF NOT SUPPORED SETS THE YEAR TO ZERO). THIS FORMAT IS AS FOLLOWS: "mm\dd" OR "mmm-dd" OR "mmm/dd" OR SET THE DAY AUTOMATICALLY TO ONE WITH "mmm"'//BLN//'IF YOU WANT TO ADD A 24-HOUR TIME TO IT YOU MUST ADD TO THE DATE "Thh:mm:ss"'//NL//'(e.g. "YYYY-mm-ddThh:mm:ss")'
              IF(LN(ISTART:ISTOP)==BLNK) ERRMSG = ERRMSG//BLN//'THE POSSIBLE REASON FOR THIS ERROR IS DUE TO READING IN A BLANK/EMPTY LINE OR YOU DID NOT PROVIDE ENOUGH NUMBERS ON THE LINE.'
              IF(PRESENT(MSG)) ERRMSG = ERRMSG//BLN//'THE FOLLOWING IS AN ADDITIONAL COMMENT PASSED TO GET_NUMBER:'//BLN//TRIM(MSG)
              !
              CALL STOP_ERROR(LN, IN, IOUT, ERRMSG )
          END IF
    END IF
    !
  END SUBROUTINE
  !
  !#############################################################################################################################################################
  !
  SUBROUTINE GET_DOUBLE_DATE(LN,LOC,ISTART,ISTOP,IOUT,IN,VAL,DATE,NO_PARSE_WORD,MSG,HAS_ERROR,FOUND_DATE,ONLY_DYEAR,TIME_SPACE, IOSTAT)  !IF NO_PARSE_WORD=TRUE, THEN ASSUMES ISTART AND ISTOP ARE ALREADY SET TO CORRECT LOCATION
    CHARACTER(*),           INTENT(IN   ):: LN                ! LINE TO PARSE DOUBLE FROM
    INTEGER,                INTENT(INOUT):: LOC,ISTART,ISTOP  ! LOC => STARTING LOCATION TO FIND NUMBER, NUMBER AT EXIT IS LOCATED AT LN(ISTART:ISTOP) AND LOC = ISTOP+1
    INTEGER,                INTENT(IN   ):: IOUT, IN          ! ERROR UNIT TO WRITE TO, FILE UNIT THAT LN ORIGINATED FROM
    DOUBLE PRECISION,       INTENT(OUT  ):: VAL               ! DOUBLE VALUE TO RETURN
    CLASS(DATE_OPERATOR),   INTENT(OUT  ):: DATE
    LOGICAL,      OPTIONAL, INTENT(IN   ):: NO_PARSE_WORD     ! INDCIATES THAT ISTART AND ISTOP ALREADY POINT TO NUMBER
    CHARACTER(*), OPTIONAL, INTENT(IN   ):: MSG               ! ERROR MESSAGE PRINTED WHEN FAIL TO LOAD NUMBER AND STOP PROGRAM, IF "NOSTOP" THEN VAL IS SET TO NaN
    LOGICAL,      OPTIONAL, INTENT(  OUT):: HAS_ERROR         ! SET TO TRUE IF THERE IS AN ERROR
    LOGICAL,      OPTIONAL, INTENT(  OUT):: FOUND_DATE        ! SET TO TRUE IF THE DATE WAS FOUND AND NOT THE DOUBLE
    LOGICAL,      OPTIONAL, INTENT(IN   ):: ONLY_DYEAR        ! IF SET TO TRUE THEN ONLY THE DYEAR IS CALCLATED
    LOGICAL,      OPTIONAL, INTENT(IN   ):: TIME_SPACE        ! IF TRUE then allow blank spaces/tabs to separte date and time in addition to Tt
    INTEGER,      OPTIONAL, INTENT(  OUT):: IOSTAT            ! SET to zero if there is no error, otherwise non-zero error code
    !
    CALL GET_DATE_VAL(LN,LOC,ISTART,ISTOP,IOUT,IN,DATE,NO_PARSE_WORD,MSG, HAS_ERROR, FOUND_DATE,ONLY_DYEAR, TIME_SPACE=TIME_SPACE,IOSTAT=IOSTAT)
    !
    VAL = DATE%DYEAR
    !
  END SUBROUTINE
  !
  !#############################################################################################################################################################
  !
  SUBROUTINE GET_REAL_DATE(LN,LOC,ISTART,ISTOP,IOUT,IN,VAL,DATE,NO_PARSE_WORD,MSG,HAS_ERROR,FOUND_DATE,ONLY_DYEAR,TIME_SPACE, IOSTAT)  !IF NO_PARSE_WORD=TRUE, THEN ASSUMES ISTART AND ISTOP ARE ALREADY SET TO CORRECT LOCATION
    CHARACTER(*),           INTENT(IN   ):: LN                ! LINE TO PARSE DOUBLE FROM
    INTEGER,                INTENT(INOUT):: LOC,ISTART,ISTOP  ! LOC => STARTING LOCATION TO FIND NUMBER, NUMBER AT EXIT IS LOCATED AT LN(ISTART:ISTOP) AND LOC = ISTOP+1
    INTEGER,                INTENT(IN   ):: IOUT, IN          ! ERROR UNIT TO WRITE TO, FILE UNIT THAT LN ORIGINATED FROM
    REAL(REAL32),           INTENT(OUT  ):: VAL               ! SINGLE PREC VALUE TO RETURN
    CLASS(DATE_OPERATOR),   INTENT(OUT  ):: DATE
    LOGICAL,      OPTIONAL, INTENT(IN   ):: NO_PARSE_WORD     ! INDCIATES THAT ISTART AND ISTOP ALREADY POINT TO NUMBER
    CHARACTER(*), OPTIONAL, INTENT(IN   ):: MSG               ! ERROR MESSAGE PRINTED WHEN FAIL TO LOAD NUMBER AND STOP PROGRAM, IF "NOSTOP" THEN VAL IS SET TO NaN
    LOGICAL,      OPTIONAL, INTENT(  OUT):: HAS_ERROR         ! SET TO TRUE IF THERE IS AN ERROR
    LOGICAL,      OPTIONAL, INTENT(  OUT):: FOUND_DATE        ! SET TO TRUE IF THE DATE WAS FOUND AND NOT THE DOUBLE
    LOGICAL,      OPTIONAL, INTENT(IN   ):: ONLY_DYEAR        ! IF SET TO TRUE THEN ONLY THE DYEAR IS CALCLATED
    LOGICAL,      OPTIONAL, INTENT(IN   ):: TIME_SPACE        ! IF TRUE then allow blank spaces/tabs to separte date and time in addition to Tt
    INTEGER,      OPTIONAL, INTENT(  OUT):: IOSTAT            ! SET to zero if there is no error, otherwise non-zero error code
    !
    CALL GET_DATE_VAL(LN,LOC,ISTART,ISTOP,IOUT,IN,DATE,NO_PARSE_WORD,MSG, HAS_ERROR, FOUND_DATE,ONLY_DYEAR, TIME_SPACE=TIME_SPACE,IOSTAT=IOSTAT)
    !
    VAL = SNGL(DATE%DYEAR)
    !
  END SUBROUTINE
  !
  !#############################################################################################################################################################
  !
  !SUBROUTINE GET_DOUBLE_DATE(LN,LOC,ISTART,ISTOP,IOUT,IN,VAL,DATE,NO_PARSE_WORD,MSG,ERROR_VAL,NODATE,HAS_ERROR,FOUND_DATE)  !IF NO_PARSE_WORD=TRUE, THEN ASSUMES ISTART AND ISTOP ARE ALREADY SET TO CORRECT LOCATION
  !  CHARACTER(*),           INTENT(IN   ):: LN                ! LINE TO PARSE DOUBLE FROM
  !  INTEGER,                INTENT(INOUT):: LOC,ISTART,ISTOP  ! LOC => STARTING LOCATION TO FIND NUMBER, NUMBER AT EXIT IS LOCATED AT LN(ISTART:ISTOP) AND LOC = ISTOP+1
  !  INTEGER,                INTENT(IN   ):: IOUT, IN          ! ERROR UNIT TO WRITE TO, FILE UNIT THAT LN ORIGINATED FROM
  !  DOUBLE PRECISION,       INTENT(OUT  ):: VAL               ! DOUBLE VALUE TO RETURN
  !  CLASS(DATE_OPERATOR),   INTENT(OUT  ):: DATE
  !  LOGICAL,      OPTIONAL, INTENT(IN   ):: NO_PARSE_WORD     ! INDCIATES THAT ISTART AND ISTOP ALREADY POINT TO NUMBER
  !  CHARACTER(*), OPTIONAL, INTENT(IN   ):: MSG               ! ERROR MESSAGE PRINTED WHEN FAIL TO LOAD NUMBER AND STOP PROGRAM, IF "NOSTOP" THEN VAL IS SET TO NaN
  !  DOUBLE PRECISION,OPTIONAL,INTENT(IN ):: ERROR_VAL         ! IF FAIL TO LOAD NUMBER, DO NOT STOP, BUT INSTEAD SET VAL =  ERROR_VAL
  !  LOGICAL,         OPTIONAL,INTENT(IN ):: NODATE
  !  LOGICAL,      OPTIONAL, INTENT(  OUT):: HAS_ERROR         ! SET TO TRUE IF THERE IS AN ERROR
  !  LOGICAL,      OPTIONAL, INTENT(  OUT):: FOUND_DATE        ! SET TO TRUE IF THE DATE WAS FOUND AND NOT THE DOUBLE
  !  LOGICAL:: GET_WORD, INCLUDE_DATE
  !  INTEGER:: IERR
  !  CHARACTER(:), ALLOCATABLE:: ERRMSG
  !  !
  !  IF(PRESENT(HAS_ERROR )) HAS_ERROR  = FALSE
  !  IF(PRESENT(FOUND_DATE)) FOUND_DATE = FALSE
  !  !
  !  IF(PRESENT(NODATE)) THEN
  !      INCLUDE_DATE = .NOT. NODATE
  !  ELSE
  !      INCLUDE_DATE = TRUE
  !  END IF
  !  !
  !  IF(PRESENT(NO_PARSE_WORD)) THEN
  !      GET_WORD = .NOT. NO_PARSE_WORD
  !  ELSE
  !      GET_WORD = TRUE
  !  END IF
  !  !
  !  IF(GET_WORD) CALL PARSE_WORD(LN,LOC,ISTART,ISTOP)
  !  !
  !  READ(LN(ISTART:ISTOP),*, IOSTAT=IERR) VAL
  !  !
  !  IF(IERR /= Z) THEN
  !                           CALL DATE%INIT( LN(ISTART:ISTOP) )
  !                           !
  !                           IF( DATE%IS_SET() ) THEN
  !                               VAL = DATE%DYEAR
  !                               IERR = Z
  !                           END IF
  !                           IF(PRESENT(FOUND_DATE)) FOUND_DATE = TRUE
  !  ELSEIF(INCLUDE_DATE) THEN
  !                           CALL DATE%INIT( VAL )
  !  END IF
  !  !
  !  IF(IERR /= Z .OR. LN(ISTART:ISTOP)==BLNK) THEN
  !        GET_WORD = TRUE
  !        IF(PRESENT(ERROR_VAL)) THEN
  !                 !
  !                 GET_WORD = FALSE
  !                 VAL = ERROR_VAL
  !                 DATE%DYEAR = ERROR_VAL
  !                 !
  !        ELSEIF(PRESENT(MSG)) THEN
  !             !
  !             IF (MSG=='NOSTOP') THEN
  !                 GET_WORD = FALSE
  !                 VAL = IEEE_VALUE(VAL, IEEE_QUIET_NAN)  !SET VAL TO NaN WHEN THERE IS NOSTOP OPTION
  !             END IF
  !             IF(PRESENT(HAS_ERROR)) HAS_ERROR = TRUE
  !        ELSE IF(PRESENT(HAS_ERROR)) THEN
  !             !
  !             HAS_ERROR = TRUE
  !             GET_WORD  = FALSE
  !             VAL = IEEE_VALUE(VAL, IEEE_QUIET_NAN)  !SET VAL TO NaN WHEN THERE IS NOSTOP OPTION
  !        END IF
  !        !
  !        IF(GET_WORD) THEN
  !            ERRMSG = 'GET_DOUBLE_DATE READ UTILITY ERROR: FAILED TO CONVERT TEXT TO DOUBLE PRECISION NUMBER OR TO A CALENDAR DATE/DECIMAL YEAR.'//BLN//'THE FOLLOWING IS THE TEXT ATTEMPTED TO BE CONVERTED "'//LN(ISTART:ISTOP)//'".'
  !            IF(PRESENT(MSG)) ERRMSG = ERRMSG//BLN//'THE FOLLOWING IS AN ADDITIONAL COMMENT PASSED TO GET_DOUBLE_DATE:'//BLN//TRIM(MSG)
  !            !
  !            CALL STOP_ERROR(LN, IN, IOUT, ERRMSG )
  !        END IF
  !  END IF
  !  !
  !END SUBROUTINE
  !
  !#############################################################################################################################################################
  !
  !SUBROUTINE GET_REAL_DATE(LN,LOC,ISTART,ISTOP,IOUT,IN,VAL,DATE,NO_PARSE_WORD,MSG,ERROR_VAL, NODATE, HAS_ERROR)  !IF NO_PARSE_WORD=TRUE, THEN ASSUMES ISTART AND ISTOP ARE ALREADY SET TO CORRECT LOCATION
  !  CHARACTER(*),           INTENT(IN   ):: LN                ! LINE TO PARSE DOUBLE FROM
  !  INTEGER,                INTENT(INOUT):: LOC,ISTART,ISTOP  ! LOC => STARTING LOCATION TO FIND NUMBER, NUMBER AT EXIT IS LOCATED AT LN(ISTART:ISTOP) AND LOC = ISTOP+1
  !  INTEGER,                INTENT(IN   ):: IOUT, IN          ! ERROR UNIT TO WRITE TO, FILE UNIT THAT LN ORIGINATED FROM
  !  REAL,                   INTENT(OUT  ):: VAL               ! REAL VALUE TO RETURN
  !  CLASS(DATE_OPERATOR),   INTENT(OUT  ):: DATE
  !  LOGICAL,      OPTIONAL, INTENT(IN   ):: NO_PARSE_WORD     ! INDCIATES THAT ISTART AND ISTOP ALREADY POINT TO NUMBER
  !  CHARACTER(*), OPTIONAL, INTENT(IN   ):: MSG               ! ERROR MESSAGE PRINTED WHEN FAIL TO LOAD NUMBER AND STOP PROGRAM, IF "NOSTOP" THEN VAL IS SET TO NaN
  !  REAL,         OPTIONAL, INTENT(IN   ):: ERROR_VAL         ! IF FAIL TO LOAD NUMBER, DO NOT STOP, BUT INSTEAD SET VAL =  ERROR_VAL
  !  LOGICAL,         OPTIONAL,INTENT(IN ):: NODATE
  !  LOGICAL,      OPTIONAL, INTENT(  OUT):: HAS_ERROR         ! SET TO TRUE IF THERE IS AN ERROR
  !  LOGICAL:: GET_WORD, INCLUDE_DATE
  !  INTEGER:: IERR
  !  CHARACTER(:), ALLOCATABLE:: ERRMSG
  !  !
  !  IF(PRESENT(HAS_ERROR)) HAS_ERROR = FALSE
  !  !
  !  IF(PRESENT(NODATE)) THEN
  !      INCLUDE_DATE = .NOT. NODATE
  !  ELSE
  !      INCLUDE_DATE = TRUE
  !  END IF
  !  !
  !  IF(PRESENT(NO_PARSE_WORD)) THEN
  !      GET_WORD = .NOT. NO_PARSE_WORD
  !  ELSE
  !      GET_WORD = TRUE
  !  END IF
  !  !
  !  IF(GET_WORD) CALL PARSE_WORD(LN,LOC,ISTART,ISTOP)
  !  !
  !  READ(LN(ISTART:ISTOP),*, IOSTAT=IERR) VAL
  !  !
  !  IF(IERR /= Z) THEN
  !                           CALL DATE%INIT( LN(ISTART:ISTOP) )
  !                           !
  !                           IF(DATE%IS_SET()) THEN
  !                               VAL = REAL(DATE%DYEAR)
  !                               IERR = Z
  !                           END IF
  !  ELSEIF(INCLUDE_DATE) THEN
  !                           CALL DATE%INIT( VAL )
  !  END IF
  !  !
  !  IF(IERR /= Z .OR. LN(ISTART:ISTOP)==BLNK) THEN
  !        GET_WORD = TRUE
  !        IF(PRESENT(ERROR_VAL)) THEN
  !                 !
  !                 GET_WORD = FALSE
  !                 VAL = ERROR_VAL
  !                 DATE%DYEAR = ERROR_VAL
  !                 !
  !        ELSEIF(PRESENT(MSG)) THEN
  !             !
  !             IF (MSG=='NOSTOP') THEN
  !                 GET_WORD = FALSE
  !                 VAL = IEEE_VALUE(VAL, IEEE_QUIET_NAN)  !SET VAL TO NaN WHEN THERE IS NOSTOP OPTION
  !             END IF
  !             IF(PRESENT(HAS_ERROR)) HAS_ERROR = TRUE
  !        ELSE IF(PRESENT(HAS_ERROR)) THEN
  !             !
  !             HAS_ERROR = TRUE
  !             GET_WORD  = FALSE
  !             VAL = IEEE_VALUE(VAL, IEEE_QUIET_NAN)  !SET VAL TO NaN WHEN THERE IS NOSTOP OPTION
  !        END IF
  !        !
  !        IF(GET_WORD) THEN
  !            ERRMSG = 'GET_DOUBLE_DATE READ UTILITY ERROR: FAILED TO CONVERT TEXT TO DOUBLE PRECISION NUMBER OR TO A CALENDAR DATE/DECIMAL YEAR.'//BLN//'THE FOLLOWING IS THE TEXT ATTEMPTED TO BE CONVERTED "'//LN(ISTART:ISTOP)//'".'
  !            IF(PRESENT(MSG)) ERRMSG = ERRMSG//BLN//'THE FOLLOWING IS AN ADDITIONAL COMMENT PASSED TO GET_NUMBER:'//BLN//TRIM(MSG)
  !            !
  !            CALL STOP_ERROR(LN, IN, IOUT, ERRMSG )
  !        END IF
  !  END IF
  !  !
  !END SUBROUTINE
  !
  !#############################################################################################################################################################
  !
  SUBROUTINE GET_DOUBLE_VEC(LN,LOC,ISTART,ISTOP,IOUT,IN,VAL,MSG,ERROR_VAL, HAS_ERROR, IOSTAT)
    CHARACTER(*),                  INTENT(IN   ):: LN                ! LINE TO PARSE DOUBLE FROM
    INTEGER,                       INTENT(INOUT):: LOC,ISTART,ISTOP  ! LOC => STARTING LOCATION TO FIND NUMBER, NUMBER AT EXIT IS LOCATED AT LN(ISTART:ISTOP) AND LOC = ISTOP+1
    INTEGER,                       INTENT(IN   ):: IOUT, IN          ! ERROR UNIT TO WRITE TO, FILE UNIT THAT LN ORIGINATED FROM
    REAL(REAL64),    DIMENSION(:), INTENT(OUT  ):: VAL               ! DOUBLE VALUE TO RETURN
    CHARACTER(*),        OPTIONAL, INTENT(IN   ):: MSG               ! ERROR MESSAGE PRINTED WHEN FAIL TO LOAD NUMBER AND STOP PROGRAM, IF "NOSTOP" THEN VAL IS SET TO NaN
    REAL(REAL64),        OPTIONAL, INTENT(IN   ):: ERROR_VAL         ! IF FAIL TO LOAD NUMBER, DO NOT STOP, BUT INSTEAD SET VAL =  ERROR_VAL
    LOGICAL,             OPTIONAL, INTENT(  OUT):: HAS_ERROR         ! SET TO TRUE IF THERE IS AN ERROR
    INTEGER,             OPTIONAL, INTENT(  OUT):: IOSTAT            ! SET to zero if there is no error, otherwise non-zero error code
    CONTIGUOUS:: VAL
    LOGICAL:: MUST_STOP
    INTEGER:: IERR, I
    CHARACTER(:), ALLOCATABLE:: ERRMSG
    !
    IF(PRESENT(HAS_ERROR)) HAS_ERROR = FALSE
    !
    MUST_STOP = FALSE
    VAL  = DZ
    IERR = Z
    !
    DO I=ONE,SIZE(VAL)
              CALL PARSE_WORD(LN,LOC,ISTART,ISTOP)
              !
              SELECT CASE(LN(ISTART:ISTOP))
              CASE('NAN',  'NaN', 'nan'); VAL(I) = IEEE_VALUE(VAL(I), IEEE_QUIET_NAN)
              CASE('INF',  'inf', 'Inf'); VAL(I) = inf
              CASE('-INF','-inf','-Inf'); VAL(I) = ninf
              CASE(  '-1', '-1.','-1.0'); VAL(I) = DNEG
              CASE(   '0',  '0.', '0.0'); VAL(I) = DZ
              CASE(   '1',  '1.', '1.0'); VAL(I) = UNO
              CASE DEFAULT
                          READ(LN(ISTART:ISTOP),*, IOSTAT=IERR) VAL(I)
              END SELECT
              !
              IF(IERR /= Z .OR. LN(ISTART:ISTOP)==BLNK) THEN
                    MUST_STOP = TRUE
                    IF(PRESENT(ERROR_VAL)) THEN
                             !
                             MUST_STOP = FALSE
                             VAL(I) = ERROR_VAL
                             !
                    ELSEIF(PRESENT(MSG)) THEN
                         !
                         IF (MSG=='NOSTOP') THEN
                             MUST_STOP = FALSE
                             VAL(I) = IEEE_VALUE(VAL(I), IEEE_QUIET_NAN)   !SET VAL(I) TO NaN WHEN THERE IS NOSTOP OPTION
                         END IF
                         IF(PRESENT(HAS_ERROR)) HAS_ERROR = TRUE
                         !
                    ELSEIF(PRESENT(IOSTAT)) THEN
                         !
                         IF(IERR == Z .AND. LN(ISTART:ISTOP)==BLNK) THEN
                                                                    IOSTAT = -1
                         ELSE
                                                                    IOSTAT = IERR
                         END IF
                         MUST_STOP = FALSE
                         VAL(I) = IEEE_VALUE(VAL(I), IEEE_QUIET_NAN)   !SET VAL(I) TO NaN WHEN THERE IS NOSTOP OPTION
                         IF(PRESENT(HAS_ERROR)) HAS_ERROR = TRUE
                    ELSE IF(PRESENT(HAS_ERROR)) THEN
                         !
                         MUST_STOP = FALSE
                         HAS_ERROR = TRUE
                         VAL(I)= IEEE_VALUE(VAL(I), IEEE_QUIET_NAN)  !SET VAL TO NaN WHEN THERE IS NOSTOP OPTION
                    END IF
                    !
                    IF(MUST_STOP) THEN
                        ERRMSG = 'GET_NUMBER READ UTILITY ERROR: FAILED TO CONVERT TEXT TO DOUBLE PRECISION NUMBER.'//BLN//'THE FOLLOWING IS THE TEXT ATTEMPTED TO BE CONVERTED "'//LN(ISTART:ISTOP)//'".'//NL//'(IF THIS IS BLANK/EMPTY THEN YOU MAY NOT HAVE ENOUGH NUMBERS SPECIFIED ON THE LINE)'
                        IF(LN(ISTART:ISTOP)==BLNK) ERRMSG = ERRMSG//BLN//'THE POSSIBLE REASON FOR THIS ERROR IS DUE TO READING IN A BLANK/EMPTY LINE OR YOU DID NOT PROVIDE ENOUGH NUMBERS ON THE LINE.'
                        IF(PRESENT(MSG)) ERRMSG = ERRMSG//BLN//'THE FOLLOWING IS AN ADDITIONAL COMMENT PASSED TO GET_NUMBER:'//BLN//TRIM(MSG)
                        !
                        CALL STOP_ERROR(LN, IN, IOUT, ERRMSG )
                    END IF
              END IF
    END DO
    !
  END SUBROUTINE
  !
  !#############################################################################################################################################################
  !
  SUBROUTINE GET_DOUBLE_VAL(LN,LOC,ISTART,ISTOP,IOUT,IN,VAL,NO_PARSE_WORD,MSG,ERROR_VAL, HAS_ERROR, IOSTAT)  !IF NO_PARSE_WORD=TRUE, THEN ASSUMES ISTART AND ISTOP ARE ALREADY SET TO CORRECT LOCATION
    CHARACTER(*),           INTENT(IN   ):: LN                ! LINE TO PARSE DOUBLE FROM
    INTEGER,                INTENT(INOUT):: LOC,ISTART,ISTOP  ! LOC => STARTING LOCATION TO FIND NUMBER, NUMBER AT EXIT IS LOCATED AT LN(ISTART:ISTOP) AND LOC = ISTOP+1
    INTEGER,                INTENT(IN   ):: IOUT, IN          ! ERROR UNIT TO WRITE TO, FILE UNIT THAT LN ORIGINATED FROM
    REAL(REAL64),           INTENT(OUT  ):: VAL               ! DOUBLE VALUE TO RETURN
    LOGICAL,      OPTIONAL, INTENT(IN   ):: NO_PARSE_WORD     ! INDCIATES THAT ISTART AND ISTOP ALREADY POINT TO NUMBER
    CHARACTER(*), OPTIONAL, INTENT(IN   ):: MSG               ! ERROR MESSAGE PRINTED WHEN FAIL TO LOAD NUMBER AND STOP PROGRAM, IF "NOSTOP" THEN VAL IS SET TO NaN
    REAL(REAL64), OPTIONAL, INTENT(IN   ):: ERROR_VAL         ! IF FAIL TO LOAD NUMBER, DO NOT STOP, BUT INSTEAD SET VAL =  ERROR_VAL
    LOGICAL,      OPTIONAL, INTENT(  OUT):: HAS_ERROR         ! SET TO TRUE IF THERE IS AN ERROR
    INTEGER,      OPTIONAL, INTENT(  OUT):: IOSTAT            ! SET to zero if there is no error, otherwise non-zero error code
    LOGICAL:: CHECK
    INTEGER:: IERR
    CHARACTER(:), ALLOCATABLE:: ERRMSG
    !
    IF(PRESENT(HAS_ERROR)) HAS_ERROR = FALSE
    !
    IF(PRESENT(NO_PARSE_WORD)) THEN
        CHECK = .NOT. NO_PARSE_WORD
    ELSE
        CHECK = TRUE
    END IF
    !
    IF(CHECK) CALL PARSE_WORD(LN,LOC,ISTART,ISTOP)
    !
    IERR = Z
    SELECT CASE(LN(ISTART:ISTOP))
    CASE('NAN',  'NaN', 'nan'); VAL = IEEE_VALUE(VAL, IEEE_QUIET_NAN)
    CASE('INF',  'inf', 'Inf'); VAL = inf
    CASE('-INF','-inf','-Inf'); VAL = ninf
    CASE(  '-1', '-1.','-1.0'); VAL = DNEG
    CASE(   '0',  '0.', '0.0'); VAL = DZ
    CASE(   '1',  '1.', '1.0'); VAL = UNO
    CASE DEFAULT
                READ(LN(ISTART:ISTOP),*, IOSTAT=IERR) VAL
    END SELECT
    !
    IF(IERR /= Z .OR. LN(ISTART:ISTOP)==BLNK) THEN
          CHECK = TRUE
          IF(PRESENT(ERROR_VAL)) THEN
                   !
                   CHECK = FALSE
                   VAL = ERROR_VAL
                   !
          ELSEIF(PRESENT(MSG)) THEN
               !
               IF (MSG=='NOSTOP') THEN
                   CHECK = FALSE
                   VAL = IEEE_VALUE(VAL, IEEE_QUIET_NAN)  !SET VAL TO NaN WHEN THERE IS NOSTOP OPTION
               END IF
               IF(PRESENT(HAS_ERROR)) HAS_ERROR = TRUE
               !
          ELSEIF(PRESENT(IOSTAT)) THEN
               !
               IF(IERR == Z .AND. LN(ISTART:ISTOP)==BLNK) THEN
                                                          IOSTAT = -1
               ELSE
                                                          IOSTAT = IERR
               END IF
               CHECK  = FALSE
               VAL = IEEE_VALUE(VAL, IEEE_QUIET_NAN)   !SET VAL(I) TO NaN WHEN THERE IS NOSTOP OPTION
               IF(PRESENT(HAS_ERROR)) HAS_ERROR = TRUE
          ELSE IF(PRESENT(HAS_ERROR)) THEN
               !
               HAS_ERROR = TRUE
               CHECK  = FALSE
               VAL = IEEE_VALUE(VAL, IEEE_QUIET_NAN)  !SET VAL TO NaN WHEN THERE IS NOSTOP OPTION
          END IF
          !
          IF(CHECK) THEN
              ERRMSG = 'GET_NUMBER READ UTILITY ERROR: FAILED TO CONVERT TEXT TO DOUBLE PRECISION NUMBER.'//BLN//'THE FOLLOWING IS THE TEXT ATTEMPTED TO BE CONVERTED "'//LN(ISTART:ISTOP)//'".'
              IF(LN(ISTART:ISTOP)==BLNK) ERRMSG = ERRMSG//BLN//'THE POSSIBLE REASON FOR THIS ERROR IS DUE TO READING IN A BLANK/EMPTY LINE OR YOU DID NOT PROVIDE ENOUGH NUMBERS ON THE LINE.'
              IF(PRESENT(MSG)) ERRMSG = ERRMSG//BLN//'THE FOLLOWING IS AN ADDITIONAL COMMENT PASSED TO GET_NUMBER:'//BLN//TRIM(MSG)
              !
              CALL STOP_ERROR(LN, IN, IOUT, ERRMSG )
          END IF
    END IF
    !
  END SUBROUTINE
  !
  !#############################################################################################################################################################
  !
  SUBROUTINE GET_INTEGER_VEC(LN,LOC,ISTART,ISTOP,IOUT,IN,VAL,MSG,ERROR_VAL, HAS_ERROR, IOSTAT)
    CHARACTER(*),                  INTENT(IN   ):: LN                ! LINE TO PARSE DOUBLE FROM
    INTEGER,                       INTENT(INOUT):: LOC,ISTART,ISTOP  ! LOC => STARTING LOCATION TO FIND NUMBER, NUMBER AT EXIT IS LOCATED AT LN(ISTART:ISTOP) AND LOC = ISTOP+1
    INTEGER,                       INTENT(IN   ):: IOUT, IN          ! ERROR UNIT TO WRITE TO, FILE UNIT THAT LN ORIGINATED FROM
    INTEGER,         DIMENSION(:), INTENT(OUT  ):: VAL               ! DOUBLE VALUE TO RETURN
    CHARACTER(*),        OPTIONAL, INTENT(IN   ):: MSG               ! ERROR MESSAGE PRINTED WHEN FAIL TO LOAD NUMBER AND STOP PROGRAM, IF "NOSTOP" THEN VAL IS SET TO NaN
    INTEGER,             OPTIONAL, INTENT(IN   ):: ERROR_VAL         ! IF FAIL TO LOAD NUMBER, DO NOT STOP, BUT INSTEAD SET VAL =  ERROR_VAL
    LOGICAL,             OPTIONAL, INTENT(  OUT):: HAS_ERROR         ! SET TO TRUE IF THERE IS AN ERROR
    INTEGER,             OPTIONAL, INTENT(  OUT):: IOSTAT            ! SET to zero if there is no error, otherwise non-zero error code
    CONTIGUOUS:: VAL
    LOGICAL:: MUST_STOP
    INTEGER:: IERR, I
    CHARACTER(:), ALLOCATABLE:: ERRMSG
    !
    IF(PRESENT(HAS_ERROR)) HAS_ERROR = FALSE
    !
    MUST_STOP = FALSE
    VAL = Z
    !
    DO I=ONE,SIZE(VAL)
              CALL PARSE_WORD(LN,LOC,ISTART,ISTOP)
              !
              READ(LN(ISTART:ISTOP),*, IOSTAT=IERR) VAL(I)
              !
              IF(IERR /= Z .OR. LN(ISTART:ISTOP)==BLNK) THEN
                    MUST_STOP = TRUE
                    IF(PRESENT(ERROR_VAL)) THEN
                             !
                             MUST_STOP = FALSE
                             VAL(I) = ERROR_VAL
                             !
                    ELSEIF(PRESENT(MSG)) THEN
                         !
                         IF (MSG=='NOSTOP') THEN
                             MUST_STOP = FALSE
                             VAL(I) = inf_I  !SET VAL TO HUGE(I) WHEN THERE IS NOSTOP OPTION. NOTE NAN DOES NOT EXIST FOR INT
                         END IF
                         IF(PRESENT(HAS_ERROR)) HAS_ERROR = TRUE
                         !
                    ELSEIF(PRESENT(IOSTAT)) THEN
                         !
                         IF(IERR == Z .AND. LN(ISTART:ISTOP)==BLNK) THEN
                                                                    IOSTAT = -1
                         ELSE
                                                                    IOSTAT = IERR
                         END IF
                         MUST_STOP = FALSE
                         VAL(I) = inf_I  !SET VAL TO HUGE(I) WHEN THERE IS NOSTOP OPTION. NOTE NAN DOES NOT EXIST FOR INT
                         IF(PRESENT(HAS_ERROR)) HAS_ERROR = TRUE
                    ELSE IF(PRESENT(HAS_ERROR)) THEN
                         !
                         HAS_ERROR = TRUE
                         MUST_STOP = FALSE
                         VAL(I) = inf_I  !SET VAL TO HUGE(I) WHEN THERE IS NOSTOP OPTION. NOTE NAN DOES NOT EXIST FOR INT
                    END IF
                    !
                    IF(MUST_STOP) THEN
                        IERR = SIZE(VAL)
                        ERRMSG = 'GET_INTEGER READ UTILITY ERROR: EXPECTED TO READ '//NUM2STR(IERR)//' INTEGERS, BUT FAILED TO CONVERT THE POSITION '//NUM2STR(I)//' TEXT TO INTEGER.'//BLN//'THE FOLLOWING IS THE TEXT ATTEMPTED TO BE CONVERTED "'//LN(ISTART:ISTOP)//'".'//NL//'(IF THIS IS BLANK/EMPTY THEN YOU MAY NOT HAVE ENOUGH NUMBERS SPECIFIED ON THE LINE)'
                        IF(LN(ISTART:ISTOP)==BLNK) ERRMSG = ERRMSG//BLN//'THE POSSIBLE REASON FOR THIS ERROR IS DUE TO READING IN A BLANK/EMPTY LINE OR YOU DID NOT PROVIDE ENOUGH NUMBERS ON THE LINE.'
                        IF(PRESENT(MSG)) ERRMSG = ERRMSG//BLN//'THE FOLLOWING IS AN ADDITIONAL COMMENT PASSED TO GET_INTEGER:'//BLN//TRIM(MSG)
                        !
                        CALL STOP_ERROR(LN, IN, IOUT, ERRMSG )
                    END IF
              END IF
    END DO
    !
  END SUBROUTINE
  !
  !#############################################################################################################################################################
  !
  SUBROUTINE GET_INTEGER_VAL(LN,LOC,ISTART,ISTOP,IOUT,IN,VAL,NO_PARSE_WORD,MSG, ERROR_VAL, HAS_ERROR, IOSTAT)  !IF NO_PARSE_WORD=TRUE, THEN ASSUMES ISTART AND ISTOP ARE ALREADY SET TO CORRECT LOCATION
    CHARACTER(*),           INTENT(IN   ):: LN                ! LINE TO PARSE DOUBLE FROM
    INTEGER,                INTENT(INOUT):: LOC,ISTART,ISTOP  ! LOC => STARTING LOCATION TO FIND NUMBER, NUMBER AT EXIT IS LOCATED AT LN(ISTART:ISTOP) AND LOC = ISTOP+1
    INTEGER,                INTENT(IN   ):: IOUT, IN          ! ERROR UNIT TO WRITE TO, FILE UNIT THAT LN ORIGINATED FROM
    INTEGER,                INTENT(OUT  ):: VAL               ! DOUBLE VALUE TO RETURN
    LOGICAL,      OPTIONAL, INTENT(IN   ):: NO_PARSE_WORD     ! INDCIATES THAT ISTART AND ISTOP ALREADY POINT TO NUMBER
    CHARACTER(*), OPTIONAL, INTENT(IN   ):: MSG               ! ERROR MESSAGE PRINTED WHEN FAIL TO LOAD NUMBER AND STOP PROGRAM, IF "NOSTOP" THEN VAL IS SET TO inf_I = HUGE(VAL)
    INTEGER,      OPTIONAL, INTENT(IN   ):: ERROR_VAL         ! IF FAIL TO LOAD NUMBER, DO NOT STOP, BUT INSTEAD SET VAL =  ERROR_VAL
    LOGICAL,      OPTIONAL, INTENT(  OUT):: HAS_ERROR         ! SET TO TRUE IF THERE IS AN ERROR
    INTEGER,      OPTIONAL, INTENT(  OUT):: IOSTAT            ! SET to zero if there is no error, otherwise non-zero error code
    LOGICAL:: CHECK
    INTEGER:: IERR
    CHARACTER(:), ALLOCATABLE:: ERRMSG
    !
    IF(PRESENT(HAS_ERROR)) HAS_ERROR = FALSE
    !
    IF(PRESENT(NO_PARSE_WORD)) THEN
        CHECK = .NOT. NO_PARSE_WORD
    ELSE
        CHECK = TRUE
    END IF
    !
    IF(CHECK) CALL PARSE_WORD(LN,LOC,ISTART,ISTOP)
    !
    READ(LN(ISTART:ISTOP),*, IOSTAT=IERR) VAL
    !
    IF(IERR /= Z .OR. LN(ISTART:ISTOP)==BLNK) THEN
          CHECK = TRUE
          IF(PRESENT(ERROR_VAL)) THEN
                   !
                   CHECK = FALSE
                   VAL = ERROR_VAL
                   !
          ELSEIF(PRESENT(MSG)) THEN
               !
               IF (MSG=='NOSTOP') THEN
                   CHECK = FALSE
                   VAL = inf_I  !SET VAL TO HUGE(I) WHEN THERE IS NOSTOP OPTION. NOTE NAN DOES NOT EXIST FOR INT
               END IF
               IF(PRESENT(HAS_ERROR)) HAS_ERROR = TRUE
               !
          ELSEIF(PRESENT(IOSTAT)) THEN
               !
               IF(IERR == Z .AND. LN(ISTART:ISTOP)==BLNK) THEN
                                                          IOSTAT = -1
               ELSE
                                                          IOSTAT = IERR
               END IF
               CHECK  = FALSE
               VAL = inf_I  !SET VAL TO HUGE(I) WHEN THERE IS NOSTOP OPTION. NOTE NAN DOES NOT EXIST FOR INT
               IF(PRESENT(HAS_ERROR)) HAS_ERROR = TRUE
          ELSE IF(PRESENT(HAS_ERROR)) THEN
               !
               HAS_ERROR = TRUE
               CHECK  = FALSE
               VAL = inf_I  !SET VAL TO HUGE(I) WHEN THERE IS NOSTOP OPTION. NOTE NAN DOES NOT EXIST FOR INT
          END IF
          !
          IF(CHECK) THEN
              ERRMSG = 'GET_INTEGER READ UTILITY ERROR: FAILED TO CONVERT TEXT TO INTEGER NUMBER.'//BLN//'THE FOLLOWING IS THE TEXT ATTEMPTED TO BE CONVERTED "'//LN(ISTART:ISTOP)//'".'
              IF(LN(ISTART:ISTOP)==BLNK) ERRMSG = ERRMSG//BLN//'THE POSSIBLE REASON FOR THIS ERROR IS DUE TO READING IN A BLANK/EMPTY LINE OR YOU DID NOT PROVIDE ENOUGH NUMBERS ON THE LINE.'
              IF(PRESENT(MSG)) ERRMSG = ERRMSG//BLN//'THE FOLLOWING IS AN ADDITIONAL COMMENT PASSED TO GET_INTEGER:'//BLN//TRIM(MSG)
              !
              CALL STOP_ERROR(LN, IN, IOUT, ERRMSG )
          END IF
    END IF
    !
  END SUBROUTINE
  !
  !#############################################################################################################################################################
  !
  SUBROUTINE GET_REAL_VEC(LN,LOC,ISTART,ISTOP,IOUT,IN,VAL,MSG,ERROR_VAL, HAS_ERROR, IOSTAT)
    CHARACTER(*),                  INTENT(IN   ):: LN                ! LINE TO PARSE DOUBLE FROM
    INTEGER,                       INTENT(INOUT):: LOC,ISTART,ISTOP  ! LOC => STARTING LOCATION TO FIND NUMBER, NUMBER AT EXIT IS LOCATED AT LN(ISTART:ISTOP) AND LOC = ISTOP+1
    INTEGER,                       INTENT(IN   ):: IOUT, IN          ! ERROR UNIT TO WRITE TO, FILE UNIT THAT LN ORIGINATED FROM
    REAL(REAL32),    DIMENSION(:), INTENT(OUT  ):: VAL               ! DOUBLE VALUE TO RETURN
    CHARACTER(*),        OPTIONAL, INTENT(IN   ):: MSG               ! ERROR MESSAGE PRINTED WHEN FAIL TO LOAD NUMBER AND STOP PROGRAM, IF "NOSTOP" THEN VAL IS SET TO NaN
    REAL(REAL32),        OPTIONAL, INTENT(IN   ):: ERROR_VAL         ! IF FAIL TO LOAD NUMBER, DO NOT STOP, BUT INSTEAD SET VAL =  ERROR_VAL
    LOGICAL,             OPTIONAL, INTENT(  OUT):: HAS_ERROR         ! SET TO TRUE IF THERE IS AN ERROR
    INTEGER,             OPTIONAL, INTENT(  OUT):: IOSTAT            ! SET to zero if there is no error, otherwise non-zero error code
    CONTIGUOUS:: VAL
    LOGICAL:: MUST_STOP
    INTEGER:: IERR, I
    CHARACTER(:), ALLOCATABLE:: ERRMSG
    !
    IF(PRESENT(HAS_ERROR)) HAS_ERROR = FALSE
    !
    MUST_STOP = FALSE
    VAL  = DZ
    IERR = Z
    !
    DO I=ONE,SIZE(VAL)
              CALL PARSE_WORD(LN,LOC,ISTART,ISTOP)
              !
              SELECT CASE(LN(ISTART:ISTOP))
              CASE('NAN',  'NaN', 'nan'); VAL(I) = IEEE_VALUE(VAL(I), IEEE_QUIET_NAN)
              CASE('INF',  'inf', 'Inf'); VAL(I) =  inf_R
              CASE('-INF','-inf','-Inf'); VAL(I) = ninf_R
              CASE(  '-1', '-1.','-1.0'); VAL(I) = -1.0
              CASE(   '0',  '0.', '0.0'); VAL(I) =  0.0
              CASE(   '1',  '1.', '1.0'); VAL(I) =  1.0
              CASE DEFAULT
                          READ(LN(ISTART:ISTOP),*, IOSTAT=IERR) VAL(I)
              END SELECT
              !
              IF(IERR /= Z .OR. LN(ISTART:ISTOP)==BLNK) THEN
                    MUST_STOP = TRUE
                    IF(PRESENT(ERROR_VAL)) THEN
                             !
                             MUST_STOP = FALSE
                             VAL(I) = ERROR_VAL
                             !
                    ELSEIF(PRESENT(MSG)) THEN
                         !
                         IF (MSG=='NOSTOP') THEN
                             MUST_STOP = FALSE
                             VAL(I) = IEEE_VALUE(VAL(I), IEEE_QUIET_NAN)   !SET VAL(I) TO NaN WHEN THERE IS NOSTOP OPTION
                         END IF
                         IF(PRESENT(HAS_ERROR)) HAS_ERROR = TRUE
                         !
                    ELSEIF(PRESENT(IOSTAT)) THEN
                         !
                         IF(IERR == Z .AND. LN(ISTART:ISTOP)==BLNK) THEN
                                                                    IOSTAT = -1
                         ELSE
                                                                    IOSTAT = IERR
                         END IF
                         MUST_STOP = FALSE
                         VAL(I) = IEEE_VALUE(VAL(I), IEEE_QUIET_NAN)   !SET VAL(I) TO NaN WHEN THERE IS NOSTOP OPTION
                         IF(PRESENT(HAS_ERROR)) HAS_ERROR = TRUE
                    ELSE IF(PRESENT(HAS_ERROR)) THEN
                         !
                         MUST_STOP = FALSE
                         HAS_ERROR = TRUE
                         VAL(I)= IEEE_VALUE(VAL(I), IEEE_QUIET_NAN)  !SET VAL TO NaN WHEN THERE IS NOSTOP OPTION
                    END IF
                    !
                    IF(MUST_STOP) THEN
                        ERRMSG = 'GET_NUMBER READ UTILITY ERROR: FAILED TO CONVERT TEXT TO SINGLE PRECISION NUMBER.'//BLN//'THE FOLLOWING IS THE TEXT ATTEMPTED TO BE CONVERTED "'//LN(ISTART:ISTOP)//'".'//NL//'(IF THIS IS BLANK/EMPTY THEN YOU MAY NOT HAVE ENOUGH NUMBERS SPECIFIED ON THE LINE)'
                        IF(LN(ISTART:ISTOP)==BLNK) ERRMSG = ERRMSG//BLN//'THE POSSIBLE REASON FOR THIS ERROR IS DUE TO READING IN A BLANK/EMPTY LINE OR YOU DID NOT PROVIDE ENOUGH NUMBERS ON THE LINE.'
                        IF(PRESENT(MSG)) ERRMSG = ERRMSG//BLN//'THE FOLLOWING IS AN ADDITIONAL COMMENT PASSED TO GET_NUMBER:'//BLN//TRIM(MSG)
                        !
                        CALL STOP_ERROR(LN, IN, IOUT, ERRMSG )
                    END IF
              END IF
    END DO
    !
  END SUBROUTINE
  !
  !#############################################################################################################################################################
  !
  SUBROUTINE GET_REAL_VAL(LN,LOC,ISTART,ISTOP,IOUT,IN,VAL,NO_PARSE_WORD,MSG,ERROR_VAL, HAS_ERROR, IOSTAT)  !IF NO_PARSE_WORD=TRUE, THEN ASSUMES ISTART AND ISTOP ARE ALREADY SET TO CORRECT LOCATION
    CHARACTER(*),           INTENT(IN   ):: LN                ! LINE TO PARSE DOUBLE FROM
    INTEGER,                INTENT(INOUT):: LOC,ISTART,ISTOP  ! LOC => STARTING LOCATION TO FIND NUMBER, NUMBER AT EXIT IS LOCATED AT LN(ISTART:ISTOP) AND LOC = ISTOP+1
    INTEGER,                INTENT(IN   ):: IOUT, IN          ! ERROR UNIT TO WRITE TO, FILE UNIT THAT LN ORIGINATED FROM
    REAL(REAL32),           INTENT(OUT  ):: VAL               ! DOUBLE VALUE TO RETURN
    LOGICAL,      OPTIONAL, INTENT(IN   ):: NO_PARSE_WORD     ! INDCIATES THAT ISTART AND ISTOP ALREADY POINT TO NUMBER
    CHARACTER(*), OPTIONAL, INTENT(IN   ):: MSG               ! ERROR MESSAGE PRINTED WHEN FAIL TO LOAD NUMBER AND STOP PROGRAM, IF "NOSTOP" THEN VAL IS SET TO NaN
    REAL(REAL32), OPTIONAL, INTENT(IN   ):: ERROR_VAL         ! IF FAIL TO LOAD NUMBER, DO NOT STOP, BUT INSTEAD SET VAL =  ERROR_VAL
    LOGICAL,      OPTIONAL, INTENT(  OUT):: HAS_ERROR         ! SET TO TRUE IF THERE IS AN ERROR
    INTEGER,      OPTIONAL, INTENT(  OUT):: IOSTAT            ! SET to zero if there is no error, otherwise non-zero error code
    LOGICAL:: CHECK
    INTEGER:: IERR
    CHARACTER(:), ALLOCATABLE:: ERRMSG
    !
    IF(PRESENT(HAS_ERROR)) HAS_ERROR = FALSE
    !
    IF(PRESENT(NO_PARSE_WORD)) THEN
        CHECK = .NOT. NO_PARSE_WORD
    ELSE
        CHECK = TRUE
    END IF
    !
    IF(CHECK) CALL PARSE_WORD(LN,LOC,ISTART,ISTOP)
    !
    IERR = Z
    SELECT CASE(LN(ISTART:ISTOP))
    CASE('NAN',  'NaN', 'nan'); VAL = IEEE_VALUE(VAL, IEEE_QUIET_NAN)
    CASE('INF',  'inf', 'Inf'); VAL =  inf_R
    CASE('-INF','-inf','-Inf'); VAL = ninf_R
    CASE(  '-1', '-1.','-1.0'); VAL = -1.0
    CASE(   '0',  '0.', '0.0'); VAL =  0.0
    CASE(   '1',  '1.', '1.0'); VAL =  1.0
    CASE DEFAULT
                READ(LN(ISTART:ISTOP),*, IOSTAT=IERR) VAL
    END SELECT
    !
    IF(IERR /= Z .OR. LN(ISTART:ISTOP)==BLNK) THEN
          CHECK = TRUE
          IF(PRESENT(ERROR_VAL)) THEN
                   !
                   CHECK = FALSE
                   VAL = ERROR_VAL
                   !
          ELSEIF(PRESENT(MSG)) THEN
               !
               IF (MSG=='NOSTOP') THEN
                   CHECK = FALSE
                   VAL = IEEE_VALUE(VAL, IEEE_QUIET_NAN)  !SET VAL TO NaN WHEN THERE IS NOSTOP OPTION
               END IF
               IF(PRESENT(HAS_ERROR)) HAS_ERROR = TRUE
               !
          ELSEIF(PRESENT(IOSTAT)) THEN
               !
               IF(IERR == Z .AND. LN(ISTART:ISTOP)==BLNK) THEN
                                                          IOSTAT = -1
               ELSE
                                                          IOSTAT = IERR
               END IF
               CHECK  = FALSE
               VAL = IEEE_VALUE(VAL, IEEE_QUIET_NAN)   !SET VAL(I) TO NaN WHEN THERE IS NOSTOP OPTION
               IF(PRESENT(HAS_ERROR)) HAS_ERROR = TRUE
          ELSE IF(PRESENT(HAS_ERROR)) THEN
               !
               HAS_ERROR = TRUE
               CHECK  = FALSE
               VAL = IEEE_VALUE(VAL, IEEE_QUIET_NAN)  !SET VAL TO NaN WHEN THERE IS NOSTOP OPTION
          END IF
          !
          IF(CHECK) THEN
              ERRMSG = 'GET_NUMBER READ UTILITY ERROR: FAILED TO CONVERT TEXT TO SINGLE PRECISION NUMBER.'//BLN//'THE FOLLOWING IS THE TEXT ATTEMPTED TO BE CONVERTED "'//LN(ISTART:ISTOP)//'".'
              IF(LN(ISTART:ISTOP)==BLNK) ERRMSG = ERRMSG//BLN//'THE POSSIBLE REASON FOR THIS ERROR IS DUE TO READING IN A BLANK/EMPTY LINE OR YOU DID NOT PROVIDE ENOUGH NUMBERS ON THE LINE.'
              IF(PRESENT(MSG)) ERRMSG = ERRMSG//BLN//'THE FOLLOWING IS AN ADDITIONAL COMMENT PASSED TO GET_NUMBER:'//BLN//TRIM(MSG)
              !
              CALL STOP_ERROR(LN, IN, IOUT, ERRMSG )
          END IF
    END IF
    !
  END SUBROUTINE
  !
  !#############################################################################################################################################################
  !
  SUBROUTINE GET_QUAD_VEC(LN,LOC,ISTART,ISTOP,IOUT,IN,VAL,MSG,ERROR_VAL, HAS_ERROR, IOSTAT)
    CHARACTER(*),                  INTENT(IN   ):: LN                ! LINE TO PARSE DOUBLE FROM
    INTEGER,                       INTENT(INOUT):: LOC,ISTART,ISTOP  ! LOC => STARTING LOCATION TO FIND NUMBER, NUMBER AT EXIT IS LOCATED AT LN(ISTART:ISTOP) AND LOC = ISTOP+1
    INTEGER,                       INTENT(IN   ):: IOUT, IN          ! ERROR UNIT TO WRITE TO, FILE UNIT THAT LN ORIGINATED FROM
    REAL(REAL128),   DIMENSION(:), INTENT(OUT  ):: VAL               ! DOUBLE VALUE TO RETURN
    CHARACTER(*),        OPTIONAL, INTENT(IN   ):: MSG               ! ERROR MESSAGE PRINTED WHEN FAIL TO LOAD NUMBER AND STOP PROGRAM, IF "NOSTOP" THEN VAL IS SET TO NaN
    REAL(REAL128),       OPTIONAL, INTENT(IN   ):: ERROR_VAL         ! IF FAIL TO LOAD NUMBER, DO NOT STOP, BUT INSTEAD SET VAL =  ERROR_VAL
    LOGICAL,             OPTIONAL, INTENT(  OUT):: HAS_ERROR         ! SET TO TRUE IF THERE IS AN ERROR
    INTEGER,             OPTIONAL, INTENT(  OUT):: IOSTAT            ! SET to zero if there is no error, otherwise non-zero error code
    CONTIGUOUS:: VAL
    LOGICAL:: MUST_STOP
    INTEGER:: IERR, I
    CHARACTER(:), ALLOCATABLE:: ERRMSG
    !
    IF(PRESENT(HAS_ERROR)) HAS_ERROR = FALSE
    !
    MUST_STOP = FALSE
    VAL  = DZ
    IERR = Z
    !
    DO I=ONE,SIZE(VAL)
              CALL PARSE_WORD(LN,LOC,ISTART,ISTOP)
              !
              SELECT CASE(LN(ISTART:ISTOP))
              CASE('NAN',  'NaN', 'nan'); VAL(I) = IEEE_VALUE(VAL(I), IEEE_QUIET_NAN)
              CASE('INF',  'inf', 'Inf'); VAL(I) = IEEE_VALUE(VAL(I), IEEE_POSITIVE_INF)
              CASE('-INF','-inf','-Inf'); VAL(I) = IEEE_VALUE(VAL(I), IEEE_NEGATIVE_INF)
              CASE(  '-1', '-1.','-1.0'); VAL(I) = -1.0
              CASE(   '0',  '0.', '0.0'); VAL(I) =  0.0
              CASE(   '1',  '1.', '1.0'); VAL(I) =  1.0
              CASE DEFAULT
                          READ(LN(ISTART:ISTOP),*, IOSTAT=IERR) VAL(I)
              END SELECT
              !
              IF(IERR /= Z .OR. LN(ISTART:ISTOP)==BLNK) THEN
                    MUST_STOP = TRUE
                    IF(PRESENT(ERROR_VAL)) THEN
                             !
                             MUST_STOP = FALSE
                             VAL(I) = ERROR_VAL
                             !
                    ELSEIF(PRESENT(MSG)) THEN
                         !
                         IF (MSG=='NOSTOP') THEN
                             MUST_STOP = FALSE
                             VAL(I) = IEEE_VALUE(VAL(I), IEEE_QUIET_NAN)   !SET VAL(I) TO NaN WHEN THERE IS NOSTOP OPTION
                         END IF
                         IF(PRESENT(HAS_ERROR)) HAS_ERROR = TRUE
                         !
                    ELSEIF(PRESENT(IOSTAT)) THEN
                         !
                         IF(IERR == Z .AND. LN(ISTART:ISTOP)==BLNK) THEN
                                                                    IOSTAT = -1
                         ELSE
                                                                    IOSTAT = IERR
                         END IF
                         MUST_STOP = FALSE
                         VAL(I) = IEEE_VALUE(VAL(I), IEEE_QUIET_NAN)   !SET VAL(I) TO NaN WHEN THERE IS NOSTOP OPTION
                         IF(PRESENT(HAS_ERROR)) HAS_ERROR = TRUE
                    ELSE IF(PRESENT(HAS_ERROR)) THEN
                         !
                         MUST_STOP = FALSE
                         HAS_ERROR = TRUE
                         VAL(I)= IEEE_VALUE(VAL(I), IEEE_QUIET_NAN)  !SET VAL TO NaN WHEN THERE IS NOSTOP OPTION
                    END IF
                    !
                    IF(MUST_STOP) THEN
                        ERRMSG = 'GET_NUMBER READ UTILITY ERROR: FAILED TO CONVERT TEXT TO SINGLE PRECISION NUMBER.'//BLN//'THE FOLLOWING IS THE TEXT ATTEMPTED TO BE CONVERTED "'//LN(ISTART:ISTOP)//'".'//NL//'(IF THIS IS BLANK/EMPTY THEN YOU MAY NOT HAVE ENOUGH NUMBERS SPECIFIED ON THE LINE)'
                        IF(LN(ISTART:ISTOP)==BLNK) ERRMSG = ERRMSG//BLN//'THE POSSIBLE REASON FOR THIS ERROR IS DUE TO READING IN A BLANK/EMPTY LINE OR YOU DID NOT PROVIDE ENOUGH NUMBERS ON THE LINE.'
                        IF(PRESENT(MSG)) ERRMSG = ERRMSG//BLN//'THE FOLLOWING IS AN ADDITIONAL COMMENT PASSED TO GET_NUMBER:'//BLN//TRIM(MSG)
                        !
                        CALL STOP_ERROR(LN, IN, IOUT, ERRMSG )
                    END IF
              END IF
    END DO
    !
  END SUBROUTINE
  !
  !#############################################################################################################################################################
  !
  SUBROUTINE GET_QUAD_VAL(LN,LOC,ISTART,ISTOP,IOUT,IN,VAL,NO_PARSE_WORD,MSG,ERROR_VAL, HAS_ERROR, IOSTAT)  !IF NO_PARSE_WORD=TRUE, THEN ASSUMES ISTART AND ISTOP ARE ALREADY SET TO CORRECT LOCATION
    CHARACTER(*),           INTENT(IN   ):: LN                ! LINE TO PARSE DOUBLE FROM
    INTEGER,                INTENT(INOUT):: LOC,ISTART,ISTOP  ! LOC => STARTING LOCATION TO FIND NUMBER, NUMBER AT EXIT IS LOCATED AT LN(ISTART:ISTOP) AND LOC = ISTOP+1
    INTEGER,                INTENT(IN   ):: IOUT, IN          ! ERROR UNIT TO WRITE TO, FILE UNIT THAT LN ORIGINATED FROM
    REAL(REAL128),          INTENT(OUT  ):: VAL               ! DOUBLE VALUE TO RETURN
    LOGICAL,      OPTIONAL, INTENT(IN   ):: NO_PARSE_WORD     ! INDCIATES THAT ISTART AND ISTOP ALREADY POINT TO NUMBER
    CHARACTER(*), OPTIONAL, INTENT(IN   ):: MSG               ! ERROR MESSAGE PRINTED WHEN FAIL TO LOAD NUMBER AND STOP PROGRAM, IF "NOSTOP" THEN VAL IS SET TO NaN
    REAL(REAL128),OPTIONAL, INTENT(IN   ):: ERROR_VAL         ! IF FAIL TO LOAD NUMBER, DO NOT STOP, BUT INSTEAD SET VAL =  ERROR_VAL
    LOGICAL,      OPTIONAL, INTENT(  OUT):: HAS_ERROR         ! SET TO TRUE IF THERE IS AN ERROR
    INTEGER,      OPTIONAL, INTENT(  OUT):: IOSTAT            ! SET to zero if there is no error, otherwise non-zero error code
    LOGICAL:: CHECK
    INTEGER:: IERR
    CHARACTER(:), ALLOCATABLE:: ERRMSG
    !
    IF(PRESENT(HAS_ERROR)) HAS_ERROR = FALSE
    !
    IF(PRESENT(NO_PARSE_WORD)) THEN
        CHECK = .NOT. NO_PARSE_WORD
    ELSE
        CHECK = TRUE
    END IF
    !
    IF(CHECK) CALL PARSE_WORD(LN,LOC,ISTART,ISTOP)
    !
    IERR = Z
    SELECT CASE(LN(ISTART:ISTOP))
    CASE('NAN',  'NaN', 'nan'); VAL = IEEE_VALUE(VAL, IEEE_QUIET_NAN)
    CASE('INF',  'inf', 'Inf'); VAL = IEEE_VALUE(VAL, IEEE_POSITIVE_INF)
    CASE('-INF','-inf','-Inf'); VAL = IEEE_VALUE(VAL, IEEE_NEGATIVE_INF)
    CASE(  '-1', '-1.','-1.0'); VAL = -1.0
    CASE(   '0',  '0.', '0.0'); VAL =  0.0
    CASE(   '1',  '1.', '1.0'); VAL =  1.0
    CASE DEFAULT
                READ(LN(ISTART:ISTOP),*, IOSTAT=IERR) VAL
    END SELECT
    !
    IF(IERR /= Z .OR. LN(ISTART:ISTOP)==BLNK) THEN
          CHECK = TRUE
          IF(PRESENT(ERROR_VAL)) THEN
                   !
                   CHECK = FALSE
                   VAL = ERROR_VAL
                   !
          ELSEIF(PRESENT(MSG)) THEN
               !
               IF (MSG=='NOSTOP') THEN
                   CHECK = FALSE
                   VAL = IEEE_VALUE(VAL, IEEE_QUIET_NAN)  !SET VAL TO NaN WHEN THERE IS NOSTOP OPTION
               END IF
               IF(PRESENT(HAS_ERROR)) HAS_ERROR = TRUE
               !
          ELSEIF(PRESENT(IOSTAT)) THEN
               !
               IF(IERR == Z .AND. LN(ISTART:ISTOP)==BLNK) THEN
                                                          IOSTAT = -1
               ELSE
                                                          IOSTAT = IERR
               END IF
               CHECK  = FALSE
               VAL = IEEE_VALUE(VAL, IEEE_QUIET_NAN)   !SET VAL(I) TO NaN WHEN THERE IS NOSTOP OPTION
               IF(PRESENT(HAS_ERROR)) HAS_ERROR = TRUE
          ELSE IF(PRESENT(HAS_ERROR)) THEN
               !
               HAS_ERROR = TRUE
               CHECK  = FALSE
               VAL = IEEE_VALUE(VAL, IEEE_QUIET_NAN)  !SET VAL TO NaN WHEN THERE IS NOSTOP OPTION
          END IF
          !
          IF(CHECK) THEN
              ERRMSG = 'GET_NUMBER READ UTILITY ERROR: FAILED TO CONVERT TEXT TO SINGLE PRECISION NUMBER.'//BLN//'THE FOLLOWING IS THE TEXT ATTEMPTED TO BE CONVERTED "'//LN(ISTART:ISTOP)//'".'
              IF(LN(ISTART:ISTOP)==BLNK) ERRMSG = ERRMSG//BLN//'THE POSSIBLE REASON FOR THIS ERROR IS DUE TO READING IN A BLANK/EMPTY LINE OR YOU DID NOT PROVIDE ENOUGH NUMBERS ON THE LINE.'
              IF(PRESENT(MSG)) ERRMSG = ERRMSG//BLN//'THE FOLLOWING IS AN ADDITIONAL COMMENT PASSED TO GET_NUMBER:'//BLN//TRIM(MSG)
              !
              CALL STOP_ERROR(LN, IN, IOUT, ERRMSG )
          END IF
    END IF
    !
  END SUBROUTINE
  !
  !#############################################################################################################################################################
  !
  !SUBROUTINE GET_REAL_VAL(LN,LOC,ISTART,ISTOP,IOUT,IN,VAL,NO_PARSE_WORD,MSG,ERROR_VAL, HAS_ERROR)  !IF NO_PARSE_WORD=TRUE, THEN ASSUMES ISTART AND ISTOP ARE ALREADY SET TO CORRECT LOCATION
  !  CHARACTER(*),           INTENT(IN   ):: LN                ! LINE TO PARSE DOUBLE FROM
  !  INTEGER,                INTENT(INOUT):: LOC,ISTART,ISTOP  ! LOC => STARTING LOCATION TO FIND NUMBER, NUMBER AT EXIT IS LOCATED AT LN(ISTART:ISTOP) AND LOC = ISTOP+1
  !  INTEGER,                INTENT(IN   ):: IOUT, IN          ! ERROR UNIT TO WRITE TO, FILE UNIT THAT LN ORIGINATED FROM
  !  REAL(REAL32),           INTENT(OUT  ):: VAL               ! REAL VALUE TO RETURN
  !  LOGICAL,      OPTIONAL, INTENT(IN   ):: NO_PARSE_WORD     ! INDCIATES THAT ISTART AND ISTOP ALREADY POINT TO NUMBER
  !  CHARACTER(*), OPTIONAL, INTENT(IN   ):: MSG               ! ERROR MESSAGE PRINTED WHEN FAIL TO LOAD NUMBER AND STOP PROGRAM, IF "NOSTOP" THEN VAL IS SET TO NaN
  !  REAL(REAL32), OPTIONAL, INTENT(IN   ):: ERROR_VAL         ! IF FAIL TO LOAD NUMBER, DO NOT STOP, BUT INSTEAD SET VAL =  ERROR_VAL
  !  LOGICAL,      OPTIONAL, INTENT(  OUT):: HAS_ERROR         ! SET TO TRUE IF THERE IS AN ERROR
  !  LOGICAL:: CHECK
  !  INTEGER:: IERR
  !  CHARACTER(:), ALLOCATABLE:: ERRMSG
  !  !
  !  IF(PRESENT(HAS_ERROR)) HAS_ERROR = FALSE
  !  !
  !  IF(PRESENT(NO_PARSE_WORD)) THEN
  !      CHECK = .NOT. NO_PARSE_WORD
  !  ELSE
  !      CHECK = TRUE
  !  END IF
  !  !
  !  IF(CHECK) CALL PARSE_WORD(LN,LOC,ISTART,ISTOP)
  !  !
  !  READ(LN(ISTART:ISTOP),*, IOSTAT=IERR) VAL
  !  !
  !  IF(IERR /= Z .OR. LN(ISTART:ISTOP)==BLNK) THEN
  !        CHECK = TRUE
  !        IF(PRESENT(ERROR_VAL)) THEN
  !                 !
  !                 CHECK = FALSE
  !                 VAL = ERROR_VAL
  !                 !
  !        ELSEIF(PRESENT(MSG)) THEN
  !             !
  !             IF (MSG=='NOSTOP') THEN
  !                 CHECK = FALSE
  !                 VAL = IEEE_VALUE(VAL, IEEE_QUIET_NAN)  !SET VAL TO NaN WHEN THERE IS NOSTOP OPTION
  !             END IF
  !             IF(PRESENT(HAS_ERROR)) HAS_ERROR = TRUE
  !        ELSE IF(PRESENT(HAS_ERROR)) THEN
  !             !
  !             HAS_ERROR = TRUE
  !             CHECK  = FALSE
  !             VAL = IEEE_VALUE(VAL, IEEE_QUIET_NAN)  !SET VAL TO NaN WHEN THERE IS NOSTOP OPTION
  !        END IF
  !        !
  !        IF(CHECK) THEN
  !            ERRMSG = 'GET_NUMBER READ UTILITY ERROR: FAILED TO CONVERT TEXT TO SINGLE PRECISION NUMBER.'//BLN//'THE FOLLOWING IS THE TEXT ATTEMPTED TO BE CONVERTED "'//LN(ISTART:ISTOP)//'".'
  !            IF(LN(ISTART:ISTOP)==BLNK) ERRMSG = ERRMSG//BLN//'THE POSSIBLE REASON FOR THIS ERROR IS DUE TO READING IN A BLANK/EMPTY LINE OR YOU DID NOT PROVIDE ENOUGH NUMBERS ON THE LINE.'
  !            IF(PRESENT(MSG)) ERRMSG = ERRMSG//BLN//'THE FOLLOWING IS AN ADDITIONAL COMMENT PASSED TO GET_NUMBER:'//BLN//TRIM(MSG)
  !            !
  !            CALL STOP_ERROR(LN, IN, IOUT, ERRMSG )
  !        END IF
  !  END IF
  !  !
  !END SUBROUTINE
  !
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !
  !  String routines
  !
  !#########################################################################################################################
  !
  PURE FUNCTION JOIN_TXT(TXT,JOIN,ENDING) RESULT(LN)
    CHARACTER(*),DIMENSION(:),CONTIGUOUS,INTENT(IN):: TXT
    CHARACTER(*),                        INTENT(IN):: JOIN
    CHARACTER(*), OPTIONAL,              INTENT(IN):: ENDING
    CHARACTER(:),                       ALLOCATABLE:: LN
    INTEGER::I
    !
    LN=TRIM(ADJUSTL(TXT(ONE)))
    DO I=TWO, SIZE(TXT)
                      LN=LN//JOIN//TRIM(ADJUSTL(TXT(I)))
    END DO
    !
    IF(PRESENT(ENDING)) LN=LN//ENDING
    !
  END FUNCTION
  !
  PURE ELEMENTAL SUBROUTINE COMMA_STRIP(LINE)
    CHARACTER(*), INTENT(INOUT):: LINE
    INTEGER:: I
    !
    DO CONCURRENT (I=1:LEN_TRIM(LINE), LINE(I:I)==',')
        LINE(I:I)=BLNK
    END DO
    !
  END SUBROUTINE
  !
  !#########################################################################################################################
  !
  PURE ELEMENTAL SUBROUTINE TAB_STRIP(LINE)
    CHARACTER(*), INTENT(INOUT):: LINE
    INTEGER:: I
    !
    DO CONCURRENT (I=1:LEN_TRIM(LINE), LINE(I:I)==TAB)  !TAB = CHAR(9)
        LINE(I:I)=BLNK
    END DO
    !
  END SUBROUTINE
  !
  !#########################################################################################################################
  !
  PURE ELEMENTAL SUBROUTINE SPECIAL_BLANK_STRIP(LINE)
    CHARACTER(*), INTENT(INOUT):: LINE
    INTEGER:: I
    !
    DO CONCURRENT (I=1:LEN_TRIM(LINE), LINE(I:I)==TAB .OR. LINE(I:I)==CR .OR. LINE(I:I)==LF)  !TAB = CHAR(9)
        LINE(I:I)=BLNK
    END DO
    !
  END SUBROUTINE
  !
  !#########################################################################################################################
  !
  PURE FUNCTION IS_IN_STR(SUB,LINE,COM_STOP,UPCASE) RESULT(FOUND)
    CHARACTER(*),      INTENT(IN):: SUB
    CHARACTER(*),      INTENT(IN):: LINE
    LOGICAL, OPTIONAL, INTENT(IN):: COM_STOP, UPCASE
    INTEGER:: I
    LOGICAL:: FOUND
    !
    FOUND = FALSE
    IF(PRESENT(COM_STOP)) FOUND = COM_STOP
    !
    IF(FOUND) THEN
        I = COMMENT_INDEX(LINE)
    ELSE
        I = LEN_TRIM(LINE)
    END IF
    !
    IF(I == Z) THEN
                         FOUND = FALSE
    ELSEIF(.NOT. PRESENT(UPCASE)) THEN
                         FOUND = INDEX(LINE(:I),SUB) > Z
    ELSEIF(UPCASE) THEN!
                         FOUND = INDEX(GO_UP(LINE(:I)),GO_UP(SUB)) > Z
    ELSE
                         FOUND = INDEX(LINE(:I),SUB) > Z
    END IF
    !
  END FUNCTION
  !
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !
  !  Get Word Routines - Note these are cloned from PARSE_WORD_INTERFACE to coerse function inlining within this module
  !
  SUBROUTINE GET_WORD_ASSUM(WORD,LN,LOC,OLD_LOC,COM_STOP,NO_UPCASE)  ! SETS WORD TO PARSED WORD, SETS IT TO "" IF END OF LINE OR NO WORD PARSED
    CHARACTER(*),           INTENT(IN   ):: LN                ! LINE TO PARSE WORD FROM
    INTEGER,                INTENT(INOUT):: LOC               ! LOC => STARTING LOCATION TO FIND WORD
    CHARACTER(*),           INTENT(  OUT):: WORD              ! RETURN WORD
    LOGICAL,      OPTIONAL, INTENT(IN   ):: NO_UPCASE         !SET IF YOU WANT TO LEAVE WORD ALONE
    INTEGER,      OPTIONAL, INTENT(INOUT):: OLD_LOC           !RETURN ORIGINAL LOC LOCATION
    LOGICAL,      OPTIONAL, INTENT(IN   ):: COM_STOP
    INTEGER:: LOC0,ISTART,ISTOP
    !
    LOC0=LOC
    CALL PARSE_WORD(LN,LOC,ISTART,ISTOP,COM_STOP)
    !
    IF(ISTART.LE.ISTOP) THEN
        !
        WORD = LN(ISTART:ISTOP)
        !
        IF(PRESENT(NO_UPCASE)) THEN
            IF(.NOT. NO_UPCASE) CALL UPPER(WORD)
        ELSE
                                CALL UPPER(WORD)
        END IF
    ELSE
        WORD = BLNK
    END IF
    !
    IF(PRESENT(OLD_LOC)) OLD_LOC = LOC0  !HAS TO BE AT END BECAUSE LOC COULD BE ALSO PASSED IN AS OLD_LOC
    !
  END SUBROUTINE
  !
  !#############################################################################################################################################################
  !
  SUBROUTINE GET_WORD_ALLOC(WORD,LN,LOC,IS_ALLOC,OLD_LOC,COM_STOP,NO_UPCASE)  ! SETS WORD TO PARSED WORD, SETS IT TO "" IF END OF LINE OR NO WORD PARSED
    CHARACTER(*),             INTENT(IN   ):: LN                ! LINE TO PARSE WORD FROM
    INTEGER,                  INTENT(INOUT):: LOC               ! LOC => STARTING LOCATION TO FIND WORD
    CHARACTER(:),ALLOCATABLE, INTENT(INOUT):: WORD              ! RETURN WORD
    LOGICAL,                  INTENT(IN   ):: IS_ALLOC
    LOGICAL,      OPTIONAL,   INTENT(IN   ):: NO_UPCASE         !SET IF YOU WANT TO LEAVE WORD ALONE
    INTEGER,      OPTIONAL,   INTENT(INOUT):: OLD_LOC           !RETURN ORIGINAL LOC LOCATION
    LOGICAL,      OPTIONAL,   INTENT(IN   ):: COM_STOP
    INTEGER:: LOC0,ISTART,ISTOP
    !
    LOC0=LOC
    CALL PARSE_WORD(LN,LOC,ISTART,ISTOP,COM_STOP)
    !
    IF(ISTART.LE.ISTOP) THEN
        !
        IF(IS_ALLOC) THEN
           IF(ALLOCATED(WORD)) DEALLOCATE(WORD)
           ALLOCATE(WORD, SOURCE=LN(ISTART:ISTOP))
        ELSE
            WORD=LN(ISTART:ISTOP)
        END IF
        !
        IF(PRESENT(NO_UPCASE)) THEN
            IF(.NOT. NO_UPCASE) CALL UPPER(WORD)
        ELSE
                                CALL UPPER(WORD)
        END IF
    ELSE
        IF(IS_ALLOC) THEN
           IF(ALLOCATED(WORD)) DEALLOCATE(WORD)
           ALLOCATE(WORD, SOURCE=BLNK)
        ELSE
            WORD=BLNK
        END IF
    END IF
    !
    IF(PRESENT(OLD_LOC)) OLD_LOC = LOC0  !HAS TO BE AT END BECAUSE LOC COULD BE ALSO PASSED IN AS OLD_LOC
    !
  END SUBROUTINE
  !
  !#############################################################################################################################################################
  !
  SUBROUTINE GET_WORD_ASSUM_ISTART(LN,LOC,ISTART,ISTOP,WORD,OLD_LOC,COM_STOP,NO_UPCASE)  ! SETS WORD TO PARSED WORD, SETS IT TO "" IF END OF LINE OR NO WORD PARSED
    CHARACTER(*),           INTENT(IN   ):: LN                ! LINE TO PARSE WORD FROM
    INTEGER,                INTENT(INOUT):: LOC               ! LOC => STARTING LOCATION TO FIND WORD
    INTEGER,                INTENT(INOUT):: ISTART,ISTOP
    CHARACTER(*),           INTENT(  OUT):: WORD              ! RETURN WORD
    LOGICAL,      OPTIONAL, INTENT(IN   ):: NO_UPCASE         !SET IF YOU WANT TO LEAVE WORD ALONE
    INTEGER,      OPTIONAL, INTENT(INOUT):: OLD_LOC           !RETURN ORIGINAL LOC LOCATION
    LOGICAL,      OPTIONAL, INTENT(IN   ):: COM_STOP
    INTEGER:: LOC0
    !
    LOC0=LOC
    CALL PARSE_WORD(LN,LOC,ISTART,ISTOP,COM_STOP)
    !
    IF(ISTART.LE.ISTOP) THEN
        !
        WORD = LN(ISTART:ISTOP)
        !
        IF(PRESENT(NO_UPCASE)) THEN
            IF(.NOT. NO_UPCASE) CALL UPPER(WORD)
        ELSE
                                CALL UPPER(WORD)
        END IF
    ELSE
        WORD = BLNK
    END IF
    !
    IF(PRESENT(OLD_LOC)) OLD_LOC = LOC0  !HAS TO BE AT END BECAUSE LOC COULD BE ALSO PASSED IN AS OLD_LOC
    !
  END SUBROUTINE
  !
  !#############################################################################################################################################################
  !
  SUBROUTINE GET_WORD_ALLOC_ISTART(LN,LOC,ISTART,ISTOP,WORD,IS_ALLOC,OLD_LOC,COM_STOP,NO_UPCASE)  ! SETS WORD TO PARSED WORD, SETS IT TO "" IF END OF LINE OR NO WORD PARSED
    CHARACTER(*),             INTENT(IN   ):: LN                ! LINE TO PARSE WORD FROM
    INTEGER,                  INTENT(INOUT):: LOC               ! LOC => STARTING LOCATION TO FIND WORD
    INTEGER,                  INTENT(INOUT):: ISTART,ISTOP
    CHARACTER(:),ALLOCATABLE, INTENT(  OUT):: WORD              ! RETURN WORD
    LOGICAL,                  INTENT(IN   ):: IS_ALLOC
    LOGICAL,      OPTIONAL,   INTENT(IN   ):: NO_UPCASE         !SET IF YOU WANT TO LEAVE WORD ALONE
    INTEGER,      OPTIONAL,   INTENT(INOUT):: OLD_LOC           !RETURN ORIGINAL LOC LOCATION
    LOGICAL,      OPTIONAL,   INTENT(IN   ):: COM_STOP
    INTEGER:: LOC0
    !
    LOC0=LOC
    CALL PARSE_WORD(LN,LOC,ISTART,ISTOP,COM_STOP)
    !
    IF(ISTART.LE.ISTOP) THEN
        !
        IF(IS_ALLOC) THEN
           IF(ALLOCATED(WORD)) DEALLOCATE(WORD)
           ALLOCATE(WORD, SOURCE=LN(ISTART:ISTOP))
        ELSE
            WORD=LN(ISTART:ISTOP)
        END IF
        !
        IF(PRESENT(NO_UPCASE)) THEN
            IF(.NOT. NO_UPCASE) CALL UPPER(WORD)
        ELSE
                                CALL UPPER(WORD)
        END IF
    ELSE
        IF(IS_ALLOC) THEN
           IF(ALLOCATED(WORD)) DEALLOCATE(WORD)
           ALLOCATE(WORD, SOURCE=BLNK)
        ELSE
            WORD=BLNK
        END IF
    END IF
    !
    IF(PRESENT(OLD_LOC)) OLD_LOC = LOC0  !HAS TO BE AT END BECAUSE LOC COULD BE ALSO PASSED IN AS OLD_LOC
    !
  END SUBROUTINE
  !
END MODULE
!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
