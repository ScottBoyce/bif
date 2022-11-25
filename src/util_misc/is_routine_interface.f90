!
! CODE DEVELOPED BY SCOTT E BOYCE 
!                   CONTACT <seboyce@usgs.gov> or <Boyce@engineer.com>
!
MODULE IS_ROUTINES
  !  
  USE CONSTANTS,                 ONLY: DNEG, DZ, TENTH, NEG, Z, ONE, TWO, TRUE, FALSE, &
                                       NEARZERO_6, NEARZERO_10, NEARZERO_12, BLNK, NL, BLN, TAB, COM !TEN,UNO,DOS,DIEZ,TRUE,FALSE,ninf,inf,inf_I,inf_R,ninf_R,NEARZERO_29,NEARZERO_30,NEGNEARZERO_30,NEARZERO_5,HALF,TRES, QUIN, FOUR, SEV, EIGHT, HECTO, THOU, LF, CR, SNGL_inf, SNGL_ninf, SNGL_inf_R, SNGL_ninf_R, SNGL_inf_R, SNGL_ninf_R, SUB_ONE, NEAR_ONE, LOG_2, LOG_2_R, NO
  !
  USE, INTRINSIC:: IEEE_ARITHMETIC, ONLY: IEEE_IS_NAN
  USE, INTRINSIC:: ISO_FORTRAN_ENV, ONLY: i8 => INT8,   i16 => INT16,  &
                                         i32 => INT32,  i64 => INT64,  &
                                         SNG => REAL32, DBL => REAL64
  !
  IMPLICIT NONE
  !
  PRIVATE
  !
  PUBLIC:: IS_ODD, IS_EVEN                  !IS_ODD(NUM) - fast bitwise operation for odd/even check
  PUBLIC:: IS_CLOSE                         !IS_CLOSE(VAR1,VAR2,[RTOL],[ATOL]) or IS_CLOSE(VAR1,VAR2,DIGIT)
  !
  PUBLIC:: IS_BLANK, IS_INTEGER, IS_NUMBER
  !
  PUBLIC:: IS_WINDOWS                       ! Clone of function in PATH_INTERFACE
  !
  PUBLIC:: IS_NAN_OR                        !    IS_NAN_OR(VAR,OP,VAL)
  PUBLIC:: IS_NOT_NAN_OR                    !IS_NOT_NAN_OR(VAR,OP,VAL)
  !
  PUBLIC:: IS_PRESENT, IS_PRESENT_OR       ! IS_PRESENT(VAL,[WANT])   --Compares type of want to val to see if they match, if WANT not present, then just returns PRESENT(VAL)
  !
  PUBLIC:: IS_UNIQUE
  !
  INTERFACE IS_UNIQUE
    MODULE PROCEDURE STR_IS_UNIQUE      !(VAL, VEC, UPCASE)
    MODULE PROCEDURE STR_CASE_IS_UNIQUE !(VAL, VEC, [IQNORE])
    MODULE PROCEDURE INT_IS_UNIQUE      !(VAL, VEC)
    MODULE PROCEDURE REAL_IS_UNIQUE     !(VAL, VEC, [TOL])
    MODULE PROCEDURE DBLE_IS_UNIQUE     !(VAL, VEC, [TOL])
  END INTERFACE
  !
  INTERFACE IS_PRESENT_OR
    MODULE PROCEDURE IS_PRESENT_OR_INT  !IS_PRESENT_OR(VAL,RET)   --If VAL is present, return VAL, else return RET
    MODULE PROCEDURE IS_PRESENT_OR_DBL
    MODULE PROCEDURE IS_PRESENT_OR_SNG
  END INTERFACE
  !
  INTERFACE IS_CLOSE
    MODULE PROCEDURE IS_CLOSE_RTOL_ATOL ! IS_CLOSE(VAR1,VAR2,[RTOL],[ATOL])  ! Default RTOL=1E-6, ATOL=1E-10                                      - Match     7 digits or less than 1E-10
    MODULE PROCEDURE IS_CLOSE_DIGIT     ! IS_CLOSE(VAR1,VAR2, DIGIT)         ! RTOL set to check agianst number of DIGIT digits, sets ATOL=1E-12  - Match DIGIT digits or less than 1E-12
  END INTERFACE
  !
  INTERFACE IS_BOM                      ! Indicates if file has a byte order mark (BOM) -> 0 = FALSE, 1 = TRUE, -1 = ERROR
    MODULE PROCEDURE IS_BOM_FILE        ! IS_BOM(FILE)
    MODULE PROCEDURE IS_BOM_IU          ! IS_BOM(IU)
  END INTERFACE
  !
  CHARACTER(*), PARAMETER:: lowerCHAR="abcdefghijklmnopqrstuvwxyz"
  CHARACTER(*), PARAMETER:: upperCHAR="ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  !CHARACTER, PARAMETER:: TAB        =  ACHAR(9)
  !LOGICAL, PARAMETER:: TRUE  = .TRUE.
  !LOGICAL, PARAMETER:: FALSE = .FALSE.
  !INTEGER, PARAMETER:: Z   = 0
  !INTEGER, PARAMETER:: ONE = 1
  !INTEGER, PARAMETER:: TWO = 2
  !!
  !REAL(DBL), PARAMETER:: DNEG  = -1.0_dbl
  !REAL(DBL), PARAMETER:: DZ    =  0.0_dbl
  !REAL(DBL), PARAMETER:: TENTH =  0.1_dbl
  !REAL(DBL), PARAMETER:: inf   = huge(DZ)
  !REAL(DBL), PARAMETER:: NEARZERO_6  = 1E-6_dbl
  !REAL(DBL), PARAMETER:: NEARZERO_10 = 1E-10_dbl
  !REAL(DBL), PARAMETER:: NEARZERO_12 = 1E-12_dbl
  !
  CONTAINS
  !
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !  
  ! IS_ODD(NUM), IS_EVEN(NUM)
  !
  PURE FUNCTION IS_ODD(NUM)
    INTEGER, INTENT(IN):: NUM
    LOGICAL:: IS_ODD
    IS_ODD = BTEST(NUM, 0)   ! Odd number if least significant bit is set  -- Old Method: IAND(NUM,1) == 1
  END FUNCTION
  !
  !################################################################################
  !
  PURE FUNCTION IS_EVEN(NUM)
    INTEGER, INTENT(IN):: NUM
    LOGICAL:: IS_EVEN
    IS_EVEN = .NOT. BTEST(NUM, 0)   ! -- Old Method: IAND(NUM,1) == 0
  END FUNCTION
  !
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !  
  ! IS_BOM
  !
  FUNCTION IS_BOM_FILE(FILE) RESULT(IS_BOM)  ! IS_BOM -> 0 = FALSE, 1 = TRUE, -1 = ERROR
    CHARACTER(*), INTENT(IN):: FILE
    INTEGER :: IS_BOM
    CHARACTER(3):: BOM
    INTEGER:: IU, NOSTOP
    !
    OPEN(NEWUNIT=IU, FILE=FILE,                            &
                     STATUS='OLD',       ACTION='READ',    &   
                     FORM='UNFORMATTED', ACCESS='STREAM',  &
                     POSITION='REWIND',  IOSTAT=IS_BOM     )
    !
    IF(IS_BOM == Z) READ(IU, '(A)', ADVANCE='NO', IOSTAT=IS_BOM) BOM
    !
    IF(IS_BOM /= Z) THEN
                    IS_BOM = NEG
                    !
    ELSEIF ( ICHAR(BOM(1:1)) == 239 .AND.  &
             ICHAR(BOM(2:2)) == 187 .AND.  &
             ICHAR(BOM(3:3)) == 191        ) THEN
                                             IS_BOM = ONE
    END IF
    !
    CLOSE(IU, IOSTAT=NOSTOP)
    !
  END FUNCTION 
  !
  FUNCTION IS_BOM_IU(IU) RESULT(IS_BOM)  ! IS_BOM -> 0 = FALSE, 1 = TRUE, -1 = ERROR
    INTEGER, INTENT(IN):: IU
    INTEGER :: IS_BOM
    CHARACTER(:), ALLOCATABLE:: FNAM
    INTEGER:: I, SIZ
    LOGICAL:: FOUND
    !
    IF(IU == Z) THEN
                IS_BOM = Z
    ELSE
        ! Get File Name --------------------------------------------
        ALLOCATE(CHARACTER(256):: FNAM)
        INQUIRE(IU, NAME=FNAM, EXIST=FOUND)
        !
        IF(FOUND) THEN
           INQUIRE(FILE=FNAM, EXIST=FOUND)  ! FOUND file, check if FNAM is big enough
           IF(.NOT. FOUND) THEN
                 DO I=ONE, 15
                   IF(FOUND) THEN
                                 EXIT
                   ELSE
                       SIZ = 512 * I
                       DEALLOCATE(FNAM)
                       ALLOCATE(CHARACTER(SIZ):: FNAM)
                       INQUIRE(IU, NAME=FNAM)
                       INQUIRE(FILE=FNAM, EXIST=FOUND)  !FOUND IF FILE NAME SIZE IS BIG ENOUGH
                   END IF
                 END DO
           END IF
        END IF
        ! End Get File Name ----------------------------------------
        !
        IF(FOUND) THEN
                  IS_BOM = IS_BOM_FILE(FNAM)
        ELSE
                  IS_BOM = NEG
        END IF
    END IF
    !
  END FUNCTION
  !
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !  
  !  Zero, 0-1 Range, and Close routines
  !
  !#############################################################################################################################################################
  !
  PURE ELEMENTAL FUNCTION IS_CLOSE_RTOL_ATOL(VAR1,VAR2,RTOL,ATOL) RESULT(IS_CLOSE) !Default RTOL=1E-6, ATOL=1E-10 - Match 7 digits or less than 1E-10
    REAL(DBL),           INTENT(IN):: VAR1, VAR2
    REAL(DBL), OPTIONAL, INTENT(IN):: RTOL, ATOL
    LOGICAL:: IS_CLOSE
    REAL(DBL):: CHK, DIFF
    !ABS(VAR1-VAR2) <= (ATOL + RTOL * ABS(VAR2))
    !
    IF(VAR1 /= VAR1 .OR. VAR2 /= VAR2) THEN
        IS_CLOSE = FALSE
    ELSE
        IF(VAR1 > VAR2) THEN
            !
            IF    (VAR2 >= DZ) THEN           ! Solve CHK = MIN(ABS(VAR1), ABS(VAR2))
                               CHK = VAR2
            ELSEIF(VAR1 <= DZ) THEN 
                               CHK = DNEG*VAR1
            ELSEIF(DNEG*VAR2 > VAR1) THEN         !Only here if VAR1 > 0 and VAR2 < 0 and VAR1 > VAR2
                               CHK = VAR1
            ELSE
                               CHK = DNEG*VAR2
            END IF
            !
            DIFF  = VAR1 - VAR2
        ELSE
            IF    (VAR1 >= DZ) THEN           ! Solve CHK = MIN(ABS(VAR1), ABS(VAR2))
                               CHK = VAR1
            ELSEIF(VAR2 <= DZ) THEN 
                               CHK = DNEG*VAR2
            ELSEIF(DNEG*VAR1 > VAR2) THEN         !Only here if VAR2 > 0 and VAR1 < 0 and VAR2 > VAR1
                               CHK = VAR2
            ELSE
                               CHK = DNEG*VAR1
            END IF
            !
            DIFF   = VAR2 - VAR1
        END IF
        !
        IF(PRESENT(RTOL)) THEN
            CHK = CHK * RTOL
        ELSE
            CHK = CHK * NEARZERO_6  !COMPARE AGAINST 7th DIGIT (SINGLE PRECISION)
        END IF
        !
        IF(PRESENT(ATOL)) THEN
            IS_CLOSE = DIFF <= ATOL + CHK
        ELSE
            IS_CLOSE = DIFF <= NEARZERO_10 + CHK
        END IF
        !
    END IF
    !
  END FUNCTION
  !
  !#############################################################################################################################################################
  !
  PURE ELEMENTAL FUNCTION IS_CLOSE_DIGIT(VAR1,VAR2,DIGIT) RESULT(IS_CLOSE) !Match within DIGIT precision (DIGIT numbers match).
    REAL(DBL),           INTENT(IN):: VAR1, VAR2
    INTEGER,             INTENT(IN):: DIGIT
    LOGICAL:: IS_CLOSE
    REAL(DBL):: CHK, DIFF, RTOL
    !
    !ABS(VAR1-VAR2) <= (ATOL + RTOL * ABS(VAR2))
    ! 1E-6_dbl  !COMPARE AGAINST 7th DIGIT (SINGLE PRECISION)
    !
    IF    ( DIGIT <  3 ) THEN   !Less than 2 digits is held at 2 (or 1E-1)
                         RTOL = TENTH
    ELSEIF( DIGIT > 12 ) THEN   !More than 13 digits is held at 13 (or 1E-12)
                         RTOL =  NEARZERO_12
    ELSE
                         RTOL = TENTH**(DIGIT-ONE)  !Check against DIGIT digits
    END IF
    !
    IF(VAR1 /= VAR1 .OR. VAR2 /= VAR2) THEN
        IS_CLOSE = FALSE
    ELSE
        IF(VAR1 > VAR2) THEN
            !
            IF    (VAR2 >= DZ) THEN           ! Solve CHK = MIN(ABS(VAR1), ABS(VAR2))
                               CHK = VAR2
            ELSEIF(VAR1 <= DZ) THEN 
                               CHK = DNEG*VAR1
            ELSEIF(DNEG*VAR2 > VAR1) THEN         !Only here if VAR1 > 0 and VAR2 < 0 and VAR1 > VAR2
                               CHK = VAR1
            ELSE
                               CHK = DNEG*VAR2
            END IF
            !
            DIFF  = VAR1 - VAR2
        ELSE
            IF    (VAR1 >= DZ) THEN           ! Solve CHK = MIN(ABS(VAR1), ABS(VAR2))
                               CHK = VAR1
            ELSEIF(VAR2 <= DZ) THEN 
                               CHK = DNEG*VAR2
            ELSEIF(DNEG*VAR1 > VAR2) THEN         !Only here if VAR2 > 0 and VAR1 < 0 and VAR2 > VAR1
                               CHK = VAR2
            ELSE
                               CHK = DNEG*VAR1
            END IF
            !
            DIFF   = VAR2 - VAR1
        END IF
        !
        IS_CLOSE = DIFF <= NEARZERO_12 + CHK*RTOL
        !
    END IF
    !
  END FUNCTION
  !
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !  
  !  IS_ routines
  !
  PURE FUNCTION IS_BLANK(LN) RESULT(EMPTY)
    CHARACTER(*),      INTENT(IN):: LN
    LOGICAL:: EMPTY
    INTEGER:: I
    !
    EMPTY = TRUE
    DO I=ONE, LEN_TRIM(LN)
        IF(LN(I:I) /= " " .AND. LN(I:I) /= TAB .AND. LN(I:I) /= "," ) THEN
            EMPTY = FALSE
            EXIT
        ELSEIF(LN(I:I)=="#") THEN ! Reached the comment marker
            EXIT
        END IF
    END DO
    !
  END FUNCTION
  !
  !#############################################################################################################################################################
  !
  PURE ELEMENTAL FUNCTION IS_INTEGER(VAR)
    CHARACTER(*), INTENT(IN):: VAR
    LOGICAL:: IS_INTEGER
    INTEGER:: I, N, ISTART
    !
    N = LEN_TRIM(VAR)
    ISTART = N+1
    DO I=ONE, N
        IF(VAR(I:I) /= ' ') THEN
            ISTART = I
            IF(VAR(I:I)=='+' .OR. VAR(I:I) =='-') ISTART = ISTART + ONE
            EXIT
        END IF
    END DO
    !
    IF( ISTART <= N ) THEN
       IS_INTEGER = TRUE
       DO I=ISTART, N
           IF( INDEX('0123456789',VAR(I:I)) == Z ) THEN ! DID NOT FIND A NUMBER, SO NOT AN INTEGER
               IS_INTEGER = FALSE
               EXIT
           END IF
       END DO
    ELSE
        IS_INTEGER = FALSE
    END IF
    !
  END FUNCTION
  !
  !#############################################################################################################################################################
  !
  PURE ELEMENTAL FUNCTION IS_NUMBER(VAR)
    CHARACTER(*), INTENT(IN):: VAR
    LOGICAL:: IS_NUMBER
    INTEGER:: I, N, ISTART
    CHARACTER(17),PARAMETER:: VALID='0123456789EeDd+-.'
    CHARACTER(10),PARAMETER:: NUM='0123456789'
    CHARACTER(4), PARAMETER:: ED = 'EeDd'
    CHARACTER(2), PARAMETER:: PM = '+-'
    CHARACTER,    PARAMETER:: P  = '.'
    !
    N = LEN_TRIM(VAR)
    ISTART = N+1
    DO I=ONE, N
        IF(VAR(I:I) /= ' ') THEN
            ISTART = I
            EXIT
        END IF
    END DO
    !
    DO I=ISTART, N
        IF(VAR(I:I)==' ') THEN !NO BLANK SPACES ALLOWED
            IS_NUMBER = FALSE
            RETURN !--------------------------------------------------------
        END IF
    END DO
    !
    IF    ( ISTART > N) THEN
                             IS_NUMBER = FALSE                                                ! BLANK LINE
    ELSEIF( ISTART == N) THEN
                             IS_NUMBER = SCAN(NUM,VAR(N:N)) > Z                               ! SIZE = 1, MUST BE NUMBER
    ELSE
        IS_NUMBER = TRUE
        !
        IF(VAR(ISTART:ISTART)=='+' .OR. VAR(ISTART:ISTART) == '-') ISTART = ISTART + ONE      !IF FIRST IS SIGN,  THEN MOVE FORWARD
        IF(VAR(ISTART:ISTART)=='.') THEN                                                      !IF FIRST IS POINT, THEN MOVE FORWARD  --NOTE YOU CAN HAVE -.5
            ISTART = ISTART + ONE
            IF(ISTART > N) THEN
                IS_NUMBER = FALSE
            ELSEIF(INDEX(VAR(ISTART:N),P) > Z)  THEN! NO SECOND . ALLOWED
                IS_NUMBER = FALSE
            ELSE
                IS_NUMBER = INDEX(NUM,VAR(ISTART:ISTART)) > Z  !START WITH . MUST PROCEDE WITH NUMBER
            END IF
        ELSEIF(SCAN(NUM,VAR(ISTART:ISTART)) == Z) THEN  !NO NUMBER IN FIRST LOCATION ATER +-
            IS_NUMBER = FALSE
        ELSE
            I = INDEX(VAR(ISTART:N),P)
            IF ( I > Z) THEN
               IS_NUMBER = I == INDEX(VAR(ISTART:N),P, BACK=TRUE)  !AT MOST ONE POINT "."
               IF(IS_NUMBER) THEN
                   I = INDEX(VAR(:N),P) - ONE
                   IS_NUMBER = INDEX(NUM,VAR(I:I)) > Z  !MUST PRECEDE BY NUMBER IF NOT IN FIRST LOCATION
               END IF
            END IF
            ISTART = ISTART + ONE  !MOVE PAST FIRST NUMBER
        END IF
        !
        IF(ISTART > N) RETURN !--------------------------------------------------------
        !
        IF(IS_NUMBER) THEN
            I = SCAN(VAR(:N),ED)
            IF(I == N) THEN
                      IS_NUMBER = FALSE
            ELSEIF( I > Z ) THEN
                  IF( I /= SCAN(VAR(:N),ED, BACK=TRUE)) THEN
                      IS_NUMBER = FALSE
                  ELSE
                      IF (VAR(I-ONE:I-ONE) == P) THEN !!MUST PRECEDE BY NUMBER OR .
                          IS_NUMBER = INDEX(NUM,VAR(I-TWO:I-TWO)) > Z 
                      ELSE
                          IS_NUMBER = INDEX(NUM,VAR(I-ONE:I-ONE)) > Z 
                      END IF
                  END IF
            END IF
        END IF
        !
        IF(IS_NUMBER) THEN
            I = SCAN(VAR(ISTART:N),PM)
            IF( I > Z ) THEN
                  IF( I /= SCAN(VAR(ISTART:N),PM, BACK=TRUE)) THEN
                      IS_NUMBER = FALSE
                  ELSE
                      I = SCAN(VAR(:N),PM, BACK=TRUE) - ONE
                      IS_NUMBER = INDEX(ED,VAR(I:I)) > Z .AND. INDEX(NUM,VAR(I+TWO:I+TWO)) > Z  ! MUST BE PRECEDED BY a D or E AND PROCEDE WITH A NUM
                  END IF
            END IF
        END IF
        !
        IF(IS_NUMBER) THEN
            I = ISTART
            DO WHILE (IS_NUMBER .AND. I <= N)
                  IS_NUMBER = INDEX(VALID,VAR(I:I)) > Z
                  I = I + ONE
            END DO
        END IF
    END IF
    !
  END FUNCTION
  !
!!!  PURE ELEMENTAL FUNCTION IS_NUMBER(VAR)
!!!    CHARACTER(*), INTENT(IN):: VAR
!!!    LOGICAL:: IS_NUMBER
!!!    INTEGER:: I, N, ISTART
!!!    CHARACTER(10),PARAMETER:: NUM='0123456789'
!!!    CHARACTER(4),PARAMETER:: ED = 'EeDd'
!!!    CHARACTER(2),PARAMETER:: PM = '+-'
!!!    CHARACTER, PARAMETER:: P='.'
!!!    !
!!!    N = LEN_TRIM(VAR)
!!!    ISTART = N+1
!!!    DO I=ONE, N
!!!        IF(VAR(I:I) /= ' ') THEN
!!!            ISTART = I
!!!            EXIT
!!!        END IF
!!!    END DO
!!!    !
!!!    IF    ( ISTART > N) THEN
!!!                             IS_NUMBER = FALSE                                                ! BLANK LINE
!!!    ELSEIF( ISTART == N) THEN
!!!                             IS_NUMBER = SCAN(NUM,VAR(N:N)) > Z                               ! SIZE = 1, MUST BE NUMBER
!!!    ELSE
!!!        IF(VAR(ISTART:ISTART)=='+' .OR. VAR(ISTART:ISTART) == '-') ISTART = ISTART + ONE      !IF FIRST IS SIGN, THEN MOVE FORWARD
!!!        !
!!!        IS_NUMBER = SCAN(NUM,VAR(ISTART:ISTART)) > Z                                          !FIRST POSITION MUST BE NUMBER
!!!        !
!!!        I=ISTART+ONE
!!!        DO WHILE (IS_NUMBER .AND. I <= N)
!!!              IF(" " == VAR(I:I)) THEN                                          !SHOULD NOT HAVE A BLANK SPACE WITHIN
!!!                  IS_NUMBER = FALSE
!!!              ELSEIF(P == VAR(I:I)) THEN
!!!                  IF(I<N      ) IS_NUMBER = SCAN(VAR(I+ONE:),P) == Z             ! NO SECOND .
!!!                  IF(IS_NUMBER) IS_NUMBER = SCAN(NUM,VAR(I-ONE:I-ONE)) > Z       ! MUST PRECEDE BY NUMBER
!!!              ELSEIF(SCAN(PM,VAR(I:I)) > Z ) THEN
!!!                      IF(I<N      ) IS_NUMBER = SCAN(VAR(I+ONE:),PM) == Z .AND. SCAN(NUM,VAR(I+ONE:I+ONE)) > Z  !NO SECOND +- AND MOST PROCEDDE BY NUMBER
!!!                      IF(IS_NUMBER) IS_NUMBER = SCAN(ED,VAR(I-ONE:I-ONE)) > Z                                   !MUST PRECEDE BY E or D
!!!              ELSEIF(SCAN(ED,VAR(I:I)) > Z ) THEN
!!!                      IF(I<N      ) IS_NUMBER = SCAN(VAR(I+ONE:),ED) == Z                                       !NO SECOND E or D
!!!                      IF(IS_NUMBER) IS_NUMBER = SCAN(NUM,VAR(I-ONE:I-ONE)) > Z .OR. VAR(I-ONE:I-ONE) == P       !MUST PRECEDE BY NUMBER OR .
!!!              ELSE
!!!                  IS_NUMBER = SCAN(NUM,VAR(I:I)) > Z                                                            !MUST BE A NUMBER
!!!              END IF
!!!              !
!!!              I = I + ONE
!!!              !
!!!        END DO
!!!    END IF
!!!    !
!!!  END FUNCTION
        !IF(IS_NUMBER) THEN
        !    !
        !    DO I=ISTART+ONE, N
        !        IF(" " == VAR(I:I)) THEN                                          !SHOULD NOT HAVE A BLANK SPACE WITHIN
        !            IS_NUMBER = FALSE
        !        ELSEIF(P == VAR(I:I)) THEN
        !            IF(I<N      ) IS_NUMBER = SCAN(VAR(I+ONE:),P) == Z             ! NO SECOND .
        !            IF(IS_NUMBER) IS_NUMBER = SCAN(NUM,VAR(I-ONE:I-ONE)) > Z       ! MUST PRECEDE BY NUMBER
        !        ELSEIF(SCAN(PM,VAR(I:I)) > Z ) THEN
        !                IF(I<N      ) IS_NUMBER = SCAN(VAR(I+ONE:),PM) == Z .AND. SCAN(NUM,VAR(I+ONE:I+ONE)) > Z  !NO SECOND +- AND MOST PROCEDDE BY NUMBER
        !                IF(IS_NUMBER) IS_NUMBER = SCAN(ED,VAR(I-ONE:I-ONE)) > Z                                   !MUST PRECEDE BY E or D
        !        ELSEIF(SCAN(ED,VAR(I:I)) > Z ) THEN
        !                IF(I<N      ) IS_NUMBER = SCAN(VAR(I+ONE:),ED) == Z                                       !NO SECOND E or D
        !                IF(IS_NUMBER) IS_NUMBER = SCAN(NUM,VAR(I-ONE:I-ONE)) > Z .OR. VAR(I-ONE:I-ONE) == P       !MUST PRECEDE BY NUMBER OR .
        !        ELSE
        !            IS_NUMBER = SCAN(NUM,VAR(I:I)) > Z                                                            !MUST BE A NUMBER
        !        END IF
        !        !
        !        IF(.NOT. IS_NUMBER) EXIT
        !        !
        !    END DO
        !    !
        !END IF
  !
  !#############################################################################################################################################################
  !
  PURE ELEMENTAL FUNCTION IS_NAN_OR(VAR,OP,VAL)
    REAL(DBL),             INTENT(IN):: VAR
    CHARACTER(2),     INTENT(IN), OPTIONAL:: OP
    REAL(DBL), INTENT(IN), OPTIONAL:: VAL
    LOGICAL:: IS_NAN_OR
    !
    IS_NAN_OR = IEEE_IS_NAN(VAR)
    !
    IF(.NOT. IS_NAN_OR) THEN
       IF(PRESENT(OP)) THEN
                           SELECT CASE(OP)
                           CASE('GE','>='); IS_NAN_OR = VAR >=  VAL
                           CASE('GT','>' ); IS_NAN_OR = VAR >   VAL
                           CASE('LE','<='); IS_NAN_OR = VAR <=  VAL
                           CASE('LT','<' ); IS_NAN_OR = VAR <   VAL
                           CASE('EQ','=='); IS_NAN_OR = VAR ==  VAL
                           CASE('NE','/='); IS_NAN_OR = VAR /=  VAL
                           END SELECT
       END IF
    END IF
    !
  END FUNCTION
  !
  !#############################################################################################################################################################
  !
  PURE ELEMENTAL FUNCTION IS_NOT_NAN_OR(VAR,OP,VAL)
    REAL(DBL),             INTENT(IN):: VAR
    CHARACTER(2),     INTENT(IN), OPTIONAL:: OP
    REAL(DBL), INTENT(IN), OPTIONAL:: VAL
    LOGICAL:: IS_NOT_NAN_OR
    !
    IS_NOT_NAN_OR = .NOT. IEEE_IS_NAN(VAR)
    !
    IF(IS_NOT_NAN_OR) THEN
       IF(PRESENT(OP)) THEN
                           SELECT CASE(OP)
                           CASE('GE','>='); IS_NOT_NAN_OR = VAR >=  VAL
                           CASE('GT','>' ); IS_NOT_NAN_OR = VAR >   VAL
                           CASE('LE','<='); IS_NOT_NAN_OR = VAR <=  VAL
                           CASE('LT','<' ); IS_NOT_NAN_OR = VAR <   VAL
                           CASE('EQ','=='); IS_NOT_NAN_OR = VAR ==  VAL
                           CASE('NE','/='); IS_NOT_NAN_OR = VAR /=  VAL
                           END SELECT
       END IF
    END IF
    !
  END FUNCTION
  !
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !  
  !  IS_UNIQUE  routines
  !
  PURE FUNCTION STR_IS_UNIQUE(VAL, VEC, UPCASE) RESULT (UNI)
    CHARACTER(*),                         INTENT(IN):: VAL
    CHARACTER(*),DIMENSION(:),CONTIGUOUS, INTENT(IN):: VEC
    LOGICAL,                              INTENT(IN):: UPCASE
    LOGICAL:: UNI
    INTEGER:: I, N, M, DIM
    !
    IF(.NOT. UPCASE) THEN
                         UNI = ALL(VAL /= VEC)
    ELSE
       N   = LEN_TRIM(VAL)
       M   = LEN(VEC)
       DIM = SIZE(VEC)
       !
       BLOCK
         CHARACTER(N):: SUB
         CHARACTER(M):: STR
         !
         SUB = VAL
         CALL UPPER(N, SUB)
         !
         DO I=ONE, DIM
             UNI = TRUE
             !
             STR = VEC(I)
             CALL UPPER(M, STR)
             !
             IF(SUB==STR) THEN
                              UNI=FALSE
                              EXIT
             END IF
         END DO
       END BLOCK
    END IF
    !
  END FUNCTION
  !
  !PURE FUNCTION STR_IS_UNIQUE(VAL, VEC, UPCASE) RESULT (UNI)
  !  CHARACTER(*),                         INTENT(IN):: VAL
  !  CHARACTER(*),DIMENSION(:),CONTIGUOUS, INTENT(IN):: VEC
  !  LOGICAL,                              INTENT(IN):: UPCASE
  !  LOGICAL:: UNI
  !  CHARACTER(LEN_TRIM(VAL)):: SUB
  !  CHARACTER(LEN(VEC)):: STR
  !  INTEGER:: I
  !  !
  !  IF(UPCASE) THEN
  !     SUB = VAL
  !     CALL UPPER(SUB)
  !     !
  !     DO I=ONE, SIZE(VEC)
  !         UNI = TRUE
  !         STR = VEC(I)
  !         CALL UPPER(STR)
  !         IF(SUB==STR) THEN
  !             UNI=FALSE
  !             EXIT
  !         END IF
  !     END DO
  !  ELSE
  !      UNI = STR_CASE_IS_UNIQUE(VAL, VEC)
  !  END IF
  !  !
  !END FUNCTION
  !
  !#########################################################################################################################
  !
  PURE FUNCTION STR_CASE_IS_UNIQUE(VAL, VEC, IQNORE) RESULT (UNI)
    CHARACTER(*),                         INTENT(IN):: VAL
    CHARACTER(*),DIMENSION(:),CONTIGUOUS, INTENT(IN):: VEC
    CHARACTER(*),OPTIONAL,                INTENT(IN):: IQNORE
    LOGICAL:: UNI
    INTEGER:: I
    !
    IF(PRESENT(IQNORE)) THEN
        UNI = TRUE
        DO I=ONE, SIZE(VEC)
            IF(VAL==VEC(I) .AND. VEC(I) /= IQNORE) THEN
                UNI=FALSE
                EXIT
            END IF
        END DO
    ELSE
                UNI = ALL(VAL /= VEC)
    END IF
    !
  END FUNCTION
  !
  !#########################################################################################################################
  !
  PURE FUNCTION INT_IS_UNIQUE(VAL, VEC) RESULT (UNI)
    INTEGER,                         INTENT(IN):: VAL
    INTEGER,DIMENSION(:),CONTIGUOUS, INTENT(IN):: VEC
    LOGICAL:: UNI
    !
    UNI = ALL(VAL /= VEC)
    !
  END FUNCTION
  !
  !#########################################################################################################################
  !
  PURE FUNCTION REAL_IS_UNIQUE(VAL, VEC, TOL) RESULT (UNI)
    REAL(SNG),                         INTENT(IN):: VAL
    REAL(SNG),DIMENSION(:),CONTIGUOUS, INTENT(IN):: VEC
    REAL(SNG), OPTIONAL,               INTENT(IN):: TOL
    LOGICAL:: UNI
    INTEGER:: I
    !
    IF(PRESENT(TOL)) THEN
        UNI = TRUE
        DO I=ONE, SIZE(VEC)
            IF(ABS(VAL-VEC(I))<TOL) THEN
                UNI=FALSE
                EXIT
            END IF
        END DO
    ELSE
                UNI = ALL(VAL /= VEC)
    END IF
    !
  END FUNCTION
  !
  !#########################################################################################################################
  !
  PURE FUNCTION DBLE_IS_UNIQUE(VAL, VEC, TOL) RESULT (UNI)
    REAL(DBL),                         INTENT(IN):: VAL
    REAL(DBL),DIMENSION(:),CONTIGUOUS, INTENT(IN):: VEC
    REAL(DBL), OPTIONAL,               INTENT(IN):: TOL
    LOGICAL:: UNI
    INTEGER:: I
    !
    IF(PRESENT(TOL)) THEN
        UNI = TRUE
        DO I=ONE, SIZE(VEC)
            IF(ABS(VAL-VEC(I))<TOL) THEN
                UNI=FALSE
                EXIT
            END IF
        END DO
    ELSE
                UNI = ALL(VAL /= VEC)
    END IF
    !
  END FUNCTION 
  !
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !
  ! IS_PRESENT Routines
  !
  PURE FUNCTION IS_PRESENT(VAL,WANT) RESULT(ANS)
    CLASS(*), OPTIONAL, INTENT(IN):: VAL
    CLASS(*), OPTIONAL, INTENT(IN):: WANT
    LOGICAL:: ANS
    !
    IF (PRESENT(VAL)) THEN
                       ANS = TRUE
                       IF(PRESENT(WANT)) THEN
                                         SELECT TYPE (VAL)
                                         TYPE IS (REAL(DBL));    ANS = IS_PRESENT_REL64(VAL,WANT)
                                         TYPE IS (INTEGER(i32)); ANS = IS_PRESENT_INT32(VAL,WANT)
                                         TYPE IS (INTEGER(i64)); ANS = IS_PRESENT_INT64(VAL,WANT)
                                         TYPE IS (LOGICAL);      ANS = IS_PRESENT_BOOL (VAL,WANT)
                                         END SELECT
                       END IF
    ELSE
                       ANS = FALSE
    END IF
    !
  END FUNCTION
  !
  PURE FUNCTION IS_PRESENT_BOOL(VAL,WANT) RESULT(ANS)
    LOGICAL,            INTENT(IN):: VAL
    CLASS(*), OPTIONAL, INTENT(IN):: WANT
    LOGICAL:: ANS
    !
    IF (PRESENT(WANT)) THEN
                       SELECT TYPE (WANT)
                       TYPE IS (LOGICAL);       ANS = VAL .eqv. WANT
                       TYPE IS (REAL(DBL));     ANS = FALSE
                       TYPE IS (INTEGER(i32));  ANS = FALSE
                       TYPE IS (INTEGER(i64));  ANS = FALSE
                       END SELECT
    ELSE
                       ANS = VAL
    END IF
    !
  END FUNCTION
  !
  PURE FUNCTION IS_PRESENT_INT32(VAL,WANT) RESULT(ANS)
    INTEGER(i32),       INTENT(IN):: VAL
    CLASS(*), OPTIONAL, INTENT(IN):: WANT
    LOGICAL:: ANS
    !
    IF (PRESENT(WANT)) THEN
                       SELECT TYPE (WANT)
                       TYPE IS (INTEGER(i32));  ANS = VAL == WANT
                       TYPE IS (REAL(DBL));     ANS = FALSE
                       TYPE IS (INTEGER(i64));  ANS = FALSE
                       TYPE IS (LOGICAL);       ANS = FALSE
                       END SELECT
    ELSE
                       ANS = TRUE
    END IF
    !
  END FUNCTION
  !
  PURE FUNCTION IS_PRESENT_INT64(VAL,WANT) RESULT(ANS)
    INTEGER(i64),       INTENT(IN):: VAL
    CLASS(*), OPTIONAL, INTENT(IN):: WANT
    LOGICAL:: ANS
    !
    IF (PRESENT(WANT)) THEN
                       SELECT TYPE (WANT)
                       TYPE IS (INTEGER(i64));  ANS = VAL == WANT
                       TYPE IS (REAL(DBL));     ANS = FALSE
                       TYPE IS (INTEGER(i32));  ANS = FALSE
                       TYPE IS (LOGICAL);       ANS = FALSE
                       END SELECT
    ELSE
                       ANS = TRUE
    END IF
    !
  END FUNCTION
  !
  PURE FUNCTION IS_PRESENT_REL64(VAL,WANT) RESULT(ANS)
    REAL(DBL),          INTENT(IN):: VAL
    CLASS(*), OPTIONAL, INTENT(IN):: WANT
    LOGICAL:: ANS
    !
    IF (PRESENT(WANT)) THEN
                       SELECT TYPE (WANT)
                       TYPE IS (REAL(DBL));     ANS = VAL == WANT
                       TYPE IS (INTEGER(i32));  ANS = FALSE
                       TYPE IS (INTEGER(i64));  ANS = FALSE
                       TYPE IS (LOGICAL);       ANS = FALSE
                       END SELECT
    ELSE
                       ANS = TRUE
    END IF
    !
  END FUNCTION
  !
  PURE FUNCTION IS_PRESENT_OR_INT(VAL,RET) RESULT(ANS)
    INTEGER, OPTIONAL, INTENT(IN):: VAL
    INTEGER,           INTENT(IN):: RET
    INTEGER:: ANS
    !
    IF (PRESENT(VAL)) THEN
                       ANS = VAL
    ELSE
                       ANS = RET
    END IF
    !
  END FUNCTION
  !
  PURE FUNCTION IS_PRESENT_OR_DBL(VAL,RET) RESULT(ANS)
    REAL(DBL), OPTIONAL, INTENT(IN):: VAL
    REAL(DBL),           INTENT(IN):: RET
    REAL(DBL):: ANS
    !
    IF (PRESENT(VAL)) THEN
                       ANS = VAL
    ELSE
                       ANS = RET
    END IF
    !
  END FUNCTION
  !
  PURE FUNCTION IS_PRESENT_OR_SNG(VAL,RET) RESULT(ANS)
    REAL(SNG), OPTIONAL, INTENT(IN):: VAL
    REAL(SNG),           INTENT(IN):: RET
    REAL(SNG):: ANS
    !
    IF (PRESENT(VAL)) THEN
                       ANS = VAL
    ELSE
                       ANS = RET
    END IF
    !
  END FUNCTION
  !
  PURE FUNCTION IS_PRESENT_OR_i64(VAL,RET) RESULT(ANS)
    INTEGER(i64), OPTIONAL, INTENT(IN):: VAL
    INTEGER(i64),           INTENT(IN):: RET
    INTEGER(i64):: ANS
    !
    IF (PRESENT(VAL)) THEN
                       ANS = VAL
    ELSE
                       ANS = RET
    END IF
    !
  END FUNCTION
  !
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !
  ! IS_WINDOWS Routines
  !
  FUNCTION IS_WINDOWS() RESULT(IS_WIN)
    LOGICAL:: IS_WIN
    CHARACTER(10):: OS
    !
    CALL GET_ENVIRONMENT_VARIABLE ( "OS", value=OS )
    IS_WIN = OS == 'Windows_NT'                     !If any Windows variant then variable exists and is set to Windows_NT
    !
  END FUNCTION
  !
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !
  ! Internal Use Upper Case Routine
  !
  PURE SUBROUTINE UPPER(DIM, LN)
       INTEGER, INTENT(IN):: DIM
       CHARACTER(DIM), INTENT(INOUT):: LN
       INTEGER:: I, N
       !
       DO I=1, DIM
           N = INDEX( lowerCHAR, LN(I:I))
           !
           IF(N > 0) LN(I:I) = upperCHAR(N:N)
       END DO
  END SUBROUTINE
  !
END MODULE