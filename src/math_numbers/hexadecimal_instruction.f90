MODULE HEXADECIMAL_INSTRUCTION
  USE, INTRINSIC:: ISO_FORTRAN_ENV, ONLY: INT32, INT64
  IMPLICIT NONE
  !
  PUBLIC:: HEXADECIMAL
  !
  PRIVATE
  !
  CHARACTER(21),  PARAMETER:: HEX_LIST    = '123456789ABCDEFabcdef'
  !
  INTEGER(INT32), PARAMETER:: Z    = 0_INT32
  INTEGER(INT64), PARAMETER:: ZER  = 0_INT64
  INTEGER(INT64), PARAMETER:: LONG = 1_INT64
  !
  INTEGER(INT32), PARAMETER:: inf32  = HUGE(Z)
  INTEGER(INT64), PARAMETER:: inf64  = HUGE(LONG)
  !
  TYPE HEXADECIMAL                ! Supports Hex Range from 0 to 7FFFFFFFFFFFFFFF, which is a Number Range of 0 to 9223372036854775807
     !
     INTEGER(INT64):: NUM = ZER
     !
     CONTAINS
     !
     GENERIC::              INIT    => INIT_HEXADECIMAL_STR, & ! CALL HEX%INIT(STR)
                                       INIT_HEXADECIMAL_I32, & ! CALL HEX%INIT(NUM)
                                       INIT_HEXADECIMAL_I64
     PROCEDURE, PASS(HEX):: STR     =>     STR_HEXADECIMAL     ! HEX_STR = HEX%STR([WIDTH], [PREFIX])
     PROCEDURE, PASS(HEX):: HEX     =>     HEX_HEXADECIMAL     ! HEX_STR = HEX%HEX()     -> Get 0xF  --> same as HEX%STR(0, '0x')
     PROCEDURE, PASS(HEX):: UNICODE => UNICODE_HEXADECIMAL     ! HEX_STR = HEX%UNICODE() -> Get U+0000
     PROCEDURE, PASS(HEX):: INT32   =>   INT32_HEXADECIMAL     ! INT32   = HEX%INT32()
     PROCEDURE, PASS(HEX):: INT64   =>   INT64_HEXADECIMAL     ! INT64   = HEX%INT64()
     !
     GENERIC:: ASSIGNMENT(= ) => EQUAL_HEX_HEX, EQUAL_HEX_I32, EQUAL_HEX_I64, EQUAL_I32_HEX, EQUAL_I64_HEX
     !
     GENERIC:: OPERATOR  (+ ) =>   ADD_HEX_HEX,   ADD_HEX_I32,   ADD_HEX_I64,   ADD_I32_HEX,   ADD_I64_HEX
     GENERIC:: OPERATOR  (- ) =>   SUB_HEX_HEX,   SUB_HEX_I32,   SUB_HEX_I64,   SUB_I32_HEX,   SUB_I64_HEX
     GENERIC:: OPERATOR  (* ) =>   MLT_HEX_HEX,   MLT_HEX_I32,   MLT_HEX_I64,   MLT_I32_HEX,   MLT_I64_HEX
     GENERIC:: OPERATOR  (/ ) =>   DIV_HEX_HEX,   DIV_HEX_I32,   DIV_HEX_I64,   DIV_I32_HEX,   DIV_I64_HEX
     !
     GENERIC:: OPERATOR  (==) =>    EQ_HEX_HEX,    EQ_HEX_I32,    EQ_HEX_I64,    EQ_I32_HEX,    EQ_I64_HEX
     GENERIC:: OPERATOR  (< ) =>    LT_HEX_HEX,    LT_HEX_I32,    LT_HEX_I64,    LT_I32_HEX,    LT_I64_HEX
     GENERIC:: OPERATOR  (<=) =>    LE_HEX_HEX,    LE_HEX_I32,    LE_HEX_I64,    LE_I32_HEX,    LE_I64_HEX
     GENERIC:: OPERATOR  (> ) =>    GT_HEX_HEX,    GT_HEX_I32,    GT_HEX_I64,    GT_I32_HEX,    GT_I64_HEX
     GENERIC:: OPERATOR  (>=) =>    GE_HEX_HEX,    GE_HEX_I32,    GE_HEX_I64,    GE_I32_HEX,    GE_I64_HEX
     !
     GENERIC:: READ (FORMATTED) => FMTREAD_HEXADECIMAL
     GENERIC:: WRITE(FORMATTED) => FMTWRITE_HEXADECIMAL
     !
     PROCEDURE, PASS(HEX), PRIVATE:: INIT_HEXADECIMAL_STR, INIT_HEXADECIMAL_I32, INIT_HEXADECIMAL_I64
     !
     PROCEDURE, PASS(HEX), PRIVATE:: EQUAL_HEX_HEX, EQUAL_HEX_I32, EQUAL_HEX_I64, EQUAL_I32_HEX, EQUAL_I64_HEX
     !
     PROCEDURE, PASS(HEX), PRIVATE:: ADD_HEX_HEX, ADD_HEX_I32, ADD_HEX_I64, ADD_I32_HEX, ADD_I64_HEX
     PROCEDURE, PASS(HEX), PRIVATE:: SUB_HEX_HEX, SUB_HEX_I32, SUB_HEX_I64, SUB_I32_HEX, SUB_I64_HEX
     PROCEDURE, PASS(HEX), PRIVATE:: MLT_HEX_HEX, MLT_HEX_I32, MLT_HEX_I64, MLT_I32_HEX, MLT_I64_HEX
     PROCEDURE, PASS(HEX), PRIVATE:: DIV_HEX_HEX, DIV_HEX_I32, DIV_HEX_I64, DIV_I32_HEX, DIV_I64_HEX
     !
     PROCEDURE, PASS(HEX), PRIVATE:: EQ_HEX_HEX, EQ_HEX_I32, EQ_HEX_I64, EQ_I32_HEX, EQ_I64_HEX
     !
     PROCEDURE, PASS(HEX), PRIVATE:: LT_HEX_HEX, LT_HEX_I32, LT_HEX_I64, LT_I32_HEX, LT_I64_HEX
     PROCEDURE, PASS(HEX), PRIVATE:: LE_HEX_HEX, LE_HEX_I32, LE_HEX_I64, LE_I32_HEX, LE_I64_HEX
     PROCEDURE, PASS(HEX), PRIVATE:: GT_HEX_HEX, GT_HEX_I32, GT_HEX_I64, GT_I32_HEX, GT_I64_HEX
     PROCEDURE, PASS(HEX), PRIVATE:: GE_HEX_HEX, GE_HEX_I32, GE_HEX_I64, GE_I32_HEX, GE_I64_HEX
     !
     PROCEDURE, PASS(HEX), PRIVATE:: FMTREAD_HEXADECIMAL
     PROCEDURE, PASS(HEX), PRIVATE:: FMTWRITE_HEXADECIMAL
     !
  END TYPE
  !
  CONTAINS !#####################################################################################################
  !
  PURE SUBROUTINE INIT_HEXADECIMAL_STR(HEX, STR) ! Hex range from 0 to 7FFFFFFFFFFFFFFF, or 0 to 9223372036854775807
    CLASS(HEXADECIMAL), INTENT(INOUT):: HEX
    CHARACTER(*),       INTENT(IN   ):: STR
    INTEGER(INT64):: base, M
    INTEGER(INT32):: I, J, K
    !
    HEX%NUM = ZER
    !
    J = 1
    K = LEN_TRIM(STR)
    !
    DO WHILE (J <= K)
       IF( INDEX(HEX_LIST, STR(J:J)) > 0 ) EXIT 
       J = J + 1
    END DO
    !
    base = LONG   ! powers of 16
    !
    DO I=K, J, -1_INT32
        !
        IF(STR(I:I) == " ") CYCLE
        !
        M = INDEX(HEX_LIST, STR(I:I), KIND=INT64)
        !
        IF(M > 15_INT64) M = M - 6_INT64
        !
        IF(M > ZER) HEX%NUM = HEX%NUM + M*base
        !
        base = SHIFTL(base, 4)  ! Multiply by 2^4
        !
    END DO
    !
  END SUBROUTINE
  !
  PURE SUBROUTINE INIT_HEXADECIMAL_I32(HEX, VAL) ! Hex range from 0 to 7FFFFFFFFFFFFFFF, or 0 to 9223372036854775807
    CLASS(HEXADECIMAL), INTENT(INOUT):: HEX
    INTEGER(INT32),     INTENT(IN   ):: VAL
    !
    IF( VAL > Z) THEN
                 HEX%NUM = INT(VAL, INT64)
    ELSE
                 HEX%NUM = ZER
    END IF
    !
  END SUBROUTINE
  !
  PURE SUBROUTINE INIT_HEXADECIMAL_I64(HEX, VAL) ! Hex range from 0 to 7FFFFFFFFFFFFFFF, or 0 to 9223372036854775807
    CLASS(HEXADECIMAL), INTENT(INOUT):: HEX
    INTEGER(INT64),     INTENT(IN   ):: VAL
    !
    IF( VAL > ZER) THEN
                   HEX%NUM = VAL
    ELSE
                   HEX%NUM = ZER
    END IF
    !
  END SUBROUTINE
  !
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !#####################################################################################################
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !
  PURE FUNCTION STR_HEXADECIMAL(HEX, WIDTH, PREFIX) RESULT(HEX_STR)
    CLASS(HEXADECIMAL),          INTENT(IN):: HEX
    INTEGER(INT32),    OPTIONAL, INTENT(IN):: WIDTH
    CHARACTER(*),      OPTIONAL, INTENT(IN):: PREFIX
    CHARACTER(:), ALLOCATABLE:: HEX_STR
    !
    CHARACTER(16):: TMP
    INTEGER:: I, N, M, WID
    INTEGER(INT64):: NUM
    !
    WID = 1
    IF(PRESENT(WIDTH )) THEN
            IF(WIDTH > WID) WID = WIDTH
    END IF
    !
    M = Z
    IF(PRESENT(PREFIX)) THEN
            IF(PREFIX /= "") M = LEN(PREFIX)
    END IF
    !
    IF(NUM < LONG) THEN
       IF(M > Z) THEN
           HEX_STR = PREFIX//REPEAT("0",WID)
       ELSE
           HEX_STR = REPEAT("0",WID)
       END IF
       RETURN
    END IF
    !
    NUM = HEX%NUM
    TMP = ""
    I = 16
    DO WHILE( NUM > Z .AND. I > Z)
        !
        N = INT( IAND(NUM, 15_INT64), INT32)     ! Get remainder of NUM/16
        !
        IF(N == Z) THEN
                     TMP(I:I) = "0"
        ELSE
                     TMP(I:I) = HEX_LIST(N:N)
        END IF
        !
        I = I - 1
        NUM = SHIFTR(NUM, 4)  ! Divide by 16 via bit shift operation
        !
    END DO
    !
    N  = 16 - I
    I = I + 1   !Number now stored in TMP(I:16)
    !
    IF( WID <= N ) THEN
                   WID = Z
    ELSE
                   WID = N - WID
    END IF
    !
    IF( WID > Z ) THEN
        IF( M > Z ) THEN
                    HEX_STR = PREFIX//REPEAT("0",WID)//TMP(I:16)
        ELSE
                    HEX_STR =         REPEAT("0",WID)//TMP(I:16)
        END IF
    ELSE
        IF( M > Z ) THEN
                    HEX_STR = PREFIX//TMP(I:16)
        ELSE
                    HEX_STR = TMP(I:16)
        END IF
    END IF
    !
  END FUNCTION
  !
  PURE FUNCTION UNICODE_HEXADECIMAL(HEX) RESULT(HEX_STR)
    CLASS(HEXADECIMAL), INTENT(IN):: HEX
    CHARACTER(:), ALLOCATABLE:: HEX_STR
    !
    IF(HEX%NUM < 65536_INT64) THEN
                              HEX_STR = STR_HEXADECIMAL(HEX, 4, 'U+') 
    ELSE    
                              HEX_STR = STR_HEXADECIMAL(HEX, 5, 'U+') 
    END IF
    !
  END FUNCTION
  !
  PURE FUNCTION HEX_HEXADECIMAL(HEX) RESULT(HEX_STR)
    CLASS(HEXADECIMAL), INTENT(IN):: HEX
    CHARACTER(:), ALLOCATABLE:: HEX_STR
    !
    HEX_STR = STR_HEXADECIMAL(HEX, 0, '0x') 
    !
  END FUNCTION
  !
  PURE FUNCTION INT32_HEXADECIMAL(HEX) RESULT(NUM)
    CLASS(HEXADECIMAL), INTENT(IN):: HEX
    INTEGER(INT32):: NUM
    !
    IF(HEX%NUM < inf32) THEN
          NUM = HEX%NUM
    ELSE
          NUM = inf32
    END IF
    !
  END FUNCTION
  !
  PURE FUNCTION INT64_HEXADECIMAL(HEX) RESULT(NUM)
    CLASS(HEXADECIMAL), INTENT(IN):: HEX
    INTEGER(INT64):: NUM
    !
    NUM = HEX%NUM
    !
  END FUNCTION
  !
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !#####################################################################################################
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !                                     !(OUT, IN)
  ELEMENTAL PURE SUBROUTINE EQUAL_HEX_HEX(HEX, HEX_IN)
    CLASS(HEXADECIMAL), INTENT(IN   ):: HEX_IN
    CLASS(HEXADECIMAL), INTENT(INOUT):: HEX
    !
    HEX%NUM = HEX_IN%NUM
    !
  END SUBROUTINE
  !                                     !(OUT, IN)
  ELEMENTAL PURE SUBROUTINE EQUAL_HEX_I32(HEX, VAL)
    INTEGER(INT32),     INTENT(IN   ):: VAL
    CLASS(HEXADECIMAL), INTENT(INOUT):: HEX
    !
    CALL INIT_HEXADECIMAL_I32(HEX, VAL)
    !
  END SUBROUTINE
  !                                     !(OUT, IN)
  ELEMENTAL PURE SUBROUTINE EQUAL_HEX_I64(HEX, VAL)
    INTEGER(INT64),     INTENT(IN   ):: VAL
    CLASS(HEXADECIMAL), INTENT(INOUT):: HEX
    !
    CALL INIT_HEXADECIMAL_I64(HEX, VAL)
    !
  END SUBROUTINE
  !                                     !(OUT, IN)
  ELEMENTAL PURE SUBROUTINE EQUAL_I32_HEX(VAL, HEX)
    INTEGER(INT32),     INTENT(INOUT):: VAL
    CLASS(HEXADECIMAL), INTENT(IN   ):: HEX
    !
    IF(HEX%NUM < inf32) THEN
          VAL = HEX%NUM
    ELSE
          VAL = inf32
    END IF
    !
  END SUBROUTINE
  !                                     !(OUT, IN)
  ELEMENTAL PURE SUBROUTINE EQUAL_I64_HEX(VAL, HEX)
    INTEGER(INT64),     INTENT(INOUT):: VAL
    CLASS(HEXADECIMAL), INTENT(IN   ):: HEX
    !
    VAL = HEX%NUM
    !
  END SUBROUTINE
  !
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !#####################################################################################################
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !
  ELEMENTAL PURE FUNCTION ADD_HEX_HEX(HEX, HEX2) RESULT(NUM)
    CLASS(HEXADECIMAL), INTENT(IN):: HEX
    CLASS(HEXADECIMAL), INTENT(IN):: HEX2
    INTEGER(INT64):: NUM
    !
    NUM = HEX%NUM + HEX2%NUM
    !
  END FUNCTION
  !
  ELEMENTAL PURE FUNCTION ADD_HEX_I32(HEX, VAL) RESULT(NUM)
    CLASS(HEXADECIMAL), INTENT(IN):: HEX
    INTEGER(INT32),     INTENT(IN):: VAL
    INTEGER(INT64):: NUM
    !
    NUM = HEX%NUM + INT(VAL, INT64)
    !
  END FUNCTION
  !
  ELEMENTAL PURE FUNCTION ADD_HEX_I64(HEX, VAL) RESULT(NUM)
    CLASS(HEXADECIMAL), INTENT(IN):: HEX
    INTEGER(INT64),     INTENT(IN):: VAL
    INTEGER(INT64):: NUM
    !
    NUM = HEX%NUM + VAL
    !
  END FUNCTION
  !
  ELEMENTAL PURE FUNCTION ADD_I32_HEX(VAL, HEX) RESULT(NUM)
    CLASS(HEXADECIMAL), INTENT(IN):: HEX
    INTEGER(INT32),     INTENT(IN):: VAL
    INTEGER(INT64):: NUM
    !
    NUM = HEX%NUM + INT(VAL, INT64)
    !
  END FUNCTION
  !
  ELEMENTAL PURE FUNCTION ADD_I64_HEX(VAL, HEX) RESULT(NUM)
    CLASS(HEXADECIMAL), INTENT(IN):: HEX
    INTEGER(INT64),     INTENT(IN):: VAL
    INTEGER(INT64):: NUM
    !
    NUM = HEX%NUM + VAL
    !
  END FUNCTION
  !
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !#####################################################################################################
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !
  ELEMENTAL PURE FUNCTION SUB_HEX_HEX(HEX, HEX2) RESULT(NUM)
    CLASS(HEXADECIMAL), INTENT(IN):: HEX
    CLASS(HEXADECIMAL), INTENT(IN):: HEX2
    INTEGER(INT64):: NUM
    !
    NUM = HEX%NUM - HEX2%NUM
    !
  END FUNCTION
  !
  ELEMENTAL PURE FUNCTION SUB_HEX_I32(HEX, VAL) RESULT(NUM)
    CLASS(HEXADECIMAL), INTENT(IN):: HEX
    INTEGER(INT32),     INTENT(IN):: VAL
    INTEGER(INT64):: NUM
    !
    NUM = HEX%NUM - INT(VAL, INT64)
    !
  END FUNCTION
  !
  ELEMENTAL PURE FUNCTION SUB_HEX_I64(HEX, VAL) RESULT(NUM)
    CLASS(HEXADECIMAL), INTENT(IN):: HEX
    INTEGER(INT64),     INTENT(IN):: VAL
    INTEGER(INT64):: NUM
    !
    NUM = HEX%NUM - VAL
    !
  END FUNCTION
  !
  ELEMENTAL PURE FUNCTION SUB_I32_HEX(VAL, HEX) RESULT(NUM)
    CLASS(HEXADECIMAL), INTENT(IN):: HEX
    INTEGER(INT32),     INTENT(IN):: VAL
    INTEGER(INT64):: NUM
    !
    NUM = INT(VAL, INT64) - HEX%NUM
    !
  END FUNCTION
  !
  ELEMENTAL PURE FUNCTION SUB_I64_HEX(VAL, HEX) RESULT(NUM)
    CLASS(HEXADECIMAL), INTENT(IN):: HEX
    INTEGER(INT64),     INTENT(IN):: VAL
    INTEGER(INT64):: NUM
    !
    NUM = VAL - HEX%NUM
    !
  END FUNCTION
  !
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !#####################################################################################################
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !
  ELEMENTAL PURE FUNCTION MLT_HEX_HEX(HEX, HEX2) RESULT(NUM)
    CLASS(HEXADECIMAL), INTENT(IN):: HEX
    CLASS(HEXADECIMAL), INTENT(IN):: HEX2
    INTEGER(INT64):: NUM
    !
    NUM = HEX%NUM * HEX2%NUM
    !
  END FUNCTION
  !
  ELEMENTAL PURE FUNCTION MLT_HEX_I32(HEX, VAL) RESULT(NUM)
    CLASS(HEXADECIMAL), INTENT(IN):: HEX
    INTEGER(INT32),     INTENT(IN):: VAL
    INTEGER(INT64):: NUM
    !
    NUM = HEX%NUM * INT(VAL, INT64)
    !
  END FUNCTION
  !
  ELEMENTAL PURE FUNCTION MLT_HEX_I64(HEX, VAL) RESULT(NUM)
    CLASS(HEXADECIMAL), INTENT(IN):: HEX
    INTEGER(INT64),     INTENT(IN):: VAL
    INTEGER(INT64):: NUM
    !
    NUM = HEX%NUM * VAL
    !
  END FUNCTION
  !
  ELEMENTAL PURE FUNCTION MLT_I32_HEX(VAL, HEX) RESULT(NUM)
    CLASS(HEXADECIMAL), INTENT(IN):: HEX
    INTEGER(INT32),     INTENT(IN):: VAL
    INTEGER(INT64):: NUM
    !
    NUM = INT(VAL, INT64) * HEX%NUM
    !
  END FUNCTION
  !
  ELEMENTAL PURE FUNCTION MLT_I64_HEX(VAL, HEX) RESULT(NUM)
    CLASS(HEXADECIMAL), INTENT(IN):: HEX
    INTEGER(INT64),     INTENT(IN):: VAL
    INTEGER(INT64):: NUM
    !
    NUM = VAL * HEX%NUM
    !
  END FUNCTION
  !
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !#####################################################################################################
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !
  ELEMENTAL PURE FUNCTION DIV_HEX_HEX(HEX, HEX2) RESULT(NUM)
    CLASS(HEXADECIMAL), INTENT(IN):: HEX
    CLASS(HEXADECIMAL), INTENT(IN):: HEX2
    INTEGER(INT64):: NUM
    !
    IF(HEX2%NUM > ZER) THEN
            NUM = HEX%NUM / HEX2%NUM
    ELSE
            NUM = ZER
    END IF
    !
  END FUNCTION
  !
  ELEMENTAL PURE FUNCTION DIV_HEX_I32(HEX, VAL) RESULT(NUM)
    CLASS(HEXADECIMAL), INTENT(IN):: HEX
    INTEGER(INT32),     INTENT(IN):: VAL
    INTEGER(INT64):: NUM
    !
    IF(VAL > Z) THEN
           NUM = HEX%NUM / INT(VAL, INT64)
    ELSE
           NUM = ZER
    END IF
    !
  END FUNCTION
  !
  ELEMENTAL PURE FUNCTION DIV_HEX_I64(HEX, VAL) RESULT(NUM)
    CLASS(HEXADECIMAL), INTENT(IN):: HEX
    INTEGER(INT64),     INTENT(IN):: VAL
    INTEGER(INT64):: NUM
    !
    IF(VAL > ZER) THEN
             NUM = HEX%NUM / VAL
    ELSE
             NUM = ZER
    END IF
    !
  END FUNCTION
  !
  ELEMENTAL PURE FUNCTION DIV_I32_HEX(VAL, HEX) RESULT(NUM)
    CLASS(HEXADECIMAL), INTENT(IN):: HEX
    INTEGER(INT32),     INTENT(IN):: VAL
    INTEGER(INT64):: NUM
    !
    IF(HEX%NUM > ZER) THEN
           NUM = INT(VAL, INT64) / HEX%NUM
    ELSE
           NUM = ZER
    END IF
    !
  END FUNCTION
  !
  ELEMENTAL PURE FUNCTION DIV_I64_HEX(VAL, HEX) RESULT(NUM)
    CLASS(HEXADECIMAL), INTENT(IN):: HEX
    INTEGER(INT64),     INTENT(IN):: VAL
    INTEGER(INT64):: NUM
    !
    IF(HEX%NUM > ZER) THEN
           NUM = VAL / HEX%NUM
    ELSE
           NUM = ZER
    END IF
    !
  END FUNCTION
  !
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !#####################################################################################################
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !
  ELEMENTAL PURE FUNCTION EQ_HEX_HEX(HEX, HEX2) RESULT(ANS)
    CLASS(HEXADECIMAL), INTENT(IN):: HEX
    CLASS(HEXADECIMAL), INTENT(IN):: HEX2
    LOGICAL:: ANS
    !
    ANS = HEX%NUM == HEX2%NUM
    !
  END FUNCTION
  !
  ELEMENTAL PURE FUNCTION EQ_HEX_I32(HEX, VAL) RESULT(ANS)
    CLASS(HEXADECIMAL), INTENT(IN):: HEX
    INTEGER(INT32),     INTENT(IN):: VAL
    LOGICAL:: ANS
    !
    ANS = HEX%NUM == INT(VAL, INT64)
    !
  END FUNCTION
  !
  ELEMENTAL PURE FUNCTION EQ_HEX_I64(HEX, VAL) RESULT(ANS)
    CLASS(HEXADECIMAL), INTENT(IN):: HEX
    INTEGER(INT64),     INTENT(IN):: VAL
    LOGICAL:: ANS
    !
    ANS = HEX%NUM == VAL
    !
  END FUNCTION
  !
  ELEMENTAL PURE FUNCTION EQ_I32_HEX(VAL, HEX) RESULT(ANS)
    CLASS(HEXADECIMAL), INTENT(IN):: HEX
    INTEGER(INT32),     INTENT(IN):: VAL
    LOGICAL:: ANS
    !
    ANS = HEX%NUM == INT(VAL, INT64)
    !
  END FUNCTION
  !
  ELEMENTAL PURE FUNCTION EQ_I64_HEX(VAL, HEX) RESULT(ANS)
    CLASS(HEXADECIMAL), INTENT(IN):: HEX
    INTEGER(INT64),     INTENT(IN):: VAL
    LOGICAL:: ANS
    !
    ANS = HEX%NUM == VAL
    !
  END FUNCTION
  !
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !#####################################################################################################
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !
  ELEMENTAL PURE FUNCTION LT_HEX_HEX(HEX, HEX2) RESULT(ANS)
    CLASS(HEXADECIMAL), INTENT(IN):: HEX
    CLASS(HEXADECIMAL), INTENT(IN):: HEX2
    LOGICAL:: ANS
    !
    ANS = HEX%NUM < HEX2%NUM
    !
  END FUNCTION
  !
  ELEMENTAL PURE FUNCTION LT_HEX_I32(HEX, VAL) RESULT(ANS)
    CLASS(HEXADECIMAL), INTENT(IN):: HEX
    INTEGER(INT32),     INTENT(IN):: VAL
    LOGICAL:: ANS
    !
    ANS = HEX%NUM < INT(VAL, INT64)
    !
  END FUNCTION
  !
  ELEMENTAL PURE FUNCTION LT_HEX_I64(HEX, VAL) RESULT(ANS)
    CLASS(HEXADECIMAL), INTENT(IN):: HEX
    INTEGER(INT64),     INTENT(IN):: VAL
    LOGICAL:: ANS
    !
    ANS = HEX%NUM < VAL
    !
  END FUNCTION
  !
  ELEMENTAL PURE FUNCTION LT_I32_HEX(VAL, HEX) RESULT(ANS)
    CLASS(HEXADECIMAL), INTENT(IN):: HEX
    INTEGER(INT32),     INTENT(IN):: VAL
    LOGICAL:: ANS
    !
    ANS = INT(VAL, INT64) < HEX%NUM
    !
  END FUNCTION
  !
  ELEMENTAL PURE FUNCTION LT_I64_HEX(VAL, HEX) RESULT(ANS)
    CLASS(HEXADECIMAL), INTENT(IN):: HEX
    INTEGER(INT64),     INTENT(IN):: VAL
    LOGICAL:: ANS
    !
    ANS = VAL < HEX%NUM
    !
  END FUNCTION
  !
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !#####################################################################################################
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !
  ELEMENTAL PURE FUNCTION LE_HEX_HEX(HEX, HEX2) RESULT(ANS)
    CLASS(HEXADECIMAL), INTENT(IN):: HEX
    CLASS(HEXADECIMAL), INTENT(IN):: HEX2
    LOGICAL:: ANS
    !
    ANS = HEX%NUM <= HEX2%NUM
    !
  END FUNCTION
  !
  ELEMENTAL PURE FUNCTION LE_HEX_I32(HEX, VAL) RESULT(ANS)
    CLASS(HEXADECIMAL), INTENT(IN):: HEX
    INTEGER(INT32),     INTENT(IN):: VAL
    LOGICAL:: ANS
    !
    ANS = HEX%NUM <= INT(VAL, INT64)
    !
  END FUNCTION
  !
  ELEMENTAL PURE FUNCTION LE_HEX_I64(HEX, VAL) RESULT(ANS)
    CLASS(HEXADECIMAL), INTENT(IN):: HEX
    INTEGER(INT64),     INTENT(IN):: VAL
    LOGICAL:: ANS
    !
    ANS = HEX%NUM <= VAL
    !
  END FUNCTION
  !
  ELEMENTAL PURE FUNCTION LE_I32_HEX(VAL, HEX) RESULT(ANS)
    CLASS(HEXADECIMAL), INTENT(IN):: HEX
    INTEGER(INT32),     INTENT(IN):: VAL
    LOGICAL:: ANS
    !
    ANS = INT(VAL, INT64) <= HEX%NUM
    !
  END FUNCTION
  !
  ELEMENTAL PURE FUNCTION LE_I64_HEX(VAL, HEX) RESULT(ANS)
    CLASS(HEXADECIMAL), INTENT(IN):: HEX
    INTEGER(INT64),     INTENT(IN):: VAL
    LOGICAL:: ANS
    !
    ANS = VAL <= HEX%NUM
    !
  END FUNCTION
  !
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !#####################################################################################################
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !
  ELEMENTAL PURE FUNCTION GT_HEX_HEX(HEX, HEX2) RESULT(ANS)
    CLASS(HEXADECIMAL), INTENT(IN):: HEX
    CLASS(HEXADECIMAL), INTENT(IN):: HEX2
    LOGICAL:: ANS
    !
    ANS = HEX%NUM > HEX2%NUM
    !
  END FUNCTION
  !
  ELEMENTAL PURE FUNCTION GT_HEX_I32(HEX, VAL) RESULT(ANS)
    CLASS(HEXADECIMAL), INTENT(IN):: HEX
    INTEGER(INT32),     INTENT(IN):: VAL
    LOGICAL:: ANS
    !
    ANS = HEX%NUM > INT(VAL, INT64)
    !
  END FUNCTION
  !
  ELEMENTAL PURE FUNCTION GT_HEX_I64(HEX, VAL) RESULT(ANS)
    CLASS(HEXADECIMAL), INTENT(IN):: HEX
    INTEGER(INT64),     INTENT(IN):: VAL
    LOGICAL:: ANS
    !
    ANS = HEX%NUM > VAL
    !
  END FUNCTION
  !
  ELEMENTAL PURE FUNCTION GT_I32_HEX(VAL, HEX) RESULT(ANS)
    CLASS(HEXADECIMAL), INTENT(IN):: HEX
    INTEGER(INT32),     INTENT(IN):: VAL
    LOGICAL:: ANS
    !
    ANS = INT(VAL, INT64) > HEX%NUM
    !
  END FUNCTION
  !
  ELEMENTAL PURE FUNCTION GT_I64_HEX(VAL, HEX) RESULT(ANS)
    CLASS(HEXADECIMAL), INTENT(IN):: HEX
    INTEGER(INT64),     INTENT(IN):: VAL
    LOGICAL:: ANS
    !
    ANS = VAL > HEX%NUM
    !
  END FUNCTION
  !
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !#####################################################################################################
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !
  ELEMENTAL PURE FUNCTION GE_HEX_HEX(HEX, HEX2) RESULT(ANS)
    CLASS(HEXADECIMAL), INTENT(IN):: HEX
    CLASS(HEXADECIMAL), INTENT(IN):: HEX2
    LOGICAL:: ANS
    !
    ANS = HEX%NUM >= HEX2%NUM
    !
  END FUNCTION
  !
  ELEMENTAL PURE FUNCTION GE_HEX_I32(HEX, VAL) RESULT(ANS)
    CLASS(HEXADECIMAL), INTENT(IN):: HEX
    INTEGER(INT32),     INTENT(IN):: VAL
    LOGICAL:: ANS
    !
    ANS = HEX%NUM >= INT(VAL, INT64)
    !
  END FUNCTION
  !
  ELEMENTAL PURE FUNCTION GE_HEX_I64(HEX, VAL) RESULT(ANS)
    CLASS(HEXADECIMAL), INTENT(IN):: HEX
    INTEGER(INT64),     INTENT(IN):: VAL
    LOGICAL:: ANS
    !
    ANS = HEX%NUM >= VAL
    !
  END FUNCTION
  !
  ELEMENTAL PURE FUNCTION GE_I32_HEX(VAL, HEX) RESULT(ANS)
    CLASS(HEXADECIMAL), INTENT(IN):: HEX
    INTEGER(INT32),     INTENT(IN):: VAL
    LOGICAL:: ANS
    !
    ANS = INT(VAL, INT64) >= HEX%NUM
    !
  END FUNCTION
  !
  ELEMENTAL PURE FUNCTION GE_I64_HEX(VAL, HEX) RESULT(ANS)
    CLASS(HEXADECIMAL), INTENT(IN):: HEX
    INTEGER(INT64),     INTENT(IN):: VAL
    LOGICAL:: ANS
    !
    ANS = VAL >= HEX%NUM
    !
  END FUNCTION
  !
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !#####################################################################################################
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !
  SUBROUTINE FMTREAD_HEXADECIMAL(HEX, UNIT, IOTYPE, V_LIST, IOSTAT, IOMSG)
    CLASS(HEXADECIMAL), INTENT(INOUT):: HEX
    INTEGER,            INTENT(IN   ):: UNIT
    CHARACTER(*),       INTENT(IN   ):: IOTYPE
    INTEGER,            INTENT(IN   ):: V_LIST (:)
    INTEGER,            INTENT(OUT  ):: IOSTAT
    CHARACTER(*),       INTENT(INOUT):: IOMSG
    CHARACTER(16) :: TMP
    !
    ! This is the child I/O that gets performed when the procedure
    ! is called from a parent I/O - it uses list-directed input to read
    ! the array K
    !
    TMP = ""
    READ (UNIT, FMT='(A)', IOSTAT=IOSTAT, IOMSG=IOMSG) TMP
    !
    IF(IOSTAT ==Z)  THEN
                    CALL INIT_HEXADECIMAL_STR(HEX, TMP)
    ELSE
                    HEX%NUM = ZER
    END IF
    !
  END SUBROUTINE  
  !
  SUBROUTINE FMTWRITE_HEXADECIMAL(HEX, UNIT, IOTYPE, V_LIST, IOSTAT, IOMSG)  !* or '(DT"IOTYPE")' or '(DT)'
    CLASS(HEXADECIMAL), INTENT(IN   ):: HEX
    INTEGER,            INTENT(IN   ):: UNIT
    CHARACTER(*),       INTENT(IN   ):: IOTYPE
    INTEGER,            INTENT(IN   ):: V_LIST (:)
    INTEGER,            INTENT(OUT  ):: IOSTAT
    CHARACTER(*),       INTENT(INOUT):: IOMSG
    !
    ! This is the child I/O that gets performed when the procedure
    ! is called from a parent I/O - it uses list-directed input to read
    ! the array K
    !
    WRITE(UNIT, FMT='(A)', IOSTAT=IOSTAT, IOMSG=IOMSG) HEX_HEXADECIMAL(HEX)
    !
  END SUBROUTINE 
  !
END MODULE
  !
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !#####################################################################################################
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !
  !!!PURE FUNCTION toINT64(STR) RESULT(NUM) ! Hex range from 0 to 7FFFFFFFFFFFFFFF, or 0 to 9223372036854775807
  !!!  CHARACTER(*), INTENT(IN   ):: STR
  !!!  INTEGER(INT64):: NUM
  !!!  INTEGER(INT64):: base, M
  !!!  INTEGER(INT32):: I, J, K
  !!!  !
  !!!  NUM = ZER
  !!!  !
  !!!  J = 1
  !!!  K = LEN_TRIM(STR)
  !!!  !
  !!!  DO WHILE (J <= K)
  !!!     IF( INDEX(HEX_LIST, STR(J:J)) > 0 ) EXIT 
  !!!     J = J + 1
  !!!  END DO
  !!!  !
  !!!  base = LONG   ! powers of 16
  !!!  !
  !!!  DO I=K, J, -1_INT32
  !!!      !
  !!!      IF(STR(I:I) == " ") CYCLE
  !!!      !
  !!!      M = INDEX(HEX_LIST, STR(I:I), KIND=INT64)
  !!!      !
  !!!      IF(M > 15_INT64) M = M - 6_INT64
  !!!      !
  !!!      IF(M > ZER) NUM = NUM + M*base
  !!!      !
  !!!      base = SHIFTL(base, 4)  ! Multiply by 2^4
  !!!      !
  !!!  END DO
  !!!  !
  !!!END FUNCTION
  !!!!
  !!!PURE FUNCTION toHEX(NUM64) RESULT(STR)
  !!!  INTEGER(INT64), INTENT(IN):: NUM64
  !!!  CHARACTER(:),  ALLOCATABLE:: STR
  !!!  CHARACTER(16):: TMP
  !!!  INTEGER(INT32):: I, N
  !!!  INTEGER(INT64):: NUM
  !!!  !
  !!!  IF(NUM64 < LONG) THEN
  !!!                 STR = "0"
  !!!                 RETURN
  !!!  END IF
  !!!  !
  !!!  NUM = NUM64
  !!!  TMP = ""
  !!!  I = 16
  !!!  DO WHILE( NUM > Z .AND. I > Z)
  !!!      !
  !!!      N = INT( IAND(NUM, 15_INT64), INT32)     ! Get remainder of NUM/16
  !!!      !
  !!!      IF(N == Z) THEN
  !!!                   TMP(I:I) = "0"
  !!!      ELSE
  !!!                   TMP(I:I) = HEX_LIST(N:N)
  !!!      END IF
  !!!      !
  !!!      I = I - 1
  !!!      NUM = SHIFTR(NUM, 4)  ! Divide by 16 via bit shift operation
  !!!      !
  !!!  END DO
  !!!  !
  !!!  STR = TMP(I+1:16)
  !!!  !
  !!!END FUNCTION
  !
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !#####################################################################################################
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ! 