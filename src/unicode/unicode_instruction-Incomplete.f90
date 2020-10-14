! Code incomplete - Do NOT use
    
 MODULE UNICODE_INSTRUCTION
  USE, INTRINSIC:: ISO_FORTRAN_ENV, ONLY: INT32, INT64
  USE UNICODE_INTERFACE
  IMPLICIT NONE
  !
  PUBLIC:: UTF8
  !
  PRIVATE
  !
  INTEGER(INT32), PARAMETER :: initialSize = 128_INT32
  INTEGER(INT32), PARAMETER :: Z   = 0_INT32
  INTEGER(INT32), PARAMETER :: ONE = 1_INT32
  !
  CHARACTER(21),  PARAMETER:: HEX_LIST    = '123456789ABCDEFabcdef'
  !
  TYPE UTF8
     !
     INTEGER(INT32):: N   = Z
     INTEGER(INT32):: DIM = Z
     INTEGER(INT32), DIMENSION(:), ALLOCATABLE:: NUM
     !
     CONTAINS
     !
     GENERIC::              INIT    => RESET_UTF8, ADD_UTF8_I32, ADD_UTF8_I32_VEC
     GENERIC::              RESET   => RESET_UTF8
     !
     PROCEDURE, PASS(UTF), PRIVATE:: RESET_UTF8
     PROCEDURE, PASS(UTF), PRIVATE:: ADD_UTF8_I32
     PROCEDURE, PASS(UTF), PRIVATE:: ADD_UTF8_I32_VEC
     !
     !!!GENERIC::              INIT    => INIT_UTF8_STR, & ! CALL HEX%INIT(STR)
     !!!                                  INIT_UTF8_I32, & ! CALL HEX%INIT(NUM)
     !!!                                  INIT_UTF8_I64
     !!!PROCEDURE, PASS(HEX):: STR     =>     STR_UTF8     ! HEX_STR = HEX%STR([WIDTH], [PREFIX])
     !!!PROCEDURE, PASS(HEX):: HEX     =>     HEX_UTF8     ! HEX_STR = HEX%HEX()     -> Get 0xF  --> same as HEX%STR(0, '0x')
     !!!PROCEDURE, PASS(HEX):: UNICODE => UNICODE_UTF8     ! HEX_STR = HEX%UNICODE() -> Get U+0000
     !!!PROCEDURE, PASS(HEX):: INT32   =>   INT32_UTF8     ! INT32   = HEX%INT32()
     !!!PROCEDURE, PASS(HEX):: INT64   =>   INT64_UTF8     ! INT64   = HEX%INT64()
     !!!!
     !!!GENERIC:: ASSIGNMENT(= ) => EQUAL_HEX_HEX, EQUAL_HEX_I32, EQUAL_HEX_I64, EQUAL_I32_HEX, EQUAL_I64_HEX
     !!!!
     !!!GENERIC:: OPERATOR  (+ ) =>   ADD_HEX_HEX,   ADD_HEX_I32,   ADD_HEX_I64,   ADD_I32_HEX,   ADD_I64_HEX
     !!!GENERIC:: OPERATOR  (- ) =>   SUB_HEX_HEX,   SUB_HEX_I32,   SUB_HEX_I64,   SUB_I32_HEX,   SUB_I64_HEX
     !!!GENERIC:: OPERATOR  (* ) =>   MLT_HEX_HEX,   MLT_HEX_I32,   MLT_HEX_I64,   MLT_I32_HEX,   MLT_I64_HEX
     !!!GENERIC:: OPERATOR  (/ ) =>   DIV_HEX_HEX,   DIV_HEX_I32,   DIV_HEX_I64,   DIV_I32_HEX,   DIV_I64_HEX
     !!!!
     !!!GENERIC:: OPERATOR  (==) =>    EQ_HEX_HEX,    EQ_HEX_I32,    EQ_HEX_I64,    EQ_I32_HEX,    EQ_I64_HEX
     !!!GENERIC:: OPERATOR  (< ) =>    LT_HEX_HEX,    LT_HEX_I32,    LT_HEX_I64,    LT_I32_HEX,    LT_I64_HEX
     !!!GENERIC:: OPERATOR  (<=) =>    LE_HEX_HEX,    LE_HEX_I32,    LE_HEX_I64,    LE_I32_HEX,    LE_I64_HEX
     !!!GENERIC:: OPERATOR  (> ) =>    GT_HEX_HEX,    GT_HEX_I32,    GT_HEX_I64,    GT_I32_HEX,    GT_I64_HEX
     !!!GENERIC:: OPERATOR  (>=) =>    GE_HEX_HEX,    GE_HEX_I32,    GE_HEX_I64,    GE_I32_HEX,    GE_I64_HEX
     !!!!
     !!!GENERIC:: READ (FORMATTED) => FMTREAD_UTF8
     !!!GENERIC:: WRITE(FORMATTED) => FMTWRITE_UTF8
     !!!!
     !!!PROCEDURE, PASS(HEX), PRIVATE:: INIT_UTF8_STR, INIT_UTF8_I32, INIT_UTF8_I64
     !!!!
     !!!PROCEDURE, PASS(HEX), PRIVATE:: EQUAL_HEX_HEX, EQUAL_HEX_I32, EQUAL_HEX_I64, EQUAL_I32_HEX, EQUAL_I64_HEX
     !!!!
     !!!PROCEDURE, PASS(HEX), PRIVATE:: ADD_HEX_HEX, ADD_HEX_I32, ADD_HEX_I64, ADD_I32_HEX, ADD_I64_HEX
     !!!PROCEDURE, PASS(HEX), PRIVATE:: SUB_HEX_HEX, SUB_HEX_I32, SUB_HEX_I64, SUB_I32_HEX, SUB_I64_HEX
     !!!PROCEDURE, PASS(HEX), PRIVATE:: MLT_HEX_HEX, MLT_HEX_I32, MLT_HEX_I64, MLT_I32_HEX, MLT_I64_HEX
     !!!PROCEDURE, PASS(HEX), PRIVATE:: DIV_HEX_HEX, DIV_HEX_I32, DIV_HEX_I64, DIV_I32_HEX, DIV_I64_HEX
     !!!!
     !!!PROCEDURE, PASS(HEX), PRIVATE:: EQ_HEX_HEX, EQ_HEX_I32, EQ_HEX_I64, EQ_I32_HEX, EQ_I64_HEX
     !!!!
     !!!PROCEDURE, PASS(HEX), PRIVATE:: LT_HEX_HEX, LT_HEX_I32, LT_HEX_I64, LT_I32_HEX, LT_I64_HEX
     !!!PROCEDURE, PASS(HEX), PRIVATE:: LE_HEX_HEX, LE_HEX_I32, LE_HEX_I64, LE_I32_HEX, LE_I64_HEX
     !!!PROCEDURE, PASS(HEX), PRIVATE:: GT_HEX_HEX, GT_HEX_I32, GT_HEX_I64, GT_I32_HEX, GT_I64_HEX
     !!!PROCEDURE, PASS(HEX), PRIVATE:: GE_HEX_HEX, GE_HEX_I32, GE_HEX_I64, GE_I32_HEX, GE_I64_HEX
     !!!!
     !!!PROCEDURE, PASS(HEX), PRIVATE:: FMTREAD_UTF8
     !!!PROCEDURE, PASS(HEX), PRIVATE:: FMTWRITE_UTF8
     !
  END TYPE
  !
  CONTAINS !#####################################################################################################
  !
  PURE SUBROUTINE RESET_UTF8(UTF) 
    CLASS(UTF8),    INTENT(INOUT):: UTF
    !
    UTF%N = Z
    IF( UTF%DIM < ONE) CALL INCREASE_BUF_UTF8(UTF)
    !
  END SUBROUTINE
  !
  !--------------------------------------------------------------------------------------------------------------
  !
  PURE SUBROUTINE ADD_UTF8_I32(UTF, VAL) 
    CLASS(UTF8),    INTENT(INOUT):: UTF
    INTEGER(INT32), INTENT(IN   ):: VAL
    !
    IF( UTF%DIM < ONE .OR. UTF%N + ONE < UTF%DIM) CALL INCREASE_BUF_UTF8(UTF)
    !
    UTF%N = UTF%N + ONE
    !
    UTF%NUM(UTF%N) = VAL
    !
    !
  END SUBROUTINE
  !
  !--------------------------------------------------------------------------------------------------------------
  !
  PURE SUBROUTINE ADD_UTF8_HEX(UTF, TXT) 
    CLASS(UTF8),                              INTENT(INOUT):: UTF
    CHARACTER(*),                 CONTIGUOUS, INTENT(IN   ):: TXT
    !
    CALL ADD_UTF8_I32 ( UTF, hex2int(TXT) ) 
    !
  END SUBROUTINE
  !
  !--------------------------------------------------------------------------------------------------------------
  !
  PURE SUBROUTINE ADD_UTF8_I32_VEC(UTF, VAL) 
    CLASS(UTF8),                              INTENT(INOUT):: UTF
    INTEGER(INT32), DIMENSION(:), CONTIGUOUS, INTENT(IN   ):: VAL
    INTEGER:: M
    !
    M = SIZE(VAL)
    !
    IF( UTF%DIM < ONE .OR. UTF%N + M < UTF%DIM) CALL INCREASE_BUF_UTF8(UTF)
    !
    UTF%NUM( UTF%N + 1 : UTF%N + M ) = VAL
    !
    UTF%N = UTF%N + M
    !
  END SUBROUTINE
  !
  !--------------------------------------------------------------------------------------------------------------
  !
  PURE SUBROUTINE ADD_UTF8_TXT(UTF, TXT) 
    CLASS(UTF8),                              INTENT(INOUT):: UTF
    CHARACTER(*),                 CONTIGUOUS, INTENT(IN   ):: TXT
    INTEGER:: I, J, IOSTAT
    INTEGER(INT32):: VAL
    !
    IF( UTF%DIM < ONE .OR. UTF%N + LEN(TXT) < UTF%DIM) CALL INCREASE_BUF_UTF8(UTF)
    !
    I = Z
    CALL NEXT_UTF8(LN, I, J, IOSTAT, VAL)
    !
    DO WHILE ( IOSTAT == Z )
                         UTF%N          = UTF%N + ONE
                         UTF%NUM(UTF%N) = VAL
                         CALL NEXT_UTF8(LN, I, J, IOSTAT, VAL)
    END DO
    !
  END SUBROUTINE
  !
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !#####################################################################################################
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !
  PURE SUBROUTINE INCREASE_BUF_UTF8(UTF)
    CLASS(UTF8), INTENT(INOUT):: UTF
    INTEGER:: siz
    !
    siz = UTF%DIM
    IF    (siz <  initialSize ) THEN
                                   siz = initialSize
    ELSEIF(siz <  1024 ) THEN
                              siz = 256*(siz/256) + 256
    ELSEIF(siz < 65536 ) THEN
                              siz = 2*siz
    ELSE
                              siz = 16384*(siz/16384) + 16384
    END IF
    !
    CALL GROW_UTF8(UTF, siz)
    !
  END SUBROUTINE 
  !
  !--------------------------------------------------------------------------------------------------------------
  !
  PURE SUBROUTINE GROW_UTF8(UTF, SIZ)
    CLASS(UTF8), INTENT(INOUT):: UTF
    INTEGER,     INTENT(IN   ):: SIZ
    INTEGER(INT32), DIMENSION(:), ALLOCATABLE:: TMP
    INTEGER:: I
    !
    IF( .NOT. ALLOCATED(UTF%NUM) ) THEN
                        UTF%N   = Z
                        UTF%DIM = MAX(initialSize, SIZ)
                        ALLOCATE( UTF%NUM(UTF%DIM) )
                        !
    ELSEIF( UTF%DIM < SIZ ) THEN
                        !
                        CALL MOVE_ALLOC(UTF%NUM, TMP)
                        UTF%DIM = SIZ
                        ALLOCATE( UTF%NUM(UTF%DIM) )
                        !
                        DO I=1, UTF%N
                                UTF%NUM(I) = TMP(I)
                        END DO
    END IF
    !
  END SUBROUTINE 
  !
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !#####################################################################################################
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !
  PURE FUNCTION hex2int(HEX) RESULT(VAL) ! Ignores blank spaces
    CHARACTER(*), INTENT(IN):: HEX
    INTEGER(INT32):: VAL
    INTEGER(INT32):: base, M
    INTEGER:: I, J, N
    !
    N    = LEN_TRIM(HEX)
    base = ONE   ! powers of 16
    VAL  = Z
    !
    J = 1
    DO WHILE (J <= N)
       IF( INDEX(HEX_LIST, HEX(J:J)) > Z ) EXIT 
       J = J + 1
    END DO
    !
    DO I=N, J, -1
        !
        IF(HEX(I:I) == " ") CYCLE
        !
        M = INDEX(HEX_LIST, HEX(I:I))
        !
        IF(M > 15_INT32) M = M - 6_INT32
        !
        IF(M > Z) VAL = VAL + M*base
        !
        base = SHIFTL(base, 4)  ! Multiply by 2^4
        !
    END DO
    !
  END FUNCTION
  !
  !--------------------------------------------------------------------------------------------------------------
  !
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
!!!  
!!!  
!!!  
!!!  !
!!!  PURE SUBROUTINE INIT_UTF8_STR(HEX, STR) 
!!!    CLASS(UTF8), INTENT(INOUT):: HEX
!!!    CHARACTER(*),       INTENT(IN   ):: STR
!!!    INTEGER(INT64):: NUM
!!!    INTEGER(INT64):: base, M
!!!    INTEGER(INT32):: I, J, K
!!!    !
!!!    HEX%NUM = ZER
!!!    !
!!!    J = 1
!!!    K = LEN_TRIM(STR)
!!!    !
!!!    DO WHILE (J <= K)
!!!       IF( INDEX(HEX_LIST, STR(J:J)) > 0 ) EXIT 
!!!       J = J + 1
!!!    END DO
!!!    !
!!!    base = LONG   ! powers of 16
!!!    !
!!!    DO I=K, J, -1_INT32
!!!        !
!!!        IF(STR(I:I) == " ") CYCLE
!!!        !
!!!        M = INDEX(HEX_LIST, STR(I:I), KIND=INT64)
!!!        !
!!!        IF(M > 15_INT64) M = M - 6_INT64
!!!        !
!!!        IF(M > ZER) HEX%NUM = HEX%NUM + M*base
!!!        !
!!!        base = SHIFTL(base, 4)  ! Multiply by 2^4
!!!        !
!!!    END DO
!!!    !
!!!  END SUBROUTINE
!!!  !
!!!  PURE SUBROUTINE INIT_UTF8_STR(HEX, STR) 
!!!    CLASS(UTF8), INTENT(INOUT):: HEX
!!!    CHARACTER(*),       INTENT(IN   ):: STR
!!!    INTEGER(INT64):: NUM
!!!    INTEGER(INT64):: base, M
!!!    INTEGER(INT32):: I, J, K
!!!    !
!!!    HEX%NUM = ZER
!!!    !
!!!    J = 1
!!!    K = LEN_TRIM(STR)
!!!    !
!!!    DO WHILE (J <= K)
!!!       IF( INDEX(HEX_LIST, STR(J:J)) > 0 ) EXIT 
!!!       J = J + 1
!!!    END DO
!!!    !
!!!    base = LONG   ! powers of 16
!!!    !
!!!    DO I=K, J, -1_INT32
!!!        !
!!!        IF(STR(I:I) == " ") CYCLE
!!!        !
!!!        M = INDEX(HEX_LIST, STR(I:I), KIND=INT64)
!!!        !
!!!        IF(M > 15_INT64) M = M - 6_INT64
!!!        !
!!!        IF(M > ZER) HEX%NUM = HEX%NUM + M*base
!!!        !
!!!        base = SHIFTL(base, 4)  ! Multiply by 2^4
!!!        !
!!!    END DO
!!!    !
!!!  END SUBROUTINE
!!!  !
!!!  PURE SUBROUTINE INIT_UTF8_I64(HEX, VAL) ! Hex range from 0 to 7FFFFFFFFFFFFFFF, or 0 to 9223372036854775807
!!!    CLASS(UTF8), INTENT(INOUT):: HEX
!!!    INTEGER(INT64),     INTENT(IN   ):: VAL
!!!    !
!!!    IF( VAL > ZER) THEN
!!!                   HEX%NUM = VAL
!!!    ELSE
!!!                   HEX%NUM = ZER
!!!    END IF
!!!    !
!!!  END SUBROUTINE
!!!  !
!!!  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!!!  !#####################################################################################################
!!!  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!!!  !
!!!  PURE FUNCTION STR_UTF8(HEX, WIDTH, PREFIX) RESULT(HEX_STR)
!!!    CLASS(UTF8),          INTENT(IN):: HEX
!!!    INTEGER(INT32),    OPTIONAL, INTENT(IN):: WIDTH
!!!    CHARACTER(*),      OPTIONAL, INTENT(IN):: PREFIX
!!!    CHARACTER(:), ALLOCATABLE:: HEX_STR
!!!    !
!!!    CHARACTER(16):: TMP
!!!    INTEGER:: I, N, M, WID
!!!    INTEGER(INT64):: NUM
!!!    !
!!!    WID = 1
!!!    IF(PRESENT(WIDTH )) THEN
!!!            IF(WIDTH > WID) WID = WIDTH
!!!    END IF
!!!    !
!!!    M = Z
!!!    IF(PRESENT(PREFIX)) THEN
!!!            IF(PREFIX /= "") M = LEN(PREFIX)
!!!    END IF
!!!    !
!!!    IF(NUM < LONG) THEN
!!!       IF(M > Z) THEN
!!!           HEX_STR = PREFIX//REPEAT("0",WID)
!!!       ELSE
!!!           HEX_STR = REPEAT("0",WID)
!!!       END IF
!!!       RETURN
!!!    END IF
!!!    !
!!!    NUM = HEX%NUM
!!!    TMP = ""
!!!    I = 16
!!!    DO WHILE( NUM > Z .AND. I > Z)
!!!        !
!!!        N = INT( IAND(NUM, 15_INT64), INT32)     ! Get remainder of NUM/16
!!!        !
!!!        IF(N == Z) THEN
!!!                     TMP(I:I) = "0"
!!!        ELSE
!!!                     TMP(I:I) = HEX_LIST(N:N)
!!!        END IF
!!!        !
!!!        I = I - 1
!!!        NUM = SHIFTR(NUM, 4)  ! Divide by 16 via bit shift operation
!!!        !
!!!    END DO
!!!    !
!!!    N  = 16 - I
!!!    I = I + 1   !Number now stored in TMP(I:16)
!!!    !
!!!    IF( WID <= N ) THEN
!!!                   WID = Z
!!!    ELSE
!!!                   WID = N - WID
!!!    END IF
!!!    !
!!!    IF( WID > Z ) THEN
!!!        IF( M > Z ) THEN
!!!                    HEX_STR = PREFIX//REPEAT("0",WID)//TMP(I:16)
!!!        ELSE
!!!                    HEX_STR =         REPEAT("0",WID)//TMP(I:16)
!!!        END IF
!!!    ELSE
!!!        IF( M > Z ) THEN
!!!                    HEX_STR = PREFIX//TMP(I:16)
!!!        ELSE
!!!                    HEX_STR = TMP(I:16)
!!!        END IF
!!!    END IF
!!!    !
!!!  END FUNCTION
!!!  !
!!!  PURE FUNCTION UNICODE_UTF8(HEX) RESULT(HEX_STR)
!!!    CLASS(UTF8), INTENT(IN):: HEX
!!!    CHARACTER(:), ALLOCATABLE:: HEX_STR
!!!    !
!!!    IF(HEX%NUM < 65536_INT64) THEN
!!!                              HEX_STR = STR_UTF8(HEX, 4, 'U+') 
!!!    ELSE    
!!!                              HEX_STR = STR_UTF8(HEX, 5, 'U+') 
!!!    END IF
!!!    !
!!!  END FUNCTION
!!!  !
!!!  PURE FUNCTION HEX_UTF8(HEX) RESULT(HEX_STR)
!!!    CLASS(UTF8), INTENT(IN):: HEX
!!!    CHARACTER(:), ALLOCATABLE:: HEX_STR
!!!    !
!!!    HEX_STR = STR_UTF8(HEX, 0, '0x') 
!!!    !
!!!  END FUNCTION
!!!  !
!!!  PURE FUNCTION INT32_UTF8(HEX) RESULT(NUM)
!!!    CLASS(UTF8), INTENT(IN):: HEX
!!!    INTEGER(INT32):: NUM
!!!    !
!!!    IF(HEX%NUM < inf32) THEN
!!!          NUM = HEX%NUM
!!!    ELSE
!!!          NUM = inf32
!!!    END IF
!!!    !
!!!  END FUNCTION
!!!  !
!!!  PURE FUNCTION INT64_UTF8(HEX) RESULT(NUM)
!!!    CLASS(UTF8), INTENT(IN):: HEX
!!!    INTEGER(INT64):: NUM
!!!    !
!!!    NUM = HEX%NUM
!!!    !
!!!  END FUNCTION
!!!  !
!!!  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!!!  !#####################################################################################################
!!!  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!!!  !                                     !(OUT, IN)
!!!  ELEMENTAL PURE SUBROUTINE EQUAL_HEX_HEX(HEX, HEX_IN)
!!!    CLASS(UTF8), INTENT(IN   ):: HEX_IN
!!!    CLASS(UTF8), INTENT(INOUT):: HEX
!!!    !
!!!    HEX%NUM = HEX_IN%NUM
!!!    !
!!!  END SUBROUTINE
!!!  !                                     !(OUT, IN)
!!!  ELEMENTAL PURE SUBROUTINE EQUAL_HEX_I32(HEX, VAL)
!!!    INTEGER(INT32),     INTENT(IN   ):: VAL
!!!    CLASS(UTF8), INTENT(INOUT):: HEX
!!!    !
!!!    CALL INIT_UTF8_I32(HEX, VAL)
!!!    !
!!!  END SUBROUTINE
!!!  !                                     !(OUT, IN)
!!!  ELEMENTAL PURE SUBROUTINE EQUAL_HEX_I64(HEX, VAL)
!!!    INTEGER(INT64),     INTENT(IN   ):: VAL
!!!    CLASS(UTF8), INTENT(INOUT):: HEX
!!!    !
!!!    CALL INIT_UTF8_I64(HEX, VAL)
!!!    !
!!!  END SUBROUTINE
!!!  !                                     !(OUT, IN)
!!!  ELEMENTAL PURE SUBROUTINE EQUAL_I32_HEX(VAL, HEX)
!!!    INTEGER(INT32),     INTENT(INOUT):: VAL
!!!    CLASS(UTF8), INTENT(IN   ):: HEX
!!!    !
!!!    IF(HEX%NUM < inf32) THEN
!!!          VAL = HEX%NUM
!!!    ELSE
!!!          VAL = inf32
!!!    END IF
!!!    !
!!!  END SUBROUTINE
!!!  !                                     !(OUT, IN)
!!!  ELEMENTAL PURE SUBROUTINE EQUAL_I64_HEX(VAL, HEX)
!!!    INTEGER(INT64),     INTENT(INOUT):: VAL
!!!    CLASS(UTF8), INTENT(IN   ):: HEX
!!!    !
!!!    VAL = HEX%NUM
!!!    !
!!!  END SUBROUTINE
!!!  !
!!!  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!!!  !#####################################################################################################
!!!  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!!!  !
!!!  ELEMENTAL PURE FUNCTION ADD_HEX_HEX(HEX, HEX2) RESULT(NUM)
!!!    CLASS(UTF8), INTENT(IN):: HEX
!!!    CLASS(UTF8), INTENT(IN):: HEX2
!!!    INTEGER(INT64):: NUM
!!!    !
!!!    NUM = HEX%NUM + HEX2%NUM
!!!    !
!!!  END FUNCTION
!!!  !
!!!  ELEMENTAL PURE FUNCTION ADD_HEX_I32(HEX, VAL) RESULT(NUM)
!!!    CLASS(UTF8), INTENT(IN):: HEX
!!!    INTEGER(INT32),     INTENT(IN):: VAL
!!!    INTEGER(INT64):: NUM
!!!    !
!!!    NUM = HEX%NUM + INT(VAL, INT64)
!!!    !
!!!  END FUNCTION
!!!  !
!!!  ELEMENTAL PURE FUNCTION ADD_HEX_I64(HEX, VAL) RESULT(NUM)
!!!    CLASS(UTF8), INTENT(IN):: HEX
!!!    INTEGER(INT64),     INTENT(IN):: VAL
!!!    INTEGER(INT64):: NUM
!!!    !
!!!    NUM = HEX%NUM + VAL
!!!    !
!!!  END FUNCTION
!!!  !
!!!  ELEMENTAL PURE FUNCTION ADD_I32_HEX(VAL, HEX) RESULT(NUM)
!!!    CLASS(UTF8), INTENT(IN):: HEX
!!!    INTEGER(INT32),     INTENT(IN):: VAL
!!!    INTEGER(INT64):: NUM
!!!    !
!!!    NUM = HEX%NUM + INT(VAL, INT64)
!!!    !
!!!  END FUNCTION
!!!  !
!!!  ELEMENTAL PURE FUNCTION ADD_I64_HEX(VAL, HEX) RESULT(NUM)
!!!    CLASS(UTF8), INTENT(IN):: HEX
!!!    INTEGER(INT64),     INTENT(IN):: VAL
!!!    INTEGER(INT64):: NUM
!!!    !
!!!    NUM = HEX%NUM + VAL
!!!    !
!!!  END FUNCTION
!!!  !
!!!  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!!!  !#####################################################################################################
!!!  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!!!  !
!!!  ELEMENTAL PURE FUNCTION SUB_HEX_HEX(HEX, HEX2) RESULT(NUM)
!!!    CLASS(UTF8), INTENT(IN):: HEX
!!!    CLASS(UTF8), INTENT(IN):: HEX2
!!!    INTEGER(INT64):: NUM
!!!    !
!!!    NUM = HEX%NUM - HEX2%NUM
!!!    !
!!!  END FUNCTION
!!!  !
!!!  ELEMENTAL PURE FUNCTION SUB_HEX_I32(HEX, VAL) RESULT(NUM)
!!!    CLASS(UTF8), INTENT(IN):: HEX
!!!    INTEGER(INT32),     INTENT(IN):: VAL
!!!    INTEGER(INT64):: NUM
!!!    !
!!!    NUM = HEX%NUM - INT(VAL, INT64)
!!!    !
!!!  END FUNCTION
!!!  !
!!!  ELEMENTAL PURE FUNCTION SUB_HEX_I64(HEX, VAL) RESULT(NUM)
!!!    CLASS(UTF8), INTENT(IN):: HEX
!!!    INTEGER(INT64),     INTENT(IN):: VAL
!!!    INTEGER(INT64):: NUM
!!!    !
!!!    NUM = HEX%NUM - VAL
!!!    !
!!!  END FUNCTION
!!!  !
!!!  ELEMENTAL PURE FUNCTION SUB_I32_HEX(VAL, HEX) RESULT(NUM)
!!!    CLASS(UTF8), INTENT(IN):: HEX
!!!    INTEGER(INT32),     INTENT(IN):: VAL
!!!    INTEGER(INT64):: NUM
!!!    !
!!!    NUM = INT(VAL, INT64) - HEX%NUM
!!!    !
!!!  END FUNCTION
!!!  !
!!!  ELEMENTAL PURE FUNCTION SUB_I64_HEX(VAL, HEX) RESULT(NUM)
!!!    CLASS(UTF8), INTENT(IN):: HEX
!!!    INTEGER(INT64),     INTENT(IN):: VAL
!!!    INTEGER(INT64):: NUM
!!!    !
!!!    NUM = VAL - HEX%NUM
!!!    !
!!!  END FUNCTION
!!!  !
!!!  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!!!  !#####################################################################################################
!!!  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!!!  !
!!!  ELEMENTAL PURE FUNCTION MLT_HEX_HEX(HEX, HEX2) RESULT(NUM)
!!!    CLASS(UTF8), INTENT(IN):: HEX
!!!    CLASS(UTF8), INTENT(IN):: HEX2
!!!    INTEGER(INT64):: NUM
!!!    !
!!!    NUM = HEX%NUM * HEX2%NUM
!!!    !
!!!  END FUNCTION
!!!  !
!!!  ELEMENTAL PURE FUNCTION MLT_HEX_I32(HEX, VAL) RESULT(NUM)
!!!    CLASS(UTF8), INTENT(IN):: HEX
!!!    INTEGER(INT32),     INTENT(IN):: VAL
!!!    INTEGER(INT64):: NUM
!!!    !
!!!    NUM = HEX%NUM * INT(VAL, INT64)
!!!    !
!!!  END FUNCTION
!!!  !
!!!  ELEMENTAL PURE FUNCTION MLT_HEX_I64(HEX, VAL) RESULT(NUM)
!!!    CLASS(UTF8), INTENT(IN):: HEX
!!!    INTEGER(INT64),     INTENT(IN):: VAL
!!!    INTEGER(INT64):: NUM
!!!    !
!!!    NUM = HEX%NUM * VAL
!!!    !
!!!  END FUNCTION
!!!  !
!!!  ELEMENTAL PURE FUNCTION MLT_I32_HEX(VAL, HEX) RESULT(NUM)
!!!    CLASS(UTF8), INTENT(IN):: HEX
!!!    INTEGER(INT32),     INTENT(IN):: VAL
!!!    INTEGER(INT64):: NUM
!!!    !
!!!    NUM = INT(VAL, INT64) * HEX%NUM
!!!    !
!!!  END FUNCTION
!!!  !
!!!  ELEMENTAL PURE FUNCTION MLT_I64_HEX(VAL, HEX) RESULT(NUM)
!!!    CLASS(UTF8), INTENT(IN):: HEX
!!!    INTEGER(INT64),     INTENT(IN):: VAL
!!!    INTEGER(INT64):: NUM
!!!    !
!!!    NUM = VAL * HEX%NUM
!!!    !
!!!  END FUNCTION
!!!  !
!!!  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!!!  !#####################################################################################################
!!!  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!!!  !
!!!  ELEMENTAL PURE FUNCTION DIV_HEX_HEX(HEX, HEX2) RESULT(NUM)
!!!    CLASS(UTF8), INTENT(IN):: HEX
!!!    CLASS(UTF8), INTENT(IN):: HEX2
!!!    INTEGER(INT64):: NUM
!!!    !
!!!    IF(HEX2%NUM > ZER) THEN
!!!            NUM = HEX%NUM / HEX2%NUM
!!!    ELSE
!!!            NUM = ZER
!!!    END IF
!!!    !
!!!  END FUNCTION
!!!  !
!!!  ELEMENTAL PURE FUNCTION DIV_HEX_I32(HEX, VAL) RESULT(NUM)
!!!    CLASS(UTF8), INTENT(IN):: HEX
!!!    INTEGER(INT32),     INTENT(IN):: VAL
!!!    INTEGER(INT64):: NUM
!!!    !
!!!    IF(VAL > Z) THEN
!!!           NUM = HEX%NUM / INT(VAL, INT64)
!!!    ELSE
!!!           NUM = ZER
!!!    END IF
!!!    !
!!!  END FUNCTION
!!!  !
!!!  ELEMENTAL PURE FUNCTION DIV_HEX_I64(HEX, VAL) RESULT(NUM)
!!!    CLASS(UTF8), INTENT(IN):: HEX
!!!    INTEGER(INT64),     INTENT(IN):: VAL
!!!    INTEGER(INT64):: NUM
!!!    !
!!!    IF(VAL > ZER) THEN
!!!             NUM = HEX%NUM / VAL
!!!    ELSE
!!!             NUM = ZER
!!!    END IF
!!!    !
!!!  END FUNCTION
!!!  !
!!!  ELEMENTAL PURE FUNCTION DIV_I32_HEX(VAL, HEX) RESULT(NUM)
!!!    CLASS(UTF8), INTENT(IN):: HEX
!!!    INTEGER(INT32),     INTENT(IN):: VAL
!!!    INTEGER(INT64):: NUM
!!!    !
!!!    IF(HEX%NUM > ZER) THEN
!!!           NUM = INT(VAL, INT64) / HEX%NUM
!!!    ELSE
!!!           NUM = ZER
!!!    END IF
!!!    !
!!!  END FUNCTION
!!!  !
!!!  ELEMENTAL PURE FUNCTION DIV_I64_HEX(VAL, HEX) RESULT(NUM)
!!!    CLASS(UTF8), INTENT(IN):: HEX
!!!    INTEGER(INT64),     INTENT(IN):: VAL
!!!    INTEGER(INT64):: NUM
!!!    !
!!!    IF(HEX%NUM > ZER) THEN
!!!           NUM = VAL / HEX%NUM
!!!    ELSE
!!!           NUM = ZER
!!!    END IF
!!!    !
!!!  END FUNCTION
!!!  !
!!!  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!!!  !#####################################################################################################
!!!  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!!!  !
!!!  ELEMENTAL PURE FUNCTION EQ_HEX_HEX(HEX, HEX2) RESULT(ANS)
!!!    CLASS(UTF8), INTENT(IN):: HEX
!!!    CLASS(UTF8), INTENT(IN):: HEX2
!!!    LOGICAL:: ANS
!!!    !
!!!    ANS = HEX%NUM == HEX2%NUM
!!!    !
!!!  END FUNCTION
!!!  !
!!!  ELEMENTAL PURE FUNCTION EQ_HEX_I32(HEX, VAL) RESULT(ANS)
!!!    CLASS(UTF8), INTENT(IN):: HEX
!!!    INTEGER(INT32),     INTENT(IN):: VAL
!!!    LOGICAL:: ANS
!!!    !
!!!    ANS = HEX%NUM == INT(VAL, INT64)
!!!    !
!!!  END FUNCTION
!!!  !
!!!  ELEMENTAL PURE FUNCTION EQ_HEX_I64(HEX, VAL) RESULT(ANS)
!!!    CLASS(UTF8), INTENT(IN):: HEX
!!!    INTEGER(INT64),     INTENT(IN):: VAL
!!!    LOGICAL:: ANS
!!!    !
!!!    ANS = HEX%NUM == VAL
!!!    !
!!!  END FUNCTION
!!!  !
!!!  ELEMENTAL PURE FUNCTION EQ_I32_HEX(VAL, HEX) RESULT(ANS)
!!!    CLASS(UTF8), INTENT(IN):: HEX
!!!    INTEGER(INT32),     INTENT(IN):: VAL
!!!    LOGICAL:: ANS
!!!    !
!!!    ANS = HEX%NUM == INT(VAL, INT64)
!!!    !
!!!  END FUNCTION
!!!  !
!!!  ELEMENTAL PURE FUNCTION EQ_I64_HEX(VAL, HEX) RESULT(ANS)
!!!    CLASS(UTF8), INTENT(IN):: HEX
!!!    INTEGER(INT64),     INTENT(IN):: VAL
!!!    LOGICAL:: ANS
!!!    !
!!!    ANS = HEX%NUM == VAL
!!!    !
!!!  END FUNCTION
!!!  !
!!!  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!!!  !#####################################################################################################
!!!  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!!!  !
!!!  ELEMENTAL PURE FUNCTION LT_HEX_HEX(HEX, HEX2) RESULT(ANS)
!!!    CLASS(UTF8), INTENT(IN):: HEX
!!!    CLASS(UTF8), INTENT(IN):: HEX2
!!!    LOGICAL:: ANS
!!!    !
!!!    ANS = HEX%NUM < HEX2%NUM
!!!    !
!!!  END FUNCTION
!!!  !
!!!  ELEMENTAL PURE FUNCTION LT_HEX_I32(HEX, VAL) RESULT(ANS)
!!!    CLASS(UTF8), INTENT(IN):: HEX
!!!    INTEGER(INT32),     INTENT(IN):: VAL
!!!    LOGICAL:: ANS
!!!    !
!!!    ANS = HEX%NUM < INT(VAL, INT64)
!!!    !
!!!  END FUNCTION
!!!  !
!!!  ELEMENTAL PURE FUNCTION LT_HEX_I64(HEX, VAL) RESULT(ANS)
!!!    CLASS(UTF8), INTENT(IN):: HEX
!!!    INTEGER(INT64),     INTENT(IN):: VAL
!!!    LOGICAL:: ANS
!!!    !
!!!    ANS = HEX%NUM < VAL
!!!    !
!!!  END FUNCTION
!!!  !
!!!  ELEMENTAL PURE FUNCTION LT_I32_HEX(VAL, HEX) RESULT(ANS)
!!!    CLASS(UTF8), INTENT(IN):: HEX
!!!    INTEGER(INT32),     INTENT(IN):: VAL
!!!    LOGICAL:: ANS
!!!    !
!!!    ANS = INT(VAL, INT64) < HEX%NUM
!!!    !
!!!  END FUNCTION
!!!  !
!!!  ELEMENTAL PURE FUNCTION LT_I64_HEX(VAL, HEX) RESULT(ANS)
!!!    CLASS(UTF8), INTENT(IN):: HEX
!!!    INTEGER(INT64),     INTENT(IN):: VAL
!!!    LOGICAL:: ANS
!!!    !
!!!    ANS = VAL < HEX%NUM
!!!    !
!!!  END FUNCTION
!!!  !
!!!  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!!!  !#####################################################################################################
!!!  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!!!  !
!!!  ELEMENTAL PURE FUNCTION LE_HEX_HEX(HEX, HEX2) RESULT(ANS)
!!!    CLASS(UTF8), INTENT(IN):: HEX
!!!    CLASS(UTF8), INTENT(IN):: HEX2
!!!    LOGICAL:: ANS
!!!    !
!!!    ANS = HEX%NUM <= HEX2%NUM
!!!    !
!!!  END FUNCTION
!!!  !
!!!  ELEMENTAL PURE FUNCTION LE_HEX_I32(HEX, VAL) RESULT(ANS)
!!!    CLASS(UTF8), INTENT(IN):: HEX
!!!    INTEGER(INT32),     INTENT(IN):: VAL
!!!    LOGICAL:: ANS
!!!    !
!!!    ANS = HEX%NUM <= INT(VAL, INT64)
!!!    !
!!!  END FUNCTION
!!!  !
!!!  ELEMENTAL PURE FUNCTION LE_HEX_I64(HEX, VAL) RESULT(ANS)
!!!    CLASS(UTF8), INTENT(IN):: HEX
!!!    INTEGER(INT64),     INTENT(IN):: VAL
!!!    LOGICAL:: ANS
!!!    !
!!!    ANS = HEX%NUM <= VAL
!!!    !
!!!  END FUNCTION
!!!  !
!!!  ELEMENTAL PURE FUNCTION LE_I32_HEX(VAL, HEX) RESULT(ANS)
!!!    CLASS(UTF8), INTENT(IN):: HEX
!!!    INTEGER(INT32),     INTENT(IN):: VAL
!!!    LOGICAL:: ANS
!!!    !
!!!    ANS = INT(VAL, INT64) <= HEX%NUM
!!!    !
!!!  END FUNCTION
!!!  !
!!!  ELEMENTAL PURE FUNCTION LE_I64_HEX(VAL, HEX) RESULT(ANS)
!!!    CLASS(UTF8), INTENT(IN):: HEX
!!!    INTEGER(INT64),     INTENT(IN):: VAL
!!!    LOGICAL:: ANS
!!!    !
!!!    ANS = VAL <= HEX%NUM
!!!    !
!!!  END FUNCTION
!!!  !
!!!  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!!!  !#####################################################################################################
!!!  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!!!  !
!!!  ELEMENTAL PURE FUNCTION GT_HEX_HEX(HEX, HEX2) RESULT(ANS)
!!!    CLASS(UTF8), INTENT(IN):: HEX
!!!    CLASS(UTF8), INTENT(IN):: HEX2
!!!    LOGICAL:: ANS
!!!    !
!!!    ANS = HEX%NUM > HEX2%NUM
!!!    !
!!!  END FUNCTION
!!!  !
!!!  ELEMENTAL PURE FUNCTION GT_HEX_I32(HEX, VAL) RESULT(ANS)
!!!    CLASS(UTF8), INTENT(IN):: HEX
!!!    INTEGER(INT32),     INTENT(IN):: VAL
!!!    LOGICAL:: ANS
!!!    !
!!!    ANS = HEX%NUM > INT(VAL, INT64)
!!!    !
!!!  END FUNCTION
!!!  !
!!!  ELEMENTAL PURE FUNCTION GT_HEX_I64(HEX, VAL) RESULT(ANS)
!!!    CLASS(UTF8), INTENT(IN):: HEX
!!!    INTEGER(INT64),     INTENT(IN):: VAL
!!!    LOGICAL:: ANS
!!!    !
!!!    ANS = HEX%NUM > VAL
!!!    !
!!!  END FUNCTION
!!!  !
!!!  ELEMENTAL PURE FUNCTION GT_I32_HEX(VAL, HEX) RESULT(ANS)
!!!    CLASS(UTF8), INTENT(IN):: HEX
!!!    INTEGER(INT32),     INTENT(IN):: VAL
!!!    LOGICAL:: ANS
!!!    !
!!!    ANS = INT(VAL, INT64) > HEX%NUM
!!!    !
!!!  END FUNCTION
!!!  !
!!!  ELEMENTAL PURE FUNCTION GT_I64_HEX(VAL, HEX) RESULT(ANS)
!!!    CLASS(UTF8), INTENT(IN):: HEX
!!!    INTEGER(INT64),     INTENT(IN):: VAL
!!!    LOGICAL:: ANS
!!!    !
!!!    ANS = VAL > HEX%NUM
!!!    !
!!!  END FUNCTION
!!!  !
!!!  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!!!  !#####################################################################################################
!!!  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!!!  !
!!!  ELEMENTAL PURE FUNCTION GE_HEX_HEX(HEX, HEX2) RESULT(ANS)
!!!    CLASS(UTF8), INTENT(IN):: HEX
!!!    CLASS(UTF8), INTENT(IN):: HEX2
!!!    LOGICAL:: ANS
!!!    !
!!!    ANS = HEX%NUM >= HEX2%NUM
!!!    !
!!!  END FUNCTION
!!!  !
!!!  ELEMENTAL PURE FUNCTION GE_HEX_I32(HEX, VAL) RESULT(ANS)
!!!    CLASS(UTF8), INTENT(IN):: HEX
!!!    INTEGER(INT32),     INTENT(IN):: VAL
!!!    LOGICAL:: ANS
!!!    !
!!!    ANS = HEX%NUM >= INT(VAL, INT64)
!!!    !
!!!  END FUNCTION
!!!  !
!!!  ELEMENTAL PURE FUNCTION GE_HEX_I64(HEX, VAL) RESULT(ANS)
!!!    CLASS(UTF8), INTENT(IN):: HEX
!!!    INTEGER(INT64),     INTENT(IN):: VAL
!!!    LOGICAL:: ANS
!!!    !
!!!    ANS = HEX%NUM >= VAL
!!!    !
!!!  END FUNCTION
!!!  !
!!!  ELEMENTAL PURE FUNCTION GE_I32_HEX(VAL, HEX) RESULT(ANS)
!!!    CLASS(UTF8), INTENT(IN):: HEX
!!!    INTEGER(INT32),     INTENT(IN):: VAL
!!!    LOGICAL:: ANS
!!!    !
!!!    ANS = INT(VAL, INT64) >= HEX%NUM
!!!    !
!!!  END FUNCTION
!!!  !
!!!  ELEMENTAL PURE FUNCTION GE_I64_HEX(VAL, HEX) RESULT(ANS)
!!!    CLASS(UTF8), INTENT(IN):: HEX
!!!    INTEGER(INT64),     INTENT(IN):: VAL
!!!    LOGICAL:: ANS
!!!    !
!!!    ANS = VAL >= HEX%NUM
!!!    !
!!!  END FUNCTION
!!!  !
!!!  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!!!  !#####################################################################################################
!!!  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!!!  !
!!!  SUBROUTINE FMTREAD_UTF8(HEX, UNIT, IOTYPE, V_LIST, IOSTAT, IOMSG)
!!!    CLASS(UTF8), INTENT(INOUT):: HEX
!!!    INTEGER,            INTENT(IN   ):: UNIT
!!!    CHARACTER(*),       INTENT(IN   ):: IOTYPE
!!!    INTEGER,            INTENT(IN   ):: V_LIST (:)
!!!    INTEGER,            INTENT(OUT  ):: IOSTAT
!!!    CHARACTER(*),       INTENT(INOUT):: IOMSG
!!!    CHARACTER(16) :: TMP
!!!    INTEGER(INT64):: NUM
!!!    INTEGER:: IERR
!!!    !
!!!    ! This is the child I/O that gets performed when the procedure
!!!    ! is called from a parent I/O - it uses list-directed input to read
!!!    ! the array K
!!!    !
!!!    TMP = ""
!!!    READ (UNIT, FMT='(A)', IOSTAT=IOSTAT, IOMSG=IOMSG) TMP
!!!    !
!!!    IF(IOSTAT ==Z)  THEN
!!!                    CALL INIT_UTF8_STR(HEX, TMP)
!!!    ELSE
!!!                    HEX%NUM = ZER
!!!    END IF
!!!    !
!!!  END SUBROUTINE  
!!!  !
!!!  SUBROUTINE FMTWRITE_UTF8(HEX, UNIT, IOTYPE, V_LIST, IOSTAT, IOMSG)  !* or '(DT"IOTYPE")' or '(DT)'
!!!    CLASS(UTF8), INTENT(IN   ):: HEX
!!!    INTEGER,            INTENT(IN   ):: UNIT
!!!    CHARACTER(*),       INTENT(IN   ):: IOTYPE
!!!    INTEGER,            INTENT(IN   ):: V_LIST (:)
!!!    INTEGER,            INTENT(OUT  ):: IOSTAT
!!!    CHARACTER(*),       INTENT(INOUT):: IOMSG
!!!    INTEGER:: N, M
!!!    !
!!!    ! This is the child I/O that gets performed when the procedure
!!!    ! is called from a parent I/O - it uses list-directed input to read
!!!    ! the array K
!!!    !
!!!    WRITE(UNIT, FMT='(A)', IOSTAT=IOSTAT, IOMSG=IOMSG) HEX_UTF8(HEX)
!!!    !
!!!  END SUBROUTINE 
!!!  !
END MODULE
