
MODULE UNICODE_INTERFACE
  USE, INTRINSIC:: ISO_FORTRAN_ENV, ONLY: INT8, INT16, INT32, INT64
  IMPLICIT NONE (TYPE, EXTERNAL)
  !
  PUBLIC:: getUTF8      ! UTF8 = getUTF8(CODE_POINT)    ->  RESULT( CHARACTER(*) ) max len of 4 
  PUBLIC:: UTF8_TO_INT  ! IPNT = UTF8_TO_INT(UTF8)
  PUBLIC:: NEXT_UTF8    ! CALL NEXT_UTF8(LN, I, J, IOSTAT, [DEC], [HEX])  -> Set I=0 on first call, after that it is automatically set to J+1, if J < I, then at end of line
  !
  PRIVATE
  !
  INTERFACE getUTF8
    MODULE PROCEDURE   HEX_TO_UTF8 ! getUTF8(HEX)  RESULT(UTF8)
    MODULE PROCEDURE INT32_TO_UTF8 ! getUTF8(IPNT) RESULT(UTF8)
    MODULE PROCEDURE INT64_TO_UTF8
  END INTERFACE
  !
  !-----------------------------------------------------------------------------------------------------------
  !
  LOGICAL,  PARAMETER:: Little_Endian = IACHAR( TRANSFER( 1_INT16, ' ' ) ) == 1 ! Transfer b' 00000000 00000001' to CHAR(1), which will drop either then 1st or 2nd byte. If 2nd byte is kept, then CPU is Little-Endian. -> Note Itel/AMD x86 CPUs are Little-Endian
  !
  CHARACTER(21),  PARAMETER:: HEX_LIST = '123456789ABCDEFabcdef'
  CHARACTER,      PARAMETER:: NUL     = TRANSFER(0_INT8, ' ')
  !
  INTEGER(INT32), PARAMETER:: Z   = 0_INT32
  INTEGER(INT32), PARAMETER:: ONE = 1_INT32
  INTEGER(INT64), PARAMETER:: ZER = 0_INT64
  !
  LOGICAL, PARAMETER:: TRUE  = .TRUE.
  LOGICAL, PARAMETER:: FALSE = .FALSE.
  !
  !-----------------------------------------------------------------------------------------------------------
  !
  INTEGER(INT32), PARAMETER::                               &
    UTF8_Mask_2B     =                 b'1100000010000000' ,&!  2 byte UTF8 Mask: 110xxxxx 10xxxxxx                   -> I32 Decimal:      49280
    UTF8_Mask_3B     =         b'111000001000000010000000' ,&!  3 byte UTF8 Mask: 1110xxxx 10xxxxxx 10xxxxxx          -> I32 Decimal:   14712960
    UTF8_Mask_4B     = b'11110000100000001000000010000000'   !  4 byte UTF8 Mask: 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx -> I32 Decimal: -260013952 (note negative bit is set)
  !
  INTEGER(INT64), PARAMETER::                               &
    UTF8_Mask_2B_I64 =                 b'1100000010000000' ,&!  2 byte UTF8 Mask: 110xxxxx 10xxxxxx                   -> I64 Decimal:      49280
    UTF8_Mask_3B_I64 =         b'111000001000000010000000' ,&!  3 byte UTF8 Mask: 1110xxxx 10xxxxxx 10xxxxxx          -> I64 Decimal:   14712960
    UTF8_Mask_4B_I64 = b'11110000100000001000000010000000'   !  4 byte UTF8 Mask: 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx -> I64 Decimal: 4034953344
  !
  !-----------------------------------------------------------------------------------------------------------
  !
  INTEGER(INT32), PARAMETER::& ! Size of UTF8 Code Point is:
    UTF8_1B =   128_INT32   ,& !      <   128 is 1 byte
    UTF8_2B =  2048_INT32   ,& ! else <  2048 is 2 byte
    UTF8_3B = 65536_INT32      ! else < 65536 is 3 byte
  !
  INTEGER(INT64), PARAMETER::& ! Size of UTF8 Code Point is:
    UTF8_1B_I64 =   128_INT64   ,& !      <   128 is 1 byte
    UTF8_2B_I64 =  2048_INT64   ,& ! else <  2048 is 2 byte
    UTF8_3B_I64 = 65536_INT64      ! else < 65536 is 3 byte
  !
  !-----------------------------------------------------------------------------------------------------------
  !
  CONTAINS
  !#########################################################################################################################
  !
  ! If IOSTAT = 0, indicates all is fine
  ! If IOSTAT > 0, indicates bad byte at LN(iostat:iostat), if IOSTAT > LEN(LN), then indicates other errors
  ! If IOSTAT < 0, indicates end of line and return value will have J < I = LEN(LN)
  !
  ! Set I = 0, to start search of line
  ! If  J < I, then assumes at end of line
  ! Otherwise I is automatically set to J+1
  PURE SUBROUTINE NEXT_UTF8(LN, I, J, IOSTAT, DEC, HEX)
    ! ASSUMES COMPILER CAN HANDEL LN(I:I-1) AND SETS IT TO BLANK STRING
    CHARACTER(*),             INTENT(IN   ):: LN
    INTEGER,                  INTENT(INOUT):: I, J
    INTEGER,                  INTENT(  OUT):: IOSTAT
    INTEGER(INT32), OPTIONAL, INTENT(  OUT):: DEC
    CHARACTER(8),   OPTIONAL, INTENT(  OUT):: HEX
    INTEGER:: DIM, K
    !
    IOSTAT = Z
    DIM    = LEN(LN)
    !
    IF (I < ONE .OR. J < ONE) THEN
                 I = ONE
                 J = ONE
    ELSEIF(J < I) THEN
                 I = DIM + ONE
                 J = I
    ELSE
                 I = J + ONE
                 J = I
    END IF
    !
    IF( I > DIM ) THEN
        I = DIM
        J = I - 1
        IOSTAT = -1
        IF(PRESENT(DEC)) DEC = -1
        IF(PRESENT(HEX)) HEX = ""
        RETURN
    END IF
    !
    K = UTF_BIT_HEADER(LN(I:I)) ! Number of bytes equals the number of bits (eg 1110xxxx, indicates 3 bytes)
    !
    IF( K > Z ) J = J + K - 1  
    !
    IF    ( J > DIM ) THEN  !Should not happen, unless there is a missing byte in LN
                      IOSTAT = DIM + 1
    ELSEIF( I < J   ) THEN  ! >1 Bytes count
                      DO K=I+1, J
                          IF( UTF_BIT_HEADER(LN(K:K)) /= 1 ) THEN
                                                        IOSTAT = K
                                                        EXIT
                          END IF
                      END DO
    END IF
    !
    IF( IOSTAT > Z ) THEN
                     IF(PRESENT(DEC)) DEC = -1
                     IF(PRESENT(HEX)) HEX = ""
    ELSEIF( PRESENT(DEC) .OR. PRESENT(HEX) ) THEN
        BLOCK
           INTEGER(INT32):: ITMP
           !
           ITMP = UTF8_TO_INT( LN(I:J) )
           !
           IF(PRESENT(DEC)) DEC = ITMP
           IF(PRESENT(HEX)) HEX = int2hex(ITMP)
        END BLOCK
    END IF
    !
  END SUBROUTINE
  !
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !#####################################################################################################
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !
   FUNCTION UTF8_TO_INT(CH) RESULT(IPNT)  ! Assumes valid UTF8 character
    USE NUMBER_CONVERSION_INTERFACE
    CHARACTER(*), INTENT(IN):: CH
    INTEGER(INT32):: IPNT, ITMP
    CHARACTER(4):: TMP
    INTEGER:: I, J, N
    !
    N = LEN(CH)
    !
    IF    (N == 1) THEN
                   IPNT = IACHAR(CH)
                   RETURN
    END IF
    !
    IF(N < 1 .OR. N > 4) THEN
                   IPNT = -1
                   RETURN
    END IF
    !
    ! Byte Endian order may affect TRANSFER.
    !   Little-Endian CPUs reverse the order of the bytes causing UTF8 bytes to be in reverse order
    !   -> Unable to test/validate this concept on Big-Endian CPUs
    !                      
    CALL SET_CHAR_TO_NULL(TMP)
    !
    IF(Little_Endian) THEN 
       J = N                      
       DO I=1, N
               TMP(I:I) = CH(J:J)
               J = J - 1
       END DO
    ELSE
       TMP(1:N) = CH
    END IF
    !
    IPNT = TRANSFER(TMP, IPNT) 
    !
    IF    (N == 2) THEN  ! 2 byte UTF8 Mask: 110xxxxx 10xxxxxx
                   !
                   ITMP = IBITS(IPNT, 0, 6)
                   ITMP = IOR(  ITMP, SHIFTL(IBITS(IPNT, 8, 5), 6) )
                   !                                                         54321 9876543210
    ELSEIF(N == 3) THEN  !  3 byte UTF8 Mask: 1110xxxx 10xxxxxx 10xxxxxx  -> aaaabbbbbbcccccc
                   !                          
                   ITMP = IBITS(IPNT, 0, 6)
                   ITMP = IOR(  ITMP, SHIFTL(IBITS(IPNT,  8, 6),  6) )
                   ITMP = IOR(  ITMP, SHIFTL(IBITS(IPNT, 16, 4), 12) )
                   !                                                                 987654321 9876543210
    ELSE                ! 4 byte UTF8 Mask: 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx  -> dddccccccbbbbbbaaaaaa
                   !   
                   ITMP = IBITS(IPNT, 0, 6)
                   ITMP = IOR(  ITMP, SHIFTL(IBITS(IPNT,  8, 6),  6) )
                   ITMP = IOR(  ITMP, SHIFTL(IBITS(IPNT, 16, 6), 12) )
                   ITMP = IOR(  ITMP, SHIFTL(IBITS(IPNT, 24, 3), 18) )
    END IF
    !
    IPNT = ITMP
    !
  END FUNCTION
  !
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !#####################################################################################################
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !
  PURE FUNCTION HEX_TO_UTF8(HEX) RESULT(UTF8)
    CHARACTER(*),  INTENT(IN):: HEX
    CHARACTER(:), ALLOCATABLE:: UTF8
    INTEGER(INT32):: IPNT
    INTEGER(INT32):: base, M
    INTEGER(INT32):: I, J, K
    !
    IPNT = Z
    J    = 1
    K    = LEN_TRIM(HEX)
    !
    DO WHILE (J <= K)
       IF( INDEX(HEX_LIST, HEX(J:J)) > 0 ) EXIT 
       J = J + 1
    END DO
    !
    base = 1_INT32   ! powers of 16
    DO I=K, J, -1_INT32            ! --> Loop converts Hex to Int
        IF(HEX(I:I) == " ") CYCLE
        !
        M = INDEX(HEX_LIST, HEX(I:I))
        IF(M > 15_INT32) M = M - 6_INT32
        IF(M > ZER) IPNT = IPNT + M*base
        !
        base = SHIFTL(base, 4)  ! Multiply by 2^4
    END DO
    !
    UTF8 = INT32_TO_UTF8(IPNT)
    !
  END FUNCTION
  !
  !-----------------------------------------------------------------------------------------------------------
  !
  PURE FUNCTION INT32_TO_UTF8(IPNT) RESULT(UTF8)
    INTEGER(INT32),  INTENT(IN):: IPNT
    CHARACTER(:),   ALLOCATABLE:: UTF8
    INTEGER(INT32):: UTF
    CHARACTER(4):: TMP
    INTEGER:: I, J, N
    !
    IF    (IPNT < UTF8_1B) THEN
                       N = 1
                       UTF = IPNT        ! In the ASCII range -> ACHAR(IPNT)
    ELSEIF(IPNT < UTF8_2B) THEN  
                       N = 2                                         ! 2 byte Unicode Point
                       UTF = IOR(UTF8_Mask_2B,IBITS(IPNT, 0, 6))       ! Extract first 6 bits and fold into UNICODE prefix
                       UTF = IOR(UTF,  SHIFTL(IBITS(IPNT, 6, 5),8))    ! Extract the remaining bits, shift them, and fold into UTF
    ELSEIF(IPNT < UTF8_3B) THEN  
                       N = 3                                         ! 3 byte Unicode Point
                       UTF = IOR(UTF8_Mask_3B,IBITS(IPNT, 0, 6))       ! Extract first 6 bits and fold into UNICODE prefix
                       UTF = IOR(UTF,  SHIFTL(IBITS(IPNT, 6, 6),8))    ! Extract the next 6 bits and shift to allow folding into byte 2
                       UTF = IOR(UTF,  SHIFTL(IBITS(IPNT,12, 4),16))   ! Extract the remaining bits, shift them, and fold into UTF
    ELSE  
                       N = 4                                         ! 4 byte Unicode Point
                       UTF = IOR(UTF8_Mask_4B,IBITS(IPNT, 0, 6))       ! Extract first 6 bits and fold into UNICODE prefix
                       UTF = IOR(UTF,  SHIFTL(IBITS(IPNT, 6, 6),8))    ! Extract the next 6 bits and shift to allow folding into byte 2
                       UTF = IOR(UTF,  SHIFTL(IBITS(IPNT,12, 6),16))   ! Extract the next 6 bits and shift to allow folding into byte 3
                       UTF = IOR(UTF,  SHIFTL(IBITS(IPNT,18, 3),24))   ! Extract the remaining bits, shift them, and fold into UTF
    END IF
    !
    TMP = TRANSFER(UTF, TMP) 
    !
    ALLOCATE(CHARACTER(N):: UTF8)
    !
    ! Byte Endian order may affect TRANSFER.
    !   Little-Endian CPUs reverse the order of the bytes causing UTF8 bytes to be in reverse order
    !   -> Unable to test/validate this concept on Big-Endian CPUs
    !
    IF(Little_Endian) THEN 
       J = N                      
       DO I=1, N
               UTF8(J:J) = TMP(I:I)
               J = J - 1
       END DO
    ELSE
       UTF8 = TMP(1:N)
    END IF
    !
  END FUNCTION
  !
  !-----------------------------------------------------------------------------------------------------------
  !
  PURE FUNCTION INT64_TO_UTF8(IPNT) RESULT(UTF8)
    INTEGER(INT64),  INTENT(IN):: IPNT
    CHARACTER(:),   ALLOCATABLE:: UTF8
    INTEGER(INT64):: UTF
    CHARACTER(4):: TMP
    INTEGER:: I, J, N
    !
    IF    (IPNT < UTF8_1B_I64) THEN
                       N = 1
                       UTF = IPNT        ! In the ASCII range -> ACHAR(IPNT)
    ELSEIF(IPNT < UTF8_2B_I64) THEN  
                       N = 2                                         ! 2 byte Unicode Point
                       UTF = IOR(UTF8_Mask_2B_I64,IBITS(IPNT, 0, 6))   ! Extract first 6 bits and fold into UNICODE prefix
                       UTF = IOR(UTF,  SHIFTL(IBITS(IPNT, 6, 5),8))    ! Extract the remaining bits, shift them, and fold into UTF
    ELSEIF(IPNT < UTF8_3B_I64) THEN  
                       N = 3                                         ! 3 byte Unicode Point
                       UTF = IOR(UTF8_Mask_3B_I64,IBITS(IPNT, 0, 6))   ! Extract first 6 bits and fold into UNICODE prefix
                       UTF = IOR(UTF,  SHIFTL(IBITS(IPNT, 6, 6),8))    ! Extract the next 6 bits and shift to allow folding into byte 2
                       UTF = IOR(UTF,  SHIFTL(IBITS(IPNT,12, 4),16))   ! Extract the remaining bits, shift them, and fold into UTF
    ELSE  
                       N = 4                                         ! 4 byte Unicode Point
                       UTF = IOR(UTF8_Mask_4B_I64,IBITS(IPNT, 0, 6))   ! Extract first 6 bits and fold into UNICODE prefix
                       UTF = IOR(UTF,  SHIFTL(IBITS(IPNT, 6, 6),8))    ! Extract the next 6 bits and shift to allow folding into byte 2
                       UTF = IOR(UTF,  SHIFTL(IBITS(IPNT,12, 6),16))   ! Extract the next 6 bits and shift to allow folding into byte 3
                       UTF = IOR(UTF,  SHIFTL(IBITS(IPNT,18, 3),24))   ! Extract the remaining bits, shift them, and fold into UTF
    END IF
    !
    TMP = TRANSFER(UTF, TMP)
    !
    ALLOCATE(CHARACTER(N):: UTF8)
    !
    ! Byte Endian order may affect TRANSFER.
    !   Little-Endian CPUs reverse the order of the bytes causing UTF8 bytes to be in reverse order
    !   -> Unable to test/validate this concept on Big-Endian CPUs
    !
    IF(Little_Endian) THEN 
       J = N                      
       DO I=1, N
               UTF8(J:J) = TMP(I:I)
               J = J - 1
       END DO
    ELSE
       UTF8 = TMP(1:N)
    END IF
    !
  END FUNCTION
  !
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !#####################################################################################################
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !
  PURE SUBROUTINE SET_CHAR_TO_NULL(STR)
    CHARACTER(*), INTENT(INOUT):: STR
    DO CONCURRENT ( INTEGER:: I=1:LEN(STR) )
                                      STR(I:I) = NUL
    END DO
  END SUBROUTINE
  ! 
  !-----------------------------------------------------------------------------------------------------------
  !
  PURE FUNCTION UTF_BIT_HEADER(CH) RESULT(NBIT)
    CHARACTER, INTENT(IN):: CH
    INTEGER(INT32):: NBIT
    INTEGER(INT8 ):: BYT
    INTEGER:: K
    !
    NBIT = Z
    BYT = TRANSFER(CH, BYT)
    !                                                                   76543210 <-POS
    IF(BYT < ONE) THEN ! If negative, then leftmost bit is set. That is 1xxxxxxx <-Negative Bit
       NBIT = 1
       DO K = 6, 0, -1
                       IF(.NOT. BTEST(BYT,K)) EXIT
                       NBIT = NBIT + ONE
       END DO
    END IF
    !
  END FUNCTION
  ! 
  !-----------------------------------------------------------------------------------------------------------
  !
  PURE FUNCTION int2hex(VAL32) RESULT(HEX)
    INTEGER(INT32), INTENT(IN):: VAL32
    CHARACTER(:),  ALLOCATABLE:: HEX
    CHARACTER(16):: TMP
    INTEGER:: I, N
    INTEGER(INT32):: VAL
    !
    VAL = VAL32
    I = 16
    DO WHILE( VAL > Z .AND. I > Z)
        !
        N = IAND(VAL, 15_INT32)     ! Get remainder of VAL/16
        !
        IF(N == Z) THEN
                     TMP(I:I) = "0"
        ELSE
                     TMP(I:I) = HEX_LIST(N:N)
        END IF
        !
        I = I - 1
        VAL = SHIFTR(VAL, 4)  ! Divide by 16 via bit shift operation
        !
    END DO
    !
    IF( VAL < 1 .AND. I < 16 ) THEN
                  HEX = TMP(I+1:16)
    ELSE
                  HEX = 'ERROR - HEX'
    END IF
    !
  END FUNCTION
  ! 
END MODULE