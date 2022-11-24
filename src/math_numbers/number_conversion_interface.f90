!
! Convert from/to binary to decimal, octal, hexadecimal
!
!   - Note you can also use the Fortran READ/WRITE statements to do the same.
!     However, it may not work with a Prefix, such as 0xFF or U+FFFF 
!      
!     -> Write an INT to string that is BINary, OCTal, and HEXadecimal
!         WRITE(BIN,'(B)') INT
!         WRITE(OCT,'(O)') INT
!         WRITE(HEX,'(Z)') INT
!         
!     -> READ a string that is BINary, OCTal, and HEXadecimal and convert it to INT
!         READ(BIN,'(B)') INT
!         READ(OCT,'(O)') INT
!         READ(HEX,'(Z)') INT
!
!      => Note some compilers may require you set the width (w) in 'Bw', 'Ow', 'Zw'
!
MODULE NUMBER_CONVERSION_INTERFACE
  USE, INTRINSIC:: ISO_FORTRAN_ENV, ONLY: INT8, INT16, INT32, INT64
  IMPLICIT NONE(TYPE, EXTERNAL)
  !
  PUBLIC:: hex2int, oct2int, bin2int  ! covert string to int32
  PUBLIC:: int2hex, int2oct, int2bin  ! int32 or int64 to string
  PUBLIC:: TO_HEX_BYTES               ! Takes binary partter and represents it in HEX paris for each byte
  !
  PRIVATE
  !
  INTERFACE int2hex                   !  int2hex(VAL) RESULT(HEX)
    MODULE PROCEDURE int2hex_INT32
    MODULE PROCEDURE int2hex_INT64
  END INTERFACE
  !
  INTERFACE int2oct                   !  int2oct(VAL) RESULT(OCT)
    MODULE PROCEDURE int2oct_INT32
    MODULE PROCEDURE int2oct_INT64
  END INTERFACE
  !
  INTERFACE int2bin                   !  int2bin(VAL)     RESULT(BIN)
    MODULE PROCEDURE int2bin_INT8_GP  !  int2bin(VAL, GP) RESULT(BIN) GP = 0, 4, or 8 for how to group bits.
    MODULE PROCEDURE int2bin_INT16_GP 
    MODULE PROCEDURE int2bin_INT32_GP 
    MODULE PROCEDURE int2bin_INT64_GP
    MODULE PROCEDURE int2bin_INT8
    MODULE PROCEDURE int2bin_INT16
    MODULE PROCEDURE int2bin_INT32
    MODULE PROCEDURE int2bin_INT64
  END INTERFACE
  !
  INTERFACE TO_HEX_BYTES
    MODULE PROCEDURE   STR_TO_HEX_BYTES !TO_HEX_BYTES(VAL) RESULT(HEX)
    MODULE PROCEDURE  INT8_TO_HEX_BYTES
    MODULE PROCEDURE INT16_TO_HEX_BYTES
    MODULE PROCEDURE INT32_TO_HEX_BYTES
    MODULE PROCEDURE INT64_TO_HEX_BYTES
  END INTERFACE
  !
  CHARACTER(21),  PARAMETER:: HEX_LIST = '123456789ABCDEFabcdef'
  CHARACTER( 7),  PARAMETER:: OCT_LIST = '1234567'
  !
  INTEGER(INT8 ), PARAMETER:: ZER8   = 0_int8
  INTEGER(INT16), PARAMETER:: ZER16  = 0_int16
  INTEGER(INT32), PARAMETER:: ZER32  = 0_int32
  INTEGER(INT64), PARAMETER:: ZER64  = 0_int64
  !
  INTEGER(INT8 ), PARAMETER:: ONE8   = 1_int8
  INTEGER(INT16), PARAMETER:: ONE16  = 1_int16
  INTEGER(INT32), PARAMETER:: ONE32  = 1_int32
  INTEGER(INT64), PARAMETER:: ONE64  = 1_int64
  !
  LOGICAL,  PARAMETER:: Little_Endian = IACHAR( TRANSFER( 1_INT16, ' ' ) ) == 1 ! Transfer b' 00000000 00000001' to CHAR(1), which will drop either then 1st or 2nd byte. If 2nd byte is kept, then CPU is Little-Endian. -> Note Itel/AMD x86 CPUs are Little-Endian
  !
  CONTAINS
  !
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !#####################################################################################################
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !
  PURE FUNCTION hex2int(HEX) RESULT(VAL) ! Ignores blank spaces
    CHARACTER(*), intent(in):: HEX
    INTEGER(INT32):: VAL
    INTEGER(INT32):: base, M
    INTEGER:: I, J, N
    !
    N    = LEN_TRIM(HEX)
    base = ONE32   ! powers of 16
    VAL  = ZER32
    !
    J = 1
    DO WHILE (J <= N)
       IF( INDEX(HEX_LIST, HEX(J:J)) > 0 ) EXIT 
       J = J + 1
    END DO
    !
    DO I=N, J, -1
        !
        IF(HEX(I:I) == " ") CYCLE
        !
        M = INDEX(HEX_LIST, HEX(I:I), KIND=INT32)
        !
        IF(M > 15_INT32) M = M - 6_INT32
        !
        IF(M > ZER32) VAL = VAL + M*base
        !
        base = SHIFTL(base, 4)  ! Multiply by 2^4
        !
    END DO
    !
  END FUNCTION
  !
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !#####################################################################################################
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !
  PURE FUNCTION oct2int(OCT) RESULT(VAL) ! Ignores blank spaces
    CHARACTER(*), intent(in):: OCT
    INTEGER(INT32):: VAL
    INTEGER(INT32):: base, M
    INTEGER:: I, J, N
    !
    N    = LEN_TRIM(OCT)
    base = ONE32   ! powers of 8
    VAL  = ZER32
    !
    J = 1
    DO WHILE (J <= N)
       IF( '0' <= OCT(J:J) .AND. OCT(J:J) <= '7' ) EXIT 
       J = J + 1
    END DO
    !
    DO I=N, J, -1
        !
        IF(OCT(I:I) == " ") CYCLE
        !
        M = INDEX(OCT_LIST, OCT(I:I), KIND=INT32)
        !
        IF(M > ZER32) VAL = VAL + M*base
        !
        base = SHIFTL(base, 3)  ! Multiply by 2^3
        !
    END DO
    !
  END FUNCTION 
  !
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !#####################################################################################################
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !
  PURE FUNCTION bin2int(BIN) RESULT(VAL) ! Ignores blank spaces
    CHARACTER(*), intent(in):: BIN
    INTEGER(INT32):: VAL
    INTEGER(INT32):: base
    INTEGER:: I, J, N
    !
    N    = LEN_TRIM(BIN)
    base = ONE32   ! powers of 2
    VAL  = ZER32
    !
    J = 1
    DO WHILE (J <= N)
       IF( BIN(J:J) == '0' .OR. BIN(J:J) == '1' ) EXIT 
       J = J + 1
    END DO
    !
    DO I=N, J, -1
        !
        IF(BIN(I:I) == '1') THEN
                            VAL = VAL + base
                            base = SHIFTL(base, 1)  ! Multiply by 2
        ELSEIF(BIN(I:I) == '0') THEN
                            base = SHIFTL(base, 1) 
        END IF
        !
    END DO
    !
  END FUNCTION
  !
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !#####################################################################################################
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !
  PURE FUNCTION int2hex_INT32(VAL32) RESULT(HEX)
    INTEGER(INT32), intent(in):: VAL32
    CHARACTER(:),  allocatable:: HEX
    CHARACTER(16):: TMP
    INTEGER:: I, N
    INTEGER(INT32):: VAL
    !
    VAL = VAL32
    I = 16
    DO WHILE( VAL > ZER32 .AND. I > 0)
        !
        N = IAND(VAL, 15_INT32)     ! Get remainder of VAL/16
        !
        IF(N == 0) THEN
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
    IF( VAL < ONE32 .AND. I < 16 ) THEN
                  HEX = TMP(I+1:16)
    ELSE
                  HEX = 'ERROR - HEX'
    END IF
    !
  END FUNCTION
  !
  PURE FUNCTION int2hex_INT64(VAL64) RESULT(HEX)
    INTEGER(INT64), intent(in):: VAL64
    CHARACTER(:),  allocatable:: HEX
    CHARACTER(16):: TMP
    INTEGER:: I
    INTEGER(INT32):: N
    INTEGER(INT64):: VAL
    !
    VAL = VAL64  ! SHIFTR seems to fail when using VALUE on INTEL OneAPI
    I = 16
    DO WHILE( VAL > ZER64 .AND. I > 0)
        !
        N = INT( IAND(VAL, 15_INT64), INT32)     ! Get remainder of VAL/16
        !
        IF(N == ZER32) THEN
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
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !#####################################################################################################
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !
  PURE FUNCTION int2oct_INT32(VAL32) RESULT(OCT)
    INTEGER(INT32), intent(in):: VAL32
    CHARACTER(:),  allocatable:: OCT
    CHARACTER(32):: TMP
    INTEGER:: I, N
    INTEGER(INT32):: VAL
    !
    VAL = VAL32
    I = 32
    DO WHILE( VAL > ZER32 .AND. I > 0)
        !
        N = IAND(VAL, 7_INT32)   ! Get remainder of VAL/8
        !
        IF(N == 0) THEN
                     TMP(I:I) = "0"
        ELSE
                     TMP(I:I) = OCT_LIST(N:N)
        END IF
        !
        I = I - 1
        VAL = SHIFTR(VAL, 3)  ! Divide by 8 via bit shift operation
        !
    END DO
    !
    IF( VAL < ONE32 .AND. I < 32 ) THEN
                  OCT = TMP(I+1:32)
    ELSE
                  OCT = 'ERROR - OCT'
    END IF
    !
  END FUNCTION
  !
  PURE FUNCTION int2oct_INT64(VAL64) RESULT(OCT)
    INTEGER(INT64), intent(in):: VAL64
    CHARACTER(:),  allocatable:: OCT
    CHARACTER(32):: TMP
    INTEGER:: I
    INTEGER(INT32):: N
    INTEGER(INT64):: VAL
    !
    VAL = VAL64  ! SHIFTR seems to fail when using VALUE on INTEL OneAPI
    I = 32
    DO WHILE( VAL > ZER64 .AND. I > 0)
        !
        N = INT( IAND(VAL, 7_INT64), INT32)     ! Get remainder of VAL/8
        !
        IF(N == ZER32) THEN
                     TMP(I:I) = "0"
        ELSE
                     TMP(I:I) = OCT_LIST(N:N)
        END IF
        !
        I = I - 1
        VAL = SHIFTR(VAL, 3)  ! Divide by 8 via bit shift operation
        !
    END DO
    !
    IF( VAL < ONE64 .AND. I < 32 ) THEN
                  OCT = TMP(I+1:32)
    ELSE
                  OCT = 'ERROR - OCT'
    END IF
    !
  END FUNCTION
  !
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !#####################################################################################################
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !
  PURE FUNCTION int2bin_INT8_GP(VAL32, GROUP) RESULT(BIN)
    INTEGER(INT8),     intent(in):: VAL32
    INTEGER,           intent(in):: GROUP !Set to 0, 4, 8
    CHARACTER(:),     allocatable:: BIN
    CHARACTER(16):: TMP
    INTEGER:: I, J, GP
    INTEGER(INT8):: VAL
    !
    IF    (GROUP > 7) THEN
           GP = 8
    ELSEIF(GROUP < 2) THEN
           GP = 0
    ELSE
           GP = 4
    END IF
    !
    TMP = ""
    VAL = VAL32
    I = 16
    J = GP
    DO WHILE( VAL > ZER8 .AND. I > 0)
        !
        IF(J == 0) THEN
           J = GP
        ELSE
           J = J - 1
           !
           IF(BTEST(VAL, 0)) THEN   ! Odd number if least sig bit is set  -- Old Method: IF(IAND(VAL,ONE8) == ONE8) THEN
                        TMP(I:I) = "1"
           ELSE
                        TMP(I:I) = "0"
           END IF
           VAL = SHIFTR(VAL, 1)  ! Divide by 2 via bit shift operation
        END IF
        !
        I = I - 1
    END DO
    !
    IF( VAL < ONE8 .AND. I < 16 ) THEN
        IF( J > 0 ) THEN
                  BIN = REPEAT('0',J) // TMP(I+1:16)
        ELSE
                  BIN = TMP(I+1:16)
        END IF
    ELSE
                  BIN = 'ERROR - BIN'
    END IF
    !
  END FUNCTION
  !
  PURE FUNCTION int2bin_INT16_GP(VAL32, GROUP) RESULT(BIN)
    INTEGER(INT16),    intent(in):: VAL32
    INTEGER,           intent(in):: GROUP !Set to 0, 4, 8
    CHARACTER(:),     allocatable:: BIN
    CHARACTER(32):: TMP
    INTEGER:: I, J, GP
    INTEGER(INT16):: VAL
    !
    IF    (GROUP > 7) THEN
           GP = 8
    ELSEIF(GROUP < 2) THEN
           GP = 0
    ELSE
           GP = 4
    END IF
    !
    TMP = ""
    VAL = VAL32
    I = 32
    J = GP
    DO WHILE( VAL > ZER16 .AND. I > 0)
        !
        IF(J == 0) THEN
           J = GP
        ELSE
           J = J - 1
           !
           IF(BTEST(VAL, 0)) THEN   ! Odd number if least sig bit is set  -- Old Method: IF(IAND(VAL,ONE16) == ONE16) THEN 
                        TMP(I:I) = "1"
           ELSE
                        TMP(I:I) = "0"
           END IF
           VAL = SHIFTR(VAL, 1)  ! Divide by 2 via bit shift operation
        END IF
        !
        I = I - 1
    END DO
    !
    IF( VAL < ONE16 .AND. I < 32 ) THEN
        IF( J > 0 ) THEN
                  BIN = REPEAT('0',J) // TMP(I+1:32)
        ELSE
                  BIN = TMP(I+1:32)
        END IF
    ELSE
                  BIN = 'ERROR - BIN'
    END IF
    !
  END FUNCTION
  !
  PURE FUNCTION int2bin_INT32_GP(VAL32, GROUP) RESULT(BIN)
    INTEGER(INT32),    intent(in):: VAL32
    INTEGER,           intent(in):: GROUP !Set to 0, 4, 8
    CHARACTER(:),     allocatable:: BIN
    CHARACTER(64):: TMP
    INTEGER:: I, J, GP
    INTEGER(INT32):: VAL
    !
    IF    (GROUP > 7) THEN
           GP = 8
    ELSEIF(GROUP < 2) THEN
           GP = 0
    ELSE
           GP = 4
    END IF
    !
    TMP = ""
    VAL = VAL32
    I = 64
    J = GP
    DO WHILE( VAL > ZER32 .AND. I > 0)
        !
        IF(J == 0) THEN
           J = GP
        ELSE
           J = J - 1
           !
           IF(BTEST(VAL, 0)) THEN   ! Odd number if least sig bit is set  -- Old Method: IF(IAND(VAL,1) == 1) THEN
                        TMP(I:I) = "1"
           ELSE
                        TMP(I:I) = "0"
           END IF
           VAL = SHIFTR(VAL, 1)  ! Divide by 2 via bit shift operation
        END IF
        !
        I = I - 1
    END DO
    !
    IF( VAL < ONE32 .AND. I < 64 ) THEN
        IF( J > 0 ) THEN
                  BIN = REPEAT('0',J) // TMP(I+1:64)
        ELSE
                  BIN = TMP(I+1:64)
        END IF
    ELSE
                  BIN = 'ERROR - BIN'
    END IF
    !
  END FUNCTION
  !
  PURE FUNCTION int2bin_INT64_GP(VAL32, GROUP) RESULT(BIN)
    INTEGER(INT64),    intent(in):: VAL32
    INTEGER,           intent(in):: GROUP !Set to 0, 4, 8
    CHARACTER(:),     allocatable:: BIN
    CHARACTER(96):: TMP
    INTEGER:: I, J, GP
    INTEGER(INT64):: VAL
    !
    IF    (GROUP > 7) THEN
           GP = 8
    ELSEIF(GROUP < 2) THEN
           GP = 0
    ELSE
           GP = 4
    END IF
    !
    TMP = ""
    VAL = VAL32
    I = 96
    J = GP
    DO WHILE( VAL > ZER64 .AND. I > 0)
        !
        IF(J == 0) THEN
           J = GP
        ELSE
           J = J - 1
           !
           IF(BTEST(VAL, 0)) THEN   ! Odd number if least sig bit is set  -- Old Method: IF(IAND(VAL,1) == 1) THEN
                        TMP(I:I) = "1"
           ELSE
                        TMP(I:I) = "0"
           END IF
           VAL = SHIFTR(VAL, 1)  ! Divide by 2 via bit shift operation
        END IF
        !
        I = I - 1
    END DO
    !
    IF( VAL < ONE64 .AND. I < 96 ) THEN
        IF( J > 0 ) THEN
                  BIN = REPEAT('0',J) // TMP(I+1:96)
        ELSE
                  BIN = TMP(I+1:96)
        END IF
    ELSE
                  BIN = 'ERROR - BIN'
    END IF
    !
  END FUNCTION
  !
  PURE FUNCTION int2bin_INT8(VAL8, pad) RESULT(BIN)
    INTEGER(INT8),           intent(in):: VAL8
    LOGICAL,       optional, intent(in):: pad
    CHARACTER(:),  allocatable:: BIN
    CHARACTER(8):: TMP
    INTEGER:: I
    INTEGER(INT8):: VAL
    !
    VAL = VAL8
    I = 8
    DO WHILE( VAL > ZER8 .AND. I > 0)
        !
        IF(BTEST(VAL, 0)) THEN   ! Odd number if least sig bit is set  -- Old Method: IF(IAND(VAL,1) == 1) THEN
                     TMP(I:I) = "1"
        ELSE
                     TMP(I:I) = "0"
        END IF
        !
        I = I - 1
        VAL = SHIFTR(VAL, 1)  ! Divide by 2 via bit shift operation
        !
    END DO
    !
    IF( VAL < 1 .AND. I < 8 ) THEN
                  if(present(pad)) then
                      if(pad) CALL ZeroPad(8, TMP, I)
                  end if
                  BIN = TMP(I+1:8)
    ELSE
                  BIN = 'ERROR - BIN'
    END IF
    !
  END FUNCTION
  !
  PURE FUNCTION int2bin_INT16(VAL16, pad) RESULT(BIN)
    INTEGER(INT16),          intent(in):: VAL16
    LOGICAL,       optional, intent(in):: pad
    CHARACTER(:),  allocatable:: BIN
    CHARACTER(16):: TMP
    INTEGER:: I
    INTEGER(INT16):: VAL
    !
    VAL = VAL16
    I = 16
    DO WHILE( VAL > ZER16 .AND. I > 0)
        !
        IF(BTEST(VAL, 0)) THEN   ! Odd number if least sig bit is set  -- Old Method: IF(IAND(VAL,1) == 1) THEN
                     TMP(I:I) = "1"
        ELSE
                     TMP(I:I) = "0"
        END IF
        !
        I = I - 1
        VAL = SHIFTR(VAL, 1)  ! Divide by 2 via bit shift operation
        !
    END DO
    !
    IF( VAL < ONE16 .AND. I < 16 ) THEN
                  if(present(pad)) then
                      if(pad) CALL ZeroPad(16, TMP, I)
                  end if
                  BIN = TMP(I+1:16)
    ELSE
                  BIN = 'ERROR - BIN'
    END IF
    !
  END FUNCTION
  !
  PURE FUNCTION int2bin_INT32(VAL32, pad) RESULT(BIN)
    INTEGER(INT32),          intent(in):: VAL32
    LOGICAL,       optional, intent(in):: pad
    CHARACTER(:),  allocatable:: BIN
    CHARACTER(32):: TMP
    INTEGER:: I
    INTEGER(INT32):: VAL
    !
    VAL = VAL32
    I = 32
    DO WHILE( VAL > ZER32 .AND. I > 0)
        !
        IF(BTEST(VAL, 0)) THEN   ! Odd number if least sig bit is set  -- Old Method: IF(IAND(VAL,1) == 1) THEN
                     TMP(I:I) = "1"
        ELSE
                     TMP(I:I) = "0"
        END IF
        !
        I = I - 1
        VAL = SHIFTR(VAL, 1)  ! Divide by 2 via bit shift operation
        !
    END DO
    !
    IF( VAL < ONE32 .AND. I < 32 ) THEN
                  if(present(pad)) then
                      if(pad) CALL ZeroPad(32, TMP, I)
                  end if
                  BIN = TMP(I+1:32)
    ELSE
                  BIN = 'ERROR - BIN'
    END IF
    !
  END FUNCTION
  !
  PURE FUNCTION int2bin_INT64(VAL64, pad) RESULT(BIN)
    INTEGER(INT64),          intent(in):: VAL64
    LOGICAL,       optional, intent(in):: pad
    CHARACTER(:),  allocatable:: BIN
    CHARACTER(64):: TMP
    INTEGER:: I
    INTEGER(INT64):: VAL
    !
    VAL = VAL64  ! SHIFTR seems to fail when using VALUE on INTEL OneAPI
    I = 64
    DO WHILE( VAL > ZER64 .AND. I > 0)
        !
        IF(BTEST(VAL, 0)) THEN   ! Odd number if least sig bit is set  -- Old Method: IF(IAND(VAL,ONE64) == ONE64) THEN
                     TMP(I:I) = "1"
        ELSE
                     TMP(I:I) = "0"
        END IF
        !
        I = I - 1
        VAL = SHIFTR(VAL, 1)  ! Divide by 2 via bit shift operation
        !
    END DO
    !
    IF( VAL < ONE64 .AND. I < 64 ) THEN
                  if(present(pad)) then
                      if(pad) CALL ZeroPad(64, TMP, I)
                  end if
                  BIN = TMP(I+1:64)
    ELSE
                  BIN = 'ERROR - BIN'
    END IF
    !
  END FUNCTION
  !
  PURE SUBROUTINE ZeroPad(dim, txt, pnt)
    INTEGER,        intent(in   ):: dim
    CHARACTER(dim), intent(inout):: txt 
    INTEGER,        intent(inout):: pnt
    INTEGER:: I
    !
    DO I=1, pnt
            txt(I:I) = '0'
    END DO
    pnt = 0
    !
  END SUBROUTINE
  !
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !#####################################################################################################
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !
  PURE FUNCTION STR_TO_HEX_BYTES(STR) RESULT(HEX)
    CHARACTER(*),  intent(in):: STR
    CHARACTER(:), allocatable:: HEX
    CHARACTER(16):: TMP
    INTEGER(INT32):: VAL
    INTEGER:: I, J, K, N
    ! 
    TMP = ""
    !
    I = 1
    J = 2
    DO K=1, LEN(STR)
        !
        VAL = TRANSFER(STR(K:K), VAL)
        !
        N   = IAND(VAL, 15_INT32)     ! Get remainder of VAL/16
        IF(N == 0) THEN
             TMP(J:J) = "0"
        ELSE
             TMP(J:J) = HEX_LIST(N:N)
        END IF
        !
        N = SHIFTR(VAL, 4)  ! Divide by 16 via bit shift operation
        IF(N == 0) THEN
             TMP(I:I) = "0"
        ELSE
             TMP(I:I) = HEX_LIST(N:N)
        END IF
        !
        I = J + 2 ! Create a blank space
        J = I + 1
    END DO
    !
    HEX = TRIM(TMP)
    !
  END FUNCTION
  !
  PURE FUNCTION INT8_TO_HEX_BYTES(VAL) RESULT(HEX)  !Need to addresss negative bit - IPNT<0
    INTEGER(INT8), intent(in):: VAL
    CHARACTER(2):: HEX
    INTEGER:: N
    !
    N = IAND(VAL, 15_INT8)     ! Get remainder of VAL/16
    IF(N == 0) THEN
         HEX(2:2) = "0"
    ELSE
         HEX(2:2) = HEX_LIST(N:N)
    END IF
    !
    N = SHIFTR(VAL, 4)  ! Divide by 16 via bit shift operation
    IF(N == 0) THEN
         HEX(1:1) = "0"
    ELSE
         HEX(1:1) = HEX_LIST(N:N)
    END IF
    !
    !
  END FUNCTION
  !
  PURE FUNCTION INT16_TO_HEX_BYTES(VAL) RESULT(HEX)  !Need to addresss negative bit - IPNT<0
    INTEGER(INT16), intent(in):: VAL
    CHARACTER(5):: HEX
    CHARACTER( 2):: STR, TMP
    !
    TMP = TRANSFER(VAL, STR) !Transfer
    !
    ! Byte Endian order may affect TRANSFER.
    !   Little-Endian CPUs reverse the order of the bytes causing UTF8 bytes to be in reverse order
    !   -> Unable to test/validate this concept on Big-Endian CPUs
    !
    IF(Little_Endian) THEN 
       STR(1:1) = TMP(2:2)
       STR(2:2) = TMP(1:1)
    ELSE
       STR = TMP
    END IF
    ! 
    HEX = STR_TO_HEX_BYTES(STR)
    !
  END FUNCTION
  !
  PURE FUNCTION INT32_TO_HEX_BYTES(VAL) RESULT(HEX)  !Need to addresss negative bit - IPNT<0
    INTEGER(INT32), intent(in):: VAL
    CHARACTER(11):: HEX
    CHARACTER( 4):: STR, TMP
    INTEGER:: I, J
    !
    TMP = TRANSFER(VAL, STR) !Transfer
    !
    ! Byte Endian order may affect TRANSFER.
    !   Little-Endian CPUs reverse the order of the bytes causing UTF8 bytes to be in reverse order
    !   -> Unable to test/validate this concept on Big-Endian CPUs
    !
    IF(Little_Endian) THEN 
       J = 4                      
       DO I=1, 4
               STR(J:J) = TMP(I:I)
               J = J - 1
       END DO
    ELSE
       STR = TMP
    END IF
    ! 
    HEX = STR_TO_HEX_BYTES(STR)
    !
  END FUNCTION
  !
  PURE FUNCTION INT64_TO_HEX_BYTES(VAL) RESULT(HEX)  !Need to addresss negative bit - IPNT<0
    INTEGER(INT64), intent(in):: VAL
    CHARACTER(15):: HEX
    CHARACTER( 8):: STR, TMP
    INTEGER:: I, J
    !
    TMP = TRANSFER(VAL, STR) !Transfer
    !
    ! Byte Endian order may affect TRANSFER.
    !   Little-Endian CPUs reverse the order of the bytes causing UTF8 bytes to be in reverse order
    !   -> Unable to test/validate this concept on Big-Endian CPUs
    !
    IF(Little_Endian) THEN 
       J = 8                      
       DO I=1, 8
               STR(J:J) = TMP(I:I)
               J = J - 1
       END DO
    ELSE
       STR = TMP
    END IF
    ! 
    HEX = STR_TO_HEX_BYTES(STR)
    !
  END FUNCTION
  !
END MODULE
!
!
!hex2int('a0') == 160    
!hex2int('a2') == 162    
!hex2int('b3') == 179    
!hex2int('fd') == 253    
!
!oct2int('41'  ) == 33  
!oct2int('6527') == 3415
!oct2int('6530') == 3416
!oct2int('400' ) == 256 
!
!bin2int('10001'         ) == 17
!bin2int('1011101'       ) == 93
!bin2int('10011001100001') == 9825
!bin2int('10011001100010') == 9826
!
!int2hex(160) == 'A0'
!int2hex(162) == 'A2'
!int2hex(179) == 'B3'
!int2hex(253) == 'FD'
!
!int2oct(33  ) == '41'
!int2oct(3415) == '6527'
!int2oct(3416) == '6530'
!int2oct(256 ) == '400'
!
!int2bin(  17) == '10001'         
!int2bin(  93) == '1011101'       
!int2bin(9825) == '10011001100001'
!int2bin(9826) == '10011001100010'