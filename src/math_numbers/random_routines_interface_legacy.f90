!
! This is a legacy random generator that uses Linear Congruential Generator (LCG)
!   However this technique has been found to have issues if uses to generate hundred of thousands
!   of random variables.
!   This routine is kept for legacy purposes and is useful if you need fast generation of <1000 random terms.
!
!
!##################################################################################################
!
! CODE DEVELOPED BY SCOTT E BOYCE 
!                   CONTACT <seboyce@usgs.gov> or <Boyce@engineer.com>
! 
! Linear Congruential Generator (LCG) is an algorithm that yields a sequence of pseudo-randomized numbers 
!   calculated with a discontinuous piecewise linear equation.
!                     => Note random numbers are generated for 2^32 times before the algorithm cycles again.
! 
! Each successive pseudo-random numbers uses the current value of SEED and 
!    transforms it to a new SEED plus a random INT from 0 to 0.5*(2**31)-1     -> 0 to 2147483647; the 0.5 is because Fortran does not handle unsigned integers
!    The INT is then transformed to all other types of random values.
!   
!   LCGs are not intended, and must not be used, for cryptographic applications. 
!     They are best for random number simulations, such as Monte Carlo.
!
!   -> Note this is the method used by Fortran intrinsic CALL RANDOM_NUMBER(RND), 
!        but it is not a PURE routine and only generates a REAL32 number from 0 to 1.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!  It is best to use 
!                   TYPE(RANDOM_GENERATOR):: RG 
!                   CALL RG%INIT() ! Run one time to set a SEED based on the date and time
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!  Provided standalone routines use an internal RANDOM_GENERATOR type that is associated with that routine.
!    On the first call the routine sets the internal generator to a seed based on the 
!    current date and time for the first random generation.
!
!  For example, I = randINT(1,10), will set I to a random value from 1 to 10, inclusive.
!
!  The first call to randINT initializes its internal random_generator (LCG_BETWEEN_INT) 
!    with a SEED based on the date and time during the first call.
!
!   Any routines that accept SEED as an optional argument can override and set the SEED value, 
!     if SEED<2, then it is randomly set based on current date and time.
!
!  The standalone SHUFFLE routine use the native fortran pseudo-random generator (call random_number(rnd))
!    To derive permutations for randomly placing elements in VEC
! 
!    If you want to use this MODULES built in generator, use random_generator%shuffle(A)
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
! Example use:
!
!  TYPE(RANDOM_GENERATOR):: RG 
!  CALL RG%INIT()                   ! Run one time to set a SEED based on the date and time
! 
!  CALL RG%GEN(RAND, [LOWER, UPPER]) ! Set RAND to next pseudo-random number between 0.0 and 1.0, or 0 and 2147483647, or between LOWER and UPPER.
!                                    ! This routine is PURE
! 
!  CALL RG%SHUFFLE(A)                ! Pseudo-random shuffle of the values in A. This routine is PURE
!
!  I = RG%GET_INT   ([LOWER, UPPER]) ! Set I to next pseudo-random number between 0   and 2147483647, or between LOWER and UPPER
!  X = RG%GET_DOUBLE([LOWER, UPPER]) ! Set X to next pseudo-random number between 0.0 and 1.0,        or between LOWER and UPPER
!  X = RG%GET_SINGLE([LOWER, UPPER]) ! Set X to next pseudo-random number between 0.0 and 1.0,        or between LOWER and UPPER
!  I = RG%GET_BINARY()               ! Set I to 0 or 1
!  L = RG%FLIP_COIN ()               ! Set L to .true. or .false.
!
!
MODULE RANDOM_ROUTINES!, ONLY: RANDOM_GENERATOR, SHUFFLE, SET_FORTRAN_SEED, COIN_TOSS, BINARY_TOSS
  USE, INTRINSIC:: ISO_FORTRAN_ENV, ONLY: i8 => INT8,   i16 => INT16,  &
                                         i32 => INT32,  i64 => INT64,  &
                                         SNG => REAL32, DBL => REAL64, &
                                                        QAD => REAL128
  IMPLICIT NONE
  !
  PRIVATE
  PUBLIC:: RANDOM_GENERATOR
  !
  PUBLIC:: SHUFFLE         !CALL SHUFFLE(VEC, [SEED])
  PUBLIC:: COIN_TOSS       !()  RESULT(T or F)
  PUBLIC:: BINARY_TOSS     !()  RESULT(1 or 0)
  !
  PUBLIC:: randARRAY       !(A, [LOWER, UPPER], [SEED])
  PUBLIC:: rand            !([Lower, Upper], [SEED])  0-1 random DBL
  PUBLIC:: randINT         !( Lower, Upper,  [SEED])
  !
  PUBLIC:: SET_FORTRAN_SEED!([SEED])
  !
  !
  ! Constants used internally to module ----------------------------------------------------
  !
  INTEGER(i64), PARAMETER:: Default_seed = 105155045_i64   ! Random number I made up  -- Random seed generation only Only used by PURE routines if the SEED is not yet initialized
  INTEGER(i64), PARAMETER:: Default_M    = 2147483647_i64  ! Values from C18 standard  - Must be a power of 2 minus 1, eg 2^31-1
  INTEGER(i64), PARAMETER:: Default_A    = 1103515245_i64
  INTEGER(i64), PARAMETER:: Default_C    = 12345_i64
  !INTEGER(i64), PARAMETER:: SEED_SHFT    = 1099511627776_i64  ! Value used as IEOR (SEED, SEED_SHFT) when user specifies SEED
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  INTEGER(i32),  PARAMETER:: NEG   = -1_i32
  INTEGER(i32),  PARAMETER:: Z     =  0_i32
  INTEGER(i32),  PARAMETER:: ONE   =  1_i32
  INTEGER(i32),  PARAMETER:: TWO   =  2_i32
  INTEGER(i32),  PARAMETER:: EIGHT =  8_i32
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  REAL(DBL),     PARAMETER:: DZ   =   0_dbl
  REAL(DBL),     PARAMETER:: HALF = 0.5_dbl
  REAL(DBL),     PARAMETER:: UNO  =   1_dbl
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  INTEGER(i64),  PARAMETER:: LONG_NEG    = -1_i64
  INTEGER(i64),  PARAMETER:: LONG_ZER    =  0_i64
  INTEGER(i64),  PARAMETER:: LONG_ONE    =  1_i64
  INTEGER(i64),  PARAMETER:: LONG_TWO    =  2_i64
  INTEGER(i64),  PARAMETER:: LONG_inf_I  =  INT( HUGE(Z), i64)
  !
  ! ----------------------------------------------------------------------------------------
  INTERFACE SHUFFLE              ! SHUFFLE(VEC,SEED)
    MODULE PROCEDURE  INT_SHUFFLE  
    MODULE PROCEDURE DBLE_SHUFFLE
    MODULE PROCEDURE REAL_SHUFFLE
  END INTERFACE
  !
  INTERFACE randARRAY                ! randARRAY(A, LOWER, UPPER, SEED)
    MODULE PROCEDURE randARRAY_INT
    MODULE PROCEDURE randARRAY_DBL
    MODULE PROCEDURE randARRAY_SNG
  END INTERFACE
  ! ----------------------------------------------------------------------------------------
  !
  TYPE RANDOM_GENERATOR                      ! Uses a LINEAR_CONGRUENTIAL_GENERATOR to make random numbers
      !
      INTEGER(i64):: m    = Default_M
      INTEGER(i64):: a    = Default_A
      INTEGER(i64):: c    = Default_C
      INTEGER(i64):: seed = Default_seed     ! Default Seed
      !
      CONTAINS
      !
      PROCEDURE, PASS(LCG):: INIT       => INITIALIZE_LCG                  ! INIT([SEED], [ID], [M], [A], [C])
      PROCEDURE, PASS(LCG):: RESET      => RESET_LCG                       ! RESET()
      ! 
      GENERIC             :: GEN        => GENERATE_RANDOM_INT_0D_LCG, GENERATE_RANDOM_DBL_0D_LCG, GENERATE_RANDOM_SNG_0D_LCG, & ! GEN(RAND, [LOWER, UPPER])
                                           GENERATE_RANDOM_INT_1D_LCG, GENERATE_RANDOM_DBL_1D_LCG, GENERATE_RANDOM_SNG_1D_LCG, &
                                           GENERATE_RANDOM_INT_2D_LCG, GENERATE_RANDOM_DBL_2D_LCG, GENERATE_RANDOM_SNG_2D_LCG, &
                                           GENERATE_RANDOM_INT_3D_LCG, GENERATE_RANDOM_DBL_3D_LCG, GENERATE_RANDOM_SNG_3D_LCG
      !
      PROCEDURE, PASS(LCG):: GET_INT    => GENERATE_RANDOM_INT_LCG         ! GET_INT   ([LOWER, UPPER])
      PROCEDURE, PASS(LCG):: GET_DOUBLE => GENERATE_RANDOM_DBL_LCG         ! GET_DOUBLE([LOWER, UPPER]) 
      PROCEDURE, PASS(LCG):: GET_SINGLE => GENERATE_RANDOM_SNG_LCG         ! GET_SINGLE([LOWER, UPPER]) 
      PROCEDURE, PASS(LCG):: GET_BINARY => GENERATE_RANDOM_BINARY_LCG      ! GET_BINARY() 
      PROCEDURE, PASS(LCG):: FLIP_COIN  => GENERATE_RANDOM_COIN_LCG        ! FLIP_COIN () 
      !
      GENERIC             :: SHUFFLE    => SHUFFLE_INT_1D_LCG, SHUFFLE_DBL_1D_LCG, SHUFFLE_SNG_1D_LCG, &  ! SHUFFLE(A)
                                           SHUFFLE_INT_2D_LCG, SHUFFLE_DBL_2D_LCG, SHUFFLE_SNG_2D_LCG, &
                                           SHUFFLE_INT_3D_LCG, SHUFFLE_DBL_3D_LCG, SHUFFLE_SNG_3D_LCG
      !
      PROCEDURE, PASS(LCG):: SET_SEED   => SET_SEED_LCG                    ! SET_SEED([SEED], [M], [A], [C])                        ! SET_PROP([ID], [SEED], [M], [A], [C])
      !
      GENERIC:: ASSIGNMENT(=)           => COPY_RANDOM_GENERATOR
      !
      PROCEDURE, PASS(LCG), PRIVATE:: GENERATE_RANDOM_INT_0D_LCG, GENERATE_RANDOM_DBL_0D_LCG, GENERATE_RANDOM_SNG_0D_LCG 
      PROCEDURE, PASS(LCG), PRIVATE:: GENERATE_RANDOM_INT_1D_LCG, GENERATE_RANDOM_DBL_1D_LCG, GENERATE_RANDOM_SNG_1D_LCG
      PROCEDURE, PASS(LCG), PRIVATE:: GENERATE_RANDOM_INT_2D_LCG, GENERATE_RANDOM_DBL_2D_LCG, GENERATE_RANDOM_SNG_2D_LCG
      PROCEDURE, PASS(LCG), PRIVATE:: GENERATE_RANDOM_INT_3D_LCG, GENERATE_RANDOM_DBL_3D_LCG, GENERATE_RANDOM_SNG_3D_LCG
      !
      PROCEDURE, PASS(LCG), PRIVATE:: SHUFFLE_INT_1D_LCG, SHUFFLE_DBL_1D_LCG, SHUFFLE_SNG_1D_LCG
      PROCEDURE, PASS(LCG), PRIVATE:: SHUFFLE_INT_2D_LCG, SHUFFLE_DBL_2D_LCG, SHUFFLE_SNG_2D_LCG
      PROCEDURE, PASS(LCG), PRIVATE:: SHUFFLE_INT_3D_LCG, SHUFFLE_DBL_3D_LCG, SHUFFLE_SNG_3D_LCG
      !
      PROCEDURE,            PRIVATE:: COPY_RANDOM_GENERATOR
      !
  END TYPE
  !
  !TYPE(RANDOM_GENERATOR), ALLOCATABLE, SAVE:: LCG_COIN
  !TYPE(RANDOM_GENERATOR), ALLOCATABLE, SAVE:: LCG_BINARY
  !TYPE(RANDOM_GENERATOR), ALLOCATABLE, SAVE:: LCG_BETWEEN_INT
  !TYPE(RANDOM_GENERATOR), ALLOCATABLE, SAVE:: LCG_BETWEEN_DBL
  !TYPE(RANDOM_GENERATOR), ALLOCATABLE, SAVE:: LCG_BETWEEN_ARRAY
  !
  INTRINSIC:: RANDOM_SEED, RANDOM_NUMBER, DATE_AND_TIME
  INTRINSIC:: INT
  INTRINSIC:: REAL
  !	
  CONTAINS
  !
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  ! 
  SUBROUTINE SET_FORTRAN_SEED(SEED)
     INTEGER, optional, intent(in):: SEED
     !
     TYPE(RANDOM_GENERATOR):: LCG
     !
     INTEGER, dimension(:),ALLOCATABLE:: S
     INTEGER:: I, DIM
     !
     CALL RANDOM_SEED (SIZE = DIM)
     ALLOCATE(S(DIM))
     !
     IF(PRESENT(SEED)) THEN
                       S(:) = SEED
     ELSE
                       S(1) = NEG
     END IF
     !
     IF(S(1) < ONE) THEN
         !
         CALL LCG%INIT()
         DO I=ONE, DIM;  S(I) = LCG%GET_INT()
         END DO
     END IF
     !
     CALL RANDOM_SEED (PUT = S)
     !
     DEALLOCATE(S)
     !
  END SUBROUTINE
  !
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  ! 
  !	Given a SEEED, and tran
  !	x => LCG%seed, m => LCG%m, a  => LCG%a, c => LCG%c
  !	
  PURE SUBROUTINE GEN_INTEGER(x, m, a, c, ival)
    INTEGER(i64), intent(inout):: x
    INTEGER(i64), intent(in   ):: m, a, c
    INTEGER(I32), intent(  out):: ival
    !
    IF(x < LONG_TWO) x = Default_seed
    !
    !X1 = MODULO(x*a + c, m+LONG_ONE)
    x = IAND(x*a + c, m)             !Because M is a power of 2 minus 1, can use IAND in place of MOD()
    !
    IF(x < LONG_ZER) x = LONG_NEG*x
    !
    !!!IF       (x > LONG_inf_I) THEN  !Ensure that X is within INT32 positive range
    !!!DO WHILE (x > LONG_inf_I)
    !!!                    x = IAND(x*a + c, m)
    !!!                    !
    !!!                    IF(x < LONG_ZER) x = LONG_NEG*x
    !!!END DO
    !!!END IF
    !!!!
    !!!ival = INT(x, i32)
    !
    ! use bit mask instead
    ival = INT( IAND(x, z'7FFFFFFF'), i32)
    !                          seed   *     c     -   a
    IF(x < LONG_TWO) x = Default_seed * Default_C - Default_A !Re-init random seed
    !
  END SUBROUTINE
  !
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  ! 
  SUBROUTINE INITIALIZE_LCG(LCG, SEED, ID, M, A, C)
    CLASS(RANDOM_GENERATOR), intent(inout):: LCG
    CLASS(*), optional,      intent(in   ):: SEED  ! If specified then defines the seed, if < 1 or not specified, then randomly generates SEED based in wall tine and PID. Set > 0 for repeatability, disables use of ID)
    CLASS(*), optional,      intent(in   ):: ID    ! If returned then makes SEED unique to ID (Disables odd-even random shift)
    CLASS(*), optional,      intent(in   ):: M     ! Set range of random numbers, must be a power of 2 minus one (default is 2^31 - 1)
    CLASS(*), optional,      intent(in   ):: A, C  ! Overrules default values
    !
    CALL RESET_LCG(LCG)
    !
    CALL SET_RANDOM_PROPERTIES_LCG(LCG, ID, SEED, M, A, C)
    !  
  END SUBROUTINE
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
  PURE ELEMENTAL SUBROUTINE RESET_LCG(LCG)
    CLASS(RANDOM_GENERATOR), intent(inout):: LCG
    !
    LCG%m    = Default_M
    LCG%a    = Default_A
    LCG%c    = Default_C
    LCG%seed = Default_seed
    !  
  END SUBROUTINE
  !
  !#############################################################################################################################################################
  !	
  SUBROUTINE SET_SEED_LCG(LCG, SEED, M, A, C)
    CLASS(RANDOM_GENERATOR), intent(inout):: LCG
    CLASS(*), optional,      intent(in   ):: SEED  ! If specified then defines the seed, if < 1 or not specified, then randomly generates SEED based in wall tine and PID. Set > 0 for repeatability, disables use of ID)
    CLASS(*), optional,      intent(in   ):: M     ! Set range of random numbers, must be a power of 2 minus one (default is 2^31 - 1)
    CLASS(*), optional,      intent(in   ):: A, C  ! Overrules default values
    !
    IF(PRESENT(SEED)) THEN
         SELECT TYPE(SEED)
         TYPE IS (INTEGER(i32)); LCG%seed = INT(SEED,i64) !IEOR(INT(SEED,i64), SEED_SHFT)  ! 
         TYPE IS (INTEGER(i64)); LCG%seed = SEED          !IEOR(SEED,          SEED_SHFT)  ! 
         END SELECT
         IF(LCG%seed < LONG_ONE) CALL SET_RANDOM_PROPERTIES_LCG(LCG, SEED, 1, M, A, C)
    END IF
    !
    IF(PRESENT(M)) THEN
         SELECT TYPE(M)
         TYPE IS (INTEGER(i32)); LCG%M = INT(M,i64)
         TYPE IS (INTEGER(i64)); LCG%M = M
         END SELECT
         !
         BLOCK
             INTEGER(i64):: CNT
             !CNT = LOG2(LCG%M)
             !
             CNT = 1024_i64;   IF(LCG%M < CNT) LCG%M = CNT  !should be at least 2^10 = 1024
             !
             LCG%M = LCG%M + LONG_ONE
             !
             CNT   = LONG_NEG
             DO WHILE (LCG%M > 0)
                               CNT = CNT + LONG_ONE
                               LCG%M = SHIFTR( LCG%M, LONG_ONE )  !Drop out the right most bit
             ENDDO
             !
             LCG%M = LONG_TWO**CNT - LONG_ONE   !Ensure it is a power of 2
         END BLOCK
    END IF
    !
    IF(PRESENT(A)) THEN
         SELECT TYPE(A)
         TYPE IS (INTEGER(i32)); LCG%A = INT(A,i64)
         TYPE IS (INTEGER(i64)); LCG%A = A
         END SELECT
    END IF
    !
    IF(PRESENT(C)) THEN
         SELECT TYPE(C)
         TYPE IS (INTEGER(i32)); LCG%C = INT(C,i64)
         TYPE IS (INTEGER(i64)); LCG%C = C
         END SELECT
    END IF
    !
  END SUBROUTINE
  !
  ! ------------------------------------------------------------------------------------------------------------------------------------------------------------
  !	
  SUBROUTINE SET_RANDOM_PROPERTIES_LCG(LCG, SEED, ID, M, A, C)
    CLASS(RANDOM_GENERATOR), intent(inout):: LCG
    CLASS(*), optional,      intent(in   ):: SEED  ! If specified then defines the seed, if < 1 or not specified, then randomly generates SEED based in wall tine and PID. Set > 0 for repeatability, disables use of ID)
    CLASS(*), optional,      intent(in   ):: ID    ! If returned then makes SEED unique to ID (Disables odd-even random shift)
    CLASS(*), optional,      intent(in   ):: M     ! Set range of random numbers, must be a power of 2 minus one (default is 2^31 - 1)
    CLASS(*), optional,      intent(in   ):: A, C  ! Overrules default values
    !
    INTEGER(i64):: YER, SEC, MS, CNT, PID
    !
    INTEGER, dimension(EIGHT):: T
    INTEGER:: I, J
    CHARACTER(EIGHT):: BYT
    CHARACTER(TWO  ):: TXT
    !
    IF(PRESENT(ID)) THEN
         SELECT TYPE(ID)
         TYPE IS (INTEGER(i32)); PID = INT(ID,i64)
         TYPE IS (INTEGER(i64)); PID = ID
         END SELECT
         !
         IF(PID < LONG_ONE) PID = LONG_ONE
    ELSE
            PID = LONG_ONE
    END IF
    !
    IF(PRESENT(M)) THEN
         SELECT TYPE(M)
         TYPE IS (INTEGER(i32)); LCG%M = INT(M,i64)
         TYPE IS (INTEGER(i64)); LCG%M = M
         END SELECT
         !
         !CNT = LOG2(LCG%M)
         !
         CNT = 1024_i64;   IF(LCG%M < CNT) LCG%M = CNT  !should be at least 2^10 = 1024
         !
         LCG%M = LCG%M + LONG_ONE
         !
         CNT   = LONG_NEG
         DO WHILE (LCG%M > 0)
                           CNT = CNT + LONG_ONE
                           LCG%M = SHIFTR( LCG%M, LONG_ONE )  !Drop out the right most bit
         ENDDO
         !
         LCG%M = LONG_TWO**CNT - LONG_ONE   !Ensure it is a power of 2
    END IF
    !
    IF(PRESENT(A)) THEN
         SELECT TYPE(A)
         TYPE IS (INTEGER(i32)); LCG%A = INT(A,i64)
         TYPE IS (INTEGER(i64)); LCG%A = A
         END SELECT
    END IF
    !
    IF(PRESENT(C)) THEN
         SELECT TYPE(C)
         TYPE IS (INTEGER(i32)); LCG%C = INT(C,i64)
         TYPE IS (INTEGER(i64)); LCG%C = C
         END SELECT
    END IF
    !
    IF(PRESENT(SEED)) THEN
         SELECT TYPE(SEED)
         TYPE IS (INTEGER(i32)); LCG%seed = INT(SEED,i64) !IEOR(INT(SEED,i64), SEED_SHFT)  !
         TYPE IS (INTEGER(i64)); LCG%seed = SEED          !IEOR(SEED,          SEED_SHFT)  !
         END SELECT
         IF( PRESENT(ID) .AND. LCG%seed > LONG_ONE ) LCG%seed = LCG%seed + PID
    END IF
    !
    IF( .NOT. PRESENT(SEED) .OR. LCG%seed < LONG_TWO ) THEN
        ASSOCIATE( X => LCG%seed )
           CALL DATE_AND_TIME(VALUES = T)  !GET CURRENT TIME VALUES
           !
           YER =       INT(T(1), i64)
           CNT =       INT(T(2), i64)*25920000_i64   !MON
           CNT = CNT + INT(T(3), i64)*8640000_i64    !DAY
           CNT = CNT + INT(T(5), i64)*3600000_i64    !HR 
           CNT = CNT + INT(T(6), i64)*60000_i64      !MIN
           SEC =       INT(T(7), i64)*1000_i64
           MS  =       INT(T(8), i64)
           !
           X = YER*SEC*PID + SEC*MS + MS + PID
           !
           IF( BTEST(T(8), Z) .AND. PID > LONG_ONE) THEN      !MS is odd
               BYT = TRANSFER(X, BYT)                         !Shift Bytes around for fun
               !
               TXT  = BYT(1:2)
               BYT(1:2) = BYT(6:7)
               BYT(6:7) = TXT
               !
               TXT  = BYT(5:6)
               BYT(5:6) = BYT(7:8)
               BYT(7:8) = TXT
               !
               DO J=TWO, EIGHT, TWO
                   I = J - ONE
                   !
                   TXT(1:1)  = BYT(J:J)
                   BYT(J:J) = BYT(I:I)
                   BYT(I:I) = TXT(1:1)
               END DO
               !
               X = TRANSFER(BYT, X)
           END IF
           !
           IF(X < LONG_ZER) X = LONG_NEG*X
           !
           IF(X < 10000_i64) X = X*10000_i64
           !
        END ASSOCIATE
    END IF
    !  
  END SUBROUTINE
  !
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  ! 
  PURE SUBROUTINE GENERATE_RANDOM_INT_0D_LCG(LCG, IRAND, LOWER, UPPER)
    CLASS(RANDOM_GENERATOR), intent(inout):: LCG
    INTEGER(i32),            intent(  out):: IRAND
    INTEGER(i32),  optional, intent(in   ):: LOWER, UPPER
    !
    CALL GEN_INTEGER(LCG%seed, LCG%m, LCG%a, LCG%c, IRAND)
    !
    IF( PRESENT(LOWER) .AND. PRESENT(UPPER) ) THEN
        BLOCK
            REAL(DBL):: RAND, RANGE
            !
            RANGE = REAL(UPPER - LOWER + ONE, DBL)
            RAND  = REAL(IRAND, DBL) / REAL( LCG%m+LONG_ONE, DBL )
            !
            IF(RAND > UNO) RAND = UNO
            IF(RAND <  DZ) RAND = DZ
            !
            IRAND = LOWER + INT( RAND*RANGE, I32)
        END BLOCK
    ELSEIF(PRESENT(LOWER)) THEN
                           IRAND = IRAND + LOWER
    END IF
    !  
  END SUBROUTINE
  !	
  ! ------------------------------------------------------------------------------------------------------------------------------------------------------------
  !	
  FUNCTION GENERATE_RANDOM_INT_LCG(LCG, LOWER, UPPER)  RESULT(IRAND)
    CLASS(RANDOM_GENERATOR), intent(inout):: LCG
    INTEGER(i32),  optional, intent(in   ):: LOWER, UPPER
    INTEGER:: IRAND
    !
    CALL GENERATE_RANDOM_INT_0D_LCG(LCG, IRAND, LOWER, UPPER)
    !
  END FUNCTION
  !	
  ! ------------------------------------------------------------------------------------------------------------------------------------------------------------
  !	
  PURE SUBROUTINE GENERATE_RANDOM_DBL_0D_LCG(LCG, RAND, LOWER, UPPER)
    CLASS(RANDOM_GENERATOR), intent(inout):: LCG
    REAL(DBL),               intent(  out):: RAND
    REAL(DBL),     optional, intent(in   ):: LOWER, UPPER
    INTEGER(i32):: IRAND
    !
    CALL GEN_INTEGER(LCG%seed, LCG%m, LCG%a, LCG%c, IRAND)
    !
    RAND = REAL(IRAND, DBL) / REAL( LCG%m+LONG_ONE, DBL )
    !
    IF(RAND > UNO) RAND = UNO
    IF(RAND <  DZ) RAND = DZ
    !
    IF( PRESENT(LOWER) .AND. PRESENT(UPPER) ) THEN
                                     RAND = LOWER + RAND*(UPPER - LOWER + UNO)
    ELSEIF(PRESENT(LOWER)) THEN
                           RAND = RAND + LOWER
    END IF
    !  
  END SUBROUTINE
  !	
  ! ------------------------------------------------------------------------------------------------------------------------------------------------------------
  !	
  FUNCTION GENERATE_RANDOM_DBL_LCG(LCG, LOWER, UPPER)  RESULT(RAND)
    CLASS(RANDOM_GENERATOR), intent(inout):: LCG
    REAL(DBL),     optional, intent(in   ):: LOWER, UPPER
    REAL(DBL):: RAND
    !
    CALL GENERATE_RANDOM_DBL_0D_LCG(LCG, RAND, LOWER, UPPER)
    !
  END FUNCTION
  !	
  ! ------------------------------------------------------------------------------------------------------------------------------------------------------------
  !	
  PURE SUBROUTINE GENERATE_RANDOM_SNG_0D_LCG(LCG, RAND, LOWER, UPPER)
    CLASS(RANDOM_GENERATOR), intent(inout):: LCG
    REAL(SNG),               intent(  out):: RAND
    REAL(SNG),     optional, intent(in   ):: LOWER, UPPER
    REAL(DBL)   :: DRAND
    !
    CALL GENERATE_RANDOM_DBL_0D_LCG(LCG, DRAND)
    !
    RAND = REAL(DRAND, SNG)
    !
    IF( PRESENT(LOWER) .AND. PRESENT(UPPER) ) THEN
                                     RAND = LOWER + RAND*(UPPER - LOWER + 1.0_SNG)
    ELSEIF(PRESENT(LOWER)) THEN
                           RAND = RAND + LOWER
    END IF
    !  
  END SUBROUTINE
  !
  ! ------------------------------------------------------------------------------------------------------------------------------------------------------------
  !	
  FUNCTION GENERATE_RANDOM_SNG_LCG(LCG, LOWER, UPPER)  RESULT(RAND)
    CLASS(RANDOM_GENERATOR), intent(inout):: LCG
    REAL(SNG),     optional, intent(in   ):: LOWER, UPPER
    REAL(SNG):: RAND
    !
    CALL GENERATE_RANDOM_SNG_0D_LCG(LCG, RAND, LOWER, UPPER)
    !
  END FUNCTION
  !	
  ! ------------------------------------------------------------------------------------------------------------------------------------------------------------
  !	
  FUNCTION GENERATE_RANDOM_COIN_LCG(LCG)  RESULT(COIN)
    CLASS(RANDOM_GENERATOR), intent(inout):: LCG
    LOGICAL:: COIN
    INTEGER(i32):: IRAND
    !
    CALL GEN_INTEGER(LCG%seed, LCG%m, LCG%a, LCG%c, IRAND)
    !
    COIN = IRAND <  INT( SHIFTR(LCG%m+LONG_ONE, 1), I32)  ! If < 0.5, then coint is True
    !
  END FUNCTION
  !	
  ! ------------------------------------------------------------------------------------------------------------------------------------------------------------
  !	
  FUNCTION GENERATE_RANDOM_BINARY_LCG(LCG)  RESULT(BIN)
    CLASS(RANDOM_GENERATOR), intent(inout):: LCG
    INTEGER:: BIN
    !
    IF(GENERATE_RANDOM_COIN_LCG(LCG)) THEN
          BIN = ONE
    ELSE
          BIN = Z
    END IF
    !
  END FUNCTION
  !
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  ! Generate Random Values for Arrays
  !	
  PURE SUBROUTINE GENERATE_RANDOM_INT_1D_LCG(LCG, A, LOWER, UPPER)
    CLASS(RANDOM_GENERATOR),                intent(inout):: LCG
    INTEGER(I32), dimension(:), contiguous, intent(inout):: A
    INTEGER(I32),                 optional, intent(in   ):: LOWER, UPPER
    INTEGER:: I
    !
    DO I=1, SIZE(A)
            CALL GENERATE_RANDOM_INT_0D_LCG(LCG, A(I), LOWER, UPPER)
    END DO
    !  
  END SUBROUTINE
  !	
  ! ------------------------------------------------------------------------------------------------------------------------------------------------------------
  !	
  PURE SUBROUTINE GENERATE_RANDOM_INT_2D_LCG(LCG, A, LOWER, UPPER)
    CLASS(RANDOM_GENERATOR),                  intent(inout):: LCG
    INTEGER(I32), dimension(:,:), contiguous, intent(inout):: A
    INTEGER(I32),                   optional, intent(in   ):: LOWER, UPPER
    INTEGER:: I, J
    !
    DO J=1, SIZE(A,2)
    DO I=1, SIZE(A,1)
            CALL GENERATE_RANDOM_INT_0D_LCG(LCG, A(I,J), LOWER, UPPER)
    END DO
    END DO
    !  
  END SUBROUTINE
  !	
  ! ------------------------------------------------------------------------------------------------------------------------------------------------------------
  !	
  PURE SUBROUTINE GENERATE_RANDOM_INT_3D_LCG(LCG, A, LOWER, UPPER)
    CLASS(RANDOM_GENERATOR),                    intent(inout):: LCG
    INTEGER(I32), dimension(:,:,:), contiguous, intent(inout):: A
    INTEGER(I32),                     optional, intent(in   ):: LOWER, UPPER
    INTEGER:: I, J, K
    !
    DO K=1, SIZE(A,3)
    DO J=1, SIZE(A,2)
    DO I=1, SIZE(A,1)
            CALL GENERATE_RANDOM_INT_0D_LCG(LCG, A(I,J,K), LOWER, UPPER)
    END DO
    END DO
    END DO
    !  
  END SUBROUTINE
  !
  !#############################################################################################################################################################
  !	
  PURE SUBROUTINE GENERATE_RANDOM_DBL_1D_LCG(LCG, A, LOWER, UPPER)
    CLASS(RANDOM_GENERATOR),             intent(inout):: LCG
    REAL(DBL), dimension(:), contiguous, intent(inout):: A
    REAL(DBL),                 optional, intent(in   ):: LOWER, UPPER
    INTEGER:: I
    !
    DO I=1, SIZE(A)
            CALL GENERATE_RANDOM_DBL_0D_LCG(LCG, A(I), LOWER, UPPER)
    END DO
    !  
  END SUBROUTINE
  !	
  ! ------------------------------------------------------------------------------------------------------------------------------------------------------------
  !	
  PURE SUBROUTINE GENERATE_RANDOM_DBL_2D_LCG(LCG, A, LOWER, UPPER)
    CLASS(RANDOM_GENERATOR),               intent(inout):: LCG
    REAL(DBL), dimension(:,:), contiguous, intent(inout):: A
    REAL(DBL),                   optional, intent(in   ):: LOWER, UPPER
    INTEGER:: I, J
    !
    DO J=1, SIZE(A,2)
    DO I=1, SIZE(A,1)
            CALL GENERATE_RANDOM_DBL_0D_LCG(LCG, A(I,J), LOWER, UPPER)
    END DO
    END DO
    !  
  END SUBROUTINE
  !	
  ! ------------------------------------------------------------------------------------------------------------------------------------------------------------
  !	
  PURE SUBROUTINE GENERATE_RANDOM_DBL_3D_LCG(LCG, A, LOWER, UPPER)
    CLASS(RANDOM_GENERATOR),                 intent(inout):: LCG
    REAL(DBL), dimension(:,:,:), contiguous, intent(inout):: A
    REAL(DBL),                     optional, intent(in   ):: LOWER, UPPER
    INTEGER:: I, J, K
    !
    DO K=1, SIZE(A,3)
    DO J=1, SIZE(A,2)
    DO I=1, SIZE(A,1)
            CALL GENERATE_RANDOM_DBL_0D_LCG(LCG, A(I,J,K), LOWER, UPPER)
    END DO
    END DO
    END DO
    !  
  END SUBROUTINE
  !
  !#############################################################################################################################################################
  !	
  PURE SUBROUTINE GENERATE_RANDOM_SNG_1D_LCG(LCG, A, LOWER, UPPER)
    CLASS(RANDOM_GENERATOR),             intent(inout):: LCG
    REAL(SNG), dimension(:), contiguous, intent(inout):: A
    REAL(SNG),                 optional, intent(in   ):: LOWER, UPPER
    INTEGER:: I
    !
    DO I=1, SIZE(A)
            CALL GENERATE_RANDOM_SNG_0D_LCG(LCG, A(I), LOWER, UPPER)
    END DO
    !  
  END SUBROUTINE
  !	
  ! ------------------------------------------------------------------------------------------------------------------------------------------------------------
  !	
  PURE SUBROUTINE GENERATE_RANDOM_SNG_2D_LCG(LCG, A, LOWER, UPPER)
    CLASS(RANDOM_GENERATOR),               intent(inout):: LCG
    REAL(SNG), dimension(:,:), contiguous, intent(inout):: A
    REAL(SNG),                   optional, intent(in   ):: LOWER, UPPER
    INTEGER:: I, J
    !
    DO J=1, SIZE(A,2)
    DO I=1, SIZE(A,1)
            CALL GENERATE_RANDOM_SNG_0D_LCG(LCG, A(I,J), LOWER, UPPER)
    END DO
    END DO
    !  
  END SUBROUTINE
  !	
  ! ------------------------------------------------------------------------------------------------------------------------------------------------------------
  !	
  PURE SUBROUTINE GENERATE_RANDOM_SNG_3D_LCG(LCG, A, LOWER, UPPER)
    CLASS(RANDOM_GENERATOR),                 intent(inout):: LCG
    REAL(SNG), dimension(:,:,:), contiguous, intent(inout):: A
    REAL(SNG),                     optional, intent(in   ):: LOWER, UPPER
    INTEGER:: I, J, K
    !
    DO K=1, SIZE(A,3)
    DO J=1, SIZE(A,2)
    DO I=1, SIZE(A,1)
            CALL GENERATE_RANDOM_SNG_0D_LCG(LCG, A(I,J,K), LOWER, UPPER)
    END DO
    END DO
    END DO
    !  
  END SUBROUTINE
  !
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  ! Shuffle values in array -> Modified SATTOLO Shuffle Method  
  !		
  PURE SUBROUTINE SHUFFLE_INT_1D_LCG(LCG, A)
    CLASS(RANDOM_GENERATOR),                intent(inout):: LCG
    INTEGER(I32), dimension(:), contiguous, intent(inout):: A
    INTEGER(I32):: T
    INTEGER:: I, J, DIM
    !
    DIM = size(A)
    DO I=DIM, TWO, NEG
        !
        CALL GEN_INTEGER(LCG%seed, LCG%m, LCG%a, LCG%c, J)
        !
        J = MODULO(J, DIM) + ONE
        !
        T    = A(I)
        A(I) = A(J)
        A(J) = T
        !
    END DO
    !  
  END SUBROUTINE
  !	
  ! ------------------------------------------------------------------------------------------------------------------------------------------------------------
  !	
  PURE SUBROUTINE SHUFFLE_DBL_1D_LCG(LCG, A)
    CLASS(RANDOM_GENERATOR),             intent(inout):: LCG
    REAL(DBL), dimension(:), contiguous, intent(inout):: A
    REAL(DBL):: T
    INTEGER:: I, J, DIM
    !
    DIM = size(A)
    DO I=DIM, TWO, NEG
        !
        CALL GEN_INTEGER(LCG%seed, LCG%m, LCG%a, LCG%c, J)
        !
        J = MODULO(J, DIM) + ONE
        !
        T    = A(I)
        A(I) = A(J)
        A(J) = T
        !
    END DO
    !  
  END SUBROUTINE
  !	
  ! ------------------------------------------------------------------------------------------------------------------------------------------------------------
  !	
  PURE SUBROUTINE SHUFFLE_SNG_1D_LCG(LCG, A)
    CLASS(RANDOM_GENERATOR),             intent(inout):: LCG
    REAL(SNG), dimension(:), contiguous, intent(inout):: A
    REAL(SNG):: T
    INTEGER:: I, J, DIM
    !
    DIM = size(A)
    DO I=DIM, TWO, NEG
        !
        CALL GEN_INTEGER(LCG%seed, LCG%m, LCG%a, LCG%c, J)
        !
        J = MODULO(J, DIM) + ONE
        !
        T    = A(I)
        A(I) = A(J)
        A(J) = T
        !
    END DO
    !  
  END SUBROUTINE
  !	
  ! ------------------------------------------------------------------------------------------------------------------------------------------------------------
  !	
  PURE SUBROUTINE SHUFFLE_INT_2D_LCG(LCG, A)
    CLASS(RANDOM_GENERATOR),                  intent(inout):: LCG
    INTEGER(I32), dimension(:,:), contiguous, intent(inout):: A
    INTEGER:: DIM
    !
    DIM = size(A)
    CALL SHUFFLE_INT_DIM_LCG(LCG, DIM, A)
    !  
  END SUBROUTINE
  ! ------------------------------------------------------------------------------------------------------------------------------------------------------------
  PURE SUBROUTINE SHUFFLE_INT_3D_LCG(LCG, A)
    CLASS(RANDOM_GENERATOR),                    intent(inout):: LCG
    INTEGER(I32), dimension(:,:,:), contiguous, intent(inout):: A
    INTEGER:: DIM
    !
    DIM = size(A)
    CALL SHUFFLE_INT_DIM_LCG(LCG, DIM, A)
    !  
  END SUBROUTINE
  ! ------------------------------------------------------------------------------------------------------------------------------------------------------------
  PURE SUBROUTINE SHUFFLE_INT_DIM_LCG(LCG, DIM, A)
    CLASS(RANDOM_GENERATOR),      intent(inout):: LCG
    INTEGER,                      intent(in   ):: DIM
    INTEGER(I32), dimension(DIM), intent(inout):: A
    INTEGER(I32):: T
    INTEGER:: I, J
    !
    DO I=DIM, TWO, NEG
        !
        CALL GEN_INTEGER(LCG%seed, LCG%m, LCG%a, LCG%c, J)
        !
        J = MODULO(J, DIM) + ONE
        !
        T    = A(I)
        A(I) = A(J)
        A(J) = T
        !
    END DO
    !  
  END SUBROUTINE
  !	
  ! ------------------------------------------------------------------------------------------------------------------------------------------------------------
  !	
  PURE SUBROUTINE SHUFFLE_DBL_2D_LCG(LCG, A)
    CLASS(RANDOM_GENERATOR),                  intent(inout):: LCG
    REAL(DBL), dimension(:,:), contiguous, intent(inout):: A
    INTEGER:: DIM
    !
    DIM = size(A)
    CALL SHUFFLE_DBL_DIM_LCG(LCG, DIM, A)
    !  
  END SUBROUTINE
  ! ------------------------------------------------------------------------------------------------------------------------------------------------------------
  PURE SUBROUTINE SHUFFLE_DBL_3D_LCG(LCG, A)
    CLASS(RANDOM_GENERATOR),                    intent(inout):: LCG
    REAL(DBL), dimension(:,:,:), contiguous, intent(inout):: A
    INTEGER:: DIM
    !
    DIM = size(A)
    CALL SHUFFLE_DBL_DIM_LCG(LCG, DIM, A)
    !  
  END SUBROUTINE
  ! ------------------------------------------------------------------------------------------------------------------------------------------------------------
  PURE SUBROUTINE SHUFFLE_DBL_DIM_LCG(LCG, DIM, A)
    CLASS(RANDOM_GENERATOR),      intent(inout):: LCG
    INTEGER,                      intent(in   ):: DIM
    REAL(DBL), dimension(DIM), intent(inout):: A
    REAL(DBL):: T
    INTEGER:: I, J
    !
    DO I=DIM, TWO, NEG
        !
        CALL GEN_INTEGER(LCG%seed, LCG%m, LCG%a, LCG%c, J)
        !
        J = MODULO(J, DIM) + ONE
        !
        T    = A(I)
        A(I) = A(J)
        A(J) = T
        !
    END DO
    !  
  END SUBROUTINE
  !	
  ! ------------------------------------------------------------------------------------------------------------------------------------------------------------
  !	
  PURE SUBROUTINE SHUFFLE_SNG_2D_LCG(LCG, A)
    CLASS(RANDOM_GENERATOR),                  intent(inout):: LCG
    REAL(SNG), dimension(:,:), contiguous, intent(inout):: A
    INTEGER:: DIM
    !
    DIM = size(A)
    CALL SHUFFLE_SNG_DIM_LCG(LCG, DIM, A)
    !  
  END SUBROUTINE
  ! ------------------------------------------------------------------------------------------------------------------------------------------------------------
  PURE SUBROUTINE SHUFFLE_SNG_3D_LCG(LCG, A)
    CLASS(RANDOM_GENERATOR),                    intent(inout):: LCG
    REAL(SNG), dimension(:,:,:), contiguous, intent(inout):: A
    INTEGER:: DIM
    !
    DIM = size(A)
    CALL SHUFFLE_SNG_DIM_LCG(LCG, DIM, A)
    !  
  END SUBROUTINE
  ! ------------------------------------------------------------------------------------------------------------------------------------------------------------
  PURE SUBROUTINE SHUFFLE_SNG_DIM_LCG(LCG, DIM, A)
    CLASS(RANDOM_GENERATOR),      intent(inout):: LCG
    INTEGER,                      intent(in   ):: DIM
    REAL(SNG), dimension(DIM), intent(inout):: A
    REAL(SNG):: T
    INTEGER:: I, J
    !
    DO I=DIM, TWO, NEG
        !
        CALL GEN_INTEGER(LCG%seed, LCG%m, LCG%a, LCG%c, J)
        !
        J = MODULO(J, DIM) + ONE
        !
        T    = A(I)
        A(I) = A(J)
        A(J) = T
        !
    END DO
    !  
  END SUBROUTINE
  !
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  ! Util Routines
  !
  ELEMENTAL PURE SUBROUTINE COPY_RANDOM_GENERATOR(LCG_OUT,LCG_IN)
    CLASS(RANDOM_GENERATOR), intent(in   ):: LCG_IN
    CLASS(RANDOM_GENERATOR), intent(inout):: LCG_OUT
    !
    LCG_OUT%m    = LCG_IN%m   
    LCG_OUT%a    = LCG_IN%a   
    LCG_OUT%c    = LCG_IN%c   
    LCG_OUT%seed = LCG_IN%seed
    !
  END SUBROUTINE
  !
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  ! 
  FUNCTION COIN_TOSS(SEED)  RESULT(TIS_TRUE)
    !SEED<0 IS RANDOM SEED
    INTEGER,  optional, intent(in):: SEED
    LOGICAL:: TIS_TRUE
    TYPE(RANDOM_GENERATOR), ALLOCATABLE, SAVE:: LCG_COIN
    !
    IF( .NOT. ALLOCATED(LCG_COIN) ) THEN
                                    ALLOCATE( LCG_COIN )
                                    CALL LCG_COIN%INIT()
    END IF
    IF(PRESENT(SEED)) CALL LCG_COIN%SET_SEED(SEED)
    !
    TIS_TRUE = LCG_COIN%FLIP_COIN()
    !
  END FUNCTION
  !
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  ! 
  FUNCTION BINARY_TOSS(SEED)  RESULT(BINARY_RESULT)
    !SEED<0 IS RANDOM SEED
    INTEGER,  optional, intent(in):: SEED
    INTEGER:: BINARY_RESULT
    TYPE(RANDOM_GENERATOR), ALLOCATABLE, SAVE:: LCG_BINARY
    !
    IF( .NOT. ALLOCATED(LCG_BINARY) ) THEN
                                    ALLOCATE( LCG_BINARY )
                                    CALL LCG_BINARY%INIT()
    END IF
    IF(PRESENT(SEED)) CALL LCG_BINARY%SET_SEED(SEED)
    !
    BINARY_RESULT = LCG_BINARY%GET_BINARY()
    !
  END FUNCTION
  !
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  ! 
  FUNCTION randInt(LOWER, UPPER, SEED)  RESULT(ival)
    !
    INTEGER(i32),      intent(in):: LOWER, UPPER
    INTEGER, optional, intent(in):: SEED
    INTEGER(i32):: ival
    TYPE(RANDOM_GENERATOR), ALLOCATABLE, SAVE:: LCG_BETWEEN_INT
    !
    IF( .NOT. ALLOCATED(LCG_BETWEEN_INT) ) THEN
                                    ALLOCATE( LCG_BETWEEN_INT )
                                    CALL LCG_BETWEEN_INT%INIT()
    END IF
    IF(PRESENT(SEED)) CALL LCG_BETWEEN_INT%SET_SEED(SEED)
    !
    CALL LCG_BETWEEN_INT%GEN(ival, LOWER, UPPER)
    !
  END FUNCTION
  !	
  ! ------------------------------------------------------------------------------------------------------------------------------------------------------------
  !	
  FUNCTION rand(LOWER, UPPER, SEED)  RESULT(val)
    !
    REAL(DBL), optional, intent(in):: LOWER, UPPER
    INTEGER,   optional, intent(in):: SEED
    REAL(DBL):: val
    TYPE(RANDOM_GENERATOR), ALLOCATABLE, SAVE:: LCG_BETWEEN_DBL
    !
    IF( .NOT. ALLOCATED(LCG_BETWEEN_DBL) ) THEN
                                    ALLOCATE( LCG_BETWEEN_DBL )
                                    CALL LCG_BETWEEN_DBL%INIT()
    END IF
    IF(PRESENT(SEED)) CALL LCG_BETWEEN_DBL%SET_SEED(SEED)
    !
    CALL LCG_BETWEEN_DBL%GEN(val, LOWER, UPPER)
    !
  END FUNCTION
  !
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  ! 
  SUBROUTINE randARRAY_INT(A, LOWER, UPPER, SEED)
    INTEGER(I32), dimension(:), contiguous, intent(inout):: A
    INTEGER(I32),                 optional, intent(in   ):: LOWER, UPPER
    INTEGER,                      optional, intent(in   ):: SEED
    INTEGER:: I
    TYPE(RANDOM_GENERATOR), ALLOCATABLE, SAVE:: LCG_BETWEEN_ARRAY
    !
    IF( .NOT. ALLOCATED(LCG_BETWEEN_ARRAY) ) THEN
                                    ALLOCATE( LCG_BETWEEN_ARRAY )
                                    CALL LCG_BETWEEN_ARRAY%INIT()
    END IF
    IF(PRESENT(SEED)) CALL LCG_BETWEEN_ARRAY%SET_SEED(SEED)
    !
    DO I=1, SIZE(A)
                    CALL LCG_BETWEEN_ARRAY%GEN( A(I), LOWER, UPPER )
    END DO
    !
  END SUBROUTINE
  !	
  ! ------------------------------------------------------------------------------------------------------------------------------------------------------------
  !	
  SUBROUTINE randARRAY_DBL(A, LOWER, UPPER, SEED)
    REAL(DBL), dimension(:), contiguous, intent(inout):: A
    REAL(DBL),                 optional, intent(in   ):: LOWER, UPPER
    INTEGER,                   optional, intent(in   ):: SEED
    INTEGER:: I
    TYPE(RANDOM_GENERATOR), ALLOCATABLE, SAVE:: LCG_BETWEEN_ARRAY
    !
    IF( .NOT. ALLOCATED(LCG_BETWEEN_ARRAY) ) THEN
                                    ALLOCATE( LCG_BETWEEN_ARRAY )
                                    CALL LCG_BETWEEN_ARRAY%INIT()
    END IF
    IF(PRESENT(SEED)) CALL LCG_BETWEEN_ARRAY%SET_SEED(SEED)
    !
    DO I=1, SIZE(A)
                    CALL LCG_BETWEEN_ARRAY%GEN( A(I), LOWER, UPPER )
    END DO
    !
  END SUBROUTINE
  !	
  ! ------------------------------------------------------------------------------------------------------------------------------------------------------------
  !	
  SUBROUTINE randARRAY_SNG(A, LOWER, UPPER, SEED)
    REAL(SNG), dimension(:), contiguous, intent(inout):: A
    REAL(SNG),                 optional, intent(in   ):: LOWER, UPPER
    INTEGER,                   optional, intent(in   ):: SEED
    INTEGER:: I
    TYPE(RANDOM_GENERATOR), ALLOCATABLE, SAVE:: LCG_BETWEEN_ARRAY
    !
    IF( .NOT. ALLOCATED(LCG_BETWEEN_ARRAY) ) THEN
                                    ALLOCATE( LCG_BETWEEN_ARRAY )
                                    CALL LCG_BETWEEN_ARRAY%INIT()
    END IF
    IF(PRESENT(SEED)) CALL LCG_BETWEEN_ARRAY%SET_SEED(SEED)
    !
    DO I=1, SIZE(A)
                    CALL LCG_BETWEEN_ARRAY%GEN( A(I), LOWER, UPPER )
    END DO
    !
  END SUBROUTINE
  !
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  ! 
  SUBROUTINE INT_SHUFFLE(VEC,SEED)  
    !Modified SATTOLO Shuffle Method  
    !SEED<0 IS RANDOM SEED
    INTEGER,dimension(:),CONTIGUOUS,intent(inout):: VEC
    INTEGER,              optional, intent(in   ):: SEED
    INTEGER:: I,J,DIM
    INTEGER:: TMP
    REAL(SNG):: RND 
    !
    IF(PRESENT(SEED)) THEN; CALL SET_FORTRAN_SEED(SEED)
    END IF
    !
    DIM  = SIZE(VEC)
    !
    DO I=DIM, TWO, NEG
        !
        CALL RANDOM_NUMBER(RND)
        !
        RND = RND * REAL(I-ONE, SNG) + 1_SNG
        !
        J = NINT(RND) 
        !
        TMP    = VEC(I)
        VEC(I) = VEC(J)
        VEC(J) = TMP
        !
    END DO
    !
  END SUBROUTINE
  !
  ! ------------------------------------------------------------------------------------------------------------------------------------------------------------
  !	
  SUBROUTINE DBLE_SHUFFLE(VEC,SEED)  
    !Modified SATTOLO Shuffle Method  
    !SEED<0 IS RANDOM SEED
    REAL(DBL),dimension(:),CONTIGUOUS,intent(inout):: VEC
    INTEGER,                   optional, intent(in   ):: SEED
    INTEGER:: I,J,DIM
    REAL(DBL):: TMP
    REAL(SNG):: RND 
    !
    IF(PRESENT(SEED)) THEN; CALL SET_FORTRAN_SEED(SEED)
    END IF
    !
    DIM  = SIZE(VEC)
    !
    DO I=DIM, TWO, NEG
        !
        CALL RANDOM_NUMBER(RND)
        !
        RND = RND * REAL(I-ONE, SNG) + 1_SNG
        !
        J = NINT(RND) 
        !
        TMP    = VEC(I)
        VEC(I) = VEC(J)
        VEC(J) = TMP
        !
    END DO
    !
  END SUBROUTINE
  !
  ! ------------------------------------------------------------------------------------------------------------------------------------------------------------
  !	
  SUBROUTINE REAL_SHUFFLE(VEC,SEED)  
    !Modified SATTOLO Shuffle Method  
    !SEED<0 IS RANDOM SEED
    REAL(SNG),dimension(:),CONTIGUOUS,intent(inout):: VEC
    INTEGER,                   optional, intent(in   ):: SEED
    INTEGER:: I,J,DIM
    REAL(SNG):: TMP
    REAL(SNG):: RND 
    !
    IF(PRESENT(SEED)) THEN; CALL SET_FORTRAN_SEED(SEED)
    END IF
    !
    DIM  = SIZE(VEC)
    !
    DO I=DIM, TWO, NEG
        !
        CALL RANDOM_NUMBER(RND)
        !
        RND = RND * REAL(I-ONE, SNG) + 1_SNG
        !
        J = NINT(RND) 
        !
        TMP    = VEC(I)
        VEC(I) = VEC(J)
        VEC(J) = TMP
        !
    END DO
    !
  END SUBROUTINE

END MODULE
!