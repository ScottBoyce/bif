!
! Functions rely on BIND(C) to connect to C-standard library.
!  Most Fortran compilers will connect automatically and not require any co-compilation with C.
!
MODULE DIRECTORY_ISO_C_BINDING
  USE, INTRINSIC :: ISO_C_BINDING,   ONLY: C_INT, C_SIZE_T, C_CHAR, C_NULL_CHAR
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: stderr => ERROR_UNIT, i32 => INT32
  !Not that fortran INT = c_size_t and C_INT
  !
  IMPLICIT NONE
  !
  PUBLIC :: CHDIR          ! CALL CHDIR(PATH, [IOSTAT])
  PUBLIC :: MKDIR          ! CALL  MKDIR(DIR, [IOSTAT])  -> This will only make a single directory (./dir1), use MAKE_DIRECTORY to make multiply ones (./dir1/dir2/dir3)
  PUBLIC :: GETCWD         ! CALL GETCWD(CWD)
  !                        
  PUBLIC :: MAKE_C_CHAR    ! MAKE_C_CHAR(F_STR) RESULT(C_STR)
  PUBLIC :: CHAR_F_TO_C    ! CALL CHAR_F_TO_C(F_STR, C_STR)
  PUBLIC :: CHAR_C_TO_F    ! CALL CHAR_C_TO_F(C_STR, F_STR)
  !
  PUBLIC :: INIT_C_CHAR    ! CALL INIT_C_CHAR(STR)
  PUBLIC :: NO_NULL_C_CHAR ! CALL NO_NULL_C_CHAR(STR)
  !
  PRIVATE
  !
  INTERFACE 
          INTEGER(C_INT) FUNCTION C_CHDIR(PATH) BIND(C, NAME="chdir")
             USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_CHAR
             !
             CHARACTER( KIND=C_CHAR ), INTENT(IN) :: PATH(*)
          END FUNCTION
          !
          !------------------------------------------------------------------------------
          !
          INTEGER(C_INT) FUNCTION C_MKDIR(DIR) BIND(C, NAME="mkdir")
             USE, INTRINSIC :: ISO_C_BINDING,  ONLY: C_INT, C_CHAR
             !
             CHARACTER( KIND=C_CHAR ), INTENT(IN) :: DIR(*)
          END FUNCTION
          !
          !------------------------------------------------------------------------------
          ! Ignore the return value of getcwd -> subroutine
          SUBROUTINE C_GETCWD(CWD, N) BIND(C, NAME="getcwd")
             USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_SIZE_T, C_CHAR
             !
             CHARACTER( KIND=C_CHAR ), INTENT(INOUT) :: CWD(*)
             INTEGER(C_SIZE_T), VALUE, INTENT(IN   ) :: N
          END SUBROUTINE
  END INTERFACE
  !
  INTEGER,        PARAMETER :: F_CHAR = KIND('A')
  INTEGER(i32),   PARAMETER :: Z  = 0_i32
  INTEGER(C_INT), PARAMETER :: Zc = 0_C_INT
  !
  CONTAINS
  !
  !#############################################################################################################################################################
  !
  PURE SUBROUTINE INIT_C_CHAR(STR)
    CHARACTER(len=*, kind=C_CHAR), INTENT(INOUT) :: STR
    INTEGER:: N
    !
    N = LEN(STR)
    STR = ''
    STR(N:N) = C_NULL_CHAR
    !
  END SUBROUTINE
  !
  PURE SUBROUTINE NO_NULL_C_CHAR(STR)
    CHARACTER(len=*, kind=C_CHAR), INTENT(INOUT) :: STR
    INTEGER:: I, N
    !
    I = INDEX(STR, C_NULL_CHAR)
    IF( I > Z ) THEN
        N = LEN(STR)
        STR(I:N) = ''
    END IF
    !
  END SUBROUTINE  
  !
  PURE SUBROUTINE CHAR_F_TO_C(F_STR, C_STR)
    CHARACTER(len=*, kind=F_CHAR), INTENT(IN   ) :: F_STR
    CHARACTER(len=*, kind=C_CHAR), INTENT(  OUT) :: C_STR
    INTEGER :: NF, NC
    !
    NF = LEN(F_STR)
    NC = LEN(C_STR)
    !
    IF( NF < NC ) THEN
        C_STR        = F_STR
        C_STR(NC:NC) = C_NULL_CHAR
    ELSE
        C_STR        = F_STR(1:NC)
        C_STR(NC:NC) = C_NULL_CHAR
    END IF
    !
  END SUBROUTINE
  !
  PURE SUBROUTINE CHAR_C_TO_F(C_STR, F_STR)
    CHARACTER(len=*, kind=C_CHAR), INTENT(IN   ) :: C_STR
    CHARACTER(len=*, kind=F_CHAR), INTENT(  OUT) :: F_STR
    INTEGER :: I, N, NF
    !
    NF =   LEN(F_STR)
    N  = INDEX(C_STR, C_NULL_CHAR)
    !
    IF( N > Z ) THEN
        I = N-1
        IF(NF < N) THEN
                   F_STR = C_STR(1:NF)
        ELSE
                   F_STR(1:I ) = C_STR(1:I)
                   F_STR(N:NF) = '' 
        END IF
    ELSE
        F_STR = C_STR
    END IF
    !
  END SUBROUTINE
  !
  PURE FUNCTION MAKE_C_CHAR(F_STR) RESULT(C_STR)
    CHARACTER(len=*, kind=F_CHAR),              INTENT(IN) :: F_STR
    CHARACTER(len=1, kind=C_CHAR), DIMENSION(LEN(F_STR)+1) :: C_STR
    INTEGER :: I, N
    !
    N = LEN(F_STR)
    DO I=1, N
        C_STR(I) = F_STR(I:I)
    END DO
    C_STR(N+1) = C_NULL_CHAR
    !
  END FUNCTION
  !
  !#############################################################################################################################################################
  !
  SUBROUTINE GETCWD(CWD)
    CHARACTER(*),          INTENT(  OUT) :: CWD
    CHARACTER(len=LEN(CWD), kind=C_CHAR) :: C_STR
    INTEGER:: I, N
    INTEGER(C_SIZE_T):: T
    !
    T = INT(LEN(CWD), C_SIZE_T)
    !
    CALL INIT_C_CHAR(C_STR)
    !---------------------------
    CALL C_GETCWD(C_STR, T)    ! -> SUBROUTINE C_GETCWD(CWD, N) BIND(C, NAME="getcwd")
    !---------------------------
    CALL CHAR_C_TO_F(C_STR, CWD)
    !
  END SUBROUTINE
  !
  !#############################################################################################################################################################
  !
  SUBROUTINE CHDIR(PATH, IOSTAT)
    CHARACTER(*),      INTENT(IN   ) :: PATH
    INTEGER, OPTIONAL, INTENT(INOUT) :: IOSTAT
    INTEGER(C_INT) :: IERR
    !
    IERR = C_CHDIR( MAKE_C_CHAR(PATH) )
    !
    IF (PRESENT(IOSTAT)) THEN 
                IOSTAT = INT( IERR, kind=i32 )
    ELSEIF(IERR /= Zc) THEN
                WRITE(stderr,'(3A)') 'Failed to change directory to "',TRIM(PATH),'"'
                ERROR STOP
    END IF
    !
  END SUBROUTINE
  !
  !#############################################################################################################################################################
  !
  SUBROUTINE MKDIR(DIR, IOSTAT)
    USE ISO_C_BINDING, ONLY: C_NULL_CHAR
    USE ISO_FORTRAN_ENV, ONLY: stderr => ERROR_UNIT
    CHARACTER(*),      INTENT(IN   ) :: DIR
    INTEGER, OPTIONAL, INTENT(INOUT) :: IOSTAT
    INTEGER(C_INT) :: IERR
    CHARACTER(:), ALLOCATABLE :: TMP
    INTEGER  :: N, IU
    !
    IERR = C_MKDIR( MAKE_C_CHAR(DIR) )
    !
    IF(IERR /= Zc) THEN
        N = LEN_TRIM(DIR)
        IF( DIR(N:N) == '/' .OR. DIR(N:N) == '\') THEN
            TMP = DIR(1:N)//"fort.tmp.delme"
        ELSE
            TMP = DIR(1:N)//"/fort.tmp.delme"
        END IF
        !
        OPEN(NEWUNIT=IU, FILE=TMP, ACTION='WRITE', FORM='FORMATTED', ACCESS='SEQUENTIAL', STATUS='NEW', IOSTAT=N)
        IF(N == Z) THEN
                   CLOSE(IU, STATUS='DELETE')
                   IERR = Zc
        END IF
    END IF
    !
    IF(PRESENT(IOSTAT)) THEN 
               IOSTAT = INT( IERR, kind=i32 )
    ELSEIF(IERR /= Zc ) THEN
                WRITE(stderr,'(3A)') 'Failed to create directory "',TRIM(DIR),'"'
                ERROR STOP
    END IF
    !
  END SUBROUTINE
  !
END MODULE 
