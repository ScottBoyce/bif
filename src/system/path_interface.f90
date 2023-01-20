MODULE PATH_INTERFACE
  USE CONSTANTS,        ONLY: NEG, Z, ONE, TWO, SLASH, BSLASH, BLNK, CM, TAB, FALSE
  USE ARRAY_DATA_TYPES, ONLY: CHARACTER_TYPE, JOIN_CHAR
  IMPLICIT NONE
  !
  PRIVATE
  !
  PUBLIC:: IS_WINDOWS         ! win = IS_WINDOWS()
  !
  PUBLIC:: GET_CWD            ! cwd = GET_CWD()
  PUBLIC:: SET_TO_CWD         ! CALL SET_TO_CWD(CWD, LENGTH) 
  !
  PUBLIC:: GET_FILE_EXTENSION ! CALL GET_FILE_EXTENSION(FILE_NAME, EXT)
  !
  PUBLIC:: MAKE_DIRECTORY   ! CALL MAKE_DIRECTORY(PATH, HAS_FILE, FIXED) -->This relies on SYSTEM call of mkdir
  PUBLIC:: FIX_PATH         ! CALL FIX_PATH(PATH)
  !
  PUBLIC:: PATH_TO_ARRAY    ! CALL PATH_TO_ARRAY(PATH, ARRAY, [CHAR_SIZE]) 
  !
  PUBLIC:: BSLASH_TO_SLASH  ! CALL BSLASH_TO_SLASH(PATH)
  PUBLIC:: SLASH_TO_BSLASH  ! CALL SLASH_TO_BSLASH(PATH)
  !
  PUBLIC:: ADD_DIR_SLASH, ADD_DIR_SLASH_ALLOC
  !
  INTERFACE PATH_TO_ARRAY
     MODULE PROCEDURE PATH_TO_ARRAY_CHAR_TYPE ! PATH_TO_ARRAY(PATH, ARRAY, [SIZ])               -> Array is TYPE(CHARACTER_TYPE), which preserves spaces in path
     MODULE PROCEDURE PATH_TO_ARRAY_CHAR      ! PATH_TO_ARRAY(PATH, ARRAY, [SIZ], [CHAR_SIZE])  -> Does not work if there are blank spaces in the path. Array is CHARACTER(:),DIMENSION(:)
  END INTERFACE
  !
  CONTAINS
  !
  !#############################################################################################################################################################
  !
  FUNCTION IS_WINDOWS(always_check) RESULT(IS_WIN)
    LOGICAL, OPTIONAL, INTENT(IN):: always_check
    LOGICAL:: IS_WIN
    INTEGER, SAVE:: WinNT = -1
    !
    IF(PRESENT(always_check)) THEN
            IF(always_check) WinNT = -1
    END IF
    !
    IF(WinNT < 0) THEN
                  IF(HAS_WIN_OS_VARIABLE()) THEN
                                            WinNT = 1
                  ELSE
                                            WinNT = 0
                  END IF
    END IF
    !
    IS_WIN = WinNT == 1
    !
  END FUNCTION
  !
  FUNCTION HAS_WIN_OS_VARIABLE() RESULT(HAS_WIN_OS)
    LOGICAL:: HAS_WIN_OS
    CHARACTER(10):: OS
    !
    CALL GET_ENVIRONMENT_VARIABLE ( "OS", value=OS )
    HAS_WIN_OS = OS == 'Windows_NT'                    !If any Windows variant then variable exists and is set to Windows_NT
    !
  END FUNCTION
  !
  !#############################################################################################################################################################
  !
  subroutine get_file_extension(file_name, ext)
    character(*),              intent(in ):: file_name
    character(:), allocatable, intent(out):: ext
    integer:: i, p, dim
    !
    dim = len_trim(file_name)
    p = dim
    do i=dim, 1, -1
       if(file_name(i:i) == '.') then
          p = i
          exit
       end if
    end do
    !
    p = p + 1
    if(p > dim) then
       ext = ''
    else
       ext = file_name(p:dim)
    end if
    !
  end subroutine
  !
  !#############################################################################################################################################################
  !
  FUNCTION GET_CWD() RESULT(CWD)
    CHARACTER(:), ALLOCATABLE:: CWD
    CHARACTER(:), ALLOCATABLE:: PATH
    INTEGER:: IERR, ITMP, IU
    !
    IF(IS_WINDOWS()) THEN
       CALL EXECUTE_COMMAND_LINE('cd  > ~tmp_path_file.delme', cmdstat=IERR)
    ELSE
       CALL EXECUTE_COMMAND_LINE('pwd > ~tmp_path_file.delme', cmdstat=IERR)
    END IF
    !
    IF( IERR == Z ) OPEN(NEWUNIT=IU, FILE='~tmp_path_file.delme',       &
                         ACTION='READWRITE',  FORM='FORMATTED',         &
                         ACCESS='SEQUENTIAL', STATUS='OLD', IOSTAT=IERR )
    IF( IERR == Z )  THEN
        INQUIRE(IU, SIZE=ITMP)
        !
        IF( ITMP <= 4096 ) THEN
            ITMP = ITMP + 32
        ELSE
            ITMP = 4096
        END IF
        ALLOCATE(CHARACTER(ITMP):: PATH)
        !
        REWIND(IU)
        READ(IU, '(A)', IOSTAT=IERR) PATH
        CLOSE(IU, STATUS='DELETE', IOSTAT=ITMP)
    END IF
    !
    IF( IERR == Z ) THEN
        CWD = TRIM(ADJUSTL(PATH))
    ELSE
        CWD = 'Unknown Current Working Directory (CWD)'
    END IF
    !
  END FUNCTION
  !
  !#############################################################################################################################################################
  !
  SUBROUTINE SET_TO_CWD(CWD, LENGTH) 
    CHARACTER(*),           INTENT(INOUT):: CWD
    INTEGER,      OPTIONAL, INTENT(INOUT):: LENGTH
    CHARACTER(:), ALLOCATABLE:: PATH
    !
    PATH = GET_CWD()
    IF(PRESENT(LENGTH)) LENGTH = LEN(PATH)
    CWD = PATH
    !
  END SUBROUTINE
  !
  !#############################################################################################################################################################
  !
  SUBROUTINE MAKE_DIRECTORY(PATH, HAS_FILE, FIXED)
    CHARACTER(*),                        INTENT(IN   ):: PATH      ! Directory to make, will make multiple sub-directories. 
                                                                   !   eg:  /dir1/dir2/dir3 will make dir1, dir2, and dir3 if they dod not exist.
    LOGICAL,                   OPTIONAL, INTENT(IN   ):: HAS_FILE  ! Set to true to indicate that PATH includes a file at the end, otherwise it will make a directory in the name of the file.
                                                                   !   eg. set to True ignores file in /dir1/dir2/file and makes dir1 and dir2
    CHARACTER(:), ALLOCATABLE, OPTIONAL, INTENT(  OUT):: FIXED     ! Returns the path created from PATH
    !
    TYPE(CHARACTER_TYPE), DIMENSION(:), ALLOCATABLE:: ARRAY
    INTEGER:: I
    LOGICAL:: IS_WIN
    !
    IF(PATH == BLNK) THEN
                     IF(PRESENT(FIXED)) FIXED = PATH
                     RETURN
    END IF
    !
    CALL PATH_TO_ARRAY_CHAR_TYPE(PATH, ARRAY)
    I = SIZE(ARRAY)
    !
    IF(PRESENT(HAS_FILE)) THEN
            IF(HAS_FILE) I = I-ONE
    END IF
    !
    IS_WIN = IS_WINDOWS()
    !
    IF(IS_WIN) THEN !wait =FALSE
        CALL EXECUTE_COMMAND_LINE(    'mkdir "'//JOIN_CHAR(ARRAY(:I), '\')//'" >nul 2>nul' )
    ELSE
        CALL EXECUTE_COMMAND_LINE( 'mkdir -p "'//JOIN_CHAR(ARRAY(:I), '/')//'" >/dev/null 2>&1' )
    END IF
    !
    IF(PRESENT(FIXED)) THEN
        IF(IS_WIN) THEN
                   FIXED = JOIN_CHAR(ARRAY, '\')
        ELSE
                   FIXED = JOIN_CHAR(ARRAY, '/')
        END IF
    END IF
    !
  END SUBROUTINE
  !
  !#############################################################################################################################################################
  !
  SUBROUTINE FIX_PATH(PATH)  !replaces slashes to be in the correct direction - This does not fix drive letters or remove excess dots such as: ./dir/../dir2
    CHARACTER(*), INTENT(INOUT):: PATH
    !
    TYPE(CHARACTER_TYPE), DIMENSION(:), ALLOCATABLE:: ARRAY
    !
    IF(PATH .NE. BLNK) THEN
                       CALL PATH_TO_ARRAY_CHAR_TYPE(PATH, ARRAY)
                       !
                       IF(IS_WINDOWS()) THEN
                                  PATH = JOIN_CHAR(ARRAY, BSLASH)
                       ELSE
                                  PATH = JOIN_CHAR(ARRAY,  SLASH)
                       END IF
    END IF
    !
  END SUBROUTINE
  !
  !#############################################################################################################################################################
  !
  SUBROUTINE ADD_DIR_SLASH(LN, OS_SLASH, ONLY_SLASH, ONLY_BSLASH)  !Converts "./MyFile" to "./MyFile/" or ".\MyFile" to ".\MyFile\", This does not reconize UNIX escape \ symbol, such as a blank space "\ "
    CHARACTER(*),      INTENT(INOUT):: LN
    LOGICAL, OPTIONAL, INTENT(IN   ):: OS_SLASH, ONLY_SLASH, ONLY_BSLASH
    INTEGER:: DIM, I, J
    INTEGER:: SLASH_FLAG
    LOGICAL:: ADD_SLASH
    DIM = LEN_TRIM(LN)
    !
    SLASH_FLAG = Z
    !
    IF(PRESENT(ONLY_SLASH)) THEN
            IF(ONLY_SLASH)  SLASH_FLAG = ONE
    END IF
    !
    IF(PRESENT(ONLY_BSLASH)) THEN
            IF(ONLY_BSLASH) SLASH_FLAG = TWO
    END IF
    !
    IF(PRESENT(  OS_SLASH)) THEN
        IF(OS_SLASH) THEN
            IF(IS_WINDOWS()) THEN
                            SLASH_FLAG = TWO
                
            ELSE
                            SLASH_FLAG = ONE    
            END IF
        END IF
    END IF
    !
    IF(DIM > Z .AND. DIM+1 <= LEN(LN)) THEN
        !
        ADD_SLASH = LN(DIM:DIM) /= SLASH .AND. LN(DIM:DIM) /= BSLASH
        !
        DIM = DIM + ONE
        IF(SLASH_FLAG == Z .AND. ADD_SLASH) THEN
                                         I = INDEX(LN,BSLASH) 
                                         J = INDEX(LN, SLASH) 
                                         IF     (I > Z .AND. J > Z) THEN
                                                            LN(DIM:DIM) =  SLASH
                                         ELSEIF (I > Z) THEN
                                                            LN(DIM:DIM) = BSLASH
                                         ELSE
                                                            LN(DIM:DIM) =  SLASH
                                         END IF
        ELSE IF(SLASH_FLAG == ONE) THEN  ! Use Forward slashes
                                         CALL BSLASH_TO_SLASH(LN)
                                         IF(ADD_SLASH) LN(DIM:DIM) =  SLASH
        ELSE IF(SLASH_FLAG == TWO) THEN  ! Use Back    slashes
                                         CALL SLASH_TO_BSLASH(LN)
                                         IF(ADD_SLASH) LN(DIM:DIM) =  BSLASH
        END IF
    END  IF
    !
  END SUBROUTINE
  !
  !-------------------------------------------------------------------------------------------------------------------------------------------------
  !
  SUBROUTINE ADD_DIR_SLASH_ALLOC(LN, OS_SLASH, ONLY_SLASH, ONLY_BSLASH)  !Converts "./MyFile" to "./MyFile/" or ".\MyFile" to ".\MyFile\"   
    CHARACTER(:), ALLOCATABLE, INTENT(INOUT):: LN
    LOGICAL,         OPTIONAL, INTENT(IN   ):: OS_SLASH, ONLY_SLASH, ONLY_BSLASH
    INTEGER:: DIM, I, J
    LOGICAL:: CALL_ADD_DIR_SLASH, ADD_SLASH, GROW
    CHARACTER:: SL
    !
    IF(.NOT. ALLOCATED(LN)) RETURN
    !
    CALL_ADD_DIR_SLASH = FALSE
    !
    IF(PRESENT(ONLY_SLASH)) THEN
            IF(ONLY_SLASH)  CALL_ADD_DIR_SLASH = ONLY_SLASH
    END IF
    !
    IF(PRESENT(ONLY_BSLASH)) THEN
            IF(ONLY_BSLASH) CALL_ADD_DIR_SLASH = ONLY_BSLASH
    END IF
    !
    IF(PRESENT(  OS_SLASH)) THEN
              IF(OS_SLASH) CALL_ADD_DIR_SLASH = OS_SLASH
    END IF
    !
    DIM  = LEN_TRIM(LN)
    GROW = DIM+1 > LEN(LN)
    IF(DIM > Z) THEN
        !
        ADD_SLASH = LN(DIM:DIM) /= SLASH .AND. LN(DIM:DIM) /= BSLASH
        !
        IF(CALL_ADD_DIR_SLASH) THEN
             !
             IF(ADD_SLASH .AND. GROW) LN = LN//BLNK  ! Pad with one blank space
             CALL ADD_DIR_SLASH(LN, OS_SLASH, ONLY_SLASH, ONLY_BSLASH)
             !
        ELSE IF(ADD_SLASH) THEN
               I = INDEX(LN,BSLASH) 
               J = INDEX(LN, SLASH) 
               IF     (I > Z .AND. J > Z) THEN
                                  SL =  SLASH
               ELSEIF (I > Z) THEN
                                  SL = BSLASH
               ELSE
                                  SL =  SLASH
               END IF
               !
               DIM = DIM + ONE
               IF(GROW) THEN
                    LN = LN//SL
               ELSE
                    LN(DIM:DIM) =  SL
               END IF
        END IF
    END IF
    !
  END SUBROUTINE
  !
  !#############################################################################################################################################################
  !
  PURE SUBROUTINE PATH_TO_ARRAY_CHAR_TYPE(PATH, ARRAY, SIZ)                ! This will handle spaces in path name
    CHARACTER(*),                                    INTENT(IN   ):: PATH  ! Path to split to array
    TYPE(CHARACTER_TYPE), DIMENSION(:), ALLOCATABLE, INTENT(  OUT):: ARRAY ! Array that contains the split path
    INTEGER,                               OPTIONAL, INTENT(  OUT):: SIZ   ! Final Size of array, TYPE(CHARACTER_TYPE), DIMENSION(SIZ)
    INTEGER:: I, J, K, LENLINE, DIM
    LOGICAL:: HAS_ROOT_SLASH
    !
    IF(ALLOCATED(ARRAY)) DEALLOCATE(ARRAY) !Should be auto-deallocated from INTENT(OUT), being redundant to be safe
    ! 
    DIM     = Z
    LENLINE = LEN_TRIM(PATH)
    !
    IF(LENLINE==Z) THEN
                   IF(PRESENT(SIZ)) SIZ = Z
                   RETURN
    END IF
    !
    I = LENLINE
    DO K = ONE, LENLINE 
        IF(PATH(K:K).NE.BLNK  .AND. PATH(K:K).NE.CM .AND. PATH(K:K).NE.TAB) THEN
            I = K
            EXIT
        END IF
    END DO
    !
    HAS_ROOT_SLASH = PATH(I:I) == SLASH
    !
    IF(HAS_ROOT_SLASH) THEN
        DIM = ONE
        I = I + ONE
    END IF
    !
    J = I
    DO K=I+ONE, LENLINE
        IF( PATH(K:K) == SLASH .OR. &
            PATH(K:K) == BSLASH ) THEN
                                  DIM = DIM + ONE
                                  J = K + ONE
        END IF
    END DO
    !
    K = LENLINE
    IF( PATH(K:K) /= SLASH .AND. &
        PATH(K:K) /= BSLASH ) THEN
                              DIM = DIM + ONE
    END IF
    !
    IF(PRESENT(SIZ)) SIZ = DIM
    !
    IF(DIM == Z) RETURN
    !
    ALLOCATE(ARRAY(DIM))
    !
    DIM = Z
    IF(HAS_ROOT_SLASH) THEN
        DIM = ONE
        ARRAY(DIM) = ""
    END IF
    !
    J   = I
    DO K=I+ONE, LENLINE
        IF( PATH(K:K) == SLASH .OR. &
            PATH(K:K) == BSLASH ) THEN
                                  DIM = DIM + ONE
                                  ARRAY(DIM) = PATH(J:K-1)
                                  J = K + ONE
        END IF
    END DO
    !
    K = LENLINE
    IF( PATH(K:K) /= SLASH .AND. &
        PATH(K:K) /= BSLASH ) THEN
                              DIM = DIM + ONE
                              ARRAY(DIM) = PATH(J:K)
    END IF
    !
  END SUBROUTINE
  !
  !-------------------------------------------------------------------------------------------------------------------------------------------------
  !
  PURE SUBROUTINE PATH_TO_ARRAY_CHAR(PATH, ARRAY, SIZ, CHAR_SIZE)       ! Note this method does not work if their are blank spaces in the path
    CHARACTER(*),                            INTENT(IN   ):: PATH       ! Path to split to array
    CHARACTER(:), DIMENSION(:), ALLOCATABLE, INTENT(  OUT):: ARRAY      ! Array that contains the split path
    INTEGER,                       OPTIONAL, INTENT(  OUT):: SIZ        ! Final Size of array, CHARACTER(CHAR_SIZE), DIMENSION(SIZ) 
    INTEGER,                       OPTIONAL, INTENT(IN   ):: CHAR_SIZE  ! Optional specifing   CHARACTER(CHAR_SIZE), DIMENSION(:)
    INTEGER:: I, J, K, LENLINE, DIM, MXLEN
    LOGICAL:: HAS_ROOT_SLASH
    !
    IF(ALLOCATED(ARRAY)) DEALLOCATE(ARRAY) !Should be auto-deallocated from INTENT(OUT), being redundant to be safe
    ! 
    MXLEN   = Z
    DIM     = Z
    LENLINE = LEN_TRIM(PATH)
    !
    IF(LENLINE==Z) THEN
                   IF(PRESENT(SIZ)) SIZ = Z
                   RETURN
    END IF
    !
    I = LENLINE
    DO K = ONE, LENLINE 
        IF(PATH(K:K).NE.BLNK  .AND. PATH(K:K).NE.CM .AND. PATH(K:K).NE.TAB) THEN
            I = K
            EXIT
        END IF
    END DO
    !
    HAS_ROOT_SLASH = PATH(I:I) == SLASH
    !
    IF(HAS_ROOT_SLASH) THEN
        DIM = ONE
        I = I + ONE
    END IF
    !
    J = I
    DO K=I+ONE, LENLINE
        IF( PATH(K:K) == SLASH .OR. &
            PATH(K:K) == BSLASH ) THEN
                                  DIM = DIM + ONE
                                  IF(MXLEN < K-J) MXLEN = K-J
                                  J = K + ONE
        END IF
    END DO
    !
    K = LENLINE
    IF( PATH(K:K) /= SLASH .AND. &
        PATH(K:K) /= BSLASH ) THEN
                              DIM = DIM + ONE
                              IF(MXLEN < K-J+ONE) MXLEN = K-J+ONE
    END IF
    !
    IF(PRESENT(SIZ)) SIZ = DIM
    !
    IF(DIM == Z) RETURN
    !
    IF( PRESENT(CHAR_SIZE) ) THEN
        ALLOCATE(CHARACTER(CHAR_SIZE)::ARRAY(DIM))
    ELSE
        ALLOCATE(CHARACTER(    MXLEN)::ARRAY(DIM))
    END IF
    !
    DIM = Z
    IF(HAS_ROOT_SLASH) THEN
        DIM = ONE
        ARRAY(DIM) = BLNK
    END IF
    !
    J   = I
    DO K=I+ONE, LENLINE
        IF( PATH(K:K) == SLASH .OR. &
            PATH(K:K) == BSLASH ) THEN
                                  DIM = DIM + ONE
                                  ARRAY(DIM) = PATH(J:K-1)
                                  J = K + ONE
        END IF
    END DO
    !
    K = LENLINE
    IF( PATH(K:K) /= SLASH .AND. &
        PATH(K:K) /= BSLASH ) THEN
                              DIM = DIM + ONE
                              ARRAY(DIM) = PATH(J:K)
    END IF
    !
  END SUBROUTINE
  !
  !#############################################################################################################################################################
  !
  PURE SUBROUTINE BSLASH_TO_SLASH(PATH)  ! Convert path to ensure all are slash: / 
    CHARACTER(*), INTENT(INOUT):: PATH
    INTEGER:: I
    !
    DO I=ONE, LEN_TRIM(PATH)
        IF(PATH(I:I) == BSLASH) PATH(I:I) = SLASH
    END DO
    !
  END SUBROUTINE
  !
  !-------------------------------------------------------------------------------------------------------------------------------------------------
  !
  PURE SUBROUTINE SLASH_TO_BSLASH(PATH) ! Convert path to ensure all are backslash: \
    CHARACTER(*), INTENT(INOUT):: PATH
    INTEGER:: I
    !
    DO I=ONE, LEN_TRIM(PATH)
        IF(PATH(I:I) == SLASH) PATH(I:I) = BSLASH
    END DO
    !
  END SUBROUTINE
  !
  !#############################################################################################################################################################
  !
END MODULE

    !
    !!!PURE SUBROUTINE REDUCE_PATH(PATH, ISTOP)  !Move back to next directory. ISTOP set to -1 if there are no parent directories otherwise is set.
    !!!  CHARACTER(*), INTENT(IN ):: PATH
    !!!  INTEGER,      INTENT(OUT):: ISTOP
    !!!  INTEGER:: I, ISTART
    !!!  ! 
    !!!  ISTART= LEN_TRIM(PATH)-ONE
    !!!  ISTOP = NEG
    !!!  !
    !!!  DO I=ISTART, ONE, NEG
    !!!      IF( PATH(I:I) == SLASH .OR. PATH(I:I) == BSLASH ) THEN
    !!!          ISTOP = I
    !!!          EXIT
    !!!      END IF
    !!!  END DO
    !!!  !
    !!!END SUBROUTINE
    
    
    !
    !#############################################################################################################################################################
    !
    !PURE SUBROUTINE WIN_TO_MYSYS_POSIX(LN)  !Converts "C:\MyFile\" to "/c/MyFile/"  
    !  CHARACTER(*), INTENT(INOUT):: LN
    !  INTEGER:: DIM, I
    !  DIM = LEN_TRIM(LN)
    !  !
    !  IF( DIM > Z) THEN
    !     DO I=1, DIM
    !         IF(LN(I:I) == BSLASH) LN(I:I) = SLASH   !  SLASH = / and BSLASH = \
    !     END DO
    !     !
    !     DIM = INDEX(LN,':')
    !     IF(DIM > Z) THEN
    !                 DO I=1, DIM - ONE
    !                     IF( LN(I:I).GE.'A' .AND. LN(I:I).LE.'Z') LN(I:I)=CHAR(ICHAR(LN(I:I))+Upper_to_Lower)  !Make drive letter lower case
    !                 END DO
    !                 LN(2:DIM) = LN(1:DIM-1)
    !                 LN(1:1) = '/'
    !     END IF
    !  END IF
    !  !
    !END SUBROUTINE
    !
    !#############################################################################################################################################################
    !