MODULE WRITE_ARRAY_INTERFACE!, ONLY: WRITE_ARRAY 
  !
  USE NUM2STR_INTERFACE, ONLY: NUM2STR
  !
  USE, INTRINSIC:: ISO_FORTRAN_ENV, ONLY: REAL32, REAL64, REAL128, INT8, INT16, INT32, INT64
  !
  IMPLICIT NONE
  PRIVATE
  !
  PUBLIC:: WRITE_ARRAY
  !
  INTERFACE WRITE_ARRAY
    MODULE PROCEDURE:: WRITE_ARRAY_1D_INT08      ! WRITE_ARRAY(IU, ARR, [WIDTH, FMT, SEP, TRANSPOSE, HED, SEP_ON_LAST, ADVANCE])
    MODULE PROCEDURE:: WRITE_ARRAY_1D_INT16      !
    MODULE PROCEDURE:: WRITE_ARRAY_1D_INT32      !
    MODULE PROCEDURE:: WRITE_ARRAY_1D_INT64      !
    MODULE PROCEDURE:: WRITE_ARRAY_1D_REL32      !
    MODULE PROCEDURE:: WRITE_ARRAY_1D_REL64      !
    !                                            
    MODULE PROCEDURE:: WRITE_ARRAY_2D_INT08      ! WRITE_ARRAY(IU, ARR, [WIDTH, FMT, SEP, TRANSPOSE, HED, SEP_ON_LAST])
    MODULE PROCEDURE:: WRITE_ARRAY_2D_INT16      ! 
    MODULE PROCEDURE:: WRITE_ARRAY_2D_INT32      ! 
    MODULE PROCEDURE:: WRITE_ARRAY_2D_INT64      ! 
    MODULE PROCEDURE:: WRITE_ARRAY_2D_REL32      !
    MODULE PROCEDURE:: WRITE_ARRAY_2D_REL64      ! 
    !
    MODULE PROCEDURE:: FILE_WRITE_ARRAY_1D_INT08 ! WRITE_ARRAY(FNAME, ARR, ERROR, [WIDTH, FMT, SEP, TRANSPOSE, HED, SEP_ON_LAST, ADVANCE, IU, NO_CLOSE])
    MODULE PROCEDURE:: FILE_WRITE_ARRAY_1D_INT16 !
    MODULE PROCEDURE:: FILE_WRITE_ARRAY_1D_INT32 !
    MODULE PROCEDURE:: FILE_WRITE_ARRAY_1D_INT64 !
    MODULE PROCEDURE:: FILE_WRITE_ARRAY_1D_REL32 !
    MODULE PROCEDURE:: FILE_WRITE_ARRAY_1D_REL64 !
    !                                            
    MODULE PROCEDURE:: FILE_WRITE_ARRAY_2D_INT08 ! WRITE_ARRAY(FNAME, ARR, ERROR, [WIDTH, FMT, SEP, TRANSPOSE, HED, SEP_ON_LAST,          IU, NO_CLOSE])
    MODULE PROCEDURE:: FILE_WRITE_ARRAY_2D_INT16 !
    MODULE PROCEDURE:: FILE_WRITE_ARRAY_2D_INT32 !
    MODULE PROCEDURE:: FILE_WRITE_ARRAY_2D_INT64 !
    MODULE PROCEDURE:: FILE_WRITE_ARRAY_2D_REL32 !
    MODULE PROCEDURE:: FILE_WRITE_ARRAY_2D_REL64 !
  END INTERFACE
  !
  INTEGER, PARAMETER:: Z = 0
  LOGICAL, PARAMETER:: TRUE  = .TRUE.
  LOGICAL, PARAMETER:: FALSE = .FALSE.
  !
  CONTAINS
  !
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !  
  !  WRITE 1D ARRAY GIVEN UNIT NUMBER
  !
  !##########################################################################################################################
  ! INTEGER(INT8) 1D ARRAY WRITE
  !
  SUBROUTINE WRITE_ARRAY_1D_INT08(IU, ARR, WIDTH, FMT, SEP, TRANSPOSE, HED, SEP_ON_LAST, ADVANCE)
  INTEGER,                      INTENT(IN):: IU
  INTEGER(INT8),  DIMENSION(:), INTENT(IN):: ARR
  INTEGER,            OPTIONAL, INTENT(IN):: WIDTH
  CHARACTER(*),       OPTIONAL, INTENT(IN):: FMT
  CHARACTER(*),       OPTIONAL, INTENT(IN):: SEP
  LOGICAL,            OPTIONAL, INTENT(IN):: TRANSPOSE
  CHARACTER(*),       OPTIONAL, INTENT(IN):: HED
  LOGICAL,            OPTIONAL, INTENT(IN):: SEP_ON_LAST
  LOGICAL,            OPTIONAL, INTENT(IN):: ADVANCE
  !
  INTEGER:: I, D1, PAD
  LOGICAL:: HAS_FMT, HAS_SEP, NO_TRANSPOSE
  CHARACTER(:), ALLOCATABLE:: FORM, FM
  CHARACTER(3):: ADV
  LOGICAL:: LS, RS
  !
  IF(PRESENT(HED)) WRITE(IU, '(A)') HED
  !
  D1 = SIZE(ARR)
  !
  IF(ALLOCATED(FORM)) DEALLOCATE(FORM)
  !
  NO_TRANSPOSE = TRUE
  IF(PRESENT(TRANSPOSE)) NO_TRANSPOSE = .NOT. TRANSPOSE
  !
  HAS_SEP = PRESENT(SEP)
  !
  PAD = Z
  IF(PRESENT(WIDTH)) PAD = WIDTH
  !
  LS = FALSE
  RS = FALSE
  IF(NO_TRANSPOSE .AND. .NOT. HAS_SEP) THEN
     IF    (PAD > Z) THEN
                     LS = TRUE
     ELSEIF(PAD < Z) THEN
                     RS = TRUE
     END IF
  END IF
  !
  HAS_FMT = PRESENT(FMT)
  IF(HAS_FMT) THEN
              IF(FMT == "") HAS_FMT = FALSE
  END IF
  IF(HAS_FMT) THEN
              FM = TRIM(FMT)
  ELSE
              FM = 'A'
  END IF
  !
  IF(NO_TRANSPOSE) THEN
     IF(HAS_SEP) THEN
        FORM = '(*('//FM//', :, "'//SEP//'"))'
     ELSEIF(PAD /= Z) THEN
        FORM = '(*('//FM//'))'
     ELSE
        FORM = '(*('//FM//', :, 1x))'
     END IF
  ELSE
     IF(HAS_SEP) THEN
        FORM = '(*('//FM//', :, "'//SEP//'",   /))'
     ELSE
        FORM = '(*('//FM//', :,   /))'
     END IF
  END IF
  !
  IF(PRESENT(SEP_ON_LAST) .AND. HAS_SEP) THEN
          IF(SEP_ON_LAST) THEN
              I = INDEX(FORM, ":")
              IF( I > Z ) THEN
                  FORM(I:I+1) = "  "  !Drop : from format
              END IF
              I = INDEX(FORM, "/")
              IF( I > Z ) THEN
                  I = I - 2
                  FORM(I:I+1) = ":,"  ! Ensure there is not a double "LF" at end of write  -> LF is line feed/carriage return
              END IF
          END IF
  END IF
  !
  ADV = "YES"
  IF(PRESENT(ADVANCE)) THEN
    IF(.not. ADVANCE) ADV = "NO "
  END IF
  !
  IF(HAS_FMT) THEN
              WRITE(IU, FORM, advance=ADV) ARR
  ELSEIF(PAD /= Z) THEN
              WRITE(IU, FORM, advance=ADV) (NUM2STR(ARR(I), PAD, LS=LS, RS=RS), I=1, D1) 
  ELSE
              WRITE(IU, FORM, advance=ADV) (NUM2STR(ARR(I)), I=1, D1) 
  END IF
  !
  END SUBROUTINE WRITE_ARRAY_1D_INT08
  !
  !##########################################################################################################################
  ! INTEGER(INT16) 1D ARRAY WRITE
  !
  SUBROUTINE WRITE_ARRAY_1D_INT16(IU, ARR, WIDTH, FMT, SEP, TRANSPOSE, HED, SEP_ON_LAST, ADVANCE)
  INTEGER,                      INTENT(IN):: IU
  INTEGER(INT16), DIMENSION(:), INTENT(IN):: ARR
  INTEGER,            OPTIONAL, INTENT(IN):: WIDTH
  CHARACTER(*),       OPTIONAL, INTENT(IN):: FMT
  CHARACTER(*),       OPTIONAL, INTENT(IN):: SEP
  LOGICAL,            OPTIONAL, INTENT(IN):: TRANSPOSE
  CHARACTER(*),       OPTIONAL, INTENT(IN):: HED
  LOGICAL,            OPTIONAL, INTENT(IN):: SEP_ON_LAST
  LOGICAL,            OPTIONAL, INTENT(IN):: ADVANCE
  !
  INTEGER:: I, D1, PAD
  LOGICAL:: HAS_FMT, HAS_SEP, NO_TRANSPOSE
  CHARACTER(:), ALLOCATABLE:: FORM, FM
  CHARACTER(3):: ADV
  LOGICAL:: LS, RS
  !
  IF(PRESENT(HED)) WRITE(IU, '(A)') HED
  !
  D1 = SIZE(ARR)
  !
  IF(ALLOCATED(FORM)) DEALLOCATE(FORM)
  !
  NO_TRANSPOSE = TRUE
  IF(PRESENT(TRANSPOSE)) NO_TRANSPOSE = .NOT. TRANSPOSE
  !
  HAS_SEP = PRESENT(SEP)
  !
  PAD = Z
  IF(PRESENT(WIDTH)) PAD = WIDTH
  !
  LS = FALSE
  RS = FALSE
  IF(NO_TRANSPOSE .AND. .NOT. HAS_SEP) THEN
     IF    (PAD > Z) THEN
                     LS = TRUE
     ELSEIF(PAD < Z) THEN
                     RS = TRUE
     END IF
  END IF
  !
  HAS_FMT = PRESENT(FMT)
  IF(HAS_FMT) THEN
              IF(FMT == "") HAS_FMT = FALSE
  END IF
  IF(HAS_FMT) THEN
              FM = TRIM(FMT)
  ELSE
              FM = 'A'
  END IF
  !
  IF(NO_TRANSPOSE) THEN
     IF(HAS_SEP) THEN
        FORM = '(*('//FM//', :, "'//SEP//'"))'
     ELSEIF(PAD /= Z) THEN
        FORM = '(*('//FM//'))'
     ELSE
        FORM = '(*('//FM//', :, 1x))'
     END IF
  ELSE
     IF(HAS_SEP) THEN
        FORM = '(*('//FM//', :, "'//SEP//'",   /))'
     ELSE
        FORM = '(*('//FM//', :,   /))'
     END IF
  END IF
  !
  IF(PRESENT(SEP_ON_LAST) .AND. HAS_SEP) THEN
          IF(SEP_ON_LAST) THEN
              I = INDEX(FORM, ":")
              IF( I > Z ) THEN
                  FORM(I:I+1) = "  "  !Drop : from format
              END IF
              I = INDEX(FORM, "/")
              IF( I > Z ) THEN
                  I = I - 2
                  FORM(I:I+1) = ":,"  ! Ensure there is not a double "LF" at end of write  -> LF is line feed/carriage return
              END IF
          END IF
  END IF
  !
  ADV = "YES"
  IF(PRESENT(ADVANCE)) THEN
    IF(.not. ADVANCE) ADV = "NO "
  END IF
  !
  IF(HAS_FMT) THEN
              WRITE(IU, FORM, advance=ADV) ARR
  ELSEIF(PAD /= Z) THEN
              WRITE(IU, FORM, advance=ADV) (NUM2STR(ARR(I), PAD, LS=LS, RS=RS), I=1, D1) 
  ELSE
              WRITE(IU, FORM, advance=ADV) (NUM2STR(ARR(I)), I=1, D1) 
  END IF
  !
  END SUBROUTINE WRITE_ARRAY_1D_INT16
  !
  !##########################################################################################################################
  ! INTEGER(INT32) 1D ARRAY WRITE -> most compliers have INTEGER(INT32) <=> INTEGER
  !
  SUBROUTINE WRITE_ARRAY_1D_INT32(IU, ARR, WIDTH, FMT, SEP, TRANSPOSE, HED, SEP_ON_LAST, ADVANCE)
  INTEGER,                      INTENT(IN):: IU
  INTEGER(INT32), DIMENSION(:), INTENT(IN):: ARR
  INTEGER,            OPTIONAL, INTENT(IN):: WIDTH
  CHARACTER(*),       OPTIONAL, INTENT(IN):: FMT
  CHARACTER(*),       OPTIONAL, INTENT(IN):: SEP
  LOGICAL,            OPTIONAL, INTENT(IN):: TRANSPOSE
  CHARACTER(*),       OPTIONAL, INTENT(IN):: HED
  LOGICAL,            OPTIONAL, INTENT(IN):: SEP_ON_LAST
  LOGICAL,            OPTIONAL, INTENT(IN):: ADVANCE
  !
  INTEGER:: I, D1, PAD
  LOGICAL:: HAS_FMT, HAS_SEP, NO_TRANSPOSE
  CHARACTER(:), ALLOCATABLE:: FORM, FM
  CHARACTER(3):: ADV
  LOGICAL:: LS, RS
  !
  IF(PRESENT(HED)) WRITE(IU, '(A)') HED
  !
  D1 = SIZE(ARR)
  !
  IF(ALLOCATED(FORM)) DEALLOCATE(FORM)
  !
  NO_TRANSPOSE = TRUE
  IF(PRESENT(TRANSPOSE)) NO_TRANSPOSE = .NOT. TRANSPOSE
  !
  HAS_SEP = PRESENT(SEP)
  !
  PAD = Z
  IF(PRESENT(WIDTH)) PAD = WIDTH
  !
  LS = FALSE
  RS = FALSE
  IF(NO_TRANSPOSE .AND. .NOT. HAS_SEP) THEN
     IF    (PAD > Z) THEN
                     LS = TRUE
     ELSEIF(PAD < Z) THEN
                     RS = TRUE
     END IF
  END IF
  !
  HAS_FMT = PRESENT(FMT)
  IF(HAS_FMT) THEN
              IF(FMT == "") HAS_FMT = FALSE
  END IF
  IF(HAS_FMT) THEN
              FM = TRIM(FMT)
  ELSE
              FM = 'A'
  END IF
  !
  IF(NO_TRANSPOSE) THEN
     IF(HAS_SEP) THEN
        FORM = '(*('//FM//', :, "'//SEP//'"))'
     ELSEIF(PAD /= Z) THEN
        FORM = '(*('//FM//'))'
     ELSE
        FORM = '(*('//FM//', :, 1x))'
     END IF
  ELSE
     IF(HAS_SEP) THEN
        FORM = '(*('//FM//', :, "'//SEP//'",   /))'
     ELSE
        FORM = '(*('//FM//', :,   /))'
     END IF
  END IF
  !
  IF(PRESENT(SEP_ON_LAST) .AND. HAS_SEP) THEN
          IF(SEP_ON_LAST) THEN
              I = INDEX(FORM, ":")
              IF( I > Z ) THEN
                  FORM(I:I+1) = "  "  !Drop : from format
              END IF
              I = INDEX(FORM, "/")
              IF( I > Z ) THEN
                  I = I - 2
                  FORM(I:I+1) = ":,"  ! Ensure there is not a double "LF" at end of write  -> LF is line feed/carriage return
              END IF
          END IF
  END IF
  !
  ADV = "YES"
  IF(PRESENT(ADVANCE)) THEN
    IF(.not. ADVANCE) ADV = "NO "
  END IF
  !
  IF(HAS_FMT) THEN
              WRITE(IU, FORM, advance=ADV) ARR
  ELSEIF(PAD /= Z) THEN
              WRITE(IU, FORM, advance=ADV) (NUM2STR(ARR(I), PAD, LS=LS, RS=RS), I=1, D1) 
  ELSE
              WRITE(IU, FORM, advance=ADV) (NUM2STR(ARR(I)), I=1, D1) 
  END IF
  !
  END SUBROUTINE WRITE_ARRAY_1D_INT32
  !
  !##########################################################################################################################
  ! INTEGER(INT64) 1D ARRAY WRITE
  !
  SUBROUTINE WRITE_ARRAY_1D_INT64(IU, ARR, WIDTH, FMT, SEP, TRANSPOSE, HED, SEP_ON_LAST, ADVANCE)
  INTEGER,                      INTENT(IN):: IU
  INTEGER(INT64), DIMENSION(:), INTENT(IN):: ARR
  INTEGER,            OPTIONAL, INTENT(IN):: WIDTH
  CHARACTER(*),       OPTIONAL, INTENT(IN):: FMT
  CHARACTER(*),       OPTIONAL, INTENT(IN):: SEP
  LOGICAL,            OPTIONAL, INTENT(IN):: TRANSPOSE
  CHARACTER(*),       OPTIONAL, INTENT(IN):: HED
  LOGICAL,            OPTIONAL, INTENT(IN):: SEP_ON_LAST
  LOGICAL,            OPTIONAL, INTENT(IN):: ADVANCE
  !
  INTEGER:: I, D1, PAD
  LOGICAL:: HAS_FMT, HAS_SEP, NO_TRANSPOSE
  CHARACTER(:), ALLOCATABLE:: FORM, FM
  CHARACTER(3):: ADV
  LOGICAL:: LS, RS
  !
  IF(PRESENT(HED)) WRITE(IU, '(A)') HED
  !
  D1 = SIZE(ARR)
  !
  IF(ALLOCATED(FORM)) DEALLOCATE(FORM)
  !
  NO_TRANSPOSE = TRUE
  IF(PRESENT(TRANSPOSE)) NO_TRANSPOSE = .NOT. TRANSPOSE
  !
  HAS_SEP = PRESENT(SEP)
  !
  PAD = Z
  IF(PRESENT(WIDTH)) PAD = WIDTH
  !
  LS = FALSE
  RS = FALSE
  IF(NO_TRANSPOSE .AND. .NOT. HAS_SEP) THEN
     IF    (PAD > Z) THEN
                     LS = TRUE
     ELSEIF(PAD < Z) THEN
                     RS = TRUE
     END IF
  END IF
  !
  HAS_FMT = PRESENT(FMT)
  IF(HAS_FMT) THEN
              IF(FMT == "") HAS_FMT = FALSE
  END IF
  IF(HAS_FMT) THEN
              FM = TRIM(FMT)
  ELSE
              FM = 'A'
  END IF
  !
  IF(NO_TRANSPOSE) THEN
     IF(HAS_SEP) THEN
        FORM = '(*('//FM//', :, "'//SEP//'"))'
     ELSEIF(PAD /= Z) THEN
        FORM = '(*('//FM//'))'
     ELSE
        FORM = '(*('//FM//', :, 1x))'
     END IF
  ELSE
     IF(HAS_SEP) THEN
        FORM = '(*('//FM//', :, "'//SEP//'",   /))'
     ELSE
        FORM = '(*('//FM//', :,   /))'
     END IF
  END IF
  !
  IF(PRESENT(SEP_ON_LAST) .AND. HAS_SEP) THEN
          IF(SEP_ON_LAST) THEN
              I = INDEX(FORM, ":")
              IF( I > Z ) THEN
                  FORM(I:I+1) = "  "  !Drop : from format
              END IF
              I = INDEX(FORM, "/")
              IF( I > Z ) THEN
                  I = I - 2
                  FORM(I:I+1) = ":,"  ! Ensure there is not a double "LF" at end of write  -> LF is line feed/carriage return
              END IF
          END IF
  END IF
  !
  ADV = "YES"
  IF(PRESENT(ADVANCE)) THEN
    IF(.not. ADVANCE) ADV = "NO "
  END IF
  !
  IF(HAS_FMT) THEN
              WRITE(IU, FORM, advance=ADV) ARR
  ELSEIF(PAD /= Z) THEN
              WRITE(IU, FORM, advance=ADV) (NUM2STR(ARR(I), PAD, LS=LS, RS=RS), I=1, D1) 
  ELSE
              WRITE(IU, FORM, advance=ADV) (NUM2STR(ARR(I)), I=1, D1) 
  END IF
  !
  END SUBROUTINE WRITE_ARRAY_1D_INT64
  !
  !##########################################################################################################################
  ! REAL(REAL32) 1D ARRAY WRITE - SINGLE PRECISION ARRAY WRITE
  !
  SUBROUTINE WRITE_ARRAY_1D_REL32(IU, ARR, WIDTH, FMT, SEP, TRANSPOSE, HED, SEP_ON_LAST, ADVANCE)
  INTEGER,                    INTENT(IN):: IU
  REAL(REAL32), DIMENSION(:), INTENT(IN):: ARR
  INTEGER,          OPTIONAL, INTENT(IN):: WIDTH
  CHARACTER(*),     OPTIONAL, INTENT(IN):: FMT
  CHARACTER(*),     OPTIONAL, INTENT(IN):: SEP
  LOGICAL,          OPTIONAL, INTENT(IN):: TRANSPOSE
  CHARACTER(*),     OPTIONAL, INTENT(IN):: HED
  LOGICAL,          OPTIONAL, INTENT(IN):: SEP_ON_LAST
  LOGICAL,          OPTIONAL, INTENT(IN):: ADVANCE
  !
  INTEGER:: I, D1, PAD
  LOGICAL:: HAS_FMT, HAS_SEP, NO_TRANSPOSE
  CHARACTER(:), ALLOCATABLE:: FORM, FM
  CHARACTER(3):: ADV
  LOGICAL:: LS, RS
  !
  IF(PRESENT(HED)) WRITE(IU, '(A)') HED
  !
  D1 = SIZE(ARR)
  !
  IF(ALLOCATED(FORM)) DEALLOCATE(FORM)
  !
  NO_TRANSPOSE = TRUE
  IF(PRESENT(TRANSPOSE)) NO_TRANSPOSE = .NOT. TRANSPOSE
  !
  HAS_SEP = PRESENT(SEP)
  !
  PAD = Z
  IF(PRESENT(WIDTH)) PAD = WIDTH
  !
  LS = FALSE
  RS = FALSE
  IF(NO_TRANSPOSE .AND. .NOT. HAS_SEP) THEN
     IF    (PAD > Z) THEN
                     LS = TRUE
     ELSEIF(PAD < Z) THEN
                     RS = TRUE
     END IF
  END IF
  !
  HAS_FMT = PRESENT(FMT)
  IF(HAS_FMT) THEN
              IF(FMT == "") HAS_FMT = FALSE
  END IF
  IF(HAS_FMT) THEN
              FM = TRIM(FMT)
  ELSE
              FM = 'A'
  END IF
  !
  IF(NO_TRANSPOSE) THEN
     IF(HAS_SEP) THEN
        FORM = '(*('//FM//', :, "'//SEP//'"))'
     ELSEIF(PAD /= Z) THEN
        FORM = '(*('//FM//'))'
     ELSE
        FORM = '(*('//FM//', :, 1x))'
     END IF
  ELSE
     IF(HAS_SEP) THEN
        FORM = '(*('//FM//', :, "'//SEP//'",   /))'
     ELSE
        FORM = '(*('//FM//', :,   /))'
     END IF
  END IF
  !
  IF(PRESENT(SEP_ON_LAST) .AND. HAS_SEP) THEN
          IF(SEP_ON_LAST) THEN
              I = INDEX(FORM, ":")
              IF( I > Z ) THEN
                  FORM(I:I+1) = "  "  !Drop : from format
              END IF
              I = INDEX(FORM, "/")
              IF( I > Z ) THEN
                  I = I - 2
                  FORM(I:I+1) = ":,"  ! Ensure there is not a double "LF" at end of write  -> LF is line feed/carriage return
              END IF
          END IF
  END IF
  !
  ADV = "YES"
  IF(PRESENT(ADVANCE)) THEN
    IF(.not. ADVANCE) ADV = "NO "
  END IF
  !
  IF(HAS_FMT) THEN
              WRITE(IU, FORM, advance=ADV) ARR
  ELSEIF(PAD /= Z) THEN
              WRITE(IU, FORM, advance=ADV) (NUM2STR(ARR(I), PAD, LS=LS, RS=RS), I=1, D1) 
  ELSE
              WRITE(IU, FORM, advance=ADV) (NUM2STR(ARR(I)), I=1, D1) 
  END IF
  !
  END SUBROUTINE WRITE_ARRAY_1D_REL32
  !
  !##########################################################################################################################
  ! REAL(REAL64) 1D ARRAY WRITE - DOUBLE PRECISION ARRAY WRITE
  !
  SUBROUTINE WRITE_ARRAY_1D_REL64(IU, ARR, WIDTH, FMT, SEP, TRANSPOSE, HED, SEP_ON_LAST, ADVANCE)
  INTEGER,                    INTENT(IN):: IU
  REAL(REAL64), DIMENSION(:), INTENT(IN):: ARR
  INTEGER,          OPTIONAL, INTENT(IN):: WIDTH
  CHARACTER(*),     OPTIONAL, INTENT(IN):: FMT
  CHARACTER(*),     OPTIONAL, INTENT(IN):: SEP
  LOGICAL,          OPTIONAL, INTENT(IN):: TRANSPOSE
  CHARACTER(*),     OPTIONAL, INTENT(IN):: HED
  LOGICAL,          OPTIONAL, INTENT(IN):: SEP_ON_LAST
  LOGICAL,          OPTIONAL, INTENT(IN):: ADVANCE
  !
  INTEGER:: I, D1, PAD
  LOGICAL:: HAS_FMT, HAS_SEP, NO_TRANSPOSE
  CHARACTER(:), ALLOCATABLE:: FORM, FM
  CHARACTER(3):: ADV
  LOGICAL:: LS, RS
  !
  IF(PRESENT(HED)) WRITE(IU, '(A)') HED
  !
  D1 = SIZE(ARR)
  !
  IF(ALLOCATED(FORM)) DEALLOCATE(FORM)
  !
  NO_TRANSPOSE = TRUE
  IF(PRESENT(TRANSPOSE)) NO_TRANSPOSE = .NOT. TRANSPOSE
  !
  HAS_SEP = PRESENT(SEP)
  !
  PAD = Z
  IF(PRESENT(WIDTH)) PAD = WIDTH
  !
  LS = FALSE
  RS = FALSE
  IF(NO_TRANSPOSE .AND. .NOT. HAS_SEP) THEN
     IF    (PAD > Z) THEN
                     LS = TRUE
     ELSEIF(PAD < Z) THEN
                     RS = TRUE
     END IF
  END IF
  !
  HAS_FMT = PRESENT(FMT)
  IF(HAS_FMT) THEN
              IF(FMT == "") HAS_FMT = FALSE
  END IF
  IF(HAS_FMT) THEN
              FM = TRIM(FMT)
  ELSE
              FM = 'A'
  END IF
  !
  IF(NO_TRANSPOSE) THEN
     IF(HAS_SEP) THEN
        FORM = '(*('//FM//', :, "'//SEP//'"))'
     ELSEIF(PAD /= Z) THEN
        FORM = '(*('//FM//'))'
     ELSE
        FORM = '(*('//FM//', :, 1x))'
     END IF
  ELSE
     IF(HAS_SEP) THEN
        FORM = '(*('//FM//', :, "'//SEP//'",   /))'
     ELSE
        FORM = '(*('//FM//', :,   /))'
     END IF
  END IF
  !
  IF(PRESENT(SEP_ON_LAST) .AND. HAS_SEP) THEN
          IF(SEP_ON_LAST) THEN
              I = INDEX(FORM, ":")
              IF( I > Z ) THEN
                  FORM(I:I+1) = "  "  !Drop : from format
              END IF
              I = INDEX(FORM, "/")
              IF( I > Z ) THEN
                  I = I - 2
                  FORM(I:I+1) = ":,"  ! Ensure there is not a double "LF" at end of write  -> LF is line feed/carriage return
              END IF
          END IF
  END IF
  !
  ADV = "YES"
  IF(PRESENT(ADVANCE)) THEN
    IF(.not. ADVANCE) ADV = "NO "
  END IF
  !
  IF(HAS_FMT) THEN
              WRITE(IU, FORM, advance=ADV) ARR
  ELSEIF(PAD /= Z) THEN
              WRITE(IU, FORM, advance=ADV) (NUM2STR(ARR(I), PAD, LS=LS, RS=RS), I=1, D1) 
  ELSE
              WRITE(IU, FORM, advance=ADV) (NUM2STR(ARR(I)), I=1, D1) 
  END IF
  !
  END SUBROUTINE WRITE_ARRAY_1D_REL64
  !
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !  
  !  WRITE 2D ARRAY GIVEN UNIT NUMBER
  !
  !##########################################################################################################################
  ! INTEGER(INT8) 2D ARRAY WRITE
  !
  SUBROUTINE WRITE_ARRAY_2D_INT08(IU, ARR, WIDTH, FMT, SEP, TRANSPOSE, HED, SEP_ON_LAST)
  INTEGER,                        INTENT(IN):: IU
  INTEGER(INT8),  DIMENSION(:,:), INTENT(IN):: ARR
  INTEGER,              OPTIONAL, INTENT(IN):: WIDTH
  CHARACTER(*),         OPTIONAL, INTENT(IN):: FMT
  CHARACTER(*),         OPTIONAL, INTENT(IN):: SEP
  LOGICAL,              OPTIONAL, INTENT(IN):: TRANSPOSE
  CHARACTER(*),         OPTIONAL, INTENT(IN):: HED
  LOGICAL,              OPTIONAL, INTENT(IN):: SEP_ON_LAST
  !
  INTEGER:: I, J, D1, D2, PAD
  LOGICAL:: HAS_FMT, HAS_SEP, NO_TRANSPOSE
  CHARACTER(:), ALLOCATABLE:: FORM, FM
  LOGICAL:: LS, RS
  !
  IF(PRESENT(HED)) WRITE(IU, '(A)') HED
  !
  D1 = SIZE(ARR,1)
  D2 = SIZE(ARR,2)
  !
  IF(ALLOCATED(FORM)) DEALLOCATE(FORM)
  !
  NO_TRANSPOSE = TRUE
  IF(PRESENT(TRANSPOSE)) NO_TRANSPOSE = .NOT. TRANSPOSE
  !
  HAS_SEP = PRESENT(SEP)
  !
  PAD = Z
  IF(PRESENT(WIDTH)) PAD = WIDTH
  !
  LS = FALSE
  RS = FALSE
  IF(NO_TRANSPOSE .AND. .NOT. HAS_SEP) THEN
     IF    (PAD > Z) THEN
                     LS = TRUE
     ELSEIF(PAD < Z) THEN
                     RS = TRUE
     END IF
  END IF
  !
  HAS_FMT = PRESENT(FMT)
  IF(HAS_FMT) THEN
              IF(FMT == "") HAS_FMT = FALSE
  END IF
  IF(HAS_FMT) THEN
              FM = TRIM(FMT)
  ELSE
              FM = 'A'
  END IF
  !
  IF    (HAS_SEP)  THEN
                   FORM = '(*('//FM//', :, "'//SEP//'"))'
  ELSEIF(PAD /= Z) THEN
                   FORM = '(*('//FM//'))'
  ELSE
                    FORM = '(*('//FM//', :, 1x))'
  END IF
  !
  IF(PRESENT(SEP_ON_LAST) .AND. HAS_SEP) THEN
          IF(SEP_ON_LAST) THEN
              I = INDEX(FORM, ":")
              IF( I > Z ) THEN
                  FORM(I:I+1) = "  "  !Drop : from format
              END IF
          END IF
  END IF
  !
  IF(NO_TRANSPOSE) THEN
     IF    (HAS_FMT) THEN
                     DO J=1, D2
                         WRITE(IU, FORM) ARR(:,J)
                     END DO
     ELSEIF(PAD /= Z) THEN
                     DO J=1, D2
                         WRITE(IU, FORM) (NUM2STR(ARR(I,J), PAD, LS=LS, RS=RS), I=1, D1) 
                     END DO
     ELSE
                     DO J=1, D2
                         WRITE(IU, FORM) (NUM2STR(ARR(I,J)), I=1, D1)
                     END DO
     END IF
  ELSE
     IF    (HAS_FMT) THEN
                     DO I=1, D1
                         WRITE(IU, FORM) ARR(I,:)
                     END DO
     ELSEIF(PAD /= Z) THEN
                     DO I=1, D1
                         WRITE(IU, FORM) (NUM2STR(ARR(I,J), PAD, LS=LS, RS=RS), J=1, D2) 
                     END DO
     ELSE
                     DO I=1, D1
                         WRITE(IU, FORM) (NUM2STR(ARR(I,J)), J=1, D2)
                     END DO
     END IF
  END IF
  !
  END SUBROUTINE WRITE_ARRAY_2D_INT08
  !
  !##########################################################################################################################
  ! INTEGER(INT16) 2D ARRAY WRITE
  !
  SUBROUTINE WRITE_ARRAY_2D_INT16(IU, ARR, WIDTH, FMT, SEP, TRANSPOSE, HED, SEP_ON_LAST)
  INTEGER,                        INTENT(IN):: IU
  INTEGER(INT16), DIMENSION(:,:), INTENT(IN):: ARR
  INTEGER,              OPTIONAL, INTENT(IN):: WIDTH
  CHARACTER(*),         OPTIONAL, INTENT(IN):: FMT
  CHARACTER(*),         OPTIONAL, INTENT(IN):: SEP
  LOGICAL,              OPTIONAL, INTENT(IN):: TRANSPOSE
  CHARACTER(*),         OPTIONAL, INTENT(IN):: HED
  LOGICAL,              OPTIONAL, INTENT(IN):: SEP_ON_LAST
  !
  INTEGER:: I, J, D1, D2, PAD
  LOGICAL:: HAS_FMT, HAS_SEP, NO_TRANSPOSE
  CHARACTER(:), ALLOCATABLE:: FORM, FM
  LOGICAL:: LS, RS
  !
  IF(PRESENT(HED)) WRITE(IU, '(A)') HED
  !
  D1 = SIZE(ARR,1)
  D2 = SIZE(ARR,2)
  !
  IF(ALLOCATED(FORM)) DEALLOCATE(FORM)
  !
  NO_TRANSPOSE = TRUE
  IF(PRESENT(TRANSPOSE)) NO_TRANSPOSE = .NOT. TRANSPOSE
  !
  HAS_SEP = PRESENT(SEP)
  !
  PAD = Z
  IF(PRESENT(WIDTH)) PAD = WIDTH
  !
  LS = FALSE
  RS = FALSE
  IF(NO_TRANSPOSE .AND. .NOT. HAS_SEP) THEN
     IF    (PAD > Z) THEN
                     LS = TRUE
     ELSEIF(PAD < Z) THEN
                     RS = TRUE
     END IF
  END IF
  !
  HAS_FMT = PRESENT(FMT)
  IF(HAS_FMT) THEN
              IF(FMT == "") HAS_FMT = FALSE
  END IF
  IF(HAS_FMT) THEN
              FM = TRIM(FMT)
  ELSE
              FM = 'A'
  END IF
  !
  IF    (HAS_SEP)  THEN
                   FORM = '(*('//FM//', :, "'//SEP//'"))'
  ELSEIF(PAD /= Z) THEN
                   FORM = '(*('//FM//'))'
  ELSE
                    FORM = '(*('//FM//', :, 1x))'
  END IF
  !
  IF(PRESENT(SEP_ON_LAST) .AND. HAS_SEP) THEN
          IF(SEP_ON_LAST) THEN
              I = INDEX(FORM, ":")
              IF( I > Z ) THEN
                  FORM(I:I+1) = "  "  !Drop : from format
              END IF
          END IF
  END IF
  !
  IF(NO_TRANSPOSE) THEN
     IF    (HAS_FMT) THEN
                     DO J=1, D2
                         WRITE(IU, FORM) ARR(:,J)
                     END DO
     ELSEIF(PAD /= Z) THEN
                     DO J=1, D2
                         WRITE(IU, FORM) (NUM2STR(ARR(I,J), PAD, LS=LS, RS=RS), I=1, D1) 
                     END DO
     ELSE
                     DO J=1, D2
                         WRITE(IU, FORM) (NUM2STR(ARR(I,J)), I=1, D1)
                     END DO
     END IF
  ELSE
     IF    (HAS_FMT) THEN
                     DO I=1, D1
                         WRITE(IU, FORM) ARR(I,:)
                     END DO
     ELSEIF(PAD /= Z) THEN
                     DO I=1, D1
                         WRITE(IU, FORM) (NUM2STR(ARR(I,J), PAD, LS=LS, RS=RS), J=1, D2) 
                     END DO
     ELSE
                     DO I=1, D1
                         WRITE(IU, FORM) (NUM2STR(ARR(I,J)), J=1, D2)
                     END DO
     END IF
  END IF
  !
  END SUBROUTINE WRITE_ARRAY_2D_INT16
  !
  !##########################################################################################################################
  ! INTEGER(INT32) 2D ARRAY WRITE -> most compliers have INTEGER(INT32) <=> INTEGER
  !
  SUBROUTINE WRITE_ARRAY_2D_INT32(IU, ARR, WIDTH, FMT, SEP, TRANSPOSE, HED, SEP_ON_LAST)
  INTEGER,                        INTENT(IN):: IU
  INTEGER(INT32), DIMENSION(:,:), INTENT(IN):: ARR
  INTEGER,              OPTIONAL, INTENT(IN):: WIDTH
  CHARACTER(*),         OPTIONAL, INTENT(IN):: FMT
  CHARACTER(*),         OPTIONAL, INTENT(IN):: SEP
  LOGICAL,              OPTIONAL, INTENT(IN):: TRANSPOSE
  CHARACTER(*),         OPTIONAL, INTENT(IN):: HED
  LOGICAL,              OPTIONAL, INTENT(IN):: SEP_ON_LAST
  !
  INTEGER:: I, J, D1, D2, PAD
  LOGICAL:: HAS_FMT, HAS_SEP, NO_TRANSPOSE
  CHARACTER(:), ALLOCATABLE:: FORM, FM
  LOGICAL:: LS, RS
  !
  IF(PRESENT(HED)) WRITE(IU, '(A)') HED
  !
  D1 = SIZE(ARR,1)
  D2 = SIZE(ARR,2)
  !
  IF(ALLOCATED(FORM)) DEALLOCATE(FORM)
  !
  NO_TRANSPOSE = TRUE
  IF(PRESENT(TRANSPOSE)) NO_TRANSPOSE = .NOT. TRANSPOSE
  !
  HAS_SEP = PRESENT(SEP)
  !
  PAD = Z
  IF(PRESENT(WIDTH)) PAD = WIDTH
  !
  LS = FALSE
  RS = FALSE
  IF(NO_TRANSPOSE .AND. .NOT. HAS_SEP) THEN
     IF    (PAD > Z) THEN
                     LS = TRUE
     ELSEIF(PAD < Z) THEN
                     RS = TRUE
     END IF
  END IF
  !
  HAS_FMT = PRESENT(FMT)
  IF(HAS_FMT) THEN
              IF(FMT == "") HAS_FMT = FALSE
  END IF
  IF(HAS_FMT) THEN
              FM = TRIM(FMT)
  ELSE
              FM = 'A'
  END IF
  !
  IF    (HAS_SEP)  THEN
                   FORM = '(*('//FM//', :, "'//SEP//'"))'
  ELSEIF(PAD /= Z) THEN
                   FORM = '(*('//FM//'))'
  ELSE
                    FORM = '(*('//FM//', :, 1x))'
  END IF
  !
  IF(PRESENT(SEP_ON_LAST) .AND. HAS_SEP) THEN
          IF(SEP_ON_LAST) THEN
              I = INDEX(FORM, ":")
              IF( I > Z ) THEN
                  FORM(I:I+1) = "  "  !Drop : from format
              END IF
          END IF
  END IF
  !
  IF(NO_TRANSPOSE) THEN
     IF    (HAS_FMT) THEN
                     DO J=1, D2
                         WRITE(IU, FORM) ARR(:,J)
                     END DO
     ELSEIF(PAD /= Z) THEN
                     DO J=1, D2
                         WRITE(IU, FORM) (NUM2STR(ARR(I,J), PAD, LS=LS, RS=RS), I=1, D1) 
                     END DO
     ELSE
                     DO J=1, D2
                         WRITE(IU, FORM) (NUM2STR(ARR(I,J)), I=1, D1)
                     END DO
     END IF
  ELSE
     IF    (HAS_FMT) THEN
                     DO I=1, D1
                         WRITE(IU, FORM) ARR(I,:)
                     END DO
     ELSEIF(PAD /= Z) THEN
                     DO I=1, D1
                         WRITE(IU, FORM) (NUM2STR(ARR(I,J), PAD, LS=LS, RS=RS), J=1, D2) 
                     END DO
     ELSE
                     DO I=1, D1
                         WRITE(IU, FORM) (NUM2STR(ARR(I,J)), J=1, D2)
                     END DO
     END IF
  END IF
  !
  END SUBROUTINE WRITE_ARRAY_2D_INT32
  !
  !##########################################################################################################################
  ! INTEGER(INT64) 2D ARRAY WRITE
  !
  SUBROUTINE WRITE_ARRAY_2D_INT64(IU, ARR, WIDTH, FMT, SEP, TRANSPOSE, HED, SEP_ON_LAST)
  INTEGER,                        INTENT(IN):: IU
  INTEGER(INT64), DIMENSION(:,:), INTENT(IN):: ARR
  INTEGER,              OPTIONAL, INTENT(IN):: WIDTH
  CHARACTER(*),         OPTIONAL, INTENT(IN):: FMT
  CHARACTER(*),         OPTIONAL, INTENT(IN):: SEP
  LOGICAL,              OPTIONAL, INTENT(IN):: TRANSPOSE
  CHARACTER(*),         OPTIONAL, INTENT(IN):: HED
  LOGICAL,              OPTIONAL, INTENT(IN):: SEP_ON_LAST
  !
  INTEGER:: I, J, D1, D2, PAD
  LOGICAL:: HAS_FMT, HAS_SEP, NO_TRANSPOSE
  CHARACTER(:), ALLOCATABLE:: FORM, FM
  LOGICAL:: LS, RS
  !
  IF(PRESENT(HED)) WRITE(IU, '(A)') HED
  !
  D1 = SIZE(ARR,1)
  D2 = SIZE(ARR,2)
  !
  IF(ALLOCATED(FORM)) DEALLOCATE(FORM)
  !
  NO_TRANSPOSE = TRUE
  IF(PRESENT(TRANSPOSE)) NO_TRANSPOSE = .NOT. TRANSPOSE
  !
  HAS_SEP = PRESENT(SEP)
  !
  PAD = Z
  IF(PRESENT(WIDTH)) PAD = WIDTH
  !
  LS = FALSE
  RS = FALSE
  IF(NO_TRANSPOSE .AND. .NOT. HAS_SEP) THEN
     IF    (PAD > Z) THEN
                     LS = TRUE
     ELSEIF(PAD < Z) THEN
                     RS = TRUE
     END IF
  END IF
  !
  HAS_FMT = PRESENT(FMT)
  IF(HAS_FMT) THEN
              IF(FMT == "") HAS_FMT = FALSE
  END IF
  IF(HAS_FMT) THEN
              FM = TRIM(FMT)
  ELSE
              FM = 'A'
  END IF
  !
  IF    (HAS_SEP)  THEN
                   FORM = '(*('//FM//', :, "'//SEP//'"))'
  ELSEIF(PAD /= Z) THEN
                   FORM = '(*('//FM//'))'
  ELSE
                    FORM = '(*('//FM//', :, 1x))'
  END IF
  !
  IF(PRESENT(SEP_ON_LAST) .AND. HAS_SEP) THEN
          IF(SEP_ON_LAST) THEN
              I = INDEX(FORM, ":")
              IF( I > Z ) THEN
                  FORM(I:I+1) = "  "  !Drop : from format
              END IF
          END IF
  END IF
  !
  IF(NO_TRANSPOSE) THEN
     IF    (HAS_FMT) THEN
                     DO J=1, D2
                         WRITE(IU, FORM) ARR(:,J)
                     END DO
     ELSEIF(PAD /= Z) THEN
                     DO J=1, D2
                         WRITE(IU, FORM) (NUM2STR(ARR(I,J), PAD, LS=LS, RS=RS), I=1, D1) 
                     END DO
     ELSE
                     DO J=1, D2
                         WRITE(IU, FORM) (NUM2STR(ARR(I,J)), I=1, D1)
                     END DO
     END IF
  ELSE
     IF    (HAS_FMT) THEN
                     DO I=1, D1
                         WRITE(IU, FORM) ARR(I,:)
                     END DO
     ELSEIF(PAD /= Z) THEN
                     DO I=1, D1
                         WRITE(IU, FORM) (NUM2STR(ARR(I,J), PAD, LS=LS, RS=RS), J=1, D2) 
                     END DO
     ELSE
                     DO I=1, D1
                         WRITE(IU, FORM) (NUM2STR(ARR(I,J)), J=1, D2)
                     END DO
     END IF
  END IF
  !
  END SUBROUTINE WRITE_ARRAY_2D_INT64
  !
  !##########################################################################################################################
  ! REAL32 2D ARRAY WRITE - SINGLE PRECISION ARRAY WRITE
  !
  SUBROUTINE WRITE_ARRAY_2D_REL32(IU, ARR, WIDTH, FMT, SEP, TRANSPOSE, HED, SEP_ON_LAST)
  INTEGER,                      INTENT(IN):: IU
  REAL(REAL32), DIMENSION(:,:), INTENT(IN):: ARR
  INTEGER,            OPTIONAL, INTENT(IN):: WIDTH
  CHARACTER(*),       OPTIONAL, INTENT(IN):: FMT
  CHARACTER(*),       OPTIONAL, INTENT(IN):: SEP
  LOGICAL,            OPTIONAL, INTENT(IN):: TRANSPOSE
  CHARACTER(*),       OPTIONAL, INTENT(IN):: HED
  LOGICAL,            OPTIONAL, INTENT(IN):: SEP_ON_LAST
  !
  INTEGER:: I, J, D1, D2, PAD
  LOGICAL:: HAS_FMT, HAS_SEP, NO_TRANSPOSE
  CHARACTER(:), ALLOCATABLE:: FORM, FM
  LOGICAL:: LS, RS
  !
  IF(PRESENT(HED)) WRITE(IU, '(A)') HED
  !
  D1 = SIZE(ARR,1)
  D2 = SIZE(ARR,2)
  !
  IF(ALLOCATED(FORM)) DEALLOCATE(FORM)
  !
  NO_TRANSPOSE = TRUE
  IF(PRESENT(TRANSPOSE)) NO_TRANSPOSE = .NOT. TRANSPOSE
  !
  HAS_SEP = PRESENT(SEP)
  !
  PAD = Z
  IF(PRESENT(WIDTH)) PAD = WIDTH
  !
  LS = FALSE
  RS = FALSE
  IF(NO_TRANSPOSE .AND. .NOT. HAS_SEP) THEN
     IF    (PAD > Z) THEN
                     LS = TRUE
     ELSEIF(PAD < Z) THEN
                     RS = TRUE
     END IF
  END IF
  !
  HAS_FMT = PRESENT(FMT)
  IF(HAS_FMT) THEN
              IF(FMT == "") HAS_FMT = FALSE
  END IF
  IF(HAS_FMT) THEN
              FM = TRIM(FMT)
  ELSE
              FM = 'A'
  END IF
  !
  IF    (HAS_SEP)  THEN
                   FORM = '(*('//FM//', :, "'//SEP//'"))'
  ELSEIF(PAD /= Z) THEN
                   FORM = '(*('//FM//'))'
  ELSE
                    FORM = '(*('//FM//', :, 1x))'
  END IF
  !
  IF(PRESENT(SEP_ON_LAST) .AND. HAS_SEP) THEN
          IF(SEP_ON_LAST) THEN
              I = INDEX(FORM, ":")
              IF( I > Z ) THEN
                  FORM(I:I+1) = "  "  !Drop : from format
              END IF
          END IF
  END IF
  !
  IF(NO_TRANSPOSE) THEN
     IF    (HAS_FMT) THEN
                     DO J=1, D2
                         WRITE(IU, FORM) ARR(:,J)
                     END DO
     ELSEIF(PAD /= Z) THEN
                     DO J=1, D2
                         WRITE(IU, FORM) (NUM2STR(ARR(I,J), PAD, LS=LS, RS=RS), I=1, D1) 
                     END DO
     ELSE
                     DO J=1, D2
                         WRITE(IU, FORM) (NUM2STR(ARR(I,J)), I=1, D1)
                     END DO
     END IF
  ELSE
     IF    (HAS_FMT) THEN
                     DO I=1, D1
                         WRITE(IU, FORM) ARR(I,:)
                     END DO
     ELSEIF(PAD /= Z) THEN
                     DO I=1, D1
                         WRITE(IU, FORM) (NUM2STR(ARR(I,J), PAD, LS=LS, RS=RS), J=1, D2) 
                     END DO
     ELSE
                     DO I=1, D1
                         WRITE(IU, FORM) (NUM2STR(ARR(I,J)), J=1, D2)
                     END DO
     END IF
  END IF
  !
  END SUBROUTINE WRITE_ARRAY_2D_REL32
  !
  !##########################################################################################################################
  ! REAL64 2D ARRAY WRITE - DOUBLE PRECISION ARRAY WRITE
  !
  SUBROUTINE WRITE_ARRAY_2D_REL64(IU, ARR, WIDTH, FMT, SEP, TRANSPOSE, HED, SEP_ON_LAST)
  INTEGER,                      INTENT(IN):: IU
  REAL(REAL64), DIMENSION(:,:), INTENT(IN):: ARR
  INTEGER,            OPTIONAL, INTENT(IN):: WIDTH
  CHARACTER(*),       OPTIONAL, INTENT(IN):: FMT
  CHARACTER(*),       OPTIONAL, INTENT(IN):: SEP
  LOGICAL,            OPTIONAL, INTENT(IN):: TRANSPOSE
  CHARACTER(*),       OPTIONAL, INTENT(IN):: HED
  LOGICAL,            OPTIONAL, INTENT(IN):: SEP_ON_LAST
  !
  INTEGER:: I, J, D1, D2, PAD
  LOGICAL:: HAS_FMT, HAS_SEP, NO_TRANSPOSE
  CHARACTER(:), ALLOCATABLE:: FORM, FM
  LOGICAL:: LS, RS
  !
  IF(PRESENT(HED)) WRITE(IU, '(A)') HED
  !
  D1 = SIZE(ARR,1)
  D2 = SIZE(ARR,2)
  !
  IF(ALLOCATED(FORM)) DEALLOCATE(FORM)
  !
  NO_TRANSPOSE = TRUE
  IF(PRESENT(TRANSPOSE)) NO_TRANSPOSE = .NOT. TRANSPOSE
  !
  HAS_SEP = PRESENT(SEP)
  !
  PAD = Z
  IF(PRESENT(WIDTH)) PAD = WIDTH
  !
  LS = FALSE
  RS = FALSE
  IF(NO_TRANSPOSE .AND. .NOT. HAS_SEP) THEN
     IF    (PAD > Z) THEN
                     LS = TRUE
     ELSEIF(PAD < Z) THEN
                     RS = TRUE
     END IF
  END IF
  !
  HAS_FMT = PRESENT(FMT)
  IF(HAS_FMT) THEN
              IF(FMT == "") HAS_FMT = FALSE
  END IF
  IF(HAS_FMT) THEN
              FM = TRIM(FMT)
  ELSE
              FM = 'A'
  END IF
  !
  IF    (HAS_SEP)  THEN
                   FORM = '(*('//FM//', :, "'//SEP//'"))'
  ELSEIF(PAD /= Z) THEN
                   FORM = '(*('//FM//'))'
  ELSE
                    FORM = '(*('//FM//', :, 1x))'
  END IF
  !
  IF(PRESENT(SEP_ON_LAST) .AND. HAS_SEP) THEN
          IF(SEP_ON_LAST) THEN
              I = INDEX(FORM, ":")
              IF( I > Z ) THEN
                  FORM(I:I+1) = "  "  !Drop : from format
              END IF
          END IF
  END IF
  !
  IF(NO_TRANSPOSE) THEN
     IF    (HAS_FMT) THEN
                     DO J=1, D2
                         WRITE(IU, FORM) ARR(:,J)
                     END DO
     ELSEIF(PAD /= Z) THEN
                     DO J=1, D2
                         WRITE(IU, FORM) (NUM2STR(ARR(I,J), PAD, LS=LS, RS=RS), I=1, D1) 
                     END DO
     ELSE
                     DO J=1, D2
                         WRITE(IU, FORM) (NUM2STR(ARR(I,J)), I=1, D1)
                     END DO
     END IF
  ELSE
     IF    (HAS_FMT) THEN
                     DO I=1, D1
                         WRITE(IU, FORM) ARR(I,:)
                     END DO
     ELSEIF(PAD /= Z) THEN
                     DO I=1, D1
                         WRITE(IU, FORM) (NUM2STR(ARR(I,J), PAD, LS=LS, RS=RS), J=1, D2) 
                     END DO
     ELSE
                     DO I=1, D1
                         WRITE(IU, FORM) (NUM2STR(ARR(I,J)), J=1, D2)
                     END DO
     END IF
  END IF
  !
  END SUBROUTINE WRITE_ARRAY_2D_REL64
  !
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !  
  !  WRITE 1D ARRAY GIVEN A FILE NAME
  !
  !##########################################################################################################################
  ! INTEGER(INT8) 1D ARRAY WRITE GIVEN A FILE NAME
  !
  SUBROUTINE FILE_WRITE_ARRAY_1D_INT08(FNAME, ARR, ERROR, WIDTH, FMT, SEP, TRANSPOSE, HED, SEP_ON_LAST, ADVANCE, IU, NO_CLOSE)
  CHARACTER(*),                 INTENT(IN   ):: FNAME
  INTEGER(INT8),  DIMENSION(:), INTENT(IN   ):: ARR
  LOGICAL,                      INTENT(INOUT):: ERROR
  INTEGER,            OPTIONAL, INTENT(IN   ):: WIDTH
  CHARACTER(*),       OPTIONAL, INTENT(IN   ):: FMT
  CHARACTER(*),       OPTIONAL, INTENT(IN   ):: SEP
  LOGICAL,            OPTIONAL, INTENT(IN   ):: TRANSPOSE
  CHARACTER(*),       OPTIONAL, INTENT(IN   ):: HED
  LOGICAL,            OPTIONAL, INTENT(IN   ):: SEP_ON_LAST
  LOGICAL,            OPTIONAL, INTENT(IN   ):: ADVANCE
  INTEGER,            OPTIONAL, INTENT(INOUT):: IU
  LOGICAL,            OPTIONAL, INTENT(IN   ):: NO_CLOSE
  !
  CHARACTER(8):: WRT, BIN
  LOGICAL:: ISOPEN, CLOSE_IT
  INTEGER:: I, IERR
  !
  ERROR = FALSE
  !
  INQUIRE(FILE=FNAME, NUMBER=I, OPENED=ISOPEN, WRITE=WRT, UNFORMATTED=BIN)
  !
  IF(ISOPEN) THEN
      CLOSE_IT = FALSE
      IF( WRT == "NO" ) ERROR = TRUE
      IF( BIN == "YES") ERROR = TRUE
      IF( PRESENT(IU) ) THEN
          IF( IU /= I ) ERROR = TRUE
      END IF
  ELSE
      I = Z
      IF( PRESENT(IU) ) I = IU
      !
      IF( I /= Z ) THEN
          OPEN(   UNIT=I, FILE=FNAME, ACTION="WRITE",       FORM='FORMATTED', &
               ACCESS='SEQUENTIAL',   STATUS="REPLACE", POSITION='REWIND', IOSTAT=IERR)
      ELSE
          OPEN(NEWUNIT=I, FILE=FNAME, ACTION="WRITE",       FORM='FORMATTED', &
               ACCESS='SEQUENTIAL',   STATUS="REPLACE", POSITION='REWIND', IOSTAT=IERR)
      END IF
      ERROR = IERR /= Z
      CLOSE_IT = .not. ERROR
  END IF
  !
  IF( .not. ERROR) CALL WRITE_ARRAY_1D_INT08(I, ARR, WIDTH, FMT, SEP, TRANSPOSE, HED, SEP_ON_LAST, ADVANCE)
  !
  IF( CLOSE_IT .AND. PRESENT(NO_CLOSE)) CLOSE_IT = .not. NO_CLOSE 
  IF( CLOSE_IT ) THEN
                 CLOSE(I, IOSTAT=IERR)
  ELSEIF( PRESENT(IU) ) THEN
                 IF(IU==Z) IU = I
  END IF
  !
  END SUBROUTINE FILE_WRITE_ARRAY_1D_INT08
  !
  !##########################################################################################################################
  ! INTEGER(INT16) 1D ARRAY WRITE GIVEN A FILE NAME
  !
  SUBROUTINE FILE_WRITE_ARRAY_1D_INT16(FNAME, ARR, ERROR, WIDTH, FMT, SEP, TRANSPOSE, HED, SEP_ON_LAST, ADVANCE, IU, NO_CLOSE)
  CHARACTER(*),                 INTENT(IN   ):: FNAME
  INTEGER(INT16), DIMENSION(:), INTENT(IN   ):: ARR
  LOGICAL,                      INTENT(INOUT):: ERROR
  INTEGER,            OPTIONAL, INTENT(IN   ):: WIDTH
  CHARACTER(*),       OPTIONAL, INTENT(IN   ):: FMT
  CHARACTER(*),       OPTIONAL, INTENT(IN   ):: SEP
  LOGICAL,            OPTIONAL, INTENT(IN   ):: TRANSPOSE
  CHARACTER(*),       OPTIONAL, INTENT(IN   ):: HED
  LOGICAL,            OPTIONAL, INTENT(IN   ):: SEP_ON_LAST
  LOGICAL,            OPTIONAL, INTENT(IN   ):: ADVANCE
  INTEGER,            OPTIONAL, INTENT(INOUT):: IU
  LOGICAL,            OPTIONAL, INTENT(IN   ):: NO_CLOSE
  !
  CHARACTER(8):: WRT, BIN
  LOGICAL:: ISOPEN, CLOSE_IT
  INTEGER:: I, IERR
  !
  ERROR = FALSE
  !
  INQUIRE(FILE=FNAME, NUMBER=I, OPENED=ISOPEN, WRITE=WRT, UNFORMATTED=BIN)
  !
  IF(ISOPEN) THEN
      CLOSE_IT = FALSE
      IF( WRT == "NO" ) ERROR = TRUE
      IF( BIN == "YES") ERROR = TRUE
      IF( PRESENT(IU) ) THEN
          IF( IU /= I ) ERROR = TRUE
      END IF
  ELSE
      I = Z
      IF( PRESENT(IU) ) I = IU
      !
      IF( I /= Z ) THEN
          OPEN(   UNIT=I, FILE=FNAME, ACTION="WRITE",       FORM='FORMATTED', &
               ACCESS='SEQUENTIAL',   STATUS="REPLACE", POSITION='REWIND', IOSTAT=IERR)
      ELSE
          OPEN(NEWUNIT=I, FILE=FNAME, ACTION="WRITE",       FORM='FORMATTED', &
               ACCESS='SEQUENTIAL',   STATUS="REPLACE", POSITION='REWIND', IOSTAT=IERR)
      END IF
      ERROR = IERR /= Z
      CLOSE_IT = .not. ERROR
  END IF
  !
  IF( .not. ERROR) CALL WRITE_ARRAY_1D_INT16(I, ARR, WIDTH, FMT, SEP, TRANSPOSE, HED, SEP_ON_LAST, ADVANCE)
  !
  IF( CLOSE_IT .AND. PRESENT(NO_CLOSE)) CLOSE_IT = .not. NO_CLOSE 
  IF( CLOSE_IT ) THEN
                 CLOSE(I, IOSTAT=IERR)
  ELSEIF( PRESENT(IU) ) THEN
                 IF(IU==Z) IU = I
  END IF
  !
  END SUBROUTINE FILE_WRITE_ARRAY_1D_INT16
  !
  !##########################################################################################################################
  ! INTEGER(INT32) 1D ARRAY WRITE GIVEN A FILE NAME -> most compliers have INTEGER(INT32) <=> INTEGER
  !
  SUBROUTINE FILE_WRITE_ARRAY_1D_INT32(FNAME, ARR, ERROR, WIDTH, FMT, SEP, TRANSPOSE, HED, SEP_ON_LAST, ADVANCE, IU, NO_CLOSE)
  CHARACTER(*),                 INTENT(IN   ):: FNAME
  INTEGER(INT32), DIMENSION(:), INTENT(IN   ):: ARR
  LOGICAL,                      INTENT(INOUT):: ERROR
  INTEGER,            OPTIONAL, INTENT(IN   ):: WIDTH
  CHARACTER(*),       OPTIONAL, INTENT(IN   ):: FMT
  CHARACTER(*),       OPTIONAL, INTENT(IN   ):: SEP
  LOGICAL,            OPTIONAL, INTENT(IN   ):: TRANSPOSE
  CHARACTER(*),       OPTIONAL, INTENT(IN   ):: HED
  LOGICAL,            OPTIONAL, INTENT(IN   ):: SEP_ON_LAST
  LOGICAL,            OPTIONAL, INTENT(IN   ):: ADVANCE
  INTEGER,            OPTIONAL, INTENT(INOUT):: IU
  LOGICAL,            OPTIONAL, INTENT(IN   ):: NO_CLOSE
  !
  CHARACTER(8):: WRT, BIN
  LOGICAL:: ISOPEN, CLOSE_IT
  INTEGER:: I, IERR
  !
  ERROR = FALSE
  !
  INQUIRE(FILE=FNAME, NUMBER=I, OPENED=ISOPEN, WRITE=WRT, UNFORMATTED=BIN)
  !
  IF(ISOPEN) THEN
      CLOSE_IT = FALSE
      IF( WRT == "NO" ) ERROR = TRUE
      IF( BIN == "YES") ERROR = TRUE
      IF( PRESENT(IU) ) THEN
          IF( IU /= I ) ERROR = TRUE
      END IF
  ELSE
      I = Z
      IF( PRESENT(IU) ) I = IU
      !
      IF( I /= Z ) THEN
          OPEN(   UNIT=I, FILE=FNAME, ACTION="WRITE",       FORM='FORMATTED', &
               ACCESS='SEQUENTIAL',   STATUS="REPLACE", POSITION='REWIND', IOSTAT=IERR)
      ELSE
          OPEN(NEWUNIT=I, FILE=FNAME, ACTION="WRITE",       FORM='FORMATTED', &
               ACCESS='SEQUENTIAL',   STATUS="REPLACE", POSITION='REWIND', IOSTAT=IERR)
      END IF
      ERROR = IERR /= Z
      CLOSE_IT = .not. ERROR
  END IF
  !
  IF( .not. ERROR) CALL WRITE_ARRAY_1D_INT32(I, ARR, WIDTH, FMT, SEP, TRANSPOSE, HED, SEP_ON_LAST, ADVANCE)
  !
  IF( CLOSE_IT .AND. PRESENT(NO_CLOSE)) CLOSE_IT = .not. NO_CLOSE 
  IF( CLOSE_IT ) THEN
                 CLOSE(I, IOSTAT=IERR)
  ELSEIF( PRESENT(IU) ) THEN
                 IF(IU==Z) IU = I
  END IF
  !
  END SUBROUTINE FILE_WRITE_ARRAY_1D_INT32
  !
  !##########################################################################################################################
  ! INTEGER(INT64) 1D ARRAY WRITE GIVEN A FILE NAME
  !
  SUBROUTINE FILE_WRITE_ARRAY_1D_INT64(FNAME, ARR, ERROR, WIDTH, FMT, SEP, TRANSPOSE, HED, SEP_ON_LAST, ADVANCE, IU, NO_CLOSE)
  CHARACTER(*),                 INTENT(IN   ):: FNAME
  INTEGER(INT64), DIMENSION(:), INTENT(IN   ):: ARR
  LOGICAL,                      INTENT(INOUT):: ERROR
  INTEGER,            OPTIONAL, INTENT(IN   ):: WIDTH
  CHARACTER(*),       OPTIONAL, INTENT(IN   ):: FMT
  CHARACTER(*),       OPTIONAL, INTENT(IN   ):: SEP
  LOGICAL,            OPTIONAL, INTENT(IN   ):: TRANSPOSE
  CHARACTER(*),       OPTIONAL, INTENT(IN   ):: HED
  LOGICAL,            OPTIONAL, INTENT(IN   ):: SEP_ON_LAST
  LOGICAL,            OPTIONAL, INTENT(IN   ):: ADVANCE
  INTEGER,            OPTIONAL, INTENT(INOUT):: IU
  LOGICAL,            OPTIONAL, INTENT(IN   ):: NO_CLOSE
  !
  CHARACTER(8):: WRT, BIN
  LOGICAL:: ISOPEN, CLOSE_IT
  INTEGER:: I, IERR
  !
  ERROR = FALSE
  !
  INQUIRE(FILE=FNAME, NUMBER=I, OPENED=ISOPEN, WRITE=WRT, UNFORMATTED=BIN)
  !
  IF(ISOPEN) THEN
      CLOSE_IT = FALSE
      IF( WRT == "NO" ) ERROR = TRUE
      IF( BIN == "YES") ERROR = TRUE
      IF( PRESENT(IU) ) THEN
          IF( IU /= I ) ERROR = TRUE
      END IF
  ELSE
      I = Z
      IF( PRESENT(IU) ) I = IU
      !
      IF( I /= Z ) THEN
          OPEN(   UNIT=I, FILE=FNAME, ACTION="WRITE",       FORM='FORMATTED', &
               ACCESS='SEQUENTIAL',   STATUS="REPLACE", POSITION='REWIND', IOSTAT=IERR)
      ELSE
          OPEN(NEWUNIT=I, FILE=FNAME, ACTION="WRITE",       FORM='FORMATTED', &
               ACCESS='SEQUENTIAL',   STATUS="REPLACE", POSITION='REWIND', IOSTAT=IERR)
      END IF
      ERROR = IERR /= Z
      CLOSE_IT = .not. ERROR
  END IF
  !
  IF( .not. ERROR) CALL WRITE_ARRAY_1D_INT64(I, ARR, WIDTH, FMT, SEP, TRANSPOSE, HED, SEP_ON_LAST, ADVANCE)
  !
  IF( CLOSE_IT .AND. PRESENT(NO_CLOSE)) CLOSE_IT = .not. NO_CLOSE 
  IF( CLOSE_IT ) THEN
                 CLOSE(I, IOSTAT=IERR)
  ELSEIF( PRESENT(IU) ) THEN
                 IF(IU==Z) IU = I
  END IF
  !
  END SUBROUTINE FILE_WRITE_ARRAY_1D_INT64
  !
  !##########################################################################################################################
  ! REAL(REAL32) 1D ARRAY WRITE GIVEN A FILE NAME - SINGLE PRECISION ARRAY WRITE
  !
  SUBROUTINE FILE_WRITE_ARRAY_1D_REL32(FNAME, ARR, ERROR, WIDTH, FMT, SEP, TRANSPOSE, HED, SEP_ON_LAST, ADVANCE, IU, NO_CLOSE)
  CHARACTER(*),               INTENT(IN   ):: FNAME
  REAL(REAL32), DIMENSION(:), INTENT(IN   ):: ARR
  LOGICAL,                    INTENT(INOUT):: ERROR
  INTEGER,          OPTIONAL, INTENT(IN   ):: WIDTH
  CHARACTER(*),     OPTIONAL, INTENT(IN   ):: FMT
  CHARACTER(*),     OPTIONAL, INTENT(IN   ):: SEP
  LOGICAL,          OPTIONAL, INTENT(IN   ):: TRANSPOSE
  CHARACTER(*),     OPTIONAL, INTENT(IN   ):: HED
  LOGICAL,          OPTIONAL, INTENT(IN   ):: SEP_ON_LAST
  LOGICAL,          OPTIONAL, INTENT(IN   ):: ADVANCE
  INTEGER,          OPTIONAL, INTENT(INOUT):: IU
  LOGICAL,          OPTIONAL, INTENT(IN   ):: NO_CLOSE
  !
  CHARACTER(8):: WRT, BIN
  LOGICAL:: ISOPEN, CLOSE_IT
  INTEGER:: I, IERR
  !
  ERROR = FALSE
  !
  INQUIRE(FILE=FNAME, NUMBER=I, OPENED=ISOPEN, WRITE=WRT, UNFORMATTED=BIN)
  !
  IF(ISOPEN) THEN
      CLOSE_IT = FALSE
      IF( WRT == "NO" ) ERROR = TRUE
      IF( BIN == "YES") ERROR = TRUE
      IF( PRESENT(IU) ) THEN
          IF( IU /= I ) ERROR = TRUE
      END IF
  ELSE
      I = Z
      IF( PRESENT(IU) ) I = IU
      !
      IF( I /= Z ) THEN
          OPEN(   UNIT=I, FILE=FNAME, ACTION="WRITE",       FORM='FORMATTED', &
               ACCESS='SEQUENTIAL',   STATUS="REPLACE", POSITION='REWIND', IOSTAT=IERR)
      ELSE
          OPEN(NEWUNIT=I, FILE=FNAME, ACTION="WRITE",       FORM='FORMATTED', &
               ACCESS='SEQUENTIAL',   STATUS="REPLACE", POSITION='REWIND', IOSTAT=IERR)
      END IF
      ERROR = IERR /= Z
      CLOSE_IT = .not. ERROR
  END IF
  !
  IF( .not. ERROR) CALL WRITE_ARRAY_1D_REL32(I, ARR, WIDTH, FMT, SEP, TRANSPOSE, HED, SEP_ON_LAST, ADVANCE)
  !
  IF( CLOSE_IT .AND. PRESENT(NO_CLOSE)) CLOSE_IT = .not. NO_CLOSE 
  IF( CLOSE_IT ) THEN
                 CLOSE(I, IOSTAT=IERR)
  ELSEIF( PRESENT(IU) ) THEN
                 IF(IU==Z) IU = I
  END IF
  !
  END SUBROUTINE FILE_WRITE_ARRAY_1D_REL32
  !
  !##########################################################################################################################
  ! REAL(REAL64) 1D ARRAY WRITE GIVEN A FILE NAME - DOUBLE PRECISION ARRAY WRITE
  !
  SUBROUTINE FILE_WRITE_ARRAY_1D_REL64(FNAME, ARR, ERROR, WIDTH, FMT, SEP, TRANSPOSE, HED, SEP_ON_LAST, ADVANCE, IU, NO_CLOSE)
  CHARACTER(*),               INTENT(IN   ):: FNAME
  REAL(REAL64), DIMENSION(:), INTENT(IN   ):: ARR
  LOGICAL,                    INTENT(INOUT):: ERROR
  INTEGER,          OPTIONAL, INTENT(IN   ):: WIDTH
  CHARACTER(*),     OPTIONAL, INTENT(IN   ):: FMT
  CHARACTER(*),     OPTIONAL, INTENT(IN   ):: SEP
  LOGICAL,          OPTIONAL, INTENT(IN   ):: TRANSPOSE
  CHARACTER(*),     OPTIONAL, INTENT(IN   ):: HED
  LOGICAL,          OPTIONAL, INTENT(IN   ):: SEP_ON_LAST
  LOGICAL,          OPTIONAL, INTENT(IN   ):: ADVANCE
  INTEGER,          OPTIONAL, INTENT(INOUT):: IU
  LOGICAL,          OPTIONAL, INTENT(IN   ):: NO_CLOSE
  !
  CHARACTER(8):: WRT, BIN
  LOGICAL:: ISOPEN, CLOSE_IT
  INTEGER:: I, IERR
  !
  ERROR = FALSE
  !
  INQUIRE(FILE=FNAME, NUMBER=I, OPENED=ISOPEN, WRITE=WRT, UNFORMATTED=BIN)
  !
  IF(ISOPEN) THEN
      CLOSE_IT = FALSE
      IF( WRT == "NO" ) ERROR = TRUE
      IF( BIN == "YES") ERROR = TRUE
      IF( PRESENT(IU) ) THEN
          IF( IU /= I ) ERROR = TRUE
      END IF
  ELSE
      I = Z
      IF( PRESENT(IU) ) I = IU
      !
      IF( I /= Z ) THEN
          OPEN(   UNIT=I, FILE=FNAME, ACTION="WRITE",       FORM='FORMATTED', &
               ACCESS='SEQUENTIAL',   STATUS="REPLACE", POSITION='REWIND', IOSTAT=IERR)
      ELSE
          OPEN(NEWUNIT=I, FILE=FNAME, ACTION="WRITE",       FORM='FORMATTED', &
               ACCESS='SEQUENTIAL',   STATUS="REPLACE", POSITION='REWIND', IOSTAT=IERR)
      END IF
      ERROR = IERR /= Z
      CLOSE_IT = .not. ERROR
  END IF
  !
  IF( .not. ERROR) CALL WRITE_ARRAY_1D_REL64(I, ARR, WIDTH, FMT, SEP, TRANSPOSE, HED, SEP_ON_LAST, ADVANCE)
  !
  IF( CLOSE_IT .AND. PRESENT(NO_CLOSE)) CLOSE_IT = .not. NO_CLOSE 
  IF( CLOSE_IT ) THEN
                 CLOSE(I, IOSTAT=IERR)
  ELSEIF( PRESENT(IU) ) THEN
                 IF(IU==Z) IU = I
  END IF
  !
  END SUBROUTINE FILE_WRITE_ARRAY_1D_REL64
  !
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !  
  !  WRITE 2D ARRAY GIVEN A FILE NAME
  !
  !##########################################################################################################################
  ! INTEGER(INT8) 2D ARRAY WRITE GIVEN A FILE NAME
  !
  SUBROUTINE FILE_WRITE_ARRAY_2D_INT08(FNAME, ARR, ERROR, WIDTH, FMT, SEP, TRANSPOSE, HED, SEP_ON_LAST, IU, NO_CLOSE)
  CHARACTER(*),                   INTENT(IN   ):: FNAME
  INTEGER(INT8),  DIMENSION(:,:), INTENT(IN   ):: ARR
  LOGICAL,                        INTENT(INOUT):: ERROR
  INTEGER,              OPTIONAL, INTENT(IN   ):: WIDTH
  CHARACTER(*),         OPTIONAL, INTENT(IN   ):: FMT
  CHARACTER(*),         OPTIONAL, INTENT(IN   ):: SEP
  LOGICAL,              OPTIONAL, INTENT(IN   ):: TRANSPOSE
  CHARACTER(*),         OPTIONAL, INTENT(IN   ):: HED
  LOGICAL,              OPTIONAL, INTENT(IN   ):: SEP_ON_LAST
  INTEGER,              OPTIONAL, INTENT(INOUT):: IU
  LOGICAL,              OPTIONAL, INTENT(IN   ):: NO_CLOSE
  !
  CHARACTER(8):: WRT, BIN
  LOGICAL:: ISOPEN, CLOSE_IT
  INTEGER:: I, IERR
  !
  ERROR = FALSE
  !
  INQUIRE(FILE=FNAME, NUMBER=I, OPENED=ISOPEN, WRITE=WRT, UNFORMATTED=BIN)
  !
  IF(ISOPEN) THEN
      CLOSE_IT = FALSE
      IF( WRT == "NO" ) ERROR = TRUE
      IF( BIN == "YES") ERROR = TRUE
      IF( PRESENT(IU) ) THEN
          IF( IU /= I ) ERROR = TRUE
      END IF
  ELSE
      I = Z
      IF( PRESENT(IU) ) I = IU
      !
      IF( I /= Z ) THEN
          OPEN(   UNIT=I, FILE=FNAME, ACTION="WRITE",       FORM='FORMATTED', &
               ACCESS='SEQUENTIAL',   STATUS="REPLACE", POSITION='REWIND', IOSTAT=IERR)
      ELSE
          OPEN(NEWUNIT=I, FILE=FNAME, ACTION="WRITE",       FORM='FORMATTED', &
               ACCESS='SEQUENTIAL',   STATUS="REPLACE", POSITION='REWIND', IOSTAT=IERR)
      END IF
      ERROR = IERR /= Z
      CLOSE_IT = .not. ERROR
  END IF
  !
  IF( .not. ERROR) CALL WRITE_ARRAY_2D_INT08(I, ARR, WIDTH, FMT, SEP, TRANSPOSE, HED, SEP_ON_LAST)
  !
  IF( CLOSE_IT .AND. PRESENT(NO_CLOSE)) CLOSE_IT = .not. NO_CLOSE 
  IF( CLOSE_IT ) THEN
                 CLOSE(I, IOSTAT=IERR)
  ELSEIF( PRESENT(IU) ) THEN
                 IF(IU==Z) IU = I
  END IF
  !
  END SUBROUTINE FILE_WRITE_ARRAY_2D_INT08
  !
  !##########################################################################################################################
  ! INTEGER(INT16) 2D ARRAY WRITE GIVEN A FILE NAME
  !
  SUBROUTINE FILE_WRITE_ARRAY_2D_INT16(FNAME, ARR, ERROR, WIDTH, FMT, SEP, TRANSPOSE, HED, SEP_ON_LAST, IU, NO_CLOSE)
  CHARACTER(*),                   INTENT(IN   ):: FNAME
  INTEGER(INT16), DIMENSION(:,:), INTENT(IN   ):: ARR
  LOGICAL,                        INTENT(INOUT):: ERROR
  INTEGER,              OPTIONAL, INTENT(IN   ):: WIDTH
  CHARACTER(*),         OPTIONAL, INTENT(IN   ):: FMT
  CHARACTER(*),         OPTIONAL, INTENT(IN   ):: SEP
  LOGICAL,              OPTIONAL, INTENT(IN   ):: TRANSPOSE
  CHARACTER(*),         OPTIONAL, INTENT(IN   ):: HED
  LOGICAL,              OPTIONAL, INTENT(IN   ):: SEP_ON_LAST
  INTEGER,              OPTIONAL, INTENT(INOUT):: IU
  LOGICAL,              OPTIONAL, INTENT(IN   ):: NO_CLOSE
  !
  CHARACTER(8):: WRT, BIN
  LOGICAL:: ISOPEN, CLOSE_IT
  INTEGER:: I, IERR
  !
  ERROR = FALSE
  !
  INQUIRE(FILE=FNAME, NUMBER=I, OPENED=ISOPEN, WRITE=WRT, UNFORMATTED=BIN)
  !
  IF(ISOPEN) THEN
      CLOSE_IT = FALSE
      IF( WRT == "NO" ) ERROR = TRUE
      IF( BIN == "YES") ERROR = TRUE
      IF( PRESENT(IU) ) THEN
          IF( IU /= I ) ERROR = TRUE
      END IF
  ELSE
      I = Z
      IF( PRESENT(IU) ) I = IU
      !
      IF( I /= Z ) THEN
          OPEN(   UNIT=I, FILE=FNAME, ACTION="WRITE",       FORM='FORMATTED', &
               ACCESS='SEQUENTIAL',   STATUS="REPLACE", POSITION='REWIND', IOSTAT=IERR)
      ELSE
          OPEN(NEWUNIT=I, FILE=FNAME, ACTION="WRITE",       FORM='FORMATTED', &
               ACCESS='SEQUENTIAL',   STATUS="REPLACE", POSITION='REWIND', IOSTAT=IERR)
      END IF
      ERROR = IERR /= Z
      CLOSE_IT = .not. ERROR
  END IF
  !
  IF( .not. ERROR) CALL WRITE_ARRAY_2D_INT16(I, ARR, WIDTH, FMT, SEP, TRANSPOSE, HED, SEP_ON_LAST)
  !
  IF( CLOSE_IT .AND. PRESENT(NO_CLOSE)) CLOSE_IT = .not. NO_CLOSE 
  IF( CLOSE_IT ) THEN
                 CLOSE(I, IOSTAT=IERR)
  ELSEIF( PRESENT(IU) ) THEN
                 IF(IU==Z) IU = I
  END IF
  !
  END SUBROUTINE FILE_WRITE_ARRAY_2D_INT16
  !
  !##########################################################################################################################
  ! INTEGER(INT32) 2D ARRAY WRITE GIVEN A FILE NAME -> most compliers have INTEGER(INT32) <=> INTEGER
  !
  SUBROUTINE FILE_WRITE_ARRAY_2D_INT32(FNAME, ARR, ERROR, WIDTH, FMT, SEP, TRANSPOSE, HED, SEP_ON_LAST, IU, NO_CLOSE)
  CHARACTER(*),                   INTENT(IN   ):: FNAME
  INTEGER(INT32), DIMENSION(:,:), INTENT(IN   ):: ARR
  LOGICAL,                        INTENT(INOUT):: ERROR
  INTEGER,              OPTIONAL, INTENT(IN   ):: WIDTH
  CHARACTER(*),         OPTIONAL, INTENT(IN   ):: FMT
  CHARACTER(*),         OPTIONAL, INTENT(IN   ):: SEP
  LOGICAL,              OPTIONAL, INTENT(IN   ):: TRANSPOSE
  CHARACTER(*),         OPTIONAL, INTENT(IN   ):: HED
  LOGICAL,              OPTIONAL, INTENT(IN   ):: SEP_ON_LAST
  INTEGER,              OPTIONAL, INTENT(INOUT):: IU
  LOGICAL,              OPTIONAL, INTENT(IN   ):: NO_CLOSE
  !
  CHARACTER(8):: WRT, BIN
  LOGICAL:: ISOPEN, CLOSE_IT
  INTEGER:: I, IERR
  !
  ERROR = FALSE
  !
  INQUIRE(FILE=FNAME, NUMBER=I, OPENED=ISOPEN, WRITE=WRT, UNFORMATTED=BIN)
  !
  IF(ISOPEN) THEN
      CLOSE_IT = FALSE
      IF( WRT == "NO" ) ERROR = TRUE
      IF( BIN == "YES") ERROR = TRUE
      IF( PRESENT(IU) ) THEN
          IF( IU /= I ) ERROR = TRUE
      END IF
  ELSE
      I = Z
      IF( PRESENT(IU) ) I = IU
      !
      IF( I /= Z ) THEN
          OPEN(   UNIT=I, FILE=FNAME, ACTION="WRITE",       FORM='FORMATTED', &
               ACCESS='SEQUENTIAL',   STATUS="REPLACE", POSITION='REWIND', IOSTAT=IERR)
      ELSE
          OPEN(NEWUNIT=I, FILE=FNAME, ACTION="WRITE",       FORM='FORMATTED', &
               ACCESS='SEQUENTIAL',   STATUS="REPLACE", POSITION='REWIND', IOSTAT=IERR)
      END IF
      ERROR = IERR /= Z
      CLOSE_IT = .not. ERROR
  END IF
  !
  IF( .not. ERROR) CALL WRITE_ARRAY_2D_INT32(I, ARR, WIDTH, FMT, SEP, TRANSPOSE, HED, SEP_ON_LAST)
  !
  IF( CLOSE_IT .AND. PRESENT(NO_CLOSE)) CLOSE_IT = .not. NO_CLOSE 
  IF( CLOSE_IT ) THEN
                 CLOSE(I, IOSTAT=IERR)
  ELSEIF( PRESENT(IU) ) THEN
                 IF(IU==Z) IU = I
  END IF
  !
  END SUBROUTINE FILE_WRITE_ARRAY_2D_INT32
  !
  !##########################################################################################################################
  ! INTEGER(INT64) 2D ARRAY WRITE GIVEN A FILE NAME
  !
  SUBROUTINE FILE_WRITE_ARRAY_2D_INT64(FNAME, ARR, ERROR, WIDTH, FMT, SEP, TRANSPOSE, HED, SEP_ON_LAST, IU, NO_CLOSE)
  CHARACTER(*),                   INTENT(IN   ):: FNAME
  INTEGER(INT64), DIMENSION(:,:), INTENT(IN   ):: ARR
  LOGICAL,                        INTENT(INOUT):: ERROR
  INTEGER,              OPTIONAL, INTENT(IN   ):: WIDTH
  CHARACTER(*),         OPTIONAL, INTENT(IN   ):: FMT
  CHARACTER(*),         OPTIONAL, INTENT(IN   ):: SEP
  LOGICAL,              OPTIONAL, INTENT(IN   ):: TRANSPOSE
  CHARACTER(*),         OPTIONAL, INTENT(IN   ):: HED
  LOGICAL,              OPTIONAL, INTENT(IN   ):: SEP_ON_LAST
  INTEGER,              OPTIONAL, INTENT(INOUT):: IU
  LOGICAL,              OPTIONAL, INTENT(IN   ):: NO_CLOSE
  !
  CHARACTER(8):: WRT, BIN
  LOGICAL:: ISOPEN, CLOSE_IT
  INTEGER:: I, IERR
  !
  ERROR = FALSE
  !
  INQUIRE(FILE=FNAME, NUMBER=I, OPENED=ISOPEN, WRITE=WRT, UNFORMATTED=BIN)
  !
  IF(ISOPEN) THEN
      CLOSE_IT = FALSE
      IF( WRT == "NO" ) ERROR = TRUE
      IF( BIN == "YES") ERROR = TRUE
      IF( PRESENT(IU) ) THEN
          IF( IU /= I ) ERROR = TRUE
      END IF
  ELSE
      I = Z
      IF( PRESENT(IU) ) I = IU
      !
      IF( I /= Z ) THEN
          OPEN(   UNIT=I, FILE=FNAME, ACTION="WRITE",       FORM='FORMATTED', &
               ACCESS='SEQUENTIAL',   STATUS="REPLACE", POSITION='REWIND', IOSTAT=IERR)
      ELSE
          OPEN(NEWUNIT=I, FILE=FNAME, ACTION="WRITE",       FORM='FORMATTED', &
               ACCESS='SEQUENTIAL',   STATUS="REPLACE", POSITION='REWIND', IOSTAT=IERR)
      END IF
      ERROR = IERR /= Z
      CLOSE_IT = .not. ERROR
  END IF
  !
  IF( .not. ERROR) CALL WRITE_ARRAY_2D_INT64(I, ARR, WIDTH, FMT, SEP, TRANSPOSE, HED, SEP_ON_LAST)
  !
  IF( CLOSE_IT .AND. PRESENT(NO_CLOSE)) CLOSE_IT = .not. NO_CLOSE 
  IF( CLOSE_IT ) THEN
                 CLOSE(I, IOSTAT=IERR)
  ELSEIF( PRESENT(IU) ) THEN
                 IF(IU==Z) IU = I
  END IF
  !
  END SUBROUTINE FILE_WRITE_ARRAY_2D_INT64
  !
  !##########################################################################################################################
  ! REAL(REAL32) 2D ARRAY WRITE GIVEN A FILE NAME - SINGLE PRECISION ARRAY WRITE
  !
  SUBROUTINE FILE_WRITE_ARRAY_2D_REL32(FNAME, ARR, ERROR, WIDTH, FMT, SEP, TRANSPOSE, HED, SEP_ON_LAST, IU, NO_CLOSE)
  CHARACTER(*),                 INTENT(IN   ):: FNAME
  REAL(REAL32), DIMENSION(:,:), INTENT(IN   ):: ARR
  LOGICAL,                      INTENT(INOUT):: ERROR
  INTEGER,            OPTIONAL, INTENT(IN   ):: WIDTH
  CHARACTER(*),       OPTIONAL, INTENT(IN   ):: FMT
  CHARACTER(*),       OPTIONAL, INTENT(IN   ):: SEP
  LOGICAL,            OPTIONAL, INTENT(IN   ):: TRANSPOSE
  CHARACTER(*),       OPTIONAL, INTENT(IN   ):: HED
  LOGICAL,            OPTIONAL, INTENT(IN   ):: SEP_ON_LAST
  INTEGER,            OPTIONAL, INTENT(INOUT):: IU
  LOGICAL,            OPTIONAL, INTENT(IN   ):: NO_CLOSE
  !
  CHARACTER(8):: WRT, BIN
  LOGICAL:: ISOPEN, CLOSE_IT
  INTEGER:: I, IERR
  !
  ERROR = FALSE
  !
  INQUIRE(FILE=FNAME, NUMBER=I, OPENED=ISOPEN, WRITE=WRT, UNFORMATTED=BIN)
  !
  IF(ISOPEN) THEN
      CLOSE_IT = FALSE
      IF( WRT == "NO" ) ERROR = TRUE
      IF( BIN == "YES") ERROR = TRUE
      IF( PRESENT(IU) ) THEN
          IF( IU /= I ) ERROR = TRUE
      END IF
  ELSE
      I = Z
      IF( PRESENT(IU) ) I = IU
      !
      IF( I /= Z ) THEN
          OPEN(   UNIT=I, FILE=FNAME, ACTION="WRITE",       FORM='FORMATTED', &
               ACCESS='SEQUENTIAL',   STATUS="REPLACE", POSITION='REWIND', IOSTAT=IERR)
      ELSE
          OPEN(NEWUNIT=I, FILE=FNAME, ACTION="WRITE",       FORM='FORMATTED', &
               ACCESS='SEQUENTIAL',   STATUS="REPLACE", POSITION='REWIND', IOSTAT=IERR)
      END IF
      ERROR = IERR /= Z
      CLOSE_IT = .not. ERROR
  END IF
  !
  IF( .not. ERROR) CALL WRITE_ARRAY_2D_REL32(I, ARR, WIDTH, FMT, SEP, TRANSPOSE, HED, SEP_ON_LAST)
  !
  IF( CLOSE_IT .AND. PRESENT(NO_CLOSE)) CLOSE_IT = .not. NO_CLOSE 
  IF( CLOSE_IT ) THEN
                 CLOSE(I, IOSTAT=IERR)
  ELSEIF( PRESENT(IU) ) THEN
                 IF(IU==Z) IU = I
  END IF
  !
  END SUBROUTINE FILE_WRITE_ARRAY_2D_REL32
  !
  !##########################################################################################################################
  ! REAL(REAL64) 2D ARRAY WRITE GIVEN A FILE NAME - DOUBLE PRECISION ARRAY WRITE
  !
  SUBROUTINE FILE_WRITE_ARRAY_2D_REL64(FNAME, ARR, ERROR, WIDTH, FMT, SEP, TRANSPOSE, HED, SEP_ON_LAST, IU, NO_CLOSE)
  CHARACTER(*),                 INTENT(IN   ):: FNAME
  REAL(REAL64), DIMENSION(:,:), INTENT(IN   ):: ARR
  LOGICAL,                      INTENT(INOUT):: ERROR
  INTEGER,            OPTIONAL, INTENT(IN   ):: WIDTH
  CHARACTER(*),       OPTIONAL, INTENT(IN   ):: FMT
  CHARACTER(*),       OPTIONAL, INTENT(IN   ):: SEP
  LOGICAL,            OPTIONAL, INTENT(IN   ):: TRANSPOSE
  CHARACTER(*),       OPTIONAL, INTENT(IN   ):: HED
  LOGICAL,            OPTIONAL, INTENT(IN   ):: SEP_ON_LAST
  INTEGER,            OPTIONAL, INTENT(INOUT):: IU
  LOGICAL,            OPTIONAL, INTENT(IN   ):: NO_CLOSE
  !
  CHARACTER(8):: WRT, BIN
  LOGICAL:: ISOPEN, CLOSE_IT
  INTEGER:: I, IERR
  !
  ERROR = FALSE
  !
  INQUIRE(FILE=FNAME, NUMBER=I, OPENED=ISOPEN, WRITE=WRT, UNFORMATTED=BIN)
  !
  IF(ISOPEN) THEN
      CLOSE_IT = FALSE
      IF( WRT == "NO" ) ERROR = TRUE
      IF( BIN == "YES") ERROR = TRUE
      IF( PRESENT(IU) ) THEN
          IF( IU /= I ) ERROR = TRUE
      END IF
  ELSE
      I = Z
      IF( PRESENT(IU) ) I = IU
      !
      IF( I /= Z ) THEN
          OPEN(   UNIT=I, FILE=FNAME, ACTION="WRITE",       FORM='FORMATTED', &
               ACCESS='SEQUENTIAL',   STATUS="REPLACE", POSITION='REWIND', IOSTAT=IERR)
      ELSE
          OPEN(NEWUNIT=I, FILE=FNAME, ACTION="WRITE",       FORM='FORMATTED', &
               ACCESS='SEQUENTIAL',   STATUS="REPLACE", POSITION='REWIND', IOSTAT=IERR)
      END IF
      ERROR = IERR /= Z
      CLOSE_IT = .not. ERROR
  END IF
  !
  IF( .not. ERROR) CALL WRITE_ARRAY_2D_REL64(I, ARR, WIDTH, FMT, SEP, TRANSPOSE, HED, SEP_ON_LAST)
  !
  IF( CLOSE_IT .AND. PRESENT(NO_CLOSE)) CLOSE_IT = .not. NO_CLOSE 
  IF( CLOSE_IT ) THEN
                 CLOSE(I, IOSTAT=IERR)
  ELSEIF( PRESENT(IU) ) THEN
                 IF(IU==Z) IU = I
  END IF
  !
  END SUBROUTINE FILE_WRITE_ARRAY_2D_REL64
  !
END MODULE