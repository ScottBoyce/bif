MODULE WRITE_ARRAY_INTERFACE!, ONLY: WRITE_ARRAY 
  !
  USE NUM2STR_INTERFACE, ONLY: NUM2STR
  !
  USE, INTRINSIC:: ISO_FORTRAN_ENV, ONLY: REAL32, REAL64, REAL128, INT8, INT16, INT32, INT64, stderr => ERROR_UNIT
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
    MODULE PROCEDURE:: FILE_WRITE_ARRAY_1D_INT08 ! WRITE_ARRAY(FNAME, ARR, [WIDTH, FMT, SEP, TRANSPOSE, HED, SEP_ON_LAST, ADVANCE, APPEND, NO_CLOSE, IU, IOSTAT])
    MODULE PROCEDURE:: FILE_WRITE_ARRAY_1D_INT16 !
    MODULE PROCEDURE:: FILE_WRITE_ARRAY_1D_INT32 !
    MODULE PROCEDURE:: FILE_WRITE_ARRAY_1D_INT64 !
    MODULE PROCEDURE:: FILE_WRITE_ARRAY_1D_REL32 !
    MODULE PROCEDURE:: FILE_WRITE_ARRAY_1D_REL64 !
    !
    MODULE PROCEDURE:: FILE_WRITE_ARRAY_2D_INT08 ! WRITE_ARRAY(FNAME, ARR, [WIDTH, FMT, SEP, TRANSPOSE, HED, SEP_ON_LAST, APPEND, NO_CLOSE, IU, IOSTAT])
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
  SUBROUTINE FILE_WRITE_ARRAY_1D_INT08(FNAME, ARR, WIDTH, FMT, SEP, TRANSPOSE, HED, SEP_ON_LAST, ADVANCE, APPEND, NO_CLOSE, IU, IOSTAT)
  CHARACTER(*),                   INTENT(IN   ):: FNAME
  INTEGER(INT8),    DIMENSION(:), INTENT(IN   ):: ARR
  INTEGER,              OPTIONAL, INTENT(IN   ):: WIDTH
  CHARACTER(*),         OPTIONAL, INTENT(IN   ):: FMT
  CHARACTER(*),         OPTIONAL, INTENT(IN   ):: SEP
  LOGICAL,              OPTIONAL, INTENT(IN   ):: TRANSPOSE
  CHARACTER(*),         OPTIONAL, INTENT(IN   ):: HED
  LOGICAL,              OPTIONAL, INTENT(IN   ):: SEP_ON_LAST
  LOGICAL,              OPTIONAL, INTENT(IN   ):: ADVANCE
  LOGICAL,              OPTIONAL, INTENT(IN   ):: APPEND
  LOGICAL,              OPTIONAL, INTENT(IN   ):: NO_CLOSE
  INTEGER,              OPTIONAL, INTENT(INOUT):: IU
  INTEGER,              OPTIONAL, INTENT(INOUT):: IOSTAT
  !
  CHARACTER(8):: WRT, BIN
  LOGICAL:: ISOPEN, CLOSE_IT
  INTEGER:: I, IERR, ITMP
  !
  INQUIRE(FILE=FNAME, NUMBER=I, OPENED=ISOPEN, WRITE=WRT, UNFORMATTED=BIN)
  !
  IERR = Z
  IF(ISOPEN) THEN
      CLOSE_IT = FALSE
      IF( WRT == "NO" ) IERR = -1
      IF( BIN == "YES") IERR = -2
      IF( PRESENT(IU) ) THEN
          IF( IU /= I ) IERR = -3
      END IF
  ELSE
      I = Z
      IF( PRESENT(IU) ) I = IU
      !
      CALL OPEN_WRITE_FILE(FNAME, I, IERR, APPEND)
      CLOSE_IT = IERR == Z
  END IF
  !
  IF( IERR == Z ) CALL WRITE_ARRAY_1D_INT08(I, ARR, WIDTH, FMT, SEP, TRANSPOSE, HED, SEP_ON_LAST, ADVANCE)
  !
  IF( CLOSE_IT .AND. PRESENT(NO_CLOSE)) CLOSE_IT = .not. NO_CLOSE
  IF( CLOSE_IT ) THEN
                 CLOSE(I, IOSTAT=ITMP)
  ELSEIF( PRESENT(IU) ) THEN
                 IF(IU==Z) IU = I
  END IF
  !
  IF( PRESENT(IOSTAT) ) THEN
              IOSTAT = IERR
  ELSEIF(IERR /= Z) THEN
              CALL RAISE_ERROR(FNAME, IERR, IU)
  END IF
  !
  END SUBROUTINE FILE_WRITE_ARRAY_1D_INT08
  !
  !##########################################################################################################################
  ! INTEGER(INT16) 1D ARRAY WRITE GIVEN A FILE NAME
  !
  SUBROUTINE FILE_WRITE_ARRAY_1D_INT16(FNAME, ARR, WIDTH, FMT, SEP, TRANSPOSE, HED, SEP_ON_LAST, ADVANCE, APPEND, NO_CLOSE, IU, IOSTAT)
  CHARACTER(*),                   INTENT(IN   ):: FNAME
  INTEGER(INT16),   DIMENSION(:), INTENT(IN   ):: ARR
  INTEGER,              OPTIONAL, INTENT(IN   ):: WIDTH
  CHARACTER(*),         OPTIONAL, INTENT(IN   ):: FMT
  CHARACTER(*),         OPTIONAL, INTENT(IN   ):: SEP
  LOGICAL,              OPTIONAL, INTENT(IN   ):: TRANSPOSE
  CHARACTER(*),         OPTIONAL, INTENT(IN   ):: HED
  LOGICAL,              OPTIONAL, INTENT(IN   ):: SEP_ON_LAST
  LOGICAL,              OPTIONAL, INTENT(IN   ):: ADVANCE
  LOGICAL,              OPTIONAL, INTENT(IN   ):: APPEND
  LOGICAL,              OPTIONAL, INTENT(IN   ):: NO_CLOSE
  INTEGER,              OPTIONAL, INTENT(INOUT):: IU
  INTEGER,              OPTIONAL, INTENT(INOUT):: IOSTAT
  !
  CHARACTER(8):: WRT, BIN
  LOGICAL:: ISOPEN, CLOSE_IT
  INTEGER:: I, IERR, ITMP
  !
  INQUIRE(FILE=FNAME, NUMBER=I, OPENED=ISOPEN, WRITE=WRT, UNFORMATTED=BIN)
  !
  IERR = Z
  IF(ISOPEN) THEN
      CLOSE_IT = FALSE
      IF( WRT == "NO" ) IERR = -1
      IF( BIN == "YES") IERR = -2
      IF( PRESENT(IU) ) THEN
          IF( IU /= I ) IERR = -3
      END IF
  ELSE
      I = Z
      IF( PRESENT(IU) ) I = IU
      !
      CALL OPEN_WRITE_FILE(FNAME, I, IERR, APPEND)
      CLOSE_IT = IERR == Z
  END IF
  !
  IF( IERR == Z ) CALL WRITE_ARRAY_1D_INT16(I, ARR, WIDTH, FMT, SEP, TRANSPOSE, HED, SEP_ON_LAST, ADVANCE)
  !
  IF( CLOSE_IT .AND. PRESENT(NO_CLOSE)) CLOSE_IT = .not. NO_CLOSE
  IF( CLOSE_IT ) THEN
                 CLOSE(I, IOSTAT=ITMP)
  ELSEIF( PRESENT(IU) ) THEN
                 IF(IU==Z) IU = I
  END IF
  !
  IF( PRESENT(IOSTAT) ) THEN
              IOSTAT = IERR
  ELSEIF(IERR /= Z) THEN
              CALL RAISE_ERROR(FNAME, IERR, IU)
  END IF
  !
  END SUBROUTINE FILE_WRITE_ARRAY_1D_INT16
  !
  !##########################################################################################################################
  ! INTEGER(INT32) 1D ARRAY WRITE GIVEN A FILE NAME -> most compliers have INTEGER(INT32) <=> INTEGER
  !
  SUBROUTINE FILE_WRITE_ARRAY_1D_INT32(FNAME, ARR, WIDTH, FMT, SEP, TRANSPOSE, HED, SEP_ON_LAST, ADVANCE, APPEND, NO_CLOSE, IU, IOSTAT)
  CHARACTER(*),                   INTENT(IN   ):: FNAME
  INTEGER(INT32),   DIMENSION(:), INTENT(IN   ):: ARR
  INTEGER,              OPTIONAL, INTENT(IN   ):: WIDTH
  CHARACTER(*),         OPTIONAL, INTENT(IN   ):: FMT
  CHARACTER(*),         OPTIONAL, INTENT(IN   ):: SEP
  LOGICAL,              OPTIONAL, INTENT(IN   ):: TRANSPOSE
  CHARACTER(*),         OPTIONAL, INTENT(IN   ):: HED
  LOGICAL,              OPTIONAL, INTENT(IN   ):: SEP_ON_LAST
  LOGICAL,              OPTIONAL, INTENT(IN   ):: ADVANCE
  LOGICAL,              OPTIONAL, INTENT(IN   ):: APPEND
  LOGICAL,              OPTIONAL, INTENT(IN   ):: NO_CLOSE
  INTEGER,              OPTIONAL, INTENT(INOUT):: IU
  INTEGER,              OPTIONAL, INTENT(INOUT):: IOSTAT
  !
  CHARACTER(8):: WRT, BIN
  LOGICAL:: ISOPEN, CLOSE_IT
  INTEGER:: I, IERR, ITMP
  !
  INQUIRE(FILE=FNAME, NUMBER=I, OPENED=ISOPEN, WRITE=WRT, UNFORMATTED=BIN)
  !
  IERR = Z
  IF(ISOPEN) THEN
      CLOSE_IT = FALSE
      IF( WRT == "NO" ) IERR = -1
      IF( BIN == "YES") IERR = -2
      IF( PRESENT(IU) ) THEN
          IF( IU /= I ) IERR = -3
      END IF
  ELSE
      I = Z
      IF( PRESENT(IU) ) I = IU
      !
      CALL OPEN_WRITE_FILE(FNAME, I, IERR, APPEND)
      CLOSE_IT = IERR == Z
  END IF
  !
  IF( IERR == Z ) CALL WRITE_ARRAY_1D_INT32(I, ARR, WIDTH, FMT, SEP, TRANSPOSE, HED, SEP_ON_LAST, ADVANCE)
  !
  IF( CLOSE_IT .AND. PRESENT(NO_CLOSE)) CLOSE_IT = .not. NO_CLOSE
  IF( CLOSE_IT ) THEN
                 CLOSE(I, IOSTAT=ITMP)
  ELSEIF( PRESENT(IU) ) THEN
                 IF(IU==Z) IU = I
  END IF
  !
  IF( PRESENT(IOSTAT) ) THEN
              IOSTAT = IERR
  ELSEIF(IERR /= Z) THEN
              CALL RAISE_ERROR(FNAME, IERR, IU)
  END IF
  !
  END SUBROUTINE FILE_WRITE_ARRAY_1D_INT32
  !
  !##########################################################################################################################
  ! INTEGER(INT64) 1D ARRAY WRITE GIVEN A FILE NAME
  !
  SUBROUTINE FILE_WRITE_ARRAY_1D_INT64(FNAME, ARR, WIDTH, FMT, SEP, TRANSPOSE, HED, SEP_ON_LAST, ADVANCE, APPEND, NO_CLOSE, IU, IOSTAT)
  CHARACTER(*),                   INTENT(IN   ):: FNAME
  INTEGER(INT64),   DIMENSION(:), INTENT(IN   ):: ARR
  INTEGER,              OPTIONAL, INTENT(IN   ):: WIDTH
  CHARACTER(*),         OPTIONAL, INTENT(IN   ):: FMT
  CHARACTER(*),         OPTIONAL, INTENT(IN   ):: SEP
  LOGICAL,              OPTIONAL, INTENT(IN   ):: TRANSPOSE
  CHARACTER(*),         OPTIONAL, INTENT(IN   ):: HED
  LOGICAL,              OPTIONAL, INTENT(IN   ):: SEP_ON_LAST
  LOGICAL,              OPTIONAL, INTENT(IN   ):: ADVANCE
  LOGICAL,              OPTIONAL, INTENT(IN   ):: APPEND
  LOGICAL,              OPTIONAL, INTENT(IN   ):: NO_CLOSE
  INTEGER,              OPTIONAL, INTENT(INOUT):: IU
  INTEGER,              OPTIONAL, INTENT(INOUT):: IOSTAT
  !
  CHARACTER(8):: WRT, BIN
  LOGICAL:: ISOPEN, CLOSE_IT
  INTEGER:: I, IERR, ITMP
  !
  INQUIRE(FILE=FNAME, NUMBER=I, OPENED=ISOPEN, WRITE=WRT, UNFORMATTED=BIN)
  !
  IERR = Z
  IF(ISOPEN) THEN
      CLOSE_IT = FALSE
      IF( WRT == "NO" ) IERR = -1
      IF( BIN == "YES") IERR = -2
      IF( PRESENT(IU) ) THEN
          IF( IU /= I ) IERR = -3
      END IF
  ELSE
      I = Z
      IF( PRESENT(IU) ) I = IU
      !
      CALL OPEN_WRITE_FILE(FNAME, I, IERR, APPEND)
      CLOSE_IT = IERR == Z
  END IF
  !
  IF( IERR == Z ) CALL WRITE_ARRAY_1D_INT64(I, ARR, WIDTH, FMT, SEP, TRANSPOSE, HED, SEP_ON_LAST, ADVANCE)
  !
  IF( CLOSE_IT .AND. PRESENT(NO_CLOSE)) CLOSE_IT = .not. NO_CLOSE
  IF( CLOSE_IT ) THEN
                 CLOSE(I, IOSTAT=ITMP)
  ELSEIF( PRESENT(IU) ) THEN
                 IF(IU==Z) IU = I
  END IF
  !
  IF( PRESENT(IOSTAT) ) THEN
              IOSTAT = IERR
  ELSEIF(IERR /= Z) THEN
              CALL RAISE_ERROR(FNAME, IERR, IU)
  END IF
  !
  END SUBROUTINE FILE_WRITE_ARRAY_1D_INT64
  !
  !##########################################################################################################################
  ! REAL(REAL32) 1D ARRAY WRITE GIVEN A FILE NAME - SINGLE PRECISION ARRAY WRITE
  !
  SUBROUTINE FILE_WRITE_ARRAY_1D_REL32(FNAME, ARR, WIDTH, FMT, SEP, TRANSPOSE, HED, SEP_ON_LAST, ADVANCE, APPEND, NO_CLOSE, IU, IOSTAT)
  CHARACTER(*),                   INTENT(IN   ):: FNAME
  REAL(REAL32),     DIMENSION(:), INTENT(IN   ):: ARR
  INTEGER,              OPTIONAL, INTENT(IN   ):: WIDTH
  CHARACTER(*),         OPTIONAL, INTENT(IN   ):: FMT
  CHARACTER(*),         OPTIONAL, INTENT(IN   ):: SEP
  LOGICAL,              OPTIONAL, INTENT(IN   ):: TRANSPOSE
  CHARACTER(*),         OPTIONAL, INTENT(IN   ):: HED
  LOGICAL,              OPTIONAL, INTENT(IN   ):: SEP_ON_LAST
  LOGICAL,              OPTIONAL, INTENT(IN   ):: ADVANCE
  LOGICAL,              OPTIONAL, INTENT(IN   ):: APPEND
  LOGICAL,              OPTIONAL, INTENT(IN   ):: NO_CLOSE
  INTEGER,              OPTIONAL, INTENT(INOUT):: IU
  INTEGER,              OPTIONAL, INTENT(INOUT):: IOSTAT
  !
  CHARACTER(8):: WRT, BIN
  LOGICAL:: ISOPEN, CLOSE_IT
  INTEGER:: I, IERR, ITMP
  !
  INQUIRE(FILE=FNAME, NUMBER=I, OPENED=ISOPEN, WRITE=WRT, UNFORMATTED=BIN)
  !
  IERR = Z
  IF(ISOPEN) THEN
      CLOSE_IT = FALSE
      IF( WRT == "NO" ) IERR = -1
      IF( BIN == "YES") IERR = -2
      IF( PRESENT(IU) ) THEN
          IF( IU /= I ) IERR = -3
      END IF
  ELSE
      I = Z
      IF( PRESENT(IU) ) I = IU
      !
      CALL OPEN_WRITE_FILE(FNAME, I, IERR, APPEND)
      CLOSE_IT = IERR == Z
  END IF
  !
  IF( IERR == Z ) CALL WRITE_ARRAY_1D_REL32(I, ARR, WIDTH, FMT, SEP, TRANSPOSE, HED, SEP_ON_LAST, ADVANCE)
  !
  IF( CLOSE_IT .AND. PRESENT(NO_CLOSE)) CLOSE_IT = .not. NO_CLOSE
  IF( CLOSE_IT ) THEN
                 CLOSE(I, IOSTAT=ITMP)
  ELSEIF( PRESENT(IU) ) THEN
                 IF(IU==Z) IU = I
  END IF
  !
  IF( PRESENT(IOSTAT) ) THEN
              IOSTAT = IERR
  ELSEIF(IERR /= Z) THEN
              CALL RAISE_ERROR(FNAME, IERR, IU)
  END IF
  !
  END SUBROUTINE FILE_WRITE_ARRAY_1D_REL32
  !
  !##########################################################################################################################
  ! REAL(REAL64) 1D ARRAY WRITE GIVEN A FILE NAME - DOUBLE PRECISION ARRAY WRITE
  !
  SUBROUTINE FILE_WRITE_ARRAY_1D_REL64(FNAME, ARR, WIDTH, FMT, SEP, TRANSPOSE, HED, SEP_ON_LAST, ADVANCE, APPEND, NO_CLOSE, IU, IOSTAT)
  CHARACTER(*),                   INTENT(IN   ):: FNAME
  REAL(REAL64),     DIMENSION(:), INTENT(IN   ):: ARR
  INTEGER,              OPTIONAL, INTENT(IN   ):: WIDTH
  CHARACTER(*),         OPTIONAL, INTENT(IN   ):: FMT
  CHARACTER(*),         OPTIONAL, INTENT(IN   ):: SEP
  LOGICAL,              OPTIONAL, INTENT(IN   ):: TRANSPOSE
  CHARACTER(*),         OPTIONAL, INTENT(IN   ):: HED
  LOGICAL,              OPTIONAL, INTENT(IN   ):: SEP_ON_LAST
  LOGICAL,              OPTIONAL, INTENT(IN   ):: ADVANCE
  LOGICAL,              OPTIONAL, INTENT(IN   ):: APPEND
  LOGICAL,              OPTIONAL, INTENT(IN   ):: NO_CLOSE
  INTEGER,              OPTIONAL, INTENT(INOUT):: IU
  INTEGER,              OPTIONAL, INTENT(INOUT):: IOSTAT
  !
  CHARACTER(8):: WRT, BIN
  LOGICAL:: ISOPEN, CLOSE_IT
  INTEGER:: I, IERR, ITMP
  !
  INQUIRE(FILE=FNAME, NUMBER=I, OPENED=ISOPEN, WRITE=WRT, UNFORMATTED=BIN)
  !
  IERR = Z
  IF(ISOPEN) THEN
      CLOSE_IT = FALSE
      IF( WRT == "NO" ) IERR = -1
      IF( BIN == "YES") IERR = -2
      IF( PRESENT(IU) ) THEN
          IF( IU /= I ) IERR = -3
      END IF
  ELSE
      I = Z
      IF( PRESENT(IU) ) I = IU
      !
      CALL OPEN_WRITE_FILE(FNAME, I, IERR, APPEND)
      CLOSE_IT = IERR == Z
  END IF
  !
  IF( IERR == Z ) CALL WRITE_ARRAY_1D_REL64(I, ARR, WIDTH, FMT, SEP, TRANSPOSE, HED, SEP_ON_LAST, ADVANCE)
  !
  IF( CLOSE_IT .AND. PRESENT(NO_CLOSE)) CLOSE_IT = .not. NO_CLOSE
  IF( CLOSE_IT ) THEN
                 CLOSE(I, IOSTAT=ITMP)
  ELSEIF( PRESENT(IU) ) THEN
                 IF(IU==Z) IU = I
  END IF
  !
  IF( PRESENT(IOSTAT) ) THEN
              IOSTAT = IERR
  ELSEIF(IERR /= Z) THEN
              CALL RAISE_ERROR(FNAME, IERR, IU)
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
  SUBROUTINE FILE_WRITE_ARRAY_2D_INT08(FNAME, ARR, WIDTH, FMT, SEP, TRANSPOSE, HED, SEP_ON_LAST, APPEND, NO_CLOSE, IU, IOSTAT)
  CHARACTER(*),                   INTENT(IN   ):: FNAME
  INTEGER(INT8),  DIMENSION(:,:), INTENT(IN   ):: ARR
  INTEGER,              OPTIONAL, INTENT(IN   ):: WIDTH
  CHARACTER(*),         OPTIONAL, INTENT(IN   ):: FMT
  CHARACTER(*),         OPTIONAL, INTENT(IN   ):: SEP
  LOGICAL,              OPTIONAL, INTENT(IN   ):: TRANSPOSE
  CHARACTER(*),         OPTIONAL, INTENT(IN   ):: HED
  LOGICAL,              OPTIONAL, INTENT(IN   ):: SEP_ON_LAST
  LOGICAL,              OPTIONAL, INTENT(IN   ):: APPEND
  LOGICAL,              OPTIONAL, INTENT(IN   ):: NO_CLOSE
  INTEGER,              OPTIONAL, INTENT(INOUT):: IU
  INTEGER,              OPTIONAL, INTENT(INOUT):: IOSTAT
  !
  CHARACTER(8):: WRT, BIN
  LOGICAL:: ISOPEN, CLOSE_IT
  INTEGER:: I, IERR, ITMP
  !
  INQUIRE(FILE=FNAME, NUMBER=I, OPENED=ISOPEN, WRITE=WRT, UNFORMATTED=BIN)
  !
  IERR = Z
  IF(ISOPEN) THEN
      CLOSE_IT = FALSE
      IF( WRT == "NO" ) IERR = -1
      IF( BIN == "YES") IERR = -2
      IF( PRESENT(IU) ) THEN
          IF( IU /= I ) IERR = -3
      END IF
  ELSE
      I = Z
      IF( PRESENT(IU) ) I = IU
      !
      CALL OPEN_WRITE_FILE(FNAME, I, IERR, APPEND)
      CLOSE_IT = IERR == Z
  END IF
  !
  IF( IERR == Z ) CALL WRITE_ARRAY_2D_INT08(I, ARR, WIDTH, FMT, SEP, TRANSPOSE, HED, SEP_ON_LAST)
  !
  IF( CLOSE_IT .AND. PRESENT(NO_CLOSE)) CLOSE_IT = .not. NO_CLOSE
  IF( CLOSE_IT ) THEN
                 CLOSE(I, IOSTAT=ITMP)
  ELSEIF( PRESENT(IU) ) THEN
                 IF(IU==Z) IU = I
  END IF
  !
  IF( PRESENT(IOSTAT) ) THEN
              IOSTAT = IERR
  ELSEIF(IERR /= Z) THEN
              CALL RAISE_ERROR(FNAME, IERR, IU)
  END IF
  !
  END SUBROUTINE FILE_WRITE_ARRAY_2D_INT08
  !
  !##########################################################################################################################
  ! INTEGER(INT16) 2D ARRAY WRITE GIVEN A FILE NAME
  !
  SUBROUTINE FILE_WRITE_ARRAY_2D_INT16(FNAME, ARR, WIDTH, FMT, SEP, TRANSPOSE, HED, SEP_ON_LAST, APPEND, NO_CLOSE, IU, IOSTAT)
  CHARACTER(*),                   INTENT(IN   ):: FNAME
  INTEGER(INT16), DIMENSION(:,:), INTENT(IN   ):: ARR
  INTEGER,              OPTIONAL, INTENT(IN   ):: WIDTH
  CHARACTER(*),         OPTIONAL, INTENT(IN   ):: FMT
  CHARACTER(*),         OPTIONAL, INTENT(IN   ):: SEP
  LOGICAL,              OPTIONAL, INTENT(IN   ):: TRANSPOSE
  CHARACTER(*),         OPTIONAL, INTENT(IN   ):: HED
  LOGICAL,              OPTIONAL, INTENT(IN   ):: SEP_ON_LAST
  LOGICAL,              OPTIONAL, INTENT(IN   ):: APPEND
  LOGICAL,              OPTIONAL, INTENT(IN   ):: NO_CLOSE
  INTEGER,              OPTIONAL, INTENT(INOUT):: IU
  INTEGER,              OPTIONAL, INTENT(INOUT):: IOSTAT
  !
  CHARACTER(8):: WRT, BIN
  LOGICAL:: ISOPEN, CLOSE_IT
  INTEGER:: I, IERR, ITMP
  !
  INQUIRE(FILE=FNAME, NUMBER=I, OPENED=ISOPEN, WRITE=WRT, UNFORMATTED=BIN)
  !
  IERR = Z
  IF(ISOPEN) THEN
      CLOSE_IT = FALSE
      IF( WRT == "NO" ) IERR = -1
      IF( BIN == "YES") IERR = -2
      IF( PRESENT(IU) ) THEN
          IF( IU /= I ) IERR = -3
      END IF
  ELSE
      I = Z
      IF( PRESENT(IU) ) I = IU
      !
      CALL OPEN_WRITE_FILE(FNAME, I, IERR, APPEND)
      CLOSE_IT = IERR == Z
  END IF
  !
  IF( IERR == Z ) CALL WRITE_ARRAY_2D_INT16(I, ARR, WIDTH, FMT, SEP, TRANSPOSE, HED, SEP_ON_LAST)
  !
  IF( CLOSE_IT .AND. PRESENT(NO_CLOSE)) CLOSE_IT = .not. NO_CLOSE
  IF( CLOSE_IT ) THEN
                 CLOSE(I, IOSTAT=ITMP)
  ELSEIF( PRESENT(IU) ) THEN
                 IF(IU==Z) IU = I
  END IF
  !
  IF( PRESENT(IOSTAT) ) THEN
              IOSTAT = IERR
  ELSEIF(IERR /= Z) THEN
              CALL RAISE_ERROR(FNAME, IERR, IU)
  END IF
  !
  END SUBROUTINE FILE_WRITE_ARRAY_2D_INT16
  !
  !##########################################################################################################################
  ! INTEGER(INT32) 2D ARRAY WRITE GIVEN A FILE NAME -> most compliers have INTEGER(INT32) <=> INTEGER
  !
  SUBROUTINE FILE_WRITE_ARRAY_2D_INT32(FNAME, ARR, WIDTH, FMT, SEP, TRANSPOSE, HED, SEP_ON_LAST, APPEND, NO_CLOSE, IU, IOSTAT)
  CHARACTER(*),                   INTENT(IN   ):: FNAME
  INTEGER(INT32), DIMENSION(:,:), INTENT(IN   ):: ARR
  INTEGER,              OPTIONAL, INTENT(IN   ):: WIDTH
  CHARACTER(*),         OPTIONAL, INTENT(IN   ):: FMT
  CHARACTER(*),         OPTIONAL, INTENT(IN   ):: SEP
  LOGICAL,              OPTIONAL, INTENT(IN   ):: TRANSPOSE
  CHARACTER(*),         OPTIONAL, INTENT(IN   ):: HED
  LOGICAL,              OPTIONAL, INTENT(IN   ):: SEP_ON_LAST
  LOGICAL,              OPTIONAL, INTENT(IN   ):: APPEND
  LOGICAL,              OPTIONAL, INTENT(IN   ):: NO_CLOSE
  INTEGER,              OPTIONAL, INTENT(INOUT):: IU
  INTEGER,              OPTIONAL, INTENT(INOUT):: IOSTAT
  !
  CHARACTER(8):: WRT, BIN
  LOGICAL:: ISOPEN, CLOSE_IT
  INTEGER:: I, IERR, ITMP
  !
  INQUIRE(FILE=FNAME, NUMBER=I, OPENED=ISOPEN, WRITE=WRT, UNFORMATTED=BIN)
  !
  IERR = Z
  IF(ISOPEN) THEN
      CLOSE_IT = FALSE
      IF( WRT == "NO" ) IERR = -1
      IF( BIN == "YES") IERR = -2
      IF( PRESENT(IU) ) THEN
          IF( IU /= I ) IERR = -3
      END IF
  ELSE
      I = Z
      IF( PRESENT(IU) ) I = IU
      !
      CALL OPEN_WRITE_FILE(FNAME, I, IERR, APPEND)
      CLOSE_IT = IERR == Z
  END IF
  !
  IF( IERR == Z ) CALL WRITE_ARRAY_2D_INT32(I, ARR, WIDTH, FMT, SEP, TRANSPOSE, HED, SEP_ON_LAST)
  !
  IF( CLOSE_IT .AND. PRESENT(NO_CLOSE)) CLOSE_IT = .not. NO_CLOSE
  IF( CLOSE_IT ) THEN
                 CLOSE(I, IOSTAT=ITMP)
  ELSEIF( PRESENT(IU) ) THEN
                 IF(IU==Z) IU = I
  END IF
  !
  IF( PRESENT(IOSTAT) ) THEN
              IOSTAT = IERR
  ELSEIF(IERR /= Z) THEN
              CALL RAISE_ERROR(FNAME, IERR, IU)
  END IF
  !
  END SUBROUTINE FILE_WRITE_ARRAY_2D_INT32
  !
  !##########################################################################################################################
  ! INTEGER(INT64) 2D ARRAY WRITE GIVEN A FILE NAME
  !
  SUBROUTINE FILE_WRITE_ARRAY_2D_INT64(FNAME, ARR, WIDTH, FMT, SEP, TRANSPOSE, HED, SEP_ON_LAST, APPEND, NO_CLOSE, IU, IOSTAT)
  CHARACTER(*),                   INTENT(IN   ):: FNAME
  INTEGER(INT64), DIMENSION(:,:), INTENT(IN   ):: ARR
  INTEGER,              OPTIONAL, INTENT(IN   ):: WIDTH
  CHARACTER(*),         OPTIONAL, INTENT(IN   ):: FMT
  CHARACTER(*),         OPTIONAL, INTENT(IN   ):: SEP
  LOGICAL,              OPTIONAL, INTENT(IN   ):: TRANSPOSE
  CHARACTER(*),         OPTIONAL, INTENT(IN   ):: HED
  LOGICAL,              OPTIONAL, INTENT(IN   ):: SEP_ON_LAST
  LOGICAL,              OPTIONAL, INTENT(IN   ):: APPEND
  LOGICAL,              OPTIONAL, INTENT(IN   ):: NO_CLOSE
  INTEGER,              OPTIONAL, INTENT(INOUT):: IU
  INTEGER,              OPTIONAL, INTENT(INOUT):: IOSTAT
  !
  CHARACTER(8):: WRT, BIN
  LOGICAL:: ISOPEN, CLOSE_IT
  INTEGER:: I, IERR, ITMP
  !
  INQUIRE(FILE=FNAME, NUMBER=I, OPENED=ISOPEN, WRITE=WRT, UNFORMATTED=BIN)
  !
  IERR = Z
  IF(ISOPEN) THEN
      CLOSE_IT = FALSE
      IF( WRT == "NO" ) IERR = -1
      IF( BIN == "YES") IERR = -2
      IF( PRESENT(IU) ) THEN
          IF( IU /= I ) IERR = -3
      END IF
  ELSE
      I = Z
      IF( PRESENT(IU) ) I = IU
      !
      CALL OPEN_WRITE_FILE(FNAME, I, IERR, APPEND)
      CLOSE_IT = IERR == Z
  END IF
  !
  IF( IERR == Z ) CALL WRITE_ARRAY_2D_INT64(I, ARR, WIDTH, FMT, SEP, TRANSPOSE, HED, SEP_ON_LAST)
  !
  IF( CLOSE_IT .AND. PRESENT(NO_CLOSE)) CLOSE_IT = .not. NO_CLOSE
  IF( CLOSE_IT ) THEN
                 CLOSE(I, IOSTAT=ITMP)
  ELSEIF( PRESENT(IU) ) THEN
                 IF(IU==Z) IU = I
  END IF
  !
  IF( PRESENT(IOSTAT) ) THEN
              IOSTAT = IERR
  ELSEIF(IERR /= Z) THEN
              CALL RAISE_ERROR(FNAME, IERR, IU)
  END IF
  !
  END SUBROUTINE FILE_WRITE_ARRAY_2D_INT64
  !
  !##########################################################################################################################
  ! REAL(REAL32) 2D ARRAY WRITE GIVEN A FILE NAME - SINGLE PRECISION ARRAY WRITE
  !
  SUBROUTINE FILE_WRITE_ARRAY_2D_REL32(FNAME, ARR, WIDTH, FMT, SEP, TRANSPOSE, HED, SEP_ON_LAST, APPEND, NO_CLOSE, IU, IOSTAT)
  CHARACTER(*),                   INTENT(IN   ):: FNAME
  REAL(REAL32),   DIMENSION(:,:), INTENT(IN   ):: ARR
  INTEGER,              OPTIONAL, INTENT(IN   ):: WIDTH
  CHARACTER(*),         OPTIONAL, INTENT(IN   ):: FMT
  CHARACTER(*),         OPTIONAL, INTENT(IN   ):: SEP
  LOGICAL,              OPTIONAL, INTENT(IN   ):: TRANSPOSE
  CHARACTER(*),         OPTIONAL, INTENT(IN   ):: HED
  LOGICAL,              OPTIONAL, INTENT(IN   ):: SEP_ON_LAST
  LOGICAL,              OPTIONAL, INTENT(IN   ):: APPEND
  LOGICAL,              OPTIONAL, INTENT(IN   ):: NO_CLOSE
  INTEGER,              OPTIONAL, INTENT(INOUT):: IU
  INTEGER,              OPTIONAL, INTENT(INOUT):: IOSTAT
  !
  CHARACTER(8):: WRT, BIN
  LOGICAL:: ISOPEN, CLOSE_IT
  INTEGER:: I, IERR, ITMP
  !
  INQUIRE(FILE=FNAME, NUMBER=I, OPENED=ISOPEN, WRITE=WRT, UNFORMATTED=BIN)
  !
  IERR = Z
  IF(ISOPEN) THEN
      CLOSE_IT = FALSE
      IF( WRT == "NO" ) IERR = -1
      IF( BIN == "YES") IERR = -2
      IF( PRESENT(IU) ) THEN
          IF( IU /= I ) IERR = -3
      END IF
  ELSE
      I = Z
      IF( PRESENT(IU) ) I = IU
      !
      CALL OPEN_WRITE_FILE(FNAME, I, IERR, APPEND)
      CLOSE_IT = IERR == Z
  END IF
  !
  IF( IERR == Z ) CALL WRITE_ARRAY_2D_REL32(I, ARR, WIDTH, FMT, SEP, TRANSPOSE, HED, SEP_ON_LAST)
  !
  IF( CLOSE_IT .AND. PRESENT(NO_CLOSE)) CLOSE_IT = .not. NO_CLOSE
  IF( CLOSE_IT ) THEN
                 CLOSE(I, IOSTAT=ITMP)
  ELSEIF( PRESENT(IU) ) THEN
                 IF(IU==Z) IU = I
  END IF
  !
  IF( PRESENT(IOSTAT) ) THEN
              IOSTAT = IERR
  ELSEIF(IERR /= Z) THEN
              CALL RAISE_ERROR(FNAME, IERR, IU)
  END IF
  !
  END SUBROUTINE FILE_WRITE_ARRAY_2D_REL32
  !
  !##########################################################################################################################
  ! REAL(REAL64) 2D ARRAY WRITE GIVEN A FILE NAME - DOUBLE PRECISION ARRAY WRITE
  !
  SUBROUTINE FILE_WRITE_ARRAY_2D_REL64(FNAME, ARR, WIDTH, FMT, SEP, TRANSPOSE, HED, SEP_ON_LAST, APPEND, NO_CLOSE, IU, IOSTAT)
  CHARACTER(*),                   INTENT(IN   ):: FNAME
  REAL(REAL64),   DIMENSION(:,:), INTENT(IN   ):: ARR
  INTEGER,              OPTIONAL, INTENT(IN   ):: WIDTH
  CHARACTER(*),         OPTIONAL, INTENT(IN   ):: FMT
  CHARACTER(*),         OPTIONAL, INTENT(IN   ):: SEP
  LOGICAL,              OPTIONAL, INTENT(IN   ):: TRANSPOSE
  CHARACTER(*),         OPTIONAL, INTENT(IN   ):: HED
  LOGICAL,              OPTIONAL, INTENT(IN   ):: SEP_ON_LAST
  LOGICAL,              OPTIONAL, INTENT(IN   ):: APPEND
  LOGICAL,              OPTIONAL, INTENT(IN   ):: NO_CLOSE
  INTEGER,              OPTIONAL, INTENT(INOUT):: IU
  INTEGER,              OPTIONAL, INTENT(INOUT):: IOSTAT
  !
  CHARACTER(8):: WRT, BIN
  LOGICAL:: ISOPEN, CLOSE_IT
  INTEGER:: I, IERR, ITMP
  !
  INQUIRE(FILE=FNAME, NUMBER=I, OPENED=ISOPEN, WRITE=WRT, UNFORMATTED=BIN)
  !
  IERR = Z
  IF(ISOPEN) THEN
      CLOSE_IT = FALSE
      IF( WRT == "NO" ) IERR = -1
      IF( BIN == "YES") IERR = -2
      IF( PRESENT(IU) ) THEN
          IF( IU /= I ) IERR = -3
      END IF
  ELSE
      I = Z
      IF( PRESENT(IU) ) I = IU
      !
      CALL OPEN_WRITE_FILE(FNAME, I, IERR, APPEND)
      CLOSE_IT = IERR == Z
  END IF
  !
  IF( IERR == Z ) CALL WRITE_ARRAY_2D_REL64(I, ARR, WIDTH, FMT, SEP, TRANSPOSE, HED, SEP_ON_LAST)
  !
  IF( CLOSE_IT .AND. PRESENT(NO_CLOSE)) CLOSE_IT = .not. NO_CLOSE
  IF( CLOSE_IT ) THEN
                 CLOSE(I, IOSTAT=ITMP)
  ELSEIF( PRESENT(IU) ) THEN
                 IF(IU==Z) IU = I
  END IF
  !
  IF( PRESENT(IOSTAT) ) THEN
              IOSTAT = IERR
  ELSEIF(IERR /= Z) THEN
              CALL RAISE_ERROR(FNAME, IERR, IU)
  END IF
  !
  END SUBROUTINE FILE_WRITE_ARRAY_2D_REL64
  !
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !
  !  Simple Routine to Open a File for Writing
  !
  !##########################################################################################################################
  !
  SUBROUTINE OPEN_WRITE_FILE(FNAME, IU, IERR, APPEND)
  CHARACTER(*),      INTENT(IN   ):: FNAME
  INTEGER,           INTENT(INOUT):: IU
  INTEGER,           INTENT(INOUT):: IERR
  LOGICAL, OPTIONAL, INTENT(IN   ):: APPEND
  CHARACTER(:), ALLOCATABLE:: FIXED
  CHARACTER(8):: POS, STAT
  INTEGER:: I
  !
  STAT= 'REPLACE'
  POS = 'REWIND'
  IF(PRESENT(APPEND))THEN
          IF(APPEND) THEN
                     STAT= 'UNKNOWN'
                     POS = 'APPEND'
          END IF
  END IF
  !
  I = IU
  IF( I /= Z ) THEN
      OPEN(   UNIT=I, FILE=FNAME, ACTION="WRITE", FORM='FORMATTED', &
           ACCESS='SEQUENTIAL',   STATUS=STAT,    POSITION=POS,     IOSTAT=IERR)
  ELSE
      OPEN(NEWUNIT=I, FILE=FNAME, ACTION="WRITE", FORM='FORMATTED', &
           ACCESS='SEQUENTIAL',   STATUS=STAT,    POSITION=POS,     IOSTAT=IERR)
  END IF
  !
  IF(IERR /= Z) THEN
     CALL MAKE_DIRECTORY(FNAME, TRUE, NEWPATH=FIXED)
     !
     I = IU
     IF( I /= Z ) THEN
         OPEN(   UNIT=I, FILE=FNAME, ACTION="WRITE", FORM='FORMATTED', &
              ACCESS='SEQUENTIAL',   STATUS=STAT,    POSITION=POS,     IOSTAT=IERR)
     ELSE
         OPEN(NEWUNIT=I, FILE=FNAME, ACTION="WRITE", FORM='FORMATTED', &
              ACCESS='SEQUENTIAL',   STATUS=STAT,    POSITION=POS,     IOSTAT=IERR)
     END IF
  END IF
  !
  IF(IERR /= Z) THEN
     I = IU
     IF( I /= Z ) THEN
         OPEN(   UNIT=I, FILE=FIXED, ACTION="WRITE", FORM='FORMATTED', &
              ACCESS='SEQUENTIAL',   STATUS=STAT,    POSITION=POS,     IOSTAT=IERR)
     ELSE
         OPEN(NEWUNIT=I, FILE=FIXED, ACTION="WRITE", FORM='FORMATTED', &
              ACCESS='SEQUENTIAL',   STATUS=STAT,    POSITION=POS,     IOSTAT=IERR)
     END IF
  END IF
  !
  IF(IERR == Z) IU = I
  !
  END SUBROUTINE!
  !
  !##########################################################################################################################
  !  Simple Error Routine
  !
  SUBROUTINE RAISE_ERROR(FNAME, IERR, IU)
  CHARACTER(*),      INTENT(IN):: FNAME
  INTEGER,           INTENT(IN):: IERR
  INTEGER, OPTIONAL, INTENT(IN):: IU
  !
  WRITE(stderr,'(/, /, A, /, A, /, /, 3x, 3A, /, /, A, /)')          &
                      "WRITE_ARRAY() ERROR",                         &
                      "occured while attempting to open the file:",  &
                      '"',TRIM(FNAME),'"',                           &
                      "For writing an array."
  IF(IERR < Z) THEN
      WRITE(stderr,'(A)') "The file is already open"
      SELECT CASE(IERR)
      CASE(-1)
          WRITE(stderr,'(A)') "and was set to READ only."
      CASE(-2)
          WRITE(stderr,'(A)') "and was set to UNFORMATTED and not text output."
      CASE(-3)
          WRITE(stderr,'(A)') "but did not match the unit number requested to open the file on."
          WRITE(stderr,'(2A)')"The requested number is: ", NUM2STR(IU)
      END SELECT
  ELSE
      WRITE(stderr,'(2A)') "The error condition (IOSTAT) is: ", NUM2STR(IERR)
  END IF
  !
  ERROR STOP IERR
  !
  END SUBROUTINE
  !
  !
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !
  !  Support Routines Obtained from other parts of BiF
  !
  !##########################################################################################################################
  ! Build Missing Directory Pathes
  !
  SUBROUTINE MAKE_DIRECTORY(PATH, HAS_FILE, DIRPATH, FILENAME, NEWPATH)
    CHARACTER(*),                        INTENT(IN ):: PATH
    LOGICAL,                             INTENT(IN ):: HAS_FILE
    CHARACTER(:), ALLOCATABLE, OPTIONAL, INTENT(OUT):: DIRPATH, FILENAME, NEWPATH
    !
    CHARACTER(:), ALLOCATABLE:: BASE, FILE, CMD
    !
    IF(PATH == "") RETURN
    !
    IF(HAS_FILE) THEN
        CALL PARSE_PATH(PATH, BASE, FILE)
    ELSE
        CALL PARSE_PATH(PATH, BASE)
    END IF
    !
    IF(IS_WINDOWS()) THEN
        CMD = 'mkdir "'//BASE//'" >nul 2>nul'
    ELSE
        CMD = 'mkdir -p "'//BASE//'" >/dev/null 2>&1'
    END IF
    !
    CALL EXECUTE_COMMAND_LINE( CMD )
    !
    IF(PRESENT(DIRPATH)) DIRPATH = BASE
    !
    IF(PRESENT(FILENAME)) THEN
            IF(HAS_FILE) THEN
                FILENAME = FILE
            ELSE
                FILENAME = ""
            END IF
    END IF
    !
    IF(PRESENT(NEWPATH)) THEN
            IF(HAS_FILE) THEN
                IF(IS_WINDOWS()) THEN
                    NEWPATH = BASE // "\" // FILE
                ELSE
                    NEWPATH = BASE // "/" // FILE
                END IF
            ELSE
                NEWPATH = BASE
            END IF
    END IF
    !
  END SUBROUTINE
  !
  !#############################################################################################################################################################
  !
  SUBROUTINE PARSE_PATH(PATH, BASE, FILE)
    CHARACTER(*),                         INTENT(IN):: PATH
    CHARACTER(:), ALLOCATABLE,           INTENT(OUT):: BASE
    CHARACTER(:), ALLOCATABLE, OPTIONAL, INTENT(OUT):: FILE
    !
    INTEGER:: I, J, K, LENLINE
    LOGICAL:: HAS_TRAILING_SLASH
    CHARACTER:: SLASH, BSLASH, TAB, SL
    CHARACTER(:), ALLOCATABLE:: tmp
    !
    LENLINE = LEN_TRIM(PATH)
    !
    IF(LENLINE==Z) THEN              ! Empty Line
        BASE = TRIM(PATH)
        IF(PRESENT(FILE)) FILE = ""
        RETURN
    END IF
    !
    IF(SCAN(PATH, "/\") == Z)  THEN  ! No directory slashes
        IF(PRESENT(FILE)) THEN
           BASE = ""
           FILE = TRIM(PATH)
        ELSE
           BASE = TRIM(PATH)
        END IF
        RETURN
    END IF
    !
    TAB    = ACHAR(9)
     SLASH = "/"
    BSLASH = "\"
    HAS_TRAILING_SLASH = PATH(LENLINE:LENLINE) == SLASH .or. PATH(LENLINE:LENLINE) == BSLASH
    !
    I = LENLINE
    DO K = 1, LENLINE
        IF(PATH(K:K) /= " " .AND. PATH(K:K) /= TAB) THEN
            I = K
            EXIT
        END IF
    END DO
    !
    IF( IS_WINDOWS() ) THEN
                         SL = BSLASH
    ELSE
                         SL =  SLASH
    END IF
    !
    IF( PATH(I:I) == SLASH .OR. PATH(I:I) == BSLASH ) THEN
        I = I + 1
        tmp = SL
    ELSE
        tmp = ""
    END IF
    !
    J = I
    DO K=I, LENLINE
        IF( PATH(K:K) == SLASH .or. PATH(K:K) == BSLASH ) THEN
            tmp = tmp // PATH(J:K-1) // SL
            J = K + 1
        END IF
    END DO
    !
    K = LENLINE
    I = LEN(tmp) - 1             ! tmp always has a trailing slash
    IF(HAS_TRAILING_SLASH) THEN
        BASE = tmp(1:I)
        IF(PRESENT(FILE)) FILE = ""
    ELSEIF(PRESENT(FILE)) THEN
           BASE = tmp(1:I)
           FILE = PATH(J:K)
    ELSE
           BASE = tmp(1:I) // PATH(J:K)
    END IF
    !
  END SUBROUTINE
  !
  !#############################################################################################################################################################
  !
  FUNCTION IS_WINDOWS() RESULT(IS_WIN)
    LOGICAL:: IS_WIN
    INTEGER, SAVE:: WinNT = -1
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
END MODULE