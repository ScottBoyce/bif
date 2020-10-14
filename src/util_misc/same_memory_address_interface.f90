MODULE SAME_MEMORY_ADDRESS_INTERFACE
  USE, INTRINSIC:: ISO_C_BINDING,   ONLY: C_LOC, C_ASSOCIATED
  USE, INTRINSIC:: ISO_FORTRAN_ENV, ONLY: INT32, INT64, REL32 => REAL32, REL64 => REAL64
  !
  IMPLICIT NONE(TYPE, EXTERNAL)
  !
  PUBLIC:: SAME_MEMORY_ADDRESS    ! Compares number of array elements and head pointer. Only works for 1D, 2D, and 3D arrays; viz: (:), (:,:), and (:,:,:)
  PUBLIC:: SAME_MEMORY_LOCATION   ! Only compares the head pointer, viz A(1) and B(1) to see if they are the same memory location
  !
  PRIVATE
  !
  INTERFACE SAME_MEMORY_LOCATION                ! SAME_MEMORY_LOCATION(A, B) RESULT(ANS)
    MODULE PROCEDURE SAME_MEMORY_LOCATION_INT32
    MODULE PROCEDURE SAME_MEMORY_LOCATION_INT64
    MODULE PROCEDURE SAME_MEMORY_LOCATION_REL32
    MODULE PROCEDURE SAME_MEMORY_LOCATION_REL64
  END INTERFACE
  !
  INTERFACE SAME_MEMORY_ADDRESS              ! SAME_MEMORY_ADDRESS(A, B) RESULT(ANS)
    MODULE PROCEDURE SAME_MEMORY_ADDRESS_INT32
    MODULE PROCEDURE SAME_MEMORY_ADDRESS_INT64
    MODULE PROCEDURE SAME_MEMORY_ADDRESS_REL32
    MODULE PROCEDURE SAME_MEMORY_ADDRESS_REL64
    !
    MODULE PROCEDURE SAME_MEMORY_ADDRESS_DIM2_INT32
    MODULE PROCEDURE SAME_MEMORY_ADDRESS_DIM2_INT64
    MODULE PROCEDURE SAME_MEMORY_ADDRESS_DIM2_REL32
    MODULE PROCEDURE SAME_MEMORY_ADDRESS_DIM2_REL64
    !
    MODULE PROCEDURE SAME_MEMORY_ADDRESS_DIM3_INT32
    MODULE PROCEDURE SAME_MEMORY_ADDRESS_DIM3_INT64
    MODULE PROCEDURE SAME_MEMORY_ADDRESS_DIM3_REL32
    MODULE PROCEDURE SAME_MEMORY_ADDRESS_DIM3_REL64
  END INTERFACE
  !
  ! Constants used internally to module ----------------------------------------------------
  ! 
  LOGICAL,   PARAMETER :: TRUE  = .TRUE.
  LOGICAL,   PARAMETER :: FALSE = .FALSE.
  !
  ! ----------------------------------------------------------------------------------------
  ! 
  CONTAINS
  !
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  ! 
  PURE FUNCTION SAME_MEMORY_ADDRESS_INT32(A, B) RESULT(ANS)
    INTEGER(INT32), dimension(:), contiguous, intent(in):: A, B
    LOGICAL:: ANS
    !
    ANS = FALSE
    IF( SIZE(A) == SIZE(B) ) ANS = C_ASSOCIATED(C_LOC(A),C_LOC(B))
    !
  END FUNCTION
  !
  ! ------------------------------------------------------------------------------------------------------------------------
  ! 
  PURE FUNCTION SAME_MEMORY_ADDRESS_INT64(A, B) RESULT(ANS)
    INTEGER(INT64), dimension(:), contiguous, intent(in):: A, B
    LOGICAL:: ANS
    !
    ANS = FALSE
    IF( SIZE(A) == SIZE(B) ) ANS = C_ASSOCIATED(C_LOC(A),C_LOC(B))
    !
  END FUNCTION
  !
  ! ------------------------------------------------------------------------------------------------------------------------
  ! 
  PURE FUNCTION SAME_MEMORY_ADDRESS_REL32(A, B) RESULT(ANS)
    REAL(REL32), dimension(:), contiguous, intent(in):: A, B
    LOGICAL:: ANS
    !
    ANS = FALSE
    IF( SIZE(A) == SIZE(B) ) ANS = C_ASSOCIATED(C_LOC(A),C_LOC(B))
    !
  END FUNCTION
  !
  ! ------------------------------------------------------------------------------------------------------------------------
  ! 
  PURE FUNCTION SAME_MEMORY_ADDRESS_REL64(A, B) RESULT(ANS)
    REAL(REL64), dimension(:), contiguous, intent(in):: A, B
    LOGICAL:: ANS
    !
    ANS = FALSE
    IF( SIZE(A) == SIZE(B) ) ANS = C_ASSOCIATED(C_LOC(A),C_LOC(B))
    !
  END FUNCTION
  !
  !#########################################################################################################################
  ! 
  PURE FUNCTION SAME_MEMORY_ADDRESS_DIM2_INT32(A, B) RESULT(ANS)
    INTEGER(INT32), dimension(:,:), contiguous, intent(in):: A, B
    LOGICAL:: ANS
    !
    ANS = FALSE
    IF( SIZE(A) == SIZE(B) ) ANS = C_ASSOCIATED(C_LOC(A),C_LOC(B))
    !
  END FUNCTION
  !
  ! ------------------------------------------------------------------------------------------------------------------------
  ! 
  PURE FUNCTION SAME_MEMORY_ADDRESS_DIM2_INT64(A, B) RESULT(ANS)
    INTEGER(INT64), dimension(:,:), contiguous, intent(in):: A, B
    LOGICAL:: ANS
    !
    ANS = FALSE
    IF( SIZE(A) == SIZE(B) ) ANS = C_ASSOCIATED(C_LOC(A),C_LOC(B))
    !
  END FUNCTION
  !
  ! ------------------------------------------------------------------------------------------------------------------------
  ! 
  PURE FUNCTION SAME_MEMORY_ADDRESS_DIM2_REL32(A, B) RESULT(ANS)
    REAL(REL32), dimension(:,:), contiguous, intent(in):: A, B
    LOGICAL:: ANS
    !
    ANS = FALSE
    IF( SIZE(A) == SIZE(B) ) ANS = C_ASSOCIATED(C_LOC(A),C_LOC(B))
    !
  END FUNCTION
  !
  ! ------------------------------------------------------------------------------------------------------------------------
  ! 
  PURE FUNCTION SAME_MEMORY_ADDRESS_DIM2_REL64(A, B) RESULT(ANS)
    REAL(REL64), dimension(:,:), contiguous, intent(in):: A, B
    LOGICAL:: ANS
    !
    ANS = FALSE
    IF( SIZE(A) == SIZE(B) ) ANS = C_ASSOCIATED(C_LOC(A),C_LOC(B))
    !
  END FUNCTION
  !
  !#########################################################################################################################
  ! 
  PURE FUNCTION SAME_MEMORY_ADDRESS_DIM3_INT32(A, B) RESULT(ANS)
    INTEGER(INT32), dimension(:,:,:), contiguous, intent(in):: A, B
    LOGICAL:: ANS
    !
    ANS = FALSE
    IF( SIZE(A) == SIZE(B) ) ANS = C_ASSOCIATED(C_LOC(A),C_LOC(B))
    !
  END FUNCTION
  !
  ! ------------------------------------------------------------------------------------------------------------------------
  ! 
  PURE FUNCTION SAME_MEMORY_ADDRESS_DIM3_INT64(A, B) RESULT(ANS)
    INTEGER(INT64), dimension(:,:,:), contiguous, intent(in):: A, B
    LOGICAL:: ANS
    !
    ANS = FALSE
    IF( SIZE(A) == SIZE(B) ) ANS = C_ASSOCIATED(C_LOC(A),C_LOC(B))
    !
  END FUNCTION
  !
  ! ------------------------------------------------------------------------------------------------------------------------
  ! 
  PURE FUNCTION SAME_MEMORY_ADDRESS_DIM3_REL32(A, B) RESULT(ANS)
    REAL(REL32), dimension(:,:,:), contiguous, intent(in):: A, B
    LOGICAL:: ANS
    !
    ANS = FALSE
    IF( SIZE(A) == SIZE(B) ) ANS = C_ASSOCIATED(C_LOC(A),C_LOC(B))
    !
  END FUNCTION
  !
  ! ------------------------------------------------------------------------------------------------------------------------
  ! 
  PURE FUNCTION SAME_MEMORY_ADDRESS_DIM3_REL64(A, B) RESULT(ANS)
    REAL(REL64), dimension(:,:,:), contiguous, intent(in):: A, B
    LOGICAL:: ANS
    !
    ANS = FALSE
    IF( SIZE(A) == SIZE(B) ) ANS = C_ASSOCIATED(C_LOC(A),C_LOC(B))
    !
  END FUNCTION
  !
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  ! 
  PURE FUNCTION SAME_MEMORY_LOCATION_INT32(A, B) RESULT(ANS)
    INTEGER(INT32), dimension(*), intent(in):: A, B
    LOGICAL:: ANS
    !
    ANS = C_ASSOCIATED(C_LOC(A),C_LOC(B))
    !
  END FUNCTION
  !
  ! ------------------------------------------------------------------------------------------------------------------------
  ! 
  PURE FUNCTION SAME_MEMORY_LOCATION_INT64(A, B) RESULT(ANS)
    INTEGER(INT64), dimension(*), intent(in):: A, B
    LOGICAL:: ANS
    !
    ANS = C_ASSOCIATED(C_LOC(A),C_LOC(B))
    !
  END FUNCTION
  !
  ! ------------------------------------------------------------------------------------------------------------------------
  ! 
  PURE FUNCTION SAME_MEMORY_LOCATION_REL32(A, B) RESULT(ANS)
    REAL(REL32), dimension(*), intent(in):: A, B
    LOGICAL:: ANS
    !
    ANS = C_ASSOCIATED(C_LOC(A),C_LOC(B))
    !
  END FUNCTION
  !
  ! ------------------------------------------------------------------------------------------------------------------------
  ! 
  PURE FUNCTION SAME_MEMORY_LOCATION_REL64(A, B) RESULT(ANS)
    REAL(REL64), dimension(*), intent(in):: A, B
    LOGICAL:: ANS
    !
    ANS = C_ASSOCIATED(C_LOC(A),C_LOC(B))
    !
  END FUNCTION
  !
END MODULE