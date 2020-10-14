!
!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<!
!>>>>>>>>>##############>>>>>>>>>################>>>>>>>>>################>>>>>>>>>###############>>>>>>>>>###############>>>>>>>>>>!
!<<<<<<<<<##############<<<<<<<<<################<<<<<<<<<################<<<<<<<<<###############<<<<<<<<<###############<<<<<<<<<<!
!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>!
!
! SORT_INTERFACE_MULTI
!
SUBMODULE (SORT_INTERFACE) SORT_INTERFACE_1D_MULTI
  !---------------------------------------------------------------------------------------------------------------------------
  ! Routines For Sorting Multiple 1D Arrays together
  !---------------------------------------------------------------------------------------------------------------------------
  !
  IMPLICIT NONE
  !
  !INTERFACE SIGN_FLIP
  !  MODULE PROCEDURE SIGN_FLIP_1D_INT32
  !  MODULE PROCEDURE SIGN_FLIP_1D_INT64
  !  MODULE PROCEDURE SIGN_FLIP_1D_REL32
  !  MODULE PROCEDURE SIGN_FLIP_1D_REL64
  !END INTERFACE
  !!
  !INTERFACE JSEARCH                    ! Search for range of repeated values from pos I, that is A(I:J) are the same value
  !  MODULE PROCEDURE JSEARCH_INT32
  !  MODULE PROCEDURE JSEARCH_INT64
  !  MODULE PROCEDURE JSEARCH_REL32
  !  MODULE PROCEDURE JSEARCH_REL64
  !END INTERFACE
  !
  CONTAINS  !================================================================================================================
  ! 
  !--------------------------------------------------------------------------------------------------------------------------
  ! Initialize P
  PURE SUBROUTINE INIT_PERMUTATION(dim, P)
    INTEGER,                 intent(in   ) :: dim
    INTEGER, dimension(dim), intent(inout) :: P
    INTEGER:: I
    DO I=1, dim
            P(I) = I
    END DO
  END SUBROUTINE
  !
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !
  ! SORT_1D_MULTI_INT32_xyz
  !
  MODULE PURE SUBROUTINE SORT_1D_MULTI_DSC1_INT32_INT32(A, B, SORT_B, DESCEND, STABLE, P, QSORT, HEAP, ISORT, SymMerge)
    INTEGER(INT32), dimension(:), contiguous,    intent(inout) :: A           ! A and B must have the same dim
    INTEGER(INT32), dimension(:), contiguous,    intent(inout) :: B           !
    LOGICAL,                           optional, intent(in   ) :: SORT_B      ! If present and false, then only sort A, and permutation sort B; otherwise also sort B along repeated values in A
    LOGICAL,                           optional, intent(in   ) :: DESCEND     ! Sort in Decending Order
    LOGICAL,                           optional, intent(in   ) :: STABLE      ! Use only Stable Sorting Methods
    INTEGER, dimension(:), contiguous, optional, intent(inout) :: P           ! Contains permutations to sort A (Sorts along with A)
    LOGICAL,                           optional, intent(in   ) :: QSORT       ! Use only Dual Pivot Quicksort/IntroSort
    LOGICAL,                           optional, intent(in   ) :: HEAP        ! Use only Heap Sort
    LOGICAL,                           optional, intent(in   ) :: ISORT       ! Use only Insertion Sort
    LOGICAL,                           optional, intent(in   ) :: SymMerge    ! Use only Stable Minimum Storage Merging by Symmetric Comparisons
    LOGICAL, dimension(2) :: DSCND
    IF(present(DESCEND)) then
        DSCND = DESCEND
    else
        DSCND = .FALSE.
    end if
    !
    CALL SORT_1D_MULTI_DSC2_INT32_INT32(A, B, SORT_B, DSCND, STABLE, P, QSORT, HEAP, ISORT, SymMerge)
  END SUBROUTINE
  !
  MODULE PURE SUBROUTINE SORT_1D_MULTI_DSC2_INT32_INT32(A, B, SORT_B, DESCEND, STABLE, P, QSORT, HEAP, ISORT, SymMerge)
    INTEGER(INT32), dimension(:), contiguous,    intent(inout) :: A           ! A and B must have the same dim
    INTEGER(INT32), dimension(:), contiguous,    intent(inout) :: B           !
    LOGICAL,                           optional, intent(in   ) :: SORT_B      ! If present and false, then only sort A, and permutation sort B; otherwise also sort B along repeated values in A
    LOGICAL,        dimension(2),                intent(in   ) :: DESCEND     ! Sort in Decending Order
    LOGICAL,                           optional, intent(in   ) :: STABLE      ! Use only Stable Sorting Methods
    INTEGER, dimension(:), contiguous, optional, intent(inout) :: P           ! Contains permutations to sort A (Sorts along with A)
    LOGICAL,                           optional, intent(in   ) :: QSORT       ! Use only Dual Pivot Quicksort/IntroSort
    LOGICAL,                           optional, intent(in   ) :: HEAP        ! Use only Heap Sort
    LOGICAL,                           optional, intent(in   ) :: ISORT       ! Use only Insertion Sort
    LOGICAL,                           optional, intent(in   ) :: SymMerge    ! Use only Stable Minimum Storage Merging by Symmetric Comparisons
    INTEGER, dimension(:), allocatable:: PNT
    INTEGER:: dim, sTYP
    LOGICAL:: SortB, HAS_P
    !
    dim = SIZE(A)
    if(dim > size(B)) return
    !
    HAS_P = present(P)
    !
    SortB = .TRUE.
    if(present(SORT_B)) SortB = SORT_B
    !
    sTYP = SORT_TYPE(dim, A, DESCEND(1), STABLE, QSORT, HEAP, ISORT, SymMerge)
    !
    if(sTYP == 0) then  ! A already sorted
        if(HAS_P) call INIT_PERMUTATION(dim, P)
        if(SortB) call SORT_B_by_A_and_P(dim, A, B, P, descend(2))
    else
        ALLOCATE(PNT(dim))
        !
        CALL SORT(A, DESCEND(1), P=PNT, SORT_TYPE=sTYP)
        !
        if(SortB) then
                  CALL SORT_B_by_A_and_P(dim, A, B, PNT, descend(2))
        else
                  CALL PERMUTATION_SORT(B, PNT)
        end if
        !
        if(HAS_P) P(1:dim) = PNT(1:dim)
        DEALLOCATE(PNT)
    end if
    !
  END SUBROUTINE 
  !
  !--------------------------------------------------------------------------------------------------------------------------
  !
  MODULE PURE SUBROUTINE SORT_1D_MULTI_DSC1_INT32_INT64(A, B, SORT_B, DESCEND, STABLE, P, QSORT, HEAP, ISORT, SymMerge)
    INTEGER(INT32), dimension(:), contiguous,    intent(inout) :: A           ! A and B must have the same dim
    INTEGER(INT64), dimension(:), contiguous,    intent(inout) :: B           !
    LOGICAL,                           optional, intent(in   ) :: SORT_B      ! If present and false, then only sort A, and permutation sort B; otherwise also sort B along repeated values in A
    LOGICAL,                           optional, intent(in   ) :: DESCEND     ! Sort in Decending Order
    LOGICAL,                           optional, intent(in   ) :: STABLE      ! Use only Stable Sorting Methods
    INTEGER, dimension(:), contiguous, optional, intent(inout) :: P           ! Contains permutations to sort A (Sorts along with A)
    LOGICAL,                           optional, intent(in   ) :: QSORT       ! Use only Dual Pivot Quicksort/IntroSort
    LOGICAL,                           optional, intent(in   ) :: HEAP        ! Use only Heap Sort
    LOGICAL,                           optional, intent(in   ) :: ISORT       ! Use only Insertion Sort
    LOGICAL,                           optional, intent(in   ) :: SymMerge    ! Use only Stable Minimum Storage Merging by Symmetric Comparisons
    LOGICAL, dimension(2) :: DSCND
    IF(present(DESCEND)) then
        DSCND = DESCEND
    else
        DSCND = .FALSE.
    end if
    !
    CALL SORT_1D_MULTI_DSC2_INT32_INT64(A, B, SORT_B, DSCND, STABLE, P, QSORT, HEAP, ISORT, SymMerge)
  END SUBROUTINE
  !
  MODULE PURE SUBROUTINE SORT_1D_MULTI_DSC2_INT32_INT64(A, B, SORT_B, DESCEND, STABLE, P, QSORT, HEAP, ISORT, SymMerge)
    INTEGER(INT32), dimension(:), contiguous,    intent(inout) :: A           ! A and B must have the same dim
    INTEGER(INT64), dimension(:), contiguous,    intent(inout) :: B           !
    LOGICAL,                           optional, intent(in   ) :: SORT_B      ! If present and false, then only sort A, and permutation sort B; otherwise also sort B along repeated values in A
    LOGICAL,        dimension(2),                intent(in   ) :: DESCEND     ! Sort in Decending Order
    LOGICAL,                           optional, intent(in   ) :: STABLE      ! Use only Stable Sorting Methods
    INTEGER, dimension(:), contiguous, optional, intent(inout) :: P           ! Contains permutations to sort A (Sorts along with A)
    LOGICAL,                           optional, intent(in   ) :: QSORT       ! Use only Dual Pivot Quicksort/IntroSort
    LOGICAL,                           optional, intent(in   ) :: HEAP        ! Use only Heap Sort
    LOGICAL,                           optional, intent(in   ) :: ISORT       ! Use only Insertion Sort
    LOGICAL,                           optional, intent(in   ) :: SymMerge    ! Use only Stable Minimum Storage Merging by Symmetric Comparisons
    INTEGER, dimension(:), allocatable:: PNT
    INTEGER:: dim, sTYP
    LOGICAL:: SortB, HAS_P
    !
    dim = SIZE(A)
    if(dim > size(B)) return
    !
    HAS_P = present(P)
    !
    SortB = .TRUE.
    if(present(SORT_B)) SortB = SORT_B
    !
    sTYP = SORT_TYPE(dim, A, DESCEND(1), STABLE, QSORT, HEAP, ISORT, SymMerge)
    !
    if(sTYP == 0) then  ! A already sorted
        if(HAS_P) call INIT_PERMUTATION(dim, P)
        if(SortB) call SORT_B_by_A_and_P(dim, A, B, P, descend(2))
    else
        ALLOCATE(PNT(dim))
        !
        CALL SORT(A, DESCEND(1), P=PNT, SORT_TYPE=sTYP)
        !
        if(SortB) then
                  CALL SORT_B_by_A_and_P(dim, A, B, PNT, descend(2))
        else
                  CALL PERMUTATION_SORT(B, PNT)
        end if
        !
        if(HAS_P) P(1:dim) = PNT(1:dim)
        DEALLOCATE(PNT)
    end if
    !
  END SUBROUTINE 
  !
  !--------------------------------------------------------------------------------------------------------------------------
  !
  MODULE PURE SUBROUTINE SORT_1D_MULTI_DSC1_INT32_REL32(A, B, SORT_B, DESCEND, STABLE, P, QSORT, HEAP, ISORT, SymMerge)
    INTEGER(INT32), dimension(:), contiguous,    intent(inout) :: A           ! A and B must have the same dim
    REAL(REL32),    dimension(:), contiguous,    intent(inout) :: B           !
    LOGICAL,                           optional, intent(in   ) :: SORT_B      ! If present and false, then only sort A, and permutation sort B; otherwise also sort B along repeated values in A
    LOGICAL,                           optional, intent(in   ) :: DESCEND     ! Sort in Decending Order
    LOGICAL,                           optional, intent(in   ) :: STABLE      ! Use only Stable Sorting Methods
    INTEGER, dimension(:), contiguous, optional, intent(inout) :: P           ! Contains permutations to sort A (Sorts along with A)
    LOGICAL,                           optional, intent(in   ) :: QSORT       ! Use only Dual Pivot Quicksort/IntroSort
    LOGICAL,                           optional, intent(in   ) :: HEAP        ! Use only Heap Sort
    LOGICAL,                           optional, intent(in   ) :: ISORT       ! Use only Insertion Sort
    LOGICAL,                           optional, intent(in   ) :: SymMerge    ! Use only Stable Minimum Storage Merging by Symmetric Comparisons
    LOGICAL, dimension(2) :: DSCND
    IF(present(DESCEND)) then
        DSCND = DESCEND
    else
        DSCND = .FALSE.
    end if
    !
    CALL SORT_1D_MULTI_DSC2_INT32_REL32(A, B, SORT_B, DSCND, STABLE, P, QSORT, HEAP, ISORT, SymMerge)
  END SUBROUTINE
  !
  MODULE PURE SUBROUTINE SORT_1D_MULTI_DSC2_INT32_REL32(A, B, SORT_B, DESCEND, STABLE, P, QSORT, HEAP, ISORT, SymMerge)
    INTEGER(INT32), dimension(:), contiguous,    intent(inout) :: A           ! A and B must have the same dim
    REAL(REL32),    dimension(:), contiguous,    intent(inout) :: B           !
    LOGICAL,                           optional, intent(in   ) :: SORT_B      ! If present and false, then only sort A, and permutation sort B; otherwise also sort B along repeated values in A
    LOGICAL,        dimension(2),                intent(in   ) :: DESCEND     ! Sort in Decending Order
    LOGICAL,                           optional, intent(in   ) :: STABLE      ! Use only Stable Sorting Methods
    INTEGER, dimension(:), contiguous, optional, intent(inout) :: P           ! Contains permutations to sort A (Sorts along with A)
    LOGICAL,                           optional, intent(in   ) :: QSORT       ! Use only Dual Pivot Quicksort/IntroSort
    LOGICAL,                           optional, intent(in   ) :: HEAP        ! Use only Heap Sort
    LOGICAL,                           optional, intent(in   ) :: ISORT       ! Use only Insertion Sort
    LOGICAL,                           optional, intent(in   ) :: SymMerge    ! Use only Stable Minimum Storage Merging by Symmetric Comparisons
    INTEGER, dimension(:), allocatable:: PNT
    INTEGER:: dim, sTYP
    LOGICAL:: SortB, HAS_P
    !
    dim = SIZE(A)
    if(dim > size(B)) return
    !
    HAS_P = present(P)
    !
    SortB = .TRUE.
    if(present(SORT_B)) SortB = SORT_B
    !
    sTYP = SORT_TYPE(dim, A, DESCEND(1), STABLE, QSORT, HEAP, ISORT, SymMerge)
    !
    if(sTYP == 0) then  ! A already sorted
        if(HAS_P) call INIT_PERMUTATION(dim, P)
        if(SortB) call SORT_B_by_A_and_P(dim, A, B, P, descend(2))
    else
        ALLOCATE(PNT(dim))
        !
        CALL SORT(A, DESCEND(1), P=PNT, SORT_TYPE=sTYP)
        !
        if(SortB) then
                  CALL SORT_B_by_A_and_P(dim, A, B, PNT, descend(2))
        else
                  CALL PERMUTATION_SORT(B, PNT)
        end if
        !
        if(HAS_P) P(1:dim) = PNT(1:dim)
        DEALLOCATE(PNT)
    end if
    !
  END SUBROUTINE 
  !
  !--------------------------------------------------------------------------------------------------------------------------
  !
  MODULE PURE SUBROUTINE SORT_1D_MULTI_DSC1_INT32_REL64(A, B, SORT_B, DESCEND, STABLE, P, QSORT, HEAP, ISORT, SymMerge)
    INTEGER(INT32), dimension(:), contiguous,    intent(inout) :: A           ! A and B must have the same dim
    REAL(REL64),    dimension(:), contiguous,    intent(inout) :: B           !
    LOGICAL,                           optional, intent(in   ) :: SORT_B      ! If present and false, then only sort A, and permutation sort B; otherwise also sort B along repeated values in A
    LOGICAL,                           optional, intent(in   ) :: DESCEND     ! Sort in Decending Order
    LOGICAL,                           optional, intent(in   ) :: STABLE      ! Use only Stable Sorting Methods
    INTEGER, dimension(:), contiguous, optional, intent(inout) :: P           ! Contains permutations to sort A (Sorts along with A)
    LOGICAL,                           optional, intent(in   ) :: QSORT       ! Use only Dual Pivot Quicksort/IntroSort
    LOGICAL,                           optional, intent(in   ) :: HEAP        ! Use only Heap Sort
    LOGICAL,                           optional, intent(in   ) :: ISORT       ! Use only Insertion Sort
    LOGICAL,                           optional, intent(in   ) :: SymMerge    ! Use only Stable Minimum Storage Merging by Symmetric Comparisons
    LOGICAL, dimension(2) :: DSCND
    IF(present(DESCEND)) then
        DSCND = DESCEND
    else
        DSCND = .FALSE.
    end if
    !
    CALL SORT_1D_MULTI_DSC2_INT32_REL64(A, B, SORT_B, DSCND, STABLE, P, QSORT, HEAP, ISORT, SymMerge)
  END SUBROUTINE
  !
  MODULE PURE SUBROUTINE SORT_1D_MULTI_DSC2_INT32_REL64(A, B, SORT_B, DESCEND, STABLE, P, QSORT, HEAP, ISORT, SymMerge)
    INTEGER(INT32), dimension(:), contiguous,    intent(inout) :: A           ! A and B must have the same dim
    REAL(REL64),    dimension(:), contiguous,    intent(inout) :: B           !
    LOGICAL,                           optional, intent(in   ) :: SORT_B      ! If present and false, then only sort A, and permutation sort B; otherwise also sort B along repeated values in A
    LOGICAL,        dimension(2),                intent(in   ) :: DESCEND     ! Sort in Decending Order
    LOGICAL,                           optional, intent(in   ) :: STABLE      ! Use only Stable Sorting Methods
    INTEGER, dimension(:), contiguous, optional, intent(inout) :: P           ! Contains permutations to sort A (Sorts along with A)
    LOGICAL,                           optional, intent(in   ) :: QSORT       ! Use only Dual Pivot Quicksort/IntroSort
    LOGICAL,                           optional, intent(in   ) :: HEAP        ! Use only Heap Sort
    LOGICAL,                           optional, intent(in   ) :: ISORT       ! Use only Insertion Sort
    LOGICAL,                           optional, intent(in   ) :: SymMerge    ! Use only Stable Minimum Storage Merging by Symmetric Comparisons
    INTEGER, dimension(:), allocatable:: PNT
    INTEGER:: dim, sTYP
    LOGICAL:: SortB, HAS_P
    !
    dim = SIZE(A)
    if(dim > size(B)) return
    !
    HAS_P = present(P)
    !
    SortB = .TRUE.
    if(present(SORT_B)) SortB = SORT_B
    !
    sTYP = SORT_TYPE(dim, A, DESCEND(1), STABLE, QSORT, HEAP, ISORT, SymMerge)
    !
    if(sTYP == 0) then  ! A already sorted
        if(HAS_P) call INIT_PERMUTATION(dim, P)
        if(SortB) call SORT_B_by_A_and_P(dim, A, B, P, descend(2))
    else
        ALLOCATE(PNT(dim))
        !
        CALL SORT(A, DESCEND(1), P=PNT, SORT_TYPE=sTYP)
        !
        if(SortB) then
                  CALL SORT_B_by_A_and_P(dim, A, B, PNT, descend(2))
        else
                  CALL PERMUTATION_SORT(B, PNT)
        end if
        !
        if(HAS_P) P(1:dim) = PNT(1:dim)
        DEALLOCATE(PNT)
    end if
    !
  END SUBROUTINE 
  !
  !--------------------------------------------------------------------------------------------------------------------------
  !
  MODULE PURE SUBROUTINE SORT_1D_MULTI_DSC1_INT32_CHAR(A, B, SORT_B, DESCEND, STABLE, P, QSORT, HEAP, ISORT, SymMerge)
    INTEGER(INT32), dimension(:), contiguous,    intent(inout) :: A           ! A and B must have the same dim
    CHARACTER(*),   dimension(:), contiguous,    intent(inout) :: B           !
    LOGICAL,                           optional, intent(in   ) :: SORT_B      ! If present and false, then only sort A, and permutation sort B; otherwise also sort B along repeated values in A
    LOGICAL,                           optional, intent(in   ) :: DESCEND     ! Sort in Decending Order
    LOGICAL,                           optional, intent(in   ) :: STABLE      ! Use only Stable Sorting Methods
    INTEGER, dimension(:), contiguous, optional, intent(inout) :: P           ! Contains permutations to sort A (Sorts along with A)
    LOGICAL,                           optional, intent(in   ) :: QSORT       ! Use only Dual Pivot Quicksort/IntroSort
    LOGICAL,                           optional, intent(in   ) :: HEAP        ! Use only Heap Sort
    LOGICAL,                           optional, intent(in   ) :: ISORT       ! Use only Insertion Sort
    LOGICAL,                           optional, intent(in   ) :: SymMerge    ! Use only Stable Minimum Storage Merging by Symmetric Comparisons
    LOGICAL, dimension(2) :: DSCND
    IF(present(DESCEND)) then
        DSCND = DESCEND
    else
        DSCND = .FALSE.
    end if
    !
    CALL SORT_1D_MULTI_DSC2_INT32_CHAR(A, B, SORT_B, DSCND, STABLE, P, QSORT, HEAP, ISORT, SymMerge)
  END SUBROUTINE
  !
  MODULE PURE SUBROUTINE SORT_1D_MULTI_DSC2_INT32_CHAR(A, B, SORT_B, DESCEND, STABLE, P, QSORT, HEAP, ISORT, SymMerge)
    INTEGER(INT32), dimension(:), contiguous,    intent(inout) :: A           ! A and B must have the same dim
    CHARACTER(*),   dimension(:), contiguous,    intent(inout) :: B           !
    LOGICAL,                           optional, intent(in   ) :: SORT_B      ! If present and false, then only sort A, and permutation sort B; otherwise also sort B along repeated values in A
    LOGICAL,        dimension(2),                intent(in   ) :: DESCEND     ! Sort in Decending Order
    LOGICAL,                           optional, intent(in   ) :: STABLE      ! Use only Stable Sorting Methods
    INTEGER, dimension(:), contiguous, optional, intent(inout) :: P           ! Contains permutations to sort A (Sorts along with A)
    LOGICAL,                           optional, intent(in   ) :: QSORT       ! Use only Dual Pivot Quicksort/IntroSort
    LOGICAL,                           optional, intent(in   ) :: HEAP        ! Use only Heap Sort
    LOGICAL,                           optional, intent(in   ) :: ISORT       ! Use only Insertion Sort
    LOGICAL,                           optional, intent(in   ) :: SymMerge    ! Use only Stable Minimum Storage Merging by Symmetric Comparisons
    INTEGER, dimension(:), allocatable:: PNT
    INTEGER:: dim, sTYP
    LOGICAL:: SortB, HAS_P
    !
    dim = SIZE(A)
    if(dim > size(B)) return
    !
    HAS_P = present(P)
    !
    SortB = .TRUE.
    if(present(SORT_B)) SortB = SORT_B
    !
    sTYP = SORT_TYPE(dim, A, DESCEND(1), STABLE, QSORT, HEAP, ISORT, SymMerge)
    !
    if(sTYP == 0) then  ! A already sorted
        if(HAS_P) call INIT_PERMUTATION(dim, P)
        if(SortB) call SORT_B_by_A_and_P(dim, A, B, P, descend(2))
    else
        ALLOCATE(PNT(dim))
        !
        CALL SORT(A, DESCEND(1), P=PNT, SORT_TYPE=sTYP)
        !
        if(SortB) then
                  CALL SORT_B_by_A_and_P(dim, A, B, PNT, descend(2))
        else
                  CALL PERMUTATION_SORT(B, PNT)
        end if
        !
        if(HAS_P) P(1:dim) = PNT(1:dim)
        DEALLOCATE(PNT)
    end if
    !
  END SUBROUTINE 
  !
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !
  ! SORT_1D_MULTI_INT64_xyz
  !
  MODULE PURE SUBROUTINE SORT_1D_MULTI_DSC1_INT64_INT32(A, B, SORT_B, DESCEND, STABLE, P, QSORT, HEAP, ISORT, SymMerge)
    INTEGER(INT64), dimension(:), contiguous,    intent(inout) :: A           ! A and B must have the same dim
    INTEGER(INT32), dimension(:), contiguous,    intent(inout) :: B           !
    LOGICAL,                           optional, intent(in   ) :: SORT_B      ! If present and false, then only sort A, and permutation sort B; otherwise also sort B along repeated values in A
    LOGICAL,                           optional, intent(in   ) :: DESCEND     ! Sort in Decending Order
    LOGICAL,                           optional, intent(in   ) :: STABLE      ! Use only Stable Sorting Methods
    INTEGER, dimension(:), contiguous, optional, intent(inout) :: P           ! Contains permutations to sort A (Sorts along with A)
    LOGICAL,                           optional, intent(in   ) :: QSORT       ! Use only Dual Pivot Quicksort/IntroSort
    LOGICAL,                           optional, intent(in   ) :: HEAP        ! Use only Heap Sort
    LOGICAL,                           optional, intent(in   ) :: ISORT       ! Use only Insertion Sort
    LOGICAL,                           optional, intent(in   ) :: SymMerge    ! Use only Stable Minimum Storage Merging by Symmetric Comparisons
    LOGICAL, dimension(2) :: DSCND
    IF(present(DESCEND)) then
        DSCND = DESCEND
    else
        DSCND = .FALSE.
    end if
    !
    CALL SORT_1D_MULTI_DSC2_INT64_INT32(A, B, SORT_B, DSCND, STABLE, P, QSORT, HEAP, ISORT, SymMerge)
  END SUBROUTINE
  !
  MODULE PURE SUBROUTINE SORT_1D_MULTI_DSC2_INT64_INT32(A, B, SORT_B, DESCEND, STABLE, P, QSORT, HEAP, ISORT, SymMerge)
    INTEGER(INT64), dimension(:), contiguous,    intent(inout) :: A           ! A and B must have the same dim
    INTEGER(INT32), dimension(:), contiguous,    intent(inout) :: B           !
    LOGICAL,                           optional, intent(in   ) :: SORT_B      ! If present and false, then only sort A, and permutation sort B; otherwise also sort B along repeated values in A
    LOGICAL,        dimension(2),                intent(in   ) :: DESCEND     ! Sort in Decending Order
    LOGICAL,                           optional, intent(in   ) :: STABLE      ! Use only Stable Sorting Methods
    INTEGER, dimension(:), contiguous, optional, intent(inout) :: P           ! Contains permutations to sort A (Sorts along with A)
    LOGICAL,                           optional, intent(in   ) :: QSORT       ! Use only Dual Pivot Quicksort/IntroSort
    LOGICAL,                           optional, intent(in   ) :: HEAP        ! Use only Heap Sort
    LOGICAL,                           optional, intent(in   ) :: ISORT       ! Use only Insertion Sort
    LOGICAL,                           optional, intent(in   ) :: SymMerge    ! Use only Stable Minimum Storage Merging by Symmetric Comparisons
    INTEGER, dimension(:), allocatable:: PNT
    INTEGER:: dim, sTYP
    LOGICAL:: SortB, HAS_P
    !
    dim = SIZE(A)
    if(dim > size(B)) return
    !
    HAS_P = present(P)
    !
    SortB = .TRUE.
    if(present(SORT_B)) SortB = SORT_B
    !
    sTYP = SORT_TYPE(dim, A, DESCEND(1), STABLE, QSORT, HEAP, ISORT, SymMerge)
    !
    if(sTYP == 0) then  ! A already sorted
        if(HAS_P) call INIT_PERMUTATION(dim, P)
        if(SortB) call SORT_B_by_A_and_P(dim, A, B, P, descend(2))
    else
        ALLOCATE(PNT(dim))
        !
        CALL SORT(A, DESCEND(1), P=PNT, SORT_TYPE=sTYP)
        !
        if(SortB) then
                  CALL SORT_B_by_A_and_P(dim, A, B, PNT, descend(2))
        else
                  CALL PERMUTATION_SORT(B, PNT)
        end if
        !
        if(HAS_P) P(1:dim) = PNT(1:dim)
        DEALLOCATE(PNT)
    end if
    !
  END SUBROUTINE 
  !
  !--------------------------------------------------------------------------------------------------------------------------
  !
  MODULE PURE SUBROUTINE SORT_1D_MULTI_DSC1_INT64_INT64(A, B, SORT_B, DESCEND, STABLE, P, QSORT, HEAP, ISORT, SymMerge)
    INTEGER(INT64), dimension(:), contiguous,    intent(inout) :: A           ! A and B must have the same dim
    INTEGER(INT64), dimension(:), contiguous,    intent(inout) :: B           !
    LOGICAL,                           optional, intent(in   ) :: SORT_B      ! If present and false, then only sort A, and permutation sort B; otherwise also sort B along repeated values in A
    LOGICAL,                           optional, intent(in   ) :: DESCEND     ! Sort in Decending Order
    LOGICAL,                           optional, intent(in   ) :: STABLE      ! Use only Stable Sorting Methods
    INTEGER, dimension(:), contiguous, optional, intent(inout) :: P           ! Contains permutations to sort A (Sorts along with A)
    LOGICAL,                           optional, intent(in   ) :: QSORT       ! Use only Dual Pivot Quicksort/IntroSort
    LOGICAL,                           optional, intent(in   ) :: HEAP        ! Use only Heap Sort
    LOGICAL,                           optional, intent(in   ) :: ISORT       ! Use only Insertion Sort
    LOGICAL,                           optional, intent(in   ) :: SymMerge    ! Use only Stable Minimum Storage Merging by Symmetric Comparisons
    LOGICAL, dimension(2) :: DSCND
    IF(present(DESCEND)) then
        DSCND = DESCEND
    else
        DSCND = .FALSE.
    end if
    !
    CALL SORT_1D_MULTI_DSC2_INT64_INT64(A, B, SORT_B, DSCND, STABLE, P, QSORT, HEAP, ISORT, SymMerge)
  END SUBROUTINE
  !
  MODULE PURE SUBROUTINE SORT_1D_MULTI_DSC2_INT64_INT64(A, B, SORT_B, DESCEND, STABLE, P, QSORT, HEAP, ISORT, SymMerge)
    INTEGER(INT64), dimension(:), contiguous,    intent(inout) :: A           ! A and B must have the same dim
    INTEGER(INT64), dimension(:), contiguous,    intent(inout) :: B           !
    LOGICAL,                           optional, intent(in   ) :: SORT_B      ! If present and false, then only sort A, and permutation sort B; otherwise also sort B along repeated values in A
    LOGICAL,        dimension(2),                intent(in   ) :: DESCEND     ! Sort in Decending Order
    LOGICAL,                           optional, intent(in   ) :: STABLE      ! Use only Stable Sorting Methods
    INTEGER, dimension(:), contiguous, optional, intent(inout) :: P           ! Contains permutations to sort A (Sorts along with A)
    LOGICAL,                           optional, intent(in   ) :: QSORT       ! Use only Dual Pivot Quicksort/IntroSort
    LOGICAL,                           optional, intent(in   ) :: HEAP        ! Use only Heap Sort
    LOGICAL,                           optional, intent(in   ) :: ISORT       ! Use only Insertion Sort
    LOGICAL,                           optional, intent(in   ) :: SymMerge    ! Use only Stable Minimum Storage Merging by Symmetric Comparisons
    INTEGER, dimension(:), allocatable:: PNT
    INTEGER:: dim, sTYP
    LOGICAL:: SortB, HAS_P
    !
    dim = SIZE(A)
    if(dim > size(B)) return
    !
    HAS_P = present(P)
    !
    SortB = .TRUE.
    if(present(SORT_B)) SortB = SORT_B
    !
    sTYP = SORT_TYPE(dim, A, DESCEND(1), STABLE, QSORT, HEAP, ISORT, SymMerge)
    !
    if(sTYP == 0) then  ! A already sorted
        if(HAS_P) call INIT_PERMUTATION(dim, P)
        if(SortB) call SORT_B_by_A_and_P(dim, A, B, P, descend(2))
    else
        ALLOCATE(PNT(dim))
        !
        CALL SORT(A, DESCEND(1), P=PNT, SORT_TYPE=sTYP)
        !
        if(SortB) then
                  CALL SORT_B_by_A_and_P(dim, A, B, PNT, descend(2))
        else
                  CALL PERMUTATION_SORT(B, PNT)
        end if
        !
        if(HAS_P) P(1:dim) = PNT(1:dim)
        DEALLOCATE(PNT)
    end if
    !
  END SUBROUTINE 
  !
  !--------------------------------------------------------------------------------------------------------------------------
  !
  MODULE PURE SUBROUTINE SORT_1D_MULTI_DSC1_INT64_REL32(A, B, SORT_B, DESCEND, STABLE, P, QSORT, HEAP, ISORT, SymMerge)
    INTEGER(INT64), dimension(:), contiguous,    intent(inout) :: A           ! A and B must have the same dim
    REAL(REL32),    dimension(:), contiguous,    intent(inout) :: B           !
    LOGICAL,                           optional, intent(in   ) :: SORT_B      ! If present and false, then only sort A, and permutation sort B; otherwise also sort B along repeated values in A
    LOGICAL,                           optional, intent(in   ) :: DESCEND     ! Sort in Decending Order
    LOGICAL,                           optional, intent(in   ) :: STABLE      ! Use only Stable Sorting Methods
    INTEGER, dimension(:), contiguous, optional, intent(inout) :: P           ! Contains permutations to sort A (Sorts along with A)
    LOGICAL,                           optional, intent(in   ) :: QSORT       ! Use only Dual Pivot Quicksort/IntroSort
    LOGICAL,                           optional, intent(in   ) :: HEAP        ! Use only Heap Sort
    LOGICAL,                           optional, intent(in   ) :: ISORT       ! Use only Insertion Sort
    LOGICAL,                           optional, intent(in   ) :: SymMerge    ! Use only Stable Minimum Storage Merging by Symmetric Comparisons
    LOGICAL, dimension(2) :: DSCND
    IF(present(DESCEND)) then
        DSCND = DESCEND
    else
        DSCND = .FALSE.
    end if
    !
    CALL SORT_1D_MULTI_DSC2_INT64_REL32(A, B, SORT_B, DSCND, STABLE, P, QSORT, HEAP, ISORT, SymMerge)
  END SUBROUTINE
  !
  MODULE PURE SUBROUTINE SORT_1D_MULTI_DSC2_INT64_REL32(A, B, SORT_B, DESCEND, STABLE, P, QSORT, HEAP, ISORT, SymMerge)
    INTEGER(INT64), dimension(:), contiguous,    intent(inout) :: A           ! A and B must have the same dim
    REAL(REL32),    dimension(:), contiguous,    intent(inout) :: B           !
    LOGICAL,                           optional, intent(in   ) :: SORT_B      ! If present and false, then only sort A, and permutation sort B; otherwise also sort B along repeated values in A
    LOGICAL,        dimension(2),                intent(in   ) :: DESCEND     ! Sort in Decending Order
    LOGICAL,                           optional, intent(in   ) :: STABLE      ! Use only Stable Sorting Methods
    INTEGER, dimension(:), contiguous, optional, intent(inout) :: P           ! Contains permutations to sort A (Sorts along with A)
    LOGICAL,                           optional, intent(in   ) :: QSORT       ! Use only Dual Pivot Quicksort/IntroSort
    LOGICAL,                           optional, intent(in   ) :: HEAP        ! Use only Heap Sort
    LOGICAL,                           optional, intent(in   ) :: ISORT       ! Use only Insertion Sort
    LOGICAL,                           optional, intent(in   ) :: SymMerge    ! Use only Stable Minimum Storage Merging by Symmetric Comparisons
    INTEGER, dimension(:), allocatable:: PNT
    INTEGER:: dim, sTYP
    LOGICAL:: SortB, HAS_P
    !
    dim = SIZE(A)
    if(dim > size(B)) return
    !
    HAS_P = present(P)
    !
    SortB = .TRUE.
    if(present(SORT_B)) SortB = SORT_B
    !
    sTYP = SORT_TYPE(dim, A, DESCEND(1), STABLE, QSORT, HEAP, ISORT, SymMerge)
    !
    if(sTYP == 0) then  ! A already sorted
        if(HAS_P) call INIT_PERMUTATION(dim, P)
        if(SortB) call SORT_B_by_A_and_P(dim, A, B, P, descend(2))
    else
        ALLOCATE(PNT(dim))
        !
        CALL SORT(A, DESCEND(1), P=PNT, SORT_TYPE=sTYP)
        !
        if(SortB) then
                  CALL SORT_B_by_A_and_P(dim, A, B, PNT, descend(2))
        else
                  CALL PERMUTATION_SORT(B, PNT)
        end if
        !
        if(HAS_P) P(1:dim) = PNT(1:dim)
        DEALLOCATE(PNT)
    end if
    !
  END SUBROUTINE 
  !
  !--------------------------------------------------------------------------------------------------------------------------
  !
  MODULE PURE SUBROUTINE SORT_1D_MULTI_DSC1_INT64_REL64(A, B, SORT_B, DESCEND, STABLE, P, QSORT, HEAP, ISORT, SymMerge)
    INTEGER(INT64), dimension(:), contiguous,    intent(inout) :: A           ! A and B must have the same dim
    REAL(REL64),    dimension(:), contiguous,    intent(inout) :: B           !
    LOGICAL,                           optional, intent(in   ) :: SORT_B      ! If present and false, then only sort A, and permutation sort B; otherwise also sort B along repeated values in A
    LOGICAL,                           optional, intent(in   ) :: DESCEND     ! Sort in Decending Order
    LOGICAL,                           optional, intent(in   ) :: STABLE      ! Use only Stable Sorting Methods
    INTEGER, dimension(:), contiguous, optional, intent(inout) :: P           ! Contains permutations to sort A (Sorts along with A)
    LOGICAL,                           optional, intent(in   ) :: QSORT       ! Use only Dual Pivot Quicksort/IntroSort
    LOGICAL,                           optional, intent(in   ) :: HEAP        ! Use only Heap Sort
    LOGICAL,                           optional, intent(in   ) :: ISORT       ! Use only Insertion Sort
    LOGICAL,                           optional, intent(in   ) :: SymMerge    ! Use only Stable Minimum Storage Merging by Symmetric Comparisons
    LOGICAL, dimension(2) :: DSCND
    IF(present(DESCEND)) then
        DSCND = DESCEND
    else
        DSCND = .FALSE.
    end if
    !
    CALL SORT_1D_MULTI_DSC2_INT64_REL64(A, B, SORT_B, DSCND, STABLE, P, QSORT, HEAP, ISORT, SymMerge)
  END SUBROUTINE
  !
  MODULE PURE SUBROUTINE SORT_1D_MULTI_DSC2_INT64_REL64(A, B, SORT_B, DESCEND, STABLE, P, QSORT, HEAP, ISORT, SymMerge)
    INTEGER(INT64), dimension(:), contiguous,    intent(inout) :: A           ! A and B must have the same dim
    REAL(REL64),    dimension(:), contiguous,    intent(inout) :: B           !
    LOGICAL,                           optional, intent(in   ) :: SORT_B      ! If present and false, then only sort A, and permutation sort B; otherwise also sort B along repeated values in A
    LOGICAL,        dimension(2),                intent(in   ) :: DESCEND     ! Sort in Decending Order
    LOGICAL,                           optional, intent(in   ) :: STABLE      ! Use only Stable Sorting Methods
    INTEGER, dimension(:), contiguous, optional, intent(inout) :: P           ! Contains permutations to sort A (Sorts along with A)
    LOGICAL,                           optional, intent(in   ) :: QSORT       ! Use only Dual Pivot Quicksort/IntroSort
    LOGICAL,                           optional, intent(in   ) :: HEAP        ! Use only Heap Sort
    LOGICAL,                           optional, intent(in   ) :: ISORT       ! Use only Insertion Sort
    LOGICAL,                           optional, intent(in   ) :: SymMerge    ! Use only Stable Minimum Storage Merging by Symmetric Comparisons
    INTEGER, dimension(:), allocatable:: PNT
    INTEGER:: dim, sTYP
    LOGICAL:: SortB, HAS_P
    !
    dim = SIZE(A)
    if(dim > size(B)) return
    !
    HAS_P = present(P)
    !
    SortB = .TRUE.
    if(present(SORT_B)) SortB = SORT_B
    !
    sTYP = SORT_TYPE(dim, A, DESCEND(1), STABLE, QSORT, HEAP, ISORT, SymMerge)
    !
    if(sTYP == 0) then  ! A already sorted
        if(HAS_P) call INIT_PERMUTATION(dim, P)
        if(SortB) call SORT_B_by_A_and_P(dim, A, B, P, descend(2))
    else
        ALLOCATE(PNT(dim))
        !
        CALL SORT(A, DESCEND(1), P=PNT, SORT_TYPE=sTYP)
        !
        if(SortB) then
                  CALL SORT_B_by_A_and_P(dim, A, B, PNT, descend(2))
        else
                  CALL PERMUTATION_SORT(B, PNT)
        end if
        !
        if(HAS_P) P(1:dim) = PNT(1:dim)
        DEALLOCATE(PNT)
    end if
    !
  END SUBROUTINE 
  !
  !--------------------------------------------------------------------------------------------------------------------------
  !
  MODULE PURE SUBROUTINE SORT_1D_MULTI_DSC1_INT64_CHAR(A, B, SORT_B, DESCEND, STABLE, P, QSORT, HEAP, ISORT, SymMerge)
    INTEGER(INT64), dimension(:), contiguous,    intent(inout) :: A           ! A and B must have the same dim
    CHARACTER(*),   dimension(:), contiguous,    intent(inout) :: B           !
    LOGICAL,                           optional, intent(in   ) :: SORT_B      ! If present and false, then only sort A, and permutation sort B; otherwise also sort B along repeated values in A
    LOGICAL,                           optional, intent(in   ) :: DESCEND     ! Sort in Decending Order
    LOGICAL,                           optional, intent(in   ) :: STABLE      ! Use only Stable Sorting Methods
    INTEGER, dimension(:), contiguous, optional, intent(inout) :: P           ! Contains permutations to sort A (Sorts along with A)
    LOGICAL,                           optional, intent(in   ) :: QSORT       ! Use only Dual Pivot Quicksort/IntroSort
    LOGICAL,                           optional, intent(in   ) :: HEAP        ! Use only Heap Sort
    LOGICAL,                           optional, intent(in   ) :: ISORT       ! Use only Insertion Sort
    LOGICAL,                           optional, intent(in   ) :: SymMerge    ! Use only Stable Minimum Storage Merging by Symmetric Comparisons
    LOGICAL, dimension(2) :: DSCND
    IF(present(DESCEND)) then
        DSCND = DESCEND
    else
        DSCND = .FALSE.
    end if
    !
    CALL SORT_1D_MULTI_DSC2_INT64_CHAR(A, B, SORT_B, DSCND, STABLE, P, QSORT, HEAP, ISORT, SymMerge)
  END SUBROUTINE
  !
  MODULE PURE SUBROUTINE SORT_1D_MULTI_DSC2_INT64_CHAR(A, B, SORT_B, DESCEND, STABLE, P, QSORT, HEAP, ISORT, SymMerge)
    INTEGER(INT64), dimension(:), contiguous,    intent(inout) :: A           ! A and B must have the same dim
    CHARACTER(*),   dimension(:), contiguous,    intent(inout) :: B           !
    LOGICAL,                           optional, intent(in   ) :: SORT_B      ! If present and false, then only sort A, and permutation sort B; otherwise also sort B along repeated values in A
    LOGICAL,        dimension(2),                intent(in   ) :: DESCEND     ! Sort in Decending Order
    LOGICAL,                           optional, intent(in   ) :: STABLE      ! Use only Stable Sorting Methods
    INTEGER, dimension(:), contiguous, optional, intent(inout) :: P           ! Contains permutations to sort A (Sorts along with A)
    LOGICAL,                           optional, intent(in   ) :: QSORT       ! Use only Dual Pivot Quicksort/IntroSort
    LOGICAL,                           optional, intent(in   ) :: HEAP        ! Use only Heap Sort
    LOGICAL,                           optional, intent(in   ) :: ISORT       ! Use only Insertion Sort
    LOGICAL,                           optional, intent(in   ) :: SymMerge    ! Use only Stable Minimum Storage Merging by Symmetric Comparisons
    INTEGER, dimension(:), allocatable:: PNT
    INTEGER:: dim, sTYP
    LOGICAL:: SortB, HAS_P
    !
    dim = SIZE(A)
    if(dim > size(B)) return
    !
    HAS_P = present(P)
    !
    SortB = .TRUE.
    if(present(SORT_B)) SortB = SORT_B
    !
    sTYP = SORT_TYPE(dim, A, DESCEND(1), STABLE, QSORT, HEAP, ISORT, SymMerge)
    !
    if(sTYP == 0) then  ! A already sorted
        if(HAS_P) call INIT_PERMUTATION(dim, P)
        if(SortB) call SORT_B_by_A_and_P(dim, A, B, P, descend(2))
    else
        ALLOCATE(PNT(dim))
        !
        CALL SORT(A, DESCEND(1), P=PNT, SORT_TYPE=sTYP)
        !
        if(SortB) then
                  CALL SORT_B_by_A_and_P(dim, A, B, PNT, descend(2))
        else
                  CALL PERMUTATION_SORT(B, PNT)
        end if
        !
        if(HAS_P) P(1:dim) = PNT(1:dim)
        DEALLOCATE(PNT)
    end if
    !
  END SUBROUTINE 
  !
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !
  ! SORT_1D_MULTI_REL32_xyz
  !
  MODULE PURE SUBROUTINE SORT_1D_MULTI_DSC1_REL32_INT32(A, B, SORT_B, DESCEND, STABLE, P, QSORT, HEAP, ISORT, SymMerge)
    REAL(REL32),    dimension(:), contiguous,    intent(inout) :: A           ! A and B must have the same dim
    INTEGER(INT32), dimension(:), contiguous,    intent(inout) :: B           !
    LOGICAL,                           optional, intent(in   ) :: SORT_B      ! If present and false, then only sort A, and permutation sort B; otherwise also sort B along repeated values in A
    LOGICAL,                           optional, intent(in   ) :: DESCEND     ! Sort in Decending Order
    LOGICAL,                           optional, intent(in   ) :: STABLE      ! Use only Stable Sorting Methods
    INTEGER, dimension(:), contiguous, optional, intent(inout) :: P           ! Contains permutations to sort A (Sorts along with A)
    LOGICAL,                           optional, intent(in   ) :: QSORT       ! Use only Dual Pivot Quicksort/IntroSort
    LOGICAL,                           optional, intent(in   ) :: HEAP        ! Use only Heap Sort
    LOGICAL,                           optional, intent(in   ) :: ISORT       ! Use only Insertion Sort
    LOGICAL,                           optional, intent(in   ) :: SymMerge    ! Use only Stable Minimum Storage Merging by Symmetric Comparisons
    LOGICAL, dimension(2) :: DSCND
    IF(present(DESCEND)) then
        DSCND = DESCEND
    else
        DSCND = .FALSE.
    end if
    !
    CALL SORT_1D_MULTI_DSC2_REL32_INT32(A, B, SORT_B, DSCND, STABLE, P, QSORT, HEAP, ISORT, SymMerge)
  END SUBROUTINE
  !
  MODULE PURE SUBROUTINE SORT_1D_MULTI_DSC2_REL32_INT32(A, B, SORT_B, DESCEND, STABLE, P, QSORT, HEAP, ISORT, SymMerge)
    REAL(REL32),    dimension(:), contiguous,    intent(inout) :: A           ! A and B must have the same dim
    INTEGER(INT32), dimension(:), contiguous,    intent(inout) :: B           !
    LOGICAL,                           optional, intent(in   ) :: SORT_B      ! If present and false, then only sort A, and permutation sort B; otherwise also sort B along repeated values in A
    LOGICAL,        dimension(2),                intent(in   ) :: DESCEND     ! Sort in Decending Order
    LOGICAL,                           optional, intent(in   ) :: STABLE      ! Use only Stable Sorting Methods
    INTEGER, dimension(:), contiguous, optional, intent(inout) :: P           ! Contains permutations to sort A (Sorts along with A)
    LOGICAL,                           optional, intent(in   ) :: QSORT       ! Use only Dual Pivot Quicksort/IntroSort
    LOGICAL,                           optional, intent(in   ) :: HEAP        ! Use only Heap Sort
    LOGICAL,                           optional, intent(in   ) :: ISORT       ! Use only Insertion Sort
    LOGICAL,                           optional, intent(in   ) :: SymMerge    ! Use only Stable Minimum Storage Merging by Symmetric Comparisons
    INTEGER, dimension(:), allocatable:: PNT
    INTEGER:: dim, sTYP
    LOGICAL:: SortB, HAS_P
    !
    dim = SIZE(A)
    if(dim > size(B)) return
    !
    HAS_P = present(P)
    !
    SortB = .TRUE.
    if(present(SORT_B)) SortB = SORT_B
    !
    sTYP = SORT_TYPE(dim, A, DESCEND(1), STABLE, QSORT, HEAP, ISORT, SymMerge)
    !
    if(sTYP == 0) then  ! A already sorted
        if(HAS_P) call INIT_PERMUTATION(dim, P)
        if(SortB) call SORT_B_by_A_and_P(dim, A, B, P, descend(2))
    else
        ALLOCATE(PNT(dim))
        !
        CALL SORT(A, DESCEND(1), P=PNT, SORT_TYPE=sTYP)
        !
        if(SortB) then
                  CALL SORT_B_by_A_and_P(dim, A, B, PNT, descend(2))
        else
                  CALL PERMUTATION_SORT(B, PNT)
        end if
        !
        if(HAS_P) P(1:dim) = PNT(1:dim)
        DEALLOCATE(PNT)
    end if
    !
  END SUBROUTINE 
  !
  !--------------------------------------------------------------------------------------------------------------------------
  !
  MODULE PURE SUBROUTINE SORT_1D_MULTI_DSC1_REL32_INT64(A, B, SORT_B, DESCEND, STABLE, P, QSORT, HEAP, ISORT, SymMerge)
    REAL(REL32),    dimension(:), contiguous,    intent(inout) :: A           ! A and B must have the same dim
    INTEGER(INT64), dimension(:), contiguous,    intent(inout) :: B           !
    LOGICAL,                           optional, intent(in   ) :: SORT_B      ! If present and false, then only sort A, and permutation sort B; otherwise also sort B along repeated values in A
    LOGICAL,                           optional, intent(in   ) :: DESCEND     ! Sort in Decending Order
    LOGICAL,                           optional, intent(in   ) :: STABLE      ! Use only Stable Sorting Methods
    INTEGER, dimension(:), contiguous, optional, intent(inout) :: P           ! Contains permutations to sort A (Sorts along with A)
    LOGICAL,                           optional, intent(in   ) :: QSORT       ! Use only Dual Pivot Quicksort/IntroSort
    LOGICAL,                           optional, intent(in   ) :: HEAP        ! Use only Heap Sort
    LOGICAL,                           optional, intent(in   ) :: ISORT       ! Use only Insertion Sort
    LOGICAL,                           optional, intent(in   ) :: SymMerge    ! Use only Stable Minimum Storage Merging by Symmetric Comparisons
    LOGICAL, dimension(2) :: DSCND
    IF(present(DESCEND)) then
        DSCND = DESCEND
    else
        DSCND = .FALSE.
    end if
    !
    CALL SORT_1D_MULTI_DSC2_REL32_INT64(A, B, SORT_B, DSCND, STABLE, P, QSORT, HEAP, ISORT, SymMerge)
  END SUBROUTINE
  !
  MODULE PURE SUBROUTINE SORT_1D_MULTI_DSC2_REL32_INT64(A, B, SORT_B, DESCEND, STABLE, P, QSORT, HEAP, ISORT, SymMerge)
    REAL(REL32),    dimension(:), contiguous,    intent(inout) :: A           ! A and B must have the same dim
    INTEGER(INT64), dimension(:), contiguous,    intent(inout) :: B           !
    LOGICAL,                           optional, intent(in   ) :: SORT_B      ! If present and false, then only sort A, and permutation sort B; otherwise also sort B along repeated values in A
    LOGICAL,        dimension(2),                intent(in   ) :: DESCEND     ! Sort in Decending Order
    LOGICAL,                           optional, intent(in   ) :: STABLE      ! Use only Stable Sorting Methods
    INTEGER, dimension(:), contiguous, optional, intent(inout) :: P           ! Contains permutations to sort A (Sorts along with A)
    LOGICAL,                           optional, intent(in   ) :: QSORT       ! Use only Dual Pivot Quicksort/IntroSort
    LOGICAL,                           optional, intent(in   ) :: HEAP        ! Use only Heap Sort
    LOGICAL,                           optional, intent(in   ) :: ISORT       ! Use only Insertion Sort
    LOGICAL,                           optional, intent(in   ) :: SymMerge    ! Use only Stable Minimum Storage Merging by Symmetric Comparisons
    INTEGER, dimension(:), allocatable:: PNT
    INTEGER:: dim, sTYP
    LOGICAL:: SortB, HAS_P
    !
    dim = SIZE(A)
    if(dim > size(B)) return
    !
    HAS_P = present(P)
    !
    SortB = .TRUE.
    if(present(SORT_B)) SortB = SORT_B
    !
    sTYP = SORT_TYPE(dim, A, DESCEND(1), STABLE, QSORT, HEAP, ISORT, SymMerge)
    !
    if(sTYP == 0) then  ! A already sorted
        if(HAS_P) call INIT_PERMUTATION(dim, P)
        if(SortB) call SORT_B_by_A_and_P(dim, A, B, P, descend(2))
    else
        ALLOCATE(PNT(dim))
        !
        CALL SORT(A, DESCEND(1), P=PNT, SORT_TYPE=sTYP)
        !
        if(SortB) then
                  CALL SORT_B_by_A_and_P(dim, A, B, PNT, descend(2))
        else
                  CALL PERMUTATION_SORT(B, PNT)
        end if
        !
        if(HAS_P) P(1:dim) = PNT(1:dim)
        DEALLOCATE(PNT)
    end if
    !
  END SUBROUTINE 
  !
  !--------------------------------------------------------------------------------------------------------------------------
  !
  MODULE PURE SUBROUTINE SORT_1D_MULTI_DSC1_REL32_REL32(A, B, SORT_B, DESCEND, STABLE, P, QSORT, HEAP, ISORT, SymMerge)
    REAL(REL32),    dimension(:), contiguous,    intent(inout) :: A           ! A and B must have the same dim
    REAL(REL32),    dimension(:), contiguous,    intent(inout) :: B           !
    LOGICAL,                           optional, intent(in   ) :: SORT_B      ! If present and false, then only sort A, and permutation sort B; otherwise also sort B along repeated values in A
    LOGICAL,                           optional, intent(in   ) :: DESCEND     ! Sort in Decending Order
    LOGICAL,                           optional, intent(in   ) :: STABLE      ! Use only Stable Sorting Methods
    INTEGER, dimension(:), contiguous, optional, intent(inout) :: P           ! Contains permutations to sort A (Sorts along with A)
    LOGICAL,                           optional, intent(in   ) :: QSORT       ! Use only Dual Pivot Quicksort/IntroSort
    LOGICAL,                           optional, intent(in   ) :: HEAP        ! Use only Heap Sort
    LOGICAL,                           optional, intent(in   ) :: ISORT       ! Use only Insertion Sort
    LOGICAL,                           optional, intent(in   ) :: SymMerge    ! Use only Stable Minimum Storage Merging by Symmetric Comparisons
    LOGICAL, dimension(2) :: DSCND
    IF(present(DESCEND)) then
        DSCND = DESCEND
    else
        DSCND = .FALSE.
    end if
    !
    CALL SORT_1D_MULTI_DSC2_REL32_REL32(A, B, SORT_B, DSCND, STABLE, P, QSORT, HEAP, ISORT, SymMerge)
  END SUBROUTINE
  !
  MODULE PURE SUBROUTINE SORT_1D_MULTI_DSC2_REL32_REL32(A, B, SORT_B, DESCEND, STABLE, P, QSORT, HEAP, ISORT, SymMerge)
    REAL(REL32),    dimension(:), contiguous,    intent(inout) :: A           ! A and B must have the same dim
    REAL(REL32),    dimension(:), contiguous,    intent(inout) :: B           !
    LOGICAL,                           optional, intent(in   ) :: SORT_B      ! If present and false, then only sort A, and permutation sort B; otherwise also sort B along repeated values in A
    LOGICAL,        dimension(2),                intent(in   ) :: DESCEND     ! Sort in Decending Order
    LOGICAL,                           optional, intent(in   ) :: STABLE      ! Use only Stable Sorting Methods
    INTEGER, dimension(:), contiguous, optional, intent(inout) :: P           ! Contains permutations to sort A (Sorts along with A)
    LOGICAL,                           optional, intent(in   ) :: QSORT       ! Use only Dual Pivot Quicksort/IntroSort
    LOGICAL,                           optional, intent(in   ) :: HEAP        ! Use only Heap Sort
    LOGICAL,                           optional, intent(in   ) :: ISORT       ! Use only Insertion Sort
    LOGICAL,                           optional, intent(in   ) :: SymMerge    ! Use only Stable Minimum Storage Merging by Symmetric Comparisons
    INTEGER, dimension(:), allocatable:: PNT
    INTEGER:: dim, sTYP
    LOGICAL:: SortB, HAS_P
    !
    dim = SIZE(A)
    if(dim > size(B)) return
    !
    HAS_P = present(P)
    !
    SortB = .TRUE.
    if(present(SORT_B)) SortB = SORT_B
    !
    sTYP = SORT_TYPE(dim, A, DESCEND(1), STABLE, QSORT, HEAP, ISORT, SymMerge)
    !
    if(sTYP == 0) then  ! A already sorted
        if(HAS_P) call INIT_PERMUTATION(dim, P)
        if(SortB) call SORT_B_by_A_and_P(dim, A, B, P, descend(2))
    else
        ALLOCATE(PNT(dim))
        !
        CALL SORT(A, DESCEND(1), P=PNT, SORT_TYPE=sTYP)
        !
        if(SortB) then
                  CALL SORT_B_by_A_and_P(dim, A, B, PNT, descend(2))
        else
                  CALL PERMUTATION_SORT(B, PNT)
        end if
        !
        if(HAS_P) P(1:dim) = PNT(1:dim)
        DEALLOCATE(PNT)
    end if
    !
  END SUBROUTINE 
  !
  !--------------------------------------------------------------------------------------------------------------------------
  !
  MODULE PURE SUBROUTINE SORT_1D_MULTI_DSC1_REL32_REL64(A, B, SORT_B, DESCEND, STABLE, P, QSORT, HEAP, ISORT, SymMerge)
    REAL(REL32),    dimension(:), contiguous,    intent(inout) :: A           ! A and B must have the same dim
    REAL(REL64),    dimension(:), contiguous,    intent(inout) :: B           !
    LOGICAL,                           optional, intent(in   ) :: SORT_B      ! If present and false, then only sort A, and permutation sort B; otherwise also sort B along repeated values in A
    LOGICAL,                           optional, intent(in   ) :: DESCEND     ! Sort in Decending Order
    LOGICAL,                           optional, intent(in   ) :: STABLE      ! Use only Stable Sorting Methods
    INTEGER, dimension(:), contiguous, optional, intent(inout) :: P           ! Contains permutations to sort A (Sorts along with A)
    LOGICAL,                           optional, intent(in   ) :: QSORT       ! Use only Dual Pivot Quicksort/IntroSort
    LOGICAL,                           optional, intent(in   ) :: HEAP        ! Use only Heap Sort
    LOGICAL,                           optional, intent(in   ) :: ISORT       ! Use only Insertion Sort
    LOGICAL,                           optional, intent(in   ) :: SymMerge    ! Use only Stable Minimum Storage Merging by Symmetric Comparisons
    LOGICAL, dimension(2) :: DSCND
    IF(present(DESCEND)) then
        DSCND = DESCEND
    else
        DSCND = .FALSE.
    end if
    !
    CALL SORT_1D_MULTI_DSC2_REL32_REL64(A, B, SORT_B, DSCND, STABLE, P, QSORT, HEAP, ISORT, SymMerge)
  END SUBROUTINE
  !
  MODULE PURE SUBROUTINE SORT_1D_MULTI_DSC2_REL32_REL64(A, B, SORT_B, DESCEND, STABLE, P, QSORT, HEAP, ISORT, SymMerge)
    REAL(REL32),    dimension(:), contiguous,    intent(inout) :: A           ! A and B must have the same dim
    REAL(REL64),    dimension(:), contiguous,    intent(inout) :: B           !
    LOGICAL,                           optional, intent(in   ) :: SORT_B      ! If present and false, then only sort A, and permutation sort B; otherwise also sort B along repeated values in A
    LOGICAL,        dimension(2),                intent(in   ) :: DESCEND     ! Sort in Decending Order
    LOGICAL,                           optional, intent(in   ) :: STABLE      ! Use only Stable Sorting Methods
    INTEGER, dimension(:), contiguous, optional, intent(inout) :: P           ! Contains permutations to sort A (Sorts along with A)
    LOGICAL,                           optional, intent(in   ) :: QSORT       ! Use only Dual Pivot Quicksort/IntroSort
    LOGICAL,                           optional, intent(in   ) :: HEAP        ! Use only Heap Sort
    LOGICAL,                           optional, intent(in   ) :: ISORT       ! Use only Insertion Sort
    LOGICAL,                           optional, intent(in   ) :: SymMerge    ! Use only Stable Minimum Storage Merging by Symmetric Comparisons
    INTEGER, dimension(:), allocatable:: PNT
    INTEGER:: dim, sTYP
    LOGICAL:: SortB, HAS_P
    !
    dim = SIZE(A)
    if(dim > size(B)) return
    !
    HAS_P = present(P)
    !
    SortB = .TRUE.
    if(present(SORT_B)) SortB = SORT_B
    !
    sTYP = SORT_TYPE(dim, A, DESCEND(1), STABLE, QSORT, HEAP, ISORT, SymMerge)
    !
    if(sTYP == 0) then  ! A already sorted
        if(HAS_P) call INIT_PERMUTATION(dim, P)
        if(SortB) call SORT_B_by_A_and_P(dim, A, B, P, descend(2))
    else
        ALLOCATE(PNT(dim))
        !
        CALL SORT(A, DESCEND(1), P=PNT, SORT_TYPE=sTYP)
        !
        if(SortB) then
                  CALL SORT_B_by_A_and_P(dim, A, B, PNT, descend(2))
        else
                  CALL PERMUTATION_SORT(B, PNT)
        end if
        !
        if(HAS_P) P(1:dim) = PNT(1:dim)
        DEALLOCATE(PNT)
    end if
    !
  END SUBROUTINE 
  !
  !--------------------------------------------------------------------------------------------------------------------------
  !
  MODULE PURE SUBROUTINE SORT_1D_MULTI_DSC1_REL32_CHAR(A, B, SORT_B, DESCEND, STABLE, P, QSORT, HEAP, ISORT, SymMerge)
    REAL(REL32),    dimension(:), contiguous,    intent(inout) :: A           ! A and B must have the same dim
    CHARACTER(*),   dimension(:), contiguous,    intent(inout) :: B           !
    LOGICAL,                           optional, intent(in   ) :: SORT_B      ! If present and false, then only sort A, and permutation sort B; otherwise also sort B along repeated values in A
    LOGICAL,                           optional, intent(in   ) :: DESCEND     ! Sort in Decending Order
    LOGICAL,                           optional, intent(in   ) :: STABLE      ! Use only Stable Sorting Methods
    INTEGER, dimension(:), contiguous, optional, intent(inout) :: P           ! Contains permutations to sort A (Sorts along with A)
    LOGICAL,                           optional, intent(in   ) :: QSORT       ! Use only Dual Pivot Quicksort/IntroSort
    LOGICAL,                           optional, intent(in   ) :: HEAP        ! Use only Heap Sort
    LOGICAL,                           optional, intent(in   ) :: ISORT       ! Use only Insertion Sort
    LOGICAL,                           optional, intent(in   ) :: SymMerge    ! Use only Stable Minimum Storage Merging by Symmetric Comparisons
    LOGICAL, dimension(2) :: DSCND
    IF(present(DESCEND)) then
        DSCND = DESCEND
    else
        DSCND = .FALSE.
    end if
    !
    CALL SORT_1D_MULTI_DSC2_REL32_CHAR(A, B, SORT_B, DSCND, STABLE, P, QSORT, HEAP, ISORT, SymMerge)
  END SUBROUTINE
  !
  MODULE PURE SUBROUTINE SORT_1D_MULTI_DSC2_REL32_CHAR(A, B, SORT_B, DESCEND, STABLE, P, QSORT, HEAP, ISORT, SymMerge)
    REAL(REL32),    dimension(:), contiguous,    intent(inout) :: A           ! A and B must have the same dim
    CHARACTER(*),   dimension(:), contiguous,    intent(inout) :: B           !
    LOGICAL,                           optional, intent(in   ) :: SORT_B      ! If present and false, then only sort A, and permutation sort B; otherwise also sort B along repeated values in A
    LOGICAL,        dimension(2),                intent(in   ) :: DESCEND     ! Sort in Decending Order
    LOGICAL,                           optional, intent(in   ) :: STABLE      ! Use only Stable Sorting Methods
    INTEGER, dimension(:), contiguous, optional, intent(inout) :: P           ! Contains permutations to sort A (Sorts along with A)
    LOGICAL,                           optional, intent(in   ) :: QSORT       ! Use only Dual Pivot Quicksort/IntroSort
    LOGICAL,                           optional, intent(in   ) :: HEAP        ! Use only Heap Sort
    LOGICAL,                           optional, intent(in   ) :: ISORT       ! Use only Insertion Sort
    LOGICAL,                           optional, intent(in   ) :: SymMerge    ! Use only Stable Minimum Storage Merging by Symmetric Comparisons
    INTEGER, dimension(:), allocatable:: PNT
    INTEGER:: dim, sTYP
    LOGICAL:: SortB, HAS_P
    !
    dim = SIZE(A)
    if(dim > size(B)) return
    !
    HAS_P = present(P)
    !
    SortB = .TRUE.
    if(present(SORT_B)) SortB = SORT_B
    !
    sTYP = SORT_TYPE(dim, A, DESCEND(1), STABLE, QSORT, HEAP, ISORT, SymMerge)
    !
    if(sTYP == 0) then  ! A already sorted
        if(HAS_P) call INIT_PERMUTATION(dim, P)
        if(SortB) call SORT_B_by_A_and_P(dim, A, B, P, descend(2))
    else
        ALLOCATE(PNT(dim))
        !
        CALL SORT(A, DESCEND(1), P=PNT, SORT_TYPE=sTYP)
        !
        if(SortB) then
                  CALL SORT_B_by_A_and_P(dim, A, B, PNT, descend(2))
        else
                  CALL PERMUTATION_SORT(B, PNT)
        end if
        !
        if(HAS_P) P(1:dim) = PNT(1:dim)
        DEALLOCATE(PNT)
    end if
    !
  END SUBROUTINE 
  !
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !
  ! SORT_1D_MULTI_REL64_xyz
  !
  MODULE PURE SUBROUTINE SORT_1D_MULTI_DSC1_REL64_INT32(A, B, SORT_B, DESCEND, STABLE, P, QSORT, HEAP, ISORT, SymMerge)
    REAL(REL64),    dimension(:), contiguous,    intent(inout) :: A           ! A and B must have the same dim
    INTEGER(INT32), dimension(:), contiguous,    intent(inout) :: B           !
    LOGICAL,                           optional, intent(in   ) :: SORT_B      ! If present and false, then only sort A, and permutation sort B; otherwise also sort B along repeated values in A
    LOGICAL,                           optional, intent(in   ) :: DESCEND     ! Sort in Decending Order
    LOGICAL,                           optional, intent(in   ) :: STABLE      ! Use only Stable Sorting Methods
    INTEGER, dimension(:), contiguous, optional, intent(inout) :: P           ! Contains permutations to sort A (Sorts along with A)
    LOGICAL,                           optional, intent(in   ) :: QSORT       ! Use only Dual Pivot Quicksort/IntroSort
    LOGICAL,                           optional, intent(in   ) :: HEAP        ! Use only Heap Sort
    LOGICAL,                           optional, intent(in   ) :: ISORT       ! Use only Insertion Sort
    LOGICAL,                           optional, intent(in   ) :: SymMerge    ! Use only Stable Minimum Storage Merging by Symmetric Comparisons
    LOGICAL, dimension(2) :: DSCND
    IF(present(DESCEND)) then
        DSCND = DESCEND
    else
        DSCND = .FALSE.
    end if
    !
    CALL SORT_1D_MULTI_DSC2_REL64_INT32(A, B, SORT_B, DSCND, STABLE, P, QSORT, HEAP, ISORT, SymMerge)
  END SUBROUTINE
  !
  MODULE PURE SUBROUTINE SORT_1D_MULTI_DSC2_REL64_INT32(A, B, SORT_B, DESCEND, STABLE, P, QSORT, HEAP, ISORT, SymMerge)
    REAL(REL64),    dimension(:), contiguous,    intent(inout) :: A           ! A and B must have the same dim
    INTEGER(INT32), dimension(:), contiguous,    intent(inout) :: B           !
    LOGICAL,                           optional, intent(in   ) :: SORT_B      ! If present and false, then only sort A, and permutation sort B; otherwise also sort B along repeated values in A
    LOGICAL,        dimension(2),                intent(in   ) :: DESCEND     ! Sort in Decending Order
    LOGICAL,                           optional, intent(in   ) :: STABLE      ! Use only Stable Sorting Methods
    INTEGER, dimension(:), contiguous, optional, intent(inout) :: P           ! Contains permutations to sort A (Sorts along with A)
    LOGICAL,                           optional, intent(in   ) :: QSORT       ! Use only Dual Pivot Quicksort/IntroSort
    LOGICAL,                           optional, intent(in   ) :: HEAP        ! Use only Heap Sort
    LOGICAL,                           optional, intent(in   ) :: ISORT       ! Use only Insertion Sort
    LOGICAL,                           optional, intent(in   ) :: SymMerge    ! Use only Stable Minimum Storage Merging by Symmetric Comparisons
    INTEGER, dimension(:), allocatable:: PNT
    INTEGER:: dim, sTYP
    LOGICAL:: SortB, HAS_P
    !
    dim = SIZE(A)
    if(dim > size(B)) return
    !
    HAS_P = present(P)
    !
    SortB = .TRUE.
    if(present(SORT_B)) SortB = SORT_B
    !
    sTYP = SORT_TYPE(dim, A, DESCEND(1), STABLE, QSORT, HEAP, ISORT, SymMerge)
    !
    if(sTYP == 0) then  ! A already sorted
        if(HAS_P) call INIT_PERMUTATION(dim, P)
        if(SortB) call SORT_B_by_A_and_P(dim, A, B, P, descend(2))
    else
        ALLOCATE(PNT(dim))
        !
        CALL SORT(A, DESCEND(1), P=PNT, SORT_TYPE=sTYP)
        !
        if(SortB) then
                  CALL SORT_B_by_A_and_P(dim, A, B, PNT, descend(2))
        else
                  CALL PERMUTATION_SORT(B, PNT)
        end if
        !
        if(HAS_P) P(1:dim) = PNT(1:dim)
        DEALLOCATE(PNT)
    end if
    !
  END SUBROUTINE 
  !
  !--------------------------------------------------------------------------------------------------------------------------
  !
  MODULE PURE SUBROUTINE SORT_1D_MULTI_DSC1_REL64_INT64(A, B, SORT_B, DESCEND, STABLE, P, QSORT, HEAP, ISORT, SymMerge)
    REAL(REL64),    dimension(:), contiguous,    intent(inout) :: A           ! A and B must have the same dim
    INTEGER(INT64), dimension(:), contiguous,    intent(inout) :: B           !
    LOGICAL,                           optional, intent(in   ) :: SORT_B      ! If present and false, then only sort A, and permutation sort B; otherwise also sort B along repeated values in A
    LOGICAL,                           optional, intent(in   ) :: DESCEND     ! Sort in Decending Order
    LOGICAL,                           optional, intent(in   ) :: STABLE      ! Use only Stable Sorting Methods
    INTEGER, dimension(:), contiguous, optional, intent(inout) :: P           ! Contains permutations to sort A (Sorts along with A)
    LOGICAL,                           optional, intent(in   ) :: QSORT       ! Use only Dual Pivot Quicksort/IntroSort
    LOGICAL,                           optional, intent(in   ) :: HEAP        ! Use only Heap Sort
    LOGICAL,                           optional, intent(in   ) :: ISORT       ! Use only Insertion Sort
    LOGICAL,                           optional, intent(in   ) :: SymMerge    ! Use only Stable Minimum Storage Merging by Symmetric Comparisons
    LOGICAL, dimension(2) :: DSCND
    IF(present(DESCEND)) then
        DSCND = DESCEND
    else
        DSCND = .FALSE.
    end if
    !
    CALL SORT_1D_MULTI_DSC2_REL64_INT64(A, B, SORT_B, DSCND, STABLE, P, QSORT, HEAP, ISORT, SymMerge)
  END SUBROUTINE
  !
  MODULE PURE SUBROUTINE SORT_1D_MULTI_DSC2_REL64_INT64(A, B, SORT_B, DESCEND, STABLE, P, QSORT, HEAP, ISORT, SymMerge)
    REAL(REL64),    dimension(:), contiguous,    intent(inout) :: A           ! A and B must have the same dim
    INTEGER(INT64), dimension(:), contiguous,    intent(inout) :: B           !
    LOGICAL,                           optional, intent(in   ) :: SORT_B      ! If present and false, then only sort A, and permutation sort B; otherwise also sort B along repeated values in A
    LOGICAL,        dimension(2),                intent(in   ) :: DESCEND     ! Sort in Decending Order
    LOGICAL,                           optional, intent(in   ) :: STABLE      ! Use only Stable Sorting Methods
    INTEGER, dimension(:), contiguous, optional, intent(inout) :: P           ! Contains permutations to sort A (Sorts along with A)
    LOGICAL,                           optional, intent(in   ) :: QSORT       ! Use only Dual Pivot Quicksort/IntroSort
    LOGICAL,                           optional, intent(in   ) :: HEAP        ! Use only Heap Sort
    LOGICAL,                           optional, intent(in   ) :: ISORT       ! Use only Insertion Sort
    LOGICAL,                           optional, intent(in   ) :: SymMerge    ! Use only Stable Minimum Storage Merging by Symmetric Comparisons
    INTEGER, dimension(:), allocatable:: PNT
    INTEGER:: dim, sTYP
    LOGICAL:: SortB, HAS_P
    !
    dim = SIZE(A)
    if(dim > size(B)) return
    !
    HAS_P = present(P)
    !
    SortB = .TRUE.
    if(present(SORT_B)) SortB = SORT_B
    !
    sTYP = SORT_TYPE(dim, A, DESCEND(1), STABLE, QSORT, HEAP, ISORT, SymMerge)
    !
    if(sTYP == 0) then  ! A already sorted
        if(HAS_P) call INIT_PERMUTATION(dim, P)
        if(SortB) call SORT_B_by_A_and_P(dim, A, B, P, descend(2))
    else
        ALLOCATE(PNT(dim))
        !
        CALL SORT(A, DESCEND(1), P=PNT, SORT_TYPE=sTYP)
        !
        if(SortB) then
                  CALL SORT_B_by_A_and_P(dim, A, B, PNT, descend(2))
        else
                  CALL PERMUTATION_SORT(B, PNT)
        end if
        !
        if(HAS_P) P(1:dim) = PNT(1:dim)
        DEALLOCATE(PNT)
    end if
    !
  END SUBROUTINE 
  !
  !--------------------------------------------------------------------------------------------------------------------------
  !
  MODULE PURE SUBROUTINE SORT_1D_MULTI_DSC1_REL64_REL32(A, B, SORT_B, DESCEND, STABLE, P, QSORT, HEAP, ISORT, SymMerge)
    REAL(REL64),    dimension(:), contiguous,    intent(inout) :: A           ! A and B must have the same dim
    REAL(REL32),    dimension(:), contiguous,    intent(inout) :: B           !
    LOGICAL,                           optional, intent(in   ) :: SORT_B      ! If present and false, then only sort A, and permutation sort B; otherwise also sort B along repeated values in A
    LOGICAL,                           optional, intent(in   ) :: DESCEND     ! Sort in Decending Order
    LOGICAL,                           optional, intent(in   ) :: STABLE      ! Use only Stable Sorting Methods
    INTEGER, dimension(:), contiguous, optional, intent(inout) :: P           ! Contains permutations to sort A (Sorts along with A)
    LOGICAL,                           optional, intent(in   ) :: QSORT       ! Use only Dual Pivot Quicksort/IntroSort
    LOGICAL,                           optional, intent(in   ) :: HEAP        ! Use only Heap Sort
    LOGICAL,                           optional, intent(in   ) :: ISORT       ! Use only Insertion Sort
    LOGICAL,                           optional, intent(in   ) :: SymMerge    ! Use only Stable Minimum Storage Merging by Symmetric Comparisons
    LOGICAL, dimension(2) :: DSCND
    IF(present(DESCEND)) then
        DSCND = DESCEND
    else
        DSCND = .FALSE.
    end if
    !
    CALL SORT_1D_MULTI_DSC2_REL64_REL32(A, B, SORT_B, DSCND, STABLE, P, QSORT, HEAP, ISORT, SymMerge)
  END SUBROUTINE
  !
  MODULE PURE SUBROUTINE SORT_1D_MULTI_DSC2_REL64_REL32(A, B, SORT_B, DESCEND, STABLE, P, QSORT, HEAP, ISORT, SymMerge)
    REAL(REL64),    dimension(:), contiguous,    intent(inout) :: A           ! A and B must have the same dim
    REAL(REL32),    dimension(:), contiguous,    intent(inout) :: B           !
    LOGICAL,                           optional, intent(in   ) :: SORT_B      ! If present and false, then only sort A, and permutation sort B; otherwise also sort B along repeated values in A
    LOGICAL,        dimension(2),                intent(in   ) :: DESCEND     ! Sort in Decending Order
    LOGICAL,                           optional, intent(in   ) :: STABLE      ! Use only Stable Sorting Methods
    INTEGER, dimension(:), contiguous, optional, intent(inout) :: P           ! Contains permutations to sort A (Sorts along with A)
    LOGICAL,                           optional, intent(in   ) :: QSORT       ! Use only Dual Pivot Quicksort/IntroSort
    LOGICAL,                           optional, intent(in   ) :: HEAP        ! Use only Heap Sort
    LOGICAL,                           optional, intent(in   ) :: ISORT       ! Use only Insertion Sort
    LOGICAL,                           optional, intent(in   ) :: SymMerge    ! Use only Stable Minimum Storage Merging by Symmetric Comparisons
    INTEGER, dimension(:), allocatable:: PNT
    INTEGER:: dim, sTYP
    LOGICAL:: SortB, HAS_P
    !
    dim = SIZE(A)
    if(dim > size(B)) return
    !
    HAS_P = present(P)
    !
    SortB = .TRUE.
    if(present(SORT_B)) SortB = SORT_B
    !
    sTYP = SORT_TYPE(dim, A, DESCEND(1), STABLE, QSORT, HEAP, ISORT, SymMerge)
    !
    if(sTYP == 0) then  ! A already sorted
        if(HAS_P) call INIT_PERMUTATION(dim, P)
        if(SortB) call SORT_B_by_A_and_P(dim, A, B, P, descend(2))
    else
        ALLOCATE(PNT(dim))
        !
        CALL SORT(A, DESCEND(1), P=PNT, SORT_TYPE=sTYP)
        !
        if(SortB) then
                  CALL SORT_B_by_A_and_P(dim, A, B, PNT, descend(2))
        else
                  CALL PERMUTATION_SORT(B, PNT)
        end if
        !
        if(HAS_P) P(1:dim) = PNT(1:dim)
        DEALLOCATE(PNT)
    end if
    !
  END SUBROUTINE 
  !
  !--------------------------------------------------------------------------------------------------------------------------
  !
  MODULE PURE SUBROUTINE SORT_1D_MULTI_DSC1_REL64_REL64(A, B, SORT_B, DESCEND, STABLE, P, QSORT, HEAP, ISORT, SymMerge)
    REAL(REL64),    dimension(:), contiguous,    intent(inout) :: A           ! A and B must have the same dim
    REAL(REL64),    dimension(:), contiguous,    intent(inout) :: B           !
    LOGICAL,                           optional, intent(in   ) :: SORT_B      ! If present and false, then only sort A, and permutation sort B; otherwise also sort B along repeated values in A
    LOGICAL,                           optional, intent(in   ) :: DESCEND     ! Sort in Decending Order
    LOGICAL,                           optional, intent(in   ) :: STABLE      ! Use only Stable Sorting Methods
    INTEGER, dimension(:), contiguous, optional, intent(inout) :: P           ! Contains permutations to sort A (Sorts along with A)
    LOGICAL,                           optional, intent(in   ) :: QSORT       ! Use only Dual Pivot Quicksort/IntroSort
    LOGICAL,                           optional, intent(in   ) :: HEAP        ! Use only Heap Sort
    LOGICAL,                           optional, intent(in   ) :: ISORT       ! Use only Insertion Sort
    LOGICAL,                           optional, intent(in   ) :: SymMerge    ! Use only Stable Minimum Storage Merging by Symmetric Comparisons
    LOGICAL, dimension(2) :: DSCND
    IF(present(DESCEND)) then
        DSCND = DESCEND
    else
        DSCND = .FALSE.
    end if
    !
    CALL SORT_1D_MULTI_DSC2_REL64_REL64(A, B, SORT_B, DSCND, STABLE, P, QSORT, HEAP, ISORT, SymMerge)
  END SUBROUTINE
  !
  MODULE PURE SUBROUTINE SORT_1D_MULTI_DSC2_REL64_REL64(A, B, SORT_B, DESCEND, STABLE, P, QSORT, HEAP, ISORT, SymMerge)
    REAL(REL64),    dimension(:), contiguous,    intent(inout) :: A           ! A and B must have the same dim
    REAL(REL64),    dimension(:), contiguous,    intent(inout) :: B           !
    LOGICAL,                           optional, intent(in   ) :: SORT_B      ! If present and false, then only sort A, and permutation sort B; otherwise also sort B along repeated values in A
    LOGICAL,        dimension(2),                intent(in   ) :: DESCEND     ! Sort in Decending Order
    LOGICAL,                           optional, intent(in   ) :: STABLE      ! Use only Stable Sorting Methods
    INTEGER, dimension(:), contiguous, optional, intent(inout) :: P           ! Contains permutations to sort A (Sorts along with A)
    LOGICAL,                           optional, intent(in   ) :: QSORT       ! Use only Dual Pivot Quicksort/IntroSort
    LOGICAL,                           optional, intent(in   ) :: HEAP        ! Use only Heap Sort
    LOGICAL,                           optional, intent(in   ) :: ISORT       ! Use only Insertion Sort
    LOGICAL,                           optional, intent(in   ) :: SymMerge    ! Use only Stable Minimum Storage Merging by Symmetric Comparisons
    INTEGER, dimension(:), allocatable:: PNT
    INTEGER:: dim, sTYP
    LOGICAL:: SortB, HAS_P
    !
    dim = SIZE(A)
    if(dim > size(B)) return
    !
    HAS_P = present(P)
    !
    SortB = .TRUE.
    if(present(SORT_B)) SortB = SORT_B
    !
    sTYP = SORT_TYPE(dim, A, DESCEND(1), STABLE, QSORT, HEAP, ISORT, SymMerge)
    !
    if(sTYP == 0) then  ! A already sorted
        if(HAS_P) call INIT_PERMUTATION(dim, P)
        if(SortB) call SORT_B_by_A_and_P(dim, A, B, P, descend(2))
    else
        ALLOCATE(PNT(dim))
        !
        CALL SORT(A, DESCEND(1), P=PNT, SORT_TYPE=sTYP)
        !
        if(SortB) then
                  CALL SORT_B_by_A_and_P(dim, A, B, PNT, descend(2))
        else
                  CALL PERMUTATION_SORT(B, PNT)
        end if
        !
        if(HAS_P) P(1:dim) = PNT(1:dim)
        DEALLOCATE(PNT)
    end if
    !
  END SUBROUTINE 
  !
  !--------------------------------------------------------------------------------------------------------------------------
  !
  MODULE PURE SUBROUTINE SORT_1D_MULTI_DSC1_REL64_CHAR(A, B, SORT_B, DESCEND, STABLE, P, QSORT, HEAP, ISORT, SymMerge)
    REAL(REL64),    dimension(:), contiguous,    intent(inout) :: A           ! A and B must have the same dim
    CHARACTER(*),   dimension(:), contiguous,    intent(inout) :: B           !
    LOGICAL,                           optional, intent(in   ) :: SORT_B      ! If present and false, then only sort A, and permutation sort B; otherwise also sort B along repeated values in A
    LOGICAL,                           optional, intent(in   ) :: DESCEND     ! Sort in Decending Order
    LOGICAL,                           optional, intent(in   ) :: STABLE      ! Use only Stable Sorting Methods
    INTEGER, dimension(:), contiguous, optional, intent(inout) :: P           ! Contains permutations to sort A (Sorts along with A)
    LOGICAL,                           optional, intent(in   ) :: QSORT       ! Use only Dual Pivot Quicksort/IntroSort
    LOGICAL,                           optional, intent(in   ) :: HEAP        ! Use only Heap Sort
    LOGICAL,                           optional, intent(in   ) :: ISORT       ! Use only Insertion Sort
    LOGICAL,                           optional, intent(in   ) :: SymMerge    ! Use only Stable Minimum Storage Merging by Symmetric Comparisons
    LOGICAL, dimension(2) :: DSCND
    IF(present(DESCEND)) then
        DSCND = DESCEND
    else
        DSCND = .FALSE.
    end if
    !
    CALL SORT_1D_MULTI_DSC2_REL64_CHAR(A, B, SORT_B, DSCND, STABLE, P, QSORT, HEAP, ISORT, SymMerge)
  END SUBROUTINE
  !
  MODULE PURE SUBROUTINE SORT_1D_MULTI_DSC2_REL64_CHAR(A, B, SORT_B, DESCEND, STABLE, P, QSORT, HEAP, ISORT, SymMerge)
    REAL(REL64),    dimension(:), contiguous,    intent(inout) :: A           ! A and B must have the same dim
    CHARACTER(*),   dimension(:), contiguous,    intent(inout) :: B           !
    LOGICAL,                           optional, intent(in   ) :: SORT_B      ! If present and false, then only sort A, and permutation sort B; otherwise also sort B along repeated values in A
    LOGICAL,        dimension(2),                intent(in   ) :: DESCEND     ! Sort in Decending Order
    LOGICAL,                           optional, intent(in   ) :: STABLE      ! Use only Stable Sorting Methods
    INTEGER, dimension(:), contiguous, optional, intent(inout) :: P           ! Contains permutations to sort A (Sorts along with A)
    LOGICAL,                           optional, intent(in   ) :: QSORT       ! Use only Dual Pivot Quicksort/IntroSort
    LOGICAL,                           optional, intent(in   ) :: HEAP        ! Use only Heap Sort
    LOGICAL,                           optional, intent(in   ) :: ISORT       ! Use only Insertion Sort
    LOGICAL,                           optional, intent(in   ) :: SymMerge    ! Use only Stable Minimum Storage Merging by Symmetric Comparisons
    INTEGER, dimension(:), allocatable:: PNT
    INTEGER:: dim, sTYP
    LOGICAL:: SortB, HAS_P
    !
    dim = SIZE(A)
    if(dim > size(B)) return
    !
    HAS_P = present(P)
    !
    SortB = .TRUE.
    if(present(SORT_B)) SortB = SORT_B
    !
    sTYP = SORT_TYPE(dim, A, DESCEND(1), STABLE, QSORT, HEAP, ISORT, SymMerge)
    !
    if(sTYP == 0) then  ! A already sorted
        if(HAS_P) call INIT_PERMUTATION(dim, P)
        if(SortB) call SORT_B_by_A_and_P(dim, A, B, P, descend(2))
    else
        ALLOCATE(PNT(dim))
        !
        CALL SORT(A, DESCEND(1), P=PNT, SORT_TYPE=sTYP)
        !
        if(SortB) then
                  CALL SORT_B_by_A_and_P(dim, A, B, PNT, descend(2))
        else
                  CALL PERMUTATION_SORT(B, PNT)
        end if
        !
        if(HAS_P) P(1:dim) = PNT(1:dim)
        DEALLOCATE(PNT)
    end if
    !
  END SUBROUTINE 
  !
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !
  ! SORT_1D_MULTI_CHAR_xyz
  !
  MODULE PURE SUBROUTINE SORT_1D_MULTI_DSC1_CHAR_INT32(A, B, SORT_B, DESCEND, STABLE, P, QSORT, HEAP, ISORT, SymMerge, nat, cap)
    CHARACTER(*),   dimension(:), contiguous,    intent(inout) :: A           ! A and B must have the same dim
    INTEGER(INT32), dimension(:), contiguous,    intent(inout) :: B           !
    LOGICAL,                           optional, intent(in   ) :: SORT_B      ! If present and false, then only sort A, and permutation sort B; otherwise also sort B along repeated values in A
    LOGICAL,                           optional, intent(in   ) :: DESCEND     ! Sort in Decending Order
    LOGICAL,                           optional, intent(in   ) :: STABLE      ! Use only Stable Sorting Methods
    INTEGER, dimension(:), contiguous, optional, intent(inout) :: P           ! Contains permutations to sort A (Sorts along with A)
    LOGICAL,                           optional, intent(in   ) :: QSORT       ! Use only Dual Pivot Quicksort/IntroSort
    LOGICAL,                           optional, intent(in   ) :: HEAP        ! Use only Heap Sort
    LOGICAL,                           optional, intent(in   ) :: ISORT       ! Use only Insertion Sort
    LOGICAL,                           optional, intent(in   ) :: SymMerge    ! Use only Stable Minimum Storage Merging by Symmetric Comparisons
    LOGICAL,                           optional, intent(in   ) :: nat         ! If true, then due a natural sort of strings
    LOGICAL,                           optional, intent(in   ) :: cap         ! If true, then do case insensitive sort of strings
    LOGICAL, dimension(2) :: DSCND
    IF(present(DESCEND)) then
        DSCND = DESCEND
    else
        DSCND = .FALSE.
    end if
    !
    CALL SORT_1D_MULTI_DSC2_CHAR_INT32(A, B, SORT_B, DSCND, STABLE, P, QSORT, HEAP, ISORT, SymMerge, nat, cap)
  END SUBROUTINE
  !
  MODULE PURE SUBROUTINE SORT_1D_MULTI_DSC2_CHAR_INT32(A, B, SORT_B, DESCEND, STABLE, P, QSORT, HEAP, ISORT, SymMerge, nat, cap)
    CHARACTER(*),   dimension(:), contiguous,    intent(inout) :: A           ! A and B must have the same dim
    INTEGER(INT32), dimension(:), contiguous,    intent(inout) :: B           !
    LOGICAL,                           optional, intent(in   ) :: SORT_B      ! If present and false, then only sort A, and permutation sort B; otherwise also sort B along repeated values in A
    LOGICAL,        dimension(2),                intent(in   ) :: DESCEND     ! Sort in Decending Order
    LOGICAL,                           optional, intent(in   ) :: STABLE      ! Use only Stable Sorting Methods
    INTEGER, dimension(:), contiguous, optional, intent(inout) :: P           ! Contains permutations to sort A (Sorts along with A)
    LOGICAL,                           optional, intent(in   ) :: QSORT       ! Use only Dual Pivot Quicksort/IntroSort
    LOGICAL,                           optional, intent(in   ) :: HEAP        ! Use only Heap Sort
    LOGICAL,                           optional, intent(in   ) :: ISORT       ! Use only Insertion Sort
    LOGICAL,                           optional, intent(in   ) :: SymMerge    ! Use only Stable Minimum Storage Merging by Symmetric Comparisons
    LOGICAL,                           optional, intent(in   ) :: nat         ! If true, then due a natural sort of strings
    LOGICAL,                           optional, intent(in   ) :: cap         ! If true, then do case insensitive sort of strings
    INTEGER, dimension(:), allocatable:: PNT
    INTEGER:: dim, sTYP
    LOGICAL:: SortB, HAS_P
    !
    dim = SIZE(A)
    if(dim > size(B)) return
    !
    HAS_P = present(P)
    !
    SortB = .TRUE.
    if(present(SORT_B)) SortB = SORT_B
    !
    sTYP = SORT_TYPE(dim, A, DESCEND(1), STABLE, QSORT, HEAP, ISORT, SymMerge, nat, cap)
    !
    if(sTYP == 0) then  ! A already sorted
        if(HAS_P) call INIT_PERMUTATION(dim, P)
        if(SortB) call SORT_B_by_A_and_P(dim, A, B, P, descend(2))
    else
        ALLOCATE(PNT(dim))
        !
        CALL SORT(A, DESCEND(1), P=PNT, SORT_TYPE=sTYP, nat=nat, cap=cap)
        !
        if(SortB) then
                  CALL SORT_B_by_A_and_P(dim, A, B, PNT, descend(2))
        else
                  CALL PERMUTATION_SORT(B, PNT)
        end if
        !
        if(HAS_P) P(1:dim) = PNT(1:dim)
        DEALLOCATE(PNT)
    end if
    !
  END SUBROUTINE 
  !
  !--------------------------------------------------------------------------------------------------------------------------
  !
  MODULE PURE SUBROUTINE SORT_1D_MULTI_DSC1_CHAR_INT64(A, B, SORT_B, DESCEND, STABLE, P, QSORT, HEAP, ISORT, SymMerge, nat, cap)
    CHARACTER(*),   dimension(:), contiguous,    intent(inout) :: A           ! A and B must have the same dim
    INTEGER(INT64), dimension(:), contiguous,    intent(inout) :: B           !
    LOGICAL,                           optional, intent(in   ) :: SORT_B      ! If present and false, then only sort A, and permutation sort B; otherwise also sort B along repeated values in A
    LOGICAL,                           optional, intent(in   ) :: DESCEND     ! Sort in Decending Order
    LOGICAL,                           optional, intent(in   ) :: STABLE      ! Use only Stable Sorting Methods
    INTEGER, dimension(:), contiguous, optional, intent(inout) :: P           ! Contains permutations to sort A (Sorts along with A)
    LOGICAL,                           optional, intent(in   ) :: QSORT       ! Use only Dual Pivot Quicksort/IntroSort
    LOGICAL,                           optional, intent(in   ) :: HEAP        ! Use only Heap Sort
    LOGICAL,                           optional, intent(in   ) :: ISORT       ! Use only Insertion Sort
    LOGICAL,                           optional, intent(in   ) :: SymMerge    ! Use only Stable Minimum Storage Merging by Symmetric Comparisons
    LOGICAL,                           optional, intent(in   ) :: nat         ! If true, then due a natural sort of strings
    LOGICAL,                           optional, intent(in   ) :: cap         ! If true, then do case insensitive sort of strings
    LOGICAL, dimension(2) :: DSCND
    IF(present(DESCEND)) then
        DSCND = DESCEND
    else
        DSCND = .FALSE.
    end if
    !
    CALL SORT_1D_MULTI_DSC2_CHAR_INT64(A, B, SORT_B, DSCND, STABLE, P, QSORT, HEAP, ISORT, SymMerge, nat, cap)
  END SUBROUTINE
  !
  MODULE PURE SUBROUTINE SORT_1D_MULTI_DSC2_CHAR_INT64(A, B, SORT_B, DESCEND, STABLE, P, QSORT, HEAP, ISORT, SymMerge, nat, cap)
    CHARACTER(*),   dimension(:), contiguous,    intent(inout) :: A           ! A and B must have the same dim
    INTEGER(INT64), dimension(:), contiguous,    intent(inout) :: B           !
    LOGICAL,                           optional, intent(in   ) :: SORT_B      ! If present and false, then only sort A, and permutation sort B; otherwise also sort B along repeated values in A
    LOGICAL,        dimension(2),                intent(in   ) :: DESCEND     ! Sort in Decending Order
    LOGICAL,                           optional, intent(in   ) :: STABLE      ! Use only Stable Sorting Methods
    INTEGER, dimension(:), contiguous, optional, intent(inout) :: P           ! Contains permutations to sort A (Sorts along with A)
    LOGICAL,                           optional, intent(in   ) :: QSORT       ! Use only Dual Pivot Quicksort/IntroSort
    LOGICAL,                           optional, intent(in   ) :: HEAP        ! Use only Heap Sort
    LOGICAL,                           optional, intent(in   ) :: ISORT       ! Use only Insertion Sort
    LOGICAL,                           optional, intent(in   ) :: SymMerge    ! Use only Stable Minimum Storage Merging by Symmetric Comparisons
    LOGICAL,                           optional, intent(in   ) :: nat         ! If true, then due a natural sort of strings
    LOGICAL,                           optional, intent(in   ) :: cap         ! If true, then do case insensitive sort of strings
    INTEGER, dimension(:), allocatable:: PNT
    INTEGER:: dim, sTYP
    LOGICAL:: SortB, HAS_P
    !
    dim = SIZE(A)
    if(dim > size(B)) return
    !
    HAS_P = present(P)
    !
    SortB = .TRUE.
    if(present(SORT_B)) SortB = SORT_B
    !
    sTYP = SORT_TYPE(dim, A, DESCEND(1), STABLE, QSORT, HEAP, ISORT, SymMerge, nat, cap)
    !
    if(sTYP == 0) then  ! A already sorted
        if(HAS_P) call INIT_PERMUTATION(dim, P)
        if(SortB) call SORT_B_by_A_and_P(dim, A, B, P, descend(2))
    else
        ALLOCATE(PNT(dim))
        !
        CALL SORT(A, DESCEND(1), P=PNT, SORT_TYPE=sTYP, nat=nat, cap=cap)
        !
        if(SortB) then
                  CALL SORT_B_by_A_and_P(dim, A, B, PNT, descend(2))
        else
                  CALL PERMUTATION_SORT(B, PNT)
        end if
        !
        if(HAS_P) P(1:dim) = PNT(1:dim)
        DEALLOCATE(PNT)
    end if
    !
  END SUBROUTINE 
  !
  !--------------------------------------------------------------------------------------------------------------------------
  !
  MODULE PURE SUBROUTINE SORT_1D_MULTI_DSC1_CHAR_REL32(A, B, SORT_B, DESCEND, STABLE, P, QSORT, HEAP, ISORT, SymMerge, nat, cap)
    CHARACTER(*),   dimension(:), contiguous,    intent(inout) :: A           ! A and B must have the same dim
    REAL(REL32),    dimension(:), contiguous,    intent(inout) :: B           !
    LOGICAL,                           optional, intent(in   ) :: SORT_B      ! If present and false, then only sort A, and permutation sort B; otherwise also sort B along repeated values in A
    LOGICAL,                           optional, intent(in   ) :: DESCEND     ! Sort in Decending Order
    LOGICAL,                           optional, intent(in   ) :: STABLE      ! Use only Stable Sorting Methods
    INTEGER, dimension(:), contiguous, optional, intent(inout) :: P           ! Contains permutations to sort A (Sorts along with A)
    LOGICAL,                           optional, intent(in   ) :: QSORT       ! Use only Dual Pivot Quicksort/IntroSort
    LOGICAL,                           optional, intent(in   ) :: HEAP        ! Use only Heap Sort
    LOGICAL,                           optional, intent(in   ) :: ISORT       ! Use only Insertion Sort
    LOGICAL,                           optional, intent(in   ) :: SymMerge    ! Use only Stable Minimum Storage Merging by Symmetric Comparisons
    LOGICAL,                           optional, intent(in   ) :: nat         ! If true, then due a natural sort of strings
    LOGICAL,                           optional, intent(in   ) :: cap         ! If true, then do case insensitive sort of strings
    LOGICAL, dimension(2) :: DSCND
    IF(present(DESCEND)) then
        DSCND = DESCEND
    else
        DSCND = .FALSE.
    end if
    !
    CALL SORT_1D_MULTI_DSC2_CHAR_REL32(A, B, SORT_B, DSCND, STABLE, P, QSORT, HEAP, ISORT, SymMerge, nat, cap)
  END SUBROUTINE
  !
  MODULE PURE SUBROUTINE SORT_1D_MULTI_DSC2_CHAR_REL32(A, B, SORT_B, DESCEND, STABLE, P, QSORT, HEAP, ISORT, SymMerge, nat, cap)
    CHARACTER(*),   dimension(:), contiguous,    intent(inout) :: A           ! A and B must have the same dim
    REAL(REL32),    dimension(:), contiguous,    intent(inout) :: B           !
    LOGICAL,                           optional, intent(in   ) :: SORT_B      ! If present and false, then only sort A, and permutation sort B; otherwise also sort B along repeated values in A
    LOGICAL,        dimension(2),                intent(in   ) :: DESCEND     ! Sort in Decending Order
    LOGICAL,                           optional, intent(in   ) :: STABLE      ! Use only Stable Sorting Methods
    INTEGER, dimension(:), contiguous, optional, intent(inout) :: P           ! Contains permutations to sort A (Sorts along with A)
    LOGICAL,                           optional, intent(in   ) :: QSORT       ! Use only Dual Pivot Quicksort/IntroSort
    LOGICAL,                           optional, intent(in   ) :: HEAP        ! Use only Heap Sort
    LOGICAL,                           optional, intent(in   ) :: ISORT       ! Use only Insertion Sort
    LOGICAL,                           optional, intent(in   ) :: SymMerge    ! Use only Stable Minimum Storage Merging by Symmetric Comparisons
    LOGICAL,                           optional, intent(in   ) :: nat         ! If true, then due a natural sort of strings
    LOGICAL,                           optional, intent(in   ) :: cap         ! If true, then do case insensitive sort of strings
    INTEGER, dimension(:), allocatable:: PNT
    INTEGER:: dim, sTYP
    LOGICAL:: SortB, HAS_P
    !
    dim = SIZE(A)
    if(dim > size(B)) return
    !
    HAS_P = present(P)
    !
    SortB = .TRUE.
    if(present(SORT_B)) SortB = SORT_B
    !
    sTYP = SORT_TYPE(dim, A, DESCEND(1), STABLE, QSORT, HEAP, ISORT, SymMerge, nat, cap)
    !
    if(sTYP == 0) then  ! A already sorted
        if(HAS_P) call INIT_PERMUTATION(dim, P)
        if(SortB) call SORT_B_by_A_and_P(dim, A, B, P, descend(2))
    else
        ALLOCATE(PNT(dim))
        !
        CALL SORT(A, DESCEND(1), P=PNT, SORT_TYPE=sTYP, nat=nat, cap=cap)
        !
        if(SortB) then
                  CALL SORT_B_by_A_and_P(dim, A, B, PNT, descend(2))
        else
                  CALL PERMUTATION_SORT(B, PNT)
        end if
        !
        if(HAS_P) P(1:dim) = PNT(1:dim)
        DEALLOCATE(PNT)
    end if
    !
  END SUBROUTINE 
  !
  !--------------------------------------------------------------------------------------------------------------------------
  !
  MODULE PURE SUBROUTINE SORT_1D_MULTI_DSC1_CHAR_REL64(A, B, SORT_B, DESCEND, STABLE, P, QSORT, HEAP, ISORT, SymMerge, nat, cap)
    CHARACTER(*),   dimension(:), contiguous,    intent(inout) :: A           ! A and B must have the same dim
    REAL(REL64),    dimension(:), contiguous,    intent(inout) :: B           !
    LOGICAL,                           optional, intent(in   ) :: SORT_B      ! If present and false, then only sort A, and permutation sort B; otherwise also sort B along repeated values in A
    LOGICAL,                           optional, intent(in   ) :: DESCEND     ! Sort in Decending Order
    LOGICAL,                           optional, intent(in   ) :: STABLE      ! Use only Stable Sorting Methods
    INTEGER, dimension(:), contiguous, optional, intent(inout) :: P           ! Contains permutations to sort A (Sorts along with A)
    LOGICAL,                           optional, intent(in   ) :: QSORT       ! Use only Dual Pivot Quicksort/IntroSort
    LOGICAL,                           optional, intent(in   ) :: HEAP        ! Use only Heap Sort
    LOGICAL,                           optional, intent(in   ) :: ISORT       ! Use only Insertion Sort
    LOGICAL,                           optional, intent(in   ) :: SymMerge    ! Use only Stable Minimum Storage Merging by Symmetric Comparisons
    LOGICAL,                           optional, intent(in   ) :: nat         ! If true, then due a natural sort of strings
    LOGICAL,                           optional, intent(in   ) :: cap         ! If true, then do case insensitive sort of strings
    LOGICAL, dimension(2) :: DSCND
    IF(present(DESCEND)) then
        DSCND = DESCEND
    else
        DSCND = .FALSE.
    end if
    !
    CALL SORT_1D_MULTI_DSC2_CHAR_REL64(A, B, SORT_B, DSCND, STABLE, P, QSORT, HEAP, ISORT, SymMerge, nat, cap)
  END SUBROUTINE
  !
  MODULE PURE SUBROUTINE SORT_1D_MULTI_DSC2_CHAR_REL64(A, B, SORT_B, DESCEND, STABLE, P, QSORT, HEAP, ISORT, SymMerge, nat, cap)
    CHARACTER(*),   dimension(:), contiguous,    intent(inout) :: A           ! A and B must have the same dim
    REAL(REL64),    dimension(:), contiguous,    intent(inout) :: B           !
    LOGICAL,                           optional, intent(in   ) :: SORT_B      ! If present and false, then only sort A, and permutation sort B; otherwise also sort B along repeated values in A
    LOGICAL,        dimension(2),                intent(in   ) :: DESCEND     ! Sort in Decending Order
    LOGICAL,                           optional, intent(in   ) :: STABLE      ! Use only Stable Sorting Methods
    INTEGER, dimension(:), contiguous, optional, intent(inout) :: P           ! Contains permutations to sort A (Sorts along with A)
    LOGICAL,                           optional, intent(in   ) :: QSORT       ! Use only Dual Pivot Quicksort/IntroSort
    LOGICAL,                           optional, intent(in   ) :: HEAP        ! Use only Heap Sort
    LOGICAL,                           optional, intent(in   ) :: ISORT       ! Use only Insertion Sort
    LOGICAL,                           optional, intent(in   ) :: SymMerge    ! Use only Stable Minimum Storage Merging by Symmetric Comparisons
    LOGICAL,                           optional, intent(in   ) :: nat         ! If true, then due a natural sort of strings
    LOGICAL,                           optional, intent(in   ) :: cap         ! If true, then do case insensitive sort of strings
    INTEGER, dimension(:), allocatable:: PNT
    INTEGER:: dim, sTYP
    LOGICAL:: SortB, HAS_P
    !
    dim = SIZE(A)
    if(dim > size(B)) return
    !
    HAS_P = present(P)
    !
    SortB = .TRUE.
    if(present(SORT_B)) SortB = SORT_B
    !
    sTYP = SORT_TYPE(dim, A, DESCEND(1), STABLE, QSORT, HEAP, ISORT, SymMerge, nat, cap)
    !
    if(sTYP == 0) then  ! A already sorted
        if(HAS_P) call INIT_PERMUTATION(dim, P)
        if(SortB) call SORT_B_by_A_and_P(dim, A, B, P, descend(2))
    else
        ALLOCATE(PNT(dim))
        !
        CALL SORT(A, DESCEND(1), P=PNT, SORT_TYPE=sTYP, nat=nat, cap=cap)
        !
        if(SortB) then
                  CALL SORT_B_by_A_and_P(dim, A, B, PNT, descend(2))
        else
                  CALL PERMUTATION_SORT(B, PNT)
        end if
        !
        if(HAS_P) P(1:dim) = PNT(1:dim)
        DEALLOCATE(PNT)
    end if
    !
  END SUBROUTINE 
  !
  !--------------------------------------------------------------------------------------------------------------------------
  !
  MODULE PURE SUBROUTINE SORT_1D_MULTI_DSC1_CHAR_CHAR(A, B, SORT_B, DESCEND, STABLE, P, QSORT, HEAP, ISORT, SymMerge, nat, cap)
    CHARACTER(*),   dimension(:), contiguous,    intent(inout) :: A           ! A and B must have the same dim
    CHARACTER(*),   dimension(:), contiguous,    intent(inout) :: B           !
    LOGICAL,                           optional, intent(in   ) :: SORT_B      ! If present and false, then only sort A, and permutation sort B; otherwise also sort B along repeated values in A
    LOGICAL,                           optional, intent(in   ) :: DESCEND     ! Sort in Decending Order
    LOGICAL,                           optional, intent(in   ) :: STABLE      ! Use only Stable Sorting Methods
    INTEGER, dimension(:), contiguous, optional, intent(inout) :: P           ! Contains permutations to sort A (Sorts along with A)
    LOGICAL,                           optional, intent(in   ) :: QSORT       ! Use only Dual Pivot Quicksort/IntroSort
    LOGICAL,                           optional, intent(in   ) :: HEAP        ! Use only Heap Sort
    LOGICAL,                           optional, intent(in   ) :: ISORT       ! Use only Insertion Sort
    LOGICAL,                           optional, intent(in   ) :: SymMerge    ! Use only Stable Minimum Storage Merging by Symmetric Comparisons
    LOGICAL,                           optional, intent(in   ) :: nat         ! If true, then due a natural sort of strings
    LOGICAL,                           optional, intent(in   ) :: cap         ! If true, then do case insensitive sort of strings
    LOGICAL, dimension(2) :: DSCND
    IF(present(DESCEND)) then
        DSCND = DESCEND
    else
        DSCND = .FALSE.
    end if
    !
    CALL SORT_1D_MULTI_DSC2_CHAR_CHAR(A, B, SORT_B, DSCND, STABLE, P, QSORT, HEAP, ISORT, SymMerge, nat, cap)
  END SUBROUTINE
  !
  MODULE PURE SUBROUTINE SORT_1D_MULTI_DSC2_CHAR_CHAR(A, B, SORT_B, DESCEND, STABLE, P, QSORT, HEAP, ISORT, SymMerge, nat, cap)
    CHARACTER(*),   dimension(:), contiguous,    intent(inout) :: A           ! A and B must have the same dim
    CHARACTER(*),   dimension(:), contiguous,    intent(inout) :: B           !
    LOGICAL,                           optional, intent(in   ) :: SORT_B      ! If present and false, then only sort A, and permutation sort B; otherwise also sort B along repeated values in A
    LOGICAL,        dimension(2),                intent(in   ) :: DESCEND     ! Sort in Decending Order
    LOGICAL,                           optional, intent(in   ) :: STABLE      ! Use only Stable Sorting Methods
    INTEGER, dimension(:), contiguous, optional, intent(inout) :: P           ! Contains permutations to sort A (Sorts along with A)
    LOGICAL,                           optional, intent(in   ) :: QSORT       ! Use only Dual Pivot Quicksort/IntroSort
    LOGICAL,                           optional, intent(in   ) :: HEAP        ! Use only Heap Sort
    LOGICAL,                           optional, intent(in   ) :: ISORT       ! Use only Insertion Sort
    LOGICAL,                           optional, intent(in   ) :: SymMerge    ! Use only Stable Minimum Storage Merging by Symmetric Comparisons
    LOGICAL,                           optional, intent(in   ) :: nat         ! If true, then due a natural sort of strings
    LOGICAL,                           optional, intent(in   ) :: cap         ! If true, then do case insensitive sort of strings
    INTEGER, dimension(:), allocatable:: PNT
    INTEGER:: dim, sTYP
    LOGICAL:: SortB, HAS_P
    !
    dim = SIZE(A)
    if(dim > size(B)) return
    !
    HAS_P = present(P)
    !
    SortB = .TRUE.
    if(present(SORT_B)) SortB = SORT_B
    !
    sTYP = SORT_TYPE(dim, A, DESCEND(1), STABLE, QSORT, HEAP, ISORT, SymMerge, nat, cap)
    !
    if(sTYP == 0) then  ! A already sorted
        if(HAS_P) call INIT_PERMUTATION(dim, P)
        if(SortB) call SORT_B_by_A_and_P(dim, A, B, P, descend(2))
    else
        ALLOCATE(PNT(dim))
        !
        CALL SORT(A, DESCEND(1), P=PNT, SORT_TYPE=sTYP, nat=nat, cap=cap)
        !
        if(SortB) then
                  CALL SORT_B_by_A_and_P(dim, A, B, PNT, descend(2))
        else
                  CALL PERMUTATION_SORT(B, PNT)
        end if
        !
        if(HAS_P) P(1:dim) = PNT(1:dim)
        DEALLOCATE(PNT)
    end if
    !
  END SUBROUTINE 
  ! 
  !
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !
  ! Mult-Vector Sort via WILD select - This is not overly efficient compared to the rest of the routines in this module
  !
  ! 
  MODULE PURE SUBROUTINE SORT_1D_MULTI_DSC1_WILD_WILD_WILD(A,B,C,D,E,F,G,H,I,J, repeat_sort, DESCEND, P, nat, cap) ! A, B, C, D, E, F, and G are sorted by the dim of A, so dim=size(A) <= size(B, C, D, E, F, G)
    CLASS(*), dimension(:), contiguous,           intent(inout) :: A,B,C
    CLASS(*), dimension(:), contiguous, optional, intent(inout) :: D,E,F,G,H,I,J
    LOGICAL,                            optional, intent(in   ) :: repeat_sort
    LOGICAL,                                      intent(in   ) :: DESCEND
    INTEGER,  dimension(:), contiguous, optional, intent(inout) :: P
    LOGICAL,                            optional, intent(in   ) :: nat         ! If true, then due a natural sort of strings
    LOGICAL,                            optional, intent(in   ) :: cap         ! If true, then do case insensitive sort of strings
    LOGICAL,  dimension(:), allocatable:: DCEND
    integer:: nSORT
    nSORT = 3
    if(present(D)) nSORT = nSORT + 1
    if(present(E)) nSORT = nSORT + 1
    if(present(F)) nSORT = nSORT + 1
    if(present(G)) nSORT = nSORT + 1
    if(present(H)) nSORT = nSORT + 1
    if(present(I)) nSORT = nSORT + 1
    if(present(J)) nSORT = nSORT + 1
    !
    ALLOCATE(DCEND(nSORT))
    DCEND = DESCEND
    !
    CALL SORT_1D_MULTI_DSC2_WILD_WILD_WILD(A,B,C,D,E,F,G,H,I,J, repeat_sort, DCEND, P, nat, cap)
    !
  END SUBROUTINE
  !
  ! 
  MODULE PURE SUBROUTINE SORT_1D_MULTI_DSC2_WILD_WILD_WILD(A,B,C,D,E,F,G,H,I,J, repeat_sort, DESCEND, P, nat, cap) ! A, B, C, D, E, F, and G are sorted by the dim of A, so dim=size(A) <= size(B, C, D, E, F, G)
    CLASS(*), dimension(:), contiguous,           intent(inout) :: A,B,C
    CLASS(*), dimension(:), contiguous, optional, intent(inout) :: D,E,F,G,H,I,J
    LOGICAL,                            optional, intent(in   ) :: repeat_sort
    LOGICAL,  dimension(:),             optional, intent(in   ) :: DESCEND
    INTEGER,  dimension(:), contiguous, optional, intent(inout) :: P
    LOGICAL,                            optional, intent(in   ) :: nat         ! If true, then due a natural sort of strings
    LOGICAL,                            optional, intent(in   ) :: cap         ! If true, then do case insensitive sort of strings
    INTEGER,  DIMENSION(SIZE(A)):: PNT, REP
    INTEGER:: DIM, nSORT, nDCEND
    LOGICAL:: DCEND, rep_sort
    !
    DIM = SIZE(A)
    !
    nSORT  = 1 
    nDCEND = 0
    if(present(DESCEND)) nDCEND = size(DESCEND)
    !
    DCEND = .FALSE.
    if( nSORT <= nDCEND) DCEND = DESCEND(nSORT)
    !
    rep_sort = .TRUE.
    if(present(repeat_sort)) rep_sort = repeat_sort
    !
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    !
    SELECT TYPE (A)
    TYPE IS (INTEGER(INT32)) 
                              CALL SORT(A, DCEND, P=PNT)
                              CALL SETUP_REP(dim, A, REP)
    TYPE IS (   REAL(REL64)) 
                              CALL SORT(A, DCEND, P=PNT)
                              CALL SETUP_REP(dim, A, REP)
    TYPE IS (INTEGER(INT64)) 
                              CALL SORT(A, DCEND, P=PNT)
                              CALL SETUP_REP(dim, A, REP)
    TYPE IS (   REAL(REL32)) 
                              CALL SORT(A, DCEND, P=PNT)
                              CALL SETUP_REP(dim, A, REP)
    TYPE IS (  CHARACTER(*)) 
                              CALL SORT(A, DCEND, P=PNT)
                              CALL SETUP_REP(dim, A, REP)
    END SELECT
    !
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    !
    nSORT = nSORT + 1 
    DCEND = .FALSE.
    if( nSORT <= nDCEND) DCEND = DESCEND(nSORT)
    !
    SELECT TYPE (B)
    TYPE IS (INTEGER(INT32)); CALL SORT_REP(dim, REP, B, rep_sort, PNT, DCEND)
    TYPE IS (   REAL(REL64)); CALL SORT_REP(dim, REP, B, rep_sort, PNT, DCEND)
    TYPE IS (INTEGER(INT64)); CALL SORT_REP(dim, REP, B, rep_sort, PNT, DCEND)
    TYPE IS (   REAL(REL32)); CALL SORT_REP(dim, REP, B, rep_sort, PNT, DCEND)
    TYPE IS (  CHARACTER(*)); CALL SORT_REP(dim, REP, B, rep_sort, PNT, DCEND, nat, cap)
    END SELECT    
    !
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    !
    nSORT = nSORT + 1 
    DCEND = .FALSE.
    if( nSORT <= nDCEND) DCEND = DESCEND(nSORT)
    !
    SELECT TYPE (C)
    TYPE IS (INTEGER(INT32)); CALL SORT_REP(dim, REP, C, rep_sort, PNT, DCEND)
    TYPE IS (   REAL(REL64)); CALL SORT_REP(dim, REP, C, rep_sort, PNT, DCEND)
    TYPE IS (INTEGER(INT64)); CALL SORT_REP(dim, REP, C, rep_sort, PNT, DCEND)
    TYPE IS (   REAL(REL32)); CALL SORT_REP(dim, REP, C, rep_sort, PNT, DCEND)
    TYPE IS (  CHARACTER(*)); CALL SORT_REP(dim, REP, C, rep_sort, PNT, DCEND, nat, cap)
    END SELECT     
    !
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    !
    IF(present(D)) THEN
       nSORT = nSORT + 1 
       DCEND = .FALSE.
       if( nSORT <= nDCEND) DCEND = DESCEND(nSORT)
       !
       SELECT TYPE (D)
       TYPE IS (INTEGER(INT32)); CALL SORT_REP(dim, REP, D, rep_sort, PNT, DCEND)
       TYPE IS (   REAL(REL64)); CALL SORT_REP(dim, REP, D, rep_sort, PNT, DCEND)
       TYPE IS (INTEGER(INT64)); CALL SORT_REP(dim, REP, D, rep_sort, PNT, DCEND)
       TYPE IS (   REAL(REL32)); CALL SORT_REP(dim, REP, D, rep_sort, PNT, DCEND)
       TYPE IS (  CHARACTER(*)); CALL SORT_REP(dim, REP, D, rep_sort, PNT, DCEND, nat, cap)
       END SELECT  
    END IF
    !
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    !
    IF(present(E)) THEN
       nSORT = nSORT + 1 
       DCEND = .FALSE.
       if( nSORT <= nDCEND) DCEND = DESCEND(nSORT)
       !
       SELECT TYPE (E)
       TYPE IS (INTEGER(INT32)); CALL SORT_REP(dim, REP, E, rep_sort, PNT, DCEND)
       TYPE IS (   REAL(REL64)); CALL SORT_REP(dim, REP, E, rep_sort, PNT, DCEND)
       TYPE IS (INTEGER(INT64)); CALL SORT_REP(dim, REP, E, rep_sort, PNT, DCEND)
       TYPE IS (   REAL(REL32)); CALL SORT_REP(dim, REP, E, rep_sort, PNT, DCEND)
       TYPE IS (  CHARACTER(*)); CALL SORT_REP(dim, REP, E, rep_sort, PNT, DCEND, nat, cap)
       END SELECT    
    END IF  
    !
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    !
    IF(present(F)) THEN
       nSORT = nSORT + 1 
       DCEND = .FALSE.
       if( nSORT <= nDCEND) DCEND = DESCEND(nSORT)
       !
       SELECT TYPE (F)
       TYPE IS (INTEGER(INT32)); CALL SORT_REP(dim, REP, F, rep_sort, PNT, DCEND)
       TYPE IS (   REAL(REL64)); CALL SORT_REP(dim, REP, F, rep_sort, PNT, DCEND)
       TYPE IS (INTEGER(INT64)); CALL SORT_REP(dim, REP, F, rep_sort, PNT, DCEND)
       TYPE IS (   REAL(REL32)); CALL SORT_REP(dim, REP, F, rep_sort, PNT, DCEND)
       TYPE IS (  CHARACTER(*)); CALL SORT_REP(dim, REP, F, rep_sort, PNT, DCEND, nat, cap)
       END SELECT      
    END IF
    !
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    !
    IF(present(G)) THEN
       nSORT = nSORT + 1 
       DCEND = .FALSE.
       if( nSORT <= nDCEND) DCEND = DESCEND(nSORT)
       !
       SELECT TYPE (G)
       TYPE IS (INTEGER(INT32)); CALL SORT_REP(dim, REP, G, rep_sort, PNT, DCEND)
       TYPE IS (   REAL(REL64)); CALL SORT_REP(dim, REP, G, rep_sort, PNT, DCEND)
       TYPE IS (INTEGER(INT64)); CALL SORT_REP(dim, REP, G, rep_sort, PNT, DCEND)
       TYPE IS (   REAL(REL32)); CALL SORT_REP(dim, REP, G, rep_sort, PNT, DCEND)
       TYPE IS (  CHARACTER(*)); CALL SORT_REP(dim, REP, G, rep_sort, PNT, DCEND, nat, cap)
       END SELECT    
    END IF  
    !
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    !
    IF(present(H)) THEN
       nSORT = nSORT + 1 
       DCEND = .FALSE.
       if( nSORT <= nDCEND) DCEND = DESCEND(nSORT)
       !
       SELECT TYPE (H)
       TYPE IS (INTEGER(INT32)); CALL SORT_REP(dim, REP, H, rep_sort, PNT, DCEND)
       TYPE IS (   REAL(REL64)); CALL SORT_REP(dim, REP, H, rep_sort, PNT, DCEND)
       TYPE IS (INTEGER(INT64)); CALL SORT_REP(dim, REP, H, rep_sort, PNT, DCEND)
       TYPE IS (   REAL(REL32)); CALL SORT_REP(dim, REP, H, rep_sort, PNT, DCEND)
       TYPE IS (  CHARACTER(*)); CALL SORT_REP(dim, REP, H, rep_sort, PNT, DCEND, nat, cap)
       END SELECT     
    END IF 
    !
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    !
    IF(present(I)) THEN
       nSORT = nSORT + 1 
       DCEND = .FALSE.
       if( nSORT <= nDCEND) DCEND = DESCEND(nSORT)
       !
       SELECT TYPE (I)
       TYPE IS (INTEGER(INT32)); CALL SORT_REP(dim, REP, I, rep_sort, PNT, DCEND)
       TYPE IS (   REAL(REL64)); CALL SORT_REP(dim, REP, I, rep_sort, PNT, DCEND)
       TYPE IS (INTEGER(INT64)); CALL SORT_REP(dim, REP, I, rep_sort, PNT, DCEND)
       TYPE IS (   REAL(REL32)); CALL SORT_REP(dim, REP, I, rep_sort, PNT, DCEND)
       TYPE IS (  CHARACTER(*)); CALL SORT_REP(dim, REP, I, rep_sort, PNT, DCEND, nat, cap)
       END SELECT      
    END IF
    !
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    !
    IF(present(J)) THEN
       nSORT = nSORT + 1 
       DCEND = .FALSE.
       if( nSORT <= nDCEND) DCEND = DESCEND(nSORT)
       !
       SELECT TYPE (J)
       TYPE IS (INTEGER(INT32)); CALL SORT_REP(dim, REP, J, rep_sort, PNT, DCEND)
       TYPE IS (   REAL(REL64)); CALL SORT_REP(dim, REP, J, rep_sort, PNT, DCEND)
       TYPE IS (INTEGER(INT64)); CALL SORT_REP(dim, REP, J, rep_sort, PNT, DCEND)
       TYPE IS (   REAL(REL32)); CALL SORT_REP(dim, REP, J, rep_sort, PNT, DCEND)
       TYPE IS (  CHARACTER(*)); CALL SORT_REP(dim, REP, J, rep_sort, PNT, DCEND, nat, cap)
       END SELECT     
    END IF
    !
    IF(PRESENT(P)) P(1:dim) = PNT
    !
  END SUBROUTINE
  !! 
  !!
  !!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !!
  !! JSEARCH Routines
  !!
  !PURE SUBROUTINE JSEARCH_WILD(DIM, A, I, J)
  !  INTEGER,                  INTENT(IN   ):: DIM, I
  !  CLASS(*), DIMENSION(DIM), INTENT(IN   ):: A
  !  INTEGER,                  INTENT(INOUT):: J
  !  !
  !  SELECT TYPE (A)
  !  TYPE IS (INTEGER(INT32)); CALL JSEARCH_INT32(DIM, A, I, J)
  !  TYPE IS (   REAL(REL64)); CALL JSEARCH_REL64(DIM, A, I, J)
  !  TYPE IS (INTEGER(INT64)); CALL JSEARCH_INT64(DIM, A, I, J)
  !  TYPE IS (   REAL(REL32)); CALL JSEARCH_REL32(DIM, A, I, J)
  !  END SELECT
  !  !
  !END SUBROUTINE  
  !!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !PURE SUBROUTINE JSEARCH_INT32(DIM, A, I, J)
  !  INTEGER,                        INTENT(IN   ):: DIM, I
  !  INTEGER(INT32), DIMENSION(DIM), INTENT(IN   ):: A
  !  INTEGER,                        INTENT(INOUT):: J
  !  !
  !  JSEARCH: DO WHILE (J <= DIM) 
  !      IF( A(I) /= A(J) ) EXIT JSEARCH
  !      J = J + 1
  !  END DO JSEARCH
  !  J = J - 1
  !  !
  !END SUBROUTINE    
  !!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !PURE SUBROUTINE JSEARCH_INT64(DIM, A, I, J)
  !  INTEGER,                        INTENT(IN   ):: DIM, I
  !  INTEGER(INT64), DIMENSION(DIM), INTENT(IN   ):: A
  !  INTEGER,                        INTENT(INOUT):: J
  !  !
  !  JSEARCH: DO WHILE (J <= DIM) 
  !      IF( A(I) /= A(J) ) EXIT JSEARCH
  !      J = J + 1
  !  END DO JSEARCH
  !  J = J - 1
  !  !
  !END SUBROUTINE  
  !!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !PURE SUBROUTINE JSEARCH_REL32(DIM, A, I, J)
  !  INTEGER,                     INTENT(IN   ):: DIM, I
  !  REAL(REL32), DIMENSION(DIM), INTENT(IN   ):: A
  !  INTEGER,                     INTENT(INOUT):: J
  !  !
  !  JSEARCH: DO WHILE (J <= DIM) 
  !      IF( A(I) /= A(J) ) EXIT JSEARCH
  !      J = J + 1
  !  END DO JSEARCH
  !  J = J - 1
  !  !
  !END SUBROUTINE  
  !!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !PURE SUBROUTINE JSEARCH_REL64(DIM, A, I, J)
  !  INTEGER,                     INTENT(IN   ):: DIM, I
  !  REAL(REL64), DIMENSION(DIM), INTENT(IN   ):: A
  !  INTEGER,                     INTENT(INOUT):: J
  !  !
  !  JSEARCH: DO WHILE (J <= DIM) 
  !      IF( A(I) /= A(J) ) EXIT JSEARCH
  !      J = J + 1
  !  END DO JSEARCH
  !  J = J - 1
  !  !
  !END SUBROUTINE
  !! 
  !!
  !!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !!
  !! GREATER_THAN_SWAP Routines
  !!
  !PURE SUBROUTINE GREATER_THAN_SWAP_WILD(DIM, A, I, J, IDX)
  !  INTEGER,                  INTENT(IN   ):: DIM, I, J
  !  CLASS(*), DIMENSION(DIM), INTENT(INOUT):: A
  !  INTEGER,  DIMENSION(DIM), INTENT(INOUT):: IDX
  !  !
  !  SELECT TYPE (A)
  !  TYPE IS (INTEGER(INT32)); CALL GREATER_THAN_SWAP_INT32(DIM, A, I, J, IDX)
  !  TYPE IS (   REAL(REL64)); CALL GREATER_THAN_SWAP_REL64(DIM, A, I, J, IDX)
  !  TYPE IS (INTEGER(INT64)); CALL GREATER_THAN_SWAP_INT64(DIM, A, I, J, IDX)
  !  TYPE IS (   REAL(REL32)); CALL GREATER_THAN_SWAP_REL32(DIM, A, I, J, IDX)
  !  END SELECT
  !  !
  !END SUBROUTINE  
  !!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !PURE SUBROUTINE GREATER_THAN_SWAP_INT32(DIM, A, I, J, P)
  !  INTEGER,                        INTENT(IN   ):: DIM, I, J
  !  INTEGER(INT32), DIMENSION(DIM), INTENT(INOUT):: A
  !  INTEGER,        DIMENSION(DIM), INTENT(INOUT):: P
  !  INTEGER(INT32):: TMP
  !  INTEGER:: POS
  !  !
  !  IF (A(I) > A(J)) THEN
  !                   TMP  = A(I);  A(I) = A(J);  A(J) = TMP
  !                   POS  = P(I);  P(I) = P(J);  P(J) = POS
  !  END IF 
  !  !
  !END SUBROUTINE   
  !!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !PURE SUBROUTINE GREATER_THAN_SWAP_INT64(DIM, A, I, J, P)
  !  INTEGER,                        INTENT(IN   ):: DIM, I, J
  !  INTEGER(INT64), DIMENSION(DIM), INTENT(INOUT):: A
  !  INTEGER,        DIMENSION(DIM), INTENT(INOUT):: P
  !  INTEGER(INT64):: TMP
  !  INTEGER:: POS
  !  !
  !  IF (A(I) > A(J)) THEN
  !                   TMP  = A(I);  A(I) = A(J);  A(J) = TMP
  !                   POS  = P(I);  P(I) = P(J);  P(J) = POS
  !  END IF 
  !  !
  !END SUBROUTINE   
  !!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !PURE SUBROUTINE GREATER_THAN_SWAP_REL32(DIM, A, I, J, P)
  !  INTEGER,                     INTENT(IN   ):: DIM, I, J
  !  REAL(REL32), DIMENSION(DIM), INTENT(INOUT):: A
  !  INTEGER,     DIMENSION(DIM), INTENT(INOUT):: P
  !  REAL(REL32):: TMP
  !  INTEGER:: POS
  !  !
  !  IF (A(I) > A(J)) THEN
  !                   TMP  = A(I);  A(I) = A(J);  A(J) = TMP
  !                   POS  = P(I);  P(I) = P(J);  P(J) = POS
  !  END IF 
  !  !
  !END SUBROUTINE   
  !!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !PURE SUBROUTINE GREATER_THAN_SWAP_REL64(DIM, A, I, J, P)
  !  INTEGER,                     INTENT(IN   ):: DIM, I, J
  !  REAL(REL64), DIMENSION(DIM), INTENT(INOUT):: A
  !  INTEGER,     DIMENSION(DIM), INTENT(INOUT):: P
  !  REAL(REL64):: TMP
  !  INTEGER:: POS
  !  !
  !  IF (A(I) > A(J)) THEN
  !                   TMP  = A(I);  A(I) = A(J);  A(J) = TMP
  !                   POS  = P(I);  P(I) = P(J);  P(J) = POS
  !  END IF 
  !  !
  !END SUBROUTINE  
  !! 
  !!
  !!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !!
  !! RANGE_ISORT_WILD Routines
  !!
  !PURE SUBROUTINE RANGE_ISORT_WILD(DIM, A, I, J, IDX)
  !  INTEGER,                  INTENT(IN   ):: DIM, I, J
  !  CLASS(*), DIMENSION(DIM), INTENT(INOUT):: A
  !  INTEGER,  DIMENSION(DIM), INTENT(INOUT):: IDX
  !  !
  !  SELECT TYPE (A)
  !  TYPE IS (INTEGER(INT32)); CALL RANGE_ISORT_INT32(DIM, A, I, J, IDX)
  !  TYPE IS (   REAL(REL64)); CALL RANGE_ISORT_REL64(DIM, A, I, J, IDX)
  !  TYPE IS (INTEGER(INT64)); CALL RANGE_ISORT_INT64(DIM, A, I, J, IDX)
  !  TYPE IS (   REAL(REL32)); CALL RANGE_ISORT_REL32(DIM, A, I, J, IDX)
  !  END SELECT
  !  !
  !END SUBROUTINE  
  !!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !PURE SUBROUTINE RANGE_ISORT_INT32(DIM, A, I, J, P)
  !  INTEGER,                        INTENT(IN   ):: DIM, I, J
  !  INTEGER(INT32), DIMENSION(DIM), INTENT(INOUT):: A
  !  INTEGER,        DIMENSION(DIM), INTENT(INOUT):: P
  !  INTEGER(INT32):: TMP
  !  INTEGER:: N, M, POS
  !  !
  !  DO M=I+1, J 
  !      POS = P(M)
  !      TMP = A(M)
  !      N   = M - 1
  !      DO WHILE ( N >= I )
  !                  IF( TMP >= A(N) ) EXIT
  !                  A(N+1) = A(N)
  !                  P(N+1) = P(N)
  !                  N = N - 1
  !      END DO
  !      A(N+1) = TMP
  !      P(N+1) = POS
  !  END DO
  !  !
  !END SUBROUTINE   
  !!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !PURE SUBROUTINE RANGE_ISORT_INT64(DIM, A, I, J, P)
  !  INTEGER,                        INTENT(IN   ):: DIM, I, J
  !  INTEGER(INT64), DIMENSION(DIM), INTENT(INOUT):: A
  !  INTEGER,        DIMENSION(DIM), INTENT(INOUT):: P
  !  INTEGER(INT64):: TMP
  !  INTEGER:: N, M, POS
  !  !
  !  DO M=I+1, J 
  !      POS = P(M)
  !      TMP = A(M)
  !      N   = M - 1
  !      DO WHILE ( N >= I )
  !                  IF( TMP >= A(N) ) EXIT
  !                  A(N+1) = A(N)
  !                  P(N+1) = P(N)
  !                  N = N - 1
  !      END DO
  !      A(N+1) = TMP
  !      P(N+1) = POS
  !  END DO
  !  !
  !END SUBROUTINE   
  !!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !PURE SUBROUTINE RANGE_ISORT_REL32(DIM, A, I, J, P)
  !  INTEGER,                     INTENT(IN   ):: DIM, I, J
  !  REAL(REL32), DIMENSION(DIM), INTENT(INOUT):: A
  !  INTEGER,     DIMENSION(DIM), INTENT(INOUT):: P
  !  REAL(REL32):: TMP
  !  INTEGER:: N, M, POS
  !  !
  !  DO M=I+1, J 
  !      POS = P(M)
  !      TMP = A(M)
  !      N   = M - 1
  !      DO WHILE ( N >= I )
  !                  IF( TMP >= A(N) ) EXIT
  !                  A(N+1) = A(N)
  !                  P(N+1) = P(N)
  !                  N = N - 1
  !      END DO
  !      A(N+1) = TMP
  !      P(N+1) = POS
  !  END DO
  !  !
  !END SUBROUTINE   
  !!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !PURE SUBROUTINE RANGE_ISORT_REL64(DIM, A, I, J, P)
  !  INTEGER,                     INTENT(IN   ):: DIM, I, J
  !  REAL(REL64), DIMENSION(DIM), INTENT(INOUT):: A
  !  INTEGER,     DIMENSION(DIM), INTENT(INOUT):: P
  !  REAL(REL64):: TMP
  !  INTEGER:: N, M, POS
  !  !
  !  DO M=I+1, J 
  !      POS = P(M)
  !      TMP = A(M)
  !      N   = M - 1
  !      DO WHILE ( N >= I )
  !                  IF( TMP >= A(N) ) EXIT
  !                  A(N+1) = A(N)
  !                  P(N+1) = P(N)
  !                  N = N - 1
  !      END DO
  !      A(N+1) = TMP
  !      P(N+1) = POS
  !  END DO
  !  !
  !END SUBROUTINE  
  !!!!
  !!!!###############################################################################################
  !!!PURE SUBROUTINE SWAP_WILD(DIM, A, I, J)
  !!!  INTEGER,                  INTENT(IN   ):: DIM, I, J
  !!!  CLASS(*), DIMENSION(DIM), INTENT(INOUT):: A
  !!!  !
  !!!  SELECT TYPE (A)
  !!!  TYPE IS (INTEGER(INT32)); CALL SWAP_INT32(DIM, A, I, J)
  !!!  TYPE IS (   REAL(REL64)); CALL SWAP_REL64(DIM, A, I, J)
  !!!  TYPE IS (INTEGER(INT64)); CALL SWAP_INT64(DIM, A, I, J)
  !!!  TYPE IS (   REAL(REL32)); CALL SWAP_REL32(DIM, A, I, J)
  !!!  END SELECT
  !!!  !
  !!!END SUBROUTINE  
  !!!!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !!!PURE SUBROUTINE SWAP_INT32(DIM, A, I, J)
  !!!  INTEGER,                        INTENT(IN   ):: DIM, I, J
  !!!  INTEGER(INT32), DIMENSION(DIM), INTENT(INOUT):: A
  !!!  INTEGER(INT32):: TMP
  !!!  TMP  = A(I) 
  !!!  A(I) = A(J)
  !!!  A(J) = TMP
  !!!END SUBROUTINE   
  !!!!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !!!PURE SUBROUTINE SWAP_INT64(DIM, A, I, J)
  !!!  INTEGER,                        INTENT(IN   ):: DIM, I, J
  !!!  INTEGER(INT64), DIMENSION(DIM), INTENT(INOUT):: A
  !!!  INTEGER(INT64):: TMP
  !!!  TMP  = A(I) 
  !!!  A(I) = A(J)
  !!!  A(J) = TMP
  !!!END SUBROUTINE   
  !!!!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !!!PURE SUBROUTINE SWAP_REL32(DIM, A, I, J)
  !!!  INTEGER,                     INTENT(IN   ):: DIM, I, J
  !!!  REAL(REL32), DIMENSION(DIM), INTENT(INOUT):: A
  !!!  REAL(REL32):: TMP
  !!!  TMP  = A(I) 
  !!!  A(I) = A(J)
  !!!  A(J) = TMP
  !!!END SUBROUTINE   
  !!!!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !!!PURE SUBROUTINE SWAP_REL64(DIM, A, I, J)
  !!!  INTEGER,                     INTENT(IN   ):: DIM, I, J
  !!!  REAL(REL64), DIMENSION(DIM), INTENT(INOUT):: A
  !!!  REAL(REL64):: TMP
  !!!  TMP  = A(I) 
  !!!  A(I) = A(J)
  !!!  A(J) = TMP
  !!!END SUBROUTINE  
  !!!     
  !! 
  !!
  !!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !!
  !! Not Equal Comparison
  !!
  !PURE FUNCTION NOT_EQUAL(DIM,A,I,J) RESULT(ANS)
  !  INTEGER,                  INTENT(IN):: DIM,I,J
  !  CLASS(*), DIMENSION(DIM), INTENT(IN) :: A
  !  LOGICAL:: ANS
  !  !
  !  SELECT TYPE (A)
  !  TYPE IS (INTEGER(INT32)); ANS = A(I) /= A(J)
  !  TYPE IS (   REAL(REL64)); ANS = A(I) /= A(J)
  !  TYPE IS (INTEGER(INT64)); ANS = A(I) /= A(J)
  !  TYPE IS (   REAL(REL32)); ANS = A(I) /= A(J)
  !  END SELECT
  !  !
  !END FUNCTION
  !! - Overkill check for SINGLE and DOUBLE
  !!PURE FUNCTION NOT_EQUAL(DIM,A,I,J) RESULT(ANS)
  !!  INTEGER,                  INTENT(IN):: DIM,I,J
  !!  CLASS(*), DIMENSION(DIM), INTENT(IN) :: A
  !!  LOGICAL:: ANS
  !!  !
  !!  SELECT TYPE (A)
  !!  TYPE IS (INTEGER(INT32)); ANS = A(I) /= A(J)
  !!  TYPE IS (   REAL(REL64)); ANS = ABS( A(I) - A(J) ) <= 1D-12 * ABS(A(J))
  !!  TYPE IS (INTEGER(INT64)); ANS = A(I) /= A(J)
  !!  TYPE IS (   REAL(REL32)); ANS = ABS( A(I) - A(J) ) <=  1E-6 * ABS(A(J))
  !!  END SELECT
  !!  !
  !!END FUNCTION
  !!
  !!
  !!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !!
  !! Sign flip used for DECEND SORT
  !!
  !PURE SUBROUTINE SIGN_FLIP_1D_WILD(DIM,A)
  !  INTEGER,                  INTENT(IN   ):: DIM
  !  CLASS(*), DIMENSION(DIM), INTENT(INOUT):: A
  !  SELECT TYPE (A)
  !  TYPE IS (INTEGER(INT32)); CALL SIGN_FLIP_1D_INT32(DIM,A)
  !  TYPE IS (   REAL(REL64)); CALL SIGN_FLIP_1D_REL64(DIM,A)
  !  TYPE IS (INTEGER(INT64)); CALL SIGN_FLIP_1D_INT64(DIM,A)
  !  TYPE IS (   REAL(REL32)); CALL SIGN_FLIP_1D_REL32(DIM,A)
  !  END SELECT
  !END SUBROUTINE
  !!
  !PURE SUBROUTINE SIGN_FLIP_1D_INT32(DIM,A)
  !  INTEGER,                       INTENT(IN   ):: DIM
  !  INTEGER(INT32), DIMENSION(DIM),INTENT(INOUT):: A
  !  INTEGER:: I
  !  DO CONCURRENT (I=1:DIM)
  !                         A(I) = -1_INT32 * A(I)
  !  END DO
  !END SUBROUTINE
  !!
  !PURE SUBROUTINE SIGN_FLIP_1D_INT64(DIM,A)
  !  INTEGER,                       INTENT(IN   ):: DIM
  !  INTEGER(INT64), DIMENSION(DIM),INTENT(INOUT):: A
  !  INTEGER:: I
  !  DO CONCURRENT (I=1:DIM)
  !                         A(I) = -1_INT64 * A(I)
  !  END DO
  !END SUBROUTINE
  !!
  !PURE SUBROUTINE SIGN_FLIP_1D_REL32(DIM,A)
  !  INTEGER,                    INTENT(IN   ):: DIM
  !  REAL(REL32), DIMENSION(DIM),INTENT(INOUT):: A
  !  INTEGER:: I
  !  DO CONCURRENT (I=1:DIM)
  !                         A(I) = -1_REL32 * A(I)
  !  END DO
  !END SUBROUTINE
  !!
  !PURE SUBROUTINE SIGN_FLIP_1D_REL64(DIM,A)
  !  INTEGER,                       INTENT(IN   ):: DIM
  !  REAL(REL64), DIMENSION(DIM),INTENT(INOUT):: A
  !  INTEGER:: I
  !  DO CONCURRENT (I=1:DIM)
  !                         A(I) = -1_REL64 * A(I)
  !  END DO
  !END SUBROUTINE
  !
END SUBMODULE

