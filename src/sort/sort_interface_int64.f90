!
!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<!
!>>>>>>>>>##############>>>>>>>>>################>>>>>>>>>################>>>>>>>>>###############>>>>>>>>>###############>>>>>>>>>>!
!<<<<<<<<<##############<<<<<<<<<################<<<<<<<<<################<<<<<<<<<###############<<<<<<<<<###############<<<<<<<<<<!
!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>!
!
! SORT_INTERFACE_INT64
!
!============================================================================================================
! 
! ISORT    => Insertion Sort                                          (https://en.wikipedia.org/wiki/Insertion_sort)
! QSORT    => Dual Pivot Quicksort                                    (https://en.wikipedia.org/wiki/Quicksort)
! SYMMERGE => Stable Minimum Storage Merging by Symmetric Comparisons (Kim, P. S., & Kutzner, A. (2004, September). Stable minimum storage merging by symmetric comparisons. In European Symposium on Algorithms (pp. 714-723). Springer, Berlin, Heidelberg.)
! 
!       Submodule Defines SORT routines for INTEGER(INT64) Type
!
!-----------------------------------------------------------------------------------------------------------
!
!  If you wish to make this submodule into a standalone module 
!      (say you only wish to compile this version and not the other submodules)
!  Then you need to do the following:
!
!    Rename SUBMODULE (SORT_INTERFACE) SORT_INTERFACE_INT64 
!        to MODULE SORT_INTERFACE_INT64 
!      and
!           END SUBMODULE
!       to  END    MODULE
! 
!    Change all MODULE PURE SUBROUTINE and MODULE PURE FUNCTION
!       to just        PURE SUBROUTINE and        PURE FUNCTION
!
!    Uncomment the PUBLIC, PRIVATE and INTERFACES sections at the start of the submodule.
!      They contain !\! in front of them to make it easy to find.
!
!-----------------------------------------------------------------------------------------------------------
!
SUBMODULE (SORT_INTERFACE) SORT_INTERFACE_INT64
  !\!USE, INTRINSIC:: ISO_FORTRAN_ENV, ONLY: INT8, INT32, INT64, REL32=>REAL32, REL64=>REAL64
  !
  IMPLICIT NONE
  !
  INTEGER, PARAMETER:: QSORT_PARTITION_MIN    = 24   ! If (R - L + 1 <= SIZ), then stop QSORT partitioning and use ISORT on partition
  INTEGER, PARAMETER:: ISORT_SMALL_ARRAY_SIZE = 64   ! If (dim <= SIZ), then use ISOR
  REAL,    PARAMETER:: PARTIAL_SORTED_LIM     = 0.1  ! LIM = PARTIAL_SORTED_LIM*dim, If COUNT( A(I) > A(I+1) ) < LIM, then use SymMerge  instead of QSORT -> Note this ignores repeated elements --> A value of 0.1 means that it passes if 90% of the array appears to be sorted
  !
  INTEGER(INT64), PARAMETER:: ZER =  0_INT64
  INTEGER(INT64), PARAMETER:: NEG = -1_INT64
  !
  INTERFACE ROW_COPY                     !ROW_COPY(dim1, dim2, row, A, B)
    MODULE PROCEDURE ROW_COPY_TO_VEC
    MODULE PROCEDURE ROW_COPY_FROM_VEC
  END INTERFACE
  !
  !\!PUBLIC:: SORT, SORTED
  !\!PUBLIC:: PERMUTATION_SORT,  ALLOCATE_P
  !\!PUBLIC:: REVERSE_ORDER
  !\!!
  !\!PRIVATE
  !\!!
  !\!INTERFACE SORT
  !\!  MODULE PROCEDURE SORT_1D_INT64            ! SORT(A, [DESCEND],     [STABLE],                [P])
  !\!  MODULE PROCEDURE SORT_2D_INDEX_INT64      ! SORT(A, SORT_INDEX,   [DESCEND], [SORT_BY_ROW], [P])
  !\!  MODULE PROCEDURE SORT_2D_MULTI_INT64      ! SORT(A, SORT_INDICES, [DESCEND], [SORT_BY_ROW], [P])  -> DESCEND is array of SIZE(SORT_INDICES)
  !\!  MODULE PROCEDURE SORT_2D_MULTI_INT64_1DSC ! SORT(A, SORT_INDICES, [DESCEND], [SORT_BY_ROW], [P])  -> DESCEND is a scalar
  !\!  MODULE PROCEDURE SORT_1D_QSORT_A_INT64    ! SORT(dim, A)
  !\!  MODULE PROCEDURE SORT_1D_SymMg_P_INT64    ! SORT(dim, A, P)
  !\!END INTERFACE
  !\!!
  !\!INTERFACE SORTED
  !\!  MODULE PROCEDURE SORTED_1D_INT64          ! SORTED(A, DESCEND) RESULT(SORTED_ARRAY)
  !\!END INTERFACE
  !\!!
  !\!INTERFACE REVERSE_ORDER
  !\!  MODULE PROCEDURE REVERSE_ORDER_1D_NODIM_INT64        ! REVERSE_ORDER(A, [STABLE], [P])
  !\!  MODULE PROCEDURE REVERSE_ORDER_1D_DIM_INT64          ! REVERSE_ORDER(dim, A, [STABLE], [P])
  !\!  MODULE PROCEDURE REVERSE_ORDER_2D_INT64              ! REVERSE_ORDER(A, REVERSE_EACH_ROW)  --> Default REVERSE_EACH_ROW is TRUE
  !\!  MODULE PROCEDURE REVERSE_ORDER_2D_INDEX_INT64        ! REVERSE_ORDER(A, REV_INDEX, STABLE, REVERSE_ROW, P)
  !\!END INTERFACE
  !\!!
  !\!INTERFACE PERMUTATION_SORT
  !\!  MODULE PROCEDURE PERMUTATION_SORT_1D_INT64  ! INDEX_SORT(A, P)
  !\!  MODULE PROCEDURE PERMUTATION_SORT_2D_INT64  ! INDEX_SORT(A, P, [BY_COLUMN])   Default is BY_COLUMN=TRUE
  !\!END INTERFACE
  !\!!
  !\!INTERFACE ALLOCATE_P
  !\!  MODULE PROCEDURE ALLOCATE_PERM_INT64 ! ALLOCATE_P(A, IDX)
  !\!END INTERFACE
  !
  CONTAINS  !================================================================================================================
  !
  ! Main Driver Sort Routine for 1D Arrays
  !
  MODULE PURE SUBROUTINE SORT_1D_INT64(A, DESCEND, STABLE, P, SORT_TYPE, QSORT, HEAP, ISORT, SymMerge, INIT_P)
    INTEGER(INT64), dimension(:), contiguous,    intent(inout) :: A
    LOGICAL,                           optional, intent(in   ) :: DESCEND   ! Sort in Decending Order
    LOGICAL,                           optional, intent(in   ) :: STABLE    ! Use only Stable Sorting Methods
    INTEGER, dimension(:), contiguous, optional, intent(inout) :: P         ! Contains permutations to sort A (Sorts along with A)
    INTEGER,                           optional, intent(in   ) :: SORT_TYPE ! If present, then disables call to SORT_TYPE sets the sort type to the optiona defined. It must be a valid sort type option integer.
    LOGICAL,                           optional, intent(in   ) :: QSORT     ! Use only Dual Pivot Quicksort/IntroSort
    LOGICAL,                           optional, intent(in   ) :: HEAP      ! Use only Heap Sort
    LOGICAL,                           optional, intent(in   ) :: ISORT     ! Use only Insertion Sort
    LOGICAL,                           optional, intent(in   ) :: SymMerge  ! Use only Stable Minimum Storage Merging by Symmetric Comparisons
    LOGICAL,                           optional, intent(in   ) :: INIT_P    ! Set P = [1...to...DIM]; Default is True; which makes P a permutation vector for how A is sorted
    INTEGER:: dim, sTYP
    LOGICAL:: HAS_P
    !
    dim = SIZE(A)
    !
    HAS_P = .FALSE.
    IF( present(P) ) THEN
        IF( dim > SIZE(P) ) THEN
            P = -1
        ELSE
            HAS_P = .TRUE.
            IF( present(INIT_P) ) HAS_P = INIT_P  !Temp use for checking if it needs to be initialized
            !
            IF(HAS_P) THEN
                           CALL INIT_PERMUTATION(dim, P)
            ELSE
                           HAS_P = .TRUE.
            END IF
        END IF
    END IF
    !
    IF( dim < 2 ) RETURN
    !
    if(present(SORT_TYPE)) then
         sTYP = SORT_TYPE
    ELSE
         sTYP = SORT_TYPE_INT64(dim, A, DESCEND, STABLE, QSORT, HEAP, ISORT, SymMerge)
    END IF
    !   0 - Array is sorted
    !   1 - ISORT            - Odd options are stable sorts
    !   2 - HEAPsort
    !   3 - SYM-MERGE sort
    !   4 - QSORT         (Techically its an Intro-Sort with Dual Pivot Quick, Heap, and Insertion Sorts)
    !   5 - TIMSORT       (not implimented yet - Requires buffer size diBm/2)
    !
    IF(present(DESCEND) .and. sTYP /= ZER) THEN
            IF(DESCEND)  CALL SIGN_FLIP_1D(dim,A)
    END IF
    !
    IF    (sTYP == 1) THEN
                     IF( HAS_P ) THEN
                         CALL ISORT_1P_INT64(dim, A, P)
                     ELSE
                         CALL ISORT_1D_INT64(dim, A)
                     END IF
    ELSEIF(sTYP == 4) THEN
                     IF( HAS_P ) THEN
                         CALL QSORT_1P_INT64( dim, A, P, 1, dim, MAX_RECUR(dim) )
                     ELSE
                         CALL QSORT_1D_INT64( dim, A,      1, dim, MAX_RECUR(dim) )
                     END IF
    ELSEIF(sTYP == 3) THEN
                     IF( HAS_P ) THEN
                         CALL SymMerge_1P_INT64(dim, A, P)
                     ELSE
                         CALL SymMerge_1D_INT64(dim, A)
                     END IF
    ELSEIF(sTYP == 2) THEN
                     IF( HAS_P ) THEN
                         CALL HEAPSORT_1P_INT64(dim, A, P)
                     ELSE
                         CALL HEAPSORT_1D_INT64(dim, A)
                     END IF
    END IF
    !
    ! If P is present, sort on P for repeated elements in A to make it stable'ish
    ! If sTYP is even, then an unstable sort was used.
    IF( HAS_P  .and. sTYP > 1) THEN
         IF(IAND(sTYP,1) == 0) CALL SORT_P_ON_REPEATED_A(dim, A, P)
    END IF
    !
    IF(present(DESCEND) .and. sTYP /= ZER) THEN
            IF(DESCEND)  CALL SIGN_FLIP_1D(dim,A)
    END IF
    !
  END SUBROUTINE
  !
  !##########################################################################################################################
  ! These two routines provide fast access to QSORT and SYMMERGE
  !
  MODULE PURE SUBROUTINE SORT_1D_QSORT_A_INT64(dim, A)
    INTEGER,                        intent(in   ) :: dim
    INTEGER(INT64), dimension(dim), intent(inout) :: A
    !
    CALL QSORT_1D_INT64( dim, A, 1, dim, MAX_RECUR(dim) )
    !
  END SUBROUTINE
  !
  MODULE PURE SUBROUTINE SORT_1D_SymMg_P_INT64(dim, A, P)
    INTEGER,                        intent(in   ) :: dim
    INTEGER(INT64), dimension(dim), intent(inout) :: A
    INTEGER,        dimension(dim), intent(inout) :: P
    !
    CALL INIT_PERMUTATION(dim, P)
    !
    CALL SymMerge_1P_INT64(dim, A, P)
    !
  END SUBROUTINE
  !
  !##########################################################################################################################
  ! Function that returns a copy of the vector that is sorted
  !
  MODULE PURE FUNCTION SORTED_1D_INT64(A, DESCEND, STABLE) RESULT(SORTED_ARRAY)
    INTEGER(INT64), dimension(:), contiguous,           intent(in) :: A
    LOGICAL,                                  optional, intent(in) :: DESCEND, STABLE
    INTEGER(INT64), dimension(SIZE(A)):: SORTED_ARRAY
    SORTED_ARRAY = A
    CALL SORT_1D_INT64(SORTED_ARRAY, DESCEND=DESCEND, STABLE=STABLE)
  END FUNCTION
  !
  !##########################################################################################################################
  ! Helpful routine for ensure P is the proper size and the routine to initialize P
  !
  MODULE PURE SUBROUTINE ALLOCATE_PERM_INT64(A, P)
    INTEGER(INT64), dimension(:),              intent(in   ) :: A
    INTEGER,        dimension(:), allocatable, intent(inout) :: P
    INTEGER:: dim
    !
    dim = SIZE(A, 1)
    !
    IF( .NOT. ALLOCATED(P) ) THEN
                               allocate(P(dim))
    ELSE
        IF(SIZE(P, 1) < dim ) THEN
                                deallocate(P)
                                  allocate(P(dim))
        END IF
    END IF
    P = 0
    !
  END SUBROUTINE
  !
  !--------------------------------------------------------------------------------------------------------------------------
  ! Initialize P
  !
  PURE SUBROUTINE INIT_PERMUTATION(dim, P)
    INTEGER,                 intent(in   ) :: dim
    INTEGER, dimension(dim), intent(inout) :: P
    INTEGER:: I
    DO I=1, dim
            P(I) = I
    END DO
  END SUBROUTINE
  !
  !
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !
  ! 1D Insertion Sort       (ISORT)    and
  ! 1D Dual Pivot Quicksort (QSORT)    and    -> Techically its an IntroSort with Dual Pivot Quick and that switches to Heap and and Insertion Sorts
  ! 1D Symmetric Merge Sort (SymMerge) and
  ! 1D Heap Sort            (HEAPSORT) Routines
  !
  !##########################################################################################################################
  ! ISORT Routines
  !
  PURE SUBROUTINE ISORT_1D_INT64(dim,A)
    INTEGER,                       intent(in   ) :: dim
    INTEGER(INT64), dimension(dim),intent(inout) :: A
    INTEGER(INT64):: TMP
    INTEGER:: I, J
    !
    DO I=2, dim
        TMP = A(I)
        J   = I - 1
        DO WHILE ( J >= 1 )
                   IF( TMP >= A(J) ) EXIT
                   A(J+1) = A(J)
                   J = J - 1
        END DO
        A(J+1) = TMP
    END DO
    !
  END SUBROUTINE
  !
  !--------------------------------------------------------------------------------------------------------------------------
  ! ISORT with Index Position Array that keeps track of where value origianted from
  !
  PURE SUBROUTINE ISORT_1P_INT64(dim, A, P)
    INTEGER,                       intent(in   ) :: dim
    INTEGER(INT64), dimension(dim),intent(inout) :: A   ! Array to be sorted
    INTEGER,        dimension(dim),intent(inout) :: P   ! Index Position Array
    INTEGER(INT64):: TMP
    INTEGER:: POS
    INTEGER:: I, J
    !
    DO I=2, dim
        POS = P(I)
        TMP = A(I)
        J   = I - 1
        DO WHILE ( J >= 1 )
                   IF( TMP >= A(J) ) EXIT
                   A(J+1) = A(J)
                   P(J+1) = P(J)
                   J = J - 1
        END DO
        A(J+1) = TMP
        P(J+1) = POS
    END DO
    !
  END SUBROUTINE
  !
  !##########################################################################################################################
  ! QSORT Partition routine called by QSORT base routine
  !
  PURE SUBROUTINE PARTITION_INT64(dim, A, L, R, I, J)
    INTEGER,                        intent(in   ) :: dim, L, R
    INTEGER(INT64), dimension(dim), intent(inout) :: A
    INTEGER,                        intent(inout) :: I, J ! Pivot locations
    INTEGER(INT64):: tmp, Pivot1, Pivot2       ! Pivot Values
    INTEGER:: K
    INTEGER:: e1 ! midpoint - 2*dim/7
    INTEGER:: e2 ! midpoint - dim/7
    INTEGER:: e3 ! midpoint
    INTEGER:: e4 ! midpoint + dim/7
    INTEGER:: e5 ! midpoint + 2*dim/7
    !
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ! Find Pivot Values
    !
    K = R - L + 1 ! Length of QSORT Partition
    !
    I = SHIFTR( K, 3 ) + SHIFTR( K, 6 ) + 1 ! = dim/8 + dim/64 + 1 ~= CEIL(dim/7)
    !
    e3 = L + SHIFTR( K, 1 )  ! = L + dim/2 - 1 ~= Midpoint
    e2 = e3 - I ! mid - dim/7
    e1 = e2 - I ! mid - dim/7 - dim/7
    e4 = e3 + I ! mid + dim/7
    e5 = e4 + I ! mid + dim/7 + dim/7
    !
    IF(e1 < L) e1 = L
    IF(e2 < L) e2 = L
    IF(e3 < L) e3 = L
    !
    IF(e3 > R) e3 = R
    IF(e4 > R) e4 = R
    IF(e5 > R) e5 = R
    !
    ! Poor Man's SORT of Tercile guesses ------------------------------------
    !
    IF( A(e2) < A(e1) ) CALL SWAP_INT64( dim, A, e1, e2 ) ! SWAP_INT64( A(e1), A(e2) )
    !
    IF( A(e3) < A(e2) ) THEN
                      tmp = A(e3);  A(e3) = A(e2);  A(e2) = tmp
                      !
                      IF(tmp < A(e1)) THEN
                                    A(e2) = A(e1);  A(e1) = tmp
                      END IF
    END IF
    !
    IF( A(e4) < A(e3) ) THEN
                      tmp = A(e4);  A(e4) = A(e3);  A(e3) = tmp
                      !
                      IF(tmp < A(e2)) THEN
                                    A(e3) = A(e2);  A(e2) = tmp
                                    IF(tmp < A(e1)) THEN
                                                  A(e2) = A(e1);  A(e1) = tmp
                                    END IF
                      END IF
    END IF
    !
    IF( A(e5) < A(e4) ) THEN
                      tmp = A(e5);  A(e5) = A(e4);  A(e4) = tmp
                      !
                      IF(tmp < A(e3)) THEN
                                    A(e4) = A(e3);  A(e3) = tmp
                                    IF(tmp < A(e2)) THEN
                                                  A(e3) = A(e2);  A(e2) = tmp
                                                  IF(tmp < A(e1)) THEN
                                                                A(e2) = A(e1);  A(e1) = tmp
                                                  END IF
                                    END IF
                      END IF
    END IF
    !------------------------------------------------------------------------
    !
    ! Chosen pivots are e2 and e4, which are inexpensive approximation of
    !   the first and second terciles of the array.
    !
    Pivot1 = A(e2)  ! -> Lower Branch -- Best if closest to lower tercile
    Pivot2 = A(e4)  ! -> Upper Branch -- Best if closest to upper tercile
    !
    A(e2) = A(L)
    A(e4) = A(R)
    !
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ! Partition Code
    !
    I = L + 1
    J = R - 1
    !
    ! Partition Location -> A(I), A(J)         => L < I < J < R
    !
    DO WHILE ( I<J .AND. A(I) < Pivot1 )
       I = I + 1
    END DO
    DO WHILE ( I<J .AND. A(J) >= Pivot2 )
       J = J - 1
    END DO
    !
    K = I
    DO WHILE ( K <= J )
        !
        IF    ( A(K) <  Pivot1 ) THEN  ! Less than the left pivot
                                 tmp = A(I);   A(I) = A(K);   A(K) = tmp              ! Swap A(K) and A(I)
                                 I = I + 1
        ELSEIF( A(K) >= Pivot2 ) THEN  ! Greater than the right pivot
                            DO WHILE ( K < J .AND. A(J) > Pivot2 )
                               J = J - 1
                            END DO
                            tmp = A(K);   A(K) = A(J);   A(J) = tmp                   ! Swap A(K) and A(J)
                            J = J - 1
                            !
                            IF ( A(K) < Pivot1 ) THEN  ! Less than the left pivot
                                            tmp = A(I);   A(I) = A(K);   A(K) = tmp   ! Swap A(K) and A(I)
                                            I = I + 1
                            END IF
        END IF
        !
        K = K + 1
        !
    END DO
    !
    ! Move Pivot to Partition Location
    I = I - 1
    J = J + 1
    !
    A(L) = A(I)
    A(I) = Pivot1
    !
    A(R) = A(J)
    A(J) = Pivot2
    !
    ! End Partition Code
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  END SUBROUTINE
  !
  !--------------------------------------------------------------------------------------------------------------------------
  ! QSORT Partition routine with Index Position Array
  !      called by QSORT base routine
  !
  PURE SUBROUTINE PARTITION_wP_INT64(dim, A, P, L, R, I, J)
    INTEGER,                        intent(in   ) :: dim, L, R
    INTEGER(INT64), dimension(dim), intent(inout) :: A
    INTEGER,        dimension(dim), intent(inout) :: P
    INTEGER,                        intent(inout) :: I, J ! Pivot locations
    INTEGER(INT64):: tmp, Pivot1, Pivot2                  ! Pivot Values
    INTEGER:: K, pos
    INTEGER:: e1 ! midpoint - 2*dim/7
    INTEGER:: e2 ! midpoint - dim/7
    INTEGER:: e3 ! midpoint
    INTEGER:: e4 ! midpoint + dim/7
    INTEGER:: e5 ! midpoint + 2*dim/7
    !
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ! Find Pivot Values
    !
    K = R - L + 1 ! Length of QSORT Partition
    !
    I = SHIFTR( K, 3 ) + SHIFTR( K, 6 ) + 1 ! = dim/8 + dim/64 + 1 ~= CEIL(dim/7)
    !
    e3 = L + SHIFTR( K, 1 )  ! = L + dim/2 - 1 ~= Midpoint
    e2 = e3 - I ! mid - dim/7
    e1 = e2 - I ! mid - dim/7 - dim/7
    e4 = e3 + I ! mid + dim/7
    e5 = e4 + I ! mid + dim/7 + dim/7
    !
    IF(e1 < L) e1 = L
    IF(e2 < L) e2 = L
    IF(e3 < L) e3 = L
    !
    IF(e3 > R) e3 = R
    IF(e4 > R) e4 = R
    IF(e5 > R) e5 = R
    !
    ! Poor Man's SORT of Tercile guesses ------------------------------------
    !
    IF( A(e2) < A(e1) ) CALL SWAP_w_PERM(dim, A, P, e1, e2)
    !
    IF( A(e3) < A(e2) ) THEN
                      tmp = A(e3);  A(e3) = A(e2);  A(e2) = tmp
                      pos = P(e3);  P(e3) = P(e2);  P(e2) = pos
                      !
                      IF(tmp < A(e1)) THEN
                                    A(e2) = A(e1);  A(e1) = tmp
                                    P(e2) = P(e1);  P(e1) = pos
                      END IF
    END IF
    !
    IF( A(e4) < A(e3) ) THEN
                      tmp = A(e4);  A(e4) = A(e3);  A(e3) = tmp
                      pos = P(e4);  P(e4) = P(e3);  P(e3) = pos
                      !
                      IF(tmp < A(e2)) THEN
                                    A(e3) = A(e2);  A(e2) = tmp
                                    P(e3) = P(e2);  P(e2) = pos
                                    IF(tmp < A(e1)) THEN
                                                  A(e2) = A(e1);  A(e1) = tmp
                                                  P(e2) = P(e1);  P(e1) = pos
                                    END IF
                      END IF
    END IF
    !
    IF( A(e5) < A(e4) ) THEN
                      tmp = A(e5);  A(e5) = A(e4);  A(e4) = tmp
                      pos = P(e5);  P(e5) = P(e4);  P(e4) = pos
                      !
                      IF(tmp < A(e3)) THEN
                                    A(e4) = A(e3);  A(e3) = tmp
                                    P(e4) = P(e3);  P(e3) = pos
                                    IF(tmp < A(e2)) THEN
                                                  A(e3) = A(e2);  A(e2) = tmp
                                                  P(e3) = P(e2);  P(e2) = pos
                                                  IF(tmp < A(e1)) THEN
                                                                A(e2) = A(e1);  A(e1) = tmp
                                                                P(e2) = P(e1);  P(e1) = pos
                                                  END IF
                                    END IF
                      END IF
    END IF
    !------------------------------------------------------------------------
    !
    ! Chosen pivots are e2 and e4, which are inexpensive approximation of
    !   the first and second terciles of the array.
    !
    Pivot1 = A(e2)  ! -> Lower Branch -- Best if closest to lower tercile
    Pivot2 = A(e4)  ! -> Upper Branch -- Best if closest to upper tercile
    !
    A(e2) = A(L)
    A(e4) = A(R)
    !
    e1 = P(e2) ! Backup of index at pivots
    e5 = P(e4) !
    !
    P(e2) = P(L)
    P(e4) = P(R)
    !
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ! Partition Code
    !
    I = L + 1
    J = R - 1
    !
    ! Partition Location -> A(I), A(J)         => L < I < J < R
    !
    DO WHILE ( I<J .AND. A(I) < Pivot1 )
       I = I + 1
    END DO
    DO WHILE ( I<J .AND. A(J) >= Pivot2 )
       J = J - 1
    END DO
    !
    K = I
    DO WHILE ( K <= J )
        !
        IF    ( A(K) <  Pivot1 ) THEN  ! Less than the left pivot
                                 tmp = A(I);   A(I) = A(K);   A(K) = tmp              ! Swap A(K) and A(I)
                                 pos = P(I);   P(I) = P(K);   P(K) = pos
                                 I = I + 1
        ELSEIF( A(K) >= Pivot2 ) THEN  ! Greater than the right pivot
                            DO WHILE ( K < J .AND. A(J) > Pivot2 )
                               J = J - 1
                            END DO
                            tmp = A(K);   A(K) = A(J);   A(J) = tmp                   ! Swap A(K) and A(J)
                            pos = P(K);   P(K) = P(J);   P(J) = pos
                            J = J - 1
                            !
                            IF ( A(K) < Pivot1 ) THEN  ! Less than the left pivot
                                            tmp = A(I);   A(I) = A(K);   A(K) = tmp   ! Swap A(K) and A(I)
                                            pos = P(I);   P(I) = P(K);   P(K) = pos
                                            I = I + 1
                            END IF
        END IF
        !
        K = K + 1
        !
    END DO
    !
    ! Move Pivot to Partition Location
    I = I - 1
    J = J + 1
    !
    A(L) = A(I)
    A(I) = Pivot1
    !
    A(R) = A(J)
    A(J) = Pivot2
    !
    P(L) = P(I)
    P(I) = e1
    !
    P(R) = P(J)
    P(J) = e5
    !
    ! End Partition Code
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  END SUBROUTINE
  !
  !##########################################################################################################################
  ! QSORT base routines
  !
  RECURSIVE PURE SUBROUTINE QSORT_1D_INT64(dim, A, L, R, D)
    INTEGER,                        intent(in   ) :: dim
    INTEGER(INT64), dimension(dim), intent(inout) :: A
    INTEGER,                        intent(in   ) :: L, R   ! Lower Bound, Upper Bound -> Starting left index and right index
    INTEGER,                        intent(in   ) :: D      ! Depth recursion limit, once zero switch to Heap Sort
    INTEGER:: I, J
    !
    I = R - L + 1                        ! Length of QSORT Partition
    IF( I  < 2 ) RETURN                  ! No Partition -> Return
    IF( I == 2 ) THEN                    ! Simple Flip Check
                 IF( A(L) > A(R) ) CALL SWAP_INT64( dim, A, L, R ) !SWAP_INT64( A(L), A(R) )   !Swap A(L) and A(R)
                 RETURN
    END IF
    !
    IF( I <= QSORT_PARTITION_MIN) THEN   ! => Do Insertion Sort and Return   -- Side note: (R - L + 1 <= 16)
       CALL ISORT_1D_INT64( I, A(L:R) )
       RETURN
    END IF
    !
    IF( D < 1 ) THEN   ! No more recursion allowed, switch to Heap Sort
       CALL HEAPSORT_1D_INT64( I, A(L:R) )
       RETURN
    END IF
    !
    !  A(I) => Pivot 1 -> Lower Branch -- Best if closest to lower tercile
    !  A(J) => Pivot 2 -> Upper Branch -- Best if closest to upper tercile
    !
    CALL PARTITION_INT64(dim, A, L, R, I, J)
    !
    CALL QSORT_1D_INT64(dim, A, L  , I-1, D-1)
    CALL QSORT_1D_INT64(dim, A, I+1, J-1, D-1)
    CALL QSORT_1D_INT64(dim, A, J+1,   R, D-1)
    !
  END SUBROUTINE
  !
  !--------------------------------------------------------------------------------------------------------------------------
  ! QSORT with Index Position Array that keeps track of where value origianted from
  !
  RECURSIVE PURE SUBROUTINE QSORT_1P_INT64(dim, A, P, L, R, D)
    INTEGER,                        intent(in   ) :: dim
    INTEGER(INT64), dimension(dim), intent(inout) :: A
    INTEGER,        dimension(dim), intent(inout) :: P
    INTEGER,                        intent(in   ) :: L, R   ! Lower Bound, Upper Bound -> Starting left index and right index
    INTEGER,                        intent(in   ) :: D      ! Depth recursion limit, once zero switch to Heap Sort
    INTEGER:: I, J, K
    !
    K = R - L + 1                        ! Length of QSORT Partition
    IF( K  < 2 ) RETURN                  ! No Partition -> Return
    IF( K == 2 ) THEN                    ! Simple Flip Check
                 IF( A(L) > A(R) ) CALL SWAP_w_PERM(dim, A, P, L, R)   ! Swap A(L) and A(R)
                 RETURN
    END IF
    !
    IF( K <= QSORT_PARTITION_MIN) THEN   ! => Do Insertion Sort and Return   -- Side note: (R - L + 1 <= 16)
       CALL ISORT_1P_INT64( K, A(L:R), P(L:R) )
       RETURN
    END IF
    !
    IF( D < 1 ) THEN   ! No more recursion allowed, switch to Heap Sort
       CALL HEAPSORT_1P_INT64( K, A(L:R), P(L:R) )
       RETURN
    END IF
    !
    !  Pivot 1 -> Lower Branch -- Best if closest to lower tercile
    !  Pivot 2 -> Upper Branch -- Best if closest to upper tercile
    !
    CALL PARTITION_wP_INT64(dim, A, P, L, R, I, J)
    !
    CALL QSORT_1P_INT64(dim, A, P, L  , I-1, D-1)
    CALL QSORT_1P_INT64(dim, A, P, I+1, J-1, D-1)
    CALL QSORT_1P_INT64(dim, A, P, J+1,   R, D-1)
    !
  END SUBROUTINE
  !
  !##########################################################################################################################
  ! HEAPSORT base routines         -> Also called when QSORT has too many recursive calls
  !
  PURE SUBROUTINE HEAPSORT_1D_INT64(dim, A)
    INTEGER,                        intent(in   ) :: dim
    INTEGER(INT64), dimension(dim), intent(inout) :: A
    INTEGER(INT64):: TMP
    INTEGER:: I, P, C
    !
    HEAPIFY: DO I=dim/2, 1, -1
       P = I    ! Parent
       C = 2*I  ! Child
       DO WHILE ( C <= dim )   ! Siftdown the heap
           !
           IF( C < dim) THEN
                   IF ( A(C) < A(C+1) ) C = C + 1  ! 2nd child is bigger
           END IF
           !
           IF( A(P) >= A(C) ) CYCLE HEAPIFY
           !
           TMP  = A(P)
           A(P) = A(C)
           A(C) = TMP
           !
           P = C
           C = 2*P
       END DO
    END DO HEAPIFY
    !
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    !
    POPPER: DO I=dim-1, 1, -1
       !
       TMP    = A(1)
       A(1)   = A(I+1)  !Pop the last value
       A(I+1) = TMP
       !
       P = 1    ! Parent
       C = 2    ! Child
       DO WHILE ( C <= I )   ! Siftdown the heap
           !
           IF( C < I) THEN
                   IF ( A(C) < A(C+1) ) C = C + 1  ! 2nd child is bigger
           END IF
           !
           IF( A(P) >= A(C) ) CYCLE POPPER
           !
           TMP  = A(P)
           A(P) = A(C)
           A(C) = TMP
           !
           P = C
           C = 2*P
       END DO
       !
    END DO POPPER
    !
  END SUBROUTINE
  !
  !--------------------------------------------------------------------------------------------------------------------------
  ! HEAPSORT with Index Position Array that keeps track of where value origianted from
  !
  PURE SUBROUTINE HEAPSORT_1P_INT64(dim, A, PRM)
    INTEGER,                        intent(in   ) :: dim
    INTEGER(INT64), dimension(dim), intent(inout) :: A
    INTEGER,        dimension(dim), intent(inout) :: PRM
    INTEGER(INT64):: TMP
    INTEGER:: POS
    INTEGER:: I, P, C
    !
    HEAPIFY: DO I=dim/2, 1, -1
       P = I    ! Parent
       C = 2*I  ! Child
       DO WHILE ( C <= dim )   ! Siftdown the heap
           !
           IF( C < dim) THEN
                   IF ( A(C) < A(C+1) ) C = C + 1  ! 2nd child is bigger
           END IF
           !
           IF( A(P) >= A(C) ) CYCLE HEAPIFY
           !
           TMP  = A(P)
           A(P) = A(C)
           A(C) = TMP
           !
           POS    = PRM(P)
           PRM(P) = PRM(C)
           PRM(C) = POS
           !
           P = C
           C = 2*P
       END DO
    END DO HEAPIFY
    !
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    !
    POPPER: DO I=dim-1, 1, -1
       !
       TMP    = A(1)
       A(1)   = A(I+1)  !Pop the last value
       A(I+1) = TMP
       !
       POS      = PRM(1)
       PRM(1)   = PRM(I+1)
       PRM(I+1) = POS
       !
       P = 1    ! Parent
       C = 2    ! Child
       DO WHILE ( C <= I )   ! Siftdown the heap
           !
           IF( C < I) THEN
                   IF ( A(C) < A(C+1) ) C = C + 1  ! 2nd child is bigger
           END IF
           !
           IF( A(P) >= A(C) ) CYCLE POPPER
           !
           TMP  = A(P)
           A(P) = A(C)
           A(C) = TMP
           !
           POS    = PRM(P)
           PRM(P) = PRM(C)
           PRM(C) = POS
           !
           P = C
           C = 2*P
       END DO
       !
    END DO POPPER
    !
  END SUBROUTINE
  !
  !##########################################################################################################################
  ! SymMerge  Binary Search routine
  !
  PURE FUNCTION SYM_BSEARCH(dim, A, L, R, N) RESULT(m)
    INTEGER,                        intent(in) :: dim
    INTEGER(INT64), dimension(dim), intent(in) :: A
    INTEGER,                             value :: L, R
    INTEGER,                        intent(in) :: N
    INTEGER:: m
    DO WHILE (L < R)
              m = SHIFTR(L+R, 1)
              IF( A(m) > A(N-m) ) THEN
                               R = m
              ELSE
                               L = m + 1
              END IF
    END DO
    m = L
  END FUNCTION
  !
  !##########################################################################################################################
  ! SymMerge  routines without P
  !
  PURE SUBROUTINE SymMerge_1D_INT64(dim, A)
    INTEGER,                        intent(in   ) :: dim
    INTEGER(INT64), dimension(dim), intent(inout) :: A
    INTEGER:: I, nblock, nblock2, nb, N
    !
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ - Not necessary cause checks already applied
    !IF( dim < 65 ) THEN
    !      CALL ISORT_1D_INT64(dim, A)
    !      RETURN
    !END IF
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    !
    nblock = 32
    !
    N = nblock*(dim/nblock)  ! Array split as A(1:N);     which is a multiple of nblock
    !                                  and as A(N+1:dim); which is the left over/extra space
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    !
    nb   = nblock-1
    !DO CONCURRENT (INTEGER:: I = 1 : N : nblock) --> Compiler error for large N
    DO I = 1, N, nblock
                 CALL ISORT_1D_INT64(nblock, A(I:I+nb))
    END DO
    nb = dim-N
    IF( nb > 1 )  CALL ISORT_1D_INT64(nb, A(N+1:dim))
    !
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    DO WHILE (nblock < dim)
        !
        nblock2 = 2*nblock  ! Merge two blocks
        !
        N   = nblock2*(dim/nblock2)  ! Update N for two block merge
        nb    = nblock2-1
        !
        DO I = 1, N, nblock2
                     CALL SymMerge_INT64(dim, A, I, I+nblock, I+nb)
        END DO
        N  = N + 1
        nb = N+nblock
        IF( nb <= dim )  CALL SymMerge_INT64(dim, A, N, nb, dim)
        !
        nblock = nblock2
    END DO
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    !
  END SUBROUTINE
  !
  !--------------------------------------------------------------------------------------------------------------------------
  ! Symmetric Merge of two arrays partitioned as A(fi:si-1) and A(si,li)
  !
  !    fi = first position;   si = second position;   li = last position
  !
  RECURSIVE PURE SUBROUTINE SymMerge_INT64(dim, A, fi, si, li)
    INTEGER,                        intent(in   ) :: dim
    INTEGER(INT64), dimension(dim), intent(inout) :: A
    INTEGER,                        intent(in   ) :: fi, si, li
    INTEGER:: n, m
    INTEGER:: strt, term
    !
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
    if ( fi >= si .OR. si > li ) RETURN                                                            ! Check 1 -> Only one value, no sort necessary
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
    if ( A(si-1) <= A(si) ) RETURN                                                                 ! Check 2 -> Left and Right Blocks already ordered
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
    if (li - fi == 1) then                                                                         ! Check 3 -> Only two values and from to Check 2, A(fi) > A(si) = A(li)
                      CALL SWAP_INT64( dim, A, fi, li )  ! Check 2 failure implies: A(fi) > A(li)  !            SWAP_INT64( A(fi), A(li) )
                      RETURN                                                                       !
    end if                                                                                         !
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
    if( A(fi) > A(li) ) then                                                                       ! Check 4 -> Right block smaller than left => Rotate them
                          CALL RotrateBlock(dim, A, fi, si, li)   ! Flip Blocks                    !
                          RETURN                                                                   !
    END IF                                                                                         !
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
    if (si == li) then                                                                             ! Check 5 -> Right block only contains one value. Check indicates it needs to be placed in left block.
        n = fi;   m = si                                                                           !              Binary search to find the lowest index n such that A[n] >= A[fi] for si <= n < li.
        DO WHILE (n < m)                                                                           !
                 strt = SHIFTR( n+m, 1 )                                                           !
                 !                                                                                 !
                 if( A(strt) <= A(si) ) then                                                       !
                     n  = strt + 1                                                                 !
                 else                                                                              !
                     m = strt                                                                      !
                 end if                                                                            !
        END DO                                                                                     !
        !                Move End point to correct poition                                         !
        DO m=si-1, n, -1                                                                           !
                    CALL SWAP_INT64(dim, A, m, m+1)                                                !  SWAP_INT64(A(m), A(m+1))
        END DO                                                                                     !
        RETURN                                                                                     !
    end if                                                                                         !
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
    !
    m = SHIFTR( fi+li, 1 ) ! <- midpoint; If (fi+li) overflow is possible, use this instead:  = midpoint(fi, li)
    n = m + si
    !
    if (si > m) then
        strt = SYM_BSEARCH(dim, A, n-li,  m+1, n) ! The other option is to do si-1, but SYM_BSEARCH would have to change to DO WHILE (L <= R) and include a check for L==R
    else
        strt = SYM_BSEARCH(dim, A,   fi, si, n)
    end if
    term = n - strt
    !
    IF ( fi <= strt .AND. term <= li ) CALL RotrateBlock(dim, A, strt, si, term)
    !
    CALL SymMerge_INT64(dim, A, fi, strt, m)
    !
    m = m + 1;  term = term + 1
    !
    CALL SymMerge_INT64(dim, A, m, term, li)
    !
  END SUBROUTINE
  !
  !##########################################################################################################################
  ! SymMerge  routines with P
  !
  PURE SUBROUTINE SymMerge_1P_INT64(dim, A, P)
    INTEGER,                        intent(in   ) :: dim
    INTEGER(INT64), dimension(dim), intent(inout) :: A
    INTEGER,        dimension(dim), intent(inout) :: P
    INTEGER:: I, nblock, nblock2, nb, N
    !
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    !IF( dim < 65 ) THEN
    !      CALL ISORT_1P_INT64(dim, A, P)
    !      RETURN
    !END IF
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    !
    nblock = 32
    !
    N = nblock*(dim/nblock)  ! Array split as A(1:N);     which is a multiple of nblock
    !                                  and as A(N+1:dim); which is the left over/extra space
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    !
    nb   = nblock-1
    !
    DO I = 1, N, nblock
                 CALL ISORT_1P_INT64(nblock, A(I:I+nb), P(I:I+nb))
    END DO
    nb = dim-N
    IF( nb > 1 )  CALL ISORT_1P_INT64(nb, A(N+1:dim), P(N+1:dim))
    !
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    DO WHILE (nblock < dim)
        !
        nblock2 = 2*nblock  ! Merge two blocks
        !
        N   = nblock2*(dim/nblock2)  ! Update N for two block merge
        nb    = nblock2-1
        !
        DO I = 1, N, nblock2
                     CALL SymMergeP_INT64(dim, A, P, I, I+nblock, I+nb)
        END DO
        N  = N + 1
        nb = N+nblock
        IF( nb <= dim )  CALL SymMergeP_INT64(dim, A, P, N, nb, dim)
        !
        nblock = nblock2
    END DO
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    !
  END SUBROUTINE
  !
  !--------------------------------------------------------------------------------------------------------------------------
  ! Symmetric Merge of two arrays with P
  !
  RECURSIVE PURE SUBROUTINE SymMergeP_INT64(dim, A, P, fi, si, li)
    INTEGER,                        intent(in   ) :: dim
    INTEGER(INT64), dimension(dim), intent(inout) :: A        ! Array to be sorted
    INTEGER,        dimension(dim), intent(inout) :: P        ! Index Position Array
    INTEGER,                        intent(in   ) :: fi, si, li
    INTEGER:: n, m
    INTEGER:: strt, term
    !
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
    if ( fi >= si .OR. si > li ) RETURN                                                            ! Check 1 -> Only one value, no sort necessary
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
    if ( A(si-1) <= A(si) ) RETURN                                                                 ! Check 2 -> Left and Right Blocks already ordered
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
    if (li - fi == 1) then                                                                         ! Check 3 -> Only two values and from to Check 2, A(fi) > A(si) = A(li)
                      CALL SWAP_w_PERM(dim, A, P, fi, li )                                         !            Check 2 failure implies: A(fi) > A(li)
                      RETURN                                                                       !
    end if                                                                                         !
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
    if( A(fi) > A(li) ) then                                                                       ! Check 4 -> Right block smaller than left => Rotate them
                          CALL RotrateBlockP(dim, A, P, fi, si, li)   ! Flip Blocks              !
                          RETURN                                                                   !
    END IF                                                                                         !
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
    if (si == li) then                                                                             ! Check 5 -> Right block only contains one value. Check indicates it needs to be placed in left block.
        n = fi;   m = si                                                                           !              Binary search to find the lowest index n such that A[n] >= A[fi] for si <= n < li.
        DO WHILE (n < m)                                                                           !
                 strt = SHIFTR( n+m, 1 )                                                           !
                 !                                                                                 !
                 if( A(strt) <= A(si) ) then                                                       !
                     n  = strt + 1                                                                 !
                 else                                                                              !
                     m = strt                                                                      !
                 end if                                                                            !
        END DO                                                                                     !
        !                Move End point to correct poition                                         !
        DO m=si-1, n, -1                                                                           !
                    CALL SWAP_w_PERM( dim, A, P, m, m+1 )                                          !
        END DO                                                                                     !
        RETURN                                                                                     !
    end if                                                                                         !
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
    !
    m = SHIFTR( fi+li, 1 ) ! <- midpoint; If (fi+li) overflow is possible, use this instead:  = midpoint(fi, li)
    n = m + si
    !
    if (si > m) then
        strt = SYM_BSEARCH(dim, A, n-li,  m+1, n) ! The other option is to do si-1, but SYM_BSEARCH would have to change to DO WHILE (L <= R) and include a check for L==R
    else
        strt = SYM_BSEARCH(dim, A,   fi, si, n)
    end if
    term = n - strt
    !
    IF ( fi <= strt .AND. term <= li ) CALL RotrateBlockP(dim, A, P, strt, si, term)
    !
    CALL SymMergeP_INT64(dim, A, P, fi, strt, m)
    !
    m = m + 1;  term = term + 1
    !
    CALL SymMergeP_INT64(dim, A, P, m, term, li)
    !
  END SUBROUTINE
  !
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !
  ! Main Driver Sort Routine for 2D Array that sorts on a Specified Column or Row
  !   SORT_INDEX > 0 indicates column to sort on
  !   SORT_INDEX < 0 indicates row    to sort on; ABS(SORT_INDEX) is the row to sort on
  !
  !   Optionally, SORT_BY_ROW overrides SORT_INDEX flag and uses ABS(SORT_INDEX) as the sort row/col
  !
  MODULE PURE SUBROUTINE SORT_2D_INDEX_INT64(A, SORT_INDEX, DESCEND, SORT_BY_ROW, P, NO_STABLE)
    INTEGER(INT64), dimension(:,:), contiguous,  intent(inout) :: A           !
    INTEGER,                                     intent(in   ) :: SORT_INDEX  ! Col or Row to sort, the rest are shifted based on sort. SORT_INDEX > 0 indicates column to sort on; SORT_INDEX < 0 indicates row to sort on; ABS(SORT_INDEX) is the row to sort on.
    LOGICAL,                           optional, intent(in   ) :: DESCEND     ! Sort Col/Row in desecending order, default is False
    LOGICAL,                           optional, intent(in   ) :: SORT_BY_ROW ! If present, overrides sign of SORT_INDEX and either sorts by row (True) or by column.
    INTEGER, dimension(:), contiguous, optional, intent(inout) :: P           ! Permulation vector that produced the sort of SORT_INDEX
    LOGICAL,                           optional, intent(in   ) :: NO_STABLE   ! Default is to do a stable sort (NO_STABLE=FALSE). NO_STABLE=TRUE disables this and uses the fastest sorting algorithm. *Note that if there are repeated values in the sorted col/row, then sort order of the other columns is random.
    INTEGER:: dim1, dim2, sortIDX
    LOGICAL:: SORT_BY_COL, STABLE
    !
    dim1 = SIZE(A, 1)
    dim2 = SIZE(A, 2)
    !
    STABLE = .TRUE.
    if(present(NO_STABLE)) STABLE = .not. NO_STABLE
    !
    IF( dim1 < 1 .OR. dim2 < 1 .OR. SORT_INDEX == 0) RETURN
    !
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    IF( SORT_INDEX > 0 ) THEN
        sortIDX     = SORT_INDEX
        SORT_BY_COL = .TRUE.
    ELSE
        sortIDX     = -1*SORT_INDEX
        SORT_BY_COL = .FALSE.
    END IF
    IF(present(SORT_BY_ROW)) SORT_BY_COL = .NOT. SORT_BY_ROW
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    !
    IF( dim1 == 1 .OR. dim2 == 1 ) THEN  ! Array really is 1D, so use those optimized routines
        IF    (dim1 > 1 .AND.       SORT_BY_COL .AND. sortIDX==1) THEN
              !
              CALL SORT_1D_INT64( A(:,1), DESCEND, STABLE=STABLE, P=P)
              !
        ELSEIF(dim2 > 1 .AND. .NOT. SORT_BY_COL .AND. sortIDX==1) THEN
              !
              CALL SORT_1D_INT64( A(1,:), DESCEND, STABLE=STABLE, P=P)
              !
        ELSEIF(PRESENT(P)) THEN
                           CALL INIT_PERMUTATION(size(P), P)
        END IF
    ELSEIF( SORT_BY_COL ) THEN
        CALL SORT_2D_COL_INT64(dim1, dim2, A, sortIDX, STABLE, DESCEND, P)
    ELSE
        CALL SORT_2D_ROW_INT64(dim1, dim2, A, sortIDX, STABLE, DESCEND, P)
    END IF
    !
  END SUBROUTINE
  !
  !##########################################################################################################################
  ! Sort 2D Array by specified Column; That is dim2 in DIMENSION(dim1,dim2)
  !
  PURE SUBROUTINE SORT_2D_COL_INT64(dim1, dim2, A, COL, STABLE, DESCEND, PRM)
    INTEGER,                                        intent(in   ) :: dim1, dim2, COL
    INTEGER(INT64), dimension(dim1,dim2),           intent(inout) :: A
    LOGICAL,                                        intent(in   ) :: STABLE
    LOGICAL,                              optional, intent(in   ) :: DESCEND
    INTEGER,        dimension(:),         optional, intent(inout) :: PRM
    INTEGER,        dimension(dim1):: P
    INTEGER:: k
    !
    CALL SORT_1D_INT64(A(:,COL), DESCEND, STABLE=STABLE, P=P)
    !
    IF(present(PRM)) THEN
       IF(SIZE(PRM) >= dim1) THEN
               PRM(1:dim1) = P
       ELSE
               PRM = -1            ! Faulty size; return error
       END IF
    END IF
    !
    ! Sort the other columns by permutation vector
    DO k=1, COL-1
                CALL PERM_SORT(dim1, A(:,k), P)
    END DO
    DO k=COL+1, dim2
                CALL PERM_SORT(dim1, A(:,k), P)
    END DO
    !
  END SUBROUTINE
  !
  !--------------------------------------------------------------------------------------------------------------------------
  ! Sort 2D Array by specified Row; That is dim1 in DIMENSION(dim1,dim2)
  !
  PURE SUBROUTINE SORT_2D_ROW_INT64(dim1, dim2, A, ROW, STABLE, DESCEND, PRM)
    INTEGER,                                        intent(in   ) :: dim1, dim2, ROW
    INTEGER(INT64), dimension(dim1,dim2),           intent(inout) :: A
    LOGICAL,                                        intent(in   ) :: STABLE
    LOGICAL,                              optional, intent(in   ) :: DESCEND
    INTEGER,        dimension(:),         optional, intent(inout) :: PRM
    INTEGER,        dimension(dim2):: P
    INTEGER(INT64), dimension(max(dim1,dim2)):: TMP
    INTEGER:: i, j, k
    !
    call ROW_COPY(dim1, dim2, row, A, TMP)
    !DO CONCURRENT (J=1:dim2)
    !                   TMP(J) = A(ROW,J)
    !END DO
    !
    CALL SORT_1D_INT64(TMP(1:dim2), DESCEND, STABLE=STABLE, P=P)
    !
    IF(present(PRM)) THEN
       IF(SIZE(PRM) >= dim2) THEN
               PRM(1:dim2) = P
       ELSE
               PRM = -1            ! Faulty size; return error
       END IF
    END IF
    !
    ! Sort all the rows by permutation vector *Note that P is not restored after sort
    !
    DO i=1, dim2
        if( P(i)  < 1 .OR. P(i) == i) cycle
        !
        j  = i     ! Current Index
        k  = P(i)  ! Next Index
        TMP(1:dim1) = A(:,i)
        !
        do while ( k /= i .AND. k > 0)  ! Continue until we have found the i'th position - k > 0 is only true for a bad permutation vector
                          !
                          A(:,j) = A(:,k)
                          !
                          P(j) = -P(j)  ! Flip sign to flag as being used
                          !
                          j = k          ! Current Index
                          k = P(k)     ! Next Index
        end do
        A(:,j) = TMP(1:dim1)
        P(j)   = -P(j)
    END DO
    !
  END SUBROUTINE
  !
  !
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !
  ! Main Driver Sort Routine for 2D Array that sorts on a set of Specified Columns or Rows
  !   SORT_INDICES = [ SORT_INDEX1, SORT_INDEX2, SORT_INDEX3, ... ]
  !
  !   SORT_INDEX1 > 0 indicates all are sorted by column
  !   SORT_INDEX1 < 0 indicates all are sorted by row     -> ABS(SORT_INDEX) are the rows to sort on
  !
  !   Optionally, SORT_BY_ROW overrides SORT_INDEX1 flag and uses ABS(SORT_INDICES) as the sort row/col
  !
  MODULE PURE SUBROUTINE SORT_2D_MULTI_INT64_1DSC(A, SORT_INDICES, DESCEND, SORT_BY_ROW, P, NO_STABLE)
    INTEGER(INT64), dimension(:,:), contiguous,  intent(inout) :: A
    INTEGER,        dimension(:),   contiguous,  intent(in   ) :: SORT_INDICES ! Columns or Rows to sort, the rest are shifted based on sort. SORT_INDICES(1) > 0 indicates column to sort on; SORT_INDICES(1) < 0 indicates row to sort on; ABS(SORT_INDICES) is the rows to sort on.
    LOGICAL,                                     intent(in   ) :: DESCEND      ! Sort Col/Row in desecending order, default is False
    LOGICAL,                           optional, intent(in   ) :: SORT_BY_ROW  ! If present, overrides sign of SORT_INDICES(1) and either sorts by row (True) or by column.
    INTEGER, dimension(:), contiguous, optional, intent(inout) :: P            ! Permulation vector that produced the sort of SORT_INDEX
    LOGICAL,                           optional, intent(in   ) :: NO_STABLE    ! Default is to do a stable sort (NO_STABLE=FALSE). NO_STABLE=TRUE disables this and uses the fastest sorting algorithm. *Note that if there are repeated values in the sorted col/row, then sort order of the other columns is random.
    LOGICAL,dimension(SIZE(SORT_INDICES)):: DESCEND_ORDER
    DESCEND_ORDER(:) = DESCEND
    CALL SORT_2D_MULTI_INT64(A, SORT_INDICES, DESCEND_ORDER, SORT_BY_ROW, P, NO_STABLE)
  END SUBROUTINE
  !
  MODULE PURE SUBROUTINE SORT_2D_MULTI_INT64(A, SORT_INDICES, DESCEND, SORT_BY_ROW, P, NO_STABLE)    ! SORT_INDICES must contain unique values
    INTEGER(INT64), dimension(:,:), contiguous,  intent(inout) :: A
    INTEGER,        dimension(:),   contiguous,  intent(in   ) :: SORT_INDICES ! Columns or Rows to sort, the rest are shifted based on sort. SORT_INDICES(1) > 0 indicates column to sort on; SORT_INDICES(1) < 0 indicates row to sort on; ABS(SORT_INDICES) is the rows to sort on.
    LOGICAL, dimension(:), contiguous, optional, intent(in   ) :: DESCEND      ! Sort Col/Row in desecending order, default is False
    LOGICAL,                           optional, intent(in   ) :: SORT_BY_ROW  ! If present, overrides sign of SORT_INDICES(1) and either sorts by row (True) or by column.
    INTEGER, dimension(:), contiguous, optional, intent(inout) :: P            ! Permulation vector that produced the sort of SORT_INDEX
    LOGICAL,                           optional, intent(in   ) :: NO_STABLE    ! Default is to do a stable sort (NO_STABLE=FALSE). NO_STABLE=TRUE disables this and uses the fastest sorting algorithm. *Note that if there are repeated values in the sorted col/row, then sort order of the other columns is random.
    INTEGER, dimension(SIZE(SORT_INDICES)):: SORT_DIM
    LOGICAL, dimension(SIZE(SORT_INDICES)):: DCEND
    INTEGER:: dim1, dim2, sdim
    LOGICAL:: SORT_BY_COL, STABLE
    !
    dim1 = SIZE(A, 1)
    dim2 = SIZE(A, 2)
    sdim = SIZE(SORT_INDICES)
    !
    STABLE = .TRUE.
    if(present(NO_STABLE)) STABLE = .not. NO_STABLE
    !
    IF( dim1 < 1 .OR. dim2 < 1 .OR. SORT_INDICES(1) == 0) THEN
                                                    IF(PRESENT(P)) CALL INIT_PERMUTATION(size(P), P)
                                                    RETURN
    END IF
    !
    if( present(DESCEND) ) then
       IF( size(DESCEND) /= sdim ) RETURN
        DCEND = DESCEND
    else
        DCEND = .FALSE.
    end if
    !
    IF( sdim == 1 .OR. dim1 == 1 .OR. dim2 == 1) THEN   ! Do single index sort or array is 1D, so use those optimized routines
        CALL SORT_2D_INDEX_INT64(A, SORT_INDICES(1), DCEND(1), SORT_BY_ROW, P, NO_STABLE)
        RETURN
    END IF
    !
    CALL COPY_ABS(sdim, SORT_INDICES, SORT_DIM)  ! SORT_DIM = ABS(SORT_INDICES)
    !
    CALL PREP_SORT_DIM(sdim, SORT_DIM, sdim)     ! Remove duplicates and shift zeros to right of positive values
    !
    SORT_BY_COL = SORT_INDICES(1) > 0
    IF(present(SORT_BY_ROW)) SORT_BY_COL = .NOT. SORT_BY_ROW
    !
    IF( SORT_BY_COL ) THEN
             CALL SORT_2D_COLDIM_INT64(dim1,dim2,A,sdim,SORT_DIM,DCEND,STABLE,P)
    ELSE
             CALL SORT_2D_ROWDIM_INT64(dim1,dim2,A,sdim,SORT_DIM,DCEND,STABLE,P)
    END IF
    !
  END SUBROUTINE
  !
  !##########################################################################################################################
  ! Sort 2D Array by specified Columns; That is dim2 in DIMENSION(dim1,dim2)
  !
  PURE SUBROUTINE SORT_2D_COLDIM_INT64(dim1, dim2, A, sdim, COLDIM, DESCEND, STABLE, PRM)
    INTEGER,                                   intent(in   ) :: dim1, dim2, sdim
    INTEGER(INT64), dimension(dim1,dim2),      intent(inout) :: A
    INTEGER,        dimension(sdim),           intent(inout) :: COLDIM
    LOGICAL,        dimension(sdim),           intent(in   ) :: DESCEND
    LOGICAL,                                   intent(in   ) :: STABLE
    INTEGER,        dimension(:),    optional, intent(inout) :: PRM
    INTEGER,        dimension(dim1):: P
    INTEGER(INT8),  dimension(dim2):: PERMCOL
    INTEGER(INT8):: T
    INTEGER(INT64):: TMP
    INTEGER:: I, J, K, N, S, cdim, col, POS
    LOGICAL:: END_DIM_SERACH
    !
    CALL SORT_1D_INT64( A(:,COLDIM(1)), DESCEND(1), STABLE=STABLE, P=P)
    !
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    END_DIM_SERACH = .FALSE.
    DO cdim=2, sdim        ! Sort the remaining columns
       !
       IF(END_DIM_SERACH) EXIT
       END_DIM_SERACH = .TRUE.
       !
       S   = cdim       ! Make a copy of the column being sorted
       col = COLDIM(S)  ! Current column being sorted
       !
       CALL PERM_SORT(dim1, A(:,col), P)
       !
       I=1                               ! Search for repeated values at COLDIM(S-1) to sort on col
       DIM_SEARCH: DO WHILE (I <  dim1)  ! A(I:J, col) contains the range of repeated values
           J = I + 1
          JSEARCH: DO WHILE (J <= dim1)  !   in columns COLDIM(1:S-1)
              DO K=1, S-1
                  IF( A(I,COLDIM(K)) /= A(J,COLDIM(K)) ) EXIT JSEARCH
              END DO
              J = J + 1
          END DO JSEARCH
          J = J - 1
          !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          IF( I >= J ) THEN
                       I = J + 1
                       CYCLE DIM_SEARCH
          END IF
          !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          END_DIM_SERACH = .FALSE.   ! Found Repeated values, keep searching
          !
          IF( DESCEND(S) ) A(I:J, col) = -A(I:J, col)
          !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          N = J - I + 1
          IF( N == 2 ) THEN                           ! Only 2 repeated values, check if they need to be flipped
              IF (A(I,col) > A(J,col)) THEN
                  TMP      = A(I,col)
                  A(I,col) = A(J,col)
                  A(J,col) = TMP
                  !
                  POS  = P(I)
                  P(I) = P(J)
                  P(J) = POS
              END IF
          ELSE                                        ! Repeated values from I to J
              IF ( N < ISORT_SMALL_ARRAY_SIZE) THEN
                  CALL ISORT_1P_INT64   (N, A(I:J, col), P(I:J))
              ELSE
                  CALL SymMerge_1P_INT64(N, A(I:J, col), P(I:J))
              END IF
          END IF
          !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          IF( DESCEND(S) ) A(I:J, col) = -A(I:J, col)
          I = J + 1
       END DO DIM_SEARCH
    END DO
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    !DO i=1, dim2
    !    if( .not. any(i == COLDIM(1:S)) ) CALL PERM_SORT(dim1, A(:,i), P)
    !END DO
    T = 1_int8
    PERMCOL = T
    do i=1, S
            PERMCOL(COLDIM(i)) = 0_int8
    end do
    DO i=1, dim2
        if(PERMCOL(i)==T) CALL PERM_SORT(dim1, A(:,i), P)
    END DO
    !
    IF(present(PRM)) PRM = P
    !
  END SUBROUTINE
  !
  !##########################################################################################################################
  ! Sort 2D Array by specified Rows; That is dim1 in DIMENSION(dim1,dim2)
  !
  PURE SUBROUTINE SORT_2D_ROWDIM_INT64(dim1, dim2, A, sdim, ROWDIM, DESCEND, STABLE, PRM)
    INTEGER,                                   intent(in   ) :: dim1, dim2, sdim
    INTEGER(INT64), dimension(dim1,dim2),      intent(inout) :: A
    INTEGER,        dimension(sdim),           intent(in   ) :: ROWDIM
    LOGICAL,        dimension(sdim),           intent(in   ) :: DESCEND
    LOGICAL,                                   intent(in   ) :: STABLE
    INTEGER,        dimension(:),    optional, intent(inout) :: PRM
    INTEGER,        dimension(dim2):: P
    INTEGER(INT64), dimension(dim2):: TMP
    INTEGER(INT8),  dimension(dim1):: PERMROW
    INTEGER(INT8):: T
    INTEGER:: I, J, K, N, S, row, rdim
    LOGICAL:: END_DIM_SERACH
    !
    row = ROWDIM(1)
    call ROW_COPY(dim1, dim2, row, A, TMP)
    !
    call SORT_1D_INT64( TMP, DESCEND(1), STABLE=STABLE, P=P)
    !
    call ROW_COPY(dim1, dim2, row, TMP, A)
    !
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    END_DIM_SERACH = .FALSE.
    DO rdim=2, sdim        ! Sort the remaining columns
       !
       IF(END_DIM_SERACH) EXIT
       END_DIM_SERACH = .TRUE.
       !
       S   = rdim       ! Make a copy of the row being sorted
       row = ROWDIM(S)  ! Current row being sorted
       !
       call ROW_COPY(dim1, dim2, row, A, TMP)
       call PERM_SORT(dim2, TMP, P)
       !
       I=1                               ! Search for repeated values at ROWDIM(S-1) to sort on row
       J=2
       DIM_SEARCH: DO WHILE (I <  dim2)  ! A(row, I:J) contains the range of repeated values
          J = I + 1
          JSEARCH: DO WHILE (J <= dim2)  !   in columns ROWDIM(1:S-1)
              DO K=1, S-1
                  IF( A(ROWDIM(K),I) /= A(ROWDIM(K),J) ) EXIT JSEARCH
              END DO
              J = J + 1
          END DO JSEARCH
          J = J - 1
          !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          IF( I >= J ) THEN
                       I = J + 1
                       CYCLE DIM_SEARCH
          END IF
          !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          END_DIM_SERACH = .FALSE.   ! Found Repeated values, keep searching
          !
          IF( DESCEND(S) ) TMP(I:J) = -TMP(I:J)
          !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          N = J - I + 1
          IF( N == 2 ) THEN                                       ! Only 2 repeated values, check if they need to be flipped
              IF (TMP(I) > TMP(J)) CALL SWAP_w_PERM(dim2,TMP,P,I,J)
          ELSE                                                    ! Repeated values from I to J
              IF ( N < ISORT_SMALL_ARRAY_SIZE) THEN
                  CALL ISORT_1P_INT64   (N, TMP(I:J), P(I:J))
              ELSE
                  CALL SymMerge_1P_INT64(N, TMP(I:J), P(I:J))
              END IF
          END IF
          !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          IF( DESCEND(S) ) TMP(I:J) = -TMP(I:J)
          I = J + 1
       END DO DIM_SEARCH
       call ROW_COPY(dim1, dim2, row, TMP, A)
    END DO
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    !DO i=1, dim1
    !    if( .not. any(i == ROWDIM(1:S)) ) CALL PERM_SORT(dim1, A(i,:), P)
    !END DO
    T = 1_int8
    PERMROW = T
    do i=1, S
            PERMROW(ROWDIM(i)) = 0_int8
    end do
    DO row=1, dim1
        if(PERMROW(row)==T) CALL PERM_SORT_ROW(dim1, dim2, row, A, P)
    END DO
    !
    IF(present(PRM)) PRM = P
    !
  END SUBROUTINE
  !
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !
  ! Index / Permutation Based SORT  Subroutines
  !
  MODULE PURE SUBROUTINE PERMUTATION_SORT_1D_INT64(A, P)
    INTEGER(INT64), dimension(:), contiguous, intent(inout) :: A
    INTEGER,        dimension(:), contiguous, intent(inout) :: P
    INTEGER:: dim
    dim = size(A)
    call PERM_SORT(dim, A, P)
  END SUBROUTINE
  !
  MODULE PURE SUBROUTINE PERMUTATION_SORT_2D_INT64(A, P, BY_COLUMN)
    INTEGER(INT64), dimension(:,:), contiguous, intent(inout) :: A
    INTEGER,        dimension(:),   contiguous, intent(inout) :: P
    LOGICAL,                          optional, intent(in   ) :: BY_COLUMN
    INTEGER:: i, dim1, dim2
    LOGICAL:: BY_COL
    !
    dim1 = size(A, 1)
    dim2 = size(A, 2)
    !
    IF(present(BY_COLUMN)) then
        BY_COL=BY_COLUMN
    else
        BY_COL=.TRUE.
    end if
    !
    if(BY_COL) then
        do i=1, dim2
                call PERM_SORT(dim1, A(:,i), P)
        end do
    else
        do i=1, dim1
                call PERM_SORT_ROW(dim1, dim2, i, A, P)
        end do
    end if
    !
  END SUBROUTINE
  !
  PURE SUBROUTINE PERM_SORT(dim, A, P)
    INTEGER,                        intent(in   ) :: dim
    INTEGER(INT64), dimension(dim), intent(inout) :: A
    INTEGER,        dimension(dim), intent(inout) :: P
    INTEGER(INT64):: TMP
    INTEGER:: i, j, k
    !
    if( dim < 2 ) return
    !
    DO i=1, dim
        if( P(i)  < 1 ) cycle
        if( P(i) == i ) then
                        P(i) = -P(i)
                        cycle
        end if
        !
        j  = i     ! Current Index
        k  = P(i)  ! Next Index
        TMP = A(i)
        do while ( k /= i .AND. k > 0)  ! Continue until we have found the i'th position - k > 0 is only true for a bad permutation vector
                          A(j) = A(k)
                          !
                          P(j) = -P(j)  ! Flip sign to flag as being used
                          !
                          j = k        ! Current Index
                          k = P(k)     ! Next Index
        end do
        A(j) = TMP
        P(j) = -P(j)
    END DO
    !
    CALL MAKE_POSTIVE(dim, P)  ! P = -P => Revert the Permutation vector back to normal.
    !
  END SUBROUTINE
  !
  PURE SUBROUTINE PERM_SORT_ROW(dim1, dim2, row, A, P)
    INTEGER,                              intent(in   ) :: dim1, dim2, row
    INTEGER(INT64), dimension(dim1,dim2), intent(inout) :: A
    INTEGER,        dimension(dim2),      intent(inout) :: P
    INTEGER(INT64):: TMP
    INTEGER:: i, j, k
    !
    if( dim2 < 2 ) return
    !
    DO i=1, dim2
        if( P(i)  < 1 ) cycle
        if( P(i) == i ) then
                        P(i) = -P(i)
                        cycle
        end if
        !
        j  = i     ! Current Index
        k  = P(i)  ! Next Index
        TMP = A(row,i)
        do while ( k /= i .AND. k > 0)  ! Continue until we have found the i'th position - k > 0 is only true for a bad permutation vector
                          A(row,j) = A(row,k)
                          !
                          P(j) = -P(j)  ! Flip sign to flag as being used
                          !
                          j = k        ! Current Index
                          k = P(k)     ! Next Index
        end do
        A(row,j) = TMP
        P(j) = -P(j)
    END DO
    !
    CALL MAKE_POSTIVE(dim2, P)  ! P = -P => Revert the Permutation vector back to normal.
    !
  END SUBROUTINE
  !
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !
  ! Reverse Subroutines
  !
  MODULE PURE SUBROUTINE REVERSE_ORDER_1D_NODIM_INT64(A, STABLE, P, INIT_P)
    INTEGER(INT64), dimension(:), contiguous,           intent(inout) :: A
    LOGICAL,                                  optional, intent(in   ) :: STABLE
    INTEGER,        dimension(:), contiguous, optional, intent(inout) :: P
    LOGICAL,                                  optional, intent(in   ) :: INIT_P
    INTEGER:: dim
    LOGICAL:: HAS_P
    dim = SIZE(A)
    !
    HAS_P = present(P)
    IF( HAS_P ) THEN
        IF( dim > SIZE(P) ) THEN
            P = -1
            HAS_P = .FALSE.
        END IF
    END IF
    !
    IF(HAS_P) THEN
              CALL REVERSE_ORDER_1D_DIM_INT64(dim, A, STABLE, P, INIT_P)
    ELSE
              CALL REVERSE_ORDER_1D_DIM_INT64(dim, A, STABLE)
    END IF
    !
  END SUBROUTINE
  !
  MODULE PURE SUBROUTINE REVERSE_ORDER_1D_DIM_INT64(dim, A, STABLE, P, INIT_P)
    INTEGER,                                  intent(in   ) :: dim
    INTEGER(INT64), dimension(dim),           intent(inout) :: A
    LOGICAL,                        optional, intent(in   ) :: STABLE
    INTEGER,        dimension(dim), optional, intent(inout) :: P
    LOGICAL,                        optional, intent(in   ) :: INIT_P
    LOGICAL:: HAS_P
    INTEGER:: I, J, N  ! Terms only used if STABLE = TRUE
    !
    HAS_P = present(P)
    IF( HAS_P ) THEN
            IF( present(INIT_P) ) HAS_P = INIT_P  !Temp use for checking if it needs to be initialized
            !
            IF(HAS_P) THEN
                           CALL INIT_PERMUTATION(dim, P)
            ELSE
                           HAS_P = .TRUE.
            END IF
    END IF
    !
    CALL REVERSE_ORDER_1D_INT64(dim, A)
    !
    IF(HAS_P) CALL REVERSE_ORDER_PERM(dim, P)
    !
    IF(present(STABLE)) THEN
      IF(STABLE) THEN       ! Search for repeated values and reverse their order so they appear in the original placement order
           I=1
           ISEARCH: DO WHILE (I < dim)      ! A(I:J) contains the range of repeated values
              !
              IF( A(I) /= A(I+1) ) THEN
                           I = I + 1
                           CYCLE ISEARCH
              END IF
              J = I + 2
              !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
              JSEARCH: DO WHILE (J <= dim)
                  IF( A(I) /= A(J) ) EXIT JSEARCH
                  J = J + 1
              END DO JSEARCH
              J = J - 1
              !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
              N = J - I + 1
              CALL REVERSE_ORDER_1D_INT64( N, A(I:J) )
              !
              IF(HAS_P) CALL REVERSE_ORDER_PERM( N, P(I:J) )
              !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
              I = J + 1
           END DO ISEARCH
      END IF
    END IF
    !
  END SUBROUTINE
  !
  PURE SUBROUTINE REVERSE_ORDER_1D_INT64(dim, A)
    INTEGER,                        intent(in   ) :: dim
    INTEGER(INT64), dimension(dim), intent(inout) :: A
    INTEGER(INT64):: TMP
    INTEGER:: I, J, HALF_DIM
    !
    IF(dim > 1) THEN
       !
       J = dim
       HALF_DIM = dim/2
       !
       DO I=1, HALF_DIM
           !
           TMP = A(I);  A(I) = A(J);  A(J) = TMP
           !
           J = J - 1
       END DO
    END IF
    !
  END SUBROUTINE
  !
  !##########################################################################################################################
  !
  MODULE PURE SUBROUTINE REVERSE_ORDER_2D_INT64(A, REVERSE_EACH_ROW)
    INTEGER(INT64), dimension(:,:), intent(inout) :: A
    LOGICAL,              optional, intent(in   ) :: REVERSE_EACH_ROW ! Default is TRUE
    INTEGER:: dim1, dim2
    !
    dim1 = SIZE(A,1)
    dim2 = SIZE(A,2)
    !
    IF(present(REVERSE_EACH_ROW)) THEN
      IF(REVERSE_EACH_ROW)  THEN
                            CALL REVERSE_ORDER_2D_EACH_ROW_INT64(dim1,dim2,A)
                            RETURN
      END IF
    END IF
    !
    CALL REVERSE_ORDER_2D_EACH_COL_INT64(dim1,dim2,A)
    !
  END SUBROUTINE
  !
  !--------------------------------------------------------------------------------------------------------------------------
  !
  MODULE PURE SUBROUTINE REVERSE_ORDER_2D_INDEX_INT64(A, REV_INDEX, STABLE, REVERSE_ROW, P)
    INTEGER(INT64), dimension(:,:), contiguous,           intent(inout) :: A
    INTEGER,                                              intent(in   ) :: REV_INDEX
    LOGICAL,                                    optional, intent(in   ) :: STABLE
    LOGICAL,                                    optional, intent(in   ) :: REVERSE_ROW
    INTEGER,        dimension(:),   contiguous, optional, intent(inout) :: P
    INTEGER:: dim1, dim2, revIDX
    LOGICAL:: revCOL
    !
    dim1 = SIZE(A, 1)
    dim2 = SIZE(A, 2)
    !
    IF( dim1 < 1 .OR. dim2 < 1 .OR. REV_INDEX == 0) RETURN
    !
    IF( dim1 == 1 .OR. dim2 == 1 ) THEN  ! Array really is 1D, so use those optimized routines
        IF(dim1 > 1) THEN
                CALL REVERSE_ORDER_1D_NODIM_INT64( A(:,1), STABLE, P )
        ELSE
                CALL REVERSE_ORDER_1D_NODIM_INT64( A(1,:), STABLE, P )
        END IF
        RETURN
    END IF
    !
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    IF( REV_INDEX > 0 ) THEN
        revIDX = REV_INDEX
        revCOL = .TRUE.
    ELSE
        revIDX = -1*REV_INDEX
        revCOL = .FALSE.
    END IF
    IF(present(REVERSE_ROW)) revCOL = .NOT. REVERSE_ROW
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    !
    IF( revCOL ) THEN
        CALL REVERSE_ORDER_2D_COL_INT64(dim1, dim2, A, revIDX, STABLE, P)
    ELSE
        CALL REVERSE_ORDER_2D_ROW_INT64(dim1, dim2, A, revIDX, STABLE, P)
    END IF
    !
  END SUBROUTINE
  !
  PURE SUBROUTINE REVERSE_ORDER_2D_COL_INT64(dim1, dim2, A, COL, STABLE, PRM)
    INTEGER,                                            intent(in   ) :: dim1, dim2, COL
    INTEGER(INT64), dimension(dim1,dim2),               intent(inout) :: A
    LOGICAL,                                  optional, intent(in   ) :: STABLE
    INTEGER,        dimension(:), contiguous, optional, intent(inout) :: PRM
    INTEGER, dimension(dim1):: P
    INTEGER:: k
    !
    CALL REVERSE_ORDER_1D_DIM_INT64(dim1, A(:,COL), STABLE, P)
    !
    ! Sort the other columns by permutation vector
    DO k=1, COL-1
                CALL PERM_SORT(dim1, A(:,k), P)
    END DO
    DO k=COL+1, dim2
                CALL PERM_SORT(dim1, A(:,k), P)
    END DO
    !
    IF(present(PRM))THEN
        if(dim1 > SIZE(PRM)) then
            PRM = -1
        else
            PRM(1:dim1) = P
        end if
    END IF
    !
  END SUBROUTINE
  !
  !--------------------------------------------------------------------------------------------------------------------------
  !
  PURE SUBROUTINE REVERSE_ORDER_2D_ROW_INT64(dim1, dim2, A, ROW, STABLE, PRM)
    INTEGER,                                            intent(in   ) :: dim1, dim2, ROW
    INTEGER(INT64), dimension(dim1,dim2),               intent(inout) :: A
    LOGICAL,                                  optional, intent(in   ) :: STABLE
    INTEGER,        dimension(:), contiguous, optional, intent(inout) :: PRM
    INTEGER,        dimension(dim2):: P
    INTEGER(INT64), dimension(max(dim1,dim2)):: TMP
    INTEGER:: i, j, k
    !
    call ROW_COPY(dim1, dim2, row, A, TMP)
    !
    call REVERSE_ORDER_1D_DIM_INT64(dim2, TMP(1:dim2), STABLE, P)
    !
    IF(present(PRM))THEN
        if(dim2 > SIZE(PRM)) then
                        PRM = -1
        else
            PRM(1:dim2) = P
        end if
    END IF
    !
    ! Sort all the rows by permutation vector *Note that P is not restored after sort
    DO i=1, dim2
        if( P(i)  < 1 .OR. P(i) == i) cycle
        !
        j  = i     ! Current Index
        k  = P(i)  ! Next Index
        TMP(1:dim1) = A(:,i)
        !
        do while ( k /= i .AND. k > 0)  ! Continue until we have found the i'th position - k > 0 is only true for a bad permutation vector
                          !
                          A(:,j) = A(:,k)
                          !
                          P(j) = -P(j)  ! Flip sign to flag as being used
                          !
                          j = k          ! Current Index
                          k = P(k)     ! Next Index
        end do
        A(:,j) = TMP(1:dim1)
        P(j)   = -P(j)
    END DO
    !
  END SUBROUTINE
  !
  !--------------------------------------------------------------------------------------------------------------------------
  !
  PURE SUBROUTINE REVERSE_ORDER_2D_EACH_COL_INT64 (dim1,dim2,A)
    INTEGER,                              intent(in   ) :: dim1, dim2
    INTEGER(INT64), dimension(dim1,dim2), intent(inout) :: A
    INTEGER(INT64):: TMP
    INTEGER:: I, J, K, HALF_DIM
    !
    J = dim1
    HALF_DIM = dim1/2
    !
    DO K=1, dim2
        DO I=1, HALF_DIM
            !
            TMP = A(I,K);  A(I,K) = A(J,K);  A(J,K) = TMP
            !
            J = J - 1
        END DO
    END DO
    !
  END SUBROUTINE
  !
  !--------------------------------------------------------------------------------------------------------------------------
  !
  PURE SUBROUTINE REVERSE_ORDER_2D_EACH_ROW_INT64(dim1, dim2, A)
    INTEGER,                              intent(in   ) :: dim1, dim2
    INTEGER(INT64), dimension(dim1,dim2), intent(inout) :: A
    INTEGER:: I, J, HALF_DIM
    !
    J = dim2
    HALF_DIM = dim2/2
    !
    DO I=1, HALF_DIM
            CALL VECTOR_SWAP_INT64( dim1, A(:,I), A(:,J) )
            J = J - 1
    END DO
    !
  END SUBROUTINE
  !
  !--------------------------------------------------------------------------------------------------------------------------
  !
  PURE SUBROUTINE REVERSE_ORDER_PERM(dim, P)
    INTEGER,                 intent(in   ) :: dim
    INTEGER, dimension(dim), intent(inout) :: P
    INTEGER:: POS
    INTEGER:: I, J, HALF_DIM
    !
    IF(dim > 1) THEN
       J = dim
       HALF_DIM = dim/2
       DO I=1, HALF_DIM
           POS = P(I);  P(I) = P(J);  P(J) = POS
           J = J - 1
       END DO
    END IF
    !
  END SUBROUTINE
  !
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !
  ! Array Rotation Routines
  !
  !--------------------------------------------------------------------------------------------------------------------------
  ! RotateBlocks uses Gries-Mills Method to rotates two consecutive blocks given inclusive indexes: L, M, and R
  ! such that:
  !           A(L:R) = A(L:m-1) // A(m:R)   ->   A(L:R) =  A(m:R) // A(L:m-1)
  !
  !    rotate performs at most R-L+1 many calls to SWAP(A,B)
  !
  PURE SUBROUTINE RotrateBlock(dim, A, L, M, R)
    INTEGER,                        intent(in   ) :: dim
    INTEGER(INT64), dimension(dim), intent(inout) :: A
    INTEGER,                        intent(in   ) :: L, M, R
    INTEGER:: I, J
    !
    IF( M <= L .OR. R < M ) RETURN
    IF( M == R ) THEN
                 DO I=R-1, L, -1
                         CALL SWAP_INT64(dim, A, I, I+1)     ! CALL SWAP_INT64(A(I), A(I+1))
                 END DO
                 RETURN
    END IF
    !
    I = M - L
    J = R - M + 1
    !
    DO WHILE ( I /= J )
        IF( I < J ) THEN
                    CALL BlockSwap(dim, A, M-I, M+J-I, I)
                    J = J - I
        ELSE
                    CALL BlockSwap(dim, A, M-I,     M, J)
                    I = I - J
        END IF
    END DO
    !
    CALL BlockSwap(dim, A, M-I, M, I)
    !
  END SUBROUTINE
  !
  !--------------------------------------------------------------------------------------------------------------------------
  !  BlockSwap Assumes that fi+siz <= si otherwise elements in si wil be rotated back in.
  !
  PURE SUBROUTINE BlockSwap(dim, A, fi, si, siz)
    INTEGER,                        intent(in   ) :: dim
    INTEGER(INT64), dimension(dim), intent(inout) :: A
    INTEGER,                        intent(in   ) :: fi, si, siz
    INTEGER:: I
    DO I=0, siz-1
            CALL SWAP_INT64(dim, A, fi+I, si+I)     !CALL SWAP_INT64(A(fi+I), A(si+I))
    END DO
  END SUBROUTINE
  !
  !##########################################################################################################################
  ! Rotation Routines with IDX
  !
  PURE SUBROUTINE RotrateBlockP(dim, A, P, L, M, R)
    INTEGER,                        intent(in   ) :: dim
    INTEGER(INT64), dimension(dim), intent(inout) :: A
    INTEGER,        dimension(dim), intent(inout) :: P
    INTEGER,                        intent(in   ) :: L, M, R
    INTEGER:: I, J
    !
    IF( M <= L .OR. R < M ) RETURN
    IF( M == R ) THEN
                 DO I=R-1, L, -1
                         CALL SWAP_w_PERM(dim, A, P, I, I+1)
                 END DO
                 RETURN
    END IF
    !
    I = M - L
    J = R - M + 1
    !
    DO WHILE ( I /= J )
        IF( I < J ) THEN
                    CALL BlockSwapP(dim, A, P, M-I, M+J-I, I)
                    J = J - I
        ELSE
                    CALL BlockSwapP(dim, A, P, M-I,     M, J)
                    I = I - J
        END IF
    END DO
    !
    CALL BlockSwapP(dim, A, P, M-I, M, I)
    !
  END SUBROUTINE
  !
  !--------------------------------------------------------------------------------------------------------------------------
  ! BlockSwap Assumes that fi+siz <= si otherwise elements in si wil be rotated back in.
  !
  PURE SUBROUTINE BlockSwapP(dim, A, P, fi, si, siz)
    INTEGER,                        intent(in   ) :: dim
    INTEGER(INT64), dimension(dim), intent(inout) :: A
    INTEGER,        dimension(dim), intent(inout) :: P
    INTEGER,                        intent(in   ) :: fi, si, siz
    INTEGER:: I
    DO I=0, siz-1
            CALL SWAP_w_PERM(dim, A, P, fi+I, si+I)
    END DO
  END SUBROUTINE
  !
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !
  ! Makes a QSORT with Permutation vector sort stable
  !  by sorting P on repeated elements of A.
  !
  PURE SUBROUTINE SORT_P_ON_REPEATED_A(dim, A, P)
     INTEGER,                        intent(in   ) :: dim
     INTEGER(INT64), dimension(dim), intent(inout) :: A
     INTEGER,        dimension(dim), intent(inout) :: P
     INTEGER:: I, J, M, N, POS
     !
     I=1
     J=2
     ISEARCH: DO WHILE (I < dim)      ! A(I:J) contains the range of repeated values
        !
        IF( A(I) /= A(I+1) ) THEN
                     I = I + 1
                     CYCLE ISEARCH
        END IF
        J = I + 2
        !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        JSEARCH: DO WHILE (J <= dim)
            IF( A(I) /= A(J) ) EXIT JSEARCH
            J = J + 1
        END DO JSEARCH
        J = J - 1
        !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        IF( J - I == 1 ) THEN                               ! Only 2 repeated values, check if they need to be flipped
            IF( P(I) > P(J) ) THEN
                POS = P(I);  P(I) = P(J);  P(J) = POS
            END IF
        ELSE                  ! Repeated values from I to J
            DO M=I+1, J       ! Insertion Sort Indexes -> Assume I to J is not large
                POS = P(M)
                N   = M - 1
                DO WHILE ( N >= I )
                           IF( POS >= P(N) ) EXIT
                           P(N+1) = P(N)
                           N = N - 1
                END DO
                P(N+1) = POS
            END DO
        END IF
        !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        I = J + 1
     END DO ISEARCH
     !
  END SUBROUTINE
  !
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !
  ! Support and Utility Subroutines
  !
  ! Simple Routine to SWAP values. --Compiler should optimize to not use TMP
  !
  !PURE SUBROUTINE SWAP_INT64(A, B)          ! Can cause compiler errors due to vectorization when passing array parts, such as CALL SWAP_INT64( A(fi+I), A(si+I) )
  !   INTEGER(INT64), intent(inout):: A, B
  !   INTEGER(INT64):: t
  !   t = A
  !   A = B
  !   B = t
  !END SUBROUTINE
  !
  PURE SUBROUTINE SWAP_INT64(dim, A, i, j)
    INTEGER,                        intent(in   ) :: dim, i, j
    INTEGER(INT64), dimension(dim), intent(inout) :: A
    INTEGER(INT64):: tmp
    tmp  = A(i)
    A(i) = A(j)
    A(j) = tmp
  END SUBROUTINE
  !
  PURE SUBROUTINE SWAP_w_PERM(dim, A, P, I, J)
     INTEGER,                        intent(in   ) :: dim
     INTEGER(INT64), dimension(dim), intent(inout) :: A
     INTEGER,        dimension(dim), intent(inout) :: P
     INTEGER,                        intent(in   ) :: I, J
     INTEGER(INT64):: TMP
     INTEGER:: POS
     TMP  = A(I)
     A(I) = A(J)
     A(J) = TMP
     !
     POS  = P(I)
     P(I) = P(J)
     P(J) = POS
  END SUBROUTINE
  !
  PURE SUBROUTINE VECTOR_SWAP_INT64(dim, A, B)
    INTEGER,                        intent(in   ) :: dim
    INTEGER(INT64), dimension(dim), intent(inout) :: A, B
    INTEGER(INT64):: TMP
    INTEGER:: I
    !
    DO I=1, dim
            TMP  = A(I)
            A(I) = B(I)
            B(I) = TMP
    END DO
    !
  END SUBROUTINE
  !
  !--------------------------------------------------------------------------------------------------------------------------
  ! Function that returns the max depth/recursion level that QSORT allows
  !
  PURE FUNCTION MAX_RECUR(dim) RESULT(MX)
    INTEGER, value :: dim
    INTEGER:: MX
    !
    MX = 0
    DO WHILE (dim > 0)
              dim = SHIFTR(dim, 1)
              MX  = MX + 1
    END DO
    MX = MX * 2  ! ~= 2*ceil(log2(dim+1)) -> Initial guess
    !
    IF(MX < 32   )  MX = 32
    IF(MX > 10240)  MX = 10240
    !
  END FUNCTION
  !
  !--------------------------------------------------------------------------------------------------------------------------
  ! Determine sort option to use
  !
  MODULE PURE FUNCTION SORT_TYPE_INT64(dim, A, DESCEND, STABLE, QSORT, HEAP, ISORT, SymMerge) RESULT(ANS)
    INTEGER,                        intent(in) :: dim
    INTEGER(INT64), dimension(dim), intent(in) :: A
    LOGICAL,              optional, intent(in) :: DESCEND, STABLE, QSORT, HEAP, ISORT, SymMerge
    INTEGER:: ANS
    INTEGER:: I, CNT, LIM, REQUEST
    LOGICAL:: STABLE_SORT, REVERSE
    !            1 - ISORT            -> Odd options are stable sorts
    !            2 - HEAPsort
    !            3 - SYM-MERGE sort
    !            4 - QSORT
    !            5 - TIMSORT       (not implimented yet)
    !
    REQUEST = 0
    IF(present(ISORT)   ) REQUEST = 1
    IF(present(HEAP)    ) REQUEST = 2
    IF(present(SymMerge)) REQUEST = 3
    IF(present(QSORT)   ) REQUEST = 4
    !
    IF( dim <= ISORT_SMALL_ARRAY_SIZE .AND. REQUEST == 0) THEN  ! Small arrays are fastest with Insertion Sort; which is also stable
        ANS = 1
    ELSE!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        REVERSE = .FALSE.
        IF(present(DESCEND)) REVERSE = DESCEND
        !
        STABLE_SORT = .FALSE.
        IF(present(STABLE)) STABLE_SORT = STABLE
        !
        LIM = INT( PARTIAL_SORTED_LIM * REAL(dim) )
        IF(LIM < 5) LIM = 5
        !
        CNT = 0
        IF(REVERSE) then
            DO I=1, dim-1
                         IF( A(I) < A(I+1) )  CNT = CNT + 1  ! Is zero if perfectly sorted, and dim/2 if completely not-sorted
            END DO
        else
            DO I=1, dim-1
                         IF( A(I) > A(I+1) )  CNT = CNT + 1  ! Is zero if perfectly sorted, and dim/2 if completely not-sorted
            END DO
        end if
        !
        IF    ( CNT < 1 ) THEN
                         ANS = 0                         ! Array is already sorted
        ELSEIF( REQUEST > 0 ) THEN                       ! A specific sort option was requested
                         ANS = REQUEST
        ELSEIF( STABLE_SORT ) THEN                       ! Stable Sort Requested
                         ANS = 3
        ELSEIF( CNT <= LIM ) THEN                        ! Array appears to be partially sorted
                         ANS = 3                         ! Switch to 5 (TIMSORT) once developed -- faster for nearly sorted lists
        ELSE
                         ANS = 4                         ! IntroSort/QSORT best for random data
        END IF
    END IF
  END FUNCTION
  !
  !--------------------------------------------------------------------------------------------------------------------------
  ! Sign flip used for MAGNITUDE routines to flip direction of sort
  !
  PURE SUBROUTINE SIGN_FLIP_1D(dim,A)
    INTEGER,                       intent(in   ) :: dim
    INTEGER(INT64), dimension(dim),intent(inout) :: A
    INTEGER:: I
    DO CONCURRENT (I=1:dim)
                           A(I) = NEG * A(I)
    END DO
  END SUBROUTINE
  !
  !--------------------------------------------------------------------------------------------------------------------------
  !
  PURE SUBROUTINE MAKE_POSTIVE(dim, A)
    INTEGER,                 intent(in   ) :: dim
    INTEGER, dimension(dim), intent(inout) :: A
    INTEGER:: I
    !
    DO CONCURRENT (I=1:dim, A(I) < 0)
                            A(I) = -A(I)
    END DO
    !
  END SUBROUTINE
  !
  !--------------------------------------------------------------------------------------------------------------------------
  !
  PURE SUBROUTINE COPY_ABS(dim, A, B)
    INTEGER,                 intent(in   ) :: dim
    INTEGER, dimension(dim), intent(in   ) :: A
    INTEGER, dimension(dim), intent(inout) :: B
    INTEGER:: I
    !
    DO CONCURRENT (I=1:dim)
                       IF( A(I) < ZER ) THEN
                           B(I) = A(I) * NEG
                       ELSE
                           B(I) = A(I)
                       END IF
    END DO
    !
  END SUBROUTINE
  !
  !--------------------------------------------------------------------------------------------------------------------------
  !
  PURE SUBROUTINE ROW_COPY_TO_VEC(dim1, dim2, row, A, B)
    INTEGER,                               intent(in ) :: dim1, dim2, row
    INTEGER(INT64), dimension(dim1, dim2), intent(in ) :: A
    INTEGER(INT64), dimension(dim2),       intent(out) :: B
    INTEGER:: i
    !
    DO CONCURRENT(i=1:dim2)
                  B(i) = A(row,i)
    END DO
    !
  END SUBROUTINE
  !
  !--------------------------------------------------------------------------------------------------------------------------
  !
  PURE SUBROUTINE ROW_COPY_FROM_VEC(dim1, dim2, row, A, B)
    INTEGER,                               intent(in ) :: dim1, dim2, row
    INTEGER(INT64), dimension(dim2),       intent(in ) :: A
    INTEGER(INT64), dimension(dim1, dim2), intent(out) :: B
    INTEGER:: i
    !
    DO CONCURRENT(i=1:dim2)
                  B(row,i) = A(i)
    END DO
    !
  END SUBROUTINE
  !
  !--------------------------------------------------------------------------------------------------------------------------
  !
  PURE SUBROUTINE PREP_SORT_DIM(dim, A, dimOUT)     ! No repeats or zero values
    INTEGER,                 intent(in   ) :: dim
    INTEGER, dimension(dim), intent(inout) :: A
    INTEGER,                 intent(inout) :: dimOUT
    INTEGER:: I, J, D
    !
    I = 1
    D = dim
    DO WHILE ( I < D )
       J = I + 1
       DO WHILE ( J <= D )
                  IF( A(I) == A(J) .OR. A(J) == 0 ) THEN
                      A(J:D-1) = A(J+1:D)
                      A(D) = 0
                      D = D - 1
                  ELSE
                      J = J + 1
                  END IF
       END DO
       I = I + 1
    END DO
    dimOUT = D
    !
  END SUBROUTINE
  ! 
  !
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !
  ! SORT_B_by_A_and_P_xyz_INT64 Routines
  !
  ! Given sorted A and its P, sort B with P then sort by repeated values of A
  !     Supports A as the following types:  INTEGER(INT32), INTEGER(INT64), REAL(REL32), REAL(REL64)
  !
  MODULE PURE SUBROUTINE SORT_B_by_A_and_P_INT32_INT64(dim, A, B, P, descend)
    INTEGER,                                  intent(in   ) :: dim
    INTEGER(INT32), dimension(dim),           intent(in   ) :: A       ! A and B must have the same dim
    INTEGER(INT64), dimension(dim),           intent(inout) :: B    
    INTEGER,        dimension(dim), optional, intent(inout) :: P       ! Applies permutations to sort B, then sorts B on repeated elements of A. P on exit reflect sort for both A and B
    LOGICAL,                        optional, intent(in   ) :: descend ! If present and true, then the sort of B along repeated A is in descending order.
    INTEGER:: I, J, N
    LOGICAL:: dscnd, HAS_P
    !
    HAS_P = present(P)
    !
    dscnd = .FALSE.
    if(present(descend)) dscnd = descend
    !
    IF(HAS_P) CALL PERM_SORT(dim, B, P)
    !
    I=1                               ! Search for repeated values starting at A(I), such that A(I:J) contains the repeated range
    DIM_SEARCH: DO WHILE (I < dim)  ! A(I:J, col) contains the range of repeated values
       !
       IF( A(I) /= A(I+1) ) THEN
                    I = I + 1
                    CYCLE DIM_SEARCH
       END IF
       J = I + 2
       !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
       JSEARCH: DO WHILE (J <= DIM) 
           IF( A(I) /= A(J) ) EXIT JSEARCH
           J = J + 1
       END DO JSEARCH
       J = J - 1
       !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
       N = J - I + 1
       !
       IF(HAS_P) THEN
          IF( N == 2 ) THEN                             ! Only 2 repeated values, check if they need to be flipped
              IF(dscnd) THEN
                  if(B(I) < B(J)) CALL SWAP_w_PERM(dim, B, P, I, J)
              ELSE
                  if(B(I) > B(J)) CALL SWAP_w_PERM(dim, B, P, I, J)
              END IF
          ELSE                                        ! Repeated values from I to J
              IF(dscnd) B(I:J) = -B(I:J)
              !
              IF ( N < ISORT_SMALL_ARRAY_SIZE) THEN
                  CALL ISORT_1P_INT64   (N, B(I:J), P(I:J))
              ELSE
                  CALL SymMerge_1P_INT64(N, B(I:J), P(I:J))
              END IF
              !
              IF(dscnd) B(I:J) = -B(I:J)
          END IF
       ELSE!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          IF( N == 2 ) THEN                             ! Only 2 repeated values, check if they need to be flipped
              IF(dscnd) THEN
                  if(B(I) < B(J)) CALL SWAP_w_PERM(dim, B, P, I, J)
              ELSE
                  if(B(I) > B(J)) CALL SWAP_w_PERM(dim, B, P, I, J)
              END IF
          ELSE                                        ! Repeated values from I to J
              IF(dscnd) B(I:J) = -B(I:J)
              !
              IF ( N < ISORT_SMALL_ARRAY_SIZE) THEN
                  CALL ISORT_1D_INT64   (N, B(I:J))
              ELSE
                  CALL SymMerge_1D_INT64(N, B(I:J))
              END IF
              !
              IF(dscnd) B(I:J) = -B(I:J)
          END IF
       END IF
       !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
       I = J + 1
    END DO DIM_SEARCH
    !
  END SUBROUTINE
  !
  !--------------------------------------------------------------------------------------------------------------------------
  !
  MODULE PURE SUBROUTINE SORT_B_by_A_and_P_INT64_INT64(dim, A, B, P, descend)
    INTEGER,                                  intent(in   ) :: dim
    INTEGER(INT64), dimension(dim),           intent(in   ) :: A       ! A and B must have the same dim
    INTEGER(INT64), dimension(dim),           intent(inout) :: B    
    INTEGER,        dimension(dim), optional, intent(inout) :: P       ! Applies permutations to sort B, then sorts B on repeated elements of A. P on exit reflect sort for both A and B
    LOGICAL,                        optional, intent(in   ) :: descend ! If present and true, then the sort of B along repeated A is in descending order.
    INTEGER:: I, J, N
    LOGICAL:: dscnd, HAS_P
    !
    HAS_P = present(P)
    !
    dscnd = .FALSE.
    if(present(descend)) dscnd = descend
    !
    IF(HAS_P) CALL PERM_SORT(dim, B, P)
    !
    I=1                               ! Search for repeated values starting at A(I), such that A(I:J) contains the repeated range
    DIM_SEARCH: DO WHILE (I < dim)  ! A(I:J, col) contains the range of repeated values
       !
       IF( A(I) /= A(I+1) ) THEN
                    I = I + 1
                    CYCLE DIM_SEARCH
       END IF
       J = I + 2
       !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
       JSEARCH: DO WHILE (J <= DIM) 
           IF( A(I) /= A(J) ) EXIT JSEARCH
           J = J + 1
       END DO JSEARCH
       J = J - 1
       !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
       N = J - I + 1
       !
       IF(HAS_P) THEN
          IF( N == 2 ) THEN                             ! Only 2 repeated values, check if they need to be flipped
              IF(dscnd) THEN
                  if(B(I) < B(J)) CALL SWAP_w_PERM(dim, B, P, I, J)
              ELSE
                  if(B(I) > B(J)) CALL SWAP_w_PERM(dim, B, P, I, J)
              END IF
          ELSE                                        ! Repeated values from I to J
              IF(dscnd) B(I:J) = -B(I:J)
              !
              IF ( N < ISORT_SMALL_ARRAY_SIZE) THEN
                  CALL ISORT_1P_INT64   (N, B(I:J), P(I:J))
              ELSE
                  CALL SymMerge_1P_INT64(N, B(I:J), P(I:J))
              END IF
              !
              IF(dscnd) B(I:J) = -B(I:J)
          END IF
       ELSE!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          IF( N == 2 ) THEN                             ! Only 2 repeated values, check if they need to be flipped
              IF(dscnd) THEN
                  if(B(I) < B(J)) CALL SWAP_w_PERM(dim, B, P, I, J)
              ELSE
                  if(B(I) > B(J)) CALL SWAP_w_PERM(dim, B, P, I, J)
              END IF
          ELSE                                        ! Repeated values from I to J
              IF(dscnd) B(I:J) = -B(I:J)
              !
              IF ( N < ISORT_SMALL_ARRAY_SIZE) THEN
                  CALL ISORT_1D_INT64   (N, B(I:J))
              ELSE
                  CALL SymMerge_1D_INT64(N, B(I:J))
              END IF
              !
              IF(dscnd) B(I:J) = -B(I:J)
          END IF
       END IF
       !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
       I = J + 1
    END DO DIM_SEARCH
    !
  END SUBROUTINE
  !
  MODULE PURE SUBROUTINE SORT_B_by_A_and_P_REL32_INT64(dim, A, B, P, descend)
    INTEGER,                                  intent(in   ) :: dim
    REAL(REL32),    dimension(dim),           intent(in   ) :: A       ! A and B must have the same dim
    INTEGER(INT64), dimension(dim),           intent(inout) :: B    
    INTEGER,        dimension(dim), optional, intent(inout) :: P       ! Applies permutations to sort B, then sorts B on repeated elements of A. P on exit reflect sort for both A and B
    LOGICAL,                        optional, intent(in   ) :: descend ! If present and true, then the sort of B along repeated A is in descending order.
    INTEGER:: I, J, N
    LOGICAL:: dscnd, HAS_P
    !
    HAS_P = present(P)
    !
    dscnd = .FALSE.
    if(present(descend)) dscnd = descend
    !
    IF(HAS_P) CALL PERM_SORT(dim, B, P)
    !
    I=1                               ! Search for repeated values starting at A(I), such that A(I:J) contains the repeated range
    DIM_SEARCH: DO WHILE (I < dim)  ! A(I:J, col) contains the range of repeated values
       !
       IF( A(I) /= A(I+1) ) THEN
                    I = I + 1
                    CYCLE DIM_SEARCH
       END IF
       J = I + 2
       !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
       JSEARCH: DO WHILE (J <= DIM) 
           IF( A(I) /= A(J) ) EXIT JSEARCH
           J = J + 1
       END DO JSEARCH
       J = J - 1
       !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
       N = J - I + 1
       !
       IF(HAS_P) THEN
          IF( N == 2 ) THEN                             ! Only 2 repeated values, check if they need to be flipped
              IF(dscnd) THEN
                  if(B(I) < B(J)) CALL SWAP_w_PERM(dim, B, P, I, J)
              ELSE
                  if(B(I) > B(J)) CALL SWAP_w_PERM(dim, B, P, I, J)
              END IF
          ELSE                                        ! Repeated values from I to J
              IF(dscnd) B(I:J) = -B(I:J)
              !
              IF ( N < ISORT_SMALL_ARRAY_SIZE) THEN
                  CALL ISORT_1P_INT64   (N, B(I:J), P(I:J))
              ELSE
                  CALL SymMerge_1P_INT64(N, B(I:J), P(I:J))
              END IF
              !
              IF(dscnd) B(I:J) = -B(I:J)
          END IF
       ELSE!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          IF( N == 2 ) THEN                             ! Only 2 repeated values, check if they need to be flipped
              IF(dscnd) THEN
                  if(B(I) < B(J)) CALL SWAP_w_PERM(dim, B, P, I, J)
              ELSE
                  if(B(I) > B(J)) CALL SWAP_w_PERM(dim, B, P, I, J)
              END IF
          ELSE                                        ! Repeated values from I to J
              IF(dscnd) B(I:J) = -B(I:J)
              !
              IF ( N < ISORT_SMALL_ARRAY_SIZE) THEN
                  CALL ISORT_1D_INT64   (N, B(I:J))
              ELSE
                  CALL SymMerge_1D_INT64(N, B(I:J))
              END IF
              !
              IF(dscnd) B(I:J) = -B(I:J)
          END IF
       END IF
       !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
       I = J + 1
    END DO DIM_SEARCH
    !
  END SUBROUTINE
  !
  !--------------------------------------------------------------------------------------------------------------------------
  !
  MODULE PURE SUBROUTINE SORT_B_by_A_and_P_REL64_INT64(dim, A, B, P, descend)
    INTEGER,                                  intent(in   ) :: dim
    REAL(REL64),    dimension(dim),           intent(in   ) :: A       ! A and B must have the same dim
    INTEGER(INT64), dimension(dim),           intent(inout) :: B    
    INTEGER,        dimension(dim), optional, intent(inout) :: P       ! Applies permutations to sort B, then sorts B on repeated elements of A. P on exit reflect sort for both A and B
    LOGICAL,                        optional, intent(in   ) :: descend ! If present and true, then the sort of B along repeated A is in descending order.
    INTEGER:: I, J, N
    LOGICAL:: dscnd, HAS_P
    !
    HAS_P = present(P)
    !
    dscnd = .FALSE.
    if(present(descend)) dscnd = descend
    !
    IF(HAS_P) CALL PERM_SORT(dim, B, P)
    !
    I=1                               ! Search for repeated values starting at A(I), such that A(I:J) contains the repeated range
    DIM_SEARCH: DO WHILE (I < dim)  ! A(I:J, col) contains the range of repeated values
       !
       IF( A(I) /= A(I+1) ) THEN
                    I = I + 1
                    CYCLE DIM_SEARCH
       END IF
       J = I + 2
       !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
       JSEARCH: DO WHILE (J <= DIM) 
           IF( A(I) /= A(J) ) EXIT JSEARCH
           J = J + 1
       END DO JSEARCH
       J = J - 1
       !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
       N = J - I + 1
       !
       IF(HAS_P) THEN
          IF( N == 2 ) THEN                             ! Only 2 repeated values, check if they need to be flipped
              IF(dscnd) THEN
                  if(B(I) < B(J)) CALL SWAP_w_PERM(dim, B, P, I, J)
              ELSE
                  if(B(I) > B(J)) CALL SWAP_w_PERM(dim, B, P, I, J)
              END IF
          ELSE                                        ! Repeated values from I to J
              IF(dscnd) B(I:J) = -B(I:J)
              !
              IF ( N < ISORT_SMALL_ARRAY_SIZE) THEN
                  CALL ISORT_1P_INT64   (N, B(I:J), P(I:J))
              ELSE
                  CALL SymMerge_1P_INT64(N, B(I:J), P(I:J))
              END IF
              !
              IF(dscnd) B(I:J) = -B(I:J)
          END IF
       ELSE!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          IF( N == 2 ) THEN                             ! Only 2 repeated values, check if they need to be flipped
              IF(dscnd) THEN
                  if(B(I) < B(J)) CALL SWAP_w_PERM(dim, B, P, I, J)
              ELSE
                  if(B(I) > B(J)) CALL SWAP_w_PERM(dim, B, P, I, J)
              END IF
          ELSE                                        ! Repeated values from I to J
              IF(dscnd) B(I:J) = -B(I:J)
              !
              IF ( N < ISORT_SMALL_ARRAY_SIZE) THEN
                  CALL ISORT_1D_INT64   (N, B(I:J))
              ELSE
                  CALL SymMerge_1D_INT64(N, B(I:J))
              END IF
              !
              IF(dscnd) B(I:J) = -B(I:J)
          END IF
       END IF
       !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
       I = J + 1
    END DO DIM_SEARCH
    !
  END SUBROUTINE
  !
  !--------------------------------------------------------------------------------------------------------------------------
  !
  MODULE PURE SUBROUTINE SORT_B_by_A_and_P_CHAR_INT64(dim, A, B, P, descend)
    INTEGER,                                  intent(in   ) :: dim
    CHARACTER(*),   dimension(dim),           intent(in   ) :: A       ! A and B must have the same dim
    INTEGER(INT64), dimension(dim),           intent(inout) :: B    
    INTEGER,        dimension(dim), optional, intent(inout) :: P       ! Applies permutations to sort B, then sorts B on repeated elements of A. P on exit reflect sort for both A and B
    LOGICAL,                        optional, intent(in   ) :: descend ! If present and true, then the sort of B along repeated A is in descending order.
    INTEGER:: I, J, N
    LOGICAL:: dscnd, HAS_P
    !
    HAS_P = present(P)
    !
    dscnd = .FALSE.
    if(present(descend)) dscnd = descend
    !
    IF(HAS_P) CALL PERM_SORT(dim, B, P)
    !
    I=1                               ! Search for repeated values starting at A(I), such that A(I:J) contains the repeated range
    DIM_SEARCH: DO WHILE (I < dim)  ! A(I:J, col) contains the range of repeated values
       !
       IF( A(I) /= A(I+1) ) THEN
                    I = I + 1
                    CYCLE DIM_SEARCH
       END IF
       J = I + 2
       !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
       JSEARCH: DO WHILE (J <= DIM) 
           IF( A(I) /= A(J) ) EXIT JSEARCH
           J = J + 1
       END DO JSEARCH
       J = J - 1
       !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
       N = J - I + 1
       !
       IF(HAS_P) THEN
          IF( N == 2 ) THEN                             ! Only 2 repeated values, check if they need to be flipped
              IF(dscnd) THEN
                  if(B(I) < B(J)) CALL SWAP_w_PERM(dim, B, P, I, J)
              ELSE
                  if(B(I) > B(J)) CALL SWAP_w_PERM(dim, B, P, I, J)
              END IF
          ELSE                                        ! Repeated values from I to J
              IF(dscnd) B(I:J) = -B(I:J)
              !
              IF ( N < ISORT_SMALL_ARRAY_SIZE) THEN
                  CALL ISORT_1P_INT64   (N, B(I:J), P(I:J))
              ELSE
                  CALL SymMerge_1P_INT64(N, B(I:J), P(I:J))
              END IF
              !
              IF(dscnd) B(I:J) = -B(I:J)
          END IF
       ELSE!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          IF( N == 2 ) THEN                             ! Only 2 repeated values, check if they need to be flipped
              IF(dscnd) THEN
                  if(B(I) < B(J)) CALL SWAP_w_PERM(dim, B, P, I, J)
              ELSE
                  if(B(I) > B(J)) CALL SWAP_w_PERM(dim, B, P, I, J)
              END IF
          ELSE                                        ! Repeated values from I to J
              IF(dscnd) B(I:J) = -B(I:J)
              !
              IF ( N < ISORT_SMALL_ARRAY_SIZE) THEN
                  CALL ISORT_1D_INT64   (N, B(I:J))
              ELSE
                  CALL SymMerge_1D_INT64(N, B(I:J))
              END IF
              !
              IF(dscnd) B(I:J) = -B(I:J)
          END IF
       END IF
       !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
       I = J + 1
    END DO DIM_SEARCH
    !
  END SUBROUTINE
  ! 
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !
  ! SORT_B_by_REP_and_P_INT64 Routines
  !
  ! Given sorted A and its P, sort B with P then sort by repeated values of REP
  !
  MODULE PURE SUBROUTINE SETUP_REP_INT64(dim, A, REP)
    INTEGER,                        intent(in   ) :: dim
    INTEGER(INT64), dimension(dim), intent(in   ) :: A
    INTEGER,        dimension(dim), intent(inout) :: REP
    INTEGER:: N, imax, iz, one
    INTEGER:: I, J
    !
    iz   =   0
    one  =   1
    imax = huge(imax)
    REP  = iz
    !
    N = iz
    I=1                     
    DIM_SEARCH: DO WHILE (I < dim)
       !
       IF( A(I) /= A(I+1) ) THEN
                    I = I + 1
                    CYCLE DIM_SEARCH
       END IF
       J = I + 2
       !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
       JSEARCH: DO WHILE (J <= DIM) 
           IF( A(I) /= A(J) ) EXIT JSEARCH
           J = J + 1
       END DO JSEARCH
       J = J - 1
       !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
       N        = N + one
       REP(I:J) = N
       !
       if( N == imax ) N = iz
       !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
       I = J + 1
    END DO DIM_SEARCH
    !
  END SUBROUTINE
  !
  PURE SUBROUTINE UPDATE_REP_INT64(dim, A, REP, repeat_found) !Given a sorted A, update REP to include its repeated values that match with non-zero REP
    INTEGER,                        intent(in   ) :: dim
    INTEGER(INT64), dimension(dim), intent(in   ) :: A
    INTEGER,        dimension(dim), intent(inout) :: REP
    LOGICAL,                        intent(inout) :: repeat_found
    INTEGER:: N, imax, iz, one
    INTEGER:: I, J, L, R
    !
    iz   =   0
    one  =   1
    imax = huge(imax)
    repeat_found = .FALSE.
    !
    N = maxval(REP)
    !
    if( N == 0 ) RETURN
    !
    if( N > 10000000 ) CALL RESCALE_REP(dim, REP, N)
    !
    I = 1
    DIM_SEARCH: DO WHILE (I < dim)
       !
       IF( REP(I) == 0 ) THEN
                    I = I + 1
                    CYCLE DIM_SEARCH
       END IF
       J = I + 2
       !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
       JSEARCH: DO WHILE (J <= DIM) 
           IF( REP(I) /= REP(J) ) EXIT JSEARCH
           J = J + 1
       END DO JSEARCH
       J = J - 1
       !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
       REP(I:J) = iz
       !
       L = I
       REP_SEARCH: DO WHILE (L < J)       ! Find repeated groups in the range of A(I:J)
          !
          IF( A(L) /= A(L+1) ) THEN
                       L = L + 1
                       CYCLE REP_SEARCH
          END IF
          R = L + 2
          !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          RSEARCH: DO WHILE (R <= J) 
              IF( A(L) /= A(R) ) EXIT RSEARCH
              R = R + 1
          END DO RSEARCH
          R = R - 1
          !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          repeat_found = .TRUE.
          !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          N        = N + one
          REP(L:R) = N
          !
          if( N == imax ) N = iz
          !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          L = R + 1
       END DO REP_SEARCH
       !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
       I = J + 1
    END DO DIM_SEARCH
    !
  END SUBROUTINE
  !
  PURE SUBROUTINE RESCALE_REP(dim, REP, N)
    INTEGER,                        intent(in   ) :: dim
    INTEGER,        dimension(dim), intent(inout) :: REP
    INTEGER,                        intent(inout) :: N
    INTEGER:: iz, one, imax
    INTEGER:: I, J
    !
    imax = huge(imax)
    iz  =  0
    one =  1
    REP = iz
    !
    N = iz
    I=1                     
    DIM_SEARCH: DO WHILE (I < dim)
       !
       IF( REP(I) == 0 ) THEN
                    I = I + 1
                    CYCLE DIM_SEARCH
       END IF
       J = I + 2
       !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
       JSEARCH: DO WHILE (J <= DIM) 
           IF( REP(I) /= REP(J) ) EXIT JSEARCH
           J = J + 1
       END DO JSEARCH
       J = J - 1
       !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
       N        = N + one
       REP(I:J) = N
       !
       if( N == imax ) N = iz
       !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
       I = J + 1
    END DO DIM_SEARCH
    !
  END SUBROUTINE
  !
  MODULE PURE SUBROUTINE SORT_B_by_A_and_P_REP_INT64(dim, REP, B, repeat_sort, P, descend)
    INTEGER,                                  intent(in   ) :: dim
    INTEGER,        dimension(dim),           intent(inout) :: REP
    INTEGER(INT64), dimension(dim),           intent(inout) :: B    
    LOGICAL,                                  intent(inout) :: repeat_sort ! On entry, indicates if B should be sorted on along repeated values of A. On exit indicates if repeated values were found in A
    INTEGER,        dimension(dim), optional, intent(inout) :: P       ! Applies permutations to sort B, then sorts B on repeated elements of A. P on exit reflect sort for both A and B
    LOGICAL,                        optional, intent(in   ) :: descend ! If present and true, then the sort of B along repeated REP is in descending order.
    INTEGER:: I, J, N
    LOGICAL:: dscnd, HAS_P, repeat_found
    !
    HAS_P = present(P)
    !
    dscnd = .FALSE.
    if(present(descend)) dscnd = descend
    !
    IF(HAS_P) CALL PERM_SORT(dim, B, P)
    !
    if(.not. repeat_sort) RETURN
    repeat_found = .FALSE.
    !
    I=1                               ! Search for repeated values starting at REP(I), such that REP(I:J) contains the repeated range
    DIM_SEARCH: DO WHILE (I < dim)  ! REP(I:J, col) contains the range of repeated values
       !
       IF( REP(I) == 0 .OR. REP(I) /= REP(I+1) ) THEN
                                                 I = I + 1
                                                 CYCLE DIM_SEARCH
       END IF                                   
       J = I + 2
       !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
       JSEARCH: DO WHILE (J <= DIM) 
           IF( REP(I) /= REP(J) ) EXIT JSEARCH
           J = J + 1
       END DO JSEARCH
       J = J - 1
       !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
       repeat_found = .TRUE.
       !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
       N = J - I + 1
       !
       IF(HAS_P) THEN
          IF( N == 2 ) THEN                             ! Only 2 repeated values, check if they need to be flipped
              IF(dscnd) THEN
                  if(B(I) < B(J)) CALL SWAP_w_PERM(dim, B, P, I, J)
              ELSE
                  if(B(I) > B(J)) CALL SWAP_w_PERM(dim, B, P, I, J)
              END IF
          ELSE                                        ! Repeated values from I to J
              IF(dscnd) B(I:J) = -B(I:J)
              !
              IF ( N < ISORT_SMALL_ARRAY_SIZE) THEN
                  CALL ISORT_1P_INT64   (N, B(I:J), P(I:J))
              ELSE
                  CALL SymMerge_1P_INT64(N, B(I:J), P(I:J))
              END IF
              !
              IF(dscnd) B(I:J) = -B(I:J)
          END IF
       ELSE!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          IF( N == 2 ) THEN                             ! Only 2 repeated values, check if they need to be flipped
              IF(dscnd) THEN
                  if(B(I) < B(J)) CALL SWAP_w_PERM(dim, B, P, I, J)
              ELSE
                  if(B(I) > B(J)) CALL SWAP_w_PERM(dim, B, P, I, J)
              END IF
          ELSE                                        ! Repeated values from I to J
              IF(dscnd) B(I:J) = -B(I:J)
              !
              IF ( N < ISORT_SMALL_ARRAY_SIZE) THEN
                  CALL ISORT_1D_INT64   (N, B(I:J))
              ELSE
                  CALL SymMerge_1D_INT64(N, B(I:J))
              END IF
              !
              IF(dscnd) B(I:J) = -B(I:J)
          END IF
       END IF
       !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
       I = J + 1
    END DO DIM_SEARCH
    !
    IF(repeat_found) CALL UPDATE_REP_INT64(dim, B, REP, repeat_found)
    !
    repeat_sort = repeat_found
    !
  END SUBROUTINE
  !!
  !!--------------------------------------------------------------------------------------------------------------------------
  !! Given I return J such say A(I:J) are repeated elements
  !!
  !PURE FUNCTION REPEATED_RANGE_INT64(dim, A, I) RESULT(J)  ! aka JSEARCH
  !  INTEGER,                        intent(in):: dim, I
  !  INTEGER(INT64), dimension(dim), intent(in):: A
  !  INTEGER:: J
  !  !
  !  J = I + 1
  !  JSEARCH: DO WHILE (J <= dim) 
  !      IF( A(I) /= A(J) ) EXIT JSEARCH
  !      J = J + 1
  !  END DO JSEARCH
  !  J = J - 1
  !  !
  !END FUNCTION    
  !
END SUBMODULE
!