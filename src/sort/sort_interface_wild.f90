!
!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<!
!>>>>>>>>>##############>>>>>>>>>################>>>>>>>>>################>>>>>>>>>###############>>>>>>>>>###############>>>>>>>>>>!
!<<<<<<<<<##############<<<<<<<<<################<<<<<<<<<################<<<<<<<<<###############<<<<<<<<<###############<<<<<<<<<<!
!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>!
!
! SORT_INTERFACE_WILD
!
!============================================================================================================
!    
! ISORT    => Insertion Sort                                          (https://en.wikipedia.org/wiki/Insertion_sort)
! QSORT    => Dual Pivot Quicksort                                    (https://en.wikipedia.org/wiki/Quicksort)
! SYMMERGE => Stable Minimum Storage Merging by Symmetric Comparisons (Kim, P. S., & Kutzner, A. (2004, September). Stable minimum storage merging by symmetric comparisons. In European Symposium on Algorithms (pp. 714-723). Springer, Berlin, Heidelberg.)
!
!       Submodule Defines SORT routines for CLASS(*) wild card type -> which can be anything
!
!-----------------------------------------------------------------------------------------------------------
!
!  If you wish to make this submodule into a standalone module 
!      (say you only wish to compile this version and not the other submodules)
!  Then you need to do the following:
!
!    Rename SUBMODULE (SORT_INTERFACE) SORT_INTERFACE_WILD 
!        to MODULE SORT_INTERFACE_WILD 
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
SUBMODULE (SORT_INTERFACE) SORT_INTERFACE_WILD
  !
  IMPLICIT NONE
  !
  INTEGER, PARAMETER:: QSORT_PARTITION_MIN    = 24   ! If (R - L + 1 <= SIZ), then stop QSORT partitioning and use ISORT on partition
  INTEGER, PARAMETER:: ISORT_SMALL_ARRAY_SIZE = 64   ! If (dim <= SIZ), then use ISOR
  INTEGER, PARAMETER:: SYMMERGE_BLOCK_SIZE    = 32   ! Symmetric Merge block size for ISORT pass (array initially partitioned into blocks of this size)
  !
  !\!PUBLIC:: SORT
  !\!PUBLIC:: SWAP_METHOD, LESS_METHOD  ! Abstract Interface to User Supplied Subroutines neccesary for WILD routines 
  !\!PUBLIC:: REVERSE_ORDER
  !\!PUBLIC:: PERMUTATION_SORT
  !\!!
  !\!PRIVATE
  !\!!
  !\!ABSTRACT INTERFACE
  !\!             PURE SUBROUTINE SWAP_METHOD(dim, A, i, j)
  !\!                 INTEGER,                 intent(in   ) :: dim, i, j
  !\!                 CLASS(*),dimension(dim), intent(inout) :: A
  !\!                 !TYPE(TYP):: TMP                  ! Commented code serves as a template to build routine
  !\!                 !!
  !\!                 !SELECT TYPE(A)
  !\!                 !TYPE IS( TYP )
  !\!                 !              TMP  = A(i)
  !\!                 !              A(i) = A(j)
  !\!                 !              A(j) = TMP
  !\!                 !END SELECT
  !\!             END SUBROUTINE
  !\!             !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !\!             !
  !\!             PURE FUNCTION LESS_METHOD(dim, A, i, j) RESULT(CMP)
  !\!                 INTEGER,                 intent(in) :: dim, i, j
  !\!                 CLASS(*),dimension(dim), intent(in) :: A
  !\!                 LOGICAL:: CMP
  !\!                 !
  !\!                 !SELECT TYPE(A)
  !\!                 !TYPE IS( TYP )
  !\!                 !              CMP = A(i) < A(j)       ! To get ascending order flip less than sign
  !\!                 !END SELECT
  !\!             END FUNCTION
  !\!             !
  !\!END INTERFACE
  !\!!
  !\!INTERFACE SORT
  !\!  MODULE PROCEDURE SORT_1D_WILD                        ! SORT(A, SWAP, LESS, [STABLE], [P], [QSORT], [HEAP,] [ISORT], [SymMerge])
  !\!END INTERFACE  
  !\!!
  !\!INTERFACE REVERSE_ORDER  
  !\!  MODULE PROCEDURE REVERSE_ORDER_1D_NODIM_WILD         ! REVERSE_ORDER(A, SWAP, LESS, [STABLE], [P], [INIT_P]) 
  !\!  MODULE PROCEDURE REVERSE_ORDER_1D_NOLESS_WILD        ! REVERSE_ORDER(A, SWAP,                 [P], [INIT_P])
  !\!  MODULE PROCEDURE REVERSE_ORDER_1D_DIM_WILD           ! REVERSE_ORDER(dim, A, SWAP, LESS, [STABLE], [P], [INIT_P]) 
  !\!  MODULE PROCEDURE REVERSE_ORDER_1D_NOLESS_DIM_WILD    ! REVERSE_ORDER(dim, A, SWAP,                 [P], [INIT_P])
  !\!END INTERFACE
  !\!!
  !\!INTERFACE PERMUTATION_SORT
  !\!  MODULE PROCEDURE PERMUTATION_SORT_1D_WILD            ! PERMUTATION_SORT(A, P, SWAP)
  !\!  MODULE PROCEDURE PERMUTATION_SORT_1D_LESS_WILD       ! PERMUTATION_SORT(A, P, SWAP, LESS)
  !\!END INTERFACE
  !
  CONTAINS  !================================================================================================================
  !
  MODULE PURE SUBROUTINE SORT_1D_WILD(A, SWAP, LESS, STABLE, P, QSORT, HEAP, ISORT, SymMerge, INIT_P)
    CLASS(*), dimension(:), contiguous, intent(inout):: A
    PROCEDURE(SWAP_METHOD):: SWAP
    PROCEDURE(LESS_METHOD):: LESS
    LOGICAL,                           optional, intent(in   ) :: STABLE    ! Use only Stable Sorting Methods
    INTEGER, dimension(:), contiguous, optional, intent(inout) :: P         ! Contains permutations to sort A (Sorts along with A)
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
    sTYP = SORT_TYPE_WILD(dim, STABLE, QSORT, HEAP, ISORT, SymMerge)
    !   1 - ISORT            - Odd options are stable sorts
    !   2 - HEAPsort
    !   3 - SYM-MERGE sort
    !   4 - QSORT         (Techically its an Intro-Sort with Dual Pivot Quick, Heap, and Insertion Sorts)
    !   5 - TIMSORT       (not implimented yet - Requires buffer size diBm/2)
    !
    IF    (sTYP == 1) THEN
                     IF( HAS_P ) THEN
                         CALL ISORT_1P_WILD(dim, A, P, SWAP, LESS)
                     ELSE
                         CALL ISORT_1D_WILD(dim, A,    SWAP, LESS)
                     END IF
    ELSEIF(sTYP == 4) THEN
                     IF( HAS_P ) THEN
                         CALL QSORT_1P_WILD( dim, A, P, 1, dim, MAX_RECUR(dim), SWAP, LESS )
                     ELSE
                         CALL QSORT_1D_WILD( dim, A,    1, dim, MAX_RECUR(dim), SWAP, LESS )
                     END IF
    ELSEIF(sTYP == 3) THEN
                     IF( HAS_P ) THEN
                         CALL SymMerge_1P_WILD(dim, A, P, SWAP, LESS)
                     ELSE
                         CALL SymMerge_1D_WILD(dim, A, SWAP, LESS)
                     END IF
    ELSEIF(sTYP == 2) THEN
                     IF( HAS_P ) THEN
                         CALL HEAPSORT_1P_WILD(dim, A, P, SWAP, LESS)
                     ELSE
                         CALL HEAPSORT_1D_WILD(dim, A, SWAP, LESS)
                     END IF
    END IF
    !
  END SUBROUTINE
  !
  !##########################################################################################################################
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
  !##########################################################################################################################
  ! ISORT base routines
  !
  PURE SUBROUTINE ISORT_1D_WILD(dim, A, SWAP, LESS)
    INTEGER,                  intent(in   ):: dim
    CLASS(*), dimension(dim), intent(inout):: A
    PROCEDURE(SWAP_METHOD):: SWAP
    PROCEDURE(LESS_METHOD):: LESS
    INTEGER:: I, J
    !
    DO I = 2, dim
        DO J = I, 2, -1                                         ! Continue while A[j] < A[j-1]
                     IF ( .NOT. LESS( dim, A, J, J-1 ) ) EXIT   ! Stop when .not. (A[j] < A[j-1])
                     CALL SWAP( dim, A, J-1, J )              
        END DO
    END DO
    !
  END SUBROUTINE
  !  
  !--------------------------------------------------------------------------------------------------------------------------
  !
  PURE SUBROUTINE ISORT_1P_WILD(dim, A, P, SWAP, LESS)
    INTEGER,                  intent(in   ):: dim
    CLASS(*), dimension(dim), intent(inout):: A
    INTEGER,  dimension(dim), intent(inout):: P
    PROCEDURE(SWAP_METHOD):: SWAP
    PROCEDURE(LESS_METHOD):: LESS
    INTEGER:: I, J
    !
    DO I = 2, dim
        DO J = I, 2, -1                                         ! Continue while A[j] < A[j-1]
                     IF ( .NOT. LESS( dim, A, J, J-1 ) ) EXIT   ! Stop when .not. (A[j] < A[j-1])
                     CALL SWAP  ( dim, A, J-1, J )              
                     CALL SWAP_P( dim, P, J-1, J ) 
        END DO
    END DO
    !
  END SUBROUTINE
  !
  !##########################################################################################################################
  ! QSORT base routines
  !
  RECURSIVE PURE SUBROUTINE QSORT_1D_WILD(dim, A, L, R, D, SWAP, LESS)
    INTEGER,                  intent(in   ):: dim
    CLASS(*), dimension(dim), intent(inout):: A
    INTEGER,                  intent(in   ):: L, R
    INTEGER,                  intent(in   ):: D      ! Depth recursion limit, once zero switch to Heap Sort
    PROCEDURE(SWAP_METHOD):: SWAP
    PROCEDURE(LESS_METHOD):: LESS
    INTEGER:: I, J, K
    !
    K = R - L + 1                        ! Length of QSORT Partition
    IF( K  < 2 ) RETURN                   ! No Partition -> Return
    IF( K == 2 ) THEN                    ! Simple Flip Check
                 IF( LESS(dim, A, R, L) ) THEN                          ! A(L) > A(R)
                                   CALL SWAP(dim, A, L, R)              ! Swap A(L) and A(R)
                 END IF
                 RETURN
    END IF
    !
    IF( K <= QSORT_PARTITION_MIN) THEN   ! => Do Insertion Sort and Return   -- Side note: (R - L + 1 <= 16)
       CALL ISORT_1D_WILD( K, A(L:R), SWAP, LESS )
       RETURN
    END IF
    !
    IF( D < 1 ) THEN   ! No more recursion allowed, switch to Heap Sort
       CALL HEAPSORT_1D_WILD( K, A(L:R), SWAP, LESS )
       RETURN
    END IF
    !-------------------------------------------------------------------------------------
    !  A(L) => Pivot 1 -> Lower Branch
    !  A(R) => Pivot 2 -> Upper Branch
    !
    IF( LESS(dim, A, R, L) ) CALL SWAP(dim, A, L, R) 
    !
    ! Minor Optimization by taking the first and second tercile location and 
    !    place them at L and R to serve as PIVOT points. This helps if the data is already partially sorted
    !
    IF( K > 16 ) THEN                                    ! Should always be true
        J = L + SHIFTR( K, 1 ) ! ~= Midpoint
        !
        K = K / 3  ! Get third of total length
        I = J - K  ! Point should be around the lower Tercile
        J = J + K  ! Point should be around the upper Tercile
        !
        IF(L < I .and. J < R) THEN              !Sort Terciles - then swap end points with terciles
                  IF( LESS(dim, A, I, L) ) THEN
                                           CALL SWAP(dim, A, L, I)
                  END IF                 
                  IF( LESS(dim, A, J, I) ) THEN
                                           CALL SWAP(dim, A, I, J)
                                           IF( LESS( dim, A, I, L) ) THEN
                                                                     CALL SWAP(dim, A, L, I)
                                           END IF
                  END IF                 
                  IF( LESS(dim, A, R, J) ) THEN
                                           CALL SWAP(dim, A, J, R)
                                           IF( LESS( dim, A, J, I) ) THEN
                                                                     CALL SWAP(dim, A, I, J)
                                                                     IF( LESS( dim, A, I, L) ) THEN
                                                                                               CALL SWAP(dim, A, L, I)
                                                                     END IF
                                           END IF
                  END IF
                  CALL SWAP(dim, A, L, I)      !Swap tercile to pivot location - Want bigger  of the two
                  CALL SWAP(dim, A, J, R)      !Swap tercile to pivot location - Want smaller of the two
        END IF
    END IF
    !
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ! Partition Code
    ! 
    I = L + 1
    J = R - 1
    !
    ! Pivolt 1 and 2   -> A(L), A(R)
    ! Partion Location -> A(I), A(J)         => L < I < J < R
    !
    K = I
    DO WHILE ( K <= J )         
        !
        IF    ( LESS(dim, A, K, L) ) THEN  ! Less than the left pivot 
                            CALL SWAP(dim, A, K, I)                 ! Swap A(K) and A(I)
                            I = I + 1
        ELSEIF( .not. LESS(dim, A, K, R) ) THEN  ! Greater than the right pivot A(K) >= A(R)  <=> .not. A(K) < A(R)
                            DO WHILE ( K < J .AND. LESS(dim, A, R, J) )
                               J = J - 1
                            END DO
                            CALL SWAP(dim, A, K, J)                ! Swap A(K) and A(J)
                            J = J - 1
                            !
                            IF ( LESS(dim, A, K, L) ) THEN  ! Less than the left pivot 
                                            CALL SWAP(dim, A, K, I)                ! Swap A(K) and A(I)
                                            I = I + 1
                            END IF 
        END IF
        !
        K = K + 1
        !
    END DO
    !
    ! Move Pivot to Partion Location
    I = I - 1
    J = J + 1
    CALL SWAP(dim, A, L, I)   ! Swap A(L) and A(I)
    CALL SWAP(dim, A, J, R)   ! Swap A(J) and A(R)
    !
    ! End Partition Code
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    !
    !  A(I) => Pivot 1 -> Lower Branch
    !  A(J) => Pivot 2 -> Upper Branch
    !
    CALL QSORT_1D_WILD(dim, A, L  , I-1, D-1, SWAP, LESS) 
    CALL QSORT_1D_WILD(dim, A, I+1, J-1, D-1, SWAP, LESS) 
    CALL QSORT_1D_WILD(dim, A, J+1,   R, D-1, SWAP, LESS) 
    !
  END SUBROUTINE
  !  
  !--------------------------------------------------------------------------------------------------------------------------
  !
  RECURSIVE PURE SUBROUTINE QSORT_1P_WILD(dim, A, P, L, R, D, SWAP, LESS)
    INTEGER,                  intent(in   ):: dim
    CLASS(*), dimension(dim), intent(inout):: A
    INTEGER,  dimension(dim), intent(inout):: P
    INTEGER,                  intent(in   ):: L, R
    INTEGER,                  intent(in   ):: D      ! Depth recursion limit, once zero switch to Heap Sort
    PROCEDURE(SWAP_METHOD):: SWAP
    PROCEDURE(LESS_METHOD):: LESS
    INTEGER:: I, J, K
    !
    K = R - L + 1                        ! Length of QSORT Partition
    IF( K  < 2 ) RETURN                   ! No Partition -> Return
    IF( K == 2 ) THEN                    ! Simple Flip Check
                 IF( LESS(dim, A, R, L) ) THEN                          ! A(L) > A(R)
                                   CALL SWAP  (dim, A, L, R)            ! Swap A(L) and A(R)
                                   CALL SWAP_P(dim, P, L, R) 
                 END IF
                 RETURN
    END IF
    !
    IF( K <= QSORT_PARTITION_MIN) THEN   ! => Do Insertion Sort and Return   -- Side note: (R - L + 1 <= 16)
       CALL ISORT_1P_WILD( K, A(L:R), P(L:R), SWAP, LESS )
       RETURN
    END IF
    !
    IF( D < 1 ) THEN   ! No more recursion allowed, switch to Heap Sort
       CALL HEAPSORT_1P_WILD( K, A(L:R), P(L:R), SWAP, LESS )
       RETURN
    END IF
    !-------------------------------------------------------------------------------------
    !  A(L) => Pivot 1 -> Lower Branch
    !  A(R) => Pivot 2 -> Upper Branch
    !
    IF( LESS(dim, A, R, L) ) THEN
                             CALL SWAP  (dim, A, L, R)      !Swap A(L) and A(R)
                             CALL SWAP_P(dim, P, L, R)
    END IF
    !
    ! Minor Optimization by taking the first and second tercile location and 
    !    place them at L and R to serve as PIVOT points. This helps if the data is already partially sorted
    IF( K > 16 ) THEN                                    ! Should always be true
        J = L + SHIFTR( K, 1 ) ! ~= Midpoint
        !
        K = K / 3  ! Get third of total length
        I = J - K  ! Point should be around the lower Tercile
        J = J + K  ! Point should be around the upper Tercile
        !
        IF(L < I .and. J < R) THEN              !Sort Terciles - then swap end points with terciles
                  IF( LESS(dim, A, I, L) ) THEN
                                           CALL SWAP  (dim, A, L, I)
                                           CALL SWAP_P(dim, P, L, I) 
                  END IF                 
                  IF( LESS(dim, A, J, I) ) THEN
                                           CALL SWAP  (dim, A, I, J)
                                           CALL SWAP_P(dim, P, I, J) 
                                           IF( LESS( dim, A, I, L) ) THEN
                                                                     CALL SWAP  (dim, A, L, I)
                                                                     CALL SWAP_P(dim, P, L, I) 
                                           END IF
                  END IF                 
                  IF( LESS(dim, A, R, J) ) THEN
                                           CALL SWAP  (dim, A, J, R)
                                           CALL SWAP_P(dim, P, J, R) 
                                           IF( LESS( dim, A, J, I) ) THEN
                                                                     CALL SWAP  (dim, A, I, J)
                                                                     CALL SWAP_P(dim, P, I, J) 
                                                                     IF( LESS( dim, A, I, L) ) THEN
                                                                                               CALL SWAP  (dim, A, L, I)
                                                                                               CALL SWAP_P(dim, P, L, I) 
                                                                     END IF
                                           END IF
                  END IF
                  CALL SWAP  (dim, A, L, I)      !Swap tercile to pivot location - Want bigger  of the two
                  CALL SWAP  (dim, A, J, R)      !Swap tercile to pivot location - Want smaller of the two
                  CALL SWAP_P(dim, P, L, I) 
                  CALL SWAP_P(dim, P, J, R) 
        END IF
    END IF
    !
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ! Partition Code
    ! 
    I = L + 1
    J = R - 1
    !
    ! Pivolt 1 and 2   -> A(L), A(R)
    ! Partion Location -> A(I), A(J)         => L < I < J < R
    !
    K = I
    DO WHILE ( K <= J )         
        !
        IF    ( LESS(dim, A, K, L) ) THEN  ! Less than the left pivot 
                            CALL SWAP  (dim, A, K, I)                 ! Swap A(K) and A(I)
                            CALL SWAP_P(dim, P, K, I)
                            I = I + 1
        ELSEIF( .not. LESS(dim, A, K, R) ) THEN  ! Greater than the right pivot A(K) >= A(R)  <=> .not. A(K) < A(R)
                            DO WHILE ( K < J .AND. LESS(dim, A, R, J) )
                               J = J - 1
                            END DO
                            CALL SWAP  (dim, A, K, J)                ! Swap A(K) and A(J)
                            CALL SWAP_P(dim, P, K, J)
                            J = J - 1
                            !
                            IF ( LESS(dim, A, K, L) ) THEN  ! Less than the left pivot 
                                            CALL SWAP  (dim, A, K, I)                ! Swap A(K) and A(I)
                                            CALL SWAP_P(dim, P, K, I)
                                            I = I + 1
                            END IF 
        END IF
        !
        K = K + 1
        !
    END DO
    !
    ! Move Pivot to Partion Location
    I = I - 1
    J = J + 1
    CALL SWAP  (dim, A, L, I)   ! Swap A(L) and A(I)
    CALL SWAP  (dim, A, J, R)   ! Swap A(J) and A(R)
    !
    CALL SWAP_P(dim, P, L, I)
    CALL SWAP_P(dim, P, J, R)
    !
    ! End Partition Code
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    !
    !  A(I) => Pivot 1 -> Lower Branch
    !  A(J) => Pivot 2 -> Upper Branch
    !
    CALL QSORT_1P_WILD(dim, A, P, L  , I-1, D-1, SWAP, LESS) 
    CALL QSORT_1P_WILD(dim, A, P, I+1, J-1, D-1, SWAP, LESS) 
    CALL QSORT_1P_WILD(dim, A, P, J+1,   R, D-1, SWAP, LESS) 
    !
  END SUBROUTINE
  !
  !##########################################################################################################################
  ! HEAPSORT base routines for when QSORT has too many recursive calls
  !
  PURE SUBROUTINE HEAPSORT_1D_WILD(dim, A, SWAP, LESS)
    INTEGER,                  intent(in   ):: dim
    CLASS(*), dimension(dim), intent(inout):: A
    PROCEDURE(SWAP_METHOD):: SWAP
    PROCEDURE(LESS_METHOD):: LESS
    INTEGER:: I, P, C
    !
    HEAPIFY: DO I=dim/2, 1, -1
       P = I    ! Parent
       C = 2*I  ! Child
       DO WHILE ( C <= dim )   ! Siftdown the heap
           !
           IF( C < dim) THEN
                   IF ( LESS(dim, A, C, C+1) ) C = C + 1  ! 2nd child is bigger if A(C) < A(C+1)
           END IF
           !
           IF( .NOT. LESS(dim, A, P, C) ) CYCLE HEAPIFY           ! IF( A(P) >= A(C) ) CYCLE HEAPIFY
           !
           CALL SWAP(dim, A, P, C)         ! Swap A(P) and A(C)
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
       CALL SWAP(dim, A, 1, I+1)       ! Swap A(1) and A(I+1)
       !
       P = 1    ! Parent
       C = 2    ! Child
       DO WHILE ( C <= I )   ! Siftdown the heap
           !
           IF( C < I) THEN
                   IF ( LESS(dim, A, C, C+1) ) C = C + 1  ! 2nd child is bigger   A(C) < A(C+1)
           END IF
           !
           IF( .NOT. LESS(dim, A, P, C) ) CYCLE POPPER   ! IF( A(P) >= A(C) ) CYCLE POPPER
           !
           CALL SWAP(dim, A, P, C)         ! Swap A(P) and A(C)
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
  PURE SUBROUTINE HEAPSORT_1P_WILD(dim, A, PRM, SWAP, LESS)
    INTEGER,                  intent(in   ):: dim
    CLASS(*), dimension(dim), intent(inout):: A
    INTEGER,  dimension(dim), intent(inout):: PRM
    PROCEDURE(SWAP_METHOD):: SWAP
    PROCEDURE(LESS_METHOD):: LESS
    INTEGER:: I, P, C
    !
    HEAPIFY: DO I=dim/2, 1, -1
       P = I    ! Parent
       C = 2*I  ! Child
       DO WHILE ( C <= dim )   ! Siftdown the heap
           !
           IF( C < dim) THEN
                   IF ( LESS(dim, A, C, C+1) ) C = C + 1  ! 2nd child is bigger if A(C) < A(C+1)
           END IF
           !
           IF( .NOT. LESS(dim, A, P, C) ) CYCLE HEAPIFY           ! IF( A(P) >= A(C) ) CYCLE HEAPIFY
           !
           CALL SWAP  (dim, A, P, C)         ! Swap A(P) and A(C)
           CALL SWAP_P(dim, PRM, P, C ) 
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
       CALL SWAP  (dim, A, 1, I+1)       ! Swap A(1) and A(I+1)
       CALL SWAP_P(dim, PRM, 1, I+1 ) 
       !
       P = 1    ! Parent
       C = 2    ! Child
       DO WHILE ( C <= I )   ! Siftdown the heap
           !
           IF( C < I) THEN
                   IF ( LESS(dim, A, C, C+1) ) C = C + 1  ! 2nd child is bigger   A(C) < A(C+1)
           END IF
           !
           IF( .NOT. LESS(dim, A, P, C) ) CYCLE POPPER   ! IF( A(P) >= A(C) ) CYCLE POPPER
           !
           CALL SWAP  (dim, A, P, C)         ! Swap A(P) and A(C)
           CALL SWAP_P( dim, PRM, P, C ) 
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
  PURE FUNCTION SYM_BSEARCH(dim, A, L, R, N, LESS) RESULT(m)
    INTEGER,                  intent(in) :: dim
    CLASS(*), dimension(dim), intent(in) :: A
    INTEGER,                       value :: L, R
    INTEGER,                  intent(in) :: N
    PROCEDURE(LESS_METHOD):: LESS
    INTEGER:: m
    DO WHILE (L < R)
              m = SHIFTR(L+R, 1)                   ! 
              IF( .not. LESS(dim, A, N-m, m) ) THEN  ! A(m) <= A(N-m) -> For LESS function -> .NOT. (A(N-m) < A(m)) or roate options with A(N-m) < A(m)
                             L = m + 1
              ELSE
                             R = m
              END IF
              !IF( A(m) > A(N-m) ) THEN
              !                 R = m
              !ELSE
              !                 L = m + 1
              !END IF
    END DO
    m = L
  END FUNCTION
  !
  !##########################################################################################################################
  ! SymMerge routines without P
  !
  PURE SUBROUTINE SymMerge_1D_WILD(dim, A, SWAP, LESS)
    INTEGER,                  intent(in   ) :: dim
    CLASS(*), dimension(dim), intent(inout) :: A
    PROCEDURE(SWAP_METHOD):: SWAP
    PROCEDURE(LESS_METHOD):: LESS
    INTEGER:: I, nblock, nblock2, nb, N
    !
    !
    nblock = SYMMERGE_BLOCK_SIZE
    !
    N = nblock*(dim/nblock)  ! Array split as A(1:N);     which is a multiple of nblock
    !                                  and as A(N+1:dim); which is the left over/extra space
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    !
    nb   = nblock-1 
    !
    DO I = 1, N, nblock
                 CALL ISORT_1D_WILD(nblock, A(I:I+nb), SWAP, LESS)
    END DO
    nb = dim-N
    IF( nb > 1 )  CALL ISORT_1D_WILD(nb, A(N+1:dim), SWAP, LESS)
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
                     CALL SymMerge_WILD(dim, A, I, I+nblock, I+nb, SWAP, LESS)
        END DO
        N  = N + 1
        nb = N+nblock
        IF( nb <= dim )  CALL SymMerge_WILD(dim, A, N, nb, dim, SWAP, LESS)
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
  RECURSIVE PURE SUBROUTINE SymMerge_WILD(dim, A, fi, si, li, SWAP, LESS)
    INTEGER,                  intent(in   ) :: dim
    CLASS(*), dimension(dim), intent(inout) :: A
    INTEGER,                  intent(in   ) :: fi, si, li
    PROCEDURE(SWAP_METHOD):: SWAP
    PROCEDURE(LESS_METHOD):: LESS
    INTEGER:: n, m
    INTEGER:: strt, term
    !
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~! 
    if ( fi >= si .OR. si > li ) RETURN                                                            ! Check 1 -> Only one value, no sort necessary
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~! 
    if ( .NOT. LESS(dim, A, si, si-1) ) RETURN                                                     ! Check 2 -> Left and Right Blocks already ordered equilvant to: ( A(si-1) <= A(si) )
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~! 
    if (li - fi == 1) then                                                                         ! Check 3 -> Only two values and from to Check 2, A(fi) > A(si) = A(li)
                      CALL SWAP(dim, A, fi, li )  ! Check 2 failure implies: A(fi) > A(li)         !
                      RETURN                                                                       !
    end if                                                                                         !
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
    if( LESS(dim, A, li, fi) ) then                                                                ! Check 4 -> Right block smaller than left => Rotate them
                          CALL RotrateBlock_Wild(dim, A, fi, si, li, SWAP)    ! Flip Blocks        !
                          RETURN                                                                   !
    END IF                                                                                         !
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~! 
    if (si == li) then                                                                             ! Check 5 -> Right block only contains one value. Check indicates it needs to be placed in left block.
		n = fi;   m = si                                                                           !              Binary search to find the lowest index n such that A[n] >= A[fi] for si <= n < li.
        DO WHILE (n < m)                                                                           !
                 strt = SHIFTR( n+m, 1 )                                                           !
                 !                                                                                 !
                 if( LESS(dim, A, si, strt) ) then                                                 !
                     m = strt                                                                      !
                 else                                                                              !
                     n = strt + 1                                                                  !
                 end if                                                                            !
        END DO                                                                                     !
        !                Move End point to correct poition                                         !
        DO m=si-1, n, -1                                                                           !
                    CALL SWAP(dim, A, m, m+1)                                                      !
        END DO                                                                                     !
        RETURN                                                                                     !
    end if                                                                                         !
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
    !
    m = SHIFTR( fi+li, 1 ) ! <- midpoint; If (fi+li) overflow is possible, use this instead:  = midpoint(fi, li)
    n = m + si
    !
    if (si > m) then
        strt = SYM_BSEARCH(dim, A, n-li,  m+1, n, LESS) ! The other option is to do si-1, but SYM_BSEARCH would have to change to DO WHILE (L <= R) and include a check for L==R
    else
        strt = SYM_BSEARCH(dim, A,   fi, si, n, LESS)
    end if
    term = n - strt
    !
    IF ( fi <= strt .AND. term <= li ) CALL RotrateBlock_Wild(dim, A, strt, si, term, SWAP)
    !
    CALL SymMerge_WILD(dim, A, fi, strt, m, SWAP, LESS)
    !
    m = m + 1;  term = term + 1
    !
    CALL SymMerge_WILD(dim, A, m, term, li, SWAP, LESS)
    !
  END SUBROUTINE
  !
  !##########################################################################################################################
  ! SymMerge routines with P
  !
  PURE SUBROUTINE SymMerge_1P_WILD(dim, A, P, SWAP, LESS)
    INTEGER,                  intent(in   ) :: dim
    CLASS(*), dimension(dim), intent(inout) :: A
    INTEGER,  dimension(dim), intent(inout) :: P
    PROCEDURE(SWAP_METHOD):: SWAP
    PROCEDURE(LESS_METHOD):: LESS
    INTEGER:: I, nblock, nblock2, nb, N
    !
    nblock = SYMMERGE_BLOCK_SIZE
    !
    N = nblock*(dim/nblock)  ! Array split as A(1:N);     which is a multiple of nblock
    !                                  and as A(N+1:dim); which is the left over/extra space
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    !
    nb   = nblock-1 
    !
    DO I = 1, N, nblock
                 CALL ISORT_1P_WILD(nblock, A(I:I+nb), P(I:I+nb), SWAP, LESS)
    END DO
    nb = dim-N
    IF( nb > 1 ) CALL ISORT_1P_WILD(nb, A(N+1:dim), P(N+1:dim), SWAP, LESS)
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
                     CALL SymMergeP_WILD(dim, A, P, I, I+nblock, I+nb, SWAP, LESS)
        END DO
        N  = N + 1
        nb = N+nblock
        IF( nb <= dim )  CALL SymMergeP_WILD(dim, A, P, N, nb, dim, SWAP, LESS)
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
  RECURSIVE PURE SUBROUTINE SymMergeP_WILD(dim, A, P, fi, si, li, SWAP, LESS)
    INTEGER,                  intent(in   ) :: dim
    CLASS(*), dimension(dim), intent(inout) :: A        ! Array to be sorted
    INTEGER,  dimension(dim), intent(inout) :: P        ! Index Position Array
    INTEGER,                  intent(in   ) :: fi, si, li
    PROCEDURE(SWAP_METHOD):: SWAP
    PROCEDURE(LESS_METHOD):: LESS
    INTEGER:: n, m
    INTEGER:: strt, term
    !
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~! 
    if ( fi >= si .OR. si > li ) RETURN                                                            ! Check 1 -> Only one value, no sort necessary
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~! 
    if ( .NOT. LESS(dim, A, si, si-1) ) RETURN                                                     !  Check 2 -> Left and Right Blocks already ordered
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~! 
    if (li - fi == 1) then                                                                         ! Check 3 -> Only two values and from to Check 2, A(fi) > A(si) = A(li)
                      CALL SWAP  (dim, A, fi, li )  ! Check 2 failure implies: A(fi) > A(li)       !
                      CALL SWAP_P( dim, P, fi, li )                                                !
                      RETURN                                                                       !
    end if                                                                                         !
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
    if( LESS(dim, A, li, fi) ) then                                                                ! Check 4 -> Right block smaller than left => Rotate them
                          CALL RotrateBlockP_WILD(dim, A, P, fi, si, li, SWAP)   ! Flip Blocks     !
                          RETURN                                                                   !
    END IF                                                                                         !
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~! 
    if (si == li) then                                                                             ! Check 5 -> Right block only contains one value. Check indicates it needs to be placed in left block.
		n = fi;   m = si                                                                           !              Binary search to find the lowest index n such that A[n] >= A[fi] for si <= n < li.
        DO WHILE (n < m)                                                                           !
                 strt = SHIFTR( n+m, 1 )                                                           !
                 !                                                                                 !
                 if( LESS(dim, A, si, strt) ) then                                                 !
                     m = strt                                                                      !
                 else                                                                              !
                     n  = strt + 1                                                                 !
                 end if                                                                            !
        END DO                                                                                     !
        !                Move End point to correct poition                                         !
        DO m=si-1, n, -1                                                                           !
                    CALL SWAP  (dim, A, m, m+1)                                                    !
                    CALL SWAP_P(dim, P, m, m+1 )                                                   !
        END DO                                                                                     !
        RETURN                                                                                     !
    end if                                                                                         !
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
    !
    m = SHIFTR( fi+li, 1 ) ! <- midpoint; If (fi+li) overflow is possible, use this instead:  = midpoint(fi, li)
    n = m + si
    !
    if (si > m) then
        strt = SYM_BSEARCH(dim, A, n-li,  m+1, n, LESS) ! The other option is to do si-1, but SYM_BSEARCH would have to change to DO WHILE (L <= R) and include a check for L==R
    else
        strt = SYM_BSEARCH(dim, A,   fi, si, n, LESS)
    end if
    term = n - strt
    !
    IF ( fi <= strt .AND. term <= li ) CALL RotrateBlockP_WILD(dim, A, P, strt, si, term, SWAP)
    !
    CALL SymMergeP_WILD(dim, A, P, fi, strt, m, SWAP, LESS)
    !
    m = m + 1;  term = term + 1
    !
    CALL SymMergeP_WILD(dim, A, P, m, term, li, SWAP, LESS)
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
  PURE SUBROUTINE RotrateBlock_Wild(dim, A, L, M, R, SWAP)
    INTEGER,                  intent(in   ) :: dim
    CLASS(*), dimension(dim), intent(inout) :: A
    INTEGER,                  intent(in   ) :: L, M, R
    PROCEDURE(SWAP_METHOD):: SWAP
    INTEGER:: I, J
    !
    IF( M <= L .OR. R < M ) RETURN
    IF( M == R ) THEN
                 DO I=R-1, L, -1
                         CALL SWAP(dim, A, I, I+1)
                 END DO
                 RETURN
    END IF
    !
    I = M - L
    J = R - M + 1
    !
    DO WHILE ( I /= J )
        IF( I < J ) THEN
                    CALL BlockSwap_WILD(dim, A, M-I, M+J-I, I, SWAP)
                    J = J - I
        ELSE
                    CALL BlockSwap_WILD(dim, A, M-I,     M, J, SWAP)
                    I = I - J
        END IF
    END DO
    !
    CALL BlockSwap_WILD(dim, A, M-I, M, I, SWAP)
    !
  END SUBROUTINE
  !
  !--------------------------------------------------------------------------------------------------------------------------
  !  BlockSwap Assumes that fi+siz <= si otherwise elements in si wil be rotated back in.
  !
  PURE SUBROUTINE BlockSwap_WILD(dim, A, fi, si, siz, SWAP)
    INTEGER,                  intent(in   ) :: dim
    CLASS(*), dimension(dim), intent(inout) :: A
    INTEGER,                  intent(in   ) :: fi, si, siz
    PROCEDURE(SWAP_METHOD):: SWAP
    INTEGER:: I
    DO I=0, siz-1
            CALL SWAP(dim, A, fi+I, si+I)
    END DO
  END SUBROUTINE
  !
  !##########################################################################################################################
  ! Rotation Routines with IDX
  !
  PURE SUBROUTINE RotrateBlockP_WILD(dim, A, P, L, M, R, SWAP)
    INTEGER,                        intent(in   ) :: dim
    CLASS(*), dimension(dim), intent(inout) :: A
    INTEGER,  dimension(dim), intent(inout) :: P
    INTEGER,                        intent(in   ) :: L, M, R
    PROCEDURE(SWAP_METHOD):: SWAP
    INTEGER:: I, J
    !
    IF( M <= L .OR. R < M ) RETURN
    IF( M == R ) THEN
                 DO I=R-1, L, -1
                         CALL SWAP  (dim, A, I, I+1)
                         CALL SWAP_P(dim, P, I, I+1)
                 END DO
                 RETURN
    END IF
    !
    I = M - L
    J = R - M + 1
    !
    DO WHILE ( I /= J )
        IF( I < J ) THEN
                    CALL BlockSwapP_WILD(dim, A, P, M-I, M+J-I, I, SWAP)
                    J = J - I
        ELSE
                    CALL BlockSwapP_WILD(dim, A, P, M-I,     M, J, SWAP)
                    I = I - J
        END IF
    END DO
    !
    CALL BlockSwapP_WILD(dim, A, P, M-I, M, I, SWAP)
    !
  END SUBROUTINE
  !
  !--------------------------------------------------------------------------------------------------------------------------
  ! BlockSwap Assumes that fi+siz <= si otherwise elements in si wil be rotated back in.
  !
  PURE SUBROUTINE BlockSwapP_WILD(dim, A, P, fi, si, siz, SWAP)
    INTEGER,                  intent(in   ) :: dim
    CLASS(*), dimension(dim), intent(inout) :: A
    INTEGER,  dimension(dim), intent(inout) :: P
    INTEGER,                  intent(in   ) :: fi, si, siz
    PROCEDURE(SWAP_METHOD):: SWAP
    INTEGER:: I
    DO I=0, siz-1
            CALL SWAP  (dim, A, fi+I, si+I)
            CALL SWAP_P(dim, P, fi+I, si+I)
    END DO
  END SUBROUTINE
  !
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !
  ! Index / Permutation Based SORT  Subroutines
  !
  MODULE PURE SUBROUTINE PERMUTATION_SORT_1D_LESS_WILD(A, P, SWAP, LESS)
    CLASS(*), dimension(:), contiguous, intent(inout) :: A
    INTEGER,  dimension(:), contiguous, intent(inout) :: P
    PROCEDURE(SWAP_METHOD):: SWAP
    PROCEDURE(LESS_METHOD):: LESS
    INTEGER:: dim
    dim = size(A)
    call PERM_SORT(dim, A, P, SWAP)
  END SUBROUTINE
  !
  MODULE PURE SUBROUTINE PERMUTATION_SORT_1D_WILD(A, P, SWAP)
    CLASS(*), dimension(:), contiguous, intent(inout) :: A
    INTEGER,  dimension(:), contiguous, intent(inout) :: P
    PROCEDURE(SWAP_METHOD):: SWAP
    INTEGER:: dim
    dim = size(A)
    call PERM_SORT(dim, A, P, SWAP)
  END SUBROUTINE
  !
  PURE SUBROUTINE PERM_SORT(dim, A, P, SWAP)
    INTEGER,                  intent(in   ) :: dim
    CLASS(*), dimension(dim), intent(inout) :: A
    INTEGER,  dimension(dim), intent(inout) :: P
    PROCEDURE(SWAP_METHOD):: SWAP
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
        do while ( k /= i .AND. k > 0)  ! Continue until we have found the i'th position - k > 0 is only true for a bad permutation vector
                          !
                          CALL SWAP(dim, A, j, k)
                          !
                          P(j) = -P(j)  ! Flip sign to flag as being used
                          !
                          j = k        ! Current Index
                          k = P(k)     ! Next Index
        end do
        P(j) = -P(j)
    END DO
    !
    CALL MAKE_POSTIVE(dim, P)  ! P = -P => Revert the Permutation vector back to normal.
    !
  END SUBROUTINE
  !
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !
  ! Reverse Subroutines
  !
  MODULE PURE SUBROUTINE REVERSE_ORDER_1D_NOLESS_WILD(A, SWAP, P, INIT_P)
    CLASS(*), dimension(:), contiguous,           intent(inout) :: A
    INTEGER,  dimension(:), contiguous, optional, intent(inout) :: P
    LOGICAL,                            optional, intent(in   ) :: INIT_P
    PROCEDURE(SWAP_METHOD):: SWAP
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
              CALL REVERSE_ORDER_1D_DIM_WILD(dim, A, SWAP, noLESS, P=P, INIT_P=INIT_P)
    ELSE
              CALL REVERSE_ORDER_1D_DIM_WILD(dim, A, SWAP, noLESS)
    END IF
    !
    CONTAINS
    PURE FUNCTION noLESS(dim, A, i, j) RESULT(CMP)          !Function never called, but needed to complete interface
        INTEGER,                 intent(in) :: dim, i, j
        CLASS(*),dimension(dim), intent(in) :: A
        LOGICAL:: CMP
        !
        CMP = .FALSE.
    END FUNCTION
    !
  END SUBROUTINE
  !
  MODULE PURE SUBROUTINE REVERSE_ORDER_1D_NODIM_WILD(A, SWAP, LESS, STABLE, P, INIT_P)
    CLASS(*), dimension(:), contiguous,           intent(inout) :: A
    LOGICAL,                            optional, intent(in   ) :: STABLE
    INTEGER,  dimension(:), contiguous, optional, intent(inout) :: P
    LOGICAL,                            optional, intent(in   ) :: INIT_P
    PROCEDURE(SWAP_METHOD):: SWAP
    PROCEDURE(LESS_METHOD):: LESS
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
              CALL REVERSE_ORDER_1D_DIM_WILD(dim, A, SWAP, LESS, STABLE, P, INIT_P)
    ELSE
              CALL REVERSE_ORDER_1D_DIM_WILD(dim, A, SWAP, LESS, STABLE)
    END IF
    !
  END SUBROUTINE
  !
  MODULE PURE SUBROUTINE REVERSE_ORDER_1D_NOLESS_DIM_WILD(dim, A, SWAP, P, INIT_P)
    INTEGER,                            intent(in   ) :: dim
    CLASS(*), dimension(dim),           intent(inout) :: A
    INTEGER,  dimension(dim), optional, intent(inout) :: P
    LOGICAL,                  optional, intent(in   ) :: INIT_P
    PROCEDURE(SWAP_METHOD):: SWAP
    CALL REVERSE_ORDER_1D_DIM_WILD(dim, A, SWAP, noLESS, P=P, INIT_P=INIT_P)
    !
    CONTAINS
    PURE FUNCTION noLESS(dim, A, i, j) RESULT(CMP)          !Function never called, but needed to complete interface
        INTEGER,                 intent(in) :: dim, i, j
        CLASS(*),dimension(dim), intent(in) :: A
        LOGICAL:: CMP
        !
        CMP = .FALSE.
    END FUNCTION
    !
  END SUBROUTINE
  !
  MODULE PURE SUBROUTINE REVERSE_ORDER_1D_DIM_WILD(dim, A, SWAP, LESS, STABLE, P, INIT_P)
    INTEGER,                            intent(in   ) :: dim
    CLASS(*), dimension(dim),           intent(inout) :: A
    LOGICAL,                  optional, intent(in   ) :: STABLE
    INTEGER,  dimension(dim), optional, intent(inout) :: P
    LOGICAL,                  optional, intent(in   ) :: INIT_P
    PROCEDURE(SWAP_METHOD):: SWAP
    PROCEDURE(LESS_METHOD):: LESS
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
    CALL REVERSE_ORDER_1D_WILD(dim, A, SWAP)
    !
    IF(HAS_P) CALL REVERSE_ORDER_PERM(dim, P)
    !
    IF(present(STABLE)) THEN
      IF(STABLE) THEN       ! Search for repeated values and reverse their order so they appear in the original placement order
           I=1
           ISEARCH: DO WHILE (I < dim)      ! A(I:J) contains the range of repeated values
              !
              IF( LESS(dim, A, I, I+1) ) THEN    ! A(I) /= A(I+1)  <=>  A(I) < A(I+1) .or. A(I+1) < A(I) 
                           I = I + 1             !
                           CYCLE ISEARCH         !
              END IF                             !
              !                                  !
              IF( LESS(dim, A, I+1, I) ) THEN    ! A(I) /= A(I+1)
                           I = I + 1
                           CYCLE ISEARCH
              END IF
              J = I + 2
              !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
              JSEARCH: DO WHILE (J <= dim)
                  IF( LESS(dim, A, I, J) ) EXIT JSEARCH            ! A(I) /= A(J)  <=>  A(I) < A(J) .or. A(J) < A(I) 
                  IF( LESS(dim, A, J, I) ) EXIT JSEARCH            
                  J = J + 1
              END DO JSEARCH
              J = J - 1
              !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
              N = J - I + 1
              CALL REVERSE_ORDER_1D_WILD( N, A(I:J), SWAP )
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
  PURE SUBROUTINE REVERSE_ORDER_1D_WILD(dim, A, SWAP)
    INTEGER,                  intent(in   ) :: dim
    CLASS(*), dimension(dim), intent(inout) :: A
    PROCEDURE(SWAP_METHOD):: SWAP
    INTEGER:: I, J, HALF_DIM
    !
    IF(dim > 1) THEN
       !
       J = dim
       HALF_DIM = dim/2
       !
       DO I=1, HALF_DIM
           !
           CALL SWAP(dim, A, I, J)
           !
           J = J - 1
       END DO
    END IF
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
  !
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !
  ! Support and Utility Subroutines
  !
  !--------------------------------------------------------------------------------------------------------------------------
  ! Function that returns the max depth/recursion level that QSORT allows
  !
  PURE FUNCTION MAX_RECUR(dim) RESULT(MX)
    INTEGER, VALUE:: dim
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
  ! Simple Routine to SWAP values. --Compiler should optimize to not use TMP
  !
  PURE SUBROUTINE SWAP_P( dim, P, I, J)
     INTEGER,                        intent(in   ):: dim
     INTEGER,        dimension(dim), intent(inout):: P
     INTEGER,                        intent(in   ):: I, J
     INTEGER:: POS
     POS  = P(I)
     P(I) = P(J)
     P(J) = POS
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
  ! Determine sort option to use
  !
  PURE FUNCTION SORT_TYPE_WILD(dim, STABLE, QSORT, HEAP, ISORT, SymMerge) RESULT(ANS)
    INTEGER,             intent(in):: dim
    LOGICAL, optional, intent(in) :: STABLE, QSORT, HEAP, ISORT, SymMerge
    INTEGER:: ANS
    INTEGER:: REQUEST
    LOGICAL:: STABLE_SORT
    !            1 - ISORT            -> Odd options are stable sorts
    !            2 - HEAPsort
    !            3 - SYM-MERGE sort
    !            4 - QSORT
    !            5 - TIMSORT       (not implimented yet)
    !
    STABLE_SORT = .FALSE.
    IF(present(STABLE)) STABLE_SORT = STABLE
    !
    REQUEST = 0
    IF(present(ISORT)   ) REQUEST = 1
    IF(present(HEAP)    ) REQUEST = 2
    IF(present(SymMerge)) REQUEST = 3
    IF(present(QSORT)   ) REQUEST = 4
    !
    IF( REQUEST > 1 .and. STABLE_SORT) THEN
         IF(IAND(REQUEST,1) == 0) REQUEST = 0     ! Can't honor request because code does not support stable sort and even sort option
    END IF
    !
    IF    ( REQUEST > 0 ) THEN                   ! A specific sort option was requested
                     ANS = REQUEST
    ELSEIF( dim <= ISORT_SMALL_ARRAY_SIZE) THEN  ! Small arrays are fastest with Insertion Sort; which is also stable
                     ANS = 1
    ELSEIF( STABLE_SORT ) THEN                   ! Stable Sort Requested
                     ANS = 3
    ELSE
                     ANS = 4                     ! IntroSort/QSORT best for random data
    END IF
  END FUNCTION
END SUBMODULE
