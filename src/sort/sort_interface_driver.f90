!
!--------------------------------------------------------------------------------------------------------
!
! CODE DEVELOPED BY SCOTT E BOYCE 
!                   CONTACT <seboyce@usgs.gov> or <Boyce@engineer.com>
!
!--------------------------------------------------------------------------------------------------------
!
!  Generalized sorting routines that switches to best sort method based on array
!    Due to indexing limits, the largest array that can be sorted is 2147483647, 
!    anything bigger will result in a integer(int32) overflow. 
!       To sort larger arrays, it is best to compile with default INTEGER type as integer(int64)
!  
!  For small arrays, dim <= ISORT_SMALL_ARRAY_SIZE = 64, 
!    sort always uses Insertion Sort                          ( https://en.wikipedia.org/wiki/Insertion_sort )
!   
!  If a STABLE sort is requeseted, then sorted with SYMMERGE   ( Kim, P. S., & Kutzner, A. (2004, September). Stable minimum storage merging by symmetric comparisons. In European Symposium on Algorithms (pp. 714-723). Springer, Berlin, Heidelberg. )
!  
!  If the array is mostly sorted, then sorted with SYMMERGE
!    Mostly sorted is if COUNT( A(I) > A(I+1) ) < PARTIAL_SORTED_LIM * dim
!  
!  Otherwise, sort defaults to using
!    a modified Introsort/Introspective Sort                  ( https://en.wikipedia.org/wiki/Introsort )
!    that does:                                                                           
!      Starts with Dual-Pivot Quicksort                       ( https://en.wikipedia.org/wiki/Quicksort#Multi-pivot_quicksort )
!             with a recursion limit of 2Log2(DIM),
!      At recursion limit switch to Heap Sort                 ( https://en.wikipedia.org/wiki/Heapsort )
!      When the Quicksort branch size (siz) is 
!             siz <= QSORT_PARTITION_MIN = 24,  
!             the branch finished with Insertion Sort                   
!  
!--------------------------------------------------------------------------------------------------------
!    
! QuickRef Routine List 
!                          - See further down comments for indepth dscription
!                          - Argument within [] are optional, TYPE(A) indicates that the type is the same as A
!
! ** Fortran Base Type Sort     
!     
!       1D ->  CALL SORT(A,               [DESCEND],      [STABLE], [P] )    
!       2D ->  CALL SORT(A,   SORT_INDEX, [DESCEND], [SORT_BY_ROW], [P] )   
!              CALL SORT(A, SORT_INDICES, [DESCEND], [SORT_BY_ROW], [P] )  -> DESCEND is array of SIZE(SORT_INDICES)
!              CALL SORT(A, SORT_INDICES, [DESCEND], [SORT_BY_ROW], [P] )  -> DESCEND is a scalar
!    1D,1D ->  CALL SORT(A, B,  [SORT_B], [DESCEND],      [STABLE], [P] )  -> Sort by A and B such that A is sorted. Then sort B values that correspond with repeated values in A. 
!               
! 1D Funct ->  SORTED(A, DESCEND) RESULT(SORTED_ARRAY)
!     
! A             - is the array that is sorted, it must be Integer, Real, or Double Precision
! P             - Integer(:) permutation of A's' indices that puts the array into sorted order. That is, given the original array order, then P(I) = J, indicates that the Ith value is moved to the Jth location
! DESCEND       - Logical, FALSE by default, indicates that values are sorted in descending order. It can be specifed as a signle value or as an array of SIZE(SORT_INDICES)
! STABLE        - Logical, FALSE by default, indicates that a stable sort is performed. Only useful if providing P. Always set to TRUE for 2D arrays.
! SORT_BY_ROW   - Logical, FALSE by default, indicates that SORT_INDEX and SORT_INDICES specify rows to sort along (TRUE) or columns to sort along (FALSE).
! SORT_INDEX    - Integer, specify the column to sort in 2D array. If negative, then abs(SORT_INDEX) is the row to sort along. SORT_BY_ROW flag over rules sign of SORT_INDEX (that is SORT_INDEX>0 and SORT_BY_ROW=.TRUE. sorts by row)
! SORT_INDICES  - Integer(:), specify the rows/columns to sort in 2D array
! B             - is a 1D array, same size as A, which is sorted based on A. This is identical CALL SORT(M, SORT_INDICES=[1,2]) where M is an array with the first column=A and second=B; That is: M = [A|B]
!                    It must be Integer, Real, or Double Precision, but does not have to be the same as A
! SORT_B        - Logical, if present and false, then only sort A, and permutation sort B; otherwise also sort B along repeated values in A
!    
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~    
!    
! ** Fortran CLASS(*) Type Sort  
!     
!                     CALL SORT(A, SWAP, LESS, [STABLE], [P])
!    
! A    - is the array that is sorted, it must be Integer, Real, or Double Precision
! SWAP - Subroutine that fulfills SWAP_METHOD(A, B) interface (see next sections for examples)
! LESS - Subroutine that fulfills LESS_METHOD(A, B) interface (see next sections for examples)
!    
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
! ** Reverse Order Sort
!     
!       1D ->  CALL REVERSE_ORDER(A,      [STABLE], [P])
!              CALL REVERSE_ORDER(DIM, A, [STABLE], [P])
!       2D ->  CALL REVERSE_ORDER(A, [REVERSE_EACH_ROW])                        -> Reverses each column or rows in array
!              CALL REVERSE_ORDER(A, [REV_INDEX], [STABLE], [REVERSE_ROW], [P]) -> Reverses alone specified REV_INDEX, the rest of the columns/rows are shifted along with REV_INDEX
!     WILD ->  CALL REVERSE_ORDER(A, SWAP, LESS)  
!              CALL REVERSE_ORDER(DIM, A, SWAP, LESS)         
!     
! DIM               - Integer, is the size of A
! REVERSE_EACH_ROW  - Logical, FALSE by default, indicates that each row is reversed. False reverses each column.
! REVERSE_ROW       - Logical, FALSE by default, indicates that REV_INDEX specifies a row and not a column
! REV_INDEX         - Integer, specify the column to reverse the order in 2D array. The rows in the other columns are shifted with the REV_INDEX column. If negative, then abs(REV_INDEX) is the row that is reversed. REVERSE_ROW flag over rules sign of SORT_INDEX (that is REV_INDEX>0 and REVERSE_ROW=.TRUE. reverses the row) 
! SWAP              - Subroutine that fulfills SWAP_METHOD(A, B) interface (see next sections for examples)
! LESS              - Subroutine that fulfills LESS_METHOD(A, B) interface (see next sections for examples)
!    
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
! ** Use P to sort A
!
!       1D ->  CALL PERMUTATION_SORT(A, P)   
!       2D ->  CALL PERMUTATION_SORT(A, P, [BY_COLUMN])     
!     
! BY_COLUMN - Logical, TRUE by default, indicates that that P sorts each column in array, if false, then sort each row.
!    
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
! ** Helper Routine to Allocated P
!
!       CALL ALLOCATE_P(A, P)          
!     
! P - INTEGER, dimension(:), ALLOCATABLE
!    
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
! ** Fortran Base Type Multi-Vector Sort
!
!       CALL SORT(A,B,C,[D],[E],[F],[G],[H],[I],[J], [repeat_sort], [DESCEND], [P], [nat], [cap]) !  A, B, C, D, E, F, G, J, I, and J are sorted by the dim of A, so dim=size(A) <= size(B, C, D, E, F, G, H, I, J) 
!     
! A,B,C,D,E,F,G,H,I,J -> BASE_TYPEs, DIMENSION(:), all must be equal to or larger than size(A). Only sorts 1:size(A) range
!
! First sort A.
! Then permutation sort B based on the sort of A, sort values in B that correspond with repeated values in A.
! Then permutation sort C based on the sort of A and B, sort values in C that correspond with overlapped repeated values in A and B.
! Then permutation sort D based on the sort of A, B, and C, sort values in D that correspond with overlapped repeated values in A, B, and C.
! Continue same pattern for E, F, G, H, I, and J. 
!   --note that only A, B, and C are required and D, E, F, G, H, I, and J are optional.
!                                           
! This is identical to:
!                       CALL SORT(MV, SORT_INDICES=[1,2,3,4,5,6,7,8,9,10])
!   where MV is an array whose columns are composed A, B, C, D, E, F, G, H, I, and J
!     That is, MV = [ A|B|C|D|E|F|G|H|I|J ]
!
!  If repeat_sort is present and set to FALSE, then B, C, D, E, F, G, H, I, and J are only permutation sorted by A and not sorted by repeated values.
!
!  **Note that CALL SORT(A, B, [SORT_B], [DESCEND], [STABLE], [P]) are optimized versions of this multi-vector routine
!    --Note repeat_sort is equivalent to SORT_B
!    
!#########################################################################################################################
!#########################################################################################################################
!#########################################################################################################################
!    
! ISORT    => Insertion Sort        (    stable)
! QSORT    => Dual Pivot Quicksort  (not stable)  
! SymMerge => Symmetric Merge Sort  (    stable)
! HEAPSORT => Heap Sort             (not stable)
!  
!   Stable options tend to be slower, but preverse the original order of repeated values
!     For example: [3, 2, 1, 3, 5]  is sorted as  [1, 2, 3, 3, 5]
!                  A stable sort will palce the first 3 first, and the second 3 second, but unstable sorts order the two 3s at random. 
!                  This is only really important when using the sort of a vector to sort another vector.
!    
!   While QSORT and HEAPSORT are unstable, if the permutation vector, P, is provided, 
!     then the SORT algorithm will sort P by repeated values in A. 
!     This creates a psuedo-stable sort for QSORT and HEAPSORT.
!    
!   All sorts are done in ascending order 
!      --Descending order is possible with DESCEND=TRUE option
!          --Note, that this option multiplies the array by -1, sorts by ascending order, the multiplies again by -1
!
!  INTEGER and REAL Fortran Types can use prebuilt, optimized subroutines.
!    Otherwise you have to define the subroutine SWAP and LESS and use the CLASS(*) SORT (see examples below)
!
!  Sort 1D examples:
!                INTEGER,          dimension(3):: A
!                REAL,             dimension(3):: B
!                DOUBLE PRECISION, dimension(3):: C
!                INTEGER,          dimension(3):: PRM  -> is a permutation vector that keeps track of where values were moved from
!
!                A = [ 4  , 2  , 6   ]
!                B = [ 4E0, 2E0, 6E0 ]
!                C = [ 4D0, 2D0, 6D0 ]
!
!                CALL SORT(A)
!                CALL SORT(B)
!                CALL SORT(C, DESCEND=.TRUE.)  -> Descending Order Sort
!
!                A = [ 4  , 3  , 2   ]
!                B = [ 4E0, 2E0, 6E0 ]
!
!                CALL SORT(A, P=PRM)
!                CALL PERMUTATION_SORT(3, B, PRM)  ! Sort B as defined by PRM, which matches the sort arrangement of A
!
!                A = [ 4  , 3  , 2   ]
!                B = [ 4E0, 2E0, 6E0 ]
!
!                CALL SORT(A, B, P=PRM) ! Sort by A and B such that A is sorted. Then sort B values that correspond with repeated values in A. 
!                                           This is identical CALL SORT(M, [1,2]) where N is an array with the first column A and second  B; That is: N = [A|B]
!    
!##########################################################################################################################
!
! Optimized SORT options
!
! Array that is sorted must be one of the following BASE_TYPEs:
!   INTEGER, INTEGER(INT16), INTEGER(INT32), INTEGER(INT64), 
!   REAL, REAL(REAL32), REAL(REAL64), and CHARACTER(*)
!
!       --Note that 
!                   CHARACTER(*)     Default is to do a basic ASCII text sort based on the ASCII number (A < B < C < a < b < c)
!                                          Options cap=.TRUE. will do a             case free sort ( A < a < B < b < C < c )
!                                          Options nat=.TRUE. will do a natural and case free sort ( A1, a1, A10, a10, Y1, y1, Y10, y10 )
!                   INTEGER          typically is the same as INTEGER(INT32)
!                   REAL             typically is the same as   REAL(REAL32)
!                   DOUBLE PRECISION typically is the same as   REAL(REAL64)
!
! 1D Arrays are defined as DIMENSION(:),   or DIMENSION(DIM),       or DIMENSION(*)
! 2D Arrays are defined as DIMENSION(:,:), or DIMENSION(DIM1,DIM2)                 
!
!
! 1D Arrays have two Driver Routines.
!   The first has custom sort options and scans array to determine best sortting method
!   The second directly calls the QSORT or SYMMERG sort for speed. 
!--------------------------------------------------------------------------------------------------------------------------
! Opt 1:   CALL SORT(A, [DESCEND], [STABLE], [P])                                                                         |
!--------------------------------------------------------------------------------------------------------------------------
!                                                                                                                         |
!          BASE_TYPE, dimension(:),     contiguous, intent(inout):: A                                                     |
!          LOGICAL,                       optional, intent(in   ):: DESCEND                                               |
!          LOGICAL,                       optional, intent(in   ):: STABLE                                                |
!          INTEGER,   dimension(size(a)), optional, intent(inout):: P                                                     |
!                                                                                                                         |
!--------------------------------------------------------------------------------------------------------------------------
!
!
!--------------------------------------------------------------------------------------------------------------------------
! Opt 2:   CALL SORT(DIM, A, [P])                                                                                         |
!--------------------------------------------------------------------------------------------------------------------------
!                                                                                                                         |
!          INTEGER,                             intent(in   ):: DIM                                                       |
!          BASE_TYPE, dimension(dim),           intent(inout):: A                                                         |
!          INTEGER,   dimension(dim), optional, intent(inout):: P                                                         |
!                                                                                                                         |
!                   --> Uses SYMMERG when P is present, otherwise uses QSORT                                              |
!                                                                                                                         |
!--------------------------------------------------------------------------------------------------------------------------
!
!
!##########################################################################################################################
!  
!  Non-Standard Data Types can use the WILDcard SORT
!     But you must supply two subroutines that follow the following two INTERFACES
!         --Note that the next section gives examples of how to impliment these interfaces for the DATE_OPERATOR_INSTRUCTION Module
!
!      ABSTRACT INTERFACE
!               PURE SUBROUTINE SWAP_METHOD(dim, A, i, j)
!                   INTEGER,                 intent(in   ) :: dim, i, j
!                   CLASS(*),dimension(dim), intent(inout) :: A
!               END SUBROUTINE
!               !
!               PURE FUNCTION LESS_METHOD(dim, A, i, j) RESULT(CMP)
!                   INTEGER,                 intent(in) :: dim, i, j
!                   CLASS(*),dimension(dim), intent(in) :: A
!                   LOGICAL:: CMP
!               END FUNCTION
!      END INTERFACE
!
!
!##########################################################################################################################
!##########################################################################################################################
!##########################################################################################################################
!
! Example of using SORT with CLASS(*) and external Subroutines
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!PURE SUBROUTINE SWAP_DATE(dim, A, i, j)                     -> Defining the ABSTRACT SWAP_METHOD Interface
!    USE DATE_OPERATOR_INSTRUCTION, ONLY: DATE_OPERATOR
!    INTEGER,                 intent(in   ) :: dim, i, j
!    CLASS(*),dimension(dim), intent(inout) :: A
!    TYPE(DATE_OPERATOR) :: TMP 
!    !
!    SELECT TYPE(A)
!    TYPE IS( DATE_OPERATOR )       ! Note that DATE_OPERATOR defines what to do for an equal sign by including  generic:: ASSIGNMENT(=) 
!                  TMP  = A(i)
!                  A(i) = A(j)
!                  A(j) = TMP
!    END SELECT
!END SUBROUTINE
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!PURE FUNCTION LESS_DATE(dim, A, i, j) RESULT(CMP)           -> Defining the ABSTRACT LESS_METHOD Interface
!    USE DATE_OPERATOR_INSTRUCTION, ONLY: DATE_OPERATOR
!    INTEGER,                  intent(in) :: dim, i, j
!    CLASS(*), dimension(dim), intent(in) :: A
!    LOGICAL:: CMP
!    
!    SELECT TYPE(A)
!    TYPE IS( DATE_OPERATOR )                          ! Use decimal year as comparison value, since generic:: OPERATOR(<) is not defined
!                  CMP = A(i)%DYEAR < A(j)%DYEAR       ! To get ascending order flip less than sign
!    END SELECT                                        
!END FUNCTION
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!SUBROUTINE SORT_EXAMPLE1()
!  USE DATE_OPERATOR_INSTRUCTION, ONLY: DATE_OPERATOR
!  USE SORT_INTERFACE, ONLY: SORT
!  IMPLICIT NONE
!  TYPE(DATE_OPERATOR), dimension(3):: DATE
!  !
!  ! Since routines are not in a module or internal to this subroutine with a CONTAINS directive,
!  !   An interface is required to inform this subroutine of what the SWAP and LESS procedures do
!  INTERFACE
!       PURE SUBROUTINE SWAP_DATE(dim, A, i, j)              ! Defining the ABSTRACT SWAP_METHOD Interface
!            INTEGER,                 intent(in   ) :: dim, i, j
!            CLASS(*),dimension(dim), intent(inout) :: A
!       END SUBROUTINE
!       !
!       PURE FUNCTION LESS_DATE(dim, A, i, j) RESULT(CMP)              ! Defining the ABSTRACT LESS_METHOD Interface
!            INTEGER,                 intent(in) :: dim, i, j
!            CLASS(*),dimension(dim), intent(in) :: A
!            LOGICAL:: CMP
!       END FUNCTION
!  END INTERFACE
!  
!  DATE(1) = '4/23/1979'
!  DATE(2) = '1/15/1979'
!  DATE(3) = '4/15/1979'
!  
!  CALL SORT(DATE, SWAP_DATE, LESS_DATE)
!  
!END SUBROUTINE
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!SUBROUTINE SORT_EXAMPLE2()
!  USE DATE_OPERATOR_INSTRUCTION, ONLY: DATE_OPERATOR
!  USE SORT_INTERFACE, ONLY: SORT, SWAP_METHOD, LESS_METHOD
!  IMPLICIT NONE
!  
!  ! An alternative is to bring in the abstract interfaces from 
!  !   the module and declare the SWAP and LESS procedures here
!  PROCEDURE(SWAP_METHOD):: SWAP_DATE                            ! Tells compiler that SWAP_DATE has the same interface as abstract interface SWAP_METHOD
!  PROCEDURE(LESS_METHOD):: LESS_DATE                            ! Tells compiler that LESS_DATE has the same interface as abstract interface LESS_METHOD
!  
!  TYPE(DATE_OPERATOR), dimension(3):: DATE
!  
!  DATE(1) = '4/23/1979'
!  DATE(2) = '1/15/1979'
!  DATE(3) = '4/15/1979'
!  
!  CALL SORT(DATE, SWAP_DATE, LESS_DATE)
!  
!END SUBROUTINE
!    
!##########################################################################################################################
!##########################################################################################################################
!##########################################################################################################################
!
! Example of using SORT with CLASS(*) and module Subroutines, which provides an implicit interface to SWAP_DATE and LESS_DATE
!
!MODULE SORT_EXAMPLE3_MOD
!  USE DATE_OPERATOR_INSTRUCTION, ONLY: DATE_OPERATOR
!  USE SORT_INTERFACE, ONLY: SORT
!  IMPLICIT NONE
!  
!  PRIVATE:: SWAP_DATE, LESS_DATE  ! Recommended since these procedures are only used within the module
!  
!  CONTAINS
!  
!  SUBROUTINE SORT_EXAMPLE3()
!     !
!     ! SWAP_DATE and LESS_DATE have an implied interface from MODULE import
!     !
!     TYPE(DATE_OPERATOR), dimension(3):: DATE
!     
!     DATE(1) = '4/23/1979'
!     DATE(2) = '1/15/1979'
!     DATE(3) = '4/15/1979'
!     
!     CALL SORT(DATE, SWAP_DATE, LESS_DATE)  ! 
!     
!  END SUBROUTINE
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!  PURE SUBROUTINE SWAP_DATE(dim, A, i, j)                     -> Definwa the ABSTRACT SWAP_METHOD Interface
!      INTEGER,                 intent(in   ) :: dim, i, j
!      CLASS(*),dimension(dim), intent(inout) :: A
!      TYPE(DATE_OPERATOR) :: TMP 
!      !
!      SELECT TYPE(A)
!      TYPE IS( DATE_OPERATOR )
!                    TMP  = A(i)
!                    A(i) = A(j)
!                    A(j) = TMP
!      END SELECT
!  END SUBROUTINE
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!  PURE FUNCTION LESS_DATE(dim, A, i, j) RESULT(CMP)           -> Defining the ABSTRACT LESS_METHOD Interface
!      INTEGER,                  intent(in) :: dim, i, j
!      CLASS(*), dimension(dim), intent(in) :: A
!      LOGICAL:: CMP
!      
!      SELECT TYPE(A)
!      TYPE IS( DATE_OPERATOR )
!                    CMP = A(i)%DYEAR < A(j)%DYEAR       ! To get ascending order flip less than sign
!      END SELECT
!  END FUNCTION
!  
!END MODULE
!    
!##########################################################################################################################
!##########################################################################################################################
!##########################################################################################################################
!
! Code Design
!           MODULE SORT_INTERFACE
!                           |->  SUBMODULE SORT_INTERFACE_INT32 <--------^^
!                           |->  SUBMODULE SORT_INTERFACE_REL64       ==>||  These 3 SUBMODULES are identical to SORT_INTERFACE_INT32
!                           |->  SUBMODULE SORT_INTERFACE_INT64       ==>||    Except they are optimized for 
!                           |->  SUBMODULE SORT_INTERFACE_REL32       ==>||    their specific type (viz REL64, INT64, REL32)
!                           |->  SUBMODULE SORT_INTERFACE_CHAR
!                           |->  SUBMODULE SORT_INTERFACE_WILD
!                           |->  SUBMODULE SORT_INTERFACE_1D_MULTI
!    
!      If you only want to one the SORT_INTERFACE_xyz submodules and nothing else, 
!      they can be converted to a standalone module by:
!
!    Move the SUBMODULE code to a separate file
!
!    In the separate file:
!         Rename SUBMODULE (SORT_INTERFACE) SORT_INTERFACE_xyz
!             to MODULE SORT_INTERFACE_xyz 
!           and
!                END SUBMODULE
!            to  END    MODULE
!         
!         Change all MODULE PURE SUBROUTINE and MODULE PURE FUNCTION
!            to just        PURE SUBROUTINE and        PURE FUNCTION
!         
!         Uncomment the PUBLIC, PRIVATE and INTERFACES sections at the start of the submodule.
!           They contain !\! in front of them to make it easy to find.
!
!    Now you can compile it as a standalone MODULE for a specific TYPE
!    
!##########################################################################################################################
!##########################################################################################################################
!##########################################################################################################################
!
MODULE SORT_INTERFACE
  USE, INTRINSIC:: ISO_FORTRAN_ENV, ONLY: INT8, INT32, INT64, REL32 => REAL32, REL64 => REAL64
  IMPLICIT NONE
  !
  PUBLIC:: SORT, SORTED
  PUBLIC:: SWAP_METHOD, LESS_METHOD  ! Abstract Interface to User Supplied Subroutines neccesary for WILD routines 
  PUBLIC:: PERMUTATION_SORT,  ALLOCATE_P
  PUBLIC:: REVERSE_ORDER
  !
  PRIVATE
  !
  !------------------------------------------------------------------------------------------------
  !   Required Routines for WILD sorts
  !------------------------------------------------------------------------------------------------
  ABSTRACT INTERFACE
               PURE SUBROUTINE SWAP_METHOD(dim, A, i, j)
                   INTEGER,                 intent(in   ) :: dim, i, j
                   CLASS(*),dimension(dim), intent(inout) :: A
                   !TYPE(TYP):: TMP                  ! Commented code serves as a template to build routine
                   !!
                   !SELECT TYPE(A)
                   !TYPE IS( TYP )
                   !              TMP  = A(i)
                   !              A(i) = A(j)
                   !              A(j) = TMP
                   !END SELECT
               END SUBROUTINE
               !
               !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
               !
               PURE FUNCTION LESS_METHOD(dim, A, i, j) RESULT(CMP)
                   INTEGER,                 intent(in) :: dim, i, j
                   CLASS(*),dimension(dim), intent(in) :: A
                   LOGICAL:: CMP
                   !
                   !SELECT TYPE(A)
                   !TYPE IS( TYP )
                   !              CMP = A(i) < A(j)       ! To get ascending order flip less than sign
                   !END SELECT
               END FUNCTION
               !
  END INTERFACE
  !
  !------------------------------------------------------------------------------------------------
  !   Main SORT Generic Routines
  !------------------------------------------------------------------------------------------------
  !
  INTERFACE SORT
    !
    MODULE PROCEDURE SORT_1D_WILD              ! SORT(A, SWAP, LESS, [STABLE], [P], [QSORT], [HEAP,] [ISORT], [SymMerge])
    !                                          
    MODULE PROCEDURE SORT_1D_INT32             ! SORT(A, [DESCEND], [STABLE], [P], [SORT_TYPE], [QSORT], [HEAP], [ISORT], [SymMerge], [INIT_P])
    MODULE PROCEDURE SORT_2D_INDEX_INT32       ! SORT(A, SORT_INDEX,   [DESCEND], [SORT_BY_ROW], [P], [NO_STABLE])   
    MODULE PROCEDURE SORT_2D_MULTI_INT32       ! SORT(A, SORT_INDICES, [DESCEND], [SORT_BY_ROW], [P], [NO_STABLE])  -> DESCEND is array of SIZE(SORT_INDICES)
    MODULE PROCEDURE SORT_2D_MULTI_INT32_1DSC  ! SORT(A, SORT_INDICES, [DESCEND], [SORT_BY_ROW], [P], [NO_STABLE])  -> DESCEND is a scalar
    !
    MODULE PROCEDURE SORT_1D_QSORT_A_INT32     ! SORT(DIM, A)     
    MODULE PROCEDURE SORT_1D_SymMg_P_INT32     ! SORT(DIM, A, P)
    !
    MODULE PROCEDURE SORT_1D_MULTI_DSC1_WILD_WILD_WILD    ! SORT(A,B,C,[D],[E],[F],[G],[H],[I],[J], [repeat_sort], [DESCEND],    [P], [nat], [cap]) !  A, B, C, D, E, F, and G are sorted by the dim of A, so dim=size(A) <= size(B, C, D, E, F, G)
    MODULE PROCEDURE SORT_1D_MULTI_DSC2_WILD_WILD_WILD    ! SORT(A,B,C,[D],[E],[F],[G],[H],[I],[J], [repeat_sort], [DESCEND(:)], [P], [nat], [cap]) ! descend is an array of length equal to the number of vetors to be sorted
    MODULE PROCEDURE SORT_1D_MULTI_DSC1_INT32_INT32       ! SORT(A, B, [SORT_B], [DESCEND],    [STABLE], [P], [QSORT], [HEAP], [ISORT], [SymMerge])
    MODULE PROCEDURE SORT_1D_MULTI_DSC2_INT32_INT32       ! SORT(A, B, [SORT_B], [DESCEND(2)], [STABLE], [P], [QSORT], [HEAP], [ISORT], [SymMerge]) 
    MODULE PROCEDURE SORT_1D_MULTI_DSC1_INT32_INT64
    MODULE PROCEDURE SORT_1D_MULTI_DSC2_INT32_INT64
    MODULE PROCEDURE SORT_1D_MULTI_DSC1_INT32_REL32
    MODULE PROCEDURE SORT_1D_MULTI_DSC2_INT32_REL32
    MODULE PROCEDURE SORT_1D_MULTI_DSC1_INT32_REL64
    MODULE PROCEDURE SORT_1D_MULTI_DSC2_INT32_REL64
    MODULE PROCEDURE SORT_1D_MULTI_DSC1_INT32_CHAR
    MODULE PROCEDURE SORT_1D_MULTI_DSC2_INT32_CHAR
    !
    MODULE PROCEDURE SORT_1D_MULTI_DSC1_INT64_INT32 
    MODULE PROCEDURE SORT_1D_MULTI_DSC2_INT64_INT32 
    MODULE PROCEDURE SORT_1D_MULTI_DSC1_INT64_INT64
    MODULE PROCEDURE SORT_1D_MULTI_DSC2_INT64_INT64
    MODULE PROCEDURE SORT_1D_MULTI_DSC1_INT64_REL32
    MODULE PROCEDURE SORT_1D_MULTI_DSC2_INT64_REL32
    MODULE PROCEDURE SORT_1D_MULTI_DSC1_INT64_REL64
    MODULE PROCEDURE SORT_1D_MULTI_DSC2_INT64_REL64
    MODULE PROCEDURE SORT_1D_MULTI_DSC1_INT64_CHAR
    MODULE PROCEDURE SORT_1D_MULTI_DSC2_INT64_CHAR
    !
    MODULE PROCEDURE SORT_1D_MULTI_DSC1_REL32_INT32 
    MODULE PROCEDURE SORT_1D_MULTI_DSC2_REL32_INT32 
    MODULE PROCEDURE SORT_1D_MULTI_DSC1_REL32_INT64
    MODULE PROCEDURE SORT_1D_MULTI_DSC2_REL32_INT64
    MODULE PROCEDURE SORT_1D_MULTI_DSC1_REL32_REL32
    MODULE PROCEDURE SORT_1D_MULTI_DSC2_REL32_REL32
    MODULE PROCEDURE SORT_1D_MULTI_DSC1_REL32_REL64
    MODULE PROCEDURE SORT_1D_MULTI_DSC2_REL32_REL64
    MODULE PROCEDURE SORT_1D_MULTI_DSC1_REL32_CHAR
    MODULE PROCEDURE SORT_1D_MULTI_DSC2_REL32_CHAR
    !
    MODULE PROCEDURE SORT_1D_MULTI_DSC1_REL64_INT32 
    MODULE PROCEDURE SORT_1D_MULTI_DSC2_REL64_INT32 
    MODULE PROCEDURE SORT_1D_MULTI_DSC1_REL64_INT64
    MODULE PROCEDURE SORT_1D_MULTI_DSC2_REL64_INT64
    MODULE PROCEDURE SORT_1D_MULTI_DSC1_REL64_REL32
    MODULE PROCEDURE SORT_1D_MULTI_DSC2_REL64_REL32
    MODULE PROCEDURE SORT_1D_MULTI_DSC1_REL64_REL64
    MODULE PROCEDURE SORT_1D_MULTI_DSC2_REL64_REL64
    MODULE PROCEDURE SORT_1D_MULTI_DSC1_REL64_CHAR
    MODULE PROCEDURE SORT_1D_MULTI_DSC2_REL64_CHAR
    !
    MODULE PROCEDURE SORT_1D_MULTI_DSC1_CHAR_INT32 
    MODULE PROCEDURE SORT_1D_MULTI_DSC2_CHAR_INT32 
    MODULE PROCEDURE SORT_1D_MULTI_DSC1_CHAR_INT64
    MODULE PROCEDURE SORT_1D_MULTI_DSC2_CHAR_INT64
    MODULE PROCEDURE SORT_1D_MULTI_DSC1_CHAR_REL32
    MODULE PROCEDURE SORT_1D_MULTI_DSC2_CHAR_REL32
    MODULE PROCEDURE SORT_1D_MULTI_DSC1_CHAR_REL64
    MODULE PROCEDURE SORT_1D_MULTI_DSC2_CHAR_REL64
    MODULE PROCEDURE SORT_1D_MULTI_DSC1_CHAR_CHAR
    MODULE PROCEDURE SORT_1D_MULTI_DSC2_CHAR_CHAR
    !
    MODULE PROCEDURE SORT_1D_INT64            
    MODULE PROCEDURE SORT_2D_INDEX_INT64      
    MODULE PROCEDURE SORT_2D_MULTI_INT64      
    MODULE PROCEDURE SORT_2D_MULTI_INT64_1DSC 
    !
    MODULE PROCEDURE SORT_1D_REL32            
    MODULE PROCEDURE SORT_2D_INDEX_REL32      
    MODULE PROCEDURE SORT_2D_MULTI_REL32      
    MODULE PROCEDURE SORT_2D_MULTI_REL32_1DSC 
    !
    MODULE PROCEDURE SORT_1D_REL64            
    MODULE PROCEDURE SORT_2D_INDEX_REL64      
    MODULE PROCEDURE SORT_2D_MULTI_REL64      
    MODULE PROCEDURE SORT_2D_MULTI_REL64_1DSC     
    !
    MODULE PROCEDURE SORT_1D_CHAR            
    !
    MODULE PROCEDURE SORT_1D_QSORT_A_INT64 
    MODULE PROCEDURE SORT_1D_SymMg_P_INT64  
    !
    MODULE PROCEDURE SORT_1D_QSORT_A_REL32 
    MODULE PROCEDURE SORT_1D_SymMg_P_REL32 
    !
    MODULE PROCEDURE SORT_1D_QSORT_A_REL64 
    MODULE PROCEDURE SORT_1D_SymMg_P_REL64 
    !
    MODULE PROCEDURE SORT_1D_QSORT_A_CHAR 
    MODULE PROCEDURE SORT_1D_SymMg_P_CHAR 
  END INTERFACE
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !
  ! Function returns sorted array
  INTERFACE SORTED            
    MODULE PROCEDURE SORTED_1D_INT32          ! SORTED(A, DESCEND) RESULT(SORTED_ARRAY)
    MODULE PROCEDURE SORTED_1D_INT64
    MODULE PROCEDURE SORTED_1D_REL32
    MODULE PROCEDURE SORTED_1D_REL64
    MODULE PROCEDURE SORTED_1D_CHAR
  END INTERFACE
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !
  ! Reverse the array order such that the first value is the last and second is second to last, and so forth...and last value becomes first, etc
  INTERFACE REVERSE_ORDER  
    !
    MODULE PROCEDURE REVERSE_ORDER_1D_NODIM_INT32        ! REVERSE_ORDER(A, [STABLE], [P], [INIT_P])
    MODULE PROCEDURE REVERSE_ORDER_1D_DIM_INT32          ! REVERSE_ORDER(DIM, A, [STABLE], [P], [INIT_P])
    MODULE PROCEDURE REVERSE_ORDER_2D_INT32              ! REVERSE_ORDER(A, [REVERSE_EACH_ROW])  --> Default REVERSE_EACH_ROW is TRUE
    MODULE PROCEDURE REVERSE_ORDER_2D_INDEX_INT32        ! REVERSE_ORDER(A, REV_INDEX, [STABLE], [REVERSE_ROW], [P])  --> Specify Row to reverse order along
    !
    MODULE PROCEDURE REVERSE_ORDER_1D_NODIM_WILD         ! REVERSE_ORDER(A, SWAP, LESS, [STABLE], [P], [INIT_P]) 
    MODULE PROCEDURE REVERSE_ORDER_1D_NOLESS_WILD        ! REVERSE_ORDER(A, SWAP,                 [P], [INIT_P])
    MODULE PROCEDURE REVERSE_ORDER_1D_DIM_WILD           ! REVERSE_ORDER(dim, A, SWAP, LESS, [STABLE], [P], [INIT_P]) 
    MODULE PROCEDURE REVERSE_ORDER_1D_NOLESS_DIM_WILD    ! REVERSE_ORDER(dim, A, SWAP,                 [P], [INIT_P])
    !
    MODULE PROCEDURE REVERSE_ORDER_1D_NODIM_INT64        
    MODULE PROCEDURE REVERSE_ORDER_1D_DIM_INT64              
    MODULE PROCEDURE REVERSE_ORDER_2D_INT64              
    MODULE PROCEDURE REVERSE_ORDER_2D_INDEX_INT64
    !
    MODULE PROCEDURE REVERSE_ORDER_1D_NODIM_REL32
    MODULE PROCEDURE REVERSE_ORDER_1D_DIM_REL32      
    MODULE PROCEDURE REVERSE_ORDER_2D_REL32              
    MODULE PROCEDURE REVERSE_ORDER_2D_INDEX_REL32
    !
    MODULE PROCEDURE REVERSE_ORDER_1D_NODIM_REL64
    MODULE PROCEDURE REVERSE_ORDER_1D_DIM_REL64      
    MODULE PROCEDURE REVERSE_ORDER_2D_REL64              
    MODULE PROCEDURE REVERSE_ORDER_2D_INDEX_REL64
    !
    !
    MODULE PROCEDURE REVERSE_ORDER_1D_NODIM_CHAR
    MODULE PROCEDURE REVERSE_ORDER_1D_DIM_CHAR      
    MODULE PROCEDURE REVERSE_ORDER_2D_CHAR              
    MODULE PROCEDURE REVERSE_ORDER_2D_INDEX_CHAR
  END INTERFACE
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !
  INTERFACE PERMUTATION_SORT
    MODULE PROCEDURE PERMUTATION_SORT_1D_INT32     ! PERMUTATION_SORT(A, P)
    MODULE PROCEDURE PERMUTATION_SORT_2D_INT32     ! PERMUTATION_SORT(A, P, [BY_COLUMN])
    MODULE PROCEDURE PERMUTATION_SORT_1D_WILD      ! PERMUTATION_SORT(A, P, SWAP)
    MODULE PROCEDURE PERMUTATION_SORT_1D_LESS_WILD ! PERMUTATION_SORT(A, P, SWAP, LESS)
    MODULE PROCEDURE PERMUTATION_SORT_1D_INT64
    MODULE PROCEDURE PERMUTATION_SORT_2D_INT64
    MODULE PROCEDURE PERMUTATION_SORT_1D_REL32
    MODULE PROCEDURE PERMUTATION_SORT_2D_REL32
    MODULE PROCEDURE PERMUTATION_SORT_1D_REL64
    MODULE PROCEDURE PERMUTATION_SORT_2D_REL64
    MODULE PROCEDURE PERMUTATION_SORT_1D_CHAR
    MODULE PROCEDURE PERMUTATION_SORT_2D_CHAR
  END INTERFACE
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !
  INTERFACE ALLOCATE_P
    MODULE PROCEDURE ALLOCATE_PERM_INT32  ! ALLOCATE_P(A, P)
    MODULE PROCEDURE ALLOCATE_PERM_INT64
    MODULE PROCEDURE ALLOCATE_PERM_REL32
    MODULE PROCEDURE ALLOCATE_PERM_REL64
    MODULE PROCEDURE ALLOCATE_PERM_CHAR
  END INTERFACE
  !
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !
  INTERFACE SORT_TYPE ! SORT_TYPE((dim, A, [DESCEND], [STABLE], [QSORT], [HEAP], [ISORT], [SymMerge])
    !   0 - Array is sorted
    !   1 - ISORT            - Odd options are stable sorts
    !   2 - HEAPsort
    !   3 - SYM-MERGE sort
    !   4 - QSORT         (Techically its an Intro-Sort with Dual Pivot Quick, Heap, and Insertion Sorts)
    !   5 - TIMSORT       (not implimented yet - Requires buffer size diBm/2)
    MODULE PROCEDURE SORT_TYPE_INT32
    MODULE PROCEDURE SORT_TYPE_INT64
    MODULE PROCEDURE SORT_TYPE_REL32
    MODULE PROCEDURE SORT_TYPE_REL64
    MODULE PROCEDURE SORT_TYPE_CHAR
  END INTERFACE
  !
  INTERFACE SORT_B_by_A_and_P
    MODULE PROCEDURE SORT_B_by_A_and_P_INT32_INT32
    MODULE PROCEDURE SORT_B_by_A_and_P_INT64_INT32
    MODULE PROCEDURE SORT_B_by_A_and_P_REL32_INT32
    MODULE PROCEDURE SORT_B_by_A_and_P_REL64_INT32
    MODULE PROCEDURE  SORT_B_by_A_and_P_CHAR_INT32
    !
    MODULE PROCEDURE SORT_B_by_A_and_P_INT32_INT64
    MODULE PROCEDURE SORT_B_by_A_and_P_INT64_INT64
    MODULE PROCEDURE SORT_B_by_A_and_P_REL32_INT64
    MODULE PROCEDURE SORT_B_by_A_and_P_REL64_INT64
    MODULE PROCEDURE  SORT_B_by_A_and_P_CHAR_INT64
    !
    MODULE PROCEDURE SORT_B_by_A_and_P_INT32_REL32
    MODULE PROCEDURE SORT_B_by_A_and_P_INT64_REL32
    MODULE PROCEDURE SORT_B_by_A_and_P_REL32_REL32
    MODULE PROCEDURE SORT_B_by_A_and_P_REL64_REL32
    MODULE PROCEDURE  SORT_B_by_A_and_P_CHAR_REL32
    !
    MODULE PROCEDURE SORT_B_by_A_and_P_INT32_REL64
    MODULE PROCEDURE SORT_B_by_A_and_P_INT64_REL64
    MODULE PROCEDURE SORT_B_by_A_and_P_REL32_REL64
    MODULE PROCEDURE SORT_B_by_A_and_P_REL64_REL64
    MODULE PROCEDURE  SORT_B_by_A_and_P_CHAR_REL64
    !
    MODULE PROCEDURE SORT_B_by_A_and_P_INT32_CHAR
    MODULE PROCEDURE SORT_B_by_A_and_P_INT64_CHAR
    MODULE PROCEDURE SORT_B_by_A_and_P_REL32_CHAR
    MODULE PROCEDURE SORT_B_by_A_and_P_REL64_CHAR
    MODULE PROCEDURE  SORT_B_by_A_and_P_CHAR_CHAR
  END INTERFACE
  !
  INTERFACE SETUP_REP
    MODULE PROCEDURE SETUP_REP_INT32
    MODULE PROCEDURE SETUP_REP_INT64
    MODULE PROCEDURE SETUP_REP_REL32
    MODULE PROCEDURE SETUP_REP_REL64
    MODULE PROCEDURE SETUP_REP_CHAR
  END INTERFACE
  !
  INTERFACE SORT_REP
    MODULE PROCEDURE SORT_B_by_A_and_P_REP_INT32
    MODULE PROCEDURE SORT_B_by_A_and_P_REP_INT64
    MODULE PROCEDURE SORT_B_by_A_and_P_REP_REL32
    MODULE PROCEDURE SORT_B_by_A_and_P_REP_REL64
    MODULE PROCEDURE SORT_B_by_A_and_P_REP_CHAR
  END INTERFACE
  !
  !------------------------------------------------------------------------------------------------
  !
  !   Submodule Interfaces - These are Required for the SUBMODULE connection
  !
  !------------------------------------------------------------------------------------------------
  !
  INTERFACE ! SORT_INTERFACE_INT32
      !
      MODULE PURE SUBROUTINE ALLOCATE_PERM_INT32(A, P)    
             INTEGER(INT32), dimension(:),              intent(in   ):: A
             INTEGER,        dimension(:), allocatable, intent(inout):: P
      END SUBROUTINE
      !
      MODULE PURE FUNCTION SORTED_1D_INT32(A, DESCEND, STABLE) RESULT(SORTED_ARRAY)
             INTEGER(INT32), dimension(:), contiguous,           intent(in):: A
             LOGICAL,                                  optional, intent(in):: DESCEND, STABLE
             INTEGER(INT32), dimension(SIZE(A)):: SORTED_ARRAY
      END FUNCTION
      !
      MODULE PURE FUNCTION SORT_TYPE_INT32(dim, A, DESCEND, STABLE, QSORT, HEAP, ISORT, SymMerge) RESULT(ANS)
             INTEGER,                        intent(in) :: dim
             INTEGER(INT32), dimension(dim), intent(in) :: A
             LOGICAL,              optional, intent(in) :: DESCEND, STABLE, QSORT, HEAP, ISORT, SymMerge
             INTEGER:: ANS
      END FUNCTION
      !
      MODULE PURE SUBROUTINE SORT_1D_INT32(A, DESCEND, STABLE, P, SORT_TYPE, QSORT, HEAP, ISORT, SymMerge , INIT_P)
             INTEGER(INT32), dimension(:), contiguous,    intent(inout) :: A
             LOGICAL,                           optional, intent(in   ) :: DESCEND   ! Sort in Decending Order
             LOGICAL,                           optional, intent(in   ) :: STABLE    ! Use only Stable Sorting Methods
             INTEGER, dimension(:), contiguous, optional, intent(inout) :: P         ! Contains permutations to sort A (Sorts along with A)
             INTEGER,                           optional, intent(in   ) :: SORT_TYPE ! If present, then disables call to SORT_TYPE sets the sort type to the optiona defined. It must be a valid integer.
             LOGICAL,                           optional, intent(in   ) :: QSORT     ! Use only Dual Pivot Quicksort/IntroSort
             LOGICAL,                           optional, intent(in   ) :: HEAP      ! Use only Heap Sort
             LOGICAL,                           optional, intent(in   ) :: ISORT     ! Use only Insertion Sort
             LOGICAL,                           optional, intent(in   ) :: SymMerge  ! Use only Stable Minimum Storage Merging by Symmetric Comparisons
             LOGICAL,                           optional, intent(in   ) :: INIT_P    ! Set P = [1...to...DIM]; Default is True; which makes P a permutation vector for how A is sorted
      END SUBROUTINE
      !
      MODULE PURE SUBROUTINE SORT_1D_QSORT_A_INT32(DIM, A)
             INTEGER,                        intent(in   ):: DIM
             INTEGER(INT32), dimension(DIM), intent(inout):: A
      END SUBROUTINE
      !
      MODULE PURE SUBROUTINE SORT_1D_SymMg_P_INT32(dim, A, P)
             INTEGER,                        intent(in   ):: dim
             INTEGER(INT32), dimension(dim), intent(inout):: A
             INTEGER,        dimension(dim), intent(inout):: P
      END SUBROUTINE
      !
      MODULE PURE SUBROUTINE SORT_2D_INDEX_INT32(A, SORT_INDEX, DESCEND, SORT_BY_ROW, P, NO_STABLE)
             INTEGER(INT32), dimension(:,:), contiguous,  intent(inout) :: A           !
             INTEGER,                                     intent(in   ) :: SORT_INDEX  ! Col or Row to sort, the rest are shifted based on sort. SORT_INDEX > 0 indicates column to sort on; SORT_INDEX < 0 indicates row to sort on; ABS(SORT_INDEX) is the row to sort on.
             LOGICAL,                           optional, intent(in   ) :: DESCEND     ! Sort Col/Row in desecending order, default is False
             LOGICAL,                           optional, intent(in   ) :: SORT_BY_ROW ! If present, overrides sign of SORT_INDEX and either sorts by row (True) or by column.
             INTEGER, dimension(:), contiguous, optional, intent(inout) :: P           ! Permulation vector that produced the sort of SORT_INDEX
             LOGICAL,                           optional, intent(in   ) :: NO_STABLE   ! Default is to do a stable sort (NO_STABLE=FALSE). NO_STABLE=TRUE disables this and uses the fastest sorting algorithm. *Note that if there are repeated values in the sorted col/row, then sort order of the other columns is random.
      END SUBROUTINE
      !
      MODULE PURE SUBROUTINE SORT_2D_MULTI_INT32_1DSC(A, SORT_INDICES, DESCEND, SORT_BY_ROW, P, NO_STABLE)    
             INTEGER(INT32), dimension(:,:), contiguous,  intent(inout) :: A
             INTEGER,        dimension(:),   contiguous,  intent(in   ) :: SORT_INDICES ! Columns or Rows to sort, the rest are shifted based on sort. SORT_INDICES(1) > 0 indicates column to sort on; SORT_INDICES(1) < 0 indicates row to sort on; ABS(SORT_INDICES) is the rows to sort on.
             LOGICAL,                                     intent(in   ) :: DESCEND      ! Sort Col/Row in desecending order, default is False
             LOGICAL,                           optional, intent(in   ) :: SORT_BY_ROW  ! If present, overrides sign of SORT_INDICES(1) and either sorts by row (True) or by column.
             INTEGER, dimension(:), contiguous, optional, intent(inout) :: P            ! Permulation vector that produced the sort of SORT_INDEX
             LOGICAL,                           optional, intent(in   ) :: NO_STABLE    ! Default is to do a stable sort (NO_STABLE=FALSE). NO_STABLE=TRUE disables this and uses the fastest sorting algorithm. *Note that if there are repeated values in the sorted col/row, then sort order of the other columns is random.
      END SUBROUTINE 
      !
      MODULE PURE SUBROUTINE SORT_2D_MULTI_INT32(A, SORT_INDICES, DESCEND, SORT_BY_ROW, P, NO_STABLE)    ! SORT_INDICES must contain unique values
             INTEGER(INT32), dimension(:,:), contiguous,  intent(inout) :: A
             INTEGER,        dimension(:),   contiguous,  intent(in   ) :: SORT_INDICES ! Columns or Rows to sort, the rest are shifted based on sort. SORT_INDICES(1) > 0 indicates column to sort on; SORT_INDICES(1) < 0 indicates row to sort on; ABS(SORT_INDICES) is the rows to sort on.
             LOGICAL, dimension(:), contiguous, optional, intent(in   ) :: DESCEND      ! Sort Col/Row in desecending order, default is False
             LOGICAL,                           optional, intent(in   ) :: SORT_BY_ROW  ! If present, overrides sign of SORT_INDICES(1) and either sorts by row (True) or by column.
             INTEGER, dimension(:), contiguous, optional, intent(inout) :: P            ! Permulation vector that produced the sort of SORT_INDEX
             LOGICAL,                           optional, intent(in   ) :: NO_STABLE    ! Default is to do a stable sort (NO_STABLE=FALSE). NO_STABLE=TRUE disables this and uses the fastest sorting algorithm. *Note that if there are repeated values in the sorted col/row, then sort order of the other columns is random.
      END SUBROUTINE 
      !
      MODULE PURE SUBROUTINE PERMUTATION_SORT_1D_INT32(A, P) 
             INTEGER(INT32), dimension(:), contiguous, intent(inout) :: A
             INTEGER,        dimension(:), contiguous, intent(inout) :: P
      END SUBROUTINE
      !
      MODULE PURE SUBROUTINE PERMUTATION_SORT_2D_INT32(A, P, BY_COLUMN) 
             INTEGER(INT32), dimension(:,:), contiguous, intent(inout) :: A
             INTEGER,        dimension(:),   contiguous, intent(inout) :: P
             LOGICAL,                          optional, intent(in   ) :: BY_COLUMN
      END SUBROUTINE
      !
      MODULE PURE SUBROUTINE REVERSE_ORDER_1D_DIM_INT32(dim, A, STABLE, P, INIT_P)
             INTEGER,                                 intent(in   ) :: dim
             INTEGER(INT32), dimension(dim),          intent(inout) :: A
             LOGICAL,                       optional, intent(in   ) :: STABLE
             INTEGER,       dimension(dim), optional, intent(inout) :: P
             LOGICAL,                       optional, intent(in   ) :: INIT_P
      END SUBROUTINE
      !
      MODULE PURE SUBROUTINE REVERSE_ORDER_1D_NODIM_INT32(A, STABLE, P, INIT_P)
             INTEGER(INT32),dimension(:), contiguous,           intent(inout) :: A
             LOGICAL,                                 optional, intent(in   ) :: STABLE
             INTEGER,       dimension(:), contiguous, optional, intent(inout) :: P
             LOGICAL,                                 optional, intent(in   ) :: INIT_P
      END SUBROUTINE
      !  
      MODULE PURE SUBROUTINE REVERSE_ORDER_2D_INT32(A, REVERSE_EACH_ROW)
             INTEGER(INT32), dimension(:,:), intent(inout):: A
             LOGICAL,              optional, intent(in   ):: REVERSE_EACH_ROW ! Default is TRUE
      END SUBROUTINE
      !
      MODULE PURE SUBROUTINE REVERSE_ORDER_2D_INDEX_INT32(A, REV_INDEX, STABLE, REVERSE_ROW, P)
             INTEGER(INT32), dimension(:,:), contiguous,           intent(inout) :: A
             INTEGER,                                              intent(in   ) :: REV_INDEX
             LOGICAL,                                    optional, intent(in   ) :: STABLE
             LOGICAL,                                    optional, intent(in   ) :: REVERSE_ROW
             INTEGER,        dimension(:),   contiguous, optional, intent(inout) :: P
      END SUBROUTINE
      !
      MODULE PURE SUBROUTINE SORT_B_by_A_and_P_INT32_INT32(dim, A, B, P, descend)
             INTEGER,                                  intent(in   ) :: dim
             INTEGER(INT32), dimension(dim),           intent(in   ) :: A       ! A and B must have the same dim
             INTEGER(INT32), dimension(dim),           intent(inout) :: B    
             INTEGER,        dimension(dim), optional, intent(inout) :: P       ! Applies permutations to sort B, then sorts B on repeated elements of A. P on exit reflect sort for both A and B
             LOGICAL,                        optional, intent(in   ) :: descend ! If present and true, then the sort of B along repeated A is in descending order.
      END SUBROUTINE
      !
      MODULE PURE SUBROUTINE SORT_B_by_A_and_P_INT64_INT32(dim, A, B, P, descend)
             INTEGER,                                  intent(in   ) :: dim
             INTEGER(INT64), dimension(dim),           intent(in   ) :: A       ! A and B must have the same dim
             INTEGER(INT32), dimension(dim),           intent(inout) :: B    
             INTEGER,        dimension(dim), optional, intent(inout) :: P       ! Applies permutations to sort B, then sorts B on repeated elements of A. P on exit reflect sort for both A and B
             LOGICAL,                        optional, intent(in   ) :: descend ! If present and true, then the sort of B along repeated A is in descending order.
      END SUBROUTINE
      !
      MODULE PURE SUBROUTINE SORT_B_by_A_and_P_REL32_INT32(dim, A, B, P, descend)
             INTEGER,                                  intent(in   ) :: dim
             REAL(REL32),    dimension(dim),           intent(in   ) :: A       ! A and B must have the same dim
             INTEGER(INT32), dimension(dim),           intent(inout) :: B    
             INTEGER,        dimension(dim), optional, intent(inout) :: P       ! Applies permutations to sort B, then sorts B on repeated elements of A. P on exit reflect sort for both A and B
             LOGICAL,                        optional, intent(in   ) :: descend ! If present and true, then the sort of B along repeated A is in descending order.
      END SUBROUTINE
      !
      MODULE PURE SUBROUTINE SORT_B_by_A_and_P_REL64_INT32(dim, A, B, P, descend)
             INTEGER,                                  intent(in   ) :: dim
             REAL(REL64),    dimension(dim),           intent(in   ) :: A       ! A and B must have the same dim
             INTEGER(INT32), dimension(dim),           intent(inout) :: B    
             INTEGER,        dimension(dim), optional, intent(inout) :: P       ! Applies permutations to sort B, then sorts B on repeated elements of A. P on exit reflect sort for both A and B
             LOGICAL,                        optional, intent(in   ) :: descend ! If present and true, then the sort of B along repeated A is in descending order.
      END SUBROUTINE
      !
      MODULE PURE SUBROUTINE SORT_B_by_A_and_P_CHAR_INT32(dim, A, B, P, descend)
             INTEGER,                                  intent(in   ) :: dim
             CHARACTER(*),   dimension(dim),           intent(in   ) :: A       ! A and B must have the same dim
             INTEGER(INT32), dimension(dim),           intent(inout) :: B    
             INTEGER,        dimension(dim), optional, intent(inout) :: P       ! Applies permutations to sort B, then sorts B on repeated elements of A. P on exit reflect sort for both A and B
             LOGICAL,                        optional, intent(in   ) :: descend ! If present and true, then the sort of B along repeated A is in descending order.
      END SUBROUTINE
      !
      MODULE PURE SUBROUTINE SETUP_REP_INT32(dim, A, REP)
             INTEGER,                        intent(in   ) :: dim
             INTEGER(INT32), dimension(dim), intent(in   ) :: A
             INTEGER,        dimension(dim), intent(inout) :: REP
      END SUBROUTINE
      !
      MODULE PURE SUBROUTINE SORT_B_by_A_and_P_REP_INT32(dim, REP, B, repeat_sort, P, descend)
             INTEGER,                                  intent(in   ) :: dim
             INTEGER,        dimension(dim),           intent(inout) :: REP
             INTEGER(INT32), dimension(dim),           intent(inout) :: B    
             LOGICAL,                                  intent(inout) :: repeat_sort ! On entry, indicates if B should be sorted on along repeated values of A. On exit indicates if repeated values were found in A
             INTEGER,        dimension(dim), optional, intent(inout) :: P       ! Applies permutations to sort B, then sorts B on repeated elements of A. P on exit reflect sort for both A and B
             LOGICAL,                        optional, intent(in   ) :: descend ! If present and true, then the sort of B along repeated REP is in descending order.
      END SUBROUTINE
      !
  END INTERFACE
  !
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !#####################################################################################################
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !
  INTERFACE ! SORT_INTERFACE_INT64
      !
      MODULE PURE SUBROUTINE ALLOCATE_PERM_INT64(A, P)    
             INTEGER(INT64), dimension(:),              intent(in   ):: A
             INTEGER,        dimension(:), allocatable, intent(inout):: P
      END SUBROUTINE
      !
      MODULE PURE FUNCTION SORT_TYPE_INT64(dim, A, DESCEND, STABLE, QSORT, HEAP, ISORT, SymMerge) RESULT(ANS)
        INTEGER,                        intent(in) :: dim
        INTEGER(INT64), dimension(dim), intent(in) :: A
        LOGICAL,              optional, intent(in) :: DESCEND, STABLE, QSORT, HEAP, ISORT, SymMerge
        INTEGER:: ANS
      END FUNCTION
      !
      MODULE PURE FUNCTION SORTED_1D_INT64(A, DESCEND, STABLE) RESULT(SORTED_ARRAY)
             INTEGER(INT64), dimension(:), contiguous,           intent(in):: A
             LOGICAL,                                  optional, intent(in):: DESCEND, STABLE
             INTEGER(INT64), dimension(SIZE(A)):: SORTED_ARRAY
      END FUNCTION
      !
      MODULE PURE SUBROUTINE SORT_1D_INT64(A, DESCEND, STABLE, P, SORT_TYPE, QSORT, HEAP, ISORT, SymMerge , INIT_P)
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
      END SUBROUTINE
      !
      MODULE PURE SUBROUTINE SORT_1D_QSORT_A_INT64(DIM, A)
             INTEGER,                        intent(in   ):: DIM
             INTEGER(INT64), dimension(DIM), intent(inout):: A
      END SUBROUTINE
      !
      MODULE PURE SUBROUTINE SORT_1D_SymMg_P_INT64(dim, A, P)
             INTEGER,                        intent(in   ):: dim
             INTEGER(INT64), dimension(dim), intent(inout):: A
             INTEGER,        dimension(dim), intent(inout):: P
      END SUBROUTINE
      !
      MODULE PURE SUBROUTINE SORT_2D_INDEX_INT64(A, SORT_INDEX, DESCEND, SORT_BY_ROW, P, NO_STABLE)
             INTEGER(INT64), dimension(:,:), contiguous,  intent(inout) :: A           !
             INTEGER,                                     intent(in   ) :: SORT_INDEX  ! Col or Row to sort, the rest are shifted based on sort. SORT_INDEX > 0 indicates column to sort on; SORT_INDEX < 0 indicates row to sort on; ABS(SORT_INDEX) is the row to sort on.
             LOGICAL,                           optional, intent(in   ) :: DESCEND     ! Sort Col/Row in desecending order, default is False
             LOGICAL,                           optional, intent(in   ) :: SORT_BY_ROW ! If present, overrides sign of SORT_INDEX and either sorts by row (True) or by column.
             INTEGER, dimension(:), contiguous, optional, intent(inout) :: P           ! Permulation vector that produced the sort of SORT_INDEX
             LOGICAL,                           optional, intent(in   ) :: NO_STABLE   ! Default is to do a stable sort (NO_STABLE=FALSE). NO_STABLE=TRUE disables this and uses the fastest sorting algorithm. *Note that if there are repeated values in the sorted col/row, then sort order of the other columns is random.
      END SUBROUTINE
      !
      MODULE PURE SUBROUTINE SORT_2D_MULTI_INT64_1DSC(A, SORT_INDICES, DESCEND, SORT_BY_ROW, P, NO_STABLE)    
             INTEGER(INT64), dimension(:,:), contiguous,  intent(inout) :: A
             INTEGER,        dimension(:),   contiguous,  intent(in   ) :: SORT_INDICES ! Columns or Rows to sort, the rest are shifted based on sort. SORT_INDICES(1) > 0 indicates column to sort on; SORT_INDICES(1) < 0 indicates row to sort on; ABS(SORT_INDICES) is the rows to sort on.
             LOGICAL,                                     intent(in   ) :: DESCEND      ! Sort Col/Row in desecending order, default is False
             LOGICAL,                           optional, intent(in   ) :: SORT_BY_ROW  ! If present, overrides sign of SORT_INDICES(1) and either sorts by row (True) or by column.
             INTEGER, dimension(:), contiguous, optional, intent(inout) :: P            ! Permulation vector that produced the sort of SORT_INDEX
             LOGICAL,                           optional, intent(in   ) :: NO_STABLE    ! Default is to do a stable sort (NO_STABLE=FALSE). NO_STABLE=TRUE disables this and uses the fastest sorting algorithm. *Note that if there are repeated values in the sorted col/row, then sort order of the other columns is random.
      END SUBROUTINE 
      !
      MODULE PURE SUBROUTINE SORT_2D_MULTI_INT64(A, SORT_INDICES, DESCEND, SORT_BY_ROW, P, NO_STABLE)    ! SORT_INDICES must contain unique values
             INTEGER(INT64), dimension(:,:), contiguous,  intent(inout) :: A
             INTEGER,        dimension(:),   contiguous,  intent(in   ) :: SORT_INDICES ! Columns or Rows to sort, the rest are shifted based on sort. SORT_INDICES(1) > 0 indicates column to sort on; SORT_INDICES(1) < 0 indicates row to sort on; ABS(SORT_INDICES) is the rows to sort on.
             LOGICAL, dimension(:), contiguous, optional, intent(in   ) :: DESCEND      ! Sort Col/Row in desecending order, default is False
             LOGICAL,                           optional, intent(in   ) :: SORT_BY_ROW  ! If present, overrides sign of SORT_INDICES(1) and either sorts by row (True) or by column.
             INTEGER, dimension(:), contiguous, optional, intent(inout) :: P            ! Permulation vector that produced the sort of SORT_INDEX
             LOGICAL,                           optional, intent(in   ) :: NO_STABLE    ! Default is to do a stable sort (NO_STABLE=FALSE). NO_STABLE=TRUE disables this and uses the fastest sorting algorithm. *Note that if there are repeated values in the sorted col/row, then sort order of the other columns is random.
      END SUBROUTINE 
      !
      MODULE PURE SUBROUTINE PERMUTATION_SORT_1D_INT64(A, P) 
             INTEGER(INT64), dimension(:), contiguous, intent(inout) :: A
             INTEGER,        dimension(:), contiguous, intent(inout) :: P
      END SUBROUTINE
      !
      MODULE PURE SUBROUTINE PERMUTATION_SORT_2D_INT64(A, P, BY_COLUMN) 
             INTEGER(INT64), dimension(:,:), contiguous, intent(inout) :: A
             INTEGER,        dimension(:),   contiguous, intent(inout) :: P
             LOGICAL,                          optional, intent(in   ) :: BY_COLUMN
      END SUBROUTINE
      !
      MODULE PURE SUBROUTINE REVERSE_ORDER_1D_DIM_INT64(dim, A, STABLE, P, INIT_P)
             INTEGER,                                 intent(in   ) :: dim
             INTEGER(INT64), dimension(dim),          intent(inout) :: A
             LOGICAL,                       optional, intent(in   ) :: STABLE
             INTEGER,       dimension(dim), optional, intent(inout) :: P
             LOGICAL,                       optional, intent(in   ) :: INIT_P
      END SUBROUTINE
      !
      MODULE PURE SUBROUTINE REVERSE_ORDER_1D_NODIM_INT64(A, STABLE, P, INIT_P)
             INTEGER(INT64),dimension(:), contiguous,           intent(inout) :: A
             LOGICAL,                                 optional, intent(in   ) :: STABLE
             INTEGER,       dimension(:), contiguous, optional, intent(inout) :: P
             LOGICAL,                                 optional, intent(in   ) :: INIT_P
      END SUBROUTINE
      !  
      MODULE PURE SUBROUTINE REVERSE_ORDER_2D_INT64(A, REVERSE_EACH_ROW)
             INTEGER(INT64), dimension(:,:), intent(inout):: A
             LOGICAL,              optional, intent(in   ):: REVERSE_EACH_ROW ! Default is TRUE
      END SUBROUTINE
      !
      MODULE PURE SUBROUTINE REVERSE_ORDER_2D_INDEX_INT64(A, REV_INDEX, STABLE, REVERSE_ROW, P)
             INTEGER(INT64), dimension(:,:), contiguous,           intent(inout) :: A
             INTEGER,                                              intent(in   ) :: REV_INDEX
             LOGICAL,                                    optional, intent(in   ) :: STABLE
             LOGICAL,                                    optional, intent(in   ) :: REVERSE_ROW
             INTEGER,        dimension(:),   contiguous, optional, intent(inout) :: P
      END SUBROUTINE
      !
      MODULE PURE SUBROUTINE SORT_B_by_A_and_P_INT32_INT64(dim, A, B, P, descend)
             INTEGER,                                  intent(in   ) :: dim
             INTEGER(INT32), dimension(dim),           intent(in   ) :: A       ! A and B must have the same dim
             INTEGER(INT64), dimension(dim),           intent(inout) :: B    
             INTEGER,        dimension(dim), optional, intent(inout) :: P       ! Applies permutations to sort B, then sorts B on repeated elements of A. P on exit reflect sort for both A and B
             LOGICAL,                        optional, intent(in   ) :: descend ! If present and true, then the sort of B along repeated A is in descending order.
      END SUBROUTINE
      !
      MODULE PURE SUBROUTINE SORT_B_by_A_and_P_INT64_INT64(dim, A, B, P, descend)
             INTEGER,                                  intent(in   ) :: dim
             INTEGER(INT64), dimension(dim),           intent(in   ) :: A       ! A and B must have the same dim
             INTEGER(INT64), dimension(dim),           intent(inout) :: B    
             INTEGER,        dimension(dim), optional, intent(inout) :: P       ! Applies permutations to sort B, then sorts B on repeated elements of A. P on exit reflect sort for both A and B
             LOGICAL,                        optional, intent(in   ) :: descend ! If present and true, then the sort of B along repeated A is in descending order.
      END SUBROUTINE
      !
      MODULE PURE SUBROUTINE SORT_B_by_A_and_P_REL32_INT64(dim, A, B, P, descend)
             INTEGER,                                  intent(in   ) :: dim
             REAL(REL32),    dimension(dim),           intent(in   ) :: A       ! A and B must have the same dim
             INTEGER(INT64), dimension(dim),           intent(inout) :: B    
             INTEGER,        dimension(dim), optional, intent(inout) :: P       ! Applies permutations to sort B, then sorts B on repeated elements of A. P on exit reflect sort for both A and B
             LOGICAL,                        optional, intent(in   ) :: descend ! If present and true, then the sort of B along repeated A is in descending order.
      END SUBROUTINE
      !
      MODULE PURE SUBROUTINE SORT_B_by_A_and_P_REL64_INT64(dim, A, B, P, descend)
             INTEGER,                                  intent(in   ) :: dim
             REAL(REL64),    dimension(dim),           intent(in   ) :: A       ! A and B must have the same dim
             INTEGER(INT64), dimension(dim),           intent(inout) :: B    
             INTEGER,        dimension(dim), optional, intent(inout) :: P       ! Applies permutations to sort B, then sorts B on repeated elements of A. P on exit reflect sort for both A and B
             LOGICAL,                        optional, intent(in   ) :: descend ! If present and true, then the sort of B along repeated A is in descending order.
      END SUBROUTINE
      !
      MODULE PURE SUBROUTINE SORT_B_by_A_and_P_CHAR_INT64(dim, A, B, P, descend)
             INTEGER,                                  intent(in   ) :: dim
             CHARACTER(*),   dimension(dim),           intent(in   ) :: A       ! A and B must have the same dim
             INTEGER(INT64), dimension(dim),           intent(inout) :: B    
             INTEGER,        dimension(dim), optional, intent(inout) :: P       ! Applies permutations to sort B, then sorts B on repeated elements of A. P on exit reflect sort for both A and B
             LOGICAL,                        optional, intent(in   ) :: descend ! If present and true, then the sort of B along repeated A is in descending order.
      END SUBROUTINE
      !
      MODULE PURE SUBROUTINE SETUP_REP_INT64(dim, A, REP)
             INTEGER,                        intent(in   ) :: dim
             INTEGER(INT64), dimension(dim), intent(in   ) :: A
             INTEGER,        dimension(dim), intent(inout) :: REP
      END SUBROUTINE
      !
      MODULE PURE SUBROUTINE SORT_B_by_A_and_P_REP_INT64(dim, REP, B, repeat_sort, P, descend)
             INTEGER,                                  intent(in   ) :: dim
             INTEGER,        dimension(dim),           intent(inout) :: REP
             INTEGER(INT64), dimension(dim),           intent(inout) :: B    
             LOGICAL,                                  intent(inout) :: repeat_sort ! On entry, indicates if B should be sorted on along repeated values of A. On exit indicates if repeated values were found in A
             INTEGER,        dimension(dim), optional, intent(inout) :: P       ! Applies permutations to sort B, then sorts B on repeated elements of A. P on exit reflect sort for both A and B
             LOGICAL,                        optional, intent(in   ) :: descend ! If present and true, then the sort of B along repeated REP is in descending order.
      END SUBROUTINE
      !
  END INTERFACE
  !
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !#####################################################################################################
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !
  INTERFACE ! SORT_INTERFACE_REL32
      !
      MODULE PURE SUBROUTINE ALLOCATE_PERM_REL32(A, P)    
             REAL(REL32), dimension(:),              intent(in   ):: A
             INTEGER,     dimension(:), allocatable, intent(inout):: P
      END SUBROUTINE
      !
      MODULE PURE FUNCTION SORT_TYPE_REL32(dim, A, DESCEND, STABLE, QSORT, HEAP, ISORT, SymMerge) RESULT(ANS)
             INTEGER,                     intent(in) :: dim
             REAL(REL32), dimension(dim), intent(in) :: A
             LOGICAL,           optional, intent(in) :: DESCEND, STABLE, QSORT, HEAP, ISORT, SymMerge
             INTEGER:: ANS
      END FUNCTION
      !
      MODULE PURE FUNCTION SORTED_1D_REL32(A, DESCEND, STABLE) RESULT(SORTED_ARRAY)
             REAL(REL32), dimension(:), contiguous, intent(in):: A
             LOGICAL,                     optional, intent(in):: DESCEND, STABLE
             REAL(REL32), dimension(SIZE(A)):: SORTED_ARRAY
      END FUNCTION
      !
      MODULE PURE SUBROUTINE SORT_1D_REL32(A, DESCEND, STABLE, P, SORT_TYPE, QSORT, HEAP, ISORT, SymMerge , INIT_P)
             REAL(REL32), dimension(:), contiguous,       intent(inout) :: A
             LOGICAL,                           optional, intent(in   ) :: DESCEND   ! Sort in Decending Order
             LOGICAL,                           optional, intent(in   ) :: STABLE    ! Use only Stable Sorting Methods
             INTEGER, dimension(:), contiguous, optional, intent(inout) :: P         ! Contains permutations to sort A (Sorts along with A)
             INTEGER,                           optional, intent(in   ) :: SORT_TYPE ! If present, then disables call to SORT_TYPE sets the sort type to the optiona defined. It must be a valid sort type option integer.
             LOGICAL,                           optional, intent(in   ) :: QSORT     ! Use only Dual Pivot Quicksort/IntroSort
             LOGICAL,                           optional, intent(in   ) :: HEAP      ! Use only Heap Sort
             LOGICAL,                           optional, intent(in   ) :: ISORT     ! Use only Insertion Sort
             LOGICAL,                           optional, intent(in   ) :: SymMerge  ! Use only Stable Minimum Storage Merging by Symmetric Comparisons
             LOGICAL,                           optional, intent(in   ) :: INIT_P    ! Set P = [1...to...DIM]; Default is True; which makes P a permutation vector for how A is sorted
      END SUBROUTINE
      !
      MODULE PURE SUBROUTINE SORT_1D_QSORT_A_REL32(DIM, A)
             INTEGER,                     intent(in   ):: DIM
             REAL(REL32), dimension(DIM), intent(inout):: A
      END SUBROUTINE
      !
      MODULE PURE SUBROUTINE SORT_1D_SymMg_P_REL32(dim, A, P)
             INTEGER,                     intent(in   ):: dim
             REAL(REL32), dimension(dim), intent(inout):: A
             INTEGER,     dimension(dim), intent(inout):: P
      END SUBROUTINE
      !
      MODULE PURE SUBROUTINE SORT_2D_INDEX_REL32(A, SORT_INDEX, DESCEND, SORT_BY_ROW, P, NO_STABLE)
             REAL(REL32), dimension(:,:), contiguous,  intent(inout) :: A           !
             INTEGER,                                     intent(in   ) :: SORT_INDEX  ! Col or Row to sort, the rest are shifted based on sort. SORT_INDEX > 0 indicates column to sort on; SORT_INDEX < 0 indicates row to sort on; ABS(SORT_INDEX) is the row to sort on.
             LOGICAL,                           optional, intent(in   ) :: DESCEND     ! Sort Col/Row in desecending order, default is False
             LOGICAL,                           optional, intent(in   ) :: SORT_BY_ROW ! If present, overrides sign of SORT_INDEX and either sorts by row (True) or by column.
             INTEGER, dimension(:), contiguous, optional, intent(inout) :: P           ! Permulation vector that produced the sort of SORT_INDEX
             LOGICAL,                           optional, intent(in   ) :: NO_STABLE   ! Default is to do a stable sort (NO_STABLE=FALSE). NO_STABLE=TRUE disables this and uses the fastest sorting algorithm. *Note that if there are repeated values in the sorted col/row, then sort order of the other columns is random.
      END SUBROUTINE
      !
      MODULE PURE SUBROUTINE SORT_2D_MULTI_REL32_1DSC(A, SORT_INDICES, DESCEND, SORT_BY_ROW, P, NO_STABLE)    
             REAL(REL32), dimension(:,:), contiguous,     intent(inout) :: A
             INTEGER,     dimension(:),   contiguous,     intent(in   ) :: SORT_INDICES ! Columns or Rows to sort, the rest are shifted based on sort. SORT_INDICES(1) > 0 indicates column to sort on; SORT_INDICES(1) < 0 indicates row to sort on; ABS(SORT_INDICES) is the rows to sort on.
             LOGICAL,                                     intent(in   ) :: DESCEND      ! Sort Col/Row in desecending order, default is False
             LOGICAL,                           optional, intent(in   ) :: SORT_BY_ROW  ! If present, overrides sign of SORT_INDICES(1) and either sorts by row (True) or by column.
             INTEGER, dimension(:), contiguous, optional, intent(inout) :: P            ! Permulation vector that produced the sort of SORT_INDEX
             LOGICAL,                           optional, intent(in   ) :: NO_STABLE    ! Default is to do a stable sort (NO_STABLE=FALSE). NO_STABLE=TRUE disables this and uses the fastest sorting algorithm. *Note that if there are repeated values in the sorted col/row, then sort order of the other columns is random.
      END SUBROUTINE 
      !
      MODULE PURE SUBROUTINE SORT_2D_MULTI_REL32(A, SORT_INDICES, DESCEND, SORT_BY_ROW, P, NO_STABLE)    ! SORT_INDICES must contain unique values
             REAL(REL32), dimension(:,:), contiguous,     intent(inout) :: A
             INTEGER,     dimension(:),   contiguous,     intent(in   ) :: SORT_INDICES ! Columns or Rows to sort, the rest are shifted based on sort. SORT_INDICES(1) > 0 indicates column to sort on; SORT_INDICES(1) < 0 indicates row to sort on; ABS(SORT_INDICES) is the rows to sort on.
             LOGICAL, dimension(:), contiguous, optional, intent(in   ) :: DESCEND      ! Sort Col/Row in desecending order, default is False
             LOGICAL,                           optional, intent(in   ) :: SORT_BY_ROW  ! If present, overrides sign of SORT_INDICES(1) and either sorts by row (True) or by column.
             INTEGER, dimension(:), contiguous, optional, intent(inout) :: P            ! Permulation vector that produced the sort of SORT_INDEX
             LOGICAL,                           optional, intent(in   ) :: NO_STABLE    ! Default is to do a stable sort (NO_STABLE=FALSE). NO_STABLE=TRUE disables this and uses the fastest sorting algorithm. *Note that if there are repeated values in the sorted col/row, then sort order of the other columns is random.
      END SUBROUTINE 
      !
      MODULE PURE SUBROUTINE PERMUTATION_SORT_1D_REL32(A, P) 
             REAL(REL32), dimension(:), contiguous, intent(inout) :: A
             INTEGER,     dimension(:), contiguous, intent(inout) :: P
      END SUBROUTINE
      !
      MODULE PURE SUBROUTINE PERMUTATION_SORT_2D_REL32(A, P, BY_COLUMN) 
             REAL(REL32), dimension(:,:), contiguous, intent(inout) :: A
             INTEGER,        dimension(:),   contiguous, intent(inout) :: P
             LOGICAL,                          optional, intent(in   ) :: BY_COLUMN
      END SUBROUTINE
      !
      MODULE PURE SUBROUTINE REVERSE_ORDER_1D_DIM_REL32(dim, A, STABLE, P, INIT_P)
             INTEGER,                               intent(in   ) :: dim
             REAL(REL32), dimension(dim),           intent(inout) :: A
             LOGICAL,                     optional, intent(in   ) :: STABLE
             INTEGER,     dimension(dim), optional, intent(inout) :: P
             LOGICAL,                     optional, intent(in   ) :: INIT_P
      END SUBROUTINE
      !
      MODULE PURE SUBROUTINE REVERSE_ORDER_1D_NODIM_REL32(A, STABLE, P, INIT_P)
             REAL(REL32), dimension(:), contiguous,           intent(inout) :: A
             LOGICAL,                               optional, intent(in   ) :: STABLE
             INTEGER,     dimension(:), contiguous, optional, intent(inout) :: P
             LOGICAL,                               optional, intent(in   ) :: INIT_P
      END SUBROUTINE
      !  
      MODULE PURE SUBROUTINE REVERSE_ORDER_2D_REL32(A, REVERSE_EACH_ROW)
             REAL(REL32), dimension(:,:), intent(inout):: A
             LOGICAL,           optional, intent(in   ):: REVERSE_EACH_ROW ! Default is TRUE
      END SUBROUTINE
      !
      MODULE PURE SUBROUTINE REVERSE_ORDER_2D_INDEX_REL32(A, REV_INDEX, STABLE, REVERSE_ROW, P)
             REAL(REL32), dimension(:,:), contiguous,           intent(inout) :: A
             INTEGER,                                           intent(in   ) :: REV_INDEX
             LOGICAL,                                 optional, intent(in   ) :: STABLE
             LOGICAL,                                 optional, intent(in   ) :: REVERSE_ROW
             INTEGER,     dimension(:),   contiguous, optional, intent(inout) :: P
      END SUBROUTINE
      !
      MODULE PURE SUBROUTINE SORT_B_by_A_and_P_INT32_REL32(dim, A, B, P, descend)
             INTEGER,                                  intent(in   ) :: dim
             INTEGER(INT32), dimension(dim),           intent(in   ) :: A       ! A and B must have the same dim
             REAL(REL32),    dimension(dim),           intent(inout) :: B    
             INTEGER,        dimension(dim), optional, intent(inout) :: P       ! Applies permutations to sort B, then sorts B on repeated elements of A. P on exit reflect sort for both A and B
             LOGICAL,                        optional, intent(in   ) :: descend ! If present and true, then the sort of B along repeated A is in descending order.
      END SUBROUTINE
      !
      MODULE PURE SUBROUTINE SORT_B_by_A_and_P_INT64_REL32(dim, A, B, P, descend)
             INTEGER,                                  intent(in   ) :: dim
             INTEGER(INT64), dimension(dim),           intent(in   ) :: A       ! A and B must have the same dim
             REAL(REL32),    dimension(dim),           intent(inout) :: B    
             INTEGER,        dimension(dim), optional, intent(inout) :: P       ! Applies permutations to sort B, then sorts B on repeated elements of A. P on exit reflect sort for both A and B
             LOGICAL,                        optional, intent(in   ) :: descend ! If present and true, then the sort of B along repeated A is in descending order.
      END SUBROUTINE
      !
      MODULE PURE SUBROUTINE SORT_B_by_A_and_P_REL32_REL32(dim, A, B, P, descend)
             INTEGER,                                  intent(in   ) :: dim
             REAL(REL32),    dimension(dim),           intent(in   ) :: A       ! A and B must have the same dim
             REAL(REL32),    dimension(dim),           intent(inout) :: B    
             INTEGER,        dimension(dim), optional, intent(inout) :: P       ! Applies permutations to sort B, then sorts B on repeated elements of A. P on exit reflect sort for both A and B
             LOGICAL,                        optional, intent(in   ) :: descend ! If present and true, then the sort of B along repeated A is in descending order.
      END SUBROUTINE
      !
      MODULE PURE SUBROUTINE SORT_B_by_A_and_P_REL64_REL32(dim, A, B, P, descend)
             INTEGER,                                  intent(in   ) :: dim
             REAL(REL64),    dimension(dim),           intent(in   ) :: A       ! A and B must have the same dim
             REAL(REL32),    dimension(dim),           intent(inout) :: B    
             INTEGER,        dimension(dim), optional, intent(inout) :: P       ! Applies permutations to sort B, then sorts B on repeated elements of A. P on exit reflect sort for both A and B
             LOGICAL,                        optional, intent(in   ) :: descend ! If present and true, then the sort of B along repeated A is in descending order.
      END SUBROUTINE
      !
      MODULE PURE SUBROUTINE SORT_B_by_A_and_P_CHAR_REL32(dim, A, B, P, descend)
             INTEGER,                                  intent(in   ) :: dim
             CHARACTER(*),   dimension(dim),           intent(in   ) :: A       ! A and B must have the same dim
             REAL(REL32),    dimension(dim),           intent(inout) :: B    
             INTEGER,        dimension(dim), optional, intent(inout) :: P       ! Applies permutations to sort B, then sorts B on repeated elements of A. P on exit reflect sort for both A and B
             LOGICAL,                        optional, intent(in   ) :: descend ! If present and true, then the sort of B along repeated A is in descending order.
      END SUBROUTINE
      !
      MODULE PURE SUBROUTINE SETUP_REP_REL32(dim, A, REP)
             INTEGER,                        intent(in   ) :: dim
             REAL(REL32),    dimension(dim), intent(in   ) :: A
             INTEGER,        dimension(dim), intent(inout) :: REP
      END SUBROUTINE
      !
      MODULE PURE SUBROUTINE SORT_B_by_A_and_P_REP_REL32(dim, REP, B, repeat_sort, P, descend)
             INTEGER,                                  intent(in   ) :: dim
             INTEGER,        dimension(dim),           intent(inout) :: REP
             REAL(REL32),    dimension(dim),           intent(inout) :: B    
             LOGICAL,                                  intent(inout) :: repeat_sort ! On entry, indicates if B should be sorted on along repeated values of A. On exit indicates if repeated values were found in A
             INTEGER,        dimension(dim), optional, intent(inout) :: P       ! Applies permutations to sort B, then sorts B on repeated elements of A. P on exit reflect sort for both A and B
             LOGICAL,                        optional, intent(in   ) :: descend ! If present and true, then the sort of B along repeated REP is in descending order.
      END SUBROUTINE
      !
  END INTERFACE
  !
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !#####################################################################################################
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !
  INTERFACE ! SORT_INTERFACE_REL64
      !
      MODULE PURE SUBROUTINE ALLOCATE_PERM_REL64(A, P)    
             REAL(REL64), dimension(:),              intent(in   ):: A
             INTEGER,     dimension(:), allocatable, intent(inout):: P
      END SUBROUTINE
      !
      MODULE PURE FUNCTION SORT_TYPE_REL64(dim, A, DESCEND, STABLE, QSORT, HEAP, ISORT, SymMerge) RESULT(ANS)
             INTEGER,                     intent(in) :: dim
             REAL(REL64), dimension(dim), intent(in) :: A
             LOGICAL,           optional, intent(in) :: DESCEND, STABLE, QSORT, HEAP, ISORT, SymMerge
             INTEGER:: ANS
      END FUNCTION
      !
      MODULE PURE FUNCTION SORTED_1D_REL64(A, DESCEND, STABLE) RESULT(SORTED_ARRAY)
             REAL(REL64), dimension(:), contiguous, intent(in):: A
             LOGICAL,                     optional, intent(in):: DESCEND, STABLE
             REAL(REL64), dimension(SIZE(A)):: SORTED_ARRAY
      END FUNCTION
      !
      MODULE PURE SUBROUTINE SORT_1D_REL64(A, DESCEND, STABLE, P, SORT_TYPE, QSORT, HEAP, ISORT, SymMerge , INIT_P)
             REAL(REL64), dimension(:), contiguous,       intent(inout) :: A
             LOGICAL,                           optional, intent(in   ) :: DESCEND   ! Sort in Decending Order
             LOGICAL,                           optional, intent(in   ) :: STABLE    ! Use only Stable Sorting Methods
             INTEGER, dimension(:), contiguous, optional, intent(inout) :: P         ! Contains permutations to sort A (Sorts along with A)
             INTEGER,                           optional, intent(in   ) :: SORT_TYPE ! If present, then disables call to SORT_TYPE sets the sort type to the optiona defined. It must be a valid sort type option integer.
             LOGICAL,                           optional, intent(in   ) :: QSORT     ! Use only Dual Pivot Quicksort/IntroSort
             LOGICAL,                           optional, intent(in   ) :: HEAP      ! Use only Heap Sort
             LOGICAL,                           optional, intent(in   ) :: ISORT     ! Use only Insertion Sort
             LOGICAL,                           optional, intent(in   ) :: SymMerge  ! Use only Stable Minimum Storage Merging by Symmetric Comparisons
             LOGICAL,                           optional, intent(in   ) :: INIT_P    ! Set P = [1...to...DIM]; Default is True; which makes P a permutation vector for how A is sorted
      END SUBROUTINE
      !
      MODULE PURE SUBROUTINE SORT_1D_QSORT_A_REL64(DIM, A)
             INTEGER,                     intent(in   ):: DIM
             REAL(REL64), dimension(DIM), intent(inout):: A
      END SUBROUTINE
      !
      MODULE PURE SUBROUTINE SORT_1D_SymMg_P_REL64(dim, A, P)
             INTEGER,                     intent(in   ):: dim
             REAL(REL64), dimension(dim), intent(inout):: A
             INTEGER,     dimension(dim), intent(inout):: P
      END SUBROUTINE
      !
      MODULE PURE SUBROUTINE SORT_2D_INDEX_REL64(A, SORT_INDEX, DESCEND, SORT_BY_ROW, P, NO_STABLE)
             REAL(REL64), dimension(:,:), contiguous,  intent(inout) :: A           !
             INTEGER,                                     intent(in   ) :: SORT_INDEX  ! Col or Row to sort, the rest are shifted based on sort. SORT_INDEX > 0 indicates column to sort on; SORT_INDEX < 0 indicates row to sort on; ABS(SORT_INDEX) is the row to sort on.
             LOGICAL,                           optional, intent(in   ) :: DESCEND     ! Sort Col/Row in desecending order, default is False
             LOGICAL,                           optional, intent(in   ) :: SORT_BY_ROW ! If present, overrides sign of SORT_INDEX and either sorts by row (True) or by column.
             INTEGER, dimension(:), contiguous, optional, intent(inout) :: P           ! Permulation vector that produced the sort of SORT_INDEX
             LOGICAL,                           optional, intent(in   ) :: NO_STABLE   ! Default is to do a stable sort (NO_STABLE=FALSE). NO_STABLE=TRUE disables this and uses the fastest sorting algorithm. *Note that if there are repeated values in the sorted col/row, then sort order of the other columns is random.
      END SUBROUTINE
      !
      MODULE PURE SUBROUTINE SORT_2D_MULTI_REL64_1DSC(A, SORT_INDICES, DESCEND, SORT_BY_ROW, P, NO_STABLE)    
             REAL(REL64), dimension(:,:), contiguous,     intent(inout) :: A
             INTEGER,     dimension(:),   contiguous,     intent(in   ) :: SORT_INDICES ! Columns or Rows to sort, the rest are shifted based on sort. SORT_INDICES(1) > 0 indicates column to sort on; SORT_INDICES(1) < 0 indicates row to sort on; ABS(SORT_INDICES) is the rows to sort on.
             LOGICAL,                                     intent(in   ) :: DESCEND      ! Sort Col/Row in desecending order, default is False
             LOGICAL,                           optional, intent(in   ) :: SORT_BY_ROW  ! If present, overrides sign of SORT_INDICES(1) and either sorts by row (True) or by column.
             INTEGER, dimension(:), contiguous, optional, intent(inout) :: P            ! Permulation vector that produced the sort of SORT_INDEX
             LOGICAL,                           optional, intent(in   ) :: NO_STABLE    ! Default is to do a stable sort (NO_STABLE=FALSE). NO_STABLE=TRUE disables this and uses the fastest sorting algorithm. *Note that if there are repeated values in the sorted col/row, then sort order of the other columns is random.
      END SUBROUTINE 
      !
      MODULE PURE SUBROUTINE SORT_2D_MULTI_REL64(A, SORT_INDICES, DESCEND, SORT_BY_ROW, P, NO_STABLE)    ! SORT_INDICES must contain unique values
             REAL(REL64), dimension(:,:), contiguous,     intent(inout) :: A
             INTEGER,     dimension(:),   contiguous,     intent(in   ) :: SORT_INDICES ! Columns or Rows to sort, the rest are shifted based on sort. SORT_INDICES(1) > 0 indicates column to sort on; SORT_INDICES(1) < 0 indicates row to sort on; ABS(SORT_INDICES) is the rows to sort on.
             LOGICAL, dimension(:), contiguous, optional, intent(in   ) :: DESCEND      ! Sort Col/Row in desecending order, default is False
             LOGICAL,                           optional, intent(in   ) :: SORT_BY_ROW  ! If present, overrides sign of SORT_INDICES(1) and either sorts by row (True) or by column.
             INTEGER, dimension(:), contiguous, optional, intent(inout) :: P            ! Permulation vector that produced the sort of SORT_INDEX
             LOGICAL,                           optional, intent(in   ) :: NO_STABLE    ! Default is to do a stable sort (NO_STABLE=FALSE). NO_STABLE=TRUE disables this and uses the fastest sorting algorithm. *Note that if there are repeated values in the sorted col/row, then sort order of the other columns is random.
      END SUBROUTINE 
      !
      MODULE PURE SUBROUTINE PERMUTATION_SORT_1D_REL64(A, P) 
             REAL(REL64), dimension(:), contiguous, intent(inout) :: A
             INTEGER,     dimension(:), contiguous, intent(inout) :: P
      END SUBROUTINE
      !
      MODULE PURE SUBROUTINE PERMUTATION_SORT_2D_REL64(A, P, BY_COLUMN) 
             REAL(REL64), dimension(:,:), contiguous, intent(inout) :: A
             INTEGER,        dimension(:),   contiguous, intent(inout) :: P
             LOGICAL,                          optional, intent(in   ) :: BY_COLUMN
      END SUBROUTINE
      !
      MODULE PURE SUBROUTINE REVERSE_ORDER_1D_DIM_REL64(dim, A, STABLE, P, INIT_P)
             INTEGER,                               intent(in   ) :: dim
             REAL(REL64), dimension(dim),           intent(inout) :: A
             LOGICAL,                     optional, intent(in   ) :: STABLE
             INTEGER,     dimension(dim), optional, intent(inout) :: P
             LOGICAL,                     optional, intent(in   ) :: INIT_P
      END SUBROUTINE
      !
      MODULE PURE SUBROUTINE REVERSE_ORDER_1D_NODIM_REL64(A, STABLE, P, INIT_P)
             REAL(REL64), dimension(:), contiguous,           intent(inout) :: A
             LOGICAL,                               optional, intent(in   ) :: STABLE
             INTEGER,     dimension(:), contiguous, optional, intent(inout) :: P
             LOGICAL,                               optional, intent(in   ) :: INIT_P
      END SUBROUTINE
      !  
      MODULE PURE SUBROUTINE REVERSE_ORDER_2D_REL64(A, REVERSE_EACH_ROW)
             REAL(REL64), dimension(:,:), intent(inout):: A
             LOGICAL,           optional, intent(in   ):: REVERSE_EACH_ROW ! Default is TRUE
      END SUBROUTINE
      !
      MODULE PURE SUBROUTINE REVERSE_ORDER_2D_INDEX_REL64(A, REV_INDEX, STABLE, REVERSE_ROW, P)
             REAL(REL64), dimension(:,:), contiguous,           intent(inout) :: A
             INTEGER,                                           intent(in   ) :: REV_INDEX
             LOGICAL,                                 optional, intent(in   ) :: STABLE
             LOGICAL,                                 optional, intent(in   ) :: REVERSE_ROW
             INTEGER,     dimension(:),   contiguous, optional, intent(inout) :: P
      END SUBROUTINE
      !
      MODULE PURE SUBROUTINE SORT_B_by_A_and_P_INT32_REL64(dim, A, B, P, descend)
             INTEGER,                                  intent(in   ) :: dim
             INTEGER(INT32), dimension(dim),           intent(in   ) :: A       ! A and B must have the same dim
             REAL(REL64),    dimension(dim),           intent(inout) :: B    
             INTEGER,        dimension(dim), optional, intent(inout) :: P       ! Applies permutations to sort B, then sorts B on repeated elements of A. P on exit reflect sort for both A and B
             LOGICAL,                        optional, intent(in   ) :: descend ! If present and true, then the sort of B along repeated A is in descending order.
      END SUBROUTINE
      !
      MODULE PURE SUBROUTINE SORT_B_by_A_and_P_INT64_REL64(dim, A, B, P, descend)
             INTEGER,                                  intent(in   ) :: dim
             INTEGER(INT64), dimension(dim),           intent(in   ) :: A       ! A and B must have the same dim
             REAL(REL64),    dimension(dim),           intent(inout) :: B    
             INTEGER,        dimension(dim), optional, intent(inout) :: P       ! Applies permutations to sort B, then sorts B on repeated elements of A. P on exit reflect sort for both A and B
             LOGICAL,                        optional, intent(in   ) :: descend ! If present and true, then the sort of B along repeated A is in descending order.
      END SUBROUTINE
      !
      MODULE PURE SUBROUTINE SORT_B_by_A_and_P_REL32_REL64(dim, A, B, P, descend)
             INTEGER,                                  intent(in   ) :: dim
             REAL(REL32),    dimension(dim),           intent(in   ) :: A       ! A and B must have the same dim
             REAL(REL64),    dimension(dim),           intent(inout) :: B    
             INTEGER,        dimension(dim), optional, intent(inout) :: P       ! Applies permutations to sort B, then sorts B on repeated elements of A. P on exit reflect sort for both A and B
             LOGICAL,                        optional, intent(in   ) :: descend ! If present and true, then the sort of B along repeated A is in descending order.
      END SUBROUTINE
      !
      MODULE PURE SUBROUTINE SORT_B_by_A_and_P_REL64_REL64(dim, A, B, P, descend)
             INTEGER,                                  intent(in   ) :: dim
             REAL(REL64),    dimension(dim),           intent(in   ) :: A       ! A and B must have the same dim
             REAL(REL64),    dimension(dim),           intent(inout) :: B    
             INTEGER,        dimension(dim), optional, intent(inout) :: P       ! Applies permutations to sort B, then sorts B on repeated elements of A. P on exit reflect sort for both A and B
             LOGICAL,                        optional, intent(in   ) :: descend ! If present and true, then the sort of B along repeated A is in descending order.
      END SUBROUTINE
      !
      MODULE PURE SUBROUTINE SORT_B_by_A_and_P_CHAR_REL64(dim, A, B, P, descend)
             INTEGER,                                  intent(in   ) :: dim
             CHARACTER(*),   dimension(dim),           intent(in   ) :: A       ! A and B must have the same dim
             REAL(REL64),    dimension(dim),           intent(inout) :: B    
             INTEGER,        dimension(dim), optional, intent(inout) :: P       ! Applies permutations to sort B, then sorts B on repeated elements of A. P on exit reflect sort for both A and B
             LOGICAL,                        optional, intent(in   ) :: descend ! If present and true, then the sort of B along repeated A is in descending order.
      END SUBROUTINE
      !
      MODULE PURE SUBROUTINE SETUP_REP_REL64(dim, A, REP)
             INTEGER,                        intent(in   ) :: dim
             REAL(REL64),    dimension(dim), intent(in   ) :: A
             INTEGER,        dimension(dim), intent(inout) :: REP
      END SUBROUTINE
      !
      MODULE PURE SUBROUTINE SORT_B_by_A_and_P_REP_REL64(dim, REP, B, repeat_sort, P, descend)
             INTEGER,                                  intent(in   ) :: dim
             INTEGER,        dimension(dim),           intent(inout) :: REP
             REAL(REL64),    dimension(dim),           intent(inout) :: B    
             LOGICAL,                                  intent(inout) :: repeat_sort ! On entry, indicates if B should be sorted on along repeated values of A. On exit indicates if repeated values were found in A
             INTEGER,        dimension(dim), optional, intent(inout) :: P       ! Applies permutations to sort B, then sorts B on repeated elements of A. P on exit reflect sort for both A and B
             LOGICAL,                        optional, intent(in   ) :: descend ! If present and true, then the sort of B along repeated REP is in descending order.
      END SUBROUTINE
      !
  END INTERFACE
  !
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !#####################################################################################################
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !
  INTERFACE ! SORT_INTERFACE_CHAR
      !
      MODULE PURE SUBROUTINE ALLOCATE_PERM_CHAR(A, P)    
             CHARACTER(*), dimension(:),              intent(in   ) :: A
             INTEGER,      dimension(:), allocatable, intent(inout) :: P
      END SUBROUTINE
      !
      MODULE PURE FUNCTION SORT_TYPE_CHAR(dim, A, DESCEND, STABLE, QSORT, HEAP, ISORT, SymMerge, nat, cap) RESULT(ANS)
             INTEGER,                      intent(in) :: dim
             CHARACTER(*), dimension(dim), intent(in) :: A
             LOGICAL,            optional, intent(in) :: DESCEND, STABLE, QSORT, HEAP, ISORT, SymMerge, nat, cap
             INTEGER:: ANS
      END FUNCTION
      !
      MODULE PURE FUNCTION SORTED_1D_CHAR(A, DESCEND, STABLE, nat, cap) RESULT(SORTED_ARRAY)
             CHARACTER(*), dimension(:), contiguous,           intent(in) :: A
             LOGICAL,                                optional, intent(in) :: DESCEND, STABLE
             LOGICAL,                                optional, intent(in) :: nat       ! If true, then due a natural sort of strings
             LOGICAL,                                optional, intent(in) :: cap       ! If true, then do case insensitive sort of strings
             CHARACTER(len(A)), dimension(SIZE(A)):: SORTED_ARRAY
      END FUNCTION
      !
      MODULE PURE SUBROUTINE SORT_1D_CHAR(A, DESCEND, STABLE, P, SORT_TYPE, QSORT, HEAP, ISORT, SymMerge, INIT_P, nat, cap)
             CHARACTER(*), dimension(:), contiguous,    intent(inout) :: A
             LOGICAL,                           optional, intent(in   ) :: DESCEND   ! Sort in Decending Order
             LOGICAL,                           optional, intent(in   ) :: STABLE    ! Use only Stable Sorting Methods
             INTEGER, dimension(:), contiguous, optional, intent(inout) :: P         ! Contains permutations to sort A (Sorts along with A)
             INTEGER,                           optional, intent(in   ) :: SORT_TYPE ! If present, then disables call to SORT_TYPE sets the sort type to the optiona defined. It must be a valid sort type option integer.
             LOGICAL,                           optional, intent(in   ) :: QSORT     ! Use only Dual Pivot Quicksort/IntroSort
             LOGICAL,                           optional, intent(in   ) :: HEAP      ! Use only Heap Sort
             LOGICAL,                           optional, intent(in   ) :: ISORT     ! Use only Insertion Sort
             LOGICAL,                           optional, intent(in   ) :: SymMerge  ! Use only Stable Minimum Storage Merging by Symmetric Comparisons
             LOGICAL,                           optional, intent(in   ) :: INIT_P    ! Set P = [1...to...DIM]; Default is True; which makes P a permutation vector for how A is sorted
             LOGICAL,                           optional, intent(in   ) :: nat       ! If true, then due a natural sort of strings
             LOGICAL,                           optional, intent(in   ) :: cap       ! If true, then do case insensitive sort of strings
      END SUBROUTINE
      !
      MODULE PURE SUBROUTINE SORT_1D_QSORT_A_CHAR(dim, A, DESCEND, nat, cap)
             INTEGER,                      intent(in   ) :: dim
             CHARACTER(*), dimension(dim), intent(inout) :: A
             LOGICAL,            optional, intent(in   ) :: DESCEND   ! Sort in Decending Order
             LOGICAL,            optional, intent(in   ) :: nat       ! If true, then due a natural sort of strings
             LOGICAL,            optional, intent(in   ) :: cap       ! If true, then do case insensitive sort of strings
      END SUBROUTINE
      !
      MODULE PURE SUBROUTINE SORT_1D_SymMg_P_CHAR(dim, A, P, DESCEND, nat, cap)
             INTEGER,                      intent(in   ) :: dim
             CHARACTER(*), dimension(dim), intent(inout) :: A
             INTEGER,      dimension(dim), intent(inout) :: P
             LOGICAL,            optional, intent(in   ) :: DESCEND   ! Sort in Decending Order
             LOGICAL,            optional, intent(in   ) :: nat       ! If true, then due a natural sort of strings
             LOGICAL,            optional, intent(in   ) :: cap       ! If true, then do case insensitive sort of strings
      END SUBROUTINE
      !
      MODULE PURE SUBROUTINE SORT_2D_INDEX_CHAR(A, SORT_INDEX, DESCEND, SORT_BY_ROW, P, NO_STABLE, nat, cap)
             CHARACTER(*), dimension(:,:), contiguous,    intent(inout) :: A           !
             INTEGER,                                     intent(in   ) :: SORT_INDEX  ! Col or Row to sort, the rest are shifted based on sort. SORT_INDEX > 0 indicates column to sort on; SORT_INDEX < 0 indicates row to sort on; ABS(SORT_INDEX) is the row to sort on.
             LOGICAL,                           optional, intent(in   ) :: DESCEND     ! Sort Col/Row in desecending order, default is False
             LOGICAL,                           optional, intent(in   ) :: SORT_BY_ROW ! If present, overrides sign of SORT_INDEX and either sorts by row (True) or by column.
             INTEGER, dimension(:), contiguous, optional, intent(inout) :: P           ! Permulation vector that produced the sort of SORT_INDEX
             LOGICAL,                           optional, intent(in   ) :: NO_STABLE   ! Default is to do a stable sort (NO_STABLE=FALSE). NO_STABLE=TRUE disables this and uses the fastest sorting algorithm. *Note that if there are repeated values in the sorted col/row, then sort order of the other columns is random.
             LOGICAL,                           optional, intent(in   ) :: nat       ! If true, then due a natural sort of strings
             LOGICAL,                           optional, intent(in   ) :: cap       ! If true, then do case insensitive sort of strings
      END SUBROUTINE
      !
      MODULE PURE SUBROUTINE SORT_2D_MULTI_CHAR_1DSC(A, SORT_INDICES, DESCEND, SORT_BY_ROW, P, NO_STABLE, nat, cap)
             CHARACTER(*), dimension(:,:), contiguous,    intent(inout) :: A
             INTEGER,      dimension(:),   contiguous,    intent(in   ) :: SORT_INDICES ! Columns or Rows to sort, the rest are shifted based on sort. SORT_INDICES(1) > 0 indicates column to sort on; SORT_INDICES(1) < 0 indicates row to sort on; ABS(SORT_INDICES) is the rows to sort on.
             LOGICAL,                                     intent(in   ) :: DESCEND      ! Sort Col/Row in desecending order, default is False
             LOGICAL,                           optional, intent(in   ) :: SORT_BY_ROW  ! If present, overrides sign of SORT_INDICES(1) and either sorts by row (True) or by column.
             INTEGER, dimension(:), contiguous, optional, intent(inout) :: P            ! Permulation vector that produced the sort of SORT_INDEX
             LOGICAL,                           optional, intent(in   ) :: NO_STABLE    ! Default is to do a stable sort (NO_STABLE=FALSE). NO_STABLE=TRUE disables this and uses the fastest sorting algorithm. *Note that if there are repeated values in the sorted col/row, then sort order of the other columns is random.
             LOGICAL,                           optional, intent(in   ) :: nat       ! If true, then due a natural sort of strings
             LOGICAL,                           optional, intent(in   ) :: cap       ! If true, then do case insensitive sort of strings
      END SUBROUTINE 
      !
      MODULE PURE SUBROUTINE SORT_2D_MULTI_CHAR(A, SORT_INDICES, DESCEND, SORT_BY_ROW, P, NO_STABLE, nat, cap)    ! SORT_INDICES must contain unique values
             CHARACTER(*), dimension(:,:),   contiguous,  intent(inout) :: A
             INTEGER,        dimension(:),   contiguous,  intent(in   ) :: SORT_INDICES ! Columns or Rows to sort, the rest are shifted based on sort. SORT_INDICES(1) > 0 indicates column to sort on; SORT_INDICES(1) < 0 indicates row to sort on; ABS(SORT_INDICES) is the rows to sort on.
             LOGICAL, dimension(:), contiguous, optional, intent(in   ) :: DESCEND      ! Sort Col/Row in desecending order, default is False
             LOGICAL,                           optional, intent(in   ) :: SORT_BY_ROW  ! If present, overrides sign of SORT_INDICES(1) and either sorts by row (True) or by column.
             INTEGER, dimension(:), contiguous, optional, intent(inout) :: P            ! Permulation vector that produced the sort of SORT_INDEX
             LOGICAL,                           optional, intent(in   ) :: NO_STABLE    ! Default is to do a stable sort (NO_STABLE=FALSE). NO_STABLE=TRUE disables this and uses the fastest sorting algorithm. *Note that if there are repeated values in the sorted col/row, then sort order of the other columns is random.
             LOGICAL,                           optional, intent(in   ) :: nat       ! If true, then due a natural sort of strings
             LOGICAL,                           optional, intent(in   ) :: cap       ! If true, then do case insensitive sort of strings
      END SUBROUTINE 
      !
      MODULE PURE SUBROUTINE PERMUTATION_SORT_1D_CHAR(A, P)
             CHARACTER(*), dimension(:), contiguous, intent(inout) :: A
             INTEGER,      dimension(:), contiguous, intent(inout) :: P
      END SUBROUTINE
      !
      MODULE PURE SUBROUTINE PERMUTATION_SORT_2D_CHAR(A, P, BY_COLUMN) 
             CHARACTER(*), dimension(:,:), contiguous, intent(inout) :: A
             INTEGER,      dimension(:),   contiguous, intent(inout) :: P
             LOGICAL,                        optional, intent(in   ) :: BY_COLUMN
      END SUBROUTINE
      !
      MODULE PURE SUBROUTINE REVERSE_ORDER_1D_DIM_CHAR(dim, A, STABLE, P, INIT_P)
             INTEGER,                                intent(in   ) :: dim
             CHARACTER(*), dimension(dim),           intent(inout) :: A
             LOGICAL,                      optional, intent(in   ) :: STABLE
             INTEGER,      dimension(dim), optional, intent(inout) :: P
             LOGICAL,                      optional, intent(in   ) :: INIT_P
      END SUBROUTINE
      !
      MODULE PURE SUBROUTINE REVERSE_ORDER_1D_NODIM_CHAR(A, STABLE, P, INIT_P)
             CHARACTER(*), dimension(:), contiguous,           intent(inout) :: A
             LOGICAL,                                  optional, intent(in   ) :: STABLE
             INTEGER,        dimension(:), contiguous, optional, intent(inout) :: P
             LOGICAL,                                  optional, intent(in   ) :: INIT_P
      END SUBROUTINE
      !  
      MODULE PURE SUBROUTINE REVERSE_ORDER_2D_CHAR(A, REVERSE_EACH_ROW)
             CHARACTER(*), dimension(:,:), intent(inout) :: A
             LOGICAL,            optional, intent(in   ) :: REVERSE_EACH_ROW ! Default is TRUE
      END SUBROUTINE
      !
      MODULE PURE SUBROUTINE REVERSE_ORDER_2D_INDEX_CHAR(A, REV_INDEX, STABLE, REVERSE_ROW, P)
             CHARACTER(*), dimension(:,:), contiguous,           intent(inout) :: A
             INTEGER,                                            intent(in   ) :: REV_INDEX
             LOGICAL,                                  optional, intent(in   ) :: STABLE
             LOGICAL,                                  optional, intent(in   ) :: REVERSE_ROW
             INTEGER,      dimension(:),   contiguous, optional, intent(inout) :: P
      END SUBROUTINE
      !
      MODULE PURE SUBROUTINE SORT_B_by_A_and_P_INT32_CHAR(dim, A, B, P, descend, nat, cap)
             INTEGER,                                  intent(in   ) :: dim
             INTEGER(INT32), dimension(dim),           intent(in   ) :: A       ! A and B must have the same dim
             CHARACTER(*),   dimension(dim),           intent(inout) :: B    
             INTEGER,        dimension(dim), optional, intent(inout) :: P       ! Applies permutations to sort B, then sorts B on repeated elements of A. P on exit reflect sort for both A and B
             LOGICAL,                        optional, intent(in   ) :: descend ! If present and true, then the sort of B along repeated A is in descending order.
             LOGICAL,                        optional, intent(in   ) :: nat, cap
      END SUBROUTINE
      !
      MODULE PURE SUBROUTINE SORT_B_by_A_and_P_INT64_CHAR(dim, A, B, P, descend, nat, cap)
             INTEGER,                                  intent(in   ) :: dim
             INTEGER(INT64), dimension(dim),           intent(in   ) :: A       ! A and B must have the same dim
             CHARACTER(*),   dimension(dim),           intent(inout) :: B    
             INTEGER,        dimension(dim), optional, intent(inout) :: P       ! Applies permutations to sort B, then sorts B on repeated elements of A. P on exit reflect sort for both A and B
             LOGICAL,                        optional, intent(in   ) :: descend ! If present and true, then the sort of B along repeated A is in descending order.
             LOGICAL,                        optional, intent(in   ) :: nat, cap
      END SUBROUTINE
      !
      MODULE PURE SUBROUTINE SORT_B_by_A_and_P_REL32_CHAR(dim, A, B, P, descend, nat, cap)
             INTEGER,                                  intent(in   ) :: dim
             REAL(REL32),    dimension(dim),           intent(in   ) :: A       ! A and B must have the same dim
             CHARACTER(*),   dimension(dim),           intent(inout) :: B    
             INTEGER,        dimension(dim), optional, intent(inout) :: P       ! Applies permutations to sort B, then sorts B on repeated elements of A. P on exit reflect sort for both A and B
             LOGICAL,                        optional, intent(in   ) :: descend ! If present and true, then the sort of B along repeated A is in descending order.
             LOGICAL,                        optional, intent(in   ) :: nat, cap
      END SUBROUTINE
      !
      MODULE PURE SUBROUTINE SORT_B_by_A_and_P_REL64_CHAR(dim, A, B, P, descend, nat, cap)
             INTEGER,                                  intent(in   ) :: dim
             REAL(REL64),    dimension(dim),           intent(in   ) :: A       ! A and B must have the same dim
             CHARACTER(*),   dimension(dim),           intent(inout) :: B    
             INTEGER,        dimension(dim), optional, intent(inout) :: P       ! Applies permutations to sort B, then sorts B on repeated elements of A. P on exit reflect sort for both A and B
             LOGICAL,                        optional, intent(in   ) :: descend ! If present and true, then the sort of B along repeated A is in descending order.
             LOGICAL,                        optional, intent(in   ) :: nat, cap
      END SUBROUTINE
      !
      MODULE PURE SUBROUTINE SORT_B_by_A_and_P_CHAR_CHAR(dim, A, B, P, descend, nat, cap)
             INTEGER,                                  intent(in   ) :: dim
             CHARACTER(*),   dimension(dim),           intent(in   ) :: A       ! A and B must have the same dim
             CHARACTER(*),   dimension(dim),           intent(inout) :: B    
             INTEGER,        dimension(dim), optional, intent(inout) :: P       ! Applies permutations to sort B, then sorts B on repeated elements of A. P on exit reflect sort for both A and B
             LOGICAL,                        optional, intent(in   ) :: descend ! If present and true, then the sort of B along repeated A is in descending order.
             LOGICAL,                        optional, intent(in   ) :: nat, cap
      END SUBROUTINE
      !
      MODULE PURE SUBROUTINE SETUP_REP_CHAR(dim, A, REP)
             INTEGER,                        intent(in   ) :: dim
             CHARACTER(*),   dimension(dim), intent(in   ) :: A
             INTEGER,        dimension(dim), intent(inout) :: REP
      END SUBROUTINE
      !
      MODULE PURE SUBROUTINE SORT_B_by_A_and_P_REP_CHAR(dim, REP, B, repeat_sort, P, descend, nat, cap)
             INTEGER,                                  intent(in   ) :: dim
             INTEGER,        dimension(dim),           intent(inout) :: REP
             CHARACTER(*),   dimension(dim),           intent(inout) :: B    
             LOGICAL,                                  intent(inout) :: repeat_sort ! On entry, indicates if B should be sorted on along repeated values of A. On exit indicates if repeated values were found in A
             INTEGER,        dimension(dim), optional, intent(inout) :: P       ! Applies permutations to sort B, then sorts B on repeated elements of A. P on exit reflect sort for both A and B
             LOGICAL,                        optional, intent(in   ) :: descend ! If present and true, then the sort of B along repeated REP is in descending order.
             LOGICAL,                        optional, intent(in   ) :: nat, cap
      END SUBROUTINE
      !
  END INTERFACE
  !
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !#####################################################################################################
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !
  INTERFACE ! SORT_INTERFACE_WILD
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
      END SUBROUTINE
      !
      MODULE PURE SUBROUTINE PERMUTATION_SORT_1D_LESS_WILD(A, P, SWAP, LESS)
             CLASS(*), dimension(:), contiguous, intent(inout) :: A
             INTEGER,  dimension(:), contiguous, intent(inout) :: P
             PROCEDURE(SWAP_METHOD):: SWAP
             PROCEDURE(LESS_METHOD):: LESS
      END SUBROUTINE
      !
      MODULE PURE SUBROUTINE PERMUTATION_SORT_1D_WILD(A, P, SWAP)
             CLASS(*), dimension(:), contiguous, intent(inout) :: A
             INTEGER,  dimension(:), contiguous, intent(inout) :: P
             PROCEDURE(SWAP_METHOD):: SWAP
      END SUBROUTINE
      !
      MODULE PURE SUBROUTINE REVERSE_ORDER_1D_NOLESS_WILD(A, SWAP, P, INIT_P)
             CLASS(*), dimension(:), contiguous,           intent(inout) :: A
             INTEGER,  dimension(:), contiguous, optional, intent(inout) :: P
             LOGICAL,                            optional, intent(in   ) :: INIT_P
             PROCEDURE(SWAP_METHOD):: SWAP
      END SUBROUTINE
      !
      MODULE PURE SUBROUTINE REVERSE_ORDER_1D_NODIM_WILD(A, SWAP, LESS, STABLE, P, INIT_P) 
             CLASS(*), dimension(:), contiguous,           intent(inout) :: A
             LOGICAL,                            optional, intent(in   ) :: STABLE
             INTEGER,  dimension(:), contiguous, optional, intent(inout) :: P
             LOGICAL,                            optional, intent(in   ) :: INIT_P
             PROCEDURE(SWAP_METHOD):: SWAP
             PROCEDURE(LESS_METHOD):: LESS
      END SUBROUTINE
      !
      MODULE PURE SUBROUTINE REVERSE_ORDER_1D_NOLESS_DIM_WILD(dim, A, SWAP, P, INIT_P)
             INTEGER,                            intent(in   ) :: dim
             CLASS(*), dimension(dim),           intent(inout) :: A
             INTEGER,  dimension(dim), optional, intent(inout) :: P
             LOGICAL,                  optional, intent(in   ) :: INIT_P
             PROCEDURE(SWAP_METHOD):: SWAP
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
      END SUBROUTINE
      !
  END INTERFACE
  !
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !#####################################################################################################
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !
  INTERFACE ! SORT_INTERFACE_1D_MULTI
      ! 
      MODULE PURE SUBROUTINE SORT_1D_MULTI_DSC1_WILD_WILD_WILD(A,B,C,D,E,F,G,H,I,J, repeat_sort, DESCEND, P, nat, cap) ! A, B, C, ... I are sorted by the dim of A, so dim=size(A) <= size(B, C, D, E, F, G, H, I, J)
             CLASS(*), dimension(:), contiguous,           intent(inout) :: A,B,C
             CLASS(*), dimension(:), contiguous, optional, intent(inout) :: D,E,F,G,H,I,J
             LOGICAL,                            optional, intent(in   ) :: repeat_sort
             LOGICAL,                                      intent(in   ) :: DESCEND
             INTEGER,  dimension(:), contiguous, optional, intent(inout) :: P
             LOGICAL,                            optional, intent(in   ) :: nat         ! If true, then due a natural sort of strings
             LOGICAL,                            optional, intent(in   ) :: cap         ! If true, then do case insensitive sort of strings
      END SUBROUTINE
      ! 
      MODULE PURE SUBROUTINE SORT_1D_MULTI_DSC2_WILD_WILD_WILD(A,B,C,D,E,F,G,H,I,J, repeat_sort, DESCEND, P, nat, cap) ! A, B, C, ... I are sorted by the dim of A, so dim=size(A) <= size(B, C, D, E, F, G, H, I, J)
             CLASS(*), dimension(:), contiguous,           intent(inout) :: A,B,C
             CLASS(*), dimension(:), contiguous, optional, intent(inout) :: D,E,F,G,H,I,J
             LOGICAL,                            optional, intent(in   ) :: repeat_sort
             LOGICAL,  dimension(:),             optional, intent(in   ) :: DESCEND
             INTEGER,  dimension(:), contiguous, optional, intent(inout) :: P
             LOGICAL,                            optional, intent(in   ) :: nat         ! If true, then due a natural sort of strings
             LOGICAL,                            optional, intent(in   ) :: cap         ! If true, then do case insensitive sort of strings
      END SUBROUTINE
      !
      !-----------------------------------------------------------------------------
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
      END SUBROUTINE
      !
      !-----------------------------------------------------------------------------
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
      END SUBROUTINE
      !
      !-----------------------------------------------------------------------------
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
      END SUBROUTINE
      !
      !-----------------------------------------------------------------------------
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
      END SUBROUTINE
      !
      !-----------------------------------------------------------------------------
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
      END SUBROUTINE
      !
      !##############################################################################################################################################
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
      END SUBROUTINE
      !
      !-----------------------------------------------------------------------------
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
      END SUBROUTINE
      !
      !-----------------------------------------------------------------------------
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
      END SUBROUTINE
      !
      !-----------------------------------------------------------------------------
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
      END SUBROUTINE
      !
      !-----------------------------------------------------------------------------
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
      END SUBROUTINE
      !
      !##############################################################################################################################################
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
      END SUBROUTINE
      !
      !-----------------------------------------------------------------------------
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
      END SUBROUTINE
      !
      !-----------------------------------------------------------------------------
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
      END SUBROUTINE
      !
      !-----------------------------------------------------------------------------
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
      END SUBROUTINE
      !
      !-----------------------------------------------------------------------------
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
      END SUBROUTINE
      !
      !##############################################################################################################################################
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
      END SUBROUTINE
      !
      !-----------------------------------------------------------------------------
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
      END SUBROUTINE
      !
      !-----------------------------------------------------------------------------
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
      END SUBROUTINE
      !
      !-----------------------------------------------------------------------------
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
      END SUBROUTINE
      !
      !-----------------------------------------------------------------------------
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
      END SUBROUTINE
      !
      !##############################################################################################################################################
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
      END SUBROUTINE
      !
      !-----------------------------------------------------------------------------
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
      END SUBROUTINE
      !
      !-----------------------------------------------------------------------------
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
      END SUBROUTINE
      !
      !-----------------------------------------------------------------------------
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
      END SUBROUTINE
      !
      !-----------------------------------------------------------------------------
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
      END SUBROUTINE
      !
      !##############################################################################################################################################
      !
      MODULE PURE SUBROUTINE SORT_1D_MULTI_INT32_INT64(A, B, DESCEND, IDX) 
             INTEGER(INT32), dimension(:), contiguous,           intent(inout):: A
             INTEGER(INT64), dimension(:), contiguous,           intent(inout):: B
             LOGICAL,                                  optional, intent(in   ):: DESCEND
             INTEGER,        dimension(:), contiguous, optional, intent(inout):: IDX
      END SUBROUTINE
      !
      MODULE PURE SUBROUTINE SORT_1D_MULTI_INT32_REL32(A, B, DESCEND, IDX) 
             INTEGER(INT32), dimension(:), contiguous,           intent(inout):: A
             REAL   (REL32), dimension(:), contiguous,           intent(inout):: B
             LOGICAL,                                  optional, intent(in   ):: DESCEND
             INTEGER,        dimension(:), contiguous, optional, intent(inout):: IDX
      END SUBROUTINE
      !
      MODULE PURE SUBROUTINE SORT_1D_MULTI_INT32_REL64(A, B, DESCEND, IDX) 
             INTEGER(INT32), dimension(:), contiguous,           intent(inout):: A
             REAL   (REL64), dimension(:), contiguous,           intent(inout):: B
             LOGICAL,                                  optional, intent(in   ):: DESCEND
             INTEGER,        dimension(:), contiguous, optional, intent(inout):: IDX
      END SUBROUTINE
      !
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !
      MODULE PURE SUBROUTINE SORT_1D_DIM_MULTI_INT32_INT32(DIM, A, B, DESCEND, IDX) 
        INTEGER,                                  intent(in   ):: DIM
        INTEGER(INT32), dimension(DIM),           intent(inout):: A
        INTEGER(INT32), dimension(DIM),           intent(inout):: B
        LOGICAL,                        optional, intent(in   ):: DESCEND
        INTEGER,        dimension(DIM), optional, intent(inout):: IDX
      END SUBROUTINE
      !
      MODULE PURE SUBROUTINE SORT_1D_DIM_MULTI_INT32_INT64(DIM, A, B, DESCEND, IDX) 
        INTEGER,                                  intent(in   ):: DIM
        INTEGER(INT32), dimension(DIM),           intent(inout):: A
        INTEGER(INT64), dimension(DIM),           intent(inout):: B
        LOGICAL,                        optional, intent(in   ):: DESCEND
        INTEGER,        dimension(DIM), optional, intent(inout):: IDX
      END SUBROUTINE
      !
      MODULE PURE SUBROUTINE SORT_1D_DIM_MULTI_INT32_REL32(DIM, A, B, DESCEND, IDX) 
        INTEGER,                                  intent(in   ):: DIM
        INTEGER(INT32), dimension(DIM),           intent(inout):: A
        REAL   (REL32), dimension(DIM),           intent(inout):: B
        LOGICAL,                        optional, intent(in   ):: DESCEND
        INTEGER,        dimension(DIM), optional, intent(inout):: IDX
      END SUBROUTINE
      !
      MODULE PURE SUBROUTINE SORT_1D_DIM_MULTI_INT32_REL64(DIM, A, B, DESCEND, IDX) 
        INTEGER,                                  intent(in   ):: DIM
        INTEGER(INT32), dimension(DIM),           intent(inout):: A
        REAL   (REL64), dimension(DIM),           intent(inout):: B
        LOGICAL,                        optional, intent(in   ):: DESCEND
        INTEGER,        dimension(DIM), optional, intent(inout):: IDX
      END SUBROUTINE
      !
      !-----------------------------------------------------------------------------
      !
      MODULE PURE SUBROUTINE SORT_1D_MULTI_INT64_INT32(A, B, DESCEND, IDX)
             INTEGER(INT64), dimension(:), contiguous,           intent(inout):: A
             INTEGER(INT32), dimension(:), contiguous,           intent(inout):: B
             LOGICAL,                                  optional, intent(in   ):: DESCEND
             INTEGER,        dimension(:), contiguous, optional, intent(inout):: IDX
      END SUBROUTINE
      !
      MODULE PURE SUBROUTINE SORT_1D_MULTI_INT64_INT64(A, B, DESCEND, IDX) 
             INTEGER(INT64), dimension(:), contiguous,           intent(inout):: A
             INTEGER(INT64), dimension(:), contiguous,           intent(inout):: B
             LOGICAL,                                  optional, intent(in   ):: DESCEND
             INTEGER,        dimension(:), contiguous, optional, intent(inout):: IDX
      END SUBROUTINE
      !
      MODULE PURE SUBROUTINE SORT_1D_MULTI_INT64_REL32(A, B, DESCEND, IDX) 
             INTEGER(INT64), dimension(:), contiguous,           intent(inout):: A
             REAL   (REL32), dimension(:), contiguous,           intent(inout):: B
             LOGICAL,                                  optional, intent(in   ):: DESCEND
             INTEGER,        dimension(:), contiguous, optional, intent(inout):: IDX
      END SUBROUTINE
      !
      MODULE PURE SUBROUTINE SORT_1D_MULTI_INT64_REL64(A, B, DESCEND, IDX) 
             INTEGER(INT64), dimension(:), contiguous,           intent(inout):: A
             REAL   (REL64), dimension(:), contiguous,           intent(inout):: B
             LOGICAL,                                  optional, intent(in   ):: DESCEND
             INTEGER,        dimension(:), contiguous, optional, intent(inout):: IDX
      END SUBROUTINE
      !
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !
      MODULE PURE SUBROUTINE SORT_1D_DIM_MULTI_INT64_INT32(DIM, A, B, DESCEND, IDX) 
        INTEGER,                                  intent(in   ):: DIM
        INTEGER(INT64), dimension(DIM),           intent(inout):: A
        INTEGER(INT32), dimension(DIM),           intent(inout):: B
        LOGICAL,                        optional, intent(in   ):: DESCEND
        INTEGER,        dimension(DIM), optional, intent(inout):: IDX
      END SUBROUTINE
      !
      MODULE PURE SUBROUTINE SORT_1D_DIM_MULTI_INT64_INT64(DIM, A, B, DESCEND, IDX) 
        INTEGER,                                  intent(in   ):: DIM
        INTEGER(INT64), dimension(DIM),           intent(inout):: A
        INTEGER(INT64), dimension(DIM),           intent(inout):: B
        LOGICAL,                        optional, intent(in   ):: DESCEND
        INTEGER,        dimension(DIM), optional, intent(inout):: IDX
      END SUBROUTINE
      !
      MODULE PURE SUBROUTINE SORT_1D_DIM_MULTI_INT64_REL32(DIM, A, B, DESCEND, IDX) 
        INTEGER,                                  intent(in   ):: DIM
        INTEGER(INT64), dimension(DIM),           intent(inout):: A
        REAL   (REL32), dimension(DIM),           intent(inout):: B
        LOGICAL,                        optional, intent(in   ):: DESCEND
        INTEGER,        dimension(DIM), optional, intent(inout):: IDX
      END SUBROUTINE
      !
      MODULE PURE SUBROUTINE SORT_1D_DIM_MULTI_INT64_REL64(DIM, A, B, DESCEND, IDX) 
        INTEGER,                                  intent(in   ):: DIM
        INTEGER(INT64), dimension(DIM),           intent(inout):: A
        REAL   (REL64), dimension(DIM),           intent(inout):: B
        LOGICAL,                        optional, intent(in   ):: DESCEND
        INTEGER,        dimension(DIM), optional, intent(inout):: IDX
      END SUBROUTINE
      !
      !-----------------------------------------------------------------------------
      !
      MODULE PURE SUBROUTINE SORT_1D_MULTI_REL32_INT32(A, B, DESCEND, IDX) 
             REAL   (REL32), dimension(:), contiguous,           intent(inout):: A
             INTEGER(INT32), dimension(:), contiguous,           intent(inout):: B
             LOGICAL,                                  optional, intent(in   ):: DESCEND
             INTEGER,        dimension(:), contiguous, optional, intent(inout):: IDX
      END SUBROUTINE
      !
      MODULE PURE SUBROUTINE SORT_1D_MULTI_REL32_INT64(A, B, DESCEND, IDX) 
             REAL   (REL32), dimension(:), contiguous,           intent(inout):: A
             INTEGER(INT64), dimension(:), contiguous,           intent(inout):: B
             LOGICAL,                                  optional, intent(in   ):: DESCEND
             INTEGER,        dimension(:), contiguous, optional, intent(inout):: IDX
      END SUBROUTINE
      !
      MODULE PURE SUBROUTINE SORT_1D_MULTI_REL32_REL32(A, B, DESCEND, IDX) 
             REAL(REL32), dimension(:), contiguous,           intent(inout):: A
             REAL(REL32), dimension(:), contiguous,           intent(inout):: B
             LOGICAL,                               optional, intent(in   ):: DESCEND
             INTEGER,     dimension(:), contiguous, optional, intent(inout):: IDX
      END SUBROUTINE
      !
      MODULE PURE SUBROUTINE SORT_1D_MULTI_REL32_REL64(A, B, DESCEND, IDX) 
             REAL(REL32), dimension(:), contiguous,           intent(inout):: A
             REAL(REL64), dimension(:), contiguous,           intent(inout):: B
             LOGICAL,                               optional, intent(in   ):: DESCEND
             INTEGER,     dimension(:), contiguous, optional, intent(inout):: IDX
      END SUBROUTINE
      !
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !
      MODULE PURE SUBROUTINE SORT_1D_DIM_MULTI_REL32_INT32(DIM, A, B, DESCEND, IDX) 
        INTEGER,                                  intent(in   ):: DIM
        REAL   (REL32), dimension(DIM),           intent(inout):: A
        INTEGER(INT32), dimension(DIM),           intent(inout):: B
        LOGICAL,                        optional, intent(in   ):: DESCEND
        INTEGER,        dimension(DIM), optional, intent(inout):: IDX
      END SUBROUTINE
      !
      MODULE PURE SUBROUTINE SORT_1D_DIM_MULTI_REL32_INT64(DIM, A, B, DESCEND, IDX) 
        INTEGER,                                  intent(in   ):: DIM
        REAL   (REL32), dimension(DIM),           intent(inout):: A
        INTEGER(INT64), dimension(DIM),           intent(inout):: B
        LOGICAL,                        optional, intent(in   ):: DESCEND
        INTEGER,        dimension(DIM), optional, intent(inout):: IDX
      END SUBROUTINE
      !
      MODULE PURE SUBROUTINE SORT_1D_DIM_MULTI_REL32_REL32(DIM, A, B, DESCEND, IDX) 
        INTEGER,                               intent(in   ):: DIM
        REAL(REL32), dimension(DIM),           intent(inout):: A
        REAL(REL32), dimension(DIM),           intent(inout):: B
        LOGICAL,                     optional, intent(in   ):: DESCEND
        INTEGER,     dimension(DIM), optional, intent(inout):: IDX
      END SUBROUTINE
      !
      MODULE PURE SUBROUTINE SORT_1D_DIM_MULTI_REL32_REL64(DIM, A, B, DESCEND, IDX) 
        INTEGER,                               intent(in   ):: DIM
        REAL(REL32), dimension(DIM),           intent(inout):: A
        REAL(REL64), dimension(DIM),           intent(inout):: B
        LOGICAL,                     optional, intent(in   ):: DESCEND
        INTEGER,     dimension(DIM), optional, intent(inout):: IDX
      END SUBROUTINE
      !
      !-----------------------------------------------------------------------------
      !
      MODULE PURE SUBROUTINE SORT_1D_MULTI_REL64_INT32(A, B, DESCEND, IDX) 
             REAL   (REL64), dimension(:), contiguous,           intent(inout):: A
             INTEGER(INT32), dimension(:), contiguous,           intent(inout):: B
             LOGICAL,                                  optional, intent(in   ):: DESCEND
             INTEGER,        dimension(:), contiguous, optional, intent(inout):: IDX
      END SUBROUTINE
      !
      MODULE PURE SUBROUTINE SORT_1D_MULTI_REL64_INT64(A, B, DESCEND, IDX) 
             REAL   (REL64), dimension(:), contiguous,           intent(inout):: A
             INTEGER(INT64), dimension(:), contiguous,           intent(inout):: B
             LOGICAL,                                  optional, intent(in   ):: DESCEND
             INTEGER,        dimension(:), contiguous, optional, intent(inout):: IDX
      END SUBROUTINE
      !
      MODULE PURE SUBROUTINE SORT_1D_MULTI_REL64_REL32(A, B, DESCEND, IDX) 
             REAL(REL64), dimension(:), contiguous,           intent(inout):: A
             REAL(REL32), dimension(:), contiguous,           intent(inout):: B
             LOGICAL,                               optional, intent(in   ):: DESCEND
             INTEGER,     dimension(:), contiguous, optional, intent(inout):: IDX
      END SUBROUTINE
      !
      MODULE PURE SUBROUTINE SORT_1D_MULTI_REL64_REL64(A, B, DESCEND, IDX) 
             REAL(REL64), dimension(:), contiguous,           intent(inout):: A
             REAL(REL64), dimension(:), contiguous,           intent(inout):: B
             LOGICAL,                               optional, intent(in   ):: DESCEND
             INTEGER,     dimension(:), contiguous, optional, intent(inout):: IDX
      END SUBROUTINE
      !
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !
      MODULE PURE SUBROUTINE SORT_1D_DIM_MULTI_REL64_INT32(DIM, A, B, DESCEND, IDX) 
        INTEGER,                                  intent(in   ):: DIM
        REAL   (REL64), dimension(DIM),           intent(inout):: A
        INTEGER(INT32), dimension(DIM),           intent(inout):: B
        LOGICAL,                        optional, intent(in   ):: DESCEND
        INTEGER,        dimension(DIM), optional, intent(inout):: IDX
      END SUBROUTINE
      !
      MODULE PURE SUBROUTINE SORT_1D_DIM_MULTI_REL64_INT64(DIM, A, B, DESCEND, IDX) 
        INTEGER,                                  intent(in   ):: DIM
        REAL   (REL64), dimension(DIM),           intent(inout):: A
        INTEGER(INT64), dimension(DIM),           intent(inout):: B
        LOGICAL,                        optional, intent(in   ):: DESCEND
        INTEGER,        dimension(DIM), optional, intent(inout):: IDX
      END SUBROUTINE
      !
      MODULE PURE SUBROUTINE SORT_1D_DIM_MULTI_REL64_REL32(DIM, A, B, DESCEND, IDX) 
        INTEGER,                               intent(in   ):: DIM
        REAL(REL64), dimension(DIM),           intent(inout):: A
        REAL(REL32), dimension(DIM),           intent(inout):: B
        LOGICAL,                     optional, intent(in   ):: DESCEND
        INTEGER,     dimension(DIM), optional, intent(inout):: IDX
      END SUBROUTINE
      !
      MODULE PURE SUBROUTINE SORT_1D_DIM_MULTI_REL64_REL64(DIM, A, B, DESCEND, IDX) 
        INTEGER,                               intent(in   ):: DIM
        REAL(REL64), dimension(DIM),           intent(inout):: A
        REAL(REL64), dimension(DIM),           intent(inout):: B
        LOGICAL,                     optional, intent(in   ):: DESCEND
        INTEGER,     dimension(DIM), optional, intent(inout):: IDX
      END SUBROUTINE
      !
      !-----------------------------------------------------------------------------
      !
  END INTERFACE
  !
END MODULE
