!
! CODE DEVELOPED BY SCOTT E BOYCE 
!                   CONTACT <seboyce@usgs.gov> or <Boyce@engineer.com>
!
! The math operators should be avoided cause they create copies
!    For example, DT = DT + 3, will add three to each element, 
!                 but the "DT + 3" will make a temp copy of the base array
!                 Instead it is better to do "CALL DT%ADD(3)", which will operate inplace.
!    
!    
MODULE DYNAMIC_ARRAY_INT32_INSTRUCTION!, ONLY: SIZE, DYNAMIC_ARRAY_INT32
  USE, INTRINSIC:: ISO_FORTRAN_ENV, ONLY: INT32
  USE, INTRINSIC:: ISO_C_BINDING,   ONLY: C_LOC, C_ASSOCIATED, C_PTR
  !
  IMPLICIT NONE(TYPE, EXTERNAL)
  !
  PUBLIC:: DYNAMIC_ARRAY_INT32           ! Optimized for adding and removing values at the end of array (append and pop)
  !PUBLIC:: ARRAY_DEQUE_INT32             ! Optimized for adding and removing values at the front and end of array (prepend, pop(1), append, and pop). If you only append/pop, use DYNAMIC_ARRAY_INT32
  PUBLIC:: CAP, SIZE
  PUBLIC:: SIZE_INT32, CAPACITY_INT32
  !
  PRIVATE
  !
  ! Constants used internally to module ----------------------------------------------------
  ! 
  LOGICAL,   PARAMETER :: TRUE  = .TRUE.
  LOGICAL,   PARAMETER :: FALSE = .FALSE.
  !
  INTEGER(INT32),   PARAMETER :: NEG   =   -1_int32
  INTEGER(INT32),   PARAMETER :: Z     =    0_int32
  INTEGER(INT32),   PARAMETER :: i16   =   16_int32
  INTEGER(INT32),   PARAMETER :: i2048 = 2048_int32
  INTEGER(INT32),   PARAMETER :: i4096 = 4096_int32
  !
  CHARACTER,    PARAMETER:: TAB       = ACHAR( 9)
  CHARACTER,    PARAMETER:: CR        = ACHAR(13)  !CARAGE RETURN (UNIX ENDING)
  CHARACTER,    PARAMETER:: LF        = ACHAR(10)  !LINE   FEED (CRLF IS WINDOWNS ENDING)
  CHARACTER(*), PARAMETER:: lowerCHAR = "abcdefghijklmnopqrstuvwxyz"
  CHARACTER(*), PARAMETER:: upperCHAR = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  !
  CHARACTER(*), PARAMETER:: fmtCOM = '(I0, :", ")'             !-> Comma delimited (default)
  CHARACTER(*), PARAMETER:: fmtSPC = '(I0, :1x)'               !-> Space delimited
  CHARACTER(*), PARAMETER:: fmtNLn = '(I0, :/)'                !-> NewLine delimited
  CHARACTER(*), PARAMETER:: fmtTAB = '(I0, :"'//TAB//'")' !-> Tab delimited  => ACHAR(9) returns the ASCII TAB character, so the final output is '(I0, :"~")', where ~ is used here to represent the invsible tab charater
  !
  ! ----------------------------------------------------------------------------------------
  ! 
  INTERFACE CAP
     MODULE PROCEDURE CAPACITY_INT32
  END INTERFACE
  !
  INTERFACE SIZE
     MODULE PROCEDURE SIZE_INT32
  END INTERFACE
  !
  ! ----------------------------------------------------------------------------------------
  !
  TYPE DYNAMIC_ARRAY_INT32
    INTEGER, private :: siz  = Z  ! Dynamic Size in use
    INTEGER, private :: dim  = Z  ! Allocated space in data
    INTEGER          :: iter = Z  ! Counter for iteration, - allow the user to change it outside of type
    !
    INTEGER(INT32), dimension(:), allocatable, private :: array
    !
    INTEGER(INT32), dimension(:),  contiguous, pointer :: ptr => NULL()  ! Points to array(1:siz)
    !
    CONTAINS
    !
    GENERIC ::                INIT => INIT_VAL, &       ! CALL dyn%INIT([DIM], [val], [siz])
                                      INIT_VEC          ! CALL dyn%INIT(vec, [rep])
    !                                                   
    PROCEDURE, pass(dyn) ::   SIZE => SIZE_INT32        ! dyn%SIZE() RESULT(siz)
    PROCEDURE, pass(dyn) ::    CAP => CAPACITY_INT32    ! dyn%CAP () RESULT(dim)
    !                                                    
    PROCEDURE, pass(dyn) ::  CLEAR => CLEAR_ARRAY       ! CALL dyn%CLEAR()
    PROCEDURE, pass(dyn) :: RESIZE => RESIZE_REFERENCE  ! CALL dyn%RESIZE(siz)  -> Only changes siz parameter; may make array bigger if necessary
    !                                                   
    GENERIC ::              APPEND => APPEND_VAL, &     ! CALL dyn%APPEND(val, [unqiue])
                                      APPEND_VEC        ! CALL dyn%APPEND(vec, [unqiue])
    !                                                   
    GENERIC ::             PREPEND => PREPEND_VAL, &    ! CALL dyn%PREPEND(val, [unqiue])
                                      PREPEND_VEC       ! CALL dyn%PREPEND(vec, [unqiue])
    !                                                   
    PROCEDURE, pass(dyn) :: INSERT => INSERT_VAL        ! CALL dyn%INSERT(pos, val)
    !                                                                 
    GENERIC ::              SET    => SET_POS,  &       ! CALL dyn%SET(pos,  val)     --> Note pos will auto-grow array if pos > dim
                                      SET_RANGE         ! CALL dyn%SET(I, J, val)     --> Note J   will auto-grow array if J   > dim
    !                                                   
    PROCEDURE, pass(dyn) :: GET    => GET_VAL           ! CALL dyn%GET(pos) RESULT(val)
    !                                                   
    PROCEDURE, pass(dyn) :: POP    => DELETE_pop        ! CALL dyn%POP   ([val], [pos]) -> same as DELETE(-1)
    PROCEDURE, pass(dyn) :: DELETE => DELETE_pos        ! CALL dyn%DELETE(pos)
    !                                                     
    GENERIC ::              REMOVE => REMOVE_VAL, &     ! CALL dyn%REMOVE(val, [first], [back], [found])
                                      REMOVE_VEC        ! CALL dyn%REMOVE(vec, [first], [back], [found])
    !                                                   
    PROCEDURE, pass(dyn) ::    START => ITERATE_START   ! CALL dyn%START()
    PROCEDURE, pass(dyn) ::     NEXT => ITERATE_NEXT    ! dyn%NEXT([index], [val])   RESULT(cont)  -> cont is set to true so long as iteration is betweeen 1 and dyn%siz
    PROCEDURE, pass(dyn) :: NEXT_PTR => ITERATE_PTR     ! dyn%NEXT_PTR(ptr, [index]) RESULT(cont)  -> If cont=T, then PTR => current iterated array value
    !                                                   
    PROCEDURE, pass(dyn) ::     SORT =>    SORT_PTR     ! CALL dyn%SORT([descend])
    PROCEDURE, pass(dyn) ::  REVERSE => REVERSE_PTR     ! CALL dyn%REVERSE()
    !                                                    
    PROCEDURE, pass(dyn) ::    INDEX => INDEX_VAL       ! dyn%INDEX(val, [ipos], [back]) RESULT(pos)
    PROCEDURE, pass(dyn) ::    COUNT => COUNT_VAL       ! dyn%COUNT(val) RESULT(cnt)
    !
    PROCEDURE, pass(dyn) ::  SHIFT_RIGHT, CIRCLE_RIGHT  ! CALL dyn% SHIFT_RIGHT(shift); CALL dyn%CIRCLE_RIGHT(shift)
    PROCEDURE, pass(dyn) ::   SHIFT_LEFT, CIRCLE_LEFT   ! CALL dyn% SHIFT_LEFT (shift); CALL dyn%CIRCLE_LEFT (shift)
    !
    PROCEDURE, pass(dyn) :: PRINT  =>  PRINT_PTR        ! CALL dyn%PRINT(iu, [sep], [fmt])
    PROCEDURE, pass(dyn) :: STRING => STRING_PTR        !      dyn%STRING([sep], [fmt]) RESULT(str)
    !
    PROCEDURE, pass(dyn) :: ADD  => ADD_VAL             ! CALL dyn%ADD (val) -> dyn = dyn + val   => Element-wise
    PROCEDURE, pass(dyn) :: SUB  => SUB_VAL_RIGHT       ! CALL dyn%SUB (val) -> dyn = dyn - val
    PROCEDURE, pass(dyn) :: MULT => MLT_VAL             ! CALL dyn%MULT(val) -> dyn = dyn * val
    PROCEDURE, pass(dyn) :: MLT  => MLT_VAL             ! CALL dyn%MLT (val) -> dyn = dyn * val
    PROCEDURE, pass(dyn) :: DIV  => DIV_VAL_DENOM       ! CALL dyn%DIV (val) -> dyn = dyn / val
    PROCEDURE, pass(dyn) :: POW  => POW_VAL             ! CALL dyn%POW (val) -> dyn = dyn ^ val
    !
    PROCEDURE, pass(dyn) :: SUB_LEFT  => SUB_VAL_LEFT   ! CALL dyn%SUB_LEFT(val)  --> Solves val - dyn
    PROCEDURE, pass(dyn) :: DIV_NUMER => DIV_VAL_NUMER  ! CALL dyn%DIV_NUMER(val) --> Solves val/dyn
    !
    GENERIC:: ASSIGNMENT(= ) => EQUAL_DYN_DYN, EQUAL_DYN_VAL, EQUAL_DYN_VEC, EQUAL_VEC_DYN
    !
    GENERIC:: OPERATOR  (+ ) => ADD_DYN_DYN, ADD_DYN_VAL, ADD_VAL_DYN, ADD_DYN_VEC, ADD_VEC_DYN
    GENERIC:: OPERATOR  (- ) => SUB_DYN_DYN, SUB_DYN_VAL, SUB_VAL_DYN, SUB_DYN_VEC, SUB_VEC_DYN
    GENERIC:: OPERATOR  (* ) => MLT_DYN_DYN, MLT_DYN_VAL, MLT_VAL_DYN, MLT_DYN_VEC, MLT_VEC_DYN
    GENERIC:: OPERATOR  (/ ) => DIV_DYN_DYN, DIV_DYN_VAL, DIV_VAL_DYN, DIV_DYN_VEC, DIV_VEC_DYN
    GENERIC:: OPERATOR  (**) => POW_DYN_DYN, POW_DYN_VAL, POW_VAL_DYN, POW_DYN_VEC, POW_VEC_DYN
    GENERIC:: OPERATOR  (//) => CONCAT_DYN_DYN
    !
    GENERIC:: OPERATOR  (==) => EQ_DYN_DYN, EQ_DYN_VAL, EQ_VAL_DYN, EQ_DYN_VEC, EQ_VEC_DYN
    GENERIC:: OPERATOR  (< ) => LT_DYN_DYN, LT_DYN_VAL, LT_VAL_DYN, LT_DYN_VEC, LT_VEC_DYN
    GENERIC:: OPERATOR  (<=) => LE_DYN_DYN, LE_DYN_VAL, LE_VAL_DYN, LE_DYN_VEC, LE_VEC_DYN
    GENERIC:: OPERATOR  (> ) => GT_DYN_DYN, GT_DYN_VAL, GT_VAL_DYN, GT_DYN_VEC, GT_VEC_DYN
    GENERIC:: OPERATOR  (>=) => GE_DYN_DYN, GE_DYN_VAL, GE_VAL_DYN, GE_DYN_VEC, GE_VEC_DYN
    !
    PROCEDURE, pass(dyn), private ::  APPEND_VAL,  APPEND_VEC
    PROCEDURE, pass(dyn), private :: PREPEND_VAL, PREPEND_VEC
    PROCEDURE, pass(dyn), private ::  REMOVE_VAL,  REMOVE_VEC 
    PROCEDURE, pass(dyn), private ::     SET_POS,   SET_RANGE
    PROCEDURE, pass(dyn), private ::    INIT_VAL,    INIT_VEC
    !
    PROCEDURE, pass(dyn), private :: EQUAL_DYN_DYN, EQUAL_DYN_VAL, EQUAL_DYN_VEC, EQUAL_VEC_DYN
    !
    PROCEDURE, pass(dyn1),private :: CONCAT_DYN_DYN
    PROCEDURE, pass(dyn1),private :: ADD_DYN_DYN, ADD_DYN_VAL, ADD_VAL_DYN, ADD_DYN_VEC, ADD_VEC_DYN
    PROCEDURE, pass(dyn1),private :: SUB_DYN_DYN, SUB_DYN_VAL, SUB_VAL_DYN, SUB_DYN_VEC, SUB_VEC_DYN
    PROCEDURE, pass(dyn1),private :: MLT_DYN_DYN, MLT_DYN_VAL, MLT_VAL_DYN, MLT_DYN_VEC, MLT_VEC_DYN
    PROCEDURE, pass(dyn1),private :: DIV_DYN_DYN, DIV_DYN_VAL, DIV_VAL_DYN, DIV_DYN_VEC, DIV_VEC_DYN
    PROCEDURE, pass(dyn1),private :: POW_DYN_DYN, POW_DYN_VAL, POW_VAL_DYN, POW_DYN_VEC, POW_VEC_DYN
    !
    PROCEDURE, pass(dyn1),private :: EQ_DYN_DYN, EQ_DYN_VAL, EQ_VAL_DYN, EQ_DYN_VEC, EQ_VEC_DYN
    PROCEDURE, pass(dyn1),private :: LT_DYN_DYN, LT_DYN_VAL, LT_VAL_DYN, LT_DYN_VEC, LT_VEC_DYN
    PROCEDURE, pass(dyn1),private :: LE_DYN_DYN, LE_DYN_VAL, LE_VAL_DYN, LE_DYN_VEC, LE_VEC_DYN
    PROCEDURE, pass(dyn1),private :: GT_DYN_DYN, GT_DYN_VAL, GT_VAL_DYN, GT_DYN_VEC, GT_VEC_DYN
    PROCEDURE, pass(dyn1),private :: GE_DYN_DYN, GE_DYN_VAL, GE_VAL_DYN, GE_DYN_VEC, GE_VEC_DYN
    !
    FINAL:: FINAL_DYNAMIC_ARRAY
  END TYPE
  !
  CONTAINS
  !
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  ! 
  PURE FUNCTION SIZE_INT32(dyn) RESULT(siz)
    CLASS(DYNAMIC_ARRAY_INT32), intent(in):: dyn
    INTEGER:: siz
    siz = dyn%siz
  END FUNCTION
  !
  PURE FUNCTION CAPACITY_INT32(dyn) RESULT(cap)
    CLASS(DYNAMIC_ARRAY_INT32), intent(in):: dyn
    INTEGER:: cap
    cap = dyn%dim
  END FUNCTION
  !
  !#########################################################################################################################
  ! 
  PURE FUNCTION get_c_loc(array) RESULT(cloc)
    INTEGER(INT32), dimension(:), target, intent(in) :: array
    TYPE(C_PTR):: cloc
    cloc = C_LOC(array)
  END FUNCTION
  !
  !#########################################################################################################################
  ! 
  PURE SUBROUTINE set_ptr(dyn)
    CLASS(DYNAMIC_ARRAY_INT32), target, intent(inout) :: dyn
    IF ( dyn%siz > Z ) THEN
         dyn%ptr => dyn%array(1:dyn%siz) 
    ELSE
         NULLIFY(dyn%ptr)
    END IF
  END SUBROUTINE
  !
  !#########################################################################################################################
  ! 
  PURE FUNCTION COUNT_VAL(dyn, val) RESULT(cnt)
    CLASS(DYNAMIC_ARRAY_INT32), intent(in):: dyn
    INTEGER(INT32),             intent(in):: val
    INTEGER:: cnt
    !
    if(dyn%siz > Z) THEN
                    CALL COUNT_VAL_PASS(dyn%siz, dyn%ptr, val, cnt)
    ELSE
        cnt = Z
    END IF
  END FUNCTION
  !
  ! ------------------------------------------------------------------------------------------------------------------------
  ! 
  PURE SUBROUTINE COUNT_VAL_PASS(dim, array, val, cnt)
    INTEGER,                        intent(in ):: dim
    INTEGER(INT32), dimension(dim), intent(in ):: array
    INTEGER(INT32),                 intent(in ):: val
    INTEGER,                        intent(out):: cnt
    CNT = COUNT(array == val)
  END SUBROUTINE
  !
  !#########################################################################################################################
  ! 
  PURE FUNCTION INDEX_VAL(dyn, val, ipos, back) RESULT(pos)
    CLASS(DYNAMIC_ARRAY_INT32), intent(in):: dyn
    INTEGER(INT32),             intent(in):: val
    INTEGER,          optional, intent(in):: ipos
    LOGICAL,          optional, intent(in):: back
    LOGICAL:: from_back
    INTEGER:: pos, start
    !
    IF(dyn%siz < 1) THEN
                    pos = Z
                    RETURN
    END IF
    !
    from_back = FALSE
    IF( PRESENT(back) ) from_back = back
    !
    start = 1
    IF(     from_back ) start = dyn%siz
    IF( PRESENT(ipos) ) start = ipos
    !
    IF(       from_back .AND. start > dyn%siz ) start = dyn%siz
    IF( .NOT. from_back .AND. start < 1       ) start = 1
    !
    CALL INDEX_VAL_PASS(dyn%siz, dyn%ptr, val, start, from_back, pos)
    ! 
  END FUNCTION
  !
  ! ------------------------------------------------------------------------------------------------------------------------
  ! 
  PURE SUBROUTINE INDEX_VAL_PASS(dim, array, val, start, from_back, pos)
    INTEGER,                        intent(in ):: dim, start
    INTEGER(INT32), dimension(dim), intent(in ):: array
    INTEGER(INT32),                 intent(in ):: val
    LOGICAL,                        intent(in ):: from_back
    INTEGER,                        intent(out):: pos
    INTEGER:: I
    pos = Z
    IF(from_back) THEN
                  DO I=start, 1, -1
                      IF( array(I) == val ) THEN
                                      pos = I
                                      RETURN
                      END IF
                  END DO
    ELSE !---------------------------------------|
                  DO I=start, dim
                      IF( array(I) == val ) THEN
                                      pos = I
                                      RETURN
                      END IF
                  END DO
    END IF
  END SUBROUTINE
  !
  !#########################################################################################################################
  ! 
  PURE SUBROUTINE CLEAR_ARRAY(dyn)
    CLASS(DYNAMIC_ARRAY_INT32), intent(inout):: dyn
    !
    dyn%siz  = Z
    dyn%iter = Z
    !
    nullify(dyn%ptr)
    !
  END SUBROUTINE
  !
  ! ------------------------------------------------------------------------------------------------------------------------
  ! 
  PURE SUBROUTINE RESIZE_REFERENCE(dyn, siz)
    CLASS(DYNAMIC_ARRAY_INT32), intent(inout):: dyn
    INTEGER,                    intent(in   ):: siz
    !
    dyn%iter = Z
    !
    IF(dyn%dim < siz ) CALL GROW_DIM_INT32(dyn, siz)
    !
    dyn%siz = siz
    !
    call set_ptr(dyn)
    !
  END SUBROUTINE
  !
  ! ------------------------------------------------------------------------------------------------------------------------
  ! 
  PURE SUBROUTINE ITERATE_START(dyn, istart)
    CLASS(DYNAMIC_ARRAY_INT32), intent(inout):: dyn
    INTEGER,          optional, intent(in   ):: istart
    !
    dyn%iter = Z
    !
    IF(PRESENT(istart)) dyn%iter = istart
    !
    IF( dyn%iter < Z  ) dyn%iter = Z
    !
  END SUBROUTINE
  !
  ! ------------------------------------------------------------------------------------------------------------------------
  ! 
  FUNCTION ITERATE_NEXT(dyn, index, val) RESULT(cont)
    CLASS(DYNAMIC_ARRAY_INT32), intent(inout):: dyn
    INTEGER,          optional, intent(  out):: index
    INTEGER(INT32),   optional, intent(  out):: val
    LOGICAL:: cont
    !
    dyn%iter = dyn%iter + 1
    !
    IF( dyn%siz < dyn%iter ) THEN
        dyn%iter = Z
        cont = FALSE
        IF(PRESENT(val)  ) val   = Z
        IF(PRESENT(index)) index = dyn%iter
    ELSE
        cont = TRUE
        IF(PRESENT(val)  ) val   = dyn%array(dyn%iter) 
        IF(PRESENT(index)) index = dyn%iter
    END IF
    !
  END FUNCTION
  !
  ! ------------------------------------------------------------------------------------------------------------------------
  ! 
  FUNCTION ITERATE_PTR(dyn, ptr, index) RESULT(cont)
    CLASS(DYNAMIC_ARRAY_INT32), target, intent(inout):: dyn
    INTEGER(INT32),            pointer, intent(inout):: ptr
    INTEGER,                  optional, intent(  out):: index
    LOGICAL:: cont
    !
    dyn%iter = dyn%iter + 1
    !
    IF( dyn%siz < dyn%iter ) THEN
        dyn%iter = Z
        cont = FALSE
        NULLIFY(ptr)
        IF(PRESENT(index)) index = dyn%iter
    ELSE
        cont = TRUE
        ptr  => dyn%array(dyn%iter) 
        IF(PRESENT(index)) index = dyn%iter
    END IF
    !
  END FUNCTION
  ! 
  ! ------------------------------------------------------------------------------------------------------------------------
  ! 
  PURE SUBROUTINE FINAL_DYNAMIC_ARRAY(dyn)
    TYPE(DYNAMIC_ARRAY_INT32), intent(inout):: dyn
    !
    dyn%siz  = Z
    dyn%iter = Z
    dyn%dim  = Z
    nullify(dyn%ptr)
    IF(ALLOCATED(dyn%array)) DEALLOCATE(dyn%array)
    !
  END SUBROUTINE
  !
  !#########################################################################################################################
  ! 
  PURE SUBROUTINE INIT_VAL(dyn, DIM, val, siz)
    CLASS(DYNAMIC_ARRAY_INT32), intent(inout):: dyn
    INTEGER,          optional, intent(in   ):: DIM
    INTEGER(INT32),   optional, intent(in   ):: val
    INTEGER,          optional, intent(in   ):: siz
    !
    dyn%siz  = Z
    dyn%iter = Z
    !
    IF( PRESENT(DIM) ) THEN
                IF( dyn%dim < DIM ) CALL GROW_DIM_INT32(dyn, DIM)
    END IF
    !
    IF( PRESENT(siz) ) THEN
                IF( dyn%dim < siz ) CALL GROW_DIM_INT32(dyn, siz)
                !
                dyn%siz = siz
    END IF
    !
    IF( PRESENT(VAL) ) THEN
            IF( dyn%dim < 1 ) CALL GROW_INT32(dyn)
            dyn%array = VAL
            !
    ELSEIF( Z < dyn%dim ) THEN
                           dyn%array = Z
    ELSE
        CALL GROW_INT32(dyn)
    END IF
    !
    call set_ptr(dyn)
    !
  END SUBROUTINE
  !
  ! ------------------------------------------------------------------------------------------------------------------------
  ! 
  PURE SUBROUTINE INIT_VEC(dyn, vec, rep)
    CLASS(DYNAMIC_ARRAY_INT32),               intent(inout):: dyn
    INTEGER(INT32), dimension(:), contiguous, intent(in   ):: vec
    INTEGER,                        optional, intent(in   ):: rep
    INTEGER:: I
    !
    dyn%siz  = Z
    dyn%iter = Z
    !
    IF( PRESENT(rep) ) THEN
        !
        I = size(vec) * rep
        !
        IF( dyn%dim < I ) CALL GROW_DIM_INT32(dyn, I)
        !
        DO I=1, rep
           CALL APPEND_VEC(dyn, vec)
        END DO
    ELSE
        CALL APPEND_VEC(dyn, vec)
    END IF
    !
  END SUBROUTINE
  !
  !#########################################################################################################################
  ! 
  PURE SUBROUTINE APPEND_VAL(dyn, val, unqiue)
    CLASS(DYNAMIC_ARRAY_INT32), intent(inout):: dyn
    INTEGER(INT32),             intent(in   ):: val
    LOGICAL,          optional, intent(in   ):: unqiue
    !
    IF(PRESENT(unqiue)) THEN
            IF(unqiue .AND. dyn%siz > Z) THEN
                 IF(FINDLOC(dyn%array(1:dyn%siz), val, 1) > Z) RETURN
            END IF
    END IF
    !
    IF( dyn%siz+1 > dyn%dim ) CALL GROW_INT32(dyn)
    !
    dyn%siz = dyn%siz + 1
    !
    dyn%array(dyn%siz) = val
    !
    call set_ptr(dyn)
    !
  END SUBROUTINE
  !
  ! ------------------------------------------------------------------------------------------------------------------------
  ! 
  PURE SUBROUTINE APPEND_VEC(dyn, VEC, unqiue)
    CLASS(DYNAMIC_ARRAY_INT32),               intent(inout):: dyn
    INTEGER(INT32), dimension(:), contiguous, intent(in   ):: VEC
    LOGICAL,                        optional, intent(in   ):: unqiue
    INTEGER:: DIM
    !
    IF(PRESENT(unqiue)) THEN
            IF(unqiue) THEN
                       DO dim=1, SIZE(VEC) 
                                    CALL APPEND_VAL(dyn, VEC(dim), TRUE)
                       END DO
                       RETURN
            END IF
    END IF
    !
    DIM = size(VEC) + dyn%siz
    !
    IF( DIM > dyn%dim ) CALL GROW_DIM_INT32(dyn, DIM)
    !
    dyn%array(dyn%siz+1 : DIM) = VEC
    !
    dyn%siz = DIM
    !
    call set_ptr(dyn)
    !
  END SUBROUTINE
  !
  !#########################################################################################################################
  ! 
  PURE SUBROUTINE PREPEND_VAL(dyn, val, unqiue)
    CLASS(DYNAMIC_ARRAY_INT32), intent(inout):: dyn
    INTEGER(INT32),             intent(in   ):: val
    LOGICAL,          optional, intent(in   ):: unqiue
    !
    IF(PRESENT(unqiue)) THEN
            IF(unqiue .AND. dyn%siz > Z) THEN
                 IF(FINDLOC(dyn%array(1:dyn%siz), val, 1) > Z) RETURN
            END IF
    END IF
    !
    dyn%siz = dyn%siz + 1
    !
    IF( dyn%siz > dyn%dim ) CALL GROW_INT32(dyn)
    !
    dyn%array(2:dyn%siz) = dyn%array(1:dyn%siz-1)
    dyn%array(1) = val
    !
    call set_ptr(dyn)
    !
  END SUBROUTINE
  !
  ! ------------------------------------------------------------------------------------------------------------------------
  ! 
  PURE SUBROUTINE PREPEND_VEC(dyn, VEC, unqiue)
    CLASS(DYNAMIC_ARRAY_INT32),               intent(inout):: dyn
    INTEGER(INT32), dimension(:), contiguous, intent(in   ):: VEC
    LOGICAL,                        optional, intent(in   ):: unqiue
    INTEGER:: DIM, CNT, J
    !
    DIM = size(VEC)
    CNT = Z
    !
    IF(PRESENT(unqiue)) THEN
            IF(unqiue) THEN
                       DO J=1, DIM
                           IF( FINDLOC(dyn%array(1:dyn%siz), vec(J), 1) == Z ) CNT = CNT + 1
                       END DO
                       !
                       IF( CNT > Z ) THEN
                           CALL SHIFT_RIGHT(dyn, CNT)
                           CNT = Z
                           DO J=1, DIM
                                 IF( FINDLOC(dyn%array(1:dyn%siz), vec(J), 1) == Z ) THEN 
                                             CNT = CNT + 1
                                             dyn%array(CNT) = vec(J)
                                 END IF
                           END DO
                           RETURN
                       END IF
            END IF
    END IF
    !
    CALL SHIFT_RIGHT(dyn, DIM)
    !
    dyn%array(1 : DIM) = VEC
    !
  END SUBROUTINE
  !
  !#########################################################################################################################
  ! 
  PURE SUBROUTINE INSERT_VAL(dyn, pos, val)
    CLASS(DYNAMIC_ARRAY_INT32), intent(inout):: dyn
    INTEGER,                           value :: pos
    INTEGER(INT32),               intent(in) :: val
    !
    IF (pos == Z) pos = dyn%siz + 1
    IF (pos <  Z) pos = dyn%siz + pos + 2 ! Negative starts from end, such that -1 points to end of list
    !
    IF( pos <= Z ) RETURN ! Bad index
    !
    IF( dyn%siz + 1 == pos ) THEN
                             CALL APPEND_VAL(dyn, val)
                             RETURN
    ELSEIF( pos == 1 ) THEN
                             CALL PREPEND_VAL(dyn, val)
                             RETURN
    END IF
    !
    IF( dyn%dim < pos ) CALL GROW_DIM_INT32(dyn, pos)
    IF( dyn%siz < pos ) THEN
                        dyn%siz = dyn%siz + 1
                        DO CONCURRENT ( INTEGER:: I = dyn%siz : pos-1 )
                                                                dyn%array(I) = Z
                        END DO
                        dyn%array(pos) = Z
                        RETURN
    END IF
    !
    dyn%siz = dyn%siz + 1
    !
    IF( dyn%siz > dyn%dim ) CALL GROW_INT32(dyn)
    !
    dyn%array(pos+1:dyn%siz) = dyn%array(pos:dyn%siz-1)
    dyn%array(pos) = VAL
    !
    call set_ptr(dyn)
    !
  END SUBROUTINE
  !
  !#########################################################################################################################
  ! 
  PURE FUNCTION GET_VAL(dyn, pos) RESULT(val)
    CLASS(DYNAMIC_ARRAY_INT32), intent(in):: dyn
    INTEGER,                         value:: pos
    INTEGER(INT32):: val
    !
    IF (pos == Z) pos = dyn%siz
    IF (pos <  Z) pos = dyn%siz + pos + 1 ! Negative starts from end, such that -1 points to lst%tail
    !
    IF    ( dyn%siz < 1   ) THEN
                            val = HUGE(val)
    ELSEIF( pos < Z       ) THEN
                            val = dyn%array(1)
    ELSEIF( pos > dyn%siz ) THEN
                            val = dyn%array(dyn%siz)
    ELSE
                            val = dyn%array(pos)
    END IF
    !
  END FUNCTION
  !
  !#########################################################################################################################
  ! 
  PURE SUBROUTINE SET_POS(dyn, pos, val)
    CLASS(DYNAMIC_ARRAY_INT32), intent(inout) :: dyn
    INTEGER,                            value :: pos
    INTEGER(INT32),                intent(in) :: val
    !
    IF (pos == Z) pos = dyn%siz
    IF (pos <  Z) pos = dyn%siz + pos + 1 ! Negative starts from end, such that -1 points to lst%tail
    !
    IF( pos <= Z ) RETURN ! Bad index
    !
    IF( dyn%dim < pos ) CALL GROW_DIM_INT32(dyn, pos)
    IF( dyn%siz < pos ) THEN
                        dyn%siz = dyn%siz + 1
                        DO CONCURRENT ( INTEGER:: I = dyn%siz : pos-1 )
                                                                dyn%array(I) = Z
                        END DO
    END IF
    !
    dyn%array(pos) = val
    !
  END SUBROUTINE
  ! 
  PURE SUBROUTINE SET_RANGE(dyn, I, J, val)
    CLASS(DYNAMIC_ARRAY_INT32), intent(inout) :: dyn
    INTEGER,                       intent(in) :: I, J
    INTEGER(INT32),                intent(in) :: val
    !
    IF( I > J .OR. I < 1 .OR. J < 1 ) RETURN ! Bad index
    !
    IF( dyn%dim < J ) CALL GROW_DIM_INT32(dyn, J)
    IF( dyn%siz < J ) THEN
                        dyn%siz = dyn%siz + 1
                        DO CONCURRENT ( INTEGER:: K = dyn%siz : J-1 )
                                                                dyn%array(K) = Z
                        END DO
                        dyn%siz = J
                        call set_ptr(dyn)
    END IF
    !
    dyn%array(I:J) = val
    !
  END SUBROUTINE
  !
  !#########################################################################################################################
  ! 
  PURE SUBROUTINE DELETE_POP(dyn, val, pos)
    CLASS(DYNAMIC_ARRAY_INT32), intent(inout):: dyn
    INTEGER(INT32),   optional, intent(  out):: val
    INTEGER,          optional, intent(in   ):: pos
    INTEGER:: I, P
    !
    IF( dyn%siz < 1 ) THEN
                        IF( PRESENT(VAL) ) val = HUGE(val)
                        RETURN
    END IF
    !
    IF(PRESENT(POS)) THEN
         P = POS
         IF (P == Z) P = dyn%siz
         IF (P <  Z) P = dyn%siz + P + 1 ! Negative starts from end, such that -1 points to lst%tail
         !
         IF (P < 1    ) P = 1
         IF (P > dyn%siz) P = dyn%siz
    ELSE
         P = dyn%siz
    END IF
    !
    dyn%siz = dyn%siz + NEG
    !
    IF( PRESENT(val) ) val = dyn%array(P)
    !
    IF( P <= dyn%siz ) THEN
                       DO I=P, dyn%siz
                               dyn%array(I) = dyn%array(I+1)
                       END DO
    END IF
    !
    call set_ptr(dyn)
    !
  END SUBROUTINE
  !
  ! ------------------------------------------------------------------------------------------------------------------------
  ! 
  PURE SUBROUTINE DELETE_POS(dyn, pos)
    CLASS(DYNAMIC_ARRAY_INT32), intent(inout):: dyn
    INTEGER,                           value :: pos
    INTEGER:: I
    !
    IF( dyn%siz < 1 ) RETURN
    !
    IF (pos == Z) pos = dyn%siz
    IF (pos <  Z) pos = dyn%siz + pos + 1 ! Negative starts from end, such that -1 points to lst%tail
    !
    IF (pos < 1    ) pos = 1
    IF (pos > dyn%siz) pos = dyn%siz
    !
    dyn%siz = dyn%siz + NEG
    !
    IF( pos <= dyn%siz ) THEN
                       DO I=pos, dyn%siz
                               dyn%array(I) = dyn%array(I+1)
                       END DO
    END IF
    !
    call set_ptr(dyn)
    !
  END SUBROUTINE
  !
  !#########################################################################################################################
  ! 
  PURE SUBROUTINE REMOVE_VAL(dyn, val, first, back, found)
    CLASS(DYNAMIC_ARRAY_INT32), intent(inout):: dyn
    INTEGER(INT32),             intent(in   ):: val
    LOGICAL,          optional, intent(in   ):: first, back
    LOGICAL,          optional, intent(  out):: found
    INTEGER:: I, siz
    LOGICAL:: del_all, from_back
    !
    IF( dyn%siz < 1 ) RETURN
    !
    del_all = TRUE
    IF( PRESENT(first) ) del_all = .NOT. first
    !
    from_back = FALSE
    IF( PRESENT(back) ) from_back = back
    !
    siz = dyn%siz
    !
    IF( del_all ) THEN
                  DO I=dyn%siz, 1, NEG
                          IF( dyn%array(I) == val ) CALL DELETE_POS(dyn, I)
                  END DO
                  !
    ELSEIF( from_back ) THEN
         DO I=dyn%siz, 1, NEG
                 IF( dyn%array(I) == val ) THEN
                                           CALL DELETE_POS(dyn, I)
                                           EXIT
                 END IF
         END DO
    ELSE
         DO I=1, dyn%siz
                 IF( dyn%array(I) == val ) THEN
                                           CALL DELETE_POS(dyn, I)
                                           EXIT
                 END IF
         END DO
    END IF
    !
    IF( PRESENT(found) ) found = dyn%siz < siz
    !
    IF(dyn%siz < siz ) call set_ptr(dyn)
  END SUBROUTINE
  !
  ! ------------------------------------------------------------------------------------------------------------------------
  ! 
  PURE SUBROUTINE REMOVE_VEC(dyn, vec, first, back, found)
    CLASS(DYNAMIC_ARRAY_INT32),               intent(inout):: dyn
    INTEGER(INT32), dimension(:), contiguous, intent(in   ):: vec
    LOGICAL,                        optional, intent(in   ):: first, back
    LOGICAL,                        optional, intent(  out):: found
    INTEGER:: I, J, siz
    LOGICAL:: del_all, from_back
    !
    IF( dyn%siz < 1 ) RETURN
    !
    del_all = TRUE
    IF( PRESENT(first) ) del_all = .NOT. first
    !
    from_back = FALSE
    IF( PRESENT(back) ) from_back = back
    !
    siz = dyn%siz
    !
    IF( del_all ) THEN
                  DO J=1, SIZE(vec)
                     DO I=dyn%siz, 1, NEG
                             IF( dyn%array(I) == vec(J) ) CALL DELETE_POS(dyn, I)
                     END DO
                  END DO
                  !
    ELSEIF( from_back ) THEN
         DO J=1, SIZE(vec)
            DO I=dyn%siz, 1, NEG
                 IF( dyn%array(I) == vec(J) ) THEN
                                           CALL DELETE_POS(dyn, I)
                                           EXIT
                 END IF
            END DO
         END DO
    ELSE
         DO J=1, SIZE(vec)
            DO I=1, dyn%siz
                 IF( dyn%array(I) == vec(J) ) THEN
                                           CALL DELETE_POS(dyn, I)
                                           EXIT
                 END IF
            END DO
         END DO
    END IF
    !
    IF( PRESENT(found) ) found = dyn%siz < siz
    !
    IF(dyn%siz < siz ) call set_ptr(dyn)
  END SUBROUTINE
  !
  !#########################################################################################################################
  ! 
  PURE SUBROUTINE SHIFT_RIGHT(dyn, shift)
    CLASS(DYNAMIC_ARRAY_INT32), intent(inout):: dyn
    INTEGER,                    intent(in   ):: shift
    INTEGER:: I, siz
    !
    IF( shift < 1 ) RETURN
    !
    siz = dyn%siz + shift
    IF( dyn%dim < siz ) CALL GROW_DIM_INT32(dyn, siz)
    !
    DO I=dyn%siz, 1, NEG
                  dyn%array(I+shift) = dyn%array(I)
    END DO
    dyn%siz = siz
    dyn%array(1 : shift) = Z
    !
    call set_ptr(dyn)
    !
  END SUBROUTINE
  !
  ! ------------------------------------------------------------------------------------------------------------------------
  ! 
  PURE SUBROUTINE CIRCLE_RIGHT(dyn, shift)
    CLASS(DYNAMIC_ARRAY_INT32), intent(inout):: dyn
    INTEGER,                    intent(in   ):: shift
    INTEGER:: I, siz
    !
    IF( shift < 1 ) RETURN
    !
    siz = dyn%siz + shift
    IF( dyn%dim < siz ) CALL GROW_DIM_INT32(dyn, siz)
    !
    DO I=dyn%siz, 1, NEG
                  dyn%array(I+shift) = dyn%array(I)
    END DO
    !
    dyn%array(1 : shift) = dyn%array(dyn%siz+1 : siz)
    !
  END SUBROUTINE
  !
  !#########################################################################################################################
  ! 
  PURE SUBROUTINE SHIFT_LEFT(dyn, shift)
    CLASS(DYNAMIC_ARRAY_INT32), intent(inout):: dyn
    INTEGER,                    intent(in   ):: shift
    INTEGER:: I, siz
    !
    IF( shift < 1 ) RETURN
    !
    siz = dyn%siz - shift
    !
    DO I=1, siz
            dyn%array(I) = dyn%array(I+shift)
    END DO
    dyn%array(siz+1 : dyn%siz) = Z
    !
    dyn%siz = siz
    !
    call set_ptr(dyn)
    !
  END SUBROUTINE
  !
  ! ------------------------------------------------------------------------------------------------------------------------
  ! 
  PURE SUBROUTINE CIRCLE_LEFT(dyn, shift)
    CLASS(DYNAMIC_ARRAY_INT32), intent(inout):: dyn
    INTEGER,                    intent(in   ):: shift
    INTEGER:: siz
    !
    IF( shift < 1 ) RETURN
    !
    siz = dyn%siz + shift
    IF( dyn%dim < siz ) CALL GROW_DIM_INT32(dyn, siz)  !Use end of array as TMP storage
    !
    dyn%array(dyn%siz+1:siz) = dyn%array(1:shift)
    !
    dyn%siz = siz
    !
    CALL SHIFT_LEFT(dyn, shift)
    !
  END SUBROUTINE
  !
  !#########################################################################################################################
  ! 
  PURE SUBROUTINE GROW_DIM_INT32(dyn, DIM)
    CLASS(DYNAMIC_ARRAY_INT32), intent(inout):: dyn
    INTEGER, value:: DIM
    !
    IF ( DIM <= dyn%dim ) RETURN
    !
    IF ( DIM <  i16 ) DIM = i16
    !
    IF( dyn%dim < 1 .AND. DIM >= i4096) dyn%dim = i4096
    !
    IF( dyn%dim < 1 ) dyn%dim = i16
    !
    IF( DIM < i4096 ) THEN
                     DO WHILE (dyn%dim < DIM)
                               dyn%dim = SHIFTL(dyn%dim,1)
                     END DO
    ELSE
                     DO WHILE (dyn%dim < DIM)
                               dyn%dim = dyn%dim + i2048
                     END DO
    END IF
    !
    CALL REALLOCATE_INT32(dyn)
    !
  END SUBROUTINE
  !
  ! ------------------------------------------------------------------------------------------------------------------------
  ! 
  PURE SUBROUTINE GROW_INT32(dyn)       ! Only call to increase size and assumes that dyn%dim is a multiple of 16
    CLASS(DYNAMIC_ARRAY_INT32), intent(inout):: dyn
    !
    IF    ( dyn%dim < 1   ) THEN
                              dyn%dim = i16
    ELSEIF( dyn%dim < i4096 ) THEN
                              dyn%dim = SHIFTL(dyn%dim,1)
    ELSE
                              dyn%dim = dyn%dim + i2048
    END IF
    !
    CALL REALLOCATE_INT32(dyn)
    !
  END SUBROUTINE
  !
  ! ------------------------------------------------------------------------------------------------------------------------
  ! 
  PURE SUBROUTINE REALLOCATE_INT32(dyn)
    CLASS(DYNAMIC_ARRAY_INT32), intent(inout):: dyn
    INTEGER(INT32), dimension(:), allocatable:: tmp
    !
    IF(dyn%dim < 1) RETURN
    !
    IF( .NOT. ALLOCATED(dyn%array)    ) THEN
                                            dyn%siz = Z
                                            nullify(dyn%ptr)                 ! assumes that ptr will be set by the callling routine stack
                                            ALLOCATE(dyn%array(dyn%dim), SOURCE=Z)
    ELSEIF( SIZE(dyn%array) < dyn%dim ) THEN
                           nullify(dyn%ptr)                 ! assumes that ptr will be set by the callling routine stack
                           IF( dyn%siz < 1 ) THEN
                                               dyn%siz = Z
                                               DEALLOCATE(dyn%array)
                                                 ALLOCATE(dyn%array(dyn%dim), SOURCE=Z)
                           ELSE
                               ALLOCATE(tmp(dyn%dim), SOURCE=Z)
                               !
                               DO CONCURRENT( INTEGER:: I=1 : dyn%siz )
                                                                       TMP(I) = dyn%array(I)
                               END DO
                               !DO CONCURRENT( INTEGER:: I=dyn%siz+1 : dyn%dim )
                               !                                        TMP(I) = Z
                               !END DO
                               !
                               CALL MOVE_ALLOC(TMP, dyn%array)
                           END IF
    END IF
    !
  END SUBROUTINE
  !
  !#########################################################################################################################
  ! 
  PURE SUBROUTINE ADD_VAL(dyn, val)
    CLASS(DYNAMIC_ARRAY_INT32), intent(inout):: dyn
    INTEGER(INT32),             intent(in   ):: val
    !
    DO CONCURRENT ( INTEGER:: I=1 : dyn%siz )
                                    dyn%array(I) = dyn%array(I) + val
    END DO
    !
  END SUBROUTINE
  !
  ! ------------------------------------------------------------------------------------------------------------------------
  ! 
  PURE SUBROUTINE SUB_VAL_RIGHT(dyn, val)
    CLASS(DYNAMIC_ARRAY_INT32), intent(inout):: dyn
    INTEGER(INT32),                    value :: val
    !
    IF( val /= Z ) THEN
        val = -1_int32 * val
        DO CONCURRENT ( INTEGER:: I=1 : dyn%siz )
                                        dyn%array(I) = dyn%array(I) + val
        END DO
    END IF
    !
  END SUBROUTINE
  !
  PURE SUBROUTINE SUB_VAL_LEFT(dyn, val)
    CLASS(DYNAMIC_ARRAY_INT32), intent(inout):: dyn
    INTEGER(INT32),             intent(in   ):: val
    !
    DO CONCURRENT ( INTEGER:: I=1 : dyn%siz )
                                    dyn%array(I) = val - dyn%array(I)
    END DO
    !
  END SUBROUTINE
  !
  ! ------------------------------------------------------------------------------------------------------------------------
  ! 
  PURE SUBROUTINE MLT_VAL(dyn, val)
    CLASS(DYNAMIC_ARRAY_INT32), intent(inout):: dyn
    INTEGER(INT32),             intent(in   ):: val
    !
    DO CONCURRENT ( INTEGER:: I=1 : dyn%siz )
                                    dyn%array(I) = dyn%array(I) * val
    END DO
    !
  END SUBROUTINE
  !
  ! ------------------------------------------------------------------------------------------------------------------------
  ! 
  PURE SUBROUTINE DIV_VAL_DENOM(dyn, val)
    CLASS(DYNAMIC_ARRAY_INT32), intent(inout):: dyn
    INTEGER(INT32),             intent(in   ):: val
    !
    IF( val /= Z ) THEN
        DO CONCURRENT ( INTEGER:: I=1 : dyn%siz )
                                        dyn%array(I) = dyn%array(I) / val
        END DO
    ELSEIF( dyn%siz > Z ) THEN
            dyn%array(1:dyn%siz) = Z
    END IF
    !
  END SUBROUTINE
  !
  ! ------------------------------------------------------------------------------------------------------------------------
  ! 
  PURE SUBROUTINE DIV_VAL_NUMER(val, dyn)
    INTEGER(INT32),             intent(in   ):: val
    CLASS(DYNAMIC_ARRAY_INT32), intent(inout):: dyn
    !
    IF( val /= Z ) THEN
        DO CONCURRENT ( INTEGER:: I=1 : dyn%siz, dyn%array(I) /= Z )
                                                 dyn%array(I) =  val / dyn%array(I)
        END DO
    ELSEIF( dyn%siz > Z ) THEN
            dyn%array(1:dyn%siz) = Z
    END IF
    !
  END SUBROUTINE
  !
  ! ------------------------------------------------------------------------------------------------------------------------
  ! 
  PURE SUBROUTINE POW_VAL(dyn, val)
    CLASS(DYNAMIC_ARRAY_INT32), intent(inout):: dyn
    INTEGER(INT32),             intent(in   ):: val
    !
    DO CONCURRENT ( INTEGER:: I=1 : dyn%siz )
                                    dyn%array(I) = dyn%array(I) ** val
    END DO
    !
  END SUBROUTINE
  !
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !                            (OUT, IN)
  PURE SUBROUTINE EQUAL_DYN_DYN(dyn, dyn_in)
    CLASS(DYNAMIC_ARRAY_INT32), intent(in   ):: dyn_in
    CLASS(DYNAMIC_ARRAY_INT32), intent(inout):: dyn
    !
    IF(dyn_in%siz < 1) THEN
           dyn%siz  = Z
           dyn%iter = Z
           nullify(dyn%ptr)
           RETURN
    END IF
    !
    IF( dyn_in%siz > dyn%dim) THEN            ! OUT not big enough
                              dyn%siz = Z
                              CALL GROW_DIM_INT32(dyn, dyn_in%dim)
                              !
                              dyn%array(1:dyn_in%dim) = dyn_in%array
                              !
                              dyn%siz  = dyn_in%siz
                              dyn%iter = dyn_in%iter
                              call set_ptr(dyn)
                              RETURN
    END IF
    !
    IF( dyn%dim == dyn_in%dim ) THEN
            IF( C_ASSOCIATED(get_c_loc(dyn%array), get_c_loc(dyn_in%array)) ) THEN  ! Arrays are the same memory location
                              !
                              dyn%siz  = dyn_in%siz
                              dyn%iter = dyn_in%iter
                              call set_ptr(dyn)
                              RETURN
            END IF
    END IF
    !
    IF( dyn%dim > dyn_in%dim ) THEN
                  dyn%array(1:dyn_in%dim) = dyn_in%array(1:dyn_in%dim)
    ELSE
                  dyn%array(1:dyn_in%siz) = dyn_in%array(1:dyn_in%siz)
    END IF
    !
    dyn%siz  = dyn_in%siz
    dyn%iter = dyn_in%iter
    call set_ptr(dyn)
    !
  END SUBROUTINE
  !
  ! ------------------------------------------------------------------------------------------------------------------------
  !                            (OUT, IN)
  PURE SUBROUTINE EQUAL_DYN_VAL(dyn, val)
    INTEGER(INT32),             INTENT(IN   ):: VAL
    CLASS(DYNAMIC_ARRAY_INT32), INTENT(INOUT):: dyn
    !
    IF( dyn%siz > 0 ) dyn%array(1:dyn%siz) = VAL
    !
  END SUBROUTINE
  !
  ! ------------------------------------------------------------------------------------------------------------------------
  !                            (OUT, IN)
  PURE SUBROUTINE EQUAL_DYN_VEC(dyn, vec)
    INTEGER(INT32), dimension(:), contiguous, intent(in   ):: vec
    CLASS(DYNAMIC_ARRAY_INT32),               intent(inout):: dyn
    INTEGER:: siz
    !
    siz = SIZE(vec)
    !
    dyn%siz = Z
    !
    IF( dyn%dim < siz ) CALL GROW_DIM_INT32(dyn, siz)
    !
    dyn%siz = siz
    !
    dyn%array(1:siz) = vec
    !
    call set_ptr(dyn)
    !
  END SUBROUTINE
  !
  ! ------------------------------------------------------------------------------------------------------------------------
  !                            (OUT, IN)
  PURE SUBROUTINE EQUAL_VEC_DYN(vec, dyn)
    INTEGER(INT32), dimension(:), allocatable, intent(inout):: vec
    CLASS(DYNAMIC_ARRAY_INT32),                intent(in   ):: dyn
    INTEGER:: siz
    !
    siz = Z
    IF( ALLOCATED(vec) ) siz = SIZE(vec)
    !
    IF( siz > Z .AND. siz == dyn%siz ) THEN
                                       vec(:) = dyn%array(1:siz)
    ELSE
        IF( siz > Z ) DEALLOCATE(vec)
        !
        ALLOCATE( vec, SOURCE=dyn%array(1:siz) )
    END IF
    !
  END SUBROUTINE
  !
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !#####################################################################################################
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !
  PURE FUNCTION CONCAT_DYN_DYN(dyn1, dyn2) RESULT(dyn)
    CLASS(DYNAMIC_ARRAY_INT32), intent(in):: dyn1
    CLASS(DYNAMIC_ARRAY_INT32), intent(in):: dyn2
    TYPE(DYNAMIC_ARRAY_INT32):: dyn
    INTEGER:: siz
    siz = dyn1%siz + dyn2%siz
    !
    IF( dyn%dim < siz ) CALL GROW_DIM_INT32(dyn, siz)
    !
    dyn%siz = siz
    DO CONCURRENT( INTEGER:: I=1:dyn1%siz )
                                 dyn%array(I) = dyn1%array(I)
    END DO
    DO CONCURRENT( INTEGER:: I=dyn1%siz+1 : siz )
                                 dyn%array(I) = dyn2%array(I-dyn1%siz)
    END DO
    !
    call set_ptr(dyn)
    !
  END FUNCTION
  !
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !#####################################################################################################
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !
  PURE FUNCTION ADD_DYN_DYN(dyn1, dyn2) RESULT(dyn)
    CLASS(DYNAMIC_ARRAY_INT32), intent(in):: dyn1
    CLASS(DYNAMIC_ARRAY_INT32), intent(in):: dyn2
    TYPE(DYNAMIC_ARRAY_INT32):: dyn
    INTEGER:: siz
    siz = min( dyn1%siz, dyn2%siz )
    !
    IF( dyn%dim < siz ) CALL GROW_DIM_INT32(dyn, siz)
    !
    dyn%siz = siz
    DO CONCURRENT( INTEGER:: I=1:siz )
                                 dyn%array(I) = dyn1%array(I) + dyn2%array(I)
    END DO
    !
    call set_ptr(dyn)
    !
  END FUNCTION
  !
  PURE FUNCTION ADD_DYN_VAL(dyn1, val) RESULT(dyn)
    CLASS(DYNAMIC_ARRAY_INT32), intent(in):: dyn1
    INTEGER(INT32),             intent(in):: val
    TYPE(DYNAMIC_ARRAY_INT32):: dyn
    !
    IF( dyn%dim < dyn1%siz ) CALL GROW_DIM_INT32(dyn, dyn1%siz)
    !
    dyn%siz = dyn1%siz
    DO CONCURRENT( INTEGER:: I=1:dyn%siz )
                                 dyn%array(I) = dyn1%array(I) + val
    END DO
    !
    call set_ptr(dyn)
    !
  END FUNCTION
  !
  PURE FUNCTION ADD_VAL_DYN(val, dyn1) RESULT(dyn)
    CLASS(DYNAMIC_ARRAY_INT32), intent(in):: dyn1
    INTEGER(INT32),             intent(in):: val
    TYPE(DYNAMIC_ARRAY_INT32):: dyn
    !
    IF( dyn%dim < dyn1%siz ) CALL GROW_DIM_INT32(dyn, dyn1%siz)
    !
    dyn%siz = dyn1%siz
    DO CONCURRENT( INTEGER:: I=1:dyn%siz )
                                 dyn%array(I) = dyn1%array(I) + val
    END DO
    !
    call set_ptr(dyn)
    !
  END FUNCTION
  !
  PURE FUNCTION ADD_DYN_VEC(dyn1, vec) RESULT(dyn)
    CLASS(DYNAMIC_ARRAY_INT32),   intent(in):: dyn1
    INTEGER(INT32), dimension(:), intent(in):: vec
    TYPE(DYNAMIC_ARRAY_INT32):: dyn
    INTEGER:: siz
    siz = min( dyn1%siz, SIZE(vec) )
    !
    IF( dyn%dim < siz ) CALL GROW_DIM_INT32(dyn, siz)
    !
    dyn%siz = siz
    DO CONCURRENT( INTEGER:: I=1:siz )
                                 dyn%array(I) = dyn1%array(I) + vec(I)
    END DO
    !
    call set_ptr(dyn)
    !
  END FUNCTION
  !
  PURE FUNCTION ADD_VEC_DYN(vec, dyn1) RESULT(dyn)
    CLASS(DYNAMIC_ARRAY_INT32),   intent(in):: dyn1
    INTEGER(INT32), dimension(:), intent(in):: vec
    TYPE(DYNAMIC_ARRAY_INT32):: dyn
    INTEGER:: siz
    siz = min( dyn1%siz, SIZE(vec) )
    !
    IF( dyn%dim < siz ) CALL GROW_DIM_INT32(dyn, siz)
    !
    dyn%siz = siz
    DO CONCURRENT( INTEGER:: I=1:siz )
                                 dyn%array(I) = dyn1%array(I) + vec(I)
    END DO
    !
    call set_ptr(dyn)
    !
  END FUNCTION
  !
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !#####################################################################################################
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !
  PURE FUNCTION SUB_DYN_DYN(dyn1, dyn2) RESULT(dyn)
    CLASS(DYNAMIC_ARRAY_INT32), intent(in):: dyn1
    CLASS(DYNAMIC_ARRAY_INT32), intent(in):: dyn2
    TYPE(DYNAMIC_ARRAY_INT32):: dyn
    INTEGER:: siz
    siz = min( dyn1%siz, dyn2%siz )
    !
    IF( dyn%dim < siz ) CALL GROW_DIM_INT32(dyn, siz)
    !
    dyn%siz = siz
    DO CONCURRENT( INTEGER:: I=1:siz )
                                 dyn%array(I) = dyn1%array(I) - dyn2%array(I)
    END DO
    !
    call set_ptr(dyn)
    !
  END FUNCTION
  !
  PURE FUNCTION SUB_DYN_VAL(dyn1, val) RESULT(dyn)
    CLASS(DYNAMIC_ARRAY_INT32), intent(in):: dyn1
    INTEGER(INT32),             intent(in):: val
    TYPE(DYNAMIC_ARRAY_INT32):: dyn
    !
    IF( dyn%dim < dyn1%siz ) CALL GROW_DIM_INT32(dyn, dyn1%siz)
    !
    dyn%siz = dyn1%siz
    DO CONCURRENT( INTEGER:: I=1:dyn%siz )
                                 dyn%array(I) = dyn1%array(I) - val
    END DO
    !
    call set_ptr(dyn)
    !
  END FUNCTION
  !
  PURE FUNCTION SUB_VAL_DYN(val, dyn1) RESULT(dyn)
    CLASS(DYNAMIC_ARRAY_INT32), intent(in):: dyn1
    INTEGER(INT32),             intent(in):: val
    TYPE(DYNAMIC_ARRAY_INT32):: dyn
    !
    IF( dyn%dim < dyn1%siz ) CALL GROW_DIM_INT32(dyn, dyn1%siz)
    !
    dyn%siz = dyn1%siz
    DO CONCURRENT( INTEGER:: I=1:dyn%siz )
                                 dyn%array(I) =  val - dyn1%array(I)
    END DO
    !
    call set_ptr(dyn)
    !
  END FUNCTION
  !
  PURE FUNCTION SUB_DYN_VEC(dyn1, vec) RESULT(dyn)
    CLASS(DYNAMIC_ARRAY_INT32),   intent(in):: dyn1
    INTEGER(INT32), dimension(:), intent(in):: vec
    TYPE(DYNAMIC_ARRAY_INT32):: dyn
    INTEGER:: siz
    siz = min( dyn1%siz, SIZE(vec) )
    !
    IF( dyn%dim < siz ) CALL GROW_DIM_INT32(dyn, siz)
    !
    dyn%siz = siz
    DO CONCURRENT( INTEGER:: I=1:siz )
                                 dyn%array(I) = dyn1%array(I) - vec(I)
    END DO
    !
    call set_ptr(dyn)
    !
  END FUNCTION
  !
  PURE FUNCTION SUB_VEC_DYN(vec, dyn1) RESULT(dyn)
    CLASS(DYNAMIC_ARRAY_INT32),   intent(in):: dyn1
    INTEGER(INT32), dimension(:), intent(in):: vec
    TYPE(DYNAMIC_ARRAY_INT32):: dyn
    INTEGER:: siz
    siz = min( dyn1%siz, SIZE(vec) )
    !
    IF( dyn%dim < siz ) CALL GROW_DIM_INT32(dyn, siz)
    !
    dyn%siz = siz
    DO CONCURRENT( INTEGER:: I=1:siz )
                                 dyn%array(I) =  vec(I) - dyn1%array(I)
    END DO
    !
    call set_ptr(dyn)
    !
  END FUNCTION
  !
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !#####################################################################################################
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !
  PURE FUNCTION MLT_DYN_DYN(dyn1, dyn2) RESULT(dyn)
    CLASS(DYNAMIC_ARRAY_INT32), intent(in):: dyn1
    CLASS(DYNAMIC_ARRAY_INT32), intent(in):: dyn2
    TYPE(DYNAMIC_ARRAY_INT32):: dyn
    INTEGER:: siz
    siz = min( dyn1%siz, dyn2%siz )
    !
    IF( dyn%dim < siz ) CALL GROW_DIM_INT32(dyn, siz)
    !
    dyn%siz = siz
    DO CONCURRENT( INTEGER:: I=1:siz )
                                 dyn%array(I) = dyn1%array(I) * dyn2%array(I)
    END DO
    !
    call set_ptr(dyn)
    !
  END FUNCTION
  !
  PURE FUNCTION MLT_DYN_VAL(dyn1, val) RESULT(dyn)
    CLASS(DYNAMIC_ARRAY_INT32), intent(in):: dyn1
    INTEGER(INT32),             intent(in):: val
    TYPE(DYNAMIC_ARRAY_INT32):: dyn
    !
    IF( dyn%dim < dyn1%siz ) CALL GROW_DIM_INT32(dyn, dyn1%siz)
    !
    dyn%siz = dyn1%siz
    DO CONCURRENT( INTEGER:: I=1:dyn%siz )
                                 dyn%array(I) = dyn1%array(I) * val
    END DO
    !
    call set_ptr(dyn)
    !
  END FUNCTION
  !
  PURE FUNCTION MLT_VAL_DYN(val, dyn1) RESULT(dyn)
    CLASS(DYNAMIC_ARRAY_INT32), intent(in):: dyn1
    INTEGER(INT32),             intent(in):: val
    TYPE(DYNAMIC_ARRAY_INT32):: dyn
    !
    IF( dyn%dim < dyn1%siz ) CALL GROW_DIM_INT32(dyn, dyn1%siz)
    !
    dyn%siz = dyn1%siz
    DO CONCURRENT( INTEGER:: I=1:dyn%siz )
                                 dyn%array(I) = dyn1%array(I) * val
    END DO
    !
    call set_ptr(dyn)
    !
  END FUNCTION
  !
  PURE FUNCTION MLT_DYN_VEC(dyn1, vec) RESULT(dyn)
    CLASS(DYNAMIC_ARRAY_INT32),   intent(in):: dyn1
    INTEGER(INT32), dimension(:), intent(in):: vec
    TYPE(DYNAMIC_ARRAY_INT32):: dyn
    INTEGER:: siz
    siz = min( dyn1%siz, SIZE(vec) )
    !
    IF( dyn%dim < siz ) CALL GROW_DIM_INT32(dyn, siz)
    !
    dyn%siz = siz
    DO CONCURRENT( INTEGER:: I=1:siz )
                                 dyn%array(I) = dyn1%array(I) * vec(I)
    END DO
    !
    call set_ptr(dyn)
    !
  END FUNCTION
  !
  PURE FUNCTION MLT_VEC_DYN(vec, dyn1) RESULT(dyn)
    CLASS(DYNAMIC_ARRAY_INT32),   intent(in):: dyn1
    INTEGER(INT32), dimension(:), intent(in):: vec
    TYPE(DYNAMIC_ARRAY_INT32):: dyn
    INTEGER:: siz
    siz = min( dyn1%siz, SIZE(vec) )
    !
    IF( dyn%dim < siz ) CALL GROW_DIM_INT32(dyn, siz)
    !
    dyn%siz = siz
    DO CONCURRENT( INTEGER:: I=1:siz )
                                 dyn%array(I) = dyn1%array(I) * vec(I)
    END DO
    !
    call set_ptr(dyn)
    !
  END FUNCTION
  !
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !#####################################################################################################
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !
  PURE FUNCTION DIV_DYN_DYN(dyn1, dyn2) RESULT(dyn)
    CLASS(DYNAMIC_ARRAY_INT32), intent(in):: dyn1
    CLASS(DYNAMIC_ARRAY_INT32), intent(in):: dyn2
    TYPE(DYNAMIC_ARRAY_INT32):: dyn
    INTEGER:: siz
    siz = min( dyn1%siz, dyn2%siz )
    !
    IF( dyn%dim < siz ) CALL GROW_DIM_INT32(dyn, siz)
    !
    dyn%siz = siz
    DO CONCURRENT( INTEGER:: I=1:siz )
                             IF( dyn2%array(I) /= Z ) THEN
                                  dyn%array(I) = dyn1%array(I) / dyn2%array(I)
                             ELSE
                                  dyn%array(I) = Z
                             END IF
    END DO
    !
    call set_ptr(dyn)
    !
  END FUNCTION
  !
  PURE FUNCTION DIV_DYN_VAL(dyn1, val) RESULT(dyn)
    CLASS(DYNAMIC_ARRAY_INT32), intent(in):: dyn1
    INTEGER(INT32),             intent(in):: val
    TYPE(DYNAMIC_ARRAY_INT32):: dyn
    !
    IF( dyn%dim < dyn1%siz ) CALL GROW_DIM_INT32(dyn, dyn1%siz)
    !
    dyn%siz = dyn1%siz
    IF( val /= Z ) THEN
        DO CONCURRENT( INTEGER:: I=1:dyn%siz )
                                     dyn%array(I) = dyn1%array(I) / val       
        END DO
    ELSEIF(dyn%siz > Z) THEN
        dyn%array(1:dyn%siz) = Z
    END IF
    !
    call set_ptr(dyn)
    !
  END FUNCTION
  !
  PURE FUNCTION DIV_VAL_DYN(val, dyn1) RESULT(dyn)
    CLASS(DYNAMIC_ARRAY_INT32), intent(in):: dyn1
    INTEGER(INT32),             intent(in):: val
    TYPE(DYNAMIC_ARRAY_INT32):: dyn
    !
    IF( dyn%dim < dyn1%siz ) CALL GROW_DIM_INT32(dyn, dyn1%siz)
    !
    dyn%siz = dyn1%siz
    IF( val /= Z ) THEN
        DO CONCURRENT( INTEGER:: I=1:dyn%siz )
                             IF( dyn1%array(I) /= Z ) THEN
                                  dyn%array(I) = val / dyn1%array(I)
                             ELSE
                                  dyn%array(I) = Z
                             END IF      
        END DO
    ELSEIF(dyn%siz > Z) THEN
        dyn%array(1:dyn%siz) = Z
    END IF
    !
    call set_ptr(dyn)
    !
  END FUNCTION
  !
  PURE FUNCTION DIV_DYN_VEC(dyn1, vec) RESULT(dyn)
    CLASS(DYNAMIC_ARRAY_INT32),   intent(in):: dyn1
    INTEGER(INT32), dimension(:), intent(in):: vec
    TYPE(DYNAMIC_ARRAY_INT32):: dyn
    INTEGER:: siz
    siz = min( dyn1%siz, SIZE(vec) )
    !
    IF( dyn%dim < siz ) CALL GROW_DIM_INT32(dyn, siz)
    !
    dyn%siz = siz
    DO CONCURRENT( INTEGER:: I=1:siz )
                             IF( vec(I) /= Z ) THEN
                                  dyn%array(I) = dyn1%array(I) / vec(I)
                             ELSE
                                  dyn%array(I) = Z
                             END IF
    END DO
    !
    call set_ptr(dyn)
    !
  END FUNCTION
  !
  PURE FUNCTION DIV_VEC_DYN(vec, dyn1) RESULT(dyn)
    CLASS(DYNAMIC_ARRAY_INT32),   intent(in):: dyn1
    INTEGER(INT32), dimension(:), intent(in):: vec
    TYPE(DYNAMIC_ARRAY_INT32):: dyn
    INTEGER:: siz
    siz = min( dyn1%siz, SIZE(vec) )
    !
    IF( dyn%dim < siz ) CALL GROW_DIM_INT32(dyn, siz)
    !
    dyn%siz = siz
    DO CONCURRENT( INTEGER:: I=1:siz, dyn1%array(I) /= Z )
                             dyn%array(I) =  vec(I) / dyn1%array(I)
    END DO
    !
    call set_ptr(dyn)
    !
  END FUNCTION
  !
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !#####################################################################################################
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !
  PURE FUNCTION POW_DYN_DYN(dyn1, dyn2) RESULT(dyn)
    CLASS(DYNAMIC_ARRAY_INT32), intent(in):: dyn1
    CLASS(DYNAMIC_ARRAY_INT32), intent(in):: dyn2
    TYPE(DYNAMIC_ARRAY_INT32):: dyn
    INTEGER:: siz
    siz = min( dyn1%siz, dyn2%siz )
    !
    IF( dyn%dim < siz ) CALL GROW_DIM_INT32(dyn, siz)
    !
    dyn%siz = siz
    DO CONCURRENT( INTEGER:: I=1:siz )
                                 dyn%array(I) = dyn1%array(I) ** dyn2%array(I)
    END DO
    !
    call set_ptr(dyn)
    !
  END FUNCTION
  !
  PURE FUNCTION POW_DYN_VAL(dyn1, val) RESULT(dyn)
    CLASS(DYNAMIC_ARRAY_INT32), intent(in):: dyn1
    INTEGER(INT32),             intent(in):: val
    TYPE(DYNAMIC_ARRAY_INT32):: dyn
    !
    IF( dyn%dim < dyn1%siz ) CALL GROW_DIM_INT32(dyn, dyn1%siz)
    !
    dyn%siz = dyn1%siz
    DO CONCURRENT( INTEGER:: I=1:dyn%siz )
                                 dyn%array(I) = dyn1%array(I) ** val
    END DO
    !
    call set_ptr(dyn)
    !
  END FUNCTION
  !
  PURE FUNCTION POW_VAL_DYN(val, dyn1) RESULT(dyn)
    CLASS(DYNAMIC_ARRAY_INT32), intent(in):: dyn1
    INTEGER(INT32),             intent(in):: val
    TYPE(DYNAMIC_ARRAY_INT32):: dyn
    !
    IF( dyn%dim < dyn1%siz ) CALL GROW_DIM_INT32(dyn, dyn1%siz)
    !
    dyn%siz = dyn1%siz
    DO CONCURRENT( INTEGER:: I=1:dyn%siz )
                                 dyn%array(I) =  val ** dyn1%array(I)
    END DO
    !
    call set_ptr(dyn)
    !
  END FUNCTION
  !
  PURE FUNCTION POW_DYN_VEC(dyn1, vec) RESULT(dyn)
    CLASS(DYNAMIC_ARRAY_INT32),   intent(in):: dyn1
    INTEGER(INT32), dimension(:), intent(in):: vec
    TYPE(DYNAMIC_ARRAY_INT32):: dyn
    INTEGER:: siz
    siz = min( dyn1%siz, SIZE(vec) )
    !
    IF( dyn%dim < siz ) CALL GROW_DIM_INT32(dyn, siz)
    !
    dyn%siz = siz
    DO CONCURRENT( INTEGER:: I=1:siz )
                                 dyn%array(I) = dyn1%array(I) ** vec(I)
    END DO
    !
    call set_ptr(dyn)
    !
  END FUNCTION
  !
  PURE FUNCTION POW_VEC_DYN(vec, dyn1) RESULT(dyn)
    CLASS(DYNAMIC_ARRAY_INT32),   intent(in):: dyn1
    INTEGER(INT32), dimension(:), intent(in):: vec
    TYPE(DYNAMIC_ARRAY_INT32):: dyn
    INTEGER:: siz
    siz = min( dyn1%siz, SIZE(vec) )
    !
    IF( dyn%dim < siz ) CALL GROW_DIM_INT32(dyn, siz)
    !
    dyn%siz = siz
    DO CONCURRENT( INTEGER:: I=1:siz )
                                 dyn%array(I) =  vec(I) ** dyn1%array(I)
    END DO
    !
    call set_ptr(dyn)
    !
  END FUNCTION
  !
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !#####################################################################################################
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !
  PURE FUNCTION EQ_DYN_DYN(dyn1, dyn2) RESULT(ANS)
    CLASS(DYNAMIC_ARRAY_INT32), intent(in):: dyn1
    CLASS(DYNAMIC_ARRAY_INT32), intent(in):: dyn2
    LOGICAL:: ANS
    !
    ANS = dyn1%siz > Z .AND. dyn1%siz == dyn2%siz
    !
    IF(ANS) ANS = ALL( dyn1%array(1:dyn1%siz) == dyn2%array(1:dyn1%siz) )
    !
  END FUNCTION
  !
  PURE FUNCTION EQ_DYN_VAL(dyn1, val) RESULT(ANS)
    CLASS(DYNAMIC_ARRAY_INT32), intent(in):: dyn1
    INTEGER(INT32),             intent(in):: val
    LOGICAL:: ANS
    !
    ANS = dyn1%siz > Z
    !
    IF(ANS) ANS = ALL( dyn1%array(1:dyn1%siz) == val )
    !
  END FUNCTION
  !
  PURE FUNCTION EQ_VAL_DYN(val, dyn1) RESULT(ANS)
    CLASS(DYNAMIC_ARRAY_INT32), intent(in):: dyn1
    INTEGER(INT32),             intent(in):: val
    LOGICAL:: ANS
    !
    ANS = EQ_DYN_VAL(dyn1, val)
    !
  END FUNCTION
  !
  PURE FUNCTION EQ_DYN_VEC(dyn1, vec) RESULT(ANS)
    CLASS(DYNAMIC_ARRAY_INT32),   intent(in):: dyn1
    INTEGER(INT32), dimension(:), intent(in):: vec
    LOGICAL:: ANS
    !
    ANS = dyn1%siz > Z .AND. dyn1%siz == SIZE(vec)
    !
    IF(ANS) ANS = ALL( dyn1%array(1:dyn1%siz) == vec )
    !
  END FUNCTION
  !
  PURE FUNCTION EQ_VEC_DYN(vec, dyn1) RESULT(ANS)
    CLASS(DYNAMIC_ARRAY_INT32),   intent(in):: dyn1
    INTEGER(INT32), dimension(:), intent(in):: vec
    LOGICAL:: ANS
    !
    ANS = EQ_DYN_VEC(dyn1, vec)
    !
  END FUNCTION
  !
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !#####################################################################################################
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !
  PURE FUNCTION LT_DYN_DYN(dyn1, dyn2) RESULT(ANS)
    CLASS(DYNAMIC_ARRAY_INT32), intent(in):: dyn1
    CLASS(DYNAMIC_ARRAY_INT32), intent(in):: dyn2
    LOGICAL:: ANS
    !
    ANS = dyn1%siz > Z .AND. dyn1%siz == dyn2%siz
    !
    IF(ANS) ANS = ALL( dyn1%array(1:dyn1%siz) < dyn2%array(1:dyn1%siz) )
    !
  END FUNCTION
  !
  PURE FUNCTION LT_DYN_VAL(dyn1, val) RESULT(ANS)
    CLASS(DYNAMIC_ARRAY_INT32), intent(in):: dyn1
    INTEGER(INT32),             intent(in):: val
    LOGICAL:: ANS
    !
    ANS = dyn1%siz > Z
    !
    IF(ANS) ANS = ALL( dyn1%array(1:dyn1%siz) < val )
    !
  END FUNCTION
  !
  PURE FUNCTION LT_VAL_DYN(val, dyn1) RESULT(ANS)
    CLASS(DYNAMIC_ARRAY_INT32), intent(in):: dyn1
    INTEGER(INT32),             intent(in):: val
    LOGICAL:: ANS
    !
    ANS = dyn1%siz > Z
    !
    IF(ANS) ANS = ALL( val < dyn1%array(1:dyn1%siz) )
    !
  END FUNCTION
  !
  PURE FUNCTION LT_DYN_VEC(dyn1, vec) RESULT(ANS)
    CLASS(DYNAMIC_ARRAY_INT32),   intent(in):: dyn1
    INTEGER(INT32), dimension(:), intent(in):: vec
    LOGICAL:: ANS
    !
    ANS = dyn1%siz > Z .AND. dyn1%siz == SIZE(vec)
    !
    IF(ANS) ANS = ALL( dyn1%array(1:dyn1%siz) < vec )
    !
  END FUNCTION
  !
  PURE FUNCTION LT_VEC_DYN(vec, dyn1) RESULT(ANS)
    CLASS(DYNAMIC_ARRAY_INT32),   intent(in):: dyn1
    INTEGER(INT32), dimension(:), intent(in):: vec
    LOGICAL:: ANS
    !
    ANS = dyn1%siz > Z .AND. dyn1%siz == SIZE(vec)
    !
    IF(ANS) ANS = ALL( vec < dyn1%array(1:dyn1%siz) )
    !
  END FUNCTION
  !
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !#####################################################################################################
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !
  PURE FUNCTION LE_DYN_DYN(dyn1, dyn2) RESULT(ANS)
    CLASS(DYNAMIC_ARRAY_INT32), intent(in):: dyn1
    CLASS(DYNAMIC_ARRAY_INT32), intent(in):: dyn2
    LOGICAL:: ANS
    !
    ANS = dyn1%siz > Z .AND. dyn1%siz == dyn2%siz
    !
    IF(ANS) ANS = ALL( dyn1%array(1:dyn1%siz) <= dyn2%array(1:dyn1%siz) )
    !
  END FUNCTION
  !
  PURE FUNCTION LE_DYN_VAL(dyn1, val) RESULT(ANS)
    CLASS(DYNAMIC_ARRAY_INT32), intent(in):: dyn1
    INTEGER(INT32),             intent(in):: val
    LOGICAL:: ANS
    !
    ANS = dyn1%siz > Z
    !
    IF(ANS) ANS = ALL( dyn1%array(1:dyn1%siz) <= val )
    !
  END FUNCTION
  !
  PURE FUNCTION LE_VAL_DYN(val, dyn1) RESULT(ANS)
    CLASS(DYNAMIC_ARRAY_INT32), intent(in):: dyn1
    INTEGER(INT32),             intent(in):: val
    LOGICAL:: ANS
    !
    ANS = dyn1%siz > Z
    !
    IF(ANS) ANS = ALL( val <= dyn1%array(1:dyn1%siz) )
    !
  END FUNCTION
  !
  PURE FUNCTION LE_DYN_VEC(dyn1, vec) RESULT(ANS)
    CLASS(DYNAMIC_ARRAY_INT32),   intent(in):: dyn1
    INTEGER(INT32), dimension(:), intent(in):: vec
    LOGICAL:: ANS
    !
    ANS = dyn1%siz > Z .AND. dyn1%siz == SIZE(vec)
    !
    IF(ANS) ANS = ALL( dyn1%array(1:dyn1%siz) <= vec )
    !
  END FUNCTION
  !
  PURE FUNCTION LE_VEC_DYN(vec, dyn1) RESULT(ANS)
    CLASS(DYNAMIC_ARRAY_INT32),   intent(in):: dyn1
    INTEGER(INT32), dimension(:), intent(in):: vec
    LOGICAL:: ANS
    !
    ANS = dyn1%siz > Z .AND. dyn1%siz == SIZE(vec)
    !
    IF(ANS) ANS = ALL( vec <= dyn1%array(1:dyn1%siz) )
    !
  END FUNCTION
  !
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !#####################################################################################################
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !
  PURE FUNCTION GT_DYN_DYN(dyn1, dyn2) RESULT(ANS)
    CLASS(DYNAMIC_ARRAY_INT32), intent(in):: dyn1
    CLASS(DYNAMIC_ARRAY_INT32), intent(in):: dyn2
    LOGICAL:: ANS
    !
    ANS = dyn1%siz > Z .AND. dyn1%siz == dyn2%siz
    !
    IF(ANS) ANS = ALL( dyn1%array(1:dyn1%siz) > dyn2%array(1:dyn1%siz) )
    !
  END FUNCTION
  !
  PURE FUNCTION GT_DYN_VAL(dyn1, val) RESULT(ANS)
    CLASS(DYNAMIC_ARRAY_INT32), intent(in):: dyn1
    INTEGER(INT32),             intent(in):: val
    LOGICAL:: ANS
    !
    ANS = dyn1%siz > Z
    !
    IF(ANS) ANS = ALL( dyn1%array(1:dyn1%siz) > val )
    !
  END FUNCTION
  !
  PURE FUNCTION GT_VAL_DYN(val, dyn1) RESULT(ANS)
    CLASS(DYNAMIC_ARRAY_INT32), intent(in):: dyn1
    INTEGER(INT32),             intent(in):: val
    LOGICAL:: ANS
    !
    ANS = dyn1%siz > Z
    !
    IF(ANS) ANS = ALL( val > dyn1%array(1:dyn1%siz) )
    !
  END FUNCTION
  !
  PURE FUNCTION GT_DYN_VEC(dyn1, vec) RESULT(ANS)
    CLASS(DYNAMIC_ARRAY_INT32),   intent(in):: dyn1
    INTEGER(INT32), dimension(:), intent(in):: vec
    LOGICAL:: ANS
    !
    ANS = dyn1%siz > Z .AND. dyn1%siz == SIZE(vec)
    !
    IF(ANS) ANS = ALL( dyn1%array(1:dyn1%siz) > vec )
    !
  END FUNCTION
  !
  PURE FUNCTION GT_VEC_DYN(vec, dyn1) RESULT(ANS)
    CLASS(DYNAMIC_ARRAY_INT32),   intent(in):: dyn1
    INTEGER(INT32), dimension(:), intent(in):: vec
    LOGICAL:: ANS
    !
    ANS = dyn1%siz > Z .AND. dyn1%siz == SIZE(vec)
    !
    IF(ANS) ANS = ALL( vec > dyn1%array(1:dyn1%siz) )
    !
  END FUNCTION
  !
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !#####################################################################################################
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !
  PURE FUNCTION GE_DYN_DYN(dyn1, dyn2) RESULT(ANS)
    CLASS(DYNAMIC_ARRAY_INT32), intent(in):: dyn1
    CLASS(DYNAMIC_ARRAY_INT32), intent(in):: dyn2
    LOGICAL:: ANS
    !
    ANS = dyn1%siz > Z .AND. dyn1%siz == dyn2%siz
    !
    IF(ANS) ANS = ALL( dyn1%array(1:dyn1%siz) >= dyn2%array(1:dyn1%siz) )
    !
  END FUNCTION
  !
  PURE FUNCTION GE_DYN_VAL(dyn1, val) RESULT(ANS)
    CLASS(DYNAMIC_ARRAY_INT32), intent(in):: dyn1
    INTEGER(INT32),             intent(in):: val
    LOGICAL:: ANS
    !
    ANS = dyn1%siz > Z
    !
    IF(ANS) ANS = ALL( dyn1%array(1:dyn1%siz) >= val )
    !
  END FUNCTION
  !
  PURE FUNCTION GE_VAL_DYN(val, dyn1) RESULT(ANS)
    CLASS(DYNAMIC_ARRAY_INT32), intent(in):: dyn1
    INTEGER(INT32),             intent(in):: val
    LOGICAL:: ANS
    !
    ANS = dyn1%siz > Z
    !
    IF(ANS) ANS = ALL( val >= dyn1%array(1:dyn1%siz) )
    !
  END FUNCTION
  !
  PURE FUNCTION GE_DYN_VEC(dyn1, vec) RESULT(ANS)
    CLASS(DYNAMIC_ARRAY_INT32),   intent(in):: dyn1
    INTEGER(INT32), dimension(:), intent(in):: vec
    LOGICAL:: ANS
    !
    ANS = dyn1%siz > Z .AND. dyn1%siz == SIZE(vec)
    !
    IF(ANS) ANS = ALL( dyn1%array(1:dyn1%siz) >= vec )
    !
  END FUNCTION
  !
  PURE FUNCTION GE_VEC_DYN(vec, dyn1) RESULT(ANS)
    CLASS(DYNAMIC_ARRAY_INT32),   intent(in):: dyn1
    INTEGER(INT32), dimension(:), intent(in):: vec
    LOGICAL:: ANS
    !
    ANS = dyn1%siz > Z .AND. dyn1%siz == SIZE(vec)
    !
    IF(ANS) ANS = ALL( vec >= dyn1%array(1:dyn1%siz) )
    !
  END FUNCTION
  !
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !  
  !
  PURE SUBROUTINE SORT_PTR(dyn, descend)
    CLASS(DYNAMIC_ARRAY_INT32), intent(inout):: dyn
    LOGICAL,          optional, intent(in   ):: descend
    !
    IF(dyn%siz < 1 ) RETURN
    !
    IF(PRESENT(DESCEND)) THEN
            IF(DESCEND)  CALL SIGN_FLIP(dyn%siz, dyn%ptr)
    END IF
    !
    !
    CALL QSORT_1D_INT32(dyn%siz, dyn%ptr, 1, dyn%siz, MAX_RECUR(dyn%siz))
    !
    !
    IF(PRESENT(DESCEND)) THEN
            IF(DESCEND)  CALL SIGN_FLIP(dyn%siz, dyn%ptr)
    END IF
    !
  END SUBROUTINE
  !
  !--------------------------------------------------------------------------------------------------------------------------
  !  Introsort / introspective sort based off of -> MODULE SORT_INTERFACE (sort_interface_int32.f90)
  !
  RECURSIVE PURE SUBROUTINE QSORT_1D_INT32(DIM, A, L, R, D)
    INTEGER,                        INTENT(IN   ):: DIM
    INTEGER(INT32), DIMENSION(DIM), INTENT(INOUT):: A
    INTEGER,                        INTENT(IN   ):: L, R   ! Lower Bound, Upper Bound -> Starting left index and right index
    INTEGER,                        INTENT(IN   ):: D      ! Depth recursion limit, once zero switch to Heap Sort
    INTEGER(INT32):: TMP, P, Q       ! Pivot Values
    INTEGER:: I, J, K
    !
    K = R - L + 1                        ! Length of QSORT Partition
    IF( K  < 2 ) RETURN                  ! No Partition -> Return
    IF( K == 2 ) THEN                    ! Simple Flip Check
                 IF( A(L) > A(R) ) THEN
                                   TMP = A(L);  A(L) = A(R);  A(R) = TMP   !Swap A(L) and A(R)
                 END IF
                 RETURN
    END IF
    !
    IF( K <= 16 ) THEN   ! => Do Insertion Sort and Return   -- Side note: (R - L + 1 <= 16)
       DO I=L+1, R
           TMP = A(I)
           J   = I - 1
           DO WHILE ( J >= 1 )
                      IF( TMP >= A(J) ) EXIT
                      A(J+1) = A(J)
                      J = J - 1
           END DO
           A(J+1) = TMP
       END DO
       RETURN
    END IF
    !
    IF( D < 1 ) THEN   ! No more recursion allowed, switch to Heap Sort
       CALL HEAPSORT_1D_INT32( K, A(L:R) )
       RETURN
    END IF
    !
    !  A(L) => Pivot 1 -> Lower Branch
    !  A(R) => Pivot 2 -> Upper Branch
    !
    IF( A(L) > A(R) ) THEN
                      TMP = A(L);  A(L) = A(R);  A(R) = TMP   !Swap A(L) and A(R)
    END IF
    !
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ! Partition Code
    ! 
    P = A(L)
    Q = A(R)
    I = L + 1
    J = R - 1
    !
    ! Pivolt 1 and 2   -> A(L), A(R)
    ! Partion Location -> A(I), A(J)         => L < I < J < R
    !
    K = I
    DO WHILE ( K <= J )         
        !
        IF    ( A(K)  < P ) THEN  ! Less than the left pivot 
                            TMP = A(K);  A(K) = A(I);  A(I) = TMP   ! Swap A(K) and A(I)
                            I = I + 1
        ELSEIF( A(K) >= Q ) THEN  ! Greater than the right pivot 
                            DO WHILE ( K < J .AND. A(J) > Q )
                               J = J - 1
                            END DO
                            TMP = A(K);  A(K) = A(J);  A(J) = TMP   ! Swap A(K) and A(J)
                            J = J - 1
                            !
                            IF ( A(K) < P ) THEN  ! Less than the left pivot 
                                            TMP = A(K);  A(K) = A(I);  A(I) = TMP   ! Swap A(K) and A(I)
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
    TMP = A(L);  A(L) = A(I);  A(I) = TMP   !Swap A(L) and A(I)
    TMP = A(J);  A(J) = A(R);  A(R) = TMP   !Swap A(J) and A(R)
    !
    ! End Partition Code
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    !
    !  A(I) => Pivot 1 -> Lower Branch
    !  A(J) => Pivot 2 -> Upper Branch
    !
    CALL QSORT_1D_INT32(DIM, A, L  , I-1, D-1) 
    CALL QSORT_1D_INT32(DIM, A, I+1, J-1, D-1) 
    CALL QSORT_1D_INT32(DIM, A, J+1,   R, D-1) 
    !
  END SUBROUTINE
  !
  !##########################################################################################################################
  ! HEAPSORT base routines for when QSORT has too many recursive calls
  !
  PURE SUBROUTINE HEAPSORT_1D_INT32(DIM, A)
    INTEGER,                        INTENT(IN   ):: DIM
    INTEGER(INT32), DIMENSION(DIM), INTENT(INOUT):: A
    INTEGER(INT32):: TMP
    INTEGER:: I, P, C
    !
    HEAPIFY: DO I=DIM/2, 1, -1
       P = I    ! Parent
       C = 2*I  ! Child
       DO WHILE ( C <= DIM )   ! Siftdown the heap
           !
           IF( C < DIM) THEN
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
    POPPER: DO I=DIM-1, 1, -1
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
  ! Function that returns the max depth/recursion level that QSORT allows
  !
  PURE FUNCTION MAX_RECUR(DIM) RESULT(MX)
    INTEGER, VALUE:: DIM
    INTEGER:: MX
    !
    MX = 0
    DO WHILE (DIM > 0)
              DIM = SHIFTR(DIM, 1)
              MX  = MX + 1
    END DO
    MX = MX * 2  ! ~= 2*ceil(log2(DIM+1)) -> Initial guess
    !
    IF(MX < 64   )  MX = 64
    IF(MX > 10240)  MX = 10240
    !
  END FUNCTION
  !
  !--------------------------------------------------------------------------------------------------------------------------
  ! Sign flip used for MAGNITUDE routines to flip direction of sort
  !
  PURE SUBROUTINE SIGN_FLIP(DIM,A)
    INTEGER,                       INTENT(IN   ):: DIM
    INTEGER(INT32), DIMENSION(DIM),INTENT(INOUT):: A
    INTEGER:: I
    DO CONCURRENT (I=1:DIM)
                           A(I) = NEG * A(I)
    END DO
  END SUBROUTINE
  !
  !
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !
  ! Reverse Subroutine
  !
  PURE SUBROUTINE REVERSE_PTR(dyn)
    CLASS(DYNAMIC_ARRAY_INT32), intent(inout):: dyn
    !
    IF(dyn%siz > Z) CALL REVERSE_ORDER_1D_INT32(dyn%siz, dyn%ptr)
    !
  END SUBROUTINE
  !
  PURE SUBROUTINE REVERSE_ORDER_1D_INT32(DIM,A)
    INTEGER,                        INTENT(IN   ):: DIM
    INTEGER(INT32), DIMENSION(DIM), INTENT(INOUT):: A
    INTEGER(INT32):: TMP
    INTEGER:: I, J, HALF_DIM
    !
    IF(DIM > 1) THEN
       !
       J = DIM
       HALF_DIM = DIM/2
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
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  ! 
  ! Only specify SEP or FMT and not both
  !     SEP =   "COMMA"  -> Comma delimited 
  !             "SPACE"  -> Space delimited  
  !             "TAB"    -> Tab delimited
  !             "LINE"   -> NewLine delimited
  ! 
  ! fmt sets spacing and delimiter. 
  ! It should be set as 
  !   '(Iw, :D)' 
  !             where w is the number width and D is the delimiter.
  !                   w>0 indicates maximum width so w=4 will print "12  ", "123 ", "1234", but  12345 will print "****"
  !                   w=0 will autosize the exact size    such that "12",   "123",  "1234", and "12345"
  !
  ! fmt = '(I0, :", ")'             -> Comma delimited (default)            => Set to '(I0, :",")' for no space between commas
  ! fmt = '(I0, :1x)'               -> Space delimited                      => Set to '(I0, :3x)'  for three spaces instead of 1
  ! fmt = '(I0, :/)'                -> NewLine delimited
  ! fmt = '(I0, :"'//ACHAR(9)//'")' -> Tab delimited  => ACHAR(9) returns the ASCII TAB character, so the final output is '(I0, :"~")', where ~ is used here to represent the invsible tab charater
  !  
  !
  SUBROUTINE PRINT_PTR(dyn, iu, sep, fmt)
    CLASS(DYNAMIC_ARRAY_INT32), intent(in):: dyn
    INTEGER,                    intent(in):: iu
    CHARACTER(*),     optional, intent(in):: sep
    CHARACTER(*),     optional, intent(in):: fmt
    CHARACTER(:), allocatable:: form
    CHARACTER:: FLAG
    INTEGER:: N
    !
    IF( dyn%siz < 1 ) RETURN
    !
    IF(PRESENT(sep)) THEN
        !
        N = INDEX( lowerCHAR, sep(1:1))
        IF( N > Z ) THEN
            FLAG = upperCHAR(N:N)
        ELSE
            FLAG = sep(1:1)
        END IF
        !
        SELECT CASE( FLAG )
        CASE("C",",")      ; form = fmtCOM
        CASE("S"," ")      ; form = fmtSPC
        CASE("T","H",TAB)  ; form = fmtTAB
        CASE("L","N",CR,LF); form = fmtNLn
        CASE DEFAULT
                             form = fmtCOM
        END SELECT    
        !
    ELSEIF(PRESENT(fmt)) THEN
                             form = '(*'//fmt//')'
    ELSE
        form = fmtCOM
    END IF
    !
    WRITE(iu, form) dyn%ptr
    !
  END SUBROUTINE
  !
  PURE FUNCTION STRING_PTR(dyn, sep, fmt) RESULT(str)
    CLASS(DYNAMIC_ARRAY_INT32), intent(in):: dyn
    CHARACTER(*),     optional, intent(in):: sep
    CHARACTER(*),     optional, intent(in):: fmt
    CHARACTER(:), allocatable:: str
    CHARACTER(:), allocatable:: form
    CHARACTER:: FLAG
    INTEGER:: N
    !
    IF( dyn%siz < 1 ) THEN
                      str = " "
                      RETURN
    END IF
    !
    IF(PRESENT(sep)) THEN
        !
        N = INDEX( lowerCHAR, sep(1:1))
        IF( N > Z ) THEN
            FLAG = upperCHAR(N:N)
        ELSE
            FLAG = sep(1:1)
        END IF
        !
        SELECT CASE( FLAG )
        CASE("C",",")      ; form = fmtCOM
        CASE("S"," ")      ; form = fmtSPC
        CASE("T","H",TAB)  ; form = fmtTAB
        CASE("L","N",CR,LF); form = fmtNLn
        CASE DEFAULT
                             form = fmtCOM
        END SELECT    
        !
    ELSEIF(PRESENT(fmt)) THEN
                             form = '(*'//fmt//')'
    ELSE
        form = fmtCOM
    END IF
    !
    N = i16*(dyn%siz/i16) + i16
    ALLOCATE(CHARACTER(N):: str)
    !
    WRITE(str, form) dyn%ptr
    !
    N = LEN_TRIM(str)
    IF( N > Z ) THEN
                str = str(1:N)
    ELSE
                str = " "
    END IF
    !
  END FUNCTION
  !
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  ! 
  !
END MODULE
!
  !!
  !! ------------------------------------------------------------------------------------------------------------------------
  !! 
  !PURE SUBROUTINE RESIZE_REFERENCE_DEQUE(dyn, siz)
  !  CLASS(ARRAY_DEQUE_INT32), intent(inout):: dyn
  !  INTEGER,                    intent(in   ):: siz
  !  !
  !  dyn%iter = Z
  !  !
  !  IF(dyn%dim < dyn%pos+siz ) CALL GROW_DIM_INT32(dyn, siz)
  !  !
  !  dyn%siz = siz
  !  !
  !  call set_ptr(dyn)
  !  !
  !END SUBROUTINE
  !! 
  !! ------------------------------------------------------------------------------------------------------------------------
  !! 
  !PURE SUBROUTINE FINAL_DEQUE(dyn)
  !  TYPE(ARRAY_DEQUE_INT32), intent(inout):: dyn
  !  !
  !  CALL FINAL_DYNAMIC_ARRAY(dyn)
  !  !
  !  dyn%pos  = Z 
  !  dyn%pad = i16
  !  !
  !END SUBROUTINE