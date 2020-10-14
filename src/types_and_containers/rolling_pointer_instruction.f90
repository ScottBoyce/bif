!
MODULE ROLLING_POINTER_INSTRUCTION!, ONLY: ROLLING_POINTER_INT64
  USE, INTRINSIC:: ISO_FORTRAN_ENV, ONLY: INT32, INT64, REAL32, REAL64
  USE CONSTANTS, ONLY: NEG, Z, ONE, TWO, TRUE, FALSE
  IMPLICIT NONE
  !
  PRIVATE
  PUBLIC:: ROLLING_POINTER_INT32, ROLLING_POINTER_INT64!, ROLLING_POINTER_REL32, ROLLING_POINTER_REL64
  !
  TYPE POINTER_INT32
       INTEGER(INT32), POINTER:: val => NULL()
       !
       CONTAINS
       !
       PROCEDURE, PASS(PT):: INIT    =>    INIT_POINTER_INT32
       PROCEDURE, PASS(PT):: DESTROY => DESTROY_POINTER_INT32
       !
       FINAL:: FINAL_POINTER_INT32
       !
  END TYPE
  !
  TYPE ROLLING_POINTER_INT32
      INTEGER:: DIM 
      !
      TYPE( POINTER_INT32 ), DIMENSION(:), ALLOCATABLE:: pnt
      !
      CONTAINS
      !
      PROCEDURE, PASS(RP):: INIT             =>              INIT_ROLLING_POINTER_INT32  ! RP%INIT   (DIM)
      PROCEDURE, PASS(RP):: GET              =>               GET_ROLLING_POINTER_INT32  ! RP%GET    (POS)
      PROCEDURE, PASS(RP):: PREPEND          =>           PREPEND_ROLLING_POINTER_INT32  ! RP%PREPEND(VAL)
      PROCEDURE, PASS(RP):: APPEND           =>            APPEND_ROLLING_POINTER_INT32  ! RP%APPEND (VAL)
      PROCEDURE, PASS(RP):: PREPEND_NEW      =>            PRENEW_ROLLING_POINTER_INT32  ! RP%PREPEND_NEW(VAL)
      PROCEDURE, PASS(RP):: APPEND_NEW       =>            APPNEW_ROLLING_POINTER_INT32  ! RP% APPEND_NEW(VAL)
      PROCEDURE, PASS(RP):: PREPEND_ROLL     =>      PREPEND_ROLL_ROLLING_POINTER_INT32  ! RP%PREPEND_ROLL(VAL, NUL)
      PROCEDURE, PASS(RP):: APPEND_ROLL      =>       APPEND_ROLL_ROLLING_POINTER_INT32  ! RP%APPEND_ROLL (VAL, NUL)
      PROCEDURE, PASS(RP):: SORT             =>    INSERTION_SORT_ROLLING_POINTER_INT32  ! RP%SORT   ([ASCENDING])
      PROCEDURE, PASS(RP):: SORT_PREPEND_ROLL=> SORT_PREPEND_ROLL_ROLLING_POINTER_INT32  ! RP%SORT_PREPEND_ROLL(VAL, NUL, ASCENDING)
      PROCEDURE, PASS(RP):: SORT_APPEND_ROLL =>  SORT_APPEND_ROLL_ROLLING_POINTER_INT32  ! RP%SORT_APPEND_ROLL(VAL, NUL, ASCENDING)
      PROCEDURE, PASS(RP):: SET              =>               SET_ROLLING_POINTER_INT32  ! RP%SET    (VAL, [POS])
      !PROCEDURE, PASS(RP)::   PRINT =>   PRINT_ROLLING_POINTER_INT32 ! RP%PRINT  ()
      !
      PROCEDURE, PASS(RP):: DESTROY => DESTROY_ROLLING_POINTER_INT32
      !
      FINAL:: FINAL_ROLLING_POINTER_INT32
      !
  END TYPE
  !
  !############################################################################################################
  !
  TYPE POINTER_INT64
       INTEGER(INT64), POINTER:: val => NULL()
       !
       CONTAINS
       !
       PROCEDURE, PASS(PT):: INIT    =>    INIT_POINTER_INT64
       PROCEDURE, PASS(PT):: DESTROY => DESTROY_POINTER_INT64
       !
       FINAL:: FINAL_POINTER_INT64
       !
  END TYPE
  !
  TYPE ROLLING_POINTER_INT64
      INTEGER:: DIM 
      !
      TYPE( POINTER_INT64 ), DIMENSION(:), ALLOCATABLE:: pnt
      !
      CONTAINS
      !
      PROCEDURE, PASS(RP):: INIT             =>              INIT_ROLLING_POINTER_INT64  ! RP%INIT   (DIM)
      PROCEDURE, PASS(RP):: GET              =>               GET_ROLLING_POINTER_INT64  ! RP%GET    (POS)
      PROCEDURE, PASS(RP):: PREPEND          =>           PREPEND_ROLLING_POINTER_INT64  ! RP%PREPEND(VAL)
      PROCEDURE, PASS(RP):: APPEND           =>            APPEND_ROLLING_POINTER_INT64  ! RP%APPEND (VAL)
      PROCEDURE, PASS(RP):: PREPEND_NEW      =>            PRENEW_ROLLING_POINTER_INT64  ! RP%PREPEND_NEW(VAL)
      PROCEDURE, PASS(RP):: APPEND_NEW       =>            APPNEW_ROLLING_POINTER_INT64  ! RP% APPEND_NEW(VAL)
      PROCEDURE, PASS(RP):: PREPEND_ROLL     =>      PREPEND_ROLL_ROLLING_POINTER_INT64  ! RP%PREPEND_ROLL(VAL, NUL)
      PROCEDURE, PASS(RP):: APPEND_ROLL      =>       APPEND_ROLL_ROLLING_POINTER_INT64  ! RP%APPEND_ROLL (VAL, NUL)
      PROCEDURE, PASS(RP):: SORT             =>    INSERTION_SORT_ROLLING_POINTER_INT64  ! RP%SORT   ([ASCENDING])
      PROCEDURE, PASS(RP):: SORT_PREPEND_ROLL=> SORT_PREPEND_ROLL_ROLLING_POINTER_INT64  ! RP%SORT_PREPEND_ROLL(VAL, NUL, ASCENDING)
      PROCEDURE, PASS(RP):: SORT_APPEND_ROLL =>  SORT_APPEND_ROLL_ROLLING_POINTER_INT64  ! RP%SORT_APPEND_ROLL(VAL, NUL, ASCENDING)
      PROCEDURE, PASS(RP):: SET              =>               SET_ROLLING_POINTER_INT64  ! RP%SET    (VAL, [POS])
      !
      PROCEDURE, PASS(RP):: DESTROY => DESTROY_ROLLING_POINTER_INT64
      !
      FINAL:: FINAL_ROLLING_POINTER_INT64
      !
  END TYPE
  !
  !############################################################################################################
  !
  !!!TYPE POINTER_REAL64
  !!!        REAL(REAL64), POINTER:: val   => NULL()
  !!!        !
  !!!        CONTAINS
  !!!        !
  !!!        PROCEDURE, PASS(PT):: INIT    =>    INIT_POINTER_REAL64
  !!!        PROCEDURE, PASS(PT):: DESTROY => DESTROY_POINTER_REAL64
  !!!        !
  !!!        FINAL:: FINAL_POINTER_REAL64
  !!!        !
  !!!END TYPE
  !!!!
  !!!TYPE ROLLING_POINTER_REAL64
  !!!    INTEGER:: DIM 
  !!!    !
  !!!    TYPE( POINTER_REAL64 ), DIMENSION(:), ALLOCATABLE:: pnt
  !!!    !
  !!!    CONTAINS
  !!!    !
  !!!    PROCEDURE, PASS(RP):: INIT        =>    INIT_ROLLING_POINTER_REAL64  ! RP%INIT   (DIM)
  !!!    PROCEDURE, PASS(RP):: GET         =>     GET_ROLLING_POINTER_REAL64  ! RP%GET    (POS)
  !!!    PROCEDURE, PASS(RP):: PREPEND     => PREPEND_ROLLING_POINTER_REAL64  ! RP%PREPEND(VAL)
  !!!    PROCEDURE, PASS(RP)::  APPEND     =>  APPEND_ROLLING_POINTER_REAL64  ! RP%APPEND (VAL)
  !!!    PROCEDURE, PASS(RP):: PREPEND_NEW =>  PRENEW_ROLLING_POINTER_REAL64  ! RP%PREPEND_NEW(VAL)
  !!!    PROCEDURE, PASS(RP)::  APPEND_NEW =>  APPNEW_ROLLING_POINTER_REAL64  ! RP% APPEND_NEW(VAL)
  !!!    PROCEDURE, PASS(RP)::     SET     =>     SET_ROLLING_POINTER_REAL64  ! RP%SET    (VAL, [POS])
  !!!    !PROCEDURE, PASS(RP)::   PRINT =>   PRINT_ROLLING_POINTER_REAL64 ! RP%PRINT  ()
  !!!    !
  !!!    PROCEDURE, PASS(RP):: DESTROY => DESTROY_ROLLING_POINTER_REAL64
  !!!    !
  !!!    FINAL:: FINAL_ROLLING_POINTER_REAL64
  !!!    !
  !!!END TYPE
  !
  CONTAINS   ! ======================================================================================================================================= 
  !
  PURE ELEMENTAL SUBROUTINE FINAL_POINTER_INT32(PT)
    TYPE(POINTER_INT32), INTENT(INOUT):: PT
    CALL DESTROY_POINTER_INT32(PT)
  END SUBROUTINE 
  !
  PURE ELEMENTAL SUBROUTINE DESTROY_POINTER_INT32(PT)
    CLASS(POINTER_INT32), INTENT(INOUT):: PT
    INTEGER:: I
    !
    IF(ASSOCIATED(PT%val)) DEALLOCATE(PT%val, STAT=I)
    !
    NULLIFY(PT%val)
    !
  END SUBROUTINE 
  !
  PURE SUBROUTINE INIT_POINTER_INT32(PT)
    CLASS(POINTER_INT32), INTENT(INOUT):: PT
    !
    CALL DESTROY_POINTER_INT32(PT)
    !
    ALLOCATE(PT%val)
    PT%val = Z
    !
  END SUBROUTINE 
  !
  !===================================================================================================================================================
  !
  PURE SUBROUTINE INIT_ROLLING_POINTER_INT32(RP, DIM)
    CLASS(ROLLING_POINTER_INT32), INTENT(INOUT):: RP
    INTEGER, INTENT(IN):: DIM
    INTEGER:: I
    !
    CALL DESTROY_ROLLING_POINTER_INT32(RP)
    !
    IF( DIM > Z ) THEN
                   RP%DIM = DIM
                   ALLOCATE(RP%pnt(DIM))
                   DO I=ONE, DIM
                             CALL RP%pnt(I)%INIT()
                   END DO
    END IF
    !
  END SUBROUTINE 
  !
  !---------------------------------------------------------------------------------------------------------------------------------------------------
  !
  PURE SUBROUTINE FINAL_ROLLING_POINTER_INT32(RP)
    TYPE(ROLLING_POINTER_INT32), INTENT(INOUT):: RP
    !
    CALL DESTROY_ROLLING_POINTER_INT32(RP)
    !
  END SUBROUTINE 
  !
  !---------------------------------------------------------------------------------------------------------------------------------------------------
  !
  PURE SUBROUTINE DESTROY_ROLLING_POINTER_INT32(RP)
    CLASS(ROLLING_POINTER_INT32), INTENT(INOUT):: RP
    INTEGER:: I
    !
    IF( RP%DIM > Z ) DEALLOCATE(RP%pnt, STAT=I)
    RP%DIM = Z
    !
  END SUBROUTINE 
  !
  !---------------------------------------------------------------------------------------------------------------------------------------------------
  !
  PURE FUNCTION GET_ROLLING_POINTER_INT32(RP, POS) RESULT(VAL)
    CLASS(ROLLING_POINTER_INT32), INTENT(IN):: RP
    INTEGER,                   INTENT(IN):: POS
    INTEGER(INT32):: VAL
    !
    VAL = RP%pnt(POS)%val
    !
  END FUNCTION 
  !
  !---------------------------------------------------------------------------------------------------------------------------------------------------
  !
  PURE SUBROUTINE SET_ROLLING_POINTER_INT32(RP, VAL, POS)
    CLASS(ROLLING_POINTER_INT32), INTENT(INOUT):: RP
    INTEGER(INT32),    INTENT(IN):: VAL
    INTEGER, OPTIONAL, INTENT(IN):: POS
    INTEGER:: I
    !
    IF(PRESENT(POS)) THEN
        RP%pnt(POS)%val = VAL
    ELSE
        DO I=ONE, RP%DIM
                  RP%pnt(I)%val = VAL
        END DO
    END IF
    !
  END SUBROUTINE 
  !
  !---------------------------------------------------------------------------------------------------------------------------------------------------
  !
  PURE SUBROUTINE PRENEW_ROLLING_POINTER_INT32(RP, VAL)
    CLASS(ROLLING_POINTER_INT32), INTENT(INOUT):: RP
    INTEGER(INT32), INTENT(IN):: VAL
    LOGICAL:: APPEND
    INTEGER:: I
    !
    APPEND = TRUE
    !
    DO I=ONE, RP%DIM
              IF( RP%pnt(I)%val == val ) THEN
                                         APPEND = FALSE
                                         EXIT
              END IF
    END DO
    !
    IF( APPEND ) CALL PREPEND_ROLLING_POINTER_INT32(RP, VAL)
    !
  END SUBROUTINE 
  !
  !
  PURE SUBROUTINE PREPEND_ROLLING_POINTER_INT32(RP, VAL)
    CLASS(ROLLING_POINTER_INT32), INTENT(INOUT):: RP
    INTEGER(INT32), INTENT(IN):: VAL
    INTEGER(INT32), POINTER:: pt
    INTEGER:: I
    !
    IF(RP%DIM > Z) THEN
                   RP%pnt(RP%DIM)%val = val
                   pt => RP%pnt(RP%DIM)%val
                   !
                   DO I=RP%DIM , TWO, NEG
                       RP%pnt(I)%val => RP%pnt(I-1)%val
                   END DO
                   RP%pnt(ONE)%val => pt
                   !
                   NULLIFY(pt)
    END IF
    !
  END SUBROUTINE  
  !
  !---------------------------------------------------------------------------------------------------------------------------------------------------
  !
  PURE SUBROUTINE APPNEW_ROLLING_POINTER_INT32(RP, VAL)
    CLASS(ROLLING_POINTER_INT32), INTENT(INOUT):: RP
    INTEGER(INT32), INTENT(IN):: VAL
    LOGICAL:: APPEND
    INTEGER:: I
    !
    APPEND = TRUE
    !
    DO I=ONE, RP%DIM
              IF( RP%pnt(I)%val == val ) THEN
                                         APPEND = FALSE
                                         EXIT
              END IF
    END DO
    !
    IF( APPEND ) CALL APPEND_ROLLING_POINTER_INT32(RP, VAL)
    !
  END SUBROUTINE 
  !
  !
  PURE SUBROUTINE APPEND_ROLLING_POINTER_INT32(RP, VAL)
    CLASS(ROLLING_POINTER_INT32), INTENT(INOUT):: RP
    INTEGER(INT32), INTENT(IN):: VAL
    INTEGER(INT32), POINTER:: pt
    INTEGER:: I
    !
    IF(RP%DIM > Z) THEN
                   RP%pnt(ONE)%val = val
                   pt => RP%pnt(ONE)%val
                   !
                   DO I=ONE, RP%DIM - ONE
                       RP%pnt(I)%val => RP%pnt(I+1)%val
                   END DO
                   RP%pnt(RP%DIM)%val => pt
                   !
                   NULLIFY(pt)
    END IF
    !
  END SUBROUTINE 
  !
  !---------------------------------------------------------------------------------------------------------------------------------------------------
  !
  PURE SUBROUTINE PREPEND_ROLL_ROLLING_POINTER_INT32(RP, VAL, NUL)
    CLASS(ROLLING_POINTER_INT32), INTENT(INOUT):: RP
    INTEGER(INT32), INTENT(IN):: VAL, NUL
    INTEGER:: LOC
    INTEGER:: I
    !
    LOC = Z
    !
    DO I=ONE, RP%DIM
              IF( RP%pnt(I)%val == val ) THEN
                                         LOC = I
                                         EXIT
              END IF
    END DO
    IF    ( LOC == Z  ) THEN
                        CALL PREPEND_ROLLING_POINTER_INT32(RP, VAL)
    ELSEIF( LOC > ONE ) THEN
                DO I=ONE, LOC - ONE
                        CALL APPEND_ROLLING_POINTER_INT32(RP, NUL)
                END DO
    END IF
    !
  END SUBROUTINE 
  !
  !---------------------------------------------------------------------------------------------------------------------------------------------------
  !
  PURE SUBROUTINE APPEND_ROLL_ROLLING_POINTER_INT32(RP, VAL, NUL)
    CLASS(ROLLING_POINTER_INT32), INTENT(INOUT):: RP
    INTEGER(INT32), INTENT(IN):: VAL, NUL
    INTEGER:: LOC
    INTEGER:: I
    !
    LOC = Z
    !
    DO I=ONE, RP%DIM
              IF( RP%pnt(I)%val == val ) THEN
                                         LOC = I
                                         EXIT
              END IF
    END DO
    IF    ( LOC == Z     ) THEN
                           CALL APPEND_ROLLING_POINTER_INT32(RP, VAL)
    ELSEIF( LOC < RP%DIM ) THEN
                   DO I=RP%DIM, LOC+ONE, NEG
                           CALL PREPEND_ROLLING_POINTER_INT32(RP, NUL)
                   END DO
    END IF
    !
  END SUBROUTINE 
  !
  !---------------------------------------------------------------------------------------------------------------------------------------------------
  !
  PURE SUBROUTINE SORT_PREPEND_ROLL_ROLLING_POINTER_INT32(RP, VAL, NUL, ASCENDING)
    CLASS(ROLLING_POINTER_INT32), INTENT(INOUT):: RP
    INTEGER(INT32), INTENT(IN):: VAL, NUL
    LOGICAL,        INTENT(IN):: ASCENDING
    INTEGER:: LOC
    INTEGER:: I
    !
    LOC = Z
    IF(ASCENDING) THEN  ! Check if in order
        DO I=ONE, RP%DIM - ONE
                           IF( RP%pnt(I)%val >= RP%pnt(I+1)%val ) THEN
                               LOC = ONE
                               EXIT
                           END IF
        END DO
    ELSE
        DO I=ONE, RP%DIM - ONE
                           IF( RP%pnt(I)%val <= RP%pnt(I+1)%val ) THEN
                               LOC = ONE
                               EXIT
                           END IF
        END DO
    END IF
    IF( LOC == ONE ) CALL RP%SORT(ASCENDING)
    !
    LOC = Z
    IF(ASCENDING) THEN
                  DO I=ONE, RP%DIM
                            IF( RP%pnt(I)%val >= val ) THEN
                                                       LOC = I
                                                       EXIT
                            END IF
                  END DO
    ELSE
                  DO I=ONE, RP%DIM
                            IF( RP%pnt(I)%val <= val ) THEN
                                                       LOC = I
                                                       EXIT
                            END IF
                  END DO
    END IF
    !
    IF( LOC == Z ) THEN
                   CALL PREPEND_ROLLING_POINTER_INT32(RP, VAL)
    ELSE
           DO I=ONE, LOC - ONE
                   CALL APPEND_ROLLING_POINTER_INT32(RP, NUL)
           END DO
           IF( RP%pnt(ONE)%val /= val ) CALL PREPEND_ROLLING_POINTER_INT32(RP, VAL)
    END IF
    !
  END SUBROUTINE 
  !
  !---------------------------------------------------------------------------------------------------------------------------------------------------
  !
  PURE SUBROUTINE SORT_APPEND_ROLL_ROLLING_POINTER_INT32(RP, VAL, NUL, ASCENDING)
    CLASS(ROLLING_POINTER_INT32), INTENT(INOUT):: RP
    INTEGER(INT32), INTENT(IN):: VAL, NUL
    LOGICAL,        INTENT(IN):: ASCENDING
    INTEGER:: LOC
    INTEGER:: I
    !
    LOC = Z
    IF(ASCENDING) THEN  ! Check if in order
        DO I=ONE, RP%DIM - ONE
                           IF( RP%pnt(I)%val >= RP%pnt(I+1)%val ) THEN
                               LOC = ONE
                               EXIT
                           END IF
        END DO
    ELSE
        DO I=ONE, RP%DIM - ONE
                           IF( RP%pnt(I)%val <= RP%pnt(I+1)%val ) THEN
                               LOC = ONE
                               EXIT
                           END IF
        END DO
    END IF
    IF( LOC == ONE ) CALL RP%SORT(ASCENDING)
    !
    LOC = Z
    IF(ASCENDING) THEN
                  DO I=ONE, RP%DIM
                            IF( RP%pnt(I)%val >= val ) THEN
                                                       LOC = I
                                                       EXIT
                            END IF
                  END DO
    ELSE
                  DO I=ONE, RP%DIM
                            IF( RP%pnt(I)%val <= val ) THEN
                                                       LOC = I
                                                       EXIT
                            END IF
                  END DO
    END IF
    !
    IF( LOC == Z ) THEN
                    CALL APPEND_ROLLING_POINTER_INT32(RP, VAL)
    ELSE
            IF( RP%pnt(LOC)%val == val ) LOC=LOC+ONE
            DO I=RP%DIM, LOC, NEG
                    CALL PREPEND_ROLLING_POINTER_INT32(RP, NUL)
            END DO
            IF( RP%pnt(RP%DIM)%val /= val ) CALL APPEND_ROLLING_POINTER_INT32(RP, VAL)
    END IF
    !
  END SUBROUTINE 
  !
  !---------------------------------------------------------------------------------------------------------------------------------------------------
  !
  PURE SUBROUTINE INSERTION_SORT_ROLLING_POINTER_INT32(RP,ASCENDING)
    CLASS(ROLLING_POINTER_INT32),   INTENT(INOUT):: RP
    LOGICAL,        OPTIONAL,       INTENT(IN   ):: ASCENDING
    INTEGER(INT32), POINTER:: VAL
    INTEGER:: I, J
    LOGICAL:: ASCEND
    !
    IF(RP%DIM > 1) THEN
       ASCEND = TRUE
       IF(PRESENT(ASCENDING)) ASCEND = ASCENDING
       !
       IF(ASCEND) THEN
          DO I=TWO, RP%DIM
              VAL => RP%pnt(I)%val
              J   = I - 1
              DO WHILE ( j >= 1 )
                         IF( VAL > RP%pnt(J)%val ) EXIT
                         RP%pnt(J+1)%val => RP%pnt(J)%val
                         J = J - 1
              END DO
              RP%pnt(J+1)%val => VAL
          END DO
       ELSE !^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v
          DO I=TWO, RP%DIM
              VAL => RP%pnt(I)%val
              J   = I - 1
              DO WHILE ( j >= 1 )
                         IF( VAL < RP%pnt(J)%val ) EXIT  ! Flip to greater than to make descending
                         RP%pnt(J+1)%val => RP%pnt(J)%val
                         J = J - 1
              END DO
              RP%pnt(J+1)%val => VAL
          END DO
       END IF
       NULLIFY(VAL)
    END IF
    !
  END SUBROUTINE 
  !SUBROUTINE PRINT_ROLLING_POINTER_INT32(RP)
  !  CLASS(ROLLING_POINTER_INT32), INTENT(INOUT):: RP
  !  INTEGER:: I
  !  !
  !  DO I=1, RP%DIM-1
  !      WRITE(*,'(i0,1x)', ADVANCE='NO') RP%pnt(I)%val
  !  END DO
  !  WRITE(*,'(i0)') RP%pnt(RP%DIM)%val
  !  !
  !END SUBROUTINE 
  !
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !
  PURE ELEMENTAL SUBROUTINE FINAL_POINTER_INT64(PT)
    TYPE(POINTER_INT64), INTENT(INOUT):: PT
    CALL DESTROY_POINTER_INT64(PT)
  END SUBROUTINE 
  !
  PURE ELEMENTAL SUBROUTINE DESTROY_POINTER_INT64(PT)
    CLASS(POINTER_INT64), INTENT(INOUT):: PT
    INTEGER:: I
    !
    IF(ASSOCIATED(PT%val)) DEALLOCATE(PT%val, STAT=I)
    !
    NULLIFY(PT%val)
    !
  END SUBROUTINE 
  !
  PURE SUBROUTINE INIT_POINTER_INT64(PT)
    CLASS(POINTER_INT64), INTENT(INOUT):: PT
    !
    CALL DESTROY_POINTER_INT64(PT)
    !
    ALLOCATE(PT%val)
    PT%val = Z
    !
  END SUBROUTINE 
  !
  !===================================================================================================================================================
  !
  PURE SUBROUTINE INIT_ROLLING_POINTER_INT64(RP, DIM)
    CLASS(ROLLING_POINTER_INT64), INTENT(INOUT):: RP
    INTEGER, INTENT(IN):: DIM
    INTEGER:: I
    !
    CALL DESTROY_ROLLING_POINTER_INT64(RP)
    !
    IF( DIM > Z ) THEN
                   RP%DIM = DIM
                   ALLOCATE(RP%pnt(DIM))
                   DO I=ONE, DIM
                             CALL RP%pnt(I)%INIT()
                   END DO
    END IF
    !
  END SUBROUTINE 
  !
  !---------------------------------------------------------------------------------------------------------------------------------------------------
  !
  PURE SUBROUTINE FINAL_ROLLING_POINTER_INT64(RP)
    TYPE(ROLLING_POINTER_INT64), INTENT(INOUT):: RP
    !
    CALL DESTROY_ROLLING_POINTER_INT64(RP)
    !
  END SUBROUTINE 
  !
  !---------------------------------------------------------------------------------------------------------------------------------------------------
  !
  PURE SUBROUTINE DESTROY_ROLLING_POINTER_INT64(RP)
    CLASS(ROLLING_POINTER_INT64), INTENT(INOUT):: RP
    INTEGER:: I
    !
    IF( RP%DIM > Z ) DEALLOCATE(RP%pnt, STAT=I)
    RP%DIM = Z
    !
  END SUBROUTINE 
  !
  !---------------------------------------------------------------------------------------------------------------------------------------------------
  !
  PURE FUNCTION GET_ROLLING_POINTER_INT64(RP, POS) RESULT(VAL)
    CLASS(ROLLING_POINTER_INT64), INTENT(IN):: RP
    INTEGER,                   INTENT(IN):: POS
    INTEGER(INT64):: VAL
    !
    VAL = RP%pnt(POS)%val
    !
  END FUNCTION 
  !
  !---------------------------------------------------------------------------------------------------------------------------------------------------
  !
  PURE SUBROUTINE SET_ROLLING_POINTER_INT64(RP, VAL, POS)
    CLASS(ROLLING_POINTER_INT64), INTENT(INOUT):: RP
    INTEGER(INT64),    INTENT(IN):: VAL
    INTEGER, OPTIONAL, INTENT(IN):: POS
    INTEGER:: I
    !
    IF(PRESENT(POS)) THEN
        RP%pnt(POS)%val = VAL
    ELSE
        DO I=ONE, RP%DIM
                  RP%pnt(I)%val = VAL
        END DO
    END IF
    !
  END SUBROUTINE 
  !
  !---------------------------------------------------------------------------------------------------------------------------------------------------
  !
  PURE SUBROUTINE PRENEW_ROLLING_POINTER_INT64(RP, VAL)
    CLASS(ROLLING_POINTER_INT64), INTENT(INOUT):: RP
    INTEGER(INT64), INTENT(IN):: VAL
    LOGICAL:: APPEND
    INTEGER:: I
    !
    APPEND = TRUE
    !
    DO I=ONE, RP%DIM
              IF( RP%pnt(I)%val == val ) THEN
                                         APPEND = FALSE
                                         EXIT
              END IF
    END DO
    !
    IF( APPEND ) CALL PREPEND_ROLLING_POINTER_INT64(RP, VAL)
    !
  END SUBROUTINE 
  !
  !
  PURE SUBROUTINE PREPEND_ROLLING_POINTER_INT64(RP, VAL)
    CLASS(ROLLING_POINTER_INT64), INTENT(INOUT):: RP
    INTEGER(INT64), INTENT(IN):: VAL
    INTEGER(INT64), POINTER:: pt
    INTEGER:: I
    !
    IF(RP%DIM > Z) THEN
                   RP%pnt(RP%DIM)%val = val
                   pt => RP%pnt(RP%DIM)%val
                   !
                   DO I=RP%DIM , TWO, NEG
                       RP%pnt(I)%val => RP%pnt(I-1)%val
                   END DO
                   RP%pnt(ONE)%val => pt
                   !
                   NULLIFY(pt)
    END IF
    !
  END SUBROUTINE  
  !
  !---------------------------------------------------------------------------------------------------------------------------------------------------
  !
  PURE SUBROUTINE APPNEW_ROLLING_POINTER_INT64(RP, VAL)
    CLASS(ROLLING_POINTER_INT64), INTENT(INOUT):: RP
    INTEGER(INT64), INTENT(IN):: VAL
    LOGICAL:: APPEND
    INTEGER:: I
    !
    APPEND = TRUE
    !
    DO I=ONE, RP%DIM
              IF( RP%pnt(I)%val == val ) THEN
                                         APPEND = FALSE
                                         EXIT
              END IF
    END DO
    !
    IF( APPEND ) CALL APPEND_ROLLING_POINTER_INT64(RP, VAL)
    !
  END SUBROUTINE 
  !
  !
  PURE SUBROUTINE APPEND_ROLLING_POINTER_INT64(RP, VAL)
    CLASS(ROLLING_POINTER_INT64), INTENT(INOUT):: RP
    INTEGER(INT64), INTENT(IN):: VAL
    INTEGER(INT64), POINTER:: pt
    INTEGER:: I
    !
    IF(RP%DIM > Z) THEN
                   RP%pnt(ONE)%val = val
                   pt => RP%pnt(ONE)%val
                   !
                   DO I=ONE, RP%DIM - ONE
                       RP%pnt(I)%val => RP%pnt(I+1)%val
                   END DO
                   RP%pnt(RP%DIM)%val => pt
                   !
                   NULLIFY(pt)
    END IF
    !
  END SUBROUTINE 
  !
  !---------------------------------------------------------------------------------------------------------------------------------------------------
  !
  PURE SUBROUTINE PREPEND_ROLL_ROLLING_POINTER_INT64(RP, VAL, NUL)
    CLASS(ROLLING_POINTER_INT64), INTENT(INOUT):: RP
    INTEGER(INT64), INTENT(IN):: VAL, NUL
    INTEGER:: LOC
    INTEGER:: I
    !
    LOC = Z
    !
    DO I=ONE, RP%DIM
              IF( RP%pnt(I)%val == val ) THEN
                                         LOC = I
                                         EXIT
              END IF
    END DO
    IF    ( LOC == Z  ) THEN
                        CALL PREPEND_ROLLING_POINTER_INT64(RP, VAL)
    ELSEIF( LOC > ONE ) THEN
                DO I=ONE, LOC - ONE
                        CALL APPEND_ROLLING_POINTER_INT64(RP, NUL)
                END DO
    END IF
    !
  END SUBROUTINE 
  !
  !---------------------------------------------------------------------------------------------------------------------------------------------------
  !
  PURE SUBROUTINE APPEND_ROLL_ROLLING_POINTER_INT64(RP, VAL, NUL)
    CLASS(ROLLING_POINTER_INT64), INTENT(INOUT):: RP
    INTEGER(INT64), INTENT(IN):: VAL, NUL
    INTEGER:: LOC
    INTEGER:: I
    !
    LOC = Z
    !
    DO I=ONE, RP%DIM
              IF( RP%pnt(I)%val == val ) THEN
                                         LOC = I
                                         EXIT
              END IF
    END DO
    IF    ( LOC == Z     ) THEN
                           CALL APPEND_ROLLING_POINTER_INT64(RP, VAL)
    ELSEIF( LOC < RP%DIM ) THEN
                   DO I=RP%DIM, LOC+ONE, NEG
                           CALL PREPEND_ROLLING_POINTER_INT64(RP, NUL)
                   END DO
    END IF
    !
  END SUBROUTINE 
  !
  !---------------------------------------------------------------------------------------------------------------------------------------------------
  !
  PURE SUBROUTINE SORT_PREPEND_ROLL_ROLLING_POINTER_INT64(RP, VAL, NUL, ASCENDING)
    CLASS(ROLLING_POINTER_INT64), INTENT(INOUT):: RP
    INTEGER(INT64), INTENT(IN):: VAL, NUL
    LOGICAL,        INTENT(IN):: ASCENDING
    INTEGER:: LOC
    INTEGER:: I
    !
    LOC = Z
    IF(ASCENDING) THEN  ! Check if in order
        DO I=ONE, RP%DIM - ONE
                           IF( RP%pnt(I)%val > RP%pnt(I+1)%val ) THEN
                               LOC = ONE
                               EXIT
                           END IF
        END DO
    ELSE
        DO I=ONE, RP%DIM - ONE
                           IF( RP%pnt(I)%val < RP%pnt(I+1)%val ) THEN
                               LOC = ONE
                               EXIT
                           END IF
        END DO
    END IF
    IF( LOC == ONE ) CALL RP%SORT(ASCENDING)
    !
    LOC = Z
    IF(ASCENDING) THEN
                  DO I=ONE, RP%DIM
                            IF( RP%pnt(I)%val >= val ) THEN
                                                       LOC = I
                                                       EXIT
                            END IF
                  END DO
    ELSE
                  DO I=ONE, RP%DIM
                            IF( RP%pnt(I)%val <= val ) THEN
                                                       LOC = I
                                                       EXIT
                            END IF
                  END DO
    END IF
    !
    IF( LOC == Z ) THEN
                   CALL PREPEND_ROLLING_POINTER_INT64(RP, VAL)
    ELSE
           DO I=ONE, LOC - ONE
                   CALL APPEND_ROLLING_POINTER_INT64(RP, NUL)
           END DO
           IF( RP%pnt(ONE)%val /= val ) CALL PREPEND_ROLLING_POINTER_INT64(RP, VAL)
    END IF
    !
  END SUBROUTINE 
  !
  !---------------------------------------------------------------------------------------------------------------------------------------------------
  !
  PURE SUBROUTINE SORT_APPEND_ROLL_ROLLING_POINTER_INT64(RP, VAL, NUL, ASCENDING)
    CLASS(ROLLING_POINTER_INT64), INTENT(INOUT):: RP
    INTEGER(INT64), INTENT(IN):: VAL, NUL
    LOGICAL,        INTENT(IN):: ASCENDING
    INTEGER:: LOC
    INTEGER:: I
    !
    LOC = Z
    IF(ASCENDING) THEN  ! Check if in order
        DO I=ONE, RP%DIM - ONE
                           IF( RP%pnt(I)%val > RP%pnt(I+1)%val ) THEN
                               LOC = ONE
                               EXIT
                           END IF
        END DO
    ELSE
        DO I=ONE, RP%DIM - ONE
                           IF( RP%pnt(I)%val < RP%pnt(I+1)%val ) THEN
                               LOC = ONE
                               EXIT
                           END IF
        END DO
    END IF
    IF( LOC == ONE ) CALL RP%SORT(ASCENDING)
    !
    LOC = Z
    IF(ASCENDING) THEN
                  DO I=ONE, RP%DIM
                            IF( RP%pnt(I)%val >= val ) THEN
                                                       LOC = I
                                                       EXIT
                            END IF
                  END DO
    ELSE
                  DO I=ONE, RP%DIM
                            IF( RP%pnt(I)%val <= val ) THEN
                                                       LOC = I
                                                       EXIT
                            END IF
                  END DO
    END IF
    !
    IF( LOC == Z ) THEN
                    CALL APPEND_ROLLING_POINTER_INT64(RP, VAL)
    ELSE
            IF( RP%pnt(LOC)%val == val ) LOC=LOC+ONE
            DO I=RP%DIM, LOC, NEG
                    CALL PREPEND_ROLLING_POINTER_INT64(RP, NUL)
            END DO
            IF( RP%pnt(RP%DIM)%val /= val ) CALL APPEND_ROLLING_POINTER_INT64(RP, VAL)
    END IF
    !
  END SUBROUTINE 
  !
  !---------------------------------------------------------------------------------------------------------------------------------------------------
  !
  PURE SUBROUTINE INSERTION_SORT_ROLLING_POINTER_INT64(RP,ASCENDING)
    CLASS(ROLLING_POINTER_INT64),   INTENT(INOUT):: RP
    LOGICAL,        OPTIONAL,       INTENT(IN   ):: ASCENDING
    INTEGER(INT64), POINTER:: VAL
    INTEGER:: I, J
    LOGICAL:: ASCEND
    !
    IF(RP%DIM > 1) THEN
       ASCEND = TRUE
       IF(PRESENT(ASCENDING)) ASCEND = ASCENDING
       !
       IF(ASCEND) THEN
          DO I=TWO, RP%DIM
              VAL => RP%pnt(I)%val
              J   = I - 1
              DO WHILE ( j >= 1 )
                         IF( VAL > RP%pnt(J)%val ) EXIT
                         RP%pnt(J+1)%val => RP%pnt(J)%val
                         J = J - 1
              END DO
              RP%pnt(J+1)%val => VAL
          END DO
       ELSE !^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v
          DO I=TWO, RP%DIM
              VAL => RP%pnt(I)%val
              J   = I - 1
              DO WHILE ( j >= 1 )
                         IF( VAL < RP%pnt(J)%val ) EXIT  ! Flip to greater than to make descending
                         RP%pnt(J+1)%val => RP%pnt(J)%val
                         J = J - 1
              END DO
              RP%pnt(J+1)%val => VAL
          END DO
       END IF
       NULLIFY(VAL)
    END IF
    !
  END SUBROUTINE 
  !
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !
    
  !
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !
END MODULE
