!
! CODE DEVELOPED BY SCOTT E BOYCE 
!                   CONTACT <seboyce@usgs.gov> or <Boyce@engineer.com>
!
! MODULE LISTING:
!   POSITION_INTERFACE
!                                    
!                           FUNCTIONS
!                                    FIND_POS(VAL_SEARCH,VEC)
!                                    STR_POS(VAL_SEARCH,VEC)
!                                    INT_POS  (VAL_SEARCH,VEC)
!
MODULE POSITION_INTERFACE!, ONLY: FIND_POS, STR_POS, INT_POS
  !  
  USE CONSTANTS
  USE ARRAY_DATA_TYPES,          ONLY: CHARACTER_TYPE, INTEGER_VECTOR
  !
  !USE, INTRINSIC:: ISO_FORTRAN_ENV, ONLY: REAL32, REAL64, REAL128, INT8, INT16, INT32, INT64
  !
  IMPLICIT NONE
  !
  PRIVATE
  PUBLIC:: FIND_POS, STR_POS, INT_POS
  !
  !---------------------------------------------------------------------------------------------------------------------
  !
  INTERFACE FIND_POS
    MODULE PROCEDURE STR_POS_CHARACTER
    MODULE PROCEDURE STR_POS_CHARACTER_TYPE_CHAR
    MODULE PROCEDURE STR_POS_CHAR_CHARACTER_TYPE
    MODULE PROCEDURE STR_POS_CHARACTER_TYPE_CHARACTER_TYPE
    !
    MODULE PROCEDURE INT_POS_INTEGER
    MODULE PROCEDURE INT_POS_INTEGER_VECTOR
  END INTERFACE
  !
  INTERFACE STR_POS
    MODULE PROCEDURE STR_POS_CHARACTER
    MODULE PROCEDURE STR_POS_CHARACTER_TYPE_CHAR
    MODULE PROCEDURE STR_POS_CHAR_CHARACTER_TYPE
    MODULE PROCEDURE STR_POS_CHARACTER_TYPE_CHARACTER_TYPE
  END INTERFACE
  !
  INTERFACE INT_POS
    MODULE PROCEDURE INT_POS_INTEGER
    MODULE PROCEDURE INT_POS_INTEGER_VECTOR
  END INTERFACE
  !
  CONTAINS
  !
  PURE FUNCTION INT_POS_INTEGER(IVAL,VEC) RESULT(IDX)
    INTEGER,                           INTENT(IN):: IVAL
    INTEGER, CONTIGUOUS, DIMENSION(:), INTENT(IN):: VEC
    INTEGER:: IDX
    INTEGER:: I
    !
    IDX = Z
    DO I=ONE, SIZE(VEC)
        IF(IVAL == VEC(I)) THEN
            IDX = I
            EXIT
        END IF
    END DO
    !
  END FUNCTION
  !
  PURE ELEMENTAL FUNCTION INT_POS_INTEGER_VECTOR(IVAL,VEC) RESULT(IDX)
    INTEGER,              INTENT(IN):: IVAL
    TYPE(INTEGER_VECTOR), INTENT(IN):: VEC
    INTEGER:: IDX
    !
    IDX = VEC%FIND(IVAL)
    !
  END FUNCTION
  !
  !################################################################################
  !
  PURE FUNCTION STR_POS_CHARACTER(STR,VEC) RESULT(IDX)
    CHARACTER(*),                           INTENT(IN):: STR
    CHARACTER(*), CONTIGUOUS, DIMENSION(:), INTENT(IN):: VEC
    INTEGER:: IDX
    INTEGER:: I
    !
    IDX = Z
    DO I=ONE, SIZE(VEC)
        IF(STR == VEC(I)) THEN
            IDX = I
            EXIT
        END IF
    END DO
    !
  END FUNCTION
  !
  PURE FUNCTION STR_POS_CHARACTER_TYPE_CHAR(STR,VEC) RESULT(IDX)
    CHARACTER(*),                                   INTENT(IN):: STR
    TYPE(CHARACTER_TYPE), CONTIGUOUS, DIMENSION(:), INTENT(IN):: VEC
    INTEGER:: IDX
    INTEGER:: I
    !
    IDX = Z
    DO I=ONE, SIZE(VEC)
        IF(ALLOCATED(VEC(I)%STR)) THEN
              IF(STR == VEC(I)%STR) THEN
                  IDX = I
                  EXIT
              END IF
        END IF
    END DO
    !
  END FUNCTION
  !
  PURE FUNCTION STR_POS_CHAR_CHARACTER_TYPE(STR,VEC) RESULT(IDX)
    TYPE(CHARACTER_TYPE),                   INTENT(IN):: STR
    CHARACTER(*), CONTIGUOUS, DIMENSION(:), INTENT(IN):: VEC
    INTEGER:: IDX
    INTEGER:: I
    !
    IDX = Z
    IF(ALLOCATED(STR%STR)) THEN
       DO I=ONE, SIZE(VEC)
             IF(STR%STR == VEC(I)) THEN
                 IDX = I
                 EXIT
             END IF
       END DO
    END IF
    !
  END FUNCTION
  !
  PURE FUNCTION STR_POS_CHARACTER_TYPE_CHARACTER_TYPE(STR,VEC) RESULT(IDX)
    TYPE(CHARACTER_TYPE),                           INTENT(IN):: STR
    TYPE(CHARACTER_TYPE), CONTIGUOUS, DIMENSION(:), INTENT(IN):: VEC
    INTEGER:: IDX
    INTEGER:: I
    !
    IDX = Z
    IF(ALLOCATED(STR%STR)) THEN
       DO I=ONE, SIZE(VEC)
            IF(ALLOCATED(VEC(I)%STR)) THEN
                  IF(STR%STR == VEC(I)%STR) THEN
                      IDX = I
                      EXIT
                  END IF
            END IF
       END DO
    END IF
    !
  END FUNCTION
  !
END MODULE 
!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
