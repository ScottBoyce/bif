!
! CODE DEVELOPED BY SCOTT E BOYCE 
!                   CONTACT <seboyce@usgs.gov> or <Boyce@engineer.com>
!  
!  LOG2 routines
!
MODULE LOG2_INTERFACE
  !  
  !USE CONSTANTS, ONLY: ninf, ninf_R
  !
  USE, INTRINSIC:: ISO_FORTRAN_ENV, ONLY: REAL32, REAL64, INT8, INT16, INT32, INT64
  !
  IMPLICIT NONE
  !
  ! Constants used internally to module ----------------------------------------------------
  ! 
  REAL(REAL32), PARAMETER:: LOG_2_R = LOG(2.0_REAL32)
  REAL(REAL32), PARAMETER:: ninf_R  = -huge(LOG_2_R)
  !
  REAL(REAL64), PARAMETER:: LOG_2     = LOG(2.0_REAL64)
  REAL(REAL64), PARAMETER:: LOG_2_inv = 1.0_REAL64/LOG_2
  REAL(REAL64), PARAMETER:: ninf  = -huge(LOG_2)
  !
  ! ----------------------------------------------------------------------------------------
  ! 
  PRIVATE
  !
  PUBLIC:: LOG2
  !
  INTERFACE LOG2
    MODULE PROCEDURE LOG2_DBLE
    MODULE PROCEDURE LOG2_SNLG
    MODULE PROCEDURE LOG2_INT8
    MODULE PROCEDURE LOG2_INT16
    MODULE PROCEDURE LOG2_INT32
    MODULE PROCEDURE LOG2_INT64
  END INTERFACE
  !
  CONTAINS
  !
  !
  FUNCTION LOG2_DBLE( X ) RESULT(LOG2)
    REAL(REAL64), INTENT(IN):: X
    REAL(REAL64):: LOG2
    !
    IF( X > 0.0_real64) THEN
                 LOG2 = LOG(X) * LOG_2_inv ! / 0.693147180559945286_REAL64
    ELSE
                 LOG2 = -HUGE(X)
    END IF
    !
  END FUNCTION
  !
  FUNCTION LOG2_SNLG( X ) RESULT(LOG2)
    REAL(REAL32), INTENT(IN):: X
    REAL(REAL32):: LOG2
    !
    IF( X > 0.0) THEN
                 LOG2 = LOG(X) / 0.69314718056_REAL32
    ELSE
                 LOG2 = -HUGE(X)
    END IF
    !
  END FUNCTION
  !
  FUNCTION LOG2_INT32( X ) RESULT( LOG2 )
    INTEGER(INT32), INTENT(IN) :: X
    INTEGER(INT32)             :: LOG2
    INTEGER(INT32)             :: IVAL
    !
    LOG2 = -1 
    !
    IF ( X > 0 ) THEN
        !
        IVAL = X
        DO WHILE (IVAL > 0)
                          LOG2 = LOG2 + 1
                          IVAL = SHIFTR( IVAL, 1 )  !Drop out the right most bit
        ENDDO
    END IF
  END FUNCTION
  !
  FUNCTION LOG2_INT64( X ) RESULT( LOG2 )
    INTEGER(INT64), INTENT(IN) :: X
    INTEGER(INT64)             :: LOG2
    INTEGER(INT64)             :: IVAL
    !
    LOG2 = -1 
    !
    IF ( X > 0 ) THEN
        !
        IVAL = X
        DO WHILE (IVAL > 0)
                          LOG2 = LOG2 + 1
                          IVAL = SHIFTR( IVAL, 1 )  !Drop out the right most bit
        ENDDO
    END IF
  END FUNCTION
  !
  FUNCTION LOG2_INT8( X ) RESULT( LOG2 )
    INTEGER(INT8), INTENT(IN) :: X
    INTEGER(INT8)             :: LOG2
    INTEGER(INT8)             :: IVAL
    !
    LOG2 = -1 
    !
    IF ( X > 0 ) THEN
        !
        IVAL = X
        DO WHILE (IVAL > 0)
                          LOG2 = LOG2 + 1
                          IVAL = SHIFTR( IVAL, 1 )  !Drop out the right most bit
        ENDDO
    END IF
  END FUNCTION
  !
  FUNCTION LOG2_INT16( X ) RESULT( LOG2 )
    INTEGER(INT16), INTENT(IN) :: X
    INTEGER(INT16)             :: LOG2
    INTEGER(INT16)             :: IVAL
    !
    LOG2 = -1 
    !
    IF ( X > 0 ) THEN
        !
        IVAL = X
        DO WHILE (IVAL > 0)
                          LOG2 = LOG2 + 1
                          IVAL = SHIFTR( IVAL, 1 )  !Drop out the right most bit
        ENDDO
    END IF
  END FUNCTION
  !
END MODULE