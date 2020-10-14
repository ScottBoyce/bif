!
! CODE DEVELOPED BY SCOTT E BOYCE 
!                   CONTACT <seboyce@usgs.gov> or <Boyce@engineer.com>
!  
!  Relaxation and Dampening routines
!
MODULE RELAX_INTERFACE
  !  
  USE, INTRINSIC:: ISO_FORTRAN_ENV, ONLY: REAL32, REAL64
  !
  IMPLICIT NONE
  !
  PRIVATE
  !
  PUBLIC:: RELAXER, RELAX_IT, DAMP_IT
  !
  !
  INTERFACE RELAXER
    MODULE PROCEDURE RELAXER_DBLE!(NEW, OLD, RELAX) RESULT(REL)
    MODULE PROCEDURE RELAXER_SNGL!(NEW, OLD, RELAX) RESULT(REL)
  END INTERFACE
  !
  INTERFACE RELAX_IT
    MODULE PROCEDURE RELAX_IT_DBLE !(NEW, OLD, RELAX)        --NEW IS UPDATED WITH RELAXED VALUE
    MODULE PROCEDURE RELAX_IT_SNGL !(NEW, OLD, RELAX)
    MODULE PROCEDURE RELAX_IT2_DBLE!(NEW2, NEW, OLD, RELAX)  --NEW2 HOLDS RELAXED VALUE
    MODULE PROCEDURE RELAX_IT2_SNGL!(NEW2, NEW, OLD, RELAX)
  END INTERFACE
  !
  INTERFACE DAMP_IT
    MODULE PROCEDURE DAMP_IT_DBLE!(NEW, OLD, OLD2, DAMP, RELAX)
    MODULE PROCEDURE DAMP_IT_SNGL!(NEW, OLD, OLD2, DAMP, RELAX)
  END INTERFACE
  !
  ! Constants used internally to module ----------------------------------------------------
  ! 
  REAL(REAL64), PARAMETER:: DNEG     = -1_REAL64
  REAL(REAL64), PARAMETER:: DZ       =  0_REAL64
  REAL(REAL64), PARAMETER:: HALF     =  0.5_REAL64
  REAL(REAL64), PARAMETER:: SUB_ONE  =  0.999999999_REAL64
  REAL(REAL64), PARAMETER:: UNO      =  1_REAL64
  REAL(REAL64), PARAMETER:: NEAR_ONE =  1.000000001_REAL64
  REAL(REAL64), PARAMETER:: TRES     =  3_REAL64
  REAL(REAL64), PARAMETER:: NEARZERO_29 = 1E-29_REAL64
  !
  ! ----------------------------------------------------------------------------------------
  !
  CONTAINS
  !
  PURE FUNCTION RELAXER_DBLE(NEW, OLD, RELAX) RESULT(REL)
    REAL(REAL64), INTENT(IN):: NEW, OLD, RELAX
    REAL(REAL64):: REL
    !
    IF(RELAX == UNO) THEN
        REL = NEW
    ELSE
        REL = OLD + RELAX*(NEW-OLD)
    END IF
    !
  END FUNCTION
  !
  !#############################################################################################################################################################
  !
  PURE FUNCTION RELAXER_SNGL(NEW, OLD, RELAX) RESULT(REL)
    REAL(REAL32), INTENT(IN):: NEW, OLD
    REAL(REAL64), INTENT(IN):: RELAX
    REAL(REAL32):: REL
    !
    IF(RELAX == UNO) THEN
        REL = NEW
    ELSE
        REL = OLD + RELAX*(NEW-OLD)
    END IF
    !
  END FUNCTION
  !
  !#############################################################################################################################################################
  !
  PURE SUBROUTINE RELAX_IT_DBLE(NEW, OLD, RELAX)
    REAL(REAL64), INTENT(INOUT):: NEW
    REAL(REAL64), INTENT(IN   ):: OLD, RELAX
    !
    IF(RELAX < SUB_ONE .OR. RELAX > NEAR_ONE) NEW = OLD + RELAX*(NEW-OLD)
    !
  END SUBROUTINE
  !
  PURE SUBROUTINE RELAX_IT_SNGL(NEW, OLD, RELAX)
    REAL(REAL32), INTENT(INOUT):: NEW
    REAL(REAL32), INTENT(IN   ):: OLD
    REAL(REAL64), INTENT(IN   ):: RELAX
    !
    IF(RELAX < SUB_ONE .OR. RELAX > NEAR_ONE) NEW = OLD + RELAX*(NEW-OLD)
    !
  END SUBROUTINE
  !
  !#############################################################################################################################################################
  !
  PURE SUBROUTINE RELAX_IT2_DBLE(NEW2, NEW, OLD, RELAX)
    REAL(REAL64), INTENT(  OUT):: NEW2
    REAL(REAL64), INTENT(IN   ):: NEW, OLD, RELAX
    !
    IF(SUB_ONE < RELAX .AND. RELAX < NEAR_ONE) THEN
        NEW2 = NEW
    ELSE
        NEW2 = OLD + RELAX*(NEW-OLD)
    END IF
    !
  END SUBROUTINE
  !
  !#############################################################################################################################################################
  !
  PURE SUBROUTINE RELAX_IT2_SNGL(NEW2, NEW, OLD, RELAX)
    REAL(REAL32), INTENT(  OUT):: NEW2
    REAL(REAL32), INTENT(IN   ):: NEW, OLD, RELAX
    !
    IF(SUB_ONE < RELAX .AND. RELAX < NEAR_ONE) THEN
        NEW2 = NEW
    ELSE
        NEW2 = OLD + RELAX*(NEW-OLD)
    END IF
    !
  END SUBROUTINE
  !
  PURE SUBROUTINE DAMP_IT_DBLE(NEW, OLD, OLD2, DAMP, RELAX) !Based off of Mehl and Hill 2001
    REAL(REAL64), INTENT(INOUT):: NEW
    REAL(REAL64), INTENT(IN   ):: OLD, OLD2, DAMP
    REAL(REAL64), INTENT(IN   ), OPTIONAL:: RELAX
    REAL(REAL64):: DMP
    !
    IF( ABS(OLD-OLD2) > NEARZERO_29 .AND. DAMP < UNO) THEN
        DMP = (NEW-OLD)/(DAMP*(OLD-OLD2))
        IF(DMP < DNEG) THEN
                             DMP = HALF / (DNEG * DMP)  !=1/(2*ABS(DMP))
        ELSEIF(DMP < -0.015D0) THEN
                             DMP = (TRES + DMP)/(TRES - DMP)
        ELSE
                             DMP = 0.99D0
        END IF
        !
        IF(PRESENT(RELAX)) DMP = DMP * RELAX
        !
        NEW = OLD + DMP*(NEW-OLD)
    END IF
    !
  END SUBROUTINE
  !
  !#############################################################################################################################################################
  !
  PURE SUBROUTINE DAMP_IT_SNGL(NEW, OLD, OLD2, DAMP, RELAX) !Based off of Mehl and Hill 2001
    REAL(REAL32), INTENT(INOUT):: NEW
    REAL(REAL32), INTENT(IN   ):: OLD, OLD2, DAMP
    REAL(REAL64), INTENT(IN   ), OPTIONAL:: RELAX
    REAL(REAL32):: DMP
    !
    IF( ABS(OLD-OLD2) > NEARZERO_29 .AND. DAMP < UNO) THEN
        DMP = (NEW-OLD)/(DAMP*(OLD-OLD2))
        IF(DMP < DNEG) THEN
                             DMP = HALF / (DNEG * DMP)  !=1/(2*ABS(DMP))
        ELSEIF(DMP < -0.015D0) THEN
                             DMP = (TRES + DMP)/(TRES - DMP)
        ELSE
                             DMP = 0.99D0
        END IF
        !
        IF(PRESENT(RELAX)) DMP = DMP * RELAX
        !
        NEW = OLD + DMP*(NEW-OLD)
    END IF
    !
  END SUBROUTINE
  !
END MODULE