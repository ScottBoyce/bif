!
! CODE DEVELOPED BY SCOTT E BOYCE 
!                   CONTACT <seboyce@usgs.gov> or <Boyce@engineer.com>
!
! This a driver module meant for easy import for other routines.
!   It also allows for each of the DYNs to have their own model and file. 
!   (Submodules dont work for data types)
!---------------------------------------------------------------------------------------------
! The math operators should be avoided cause they create copies
!    For example, DT = DT + 3, will add three to each element, 
!                 but the "DT + 3" will make a temp copy of the base array
!                 Instead it is better to do "CALL DT%ADD(3)", which will operate inplace.
!---------------------------------------------------------------------------------------------
!    
MODULE DYNAMIC_ARRAY !, ONLY: CAP, SIZE, DYNAMIC_ARRAY_INT32
  !USE, INTRINSIC:: IEEE_ARITHMETIC, ONLY: IEEE_VALUE, IEEE_QUIET_NAN
  !USE, INTRINSIC:: ISO_FORTRAN_ENV, ONLY: INT32!, INT64, REL64 => REAL64
  !USE, INTRINSIC:: ISO_C_BINDING,   ONLY: C_LOC, C_ASSOCIATED
  USE DYNAMIC_ARRAY_INT32_INSTRUCTION, ONLY: DYNAMIC_ARRAY_INT32, SIZE_INT32, CAPACITY_INT32
  !
  IMPLICIT NONE(TYPE, EXTERNAL)
  !
  PUBLIC:: CAP, SIZE
  PUBLIC:: DYNAMIC_ARRAY_INT32
  !
  PRIVATE
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
END MODULE
