! 
! CODE DEVELOPED BY SCOTT E BOYCE 
!                   CONTACT <seboyce@usgs.gov> or <Boyce@engineer.com>
! 
! Solves for a root given the user specified function
!   That is find x such that F(x) = 0 
! 
! The algorithm first uses the secant method with a and b as the initial guess, 
!   if it failes to converge by iter iterations, then it switches to using bisection.
!   If a and b do not staddle a root for bisection, program will expand a and b until it does.
!   Notes:
!       -> If no solution is found, then HUGE(x) is returned
!       -> This does not check for floating point errors, viz, 1/( f(x1) - f(x2) )
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
! Example use:
!
! MODULE ExampleCustomFunction
!     USE SECANT_INTERFACE
!     USE, INTRINSIC:: ISO_FORTRAN_ENV, ONLY: REL64 => REAL64
!     IMPLICIT NONE
!     !
!     PRIVATE
!     PUBLIC:: SOLVE_IT
!     !
!     CONTAINS
!     !
!     SUBROUTINE SOLVE_IT(coef1, coef2, coef3, ans)
!       REAL(REL64), intent(in ):: coef1, coef2, coef3
!       REAL(REL64), intent(out):: ans
!       !
!       ans = secant(custom, -1d0, 1d0)                  ! -1 and 1 are abitrarily chosen. 
!       !                                                !    Secant will hopefully improve or an inverse bisection search for proper values
!       CONTAINS
!       !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
!       function custom(x) result(y)                  !  ! Note this implicitly satisfies PROCEDURE(funcDP)
!                real(REL64), intent(in):: x          !  ! Also function inherits the values asigned to coef1, coef2, coef3
!                real(REL64):: y                      !
!                y = coef1*x*x + coef2*x + coef3      !
!       end function                                  !
!       !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
!     END SUBROUTINE
! END MODULE
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
MODULE SECANT_INTERFACE!, ONLY: SECANT
  !USE, INTRINSIC:: IEEE_ARITHMETIC, ONLY: IEEE_VALUE, IEEE_QUIET_NAN
  USE, INTRINSIC:: ISO_FORTRAN_ENV, ONLY: INT32,  INT64,  &
                                          REL32 => REAL32, REL64 => REAL64, &
                                          qp => REAL128
  IMPLICIT NONE
  !
  PRIVATE
  !
  PUBLIC:: SECANT  ! y = SECANT(func, a, b, [tol], [iter]);   SECANT is not PURE, so func can be PURE or not. 
  !
  PUBLIC:: pSECANT ! y = pSECANT(func, a, b, [tol], [iter]); pSECANT is PURE, so func must be PURE too
  !
  PUBLIC:: funcQP, funcDP, funcSP ! Abstract Interface for:      function funcDP(x) result(y)
  PUBLIC:: pureQP, pureDP, pureSP !                    for: pure function pureDP(x) result(y)
  !	
  INTERFACE SECANT
     MODULE PROCEDURE:: SECANT_QP
     MODULE PROCEDURE:: SECANT_DP
     MODULE PROCEDURE:: SECANT_SP
  END INTERFACE
  !	
  INTERFACE pSECANT
     MODULE PROCEDURE:: SECANT_QP_pure
     MODULE PROCEDURE:: SECANT_DP_pure
     MODULE PROCEDURE:: SECANT_SP_pure
  END INTERFACE
  !	
  ABSTRACT INTERFACE
       !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
       function funcQP(x) result(y)
                import:: qp
                real(qp), intent(in):: x
                real(qp):: y
       end function
       !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
       function funcDP(x) result(y)
                import:: REL64
                real(REL64), intent(in):: x
                real(REL64):: y
       end function
       !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
       function funcSP(x) result(y)
                import:: REL32
                real(REL32), intent(in):: x
                real(REL32):: y
       end function
       !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
       pure function pureQP(x) result(y)
                import:: qp
                real(qp), intent(in):: x
                real(qp):: y
       end function
       !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
       pure function pureDP(x) result(y)
                import:: REL64
                real(REL64), intent(in):: x
                real(REL64):: y
       end function
       !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
       pure function pureSP(x) result(y)
                import:: REL32
                real(REL32), intent(in):: x
                real(REL32):: y
       end function
       !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  END INTERFACE
  !	
  CONTAINS
  !
  !#############################################################################################################################################################
  !	
  FUNCTION SECANT_QP(func, a, b, tol, iter) result(ans)  ! a<b and sign(f(a)) /= sign(f(b))
    procedure(funcQP)             :: func
    real(qp),           intent(in):: a, b
    real(qp), optional, intent(in):: tol
    integer,  optional, intent(in):: iter
    real(qp):: ans
    real(qp):: x1, x2, f1, f2
    real(qp):: Ftol
    integer:: mxiter, i
    !
    if(present(tol)) then
        Ftol = tol
    else
        Ftol = 1e-19_qp
    end if
    !
    if(present(iter)) then
        mxiter = iter
    else
        mxiter = 100
    end if
    !
    if( a > b ) then
                x1 = a;  x2 = b
    else
                x1 = b;  x2 = a
    end if
    f1 = func(x1)  !Note x1 > x2
    f2 = func(x2)
    !
    if( abs(x1-x2) < Ftol .or. abs(f1-f2) < Ftol) then
                                                  ans = x1
                                                  return
    end if
    !
    !IF( sign(f1,f2) == f1 ) ERROR STOP 'SECANT_SOLVER (a,b) guess do not straddle a root'
    !
    do i=1, mxiter
         ans = x2 - f2*(x2 - x1)/(f2 - f1)
         !
         if( abs(ans-x2) < Ftol )  return
         !
         x1 = x2
         x2 = ans
         f1 = f2
         f2 = func(x2)
    end do
    !
    !    Secant failed, so switch to bisection
    if( a < b ) then                            ! Note x1 < x2
                x1 = a;  x2 = b
    else
                x1 = b;  x2 = a
    end if
    f1 = func(x1)
    f2 = func(x2)
    !
    if(f2*f1 > 0.0_qp) then  !x2 and x1 are not bounded by a root
       ans = x2 - x1
       do i=1, 2*mxiter
          ans = 2.0_qp*ans
          x1  = x1 - ans
          x2  = x2 + ans
          !
          f1 = func(x1)
          f2 = func(x2)
          !
          if(f2*f1 < 0.0_qp) exit
          !
          IF(f1 < 0.0_qp) f1 = -f1
          IF(f2 < 0.0_qp) f2 = -f2
          if(x1 < -1.0e256_qp .and. 1.0e256_qp  < x2) exit
          if(f1 >  1.0e1024_qp) exit
          if(f2 >  1.0e1024_qp) exit
       end do
       if(f2*f1 > 0.0_rel64) then      ! ERROR STOP 'SECANT_SOLVER failed and switched to bisection, but failed to find an (a,b) guess that straddles a root'
           ans = HUGE(ans)
           return
       end if
    end if
    !
    ans = (x2-x1)/Ftol
    mxiter = CEILING( log(ans)/log(2.0_qp) )  ! Get Log2 of number of calls - Max number of bisections possible
    !
    do i=1, mxiter
         ans = (x1 + x2) * 0.5_qp
         f2  = func(ans)
         !
         if( abs(f2) < Ftol )  return
         !
         if(f2*f1 < 0.0_qp) then
             x2 = ans
        else
             x1 = ans
             f1 = f2
        end if
    end do
    !
    ans = HUGE(ans)
    !
  END FUNCTION 
  !
  !#############################################################################################################################################################
  !	
  FUNCTION SECANT_DP(func, a, b, tol, iter) result(ans)  ! a<b and sign(f(a)) /= sign(f(b))
    procedure(funcDP)             :: func
    real(rel64),           intent(in):: a, b
    real(rel64), optional, intent(in):: tol
    integer,     optional, intent(in):: iter
    real(rel64):: ans
    real(rel64):: x1, x2, f1, f2
    real(rel64):: Ftol
    integer:: mxiter, i
    !
    if(present(tol)) then
        Ftol = tol
    else
        Ftol = 1e-11_rel64
    end if
    !
    if(present(iter)) then
        mxiter = iter
    else
        mxiter = 100
    end if
    !
    if( a > b ) then
                x1 = a;  x2 = b
    else
                x1 = b;  x2 = a
    end if
    f1 = func(x1)  !Note x1 > x2
    f2 = func(x2)
    !
    if( abs(x1-x2) < Ftol .or. abs(f1-f2) < Ftol) then
                                                  ans = x1
                                                  return
    end if
    !
    !IF( sign(f1,f2) == f1 ) ERROR STOP 'SECANT_SOLVER (a,b) guess do not straddle a root'
    !
    do i=1, mxiter
         ans = x2 - f2*(x2 - x1)/(f2 - f1)
         !
         if( abs(ans-x2) < Ftol )  return
         !
         x1 = x2
         x2 = ans
         f1 = f2
         f2 = func(x2)
    end do
    !
    !    Secant failed, so switch to bisection
    if( a < b ) then                            ! Note x1 < x2
                x1 = a;  x2 = b
    else
                x1 = b;  x2 = a
    end if
    f1 = func(x1)
    f2 = func(x2)
    !
    if(f2*f1 > 0.0_rel64) then  !x2 and x1 are not bounded by a root
       ans = x2 - x1
       do i=1, 2*mxiter
          ans = 2.0_rel64*ans
          x1  = x1 - ans
          x2  = x2 + ans
          !
          f1 = func(x1)
          f2 = func(x2)
          !
          if(f2*f1 < 0.0_rel64) exit
          !
          IF(f1 < 0.0_rel64) f1 = -f1
          IF(f2 < 0.0_rel64) f2 = -f2
          if(x1 < -1.0e128_rel64 .and. 1.0e128_rel64 < x2) exit
          if(f1 >  1.0e192_rel64) exit
          if(f2 >  1.0e192_rel64) exit
       end do
       if(f2*f1 > 0.0_rel64) then      ! ERROR STOP 'SECANT_SOLVER failed and switched to bisection, but failed to find an (a,b) guess that straddles a root'
           ans = HUGE(ans)
           return
       end if
    end if
    !
    ans = (x2-x1)/Ftol
    mxiter = CEILING( log(ans)/log(2.0_rel64) )  ! Get Log2 of number of calls - Max number of bisections possible
    !
    do i=1, mxiter
         ans = (x1 + x2) * 0.5_rel64
         f2  = func(ans)
         !
         if( abs(f2) < Ftol )  return
         !
         if(f2*f1 < 0.0_rel64) then
             x2 = ans
        else
             x1 = ans
             f1 = f2
        end if
    end do
    !
    ans = HUGE(ans)
    !
  END FUNCTION 
  !
  !#############################################################################################################################################################
  !	
  FUNCTION SECANT_SP(func, a, b, tol, iter) result(ans)  ! a<b and sign(f(a)) /= sign(f(b))
    procedure(funcSP)             :: func
    real(rel32),           intent(in):: a, b
    real(rel32), optional, intent(in):: tol
    integer,     optional, intent(in):: iter
    real(rel32):: ans
    real(rel32):: x1, x2, f1, f2
    real(rel32):: Ftol
    integer:: mxiter, i
    !
    if(present(tol)) then
        Ftol = tol
    else
        Ftol = 1e-5_rel32
    end if
    !
    if(present(iter)) then
        mxiter = iter
    else
        mxiter = 100
    end if
    !
    if( a > b ) then
                x1 = a;  x2 = b
    else
                x1 = b;  x2 = a
    end if
    f1 = func(x1)  !Note x1 > x2
    f2 = func(x2)
    !
    if( abs(x1-x2) < Ftol .or. abs(f1-f2) < Ftol) then
                                                  ans = x1
                                                  return
    end if
    !
    !IF( sign(f1,f2) == f1 ) ERROR STOP 'SECANT_SOLVER (a,b) guess do not straddle a root'
    !
    do i=1, mxiter
         ans = x2 - f2*(x2 - x1)/(f2 - f1)
         !
         if( abs(ans-x2) < Ftol )  return
         !
         x1 = x2
         x2 = ans
         f1 = f2
         f2 = func(x2)
    end do
    !
    !    Secant failed, so switch to bisection
    if( a < b ) then                            ! Note x1 < x2
                x1 = a;  x2 = b
    else
                x1 = b;  x2 = a
    end if
    f1 = func(x1)
    f2 = func(x2)
    !
    if(f2*f1 > 0.0_rel32) then  !x2 and x1 are not bounded by a root
       ans = x2 - x1
       do i=1, 2*mxiter
          ans = 2.0_rel32*ans
          x1  = x1 - ans
          x2  = x2 + ans
          !
          f1 = func(x1)
          f2 = func(x2)
          !
          if(f2*f1 < 0.0_rel32) exit
          !
          IF(f1 < 0.0_rel64) f1 = -f1
          IF(f2 < 0.0_rel64) f2 = -f2
          if(x1 < -1.0e16_rel32 .and. 1.0e16_rel32 < x2) exit
          if(f1 >  1.0e24_rel32) exit
          if(f2 >  1.0e24_rel32) exit
       end do
       if(f2*f1 > 0.0_rel32) then      ! ERROR STOP 'SECANT_SOLVER failed and switched to bisection, but failed to find an (a,b) guess that straddles a root'
           ans = HUGE(ans)
           return
       end if
    end if
    !
    ans = (x2-x1)/Ftol
    mxiter = CEILING( log(ans)/log(2.0_rel32) )  ! Get Log2 of number of calls - Max number of bisections possible
    !
    do i=1, mxiter
         ans = (x1 + x2) * 0.5_rel32
         f2  = func(ans)
         !
         if( abs(f2) < Ftol )  return
         !
         if(f2*f1 < 0.0_rel32) then
             x2 = ans
        else
             x1 = ans
             f1 = f2
        end if
    end do
    !
    ans = HUGE(ans)
    !
  END FUNCTION 
  !
  !#############################################################################################################################################################
  !	
  PURE FUNCTION SECANT_QP_pure(func, a, b, tol, iter) result(ans)  ! a<b and sign(f(a)) /= sign(f(b))
    procedure(pureQP)             :: func
    real(qp),           intent(in):: a, b
    real(qp), optional, intent(in):: tol
    integer,  optional, intent(in):: iter
    real(qp):: ans
    real(qp):: x1, x2, f1, f2
    real(qp):: Ftol
    integer:: mxiter, i
    !
    if(present(tol)) then
        Ftol = tol
    else
        Ftol = 1e-19_qp
    end if
    !
    if(present(iter)) then
        mxiter = iter
    else
        mxiter = 100
    end if
    !
    if( a > b ) then
                x1 = a;  x2 = b
    else
                x1 = b;  x2 = a
    end if
    f1 = func(x1)  !Note x1 > x2
    f2 = func(x2)
    !
    if( abs(x1-x2) < Ftol .or. abs(f1-f2) < Ftol) then
                                                  ans = x1
                                                  return
    end if
    !
    !IF( sign(f1,f2) == f1 ) ERROR STOP 'SECANT_SOLVER (a,b) guess do not straddle a root'
    !
    do i=1, mxiter
         ans = x2 - f2*(x2 - x1)/(f2 - f1)
         !
         if( abs(ans-x2) < Ftol )  return
         !
         x1 = x2
         x2 = ans
         f1 = f2
         f2 = func(x2)
    end do
    !
    !    Secant failed, so switch to bisection
    if( a < b ) then                            ! Note x1 < x2
                x1 = a;  x2 = b
    else
                x1 = b;  x2 = a
    end if
    f1 = func(x1)
    f2 = func(x2)
    !
    if(f2*f1 > 0.0_qp) then  !x2 and x1 are not bounded by a root
       ans = x2 - x1
       do i=1, 2*mxiter
          ans = 2.0_qp*ans
          x1  = x1 - ans
          x2  = x2 + ans
          !
          f1 = func(x1)
          f2 = func(x2)
          !
          if(f2*f1 < 0.0_qp) exit
          !
          IF(f1 < 0.0_qp) f1 = -f1
          IF(f2 < 0.0_qp) f2 = -f2
          if(x1 < -1.0e256_qp .and. 1.0e256_qp  < x2) exit
          if(f1 >  1.0e1024_qp) exit
          if(f2 >  1.0e1024_qp) exit
       end do
       if(f2*f1 > 0.0_qp) then      ! ERROR STOP 'SECANT_SOLVER failed and switched to bisection, but failed to find an (a,b) guess that straddles a root'
           ans = HUGE(ans)
           return
       end if
    end if
    !
    ans = (x2-x1)/Ftol
    mxiter = CEILING( log(ans)/log(2.0_qp) )  ! Get Log2 of number of calls - Max number of bisections possible
    !
    do i=1, mxiter
         ans = (x1 + x2) * 0.5_qp
         f2  = func(ans)
         !
         if( abs(f2) < Ftol )  return
         !
         if(f2*f1 < 0.0_qp) then
             x2 = ans
        else
             x1 = ans
             f1 = f2
        end if
    end do
    !
    ans = HUGE(ans)
    !
  END FUNCTION 
  !
  !#############################################################################################################################################################
  !	
  PURE FUNCTION SECANT_DP_pure(func, a, b, tol, iter) result(ans)  ! a<b and sign(f(a)) /= sign(f(b))
    procedure(pureDP)             :: func
    real(rel64),           intent(in):: a, b
    real(rel64), optional, intent(in):: tol
    integer,     optional, intent(in):: iter
    real(rel64):: ans
    real(rel64):: x1, x2, f1, f2
    real(rel64):: Ftol
    integer:: mxiter, i
    !
    if(present(tol)) then
        Ftol = tol
    else
        Ftol = 1e-11_rel64
    end if
    !
    if(present(iter)) then
        mxiter = iter
    else
        mxiter = 100
    end if
    !
    if( a > b ) then
                x1 = a;  x2 = b
    else
                x1 = b;  x2 = a
    end if
    f1 = func(x1)  !Note x1 > x2
    f2 = func(x2)
    !
    if( abs(x1-x2) < Ftol .or. abs(f1-f2) < Ftol) then
                                                  ans = x1
                                                  return
    end if
    !
    !IF( sign(f1,f2) == f1 ) ERROR STOP 'SECANT_SOLVER (a,b) guess do not straddle a root'
    !
    do i=1, mxiter
         ans = x2 - f2*(x2 - x1)/(f2 - f1)
         !
         if( abs(ans-x2) < Ftol )  return
         !
         x1 = x2
         x2 = ans
         f1 = f2
         f2 = func(x2)
    end do
    !
    !    Secant failed, so switch to bisection
    if( a < b ) then                            ! Note x1 < x2
                x1 = a;  x2 = b
    else
                x1 = b;  x2 = a
    end if
    f1 = func(x1)
    f2 = func(x2)
    !
    if(f2*f1 > 0.0_rel64) then  !x2 and x1 are not bounded by a root
       ans = x2 - x1
       do i=1, 2*mxiter
          ans = 2.0_rel64*ans
          x1  = x1 - ans
          x2  = x2 + ans
          !
          f1 = func(x1)
          f2 = func(x2)
          !
          if(f2*f1 < 0.0_rel64) exit
          !
          IF(f1 < 0.0_rel64) f1 = -f1
          IF(f2 < 0.0_rel64) f2 = -f2
          if(x1 < -1.0e128_rel64 .and. 1.0e128_rel64 < x2) exit
          if(f1 >  1.0e192_rel64) exit
          if(f2 >  1.0e192_rel64) exit
       end do
       if(f2*f1 > 0.0_rel64) then      ! ERROR STOP 'SECANT_SOLVER failed and switched to bisection, but failed to find an (a,b) guess that straddles a root'
           ans = HUGE(ans)
           return
       end if
    end if
    !
    ans = (x2-x1)/Ftol
    mxiter = CEILING( log(ans)/log(2.0_rel64) )  ! Get Log2 of number of calls - Max number of bisections possible
    !
    do i=1, mxiter
         ans = (x1 + x2) * 0.5_rel64
         f2  = func(ans)
         !
         if( abs(f2) < Ftol )  return
         !
         if(f2*f1 < 0.0_rel64) then
             x2 = ans
        else
             x1 = ans
             f1 = f2
        end if
    end do
    !
    ans = HUGE(ans)
    !
  END FUNCTION 
  !
  !#############################################################################################################################################################
  !	
  PURE FUNCTION SECANT_SP_pure(func, a, b, tol, iter) result(ans)  ! a<b and sign(f(a)) /= sign(f(b))
    procedure(pureSP)             :: func
    real(rel32),           intent(in):: a, b
    real(rel32), optional, intent(in):: tol
    integer,     optional, intent(in):: iter
    real(rel32):: ans
    real(rel32):: x1, x2, f1, f2
    real(rel32):: Ftol
    integer:: mxiter, i
    !
    if(present(tol)) then
        Ftol = tol
    else
        Ftol = 1e-5_rel32
    end if
    !
    if(present(iter)) then
        mxiter = iter
    else
        mxiter = 100
    end if
    !
    if( a > b ) then
                x1 = a;  x2 = b
    else
                x1 = b;  x2 = a
    end if
    f1 = func(x1)  !Note x1 > x2
    f2 = func(x2)
    !
    if( abs(x1-x2) < Ftol .or. abs(f1-f2) < Ftol) then
                                                  ans = x1
                                                  return
    end if
    !
    !IF( sign(f1,f2) == f1 ) ERROR STOP 'SECANT_SOLVER (a,b) guess do not straddle a root'
    !
    do i=1, mxiter
         ans = x2 - f2*(x2 - x1)/(f2 - f1)
         !
         if( abs(ans-x2) < Ftol )  return
         !
         x1 = x2
         x2 = ans
         f1 = f2
         f2 = func(x2)
    end do
    !
    !    Secant failed, so switch to bisection
    if( a < b ) then                            ! Note x1 < x2
                x1 = a;  x2 = b
    else
                x1 = b;  x2 = a
    end if
    f1 = func(x1)
    f2 = func(x2)
    !
    if(f2*f1 > 0.0_rel32) then  !x2 and x1 are not bounded by a root
       ans = x2 - x1
       do i=1, 2*mxiter
          ans = 2.0_rel32*ans
          x1  = x1 - ans
          x2  = x2 + ans
          !
          f1 = func(x1)
          f2 = func(x2)
          !
          if(f2*f1 < 0.0_rel32) exit
          !
          IF(f1 < 0.0_rel64) f1 = -f1
          IF(f2 < 0.0_rel64) f2 = -f2
          if(x1 < -1.0e16_rel32 .and. 1.0e16_rel32 < x2) exit
          if(f1 >  1.0e24_rel32) exit
          if(f2 >  1.0e24_rel32) exit
       end do
       if(f2*f1 > 0.0_rel32) then      ! ERROR STOP 'SECANT_SOLVER failed and switched to bisection, but failed to find an (a,b) guess that straddles a root'
           ans = HUGE(ans)
           return
       end if
    end if
    !
    ans = (x2-x1)/Ftol
    mxiter = CEILING( log(ans)/log(2.0_rel32) )  ! Get Log2 of number of calls - Max number of bisections possible
    !
    do i=1, mxiter
         ans = (x1 + x2) * 0.5_rel32
         f2  = func(ans)
         !
         if( abs(f2) < Ftol )  return
         !
         if(f2*f1 < 0.0_rel32) then
             x2 = ans
        else
             x1 = ans
             f1 = f2
        end if
    end do
    !
    ans = HUGE(ans)
    !
  END FUNCTION 
  !
  !#############################################################################################################################################################
  !	
END MODULE
!