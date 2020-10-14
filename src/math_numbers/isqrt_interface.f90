!
! CODE DEVELOPED BY SCOTT E BOYCE 
!                   CONTACT <seboyce@usgs.gov> or <Boyce@engineer.com>
!  
!  Floor Square Root of Integer using Bisection Method
!
MODULE ISQRT_INTERFACE
  !  
  USE, INTRINSIC:: ISO_FORTRAN_ENV, ONLY: INT32, INT64
  !
  IMPLICIT NONE
  !
  PRIVATE
  !
  PUBLIC:: isqrt
  !
  INTERFACE isqrt
    MODULE PROCEDURE isqrt_INT32
    MODULE PROCEDURE isqrt_INT64 
  END INTERFACE
  !
  CONTAINS
  !
  PURE FUNCTION isqrt_INT32(N) RESULT(ans)
     INTEGER(INT32), INTENT(IN) :: N
     INTEGER(INT32) :: ans
     INTEGER(INT32) :: l, r, tmp
     !
     if( N < 2 ) then
                 ans = N
                 RETURN
     elseif(N >= 2147395600_INT32) then
                                   ans = 46340_INT32
                                   RETURN
     end if
     !
     l = 0
     r = SHIFTR(N, 1)
     IF( r > 46339_INT32 ) r = 46339_INT32  !Largest possible square -> 46340**2 is an overflow
     !
     do while (l <= r)
                 ans = SHIFTR(l+r, 1)
                 !
                 tmp = ans*ans
                 !
                 if ( tmp == N ) then
                                 RETURN
                 elseif(tmp < N) then
                                 l = ans + 1
                 else
                                 r = ans - 1
                 end if
     end do
     ans = l - 1
     !
  END FUNCTION
  !
  PURE FUNCTION isqrt_INT64(N) RESULT(ans)
     INTEGER(INT64), INTENT(IN) :: N
     INTEGER(INT64) :: ans
     INTEGER(INT64) :: l, r, tmp
     !
     if( N < 2 ) then
                 ans = N
                 RETURN
     elseif(N > 9223372030926249000_INT64) then
                                   ans = 3037000499_INT64
                                   RETURN
     end if
     !
     l = 0
     r = SHIFTR(N, 1)
     IF( r > 3037000498_INT64 ) r = 3037000498_INT64  !Largest possible square -> 46340**2 is an overflow
     !
     do while (l <= r)
                 ans = SHIFTR(l+r, 1)
                 !
                 tmp = ans*ans
                 !
                 if ( tmp == N ) then
                                 RETURN
                 elseif(tmp < N) then
                                 l = ans + 1
                 else
                                 r = ans - 1
                 end if
     end do
     ans = l - 1
     !
  END FUNCTION
  !
END MODULE