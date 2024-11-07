module factorial_interface
  use, intrinsic:: iso_fortran_env, only: int32, int64, real32, real64
  implicit none
  private
  !
  public :: factorial      ! factorial(n) -> f
  !
  interface factorial                 
    module procedure factorial_i32
    module procedure factorial_i64
    module procedure factorial_r32
    module procedure factorial_r64
  end interface
  !
  real(real64), parameter :: max_int32 = real(huge(1_int32), real64)
  !
  !----------------
  !
  contains
  !
  pure function factorial_i32(n) result(f)
    integer(int32), intent(in) :: n
    integer(int32) :: f, i
    f = 1_int32
    do i = 2_int32, n
       f = f * i
    end do
  end function
  !
  pure function factorial_i64(n) result(f)
    integer(int64), intent(in) :: n
    integer(int64) :: f, i
    f = 1_int64
    do i = 2_int64, n
       f = f * i
    end do
  end function
  !
  pure function factorial_r64(n) result(f)
    real(real64), intent(in) :: n
    real(real64) :: f, i
    !
    if (abs(n - anint(n)) < n*1.e-13_real64) then ! integer like float
        if(n < max_int32) then
            f = real(factorial_i32(int(n, int32)), real64)
        else
            f = real(factorial_i64(int(n, int64)), real64)
        end if
    else
        f = 1.0_real64
        i = 2.0_real64
        do while (i < n)
            f = f * i
            i = i + 1.0_real64
        end do
        if (2.0_real64 <= n) f = f * n
    end if
    !
  end function
  !
  pure function factorial_r32(n) result(f)
    real(real32), intent(in) :: n
    real(real32) :: f
    real(real64) :: f64
    !
    f64 = real(n, real64)
    f64 = factorial_r64(f64)
    if (f64 < real(HUGE(f), real64)) then
        f = real(f64, real32)
    else
        f = HUGE(f)
    end if
    !
  end function
  !
end module