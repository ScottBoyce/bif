module is_integer_interface
  ! Module for checking if a given number is an integer.
  ! Provides overloaded functions for various data types including int32, int64, real32, and real64.
  !---------------------------------------------------------------------------
  !   Function that checks if argument is integer-like (-2, -1, 0, 1, 2)
  !   Integer   argument always return true.
  !   Real      argument return true if there is no fractional componet (no decimal digits).
  !   Character argument return true if they only contain digits; and optionally starts with a "+" or "-". 
  !   Character argument with allow_int_like_float=.TRUE. return true if it 
  !             contains a integer-like real number (see real argument).
  !
  ! Usage:
  !       use is_integer_interface, only: is_integer
  !
  !       is_int = is_integer(val)
  !       is_int = is_integer(val, allow_int_like_float)  ! only for val of type character
  !
  ! Arguments:
  !   val  - (integer, real, character): variable to check if it is integer-like.
  !   allow_int_like_float  - (logical): Optional, only for character val, default is .FALSE.
  !                                      If false, then returns true if val only contains digits and optionally starts with a "+" or "-"
  !                                      If true,  then converts val to a real64 and checks if it is integer-like.
  ! Returns:
  !   is_int  - (logical) Is true if val is integer-like.
  !
  ! Notes:
  !   - Real numbers are assumed integers if: abs(val - anint(val)) < abs(val) * TOL
  !   - allow_int_like_float is only for character variables
  !---------------------------------------------------------------------------
  use, intrinsic:: iso_fortran_env, only: int32, int64, real32, real64
  implicit none
  private
  !
  public :: is_integer
  !
  interface is_integer                   ! is_integer(n) -> is_int
    module procedure is_integer_i32      ! Integer (int32)
    module procedure is_integer_i64      ! Integer (int64)
    module procedure is_integer_r32      ! Real (real32)
    module procedure is_integer_r64      ! Real (real64)
    module procedure is_integer_only_str ! Character (*)
    module procedure is_integer_like_str ! Character (*)
  end interface
  !
  contains
  !
  pure function is_integer_i32(n) result(is_int)
    integer(int32), intent(in) :: n
    logical :: is_int
    is_int = .TRUE.  ! Since input is already an integer, always returns true.
  end function
  !
  pure function is_integer_i64(n) result(is_int)
    integer(int64), intent(in) :: n
    logical :: is_int
    is_int = .TRUE.  ! Since input is already an integer, always returns true.
  end function
  !
  pure function is_integer_r64(n) result(is_int)
    real(real64), intent(in) :: n
    logical :: is_int
    is_int = abs(n - anint(n)) < abs(n) * 1.e-13_real64! A tolerance of n * 1.e-13_real64 is used to account for floating-point precision issues.
  end function
  !
  pure function is_integer_r32(n) result(is_int)
    real(real32), intent(in) :: n
    logical :: is_int
    is_int = abs(n - anint(n)) < abs(n) * 1.e-7_real32
  end function
  !
  pure function is_integer_only_str(n) result(is_int)
    ! Function to check if a given character string represents an integer value.
    ! String may only contain digits and optionally start with a + or -
    character(*), intent(in) :: n
    logical :: is_int
    integer :: i, dim, istart
    !
    integer, parameter :: a0 = iachar('0')
    integer, parameter :: a9 = iachar('9')
    !
    dim = len_trim(n)
    istart = dim + 1
    do i=1, dim
        if(n(i:i) /= " ") then
            istart = i
            if(n(i:i)=="+" .or. n(i:i) =="-") istart = istart + 1
            exit
        end if
    end do
    !
    is_int = istart <= dim
    do i=istart, dim
       if( iachar(n(i:i)) < a0 .or. a9 < iachar(n(i:i)) ) then ! did not find a number, so not an integer
           is_int = .FALSE.
           exit
       end if
    end do
    !
  end function
  !
  pure function is_integer_like_str(n, allow_int_like_float) result(is_int)
    ! Function to check if a given character string represents an integer value.
    ! It accepts Fortran input styles including scientific notation.
    character(*), intent(in) :: n
    logical,      intent(in) :: allow_int_like_float
    logical :: is_int
    real(real64) :: x
    integer :: i
    !
    if (allow_int_like_float) then
        if(scan(n, ".eEdD")) then
            read(n, *, iostat=i) x
            if (i == 0) then
                is_int = abs(x - anint(x)) < abs(x) * 1.e-13_real64
            else
                is_int = .FALSE.
            end if
            return
        end if
    end if
    !
    is_int = is_integer_only_str(n)
    !
  end function
  !
end module