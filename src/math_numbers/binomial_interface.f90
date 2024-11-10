module binomial_interface
  !---------------------------------------------------------------------------
  !   Calculates the binomial coefficient C(n, k) for a given n and k. 
  !   The binomial coefficient represents the number of ways to 
  !   choose k elements from a set of n elements without regard to order.
  !   Uses the multiplicative formula[1] if n and k are integer-like, otherwise
  !   uses the gamma function formulation[2].
  !     [1] https://en.wikipedia.org/wiki/Binomial_coefficient#Multiplicative_formula
  !     [2] https://en.wikipedia.org/wiki/Binomial_coefficient#Two_real_or_complex_valued_arguments
  !
  ! Usage:
  !       use binomial_interface, only: binomial, fbinomial
  !       call binomial(n, k, bc)
  !       bc = fbinomial(n, k)
  !
  ! Arguments:
  !   n   - (integer, real): The number of items in the set.
  !   k   - (integer, real): The number of items to choose from the set.
  !
  ! Returns:
  !   bc  - The binomial coefficient, C(n, k)
  !
  ! Notes:
  !   - n and k must be the same data type
  !   - If k < 0 or n < k, returns 0
  !   - If k is 0 or k equals n, the coefficient is 1 by definition.
  !   - Uses symmetry property of binomial coefficients, 
  !     C(n, k) = C(n, n-k),to reduce computational complexity.
  !---------------------------------------------------------------------------
  use, intrinsic:: iso_fortran_env, only: int32, int64, real32, real64
  implicit none
  private
  !
  public :: binomial, fbinomial
  !
  interface binomial                  ! call binomial(n, k, bc)
    module procedure binomial_i32
    module procedure binomial_i64
    module procedure binomial_i32_r64
    module procedure binomial_i64_r64
    module procedure binomial_r32
    module procedure binomial_r64
  end interface
  !
  interface fbinomial                  ! bc = binomial(n, k)
    module procedure fbinomial_i32
    module procedure fbinomial_i64
    module procedure fbinomial_r32
    module procedure fbinomial_r64
  end interface
  !
  real(real64), parameter :: d0 = 0.0_real64
  real(real64), parameter :: d1 = 1.0_real64
  real(real64), parameter :: is_int_tol = 1.e-13_real64              ! Tolerance for checking if a real value is an integer
  real(real64), parameter :: max_int32 = real(huge(1_int32), real64) ! Maximum value of int32 represented as real64
  !
  contains
  !
  pure subroutine binomial_i32(n, k, bc)
    integer(int32), intent(in ) :: n, k
    integer(int32), intent(out) :: bc
    real(real64) :: bc64
    call binomial_i32_r64(n, k, bc64)
    bc = nint(bc64, int32)
  end subroutine  
  !
  pure subroutine binomial_i64(n, k, bc)
    integer(int64), intent(in ) :: n, k
    integer(int64), intent(out) :: bc
    real(real64) :: bc64
    call binomial_i64_r64(n, k, bc64)
    bc = nint(bc64, int64)
  end subroutine
  !
  pure subroutine binomial_i32_r64(n, k, bc)
    integer(int32), intent(in ) :: n, k
    real(real64),   intent(out) :: bc
    real(real64) :: i, n1
    integer :: step, k_min
    !
    if (n < k .or. k < 0_int32) then  ! If k < 0 or n < k, result is 0
        bc = d0
    else
        k_min = k
        if (k > SHIFTR(n, 1)) k_min = n - k ! if (k > n - k) -> Use symmetry property to minimize calculations: C(n, k) = C(n, n-k)
        bc = d1
        i  = d1
        n1 = real(n, real64) + d1
        !
        do step = 1, k_min
            bc = bc * (n1-i) / i
            i = i + d1
        end do
    end if
    !
  end subroutine  
  !
  pure subroutine binomial_i64_r64(n, k, bc)
    integer(int64), intent(in ) :: n, k
    real(real64),   intent(out) :: bc
    real(real64) :: i, n1
    integer(int64) :: step, k_min
    !
    if (n < k .or. k < 0_int64) then  ! If k < 0 or n < k, result is 0
        bc = d0
    else
        bc = d1
        i  = d1
        n1 = real(n, real64) + d1
        !
        do step = 1, k
            bc = bc * (n1-i) / i
            i = i + d1
        end do
    end if
    !
  end subroutine
  !
  pure subroutine binomial_r64(n, k, bc)
    real(real64), intent(in ) :: n, k
    real(real64), intent(out) :: bc
    !
    if (n < k .or. k < 0.0_real64) then  ! If k < 0 or n < k, result is 0
        bc = d0
    else if (abs(n - anint(n)) < n*is_int_tol .AND. abs(k - anint(k)) < k*is_int_tol) then  ! both integer floats
        if ( n < max_int32 ) then
            call binomial_i32_r64(int(n, int32), int(k, int32), bc)
        else
            call binomial_i64_r64(int(n, int64), int(k, int64), bc)
        end if
    else if (k < 0.5_real64*n) then
        bc = gamma(n+d1)
        bc = bc / gamma(n+d1-k)
        bc = bc / gamma(k+d1)
    else
        bc = gamma(n+d1)
        bc = bc / gamma(k+d1)
        bc = bc / gamma(n+d1-k)
    end if
    !
  end subroutine
  !
  pure subroutine binomial_r32(n, k, bc)
    real(real32), intent(in ) :: n, k
    real(real32), intent(out) :: bc
    real(real64) :: n64, k64, bc64
    !
    n64 = real(n, real64)
    k64 = real(k, real64)
    call binomial_r64(n64, k64, bc64)
    !
    if (bc64 < real(HUGE(bc), real64)) then
        bc = real(bc64, real32)
    else
        bc = HUGE(bc)
    end if
  end subroutine
  !
  ! ===================================================================================================================
  !
  pure function fbinomial_i32(n, k) result(bc)
    integer(int32), intent(in) :: n, k
    integer(int32) :: bc
    real(real64) :: bc64
    call binomial_i32_r64(n, k, bc64)
    bc = nint(bc64, int32)
  end function  
  !
  pure function fbinomial_i64(n, k) result(bc)
    integer(int64), intent(in) :: n, k
    integer(int64) :: bc
    real(real64) :: bc64
    call binomial_i64_r64(n, k, bc64)
    bc = nint(bc64, int64)
  end function
  !
  pure function fbinomial_r64(n, k) result(bc)
    real(real64), intent(in) :: n, k
    real(real64) :: bc
    call binomial_r64(n, k, bc)
  end function
  !
  pure function fbinomial_r32(n, k) result(bc)
    real(real32), intent(in) :: n, k
    real(real32) :: bc
    call binomial_r32(n, k, bc)
  end function
  !
end module