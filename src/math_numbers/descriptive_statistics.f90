!
! CODE DEVELOPED BY SCOTT E BOYCE 
!                   CONTACT <seboyce@usgs.gov> or <Boyce@engineer.com>
! 
! Routines that calculate the folowing descriptive statistics
!                        Average/Arithmetic Mean
!                        Variance           - Population and Sample 
!                        Standard Deviation - Population and Sample
!                        Skewness           - Population and Sample
!                        Kurtosis           - Population and Sample
!                        Median
!                        Percentile         - Calculated using method described in https://www.itl.nist.gov/div898/handbook/prc/section2/prc262.htm
!                                               k.d = p(n+1)                   | p is 0 to 1 percentile; n is number of values; 
!                                               Y(p) = Y[k] + d(Y[k+1]−Y[k])   | k is integer portion, d is the decimal; Y is the data
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
! Example Functions:
!       av = average(dat)            - Returns the artihmetic mean of dat
!       vr = variance(dat, [sample]) - Returns the population variaiance of dat; 
!                                       if Sample is present and TRUE, 
!                                       then returns sample variance
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
! Example Subroutine:
!   call stats(dat, av, var, skew, kurt, median, P1, P5, P10, P25, P50, P75, P90, P95, P99, P, var_sample, skew_sample, kurt_sample, protect)
!
!   dat is the only required input, 
!   All variables are double precision, except protect is logical
!   
!     dat - Double Precision array that will be described 
!     av           is the Arithmetic Mean             <=|  Variable Group 1
!     var          is the Population Variance           |     This group reads dat, but does not modify.
!     skew         is the Population Skewness           |     Solving one or all these variables requires the same processing power
!     kurt         is the Population Excess Kurtosis    |
!     var_sample   is the Sample Variance               |
!     skew_sample  is the Sample Skewness               |
!     kurt_sample  is the Sample excess Kurtosis      <=|
!
!     protect only applies if any of the following variables are requested, 
!                which require sorting the values in dat.
!             TRUE  - (default) creates a copy of dat, which is sorted -> this keeps dat in its orginal order 
!             FALSE - dat is sorted in place (saves space)
!
!     median is the median of dat       <=|  Variable Group 2
!     P1     is the  1st Percentile       |    This group requires sorting of the values in dat.
!     P5     is the  5th Percentile       |    Solving one or all these variables requires the same processing power
!     P10    is the 10th Percentile       |
!     P25    is the 25th Percentile       |
!     P50    is the 50th Percentile       |
!     P75    is the 75th Percentile       |
!     P90    is the 90th Percentile       |
!     P95    is the 95th Percentile       |
!     P99    is the 99th Percentile       |
!     P      is an array of percentiles <=|
!               from 0 to 1 (eg 0.15 for the 15th percentile), 
!            On return P contains the value at that percentile.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
! ONLINE_STATS Data Type use:               - This allows dynamic changing of descriptive stats
!
!  TYPE(ONLINE_STATS):: stat 
!  call stat%init()               
!
!  call stat%add(val) - add val to statistic momements; Can be a scalar or vector;
!                           val can be real32, real64, real128, int32, int64
!                           Internally, val is converted to real128
!
! x = stat%mean([typ]) - All functions return x as real64
! x = stat%var([typ])       if typ is present, then x returned is the same type as typ
! x = stat%stdev([typ])
! x = stat%skew([typ])
! x = stat%kurt([typ])
! x = stat%var_sample([typ])
! x = stat%stdev_sample([typ])
! x = stat%skew_sample([typ])
! x = stat%kurt_sample([typ])
!
MODULE DESCRIPTIVE_STATISTICS
  USE, INTRINSIC:: ISO_FORTRAN_ENV, ONLY: INT32, INT64, REL64 => REAL64, qp => REAL128
  IMPLICIT NONE
  !
  PRIVATE
  !
  PUBLIC:: ONLINE_STATS
  PUBLIC:: average      ! av = average(dat)
  PUBLIC:: variance     ! vr = variance(dat, sample)
  PUBLIC:: stats        ! call stats(dat, [av], [var], [skew], [kurt], [median], [P1], [P5], [P10], [P25], [P50], [P75], [P90], [P95], [P99], [P], [var_sample], [skew_sample], [kurt_sample])
  !
  INTEGER(INT64),  PARAMETER:: LONG_NEG = -1_int64
  INTEGER(INT64),  PARAMETER:: LONG_ZER =  0_int64
  INTEGER(INT64),  PARAMETER:: LONG_ONE =  1_int64
  !                                     
  REAL(REL64), parameter :: dz  = 0.0_rel64
  REAL(REL64), parameter :: uno = 1.0_rel64
  !
  REAL(qp), parameter :: zer = 0.0_qp
  REAL(qp), parameter :: one = 1.0_qp
  REAL(qp), parameter :: q2  = 2.0_qp
  REAL(qp), parameter :: q3  = 3.0_qp
  REAL(qp), parameter :: q4  = 4.0_qp
  REAL(qp), parameter :: q6  = 6.0_qp
  REAL(qp), parameter :: tol = 1e-30_qp
  !
  ! ----------------------------------------------------------------------------------------
  !
  TYPE ONLINE_STATS
      !
      INTEGER(int64)::         N = LONG_ZER
      REAL(qp), dimension(4):: M = ZER
      !
      CONTAINS
      !
      PROCEDURE, PASS(st):: INIT => INITIALIZE_SG 
      !
      GENERIC::            MERGE => MERGE_IN_SG_qp, MERGE_OUT_SG_qp
      !
      GENERIC::             ADD  => ADD_VAL_SG_int32, ADD_VAL_SG_int64,  &
                                    ADD_VAL_SG_rel64, ADD_VAL_SG_qp,     &
                                    ADD_VEC_SG_int32, ADD_VEC_SG_int64,  &
                                    ADD_VEC_SG_rel64, ADD_VEC_SG_qp   
      
      GENERIC::             MEAN => MEAN_SG_int32,    MEAN_SG_int64, &
                                    MEAN_SG_rel64,    MEAN_SG_qp
      GENERIC::              VAR => VAR_SG_int32,     VAR_SG_int64, &
                                    VAR_SG_rel64,     VAR_SG_qp
      GENERIC::            stdev => STDEV_SG_int32,   STDEV_SG_int64, &
                                    STDEV_SG_rel64,   STDEV_SG_qp
      GENERIC::             skew => SKEW_SG_int32,    SKEW_SG_int64, &
                                    SKEW_SG_rel64,    SKEW_SG_qp
      GENERIC::             kurt => KURT_SG_int32,    KURT_SG_int64, &
                                    KURT_SG_rel64,    KURT_SG_qp
      GENERIC::       var_sample => VAR_SAMPLE_SG_int32,   VAR_SAMPLE_SG_int64, &
                                    VAR_SAMPLE_SG_rel64,   VAR_SAMPLE_SG_qp
      GENERIC::     stdev_sample => STDEV_SAMPLE_SG_int32, STDEV_SAMPLE_SG_int64, &
                                    STDEV_SAMPLE_SG_rel64, STDEV_SAMPLE_SG_qp
      GENERIC::      skew_sample => SKEW_SAMPLE_SG_int32,  SKEW_SAMPLE_SG_int64, &
                                    SKEW_SAMPLE_SG_rel64,  SKEW_SAMPLE_SG_qp
      GENERIC::      kurt_sample => KURT_SAMPLE_SG_int32,  KURT_SAMPLE_SG_int64, &
                                    KURT_SAMPLE_SG_rel64,  KURT_SAMPLE_SG_qp
      !
      GENERIC:: ASSIGNMENT(=)    => EQUAL_st_st
      GENERIC:: OPERATOR(+)      => ADD_st_st
      !
      PROCEDURE, PASS(st), PRIVATE:: ADD_VAL_SG_int32, ADD_VAL_SG_int64, &
                                     ADD_VAL_SG_rel64, ADD_VAL_SG_qp
      PROCEDURE, PASS(st), PRIVATE:: ADD_VEC_SG_int32, ADD_VEC_SG_int64, &
                                     ADD_VEC_SG_rel64, ADD_VEC_SG_qp
      PROCEDURE, PASS(st), PRIVATE:: MEAN_SG_int32,    MEAN_SG_int64, &
                                     MEAN_SG_rel64,    MEAN_SG_qp
      PROCEDURE, PASS(st), PRIVATE:: VAR_SG_int32,     VAR_SG_int64, &
                                     VAR_SG_rel64,     VAR_SG_qp
      PROCEDURE, PASS(st), PRIVATE:: STDEV_SG_int32,   STDEV_SG_int64, &
                                     STDEV_SG_rel64,   STDEV_SG_qp
      PROCEDURE, PASS(st), PRIVATE:: SKEW_SG_int32,    SKEW_SG_int64, &
                                     SKEW_SG_rel64,    SKEW_SG_qp
      PROCEDURE, PASS(st), PRIVATE:: KURT_SG_int32,    KURT_SG_int64, &
                                     KURT_SG_rel64,    KURT_SG_qp
      PROCEDURE, PASS(st), PRIVATE:: VAR_SAMPLE_SG_int32,   VAR_SAMPLE_SG_int64, &
                                     VAR_SAMPLE_SG_rel64,   VAR_SAMPLE_SG_qp
      PROCEDURE, PASS(st), PRIVATE:: STDEV_SAMPLE_SG_int32, STDEV_SAMPLE_SG_int64, &
                                     STDEV_SAMPLE_SG_rel64, STDEV_SAMPLE_SG_qp
      PROCEDURE, PASS(st), PRIVATE:: SKEW_SAMPLE_SG_int32,  SKEW_SAMPLE_SG_int64, &
                                     SKEW_SAMPLE_SG_rel64,  SKEW_SAMPLE_SG_qp
      PROCEDURE, PASS(st), PRIVATE:: KURT_SAMPLE_SG_int32,  KURT_SAMPLE_SG_int64, &
                                     KURT_SAMPLE_SG_rel64,  KURT_SAMPLE_SG_qp
      PROCEDURE, PASS(st), PRIVATE:: MERGE_IN_SG_qp, MERGE_OUT_SG_qp
      PROCEDURE, PASS(st), PRIVATE:: EQUAL_st_st
      PROCEDURE,           PRIVATE:: ADD_st_st
  END TYPE
  !
  INTEGER, PARAMETER:: QSORT_PARTITION_MIN    = 64   ! If (R - L + 1 <= SIZ), then stop QSORT partitioning and use ISORT on partition 
  INTEGER, PARAMETER:: ISORT_SMALL_ARRAY_SIZE = 96   ! If (DIM <= SIZ), then use ISORT instead of QSORT 
  INTEGER, PARAMETER:: QSORT_WELL_SORTED_LIM  = 10   ! LIM = DIM/QSORT_WELL_SORTED_LIM, If COUNT( A(I) > A(I+1) ) < LIM, then use ISORT instead of QSORT -> Note this ignores repeated elements --> A value of 20 means that no more than 5% of elements fail A(I) > A(I+1); or 10% of values are not sorted.
  !
  CONTAINS
  !
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  ! Stand Alone Routines
  !
  PURE FUNCTION average(dat) result(av)
    REAL(REL64), dimension(:), contiguous, intent(in):: dat
    REAL(REL64):: av
    INTEGER:: I
    !
    if( size(dat) == 1 ) then
                         av = dat(1)
    else
                         av = (dat(1) + dat(2)) * 0.5_rel64
    end if
    !
    do i=3, size(dat)
                 av = av + (dat(i) - av)/real(i, rel64)
    end do
    !
  END FUNCTION
  !
  PURE FUNCTION variance(dat, sample) result(vr)
    REAL(REL64), dimension(:), contiguous, intent(in):: dat
    LOGICAL,                     optional, intent(in):: sample
    REAL(REL64):: vr
    REAL(REL64):: av1, av2
    INTEGER:: I
    !
    if( size(dat) == 1 ) then
                         vr = dz
                         return
    end if
    !
    av2 = (dat(1) + dat(2)) * 0.5_rel64
    vr  = (dat(2) - dat(1))*(dat(2) - av2)
    !
    do i=3, size(dat)
                 av1 = av2
                 av2 = av1 + (dat(i) - av1)/real(i, rel64)
                 !
                 vr  = vr  + (dat(i) - av1)*(dat(i) - av2)
    end do
    I = 0
    if(present(sample)) then
            if(sample) I = 1
    end if
    if( I == 0 ) then
                 vr  = vr / real(size(dat),rel64)
    else
                 vr  = vr / real(size(dat)-1,rel64)
    end if
    !
  END FUNCTION
  !
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  ! Stats Driver Routine
  !
  PURE SUBROUTINE stats(dat, av, var, skew, kurt, median, P1, P5, P10, P25, P50, P75, P90, P95, P99, P, var_sample, skew_sample, kurt_sample, protect)
    REAL(REL64), dimension(:), contiguous,           intent(inout):: dat        ! dat array to describe
    REAL(REL64),                           optional, intent(  out):: av         ! Arithmetic Mean         <=| 
    REAL(REL64),                           optional, intent(  out):: var        ! Variance - population   <=|
    REAL(REL64),                           optional, intent(  out):: skew       ! Skewness - population   <=|
    REAL(REL64),                           optional, intent(  out):: kurt       ! Kurtosis - popul-excess <=|== Solved simultaneously; no over head for one or more
    REAL(REL64),                           optional, intent(  out):: var_sample ! Variance - sample       <=| 
    REAL(REL64),                           optional, intent(  out):: skew_sample! Skewness - sample       <=| 
    REAL(REL64),                           optional, intent(  out):: kurt_sample! Kurtosis - sampl-excess <=| 
    LOGICAL,                               optional, intent(in   ):: protect    ! <-v-v-> The folowing variables require sorting dat to obtain result, but there is no overhead for selecting more than 1 variable       
                                                                                !         protect indicates if the dat array is sortted or a copy is created.
                                                                                !         => False incates that dat sorted if any of the following are requested
                                                                                !         => True sorts a copy of dat (default when not specified)
    REAL(REL64),                           optional, intent(  out):: median     ! 
    REAL(REL64),                           optional, intent(  out):: P1         !  1st Percentile - Caculated using NIST recommend method
    REAL(REL64),                           optional, intent(  out):: P5         !  5th Percentile
    REAL(REL64),                           optional, intent(  out):: P10        ! 10th Percentile
    REAL(REL64),                           optional, intent(  out):: P25        ! 25th Percentile
    REAL(REL64),                           optional, intent(  out):: P50        ! 50th Percentile
    REAL(REL64),                           optional, intent(  out):: P75        ! 75th Percentile
    REAL(REL64),                           optional, intent(  out):: P90        ! 90th Percentile
    REAL(REL64),                           optional, intent(  out):: P95        ! 95th Percentile
    REAL(REL64),                           optional, intent(  out):: P99        ! 99th Percentile
    REAL(REL64), dimension(:), contiguous, optional, intent(inout):: P          ! Incoming is percentiles, outgoing is the value at that percentile. Values must be between 0 and 1.
    INTEGER:: dim
    REAL(qp), dimension(4):: M
    LOGICAL:: COPY
    !
    dim = size(dat)
    !
    if(present(av         ) .OR. &
       present(var        ) .OR. &
       present(skew       ) .OR. &
       present(kurt       ) .OR. &
       present(skew_sample) .OR. &
       present(kurt_sample) .OR. &
       present(var_sample )) CALL moments_dp_qp(dim, dat, M)
    !
    if(present(av)) av = real(M(1), rel64)
    !
    if(present(var))        var        = real(M(2)/real(dim,  qp), rel64)
    if(present(var_sample)) var_sample = real(M(2)/real(dim-1,qp), rel64)
    !
    if(present(skew)) then   ! Get population skew
        BLOCK
           REAL(qp):: tmp
                      if( M(2) > tol ) then             !M(2) always positive
                          tmp = sqrt( M(2)*M(2)*M(2) )
                          tmp = M(3) / tmp                 ! = M(3) / M(2)**1.5
                          tmp = tmp * sqrt( real(dim, qp) )
                          skew = real( tmp, rel64 )
                      else
                          skew = dz
                      end if
        END BLOCK
    end if
    !
    if(present(kurt)) then   ! Get population kurtosis
        BLOCK
           REAL(qp):: tmp
                      tmp = M(2)*M(2)
                      if( tmp > tol ) then
                          tmp = M(4) / tmp
                          tmp = tmp * real(dim, qp)
                          tmp = tmp - q3
                          kurt = real( tmp, rel64 )
                      else
                          kurt = dz
                      end if
        END BLOCK
    end if
    !
    if(present(skew_sample)) then   ! Get sample skew
        BLOCK
           REAL(qp):: tmp
           REAL(qp):: norm
                      if( M(2) > tol ) then                      ! M(2) always positive
                          norm = real(dim, qp)/real(dim-2, qp)      ! norm = n(n-1)^-0.5 / (n-2)
                          norm = norm * sqrt(real(dim-1, qp))
                          !
                          tmp = sqrt( M(2)*M(2)*M(2) )
                          tmp = M(3) / tmp                 ! = M(3) / M(2)**1.5
                          tmp = tmp * norm
                          skew_sample = real( tmp, rel64 )
                      else
                          skew_sample = dz
                      end if
        END BLOCK
    end if
    !
    if(present(kurt_sample)) then   ! Get sample kurtosis
        BLOCK
           REAL(qp):: tmp
           REAL(qp):: norm
                      tmp = M(2)*M(2)
                      if( tmp > tol ) then
                          norm = real(dim-1, qp)/real(dim-2, qp)              ! norm = n(n+1)(n-1) / (n-2)(n-3)
                          norm = norm * real(dim, qp)/real(dim-3, qp)
                          norm = norm * real(dim+1, qp)
                          !
                          tmp = M(4) / tmp
                          tmp = tmp * norm
                          !
                          norm = real(dim-1, qp)/real(dim-2, qp)              ! norm = (n-1)^2 / (n-2)(n-3)
                          norm = norm * real(dim-1, qp)/real(dim-3, qp)
                          !
                          tmp = tmp - q3*norm
                          kurt_sample = real( tmp, rel64 )
                      else
                          kurt_sample = dz
                      end if
        END BLOCK
    end if
    !
    if(present(median) .OR. &
       present( P1)    .OR. &
       present( P5)    .OR. &
       present(P10)    .OR. &
       present(P25)    .OR. &
       present(P50)    .OR. &
       present(P75)    .OR. &
       present(P90)    .OR. &
       present(P95)    .OR. &
       present(P99)    .OR. &
       present(P  )          ) then
                               COPY = .TRUE.
                               if(present(protect)) COPY = protect
                               !
                               if(COPY) then
                                   BLOCK
                                        REAL(REL64), dimension(dim):: dat_copy
                                        dat_copy = dat
                                        call percentiles(dim, dat_copy, median, P1, P5, P10, P25, P50, P75, P90, P95, P99, P) 
                                   END BLOCK
                               else
                                        call percentiles(dim, dat, median, P1, P5, P10, P25, P50, P75, P90, P95, P99, P) 
                               end if
                               
                               
    end if
    !
  END SUBROUTINE
  !
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  ! Stats Routines
  !
  PURE SUBROUTINE moments_dp_qp(dim, dat, M) 
    INTEGER,                     intent(in ):: dim
    REAL(REL64), DIMENSION(dim), intent(in ):: dat
    REAL(qp),    DIMENSION(4),   intent(out):: M
    REAL(qp):: tmp, N, divN
    INTEGER:: I
    !
    M = ZER
    DO i=1, dim
       N    = real(i,      qp)
       tmp  = real(dat(i), qp) - M(1)
       divN = tmp / N
       !
       tmp = tmp * divN * real(i-1, qp)
       !
       M(1) = M(1) + divN
       !
       M(4) = M(4) + tmp * divN*divN * (N*N - q3*N + q3) + q6*divN*divN*M(2) - q4*divN*M(3)
       !
       M(3) = M(3) + tmp * divN * (N - q2) - q3*divN*M(2)
       !
       M(2) = M(2) + tmp
    END DO
    !    
  END SUBROUTINE
  !
  !##########################################################################################################################
  !
  PURE SUBROUTINE percentiles(dim, dat, median, P1, P5, P10, P25, P50, P75, P90, P95, P99, P) 
    INTEGER,                     intent(in   ):: dim
    REAL(REL64), dimension(dim), intent(inout):: dat
    REAL(REL64),       optional, intent(  out):: median 
    REAL(REL64),       optional, intent(  out):: P1         !  1st Percentile - Caculated using NIST recommend method
    REAL(REL64),       optional, intent(  out):: P5         !  5th Percentile
    REAL(REL64),       optional, intent(  out):: P10        ! 10th Percentile
    REAL(REL64),       optional, intent(  out):: P25        ! 25th Percentile
    REAL(REL64),       optional, intent(  out):: P50        ! 50th Percentile
    REAL(REL64),       optional, intent(  out):: P75        ! 75th Percentile
    REAL(REL64),       optional, intent(  out):: P90        ! 90th Percentile
    REAL(REL64),       optional, intent(  out):: P95        ! 95th Percentile
    REAL(REL64),       optional, intent(  out):: P99        ! 99th Percentile
    REAL(REL64), dimension(:), contiguous, optional, intent(inout):: P          ! Incoming is percentiles, outgoing is the value at that percentile. Values must be between 0 and 1.
    !
    SELECT CASE(SORT_TYPE(dim, dat))
    CASE(1);              call isort_1d_rel64(dim,dat)
    CASE(2);              call qsort_1d_rel64(dim, dat, 1, dim, MAX_RECUR(dim))
    END SELECT
    !
    call percentiles_sorted(dim, dat, median, P1, P5, P10, P25, P50, P75, P90, P95, P99, P) 
    !
  END SUBROUTINE 
  !
  !##########################################################################################################################
  ! dat must be sorted to produce correct results
  PURE SUBROUTINE percentiles_sorted(dim, dat, median, P1, P5, P10, P25, P50, P75, P90, P95, P99, P) 
    INTEGER,                     intent(in   ):: dim
    REAL(REL64), dimension(dim), intent(in   ):: dat
    REAL(REL64),       optional, intent(  out):: median 
    REAL(REL64),       optional, intent(  out):: P1         !  1st Percentile - Caculated using NIST recommend method
    REAL(REL64),       optional, intent(  out):: P5         !  5th Percentile
    REAL(REL64),       optional, intent(  out):: P10        ! 10th Percentile
    REAL(REL64),       optional, intent(  out):: P25        ! 25th Percentile
    REAL(REL64),       optional, intent(  out):: P50        ! 50th Percentile
    REAL(REL64),       optional, intent(  out):: P75        ! 75th Percentile
    REAL(REL64),       optional, intent(  out):: P90        ! 90th Percentile
    REAL(REL64),       optional, intent(  out):: P95        ! 95th Percentile
    REAL(REL64),       optional, intent(  out):: P99        ! 99th Percentile
    REAL(REL64), dimension(:), contiguous, optional, intent(inout):: P          ! Incoming is percentiles, outgoing is the value at that percentile. Values must be between 0 and 1.
    INTEGER:: i
    !
    if(present(median)) then
      i = SHIFTR(dim, 1)
      IF( IAND(dim,1) == 1 ) THEN ! its odd
          median   = dat(i)
      else
          median = (dat(i) + dat(i+1)) * 0.5_rel64
      end if
    end if
    !
    ! Perentiles calculated as described in https://en.wikipedia.org/wiki/Percentile#Third_variant,_%7F'%22%60UNIQ--postMath-00000054-QINU%60%22'%7F
    !
    if(present(P5 )) P1  = Ptile(dim, dat, 0.01_rel64)
    if(present(P5 )) P5  = Ptile(dim, dat, 0.05_rel64)
    if(present(P5 )) P10 = Ptile(dim, dat, 0.10_rel64)
    if(present(P25)) P25 = Ptile(dim, dat, 0.25_rel64)
    if(present(P50)) P50 = Ptile(dim, dat, 0.50_rel64)
    if(present(P75)) P75 = Ptile(dim, dat, 0.75_rel64)
    if(present(P95)) P90 = Ptile(dim, dat, 0.90_rel64)
    if(present(P95)) P95 = Ptile(dim, dat, 0.95_rel64)
    if(present(P95)) P99 = Ptile(dim, dat, 0.99_rel64)
    !               
    if(present(P)) then
        do i=1, size(P)
                P(i) = Ptile(dim, dat, P(i)*0.01_rel64)
        end do
    end if
    !
  END SUBROUTINE
  !
  PURE FUNCTION Ptile(dim, A, P) result(d)          ! Return value for percentile, A must be sorted and 0<P<1
    INTEGER,                     intent(in   ):: dim
    REAL(REL64), dimension(dim), intent(in   ):: A
    REAL(REL64),                 intent(in   ):: P
    REAL(REL64):: d
    INTEGER:: K
    !
    d = P * real(dim+1, rel64)
    K = int(d)
    d = d - real(K, rel64)
    !
    if    (K == 0  ) then
                     d = A(1)
    elseif(K >= dim) then
                     d = A(dim)
    else
        d = A(K) + d*(A(K+1) - A(K))
    end if
    !
  END FUNCTION
  !
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  ! ONLINE_STATS Routines
  !
  PURE ELEMENTAL SUBROUTINE INITIALIZE_SG(st)
    CLASS(ONLINE_STATS), intent(inout):: st
    st%N = LONG_ZER
    st%M = ZER
  END SUBROUTINE
  !
  !##########################################################################################################################
  !
  PURE SUBROUTINE ADD_VAL_SG_int32(st, val)
    CLASS(ONLINE_STATS), intent(inout):: st
    INTEGER(int32),      intent(in   ):: val
    REAL(qp):: qv
    qv = real(val, qp)
    call ADD_VAL_SG_qp(st, qv)
  END SUBROUTINE 
  !
  PURE SUBROUTINE ADD_VAL_SG_int64(st, val)
    CLASS(ONLINE_STATS), intent(inout):: st
    INTEGER(int64),      intent(in   ):: val
    REAL(qp):: qv
    qv = real(val, qp)
    call ADD_VAL_SG_qp(st, qv)
  END SUBROUTINE 
  !
  PURE SUBROUTINE ADD_VAL_SG_rel64(st, val)
    CLASS(ONLINE_STATS), intent(inout):: st
    REAL(rel64),         intent(in   ):: val
    REAL(qp):: qv
    qv = real(val, qp)
    call ADD_VAL_SG_qp(st, qv)
  END SUBROUTINE 
  !
  !##########################################################################################################################
  !
  PURE SUBROUTINE ADD_VEC_SG_int32(st, val)
    CLASS(ONLINE_STATS), intent(inout):: st
    INTEGER(int32), dimension(:), contiguous, intent(in):: val
    REAL(qp):: qv
    INTEGER:: i
    do i=1, size(val)
       qv = real(val(i), qp)
       call ADD_VAL_SG_qp(st, qv)
    end do
  END SUBROUTINE 
  !
  PURE SUBROUTINE ADD_VEC_SG_int64(st, val)
    CLASS(ONLINE_STATS), intent(inout):: st
    INTEGER(int64), dimension(:), contiguous, intent(in):: val
    REAL(qp):: qv
    INTEGER:: i
    do i=1, size(val)
       qv = real(val(i), qp)
       call ADD_VAL_SG_qp(st, qv)
    end do
  END SUBROUTINE 
  !
  PURE SUBROUTINE ADD_VEC_SG_rel64(st, val)
    CLASS(ONLINE_STATS), intent(inout):: st
    REAL(rel64), dimension(:), contiguous, intent(in):: val
    REAL(qp):: qv
    INTEGER:: i
    do i=1, size(val)
       qv = real(val(i), qp)
       call ADD_VAL_SG_qp(st, qv)
    end do
  END SUBROUTINE 
  !
  PURE SUBROUTINE ADD_VEC_SG_qp(st, val)
    CLASS(ONLINE_STATS), intent(inout):: st
    REAL(qp), dimension(:), contiguous, intent(in):: val
    INTEGER:: i
    do i=1, size(val)
       call ADD_VAL_SG_qp(st, val(i))
    end do
  END SUBROUTINE 
  !
  !##########################################################################################################################
  !
  PURE SUBROUTINE ADD_VAL_SG_qp(st, val)
    CLASS(ONLINE_STATS), intent(inout):: st
    REAL(qp),            intent(in   ):: val
    REAL(qp):: tmp, N, divN
    !
    st%N = st%N + LONG_ONE
    N    = real(st%N, qp)
    !
    tmp  = val - st%M(1)
    divN = tmp / N
    !
    tmp = tmp * divN * real(st%N+LONG_NEG, qp)  ! N-1
    !
    st%M(1) = st%M(1) + divN
    !
    st%M(4) = st%M(4) + tmp * divN*divN * (N*N - q3*N + q3) + q6*divN*divN*st%M(2) - q4*divN*st%M(3)
    !
    st%M(3) = st%M(3) + tmp * divN * (N - q2) - q3*divN*st%M(2)
    !
    st%M(2) = st%M(2) + tmp
    !  
  END SUBROUTINE 
  !
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !#####################################################################################################
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !
  PURE SUBROUTINE MERGE_OUT_SG_qp(st, st1, st2)
    CLASS(ONLINE_STATS), intent(out):: st
    CLASS(ONLINE_STATS), intent(in ):: st1, st2
    REAL(qp), dimension(4):: M
    REAL(qp):: N, N1, N2, dif
    !
    if(st1%N + st2%N < LONG_ONE) then
                        call st%init()
                        return
    end if
    if(st1%N < LONG_ONE) then
                        st%N = st2%N
                        st%M = st2%M
                        return
    end if
    if(st2%N < LONG_ONE) then
                        st%N = st1%N
                        st%M = st1%M
                        return
    end if
    !
    N1 = real(st1%N, qp)
    N2 = real(st2%N, qp)
    N  = real(st1%N + st2%N,  qp)
    !
    dif = st2%M(1) - st1%M(1)
    !
    M(1) = (N1*st1%M(1) + N2*st2%M(1)) / N
    !
    M(2) = st1%M(2) + st2%M(2) + dif*dif*N1*N2/N
    !
    M(3) = st1%M(3) + st2%M(3) + (N1-N2)*dif*dif*dif*(N1/N)*(N2/N)   ! (N1-N2)*dif*dif*dif*N1*N2*/(N*N)
    !
    M(3) = M(3) + q3*dif*(N1*st2%M(2) - N2*st1%M(2)) / N
    !
    M(4) = st1%M(4) + st2%M(4) + dif*dif*dif*dif*(N1/N)*(N2/N)*(N1*N1 - N1*N2 + N2*N2) / N   ! dif*dif*dif*dif*N1*N2*(N1*N1 - N1*N2 + N2*N2) / (N*N*N)
    !
    M(4) = M(4) + q6*dif*dif*(N1*N1*st2%M(2) + N2*N2*st1%M(2))/(N*N)
    !
    M(4) = M(4) + q4*dif*(N1*st2%M(3) - N2*st1%M(3)) / N
    !  
    st%N = st1%N + st2%N
    st%M = M
    !  
  END SUBROUTINE  
  !
  !#####################################################################################################
  !
  PURE SUBROUTINE MERGE_IN_SG_qp(st, st1)
    CLASS(ONLINE_STATS), intent(inout):: st
    CLASS(ONLINE_STATS), intent(in   ):: st1
    !
    CALL MERGE_OUT_SG_qp(st, st, st1)
    !  
  END SUBROUTINE  
  !
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !#####################################################################################################
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !                          (OUT, IN)
  PURE SUBROUTINE EQUAL_st_st(st, st_in)
    CLASS(ONLINE_STATS), intent(in   ):: st_in
    CLASS(ONLINE_STATS), intent(inout):: st
    !
    st%N = st_in%N
    st%M = st_in%M
    !
  END SUBROUTINE
  !
  PURE FUNCTION ADD_st_st(st1, st2) RESULT(stout)
     CLASS(ONLINE_STATS), intent(in):: st1, st2
     TYPE (ONLINE_STATS):: stout
     !
     call MERGE_OUT_SG_qp(stout, st1, st2)
     !
  END FUNCTION
  !
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !#####################################################################################################
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !
  PURE FUNCTION MEAN_SG_rel64(st, typ) result(val)
    CLASS(ONLINE_STATS),   intent(in):: st
    REAL(rel64), optional, intent(in):: typ
    REAL(rel64):: val
    !
    val = real(st%M(1), rel64)
    !  
  END FUNCTION
  !
  PURE FUNCTION MEAN_SG_int64(st, typ) result(val)
    CLASS(ONLINE_STATS), intent(in):: st
    INTEGER(int64),      intent(in):: typ
    INTEGER(int64):: val
    !
    val = int(st%M(1), int64)
    !  
  END FUNCTION
  !
  PURE FUNCTION MEAN_SG_int32(st, typ) result(val)
    CLASS(ONLINE_STATS), intent(in):: st
    INTEGER(int32),      intent(in):: typ
    INTEGER(int32):: val
    !
    val = int(st%M(1), int32)
    !  
  END FUNCTION
  !
  PURE FUNCTION MEAN_SG_qp(st, typ) result(val)
    CLASS(ONLINE_STATS), intent(in):: st
    REAL(qp),            intent(in):: typ
    REAL(qp):: val
    !
    val = st%M(1)
    !  
  END FUNCTION
  !
  !##########################################################################################################################
  !
  PURE FUNCTION VAR_SG_rel64(st, typ) result(val)
    CLASS(ONLINE_STATS),   intent(in):: st
    REAL(rel64), optional, intent(in):: typ
    REAL(rel64):: val
    !
    if(st%N < LONG_ONE) then 
        val = dz
    else
        val = real(st%M(2)/real(st%N,  qp), rel64)
    end if
    !  
  END FUNCTION
  !
  PURE FUNCTION VAR_SG_int64(st, typ) result(val)
    CLASS(ONLINE_STATS), intent(in):: st
    INTEGER(int64),      intent(in):: typ
    INTEGER(int64):: val
    !
    if(st%N < LONG_ONE) then 
        val = LONG_ZER
    else
        val = int(st%M(2)/real(st%N,  qp), int64)
    end if
    !  
  END FUNCTION
  !
  PURE FUNCTION VAR_SG_int32(st, typ) result(val)
    CLASS(ONLINE_STATS), intent(in):: st
    INTEGER(int32),      intent(in):: typ
    INTEGER(int32):: val
    !
    if(st%N < LONG_ONE) then 
        val = 0_int32
    else
        val = int(st%M(2)/real(st%N,  qp), int32)
    end if
    !  
  END FUNCTION
  !
  PURE FUNCTION VAR_SG_qp(st, typ) result(val)
    CLASS(ONLINE_STATS), intent(in):: st
    REAL(qp),            intent(in):: typ
    REAL(qp):: val
    !
    if(st%N < LONG_ONE) then 
        val = ZER
    else
        val = st%M(2)/real(st%N,  qp)
    end if
    !  
  END FUNCTION
  !
  !##########################################################################################################################
  !
  PURE FUNCTION STDEV_SG_rel64(st, typ) result(val)
    CLASS(ONLINE_STATS),   intent(in):: st
    REAL(rel64), optional, intent(in):: typ
    REAL(rel64):: val
    !
    if(st%N < LONG_ONE) then 
        val = dz
    else
        val = real(SQRT(st%M(2)/real(st%N,  qp)), rel64)
    end if
    !  
  END FUNCTION
  !
  PURE FUNCTION STDEV_SG_int64(st, typ) result(val)
    CLASS(ONLINE_STATS), intent(in):: st
    INTEGER(int64),      intent(in):: typ
    INTEGER(int64):: val
    !
    if(st%N < LONG_ONE) then 
        val = LONG_ZER
    else
        val = int(SQRT(st%M(2)/real(st%N,  qp)), int64)
    end if
    !  
  END FUNCTION
  !
  PURE FUNCTION STDEV_SG_int32(st, typ) result(val)
    CLASS(ONLINE_STATS), intent(in):: st
    INTEGER(int32),      intent(in):: typ
    INTEGER(int32):: val
    !
    if(st%N < LONG_ONE) then 
        val = 0_int32
    else
        val = int(SQRT(st%M(2)/real(st%N,  qp)), int32)
    end if
    !  
  END FUNCTION
  !
  PURE FUNCTION STDEV_SG_qp(st, typ) result(val)
    CLASS(ONLINE_STATS), intent(in):: st
    REAL(qp),            intent(in):: typ
    REAL(qp):: val
    !
    if(st%N < LONG_ONE) then 
        val = ZER
    else
        val = SQRT(st%M(2)/real(st%N,  qp))
    end if
    !  
  END FUNCTION
  !
  !##########################################################################################################################
  !
  PURE FUNCTION SKEW_SG_rel64(st, typ) result(val)
    CLASS(ONLINE_STATS),   intent(in):: st
    REAL(rel64), optional, intent(in):: typ
    REAL(rel64):: val
    REAL(qp):: tmp
    !
    if( st%M(2) > tol ) then                    ! M(2) always positive
        tmp = sqrt( st%M(2)*st%M(2)*st%M(2) )
        tmp = st%M(3) / tmp                    ! = M(3) / M(2)**1.5
        tmp = tmp * sqrt( real(st%N, qp) )
        val = real( tmp, rel64 )
    else
        val = dz
    end if
    !  
  END FUNCTION
  !
  PURE FUNCTION SKEW_SG_int64(st, typ) result(val)
    CLASS(ONLINE_STATS), intent(in):: st
    INTEGER(int64),      intent(in):: typ
    INTEGER(int64):: val
    REAL(qp):: tmp
    !
    if( st%M(2) > tol ) then                    ! M(2) always positive
        tmp = sqrt( st%M(2)*st%M(2)*st%M(2) )
        tmp = st%M(3) / tmp                    ! = M(3) / M(2)**1.5
        tmp = tmp * sqrt( real(st%N, qp) )
        val = int(tmp, int64)
    else
        val = LONG_ZER
    end if
    !  
  END FUNCTION
  !
  PURE FUNCTION SKEW_SG_int32(st, typ) result(val)
    CLASS(ONLINE_STATS), intent(in):: st
    INTEGER(int32),      intent(in):: typ
    INTEGER(int32):: val
    REAL(qp):: tmp
    !
    if( st%M(2) > tol ) then                    ! M(2) always positive
        tmp = sqrt( st%M(2)*st%M(2)*st%M(2) )
        tmp = st%M(3) / tmp                    ! = M(3) / M(2)**1.5
        tmp = tmp * sqrt( real(st%N, qp) )
        val = int(tmp, int32)
    else
        val = 0_int32
    end if
    !  
  END FUNCTION
  !
  PURE FUNCTION SKEW_SG_qp(st, typ) result(val)
    CLASS(ONLINE_STATS), intent(in):: st
    REAL(qp),            intent(in):: typ
    REAL(qp):: val
    !
    if( st%M(2) > tol ) then                    ! M(2) always positive
        val = sqrt( st%M(2)*st%M(2)*st%M(2) )
        val = st%M(3) / val                    ! = M(3) / M(2)**1.5
        val = val * sqrt( real(st%N, qp) )
    else
        val = zer
    end if
    !  
  END FUNCTION
  !
  !##########################################################################################################################
  !
  PURE FUNCTION KURT_SG_rel64(st, typ) result(val)
    CLASS(ONLINE_STATS),   intent(in):: st
    REAL(rel64), optional, intent(in):: typ
    REAL(rel64):: val
    REAL(qp):: tmp
    !
    tmp = st%M(2)*st%M(2)
    if( tmp > tol ) then
        tmp = st%M(4) / tmp
        tmp = tmp * real(st%N, qp)
        tmp = tmp - q3
        val = real( tmp, rel64 )
    else
        val = dz
    end if
    !  
  END FUNCTION
  !
  PURE FUNCTION KURT_SG_int64(st, typ) result(val)
    CLASS(ONLINE_STATS), intent(in):: st
    INTEGER(int64),      intent(in):: typ
    INTEGER(int64):: val
    REAL(qp):: tmp
    !
    tmp = st%M(2)*st%M(2)
    if( tmp > tol ) then
        tmp = st%M(4) / tmp
        tmp = tmp * real(st%N, qp)
        tmp = tmp - q3
        val = int( tmp, int64 )
    else
        val = LONG_ZER
    end if
    !  
  END FUNCTION
  !
  PURE FUNCTION KURT_SG_int32(st, typ) result(val)
    CLASS(ONLINE_STATS), intent(in):: st
    INTEGER(int32),      intent(in):: typ
    INTEGER(int32):: val
    REAL(qp):: tmp
    !
    tmp = st%M(2)*st%M(2)
    if( tmp > tol ) then
        tmp = st%M(4) / tmp
        tmp = tmp * real(st%N, qp)
        tmp = tmp - q3
        val = int( tmp, int32 )
    else
        val = 0_int32
    end if
    !  
  END FUNCTION
  !
  PURE FUNCTION KURT_SG_qp(st, typ) result(val)
    CLASS(ONLINE_STATS), intent(in):: st
    REAL(qp),            intent(in):: typ
    REAL(qp):: val
    !
    val = st%M(2)*st%M(2)
    if( val > tol ) then
        val = st%M(4) / val
        val = val * real(st%N, qp)
        val = val - q3
    else
        val = zer
    end if
    !  
  END FUNCTION
  !
  !##########################################################################################################################
  !
  PURE FUNCTION VAR_SAMPLE_SG_rel64(st, typ) result(val)
    CLASS(ONLINE_STATS),   intent(in):: st
    REAL(rel64), optional, intent(in):: typ
    REAL(rel64):: val
    !
    if(st%N < LONG_ONE) then 
        val = dz
    else
        val = real(st%M(2)/real(st%N+LONG_NEG,  qp), rel64)
    end if
    !  
  END FUNCTION
  !
  PURE FUNCTION VAR_SAMPLE_SG_int64(st, typ) result(val)
    CLASS(ONLINE_STATS), intent(in):: st
    INTEGER(int64),      intent(in):: typ
    INTEGER(int64):: val
    !
    if(st%N < LONG_ONE) then 
        val = LONG_ZER
    else
        val = int(st%M(2)/real(st%N+LONG_NEG,  qp), int64)
    end if
    !  
  END FUNCTION
  !
  PURE FUNCTION VAR_SAMPLE_SG_int32(st, typ) result(val)
    CLASS(ONLINE_STATS), intent(in):: st
    INTEGER(int32),      intent(in):: typ
    INTEGER(int32):: val
    !
    if(st%N < LONG_ONE) then 
        val = 0_int32
    else
        val = int(st%M(2)/real(st%N+LONG_NEG,  qp), int32)
    end if
    !  
  END FUNCTION
  !
  PURE FUNCTION VAR_SAMPLE_SG_qp(st, typ) result(val)
    CLASS(ONLINE_STATS), intent(in):: st
    REAL(qp),            intent(in):: typ
    REAL(qp):: val
    !
    if(st%N < LONG_ONE) then 
        val = ZER
    else
        val = st%M(2)/real(st%N+LONG_NEG,  qp)
    end if
    !  
  END FUNCTION
  !
  !##########################################################################################################################
  !
  PURE FUNCTION STDEV_SAMPLE_SG_rel64(st, typ) result(val)
    CLASS(ONLINE_STATS),   intent(in):: st
    REAL(rel64), optional, intent(in):: typ
    REAL(rel64):: val
    !
    if(st%N < LONG_ONE) then 
        val = dz
    else
        val = real(SQRT(st%M(2)/real(st%N+LONG_NEG,  qp)), rel64)
    end if
    !  
  END FUNCTION
  !
  PURE FUNCTION STDEV_SAMPLE_SG_int64(st, typ) result(val)
    CLASS(ONLINE_STATS), intent(in):: st
    INTEGER(int64),      intent(in):: typ
    INTEGER(int64):: val
    !
    if(st%N < LONG_ONE) then 
        val = LONG_ZER
    else
        val = int(SQRT(st%M(2)/real(st%N+LONG_NEG,  qp)), int64)
    end if
    !  
  END FUNCTION
  !
  PURE FUNCTION STDEV_SAMPLE_SG_int32(st, typ) result(val)
    CLASS(ONLINE_STATS), intent(in):: st
    INTEGER(int32),      intent(in):: typ
    INTEGER(int32):: val
    !
    if(st%N < LONG_ONE) then 
        val = 0_int32
    else
        val = int(SQRT(st%M(2)/real(st%N+LONG_NEG,  qp)), int32)
    end if
    !  
  END FUNCTION
  !
  PURE FUNCTION STDEV_SAMPLE_SG_qp(st, typ) result(val)
    CLASS(ONLINE_STATS), intent(in):: st
    REAL(qp),            intent(in):: typ
    REAL(qp):: val
    !
    if(st%N < LONG_ONE) then 
        val = ZER
    else
        val = SQRT(st%M(2)/real(st%N+LONG_NEG,  qp))
    end if
    !  
  END FUNCTION
  !
  !##########################################################################################################################
  !
  PURE FUNCTION SKEW_SAMPLE_SG_rel64(st, typ) result(val)
    CLASS(ONLINE_STATS),   intent(in):: st
    REAL(rel64), optional, intent(in):: typ
    REAL(rel64):: val
    REAL(qp):: tmp, norm
    !
    if( st%M(2) > tol ) then                     ! M(2) always positive
        norm = real(st%N, qp)/real(st%N-2, qp)   ! norm = n(n-1)^-0.5 / (n-2)
        norm = norm * sqrt(real(st%N-1, qp))
        !
        tmp = sqrt( st%M(2)*st%M(2)*st%M(2) )
        tmp = st%M(3) / tmp                       ! = M(3) / M(2)**1.5
        tmp = tmp * norm
        val = real( tmp, rel64 )
    else
        val = dz
    end if
    !  
  END FUNCTION
  !
  PURE FUNCTION SKEW_SAMPLE_SG_int64(st, typ) result(val)
    CLASS(ONLINE_STATS), intent(in):: st
    INTEGER(int64),      intent(in):: typ
    INTEGER(int64):: val
    REAL(qp):: tmp, norm
    !
    if( st%M(2) > tol ) then                     ! M(2) always positive
        norm = real(st%N, qp)/real(st%N-2, qp)   ! norm = n(n-1)^-0.5 / (n-2)
        norm = norm * sqrt(real(st%N-1, qp))
        !
        tmp = sqrt( st%M(2)*st%M(2)*st%M(2) )
        tmp = st%M(3) / tmp                       ! = M(3) / M(2)**1.5
        tmp = tmp * norm
        val = int( tmp, int64 )
    else
        val = LONG_ZER
    end if
    !  
  END FUNCTION
  !
  PURE FUNCTION SKEW_SAMPLE_SG_int32(st, typ) result(val)
    CLASS(ONLINE_STATS), intent(in):: st
    INTEGER(int32),      intent(in):: typ
    INTEGER(int32):: val
    REAL(qp):: tmp, norm
    !
    if( st%M(2) > tol ) then                     ! M(2) always positive
        norm = real(st%N, qp)/real(st%N-2, qp)   ! norm = n(n-1)^-0.5 / (n-2)
        norm = norm * sqrt(real(st%N-1, qp))
        !
        tmp = sqrt( st%M(2)*st%M(2)*st%M(2) )
        tmp = st%M(3) / tmp                       ! = M(3) / M(2)**1.5
        tmp = tmp * norm
        val = int( tmp, int32 )
    else
        val = 0_int32
    end if
    !  
  END FUNCTION
  !
  PURE FUNCTION SKEW_SAMPLE_SG_qp(st, typ) result(val)
    CLASS(ONLINE_STATS), intent(in):: st
    REAL(qp),            intent(in):: typ
    REAL(qp):: val
    REAL(qp):: norm
    !
    if( st%M(2) > tol ) then                     ! M(2) always positive
        norm = real(st%N, qp)/real(st%N-2, qp)   ! norm = n(n-1)^-0.5 / (n-2)
        norm = norm * sqrt(real(st%N-1, qp))
        !
        val = sqrt( st%M(2)*st%M(2)*st%M(2) )
        val = st%M(3) / val                       ! = M(3) / M(2)**1.5
        val = val * norm
    else
        val = ZER
    end if
    !  
  END FUNCTION
  !
  !##########################################################################################################################
  !
  PURE FUNCTION KURT_SAMPLE_SG_rel64(st, typ) result(val)
    CLASS(ONLINE_STATS),   intent(in):: st
    REAL(rel64), optional, intent(in):: typ
    REAL(rel64):: val
    REAL(qp):: tmp, norm
    !
    tmp = st%M(2)*st%M(2)
    if( tmp > tol ) then
        norm = real(st%N-1, qp)/real(st%N-2, qp)              ! norm = n(n+1)(n-1) / (n-2)(n-3)
        norm = norm * real(st%N, qp)/real(st%N-3, qp)
        norm = norm * real(st%N+1, qp)
        !
        tmp = st%M(4) / tmp
        tmp = tmp * norm
        !
        norm = real(st%N-1, qp)/real(st%N-2, qp)              ! norm = (n-1)^2 / (n-2)(n-3)
        norm = norm * real(st%N-1, qp)/real(st%N-3, qp)
        !
        tmp = tmp - q3*norm
        VAL = real( tmp, rel64 )
    else
        VAL = dz
    end if
    !  
  END FUNCTION
  !
  PURE FUNCTION KURT_SAMPLE_SG_int64(st, typ) result(val)
    CLASS(ONLINE_STATS), intent(in):: st
    INTEGER(int64),      intent(in):: typ
    INTEGER(int64):: val
    REAL(qp):: tmp, norm
    !
    tmp = st%M(2)*st%M(2)
    if( tmp > tol ) then
        norm = real(st%N-1, qp)/real(st%N-2, qp)              ! norm = n(n+1)(n-1) / (n-2)(n-3)
        norm = norm * real(st%N, qp)/real(st%N-3, qp)
        norm = norm * real(st%N+1, qp)
        !
        tmp = st%M(4) / tmp
        tmp = tmp * norm
        !
        norm = real(st%N-1, qp)/real(st%N-2, qp)              ! norm = (n-1)^2 / (n-2)(n-3)
        norm = norm * real(st%N-1, qp)/real(st%N-3, qp)
        !
        tmp = tmp - q3*norm
        val = int(tmp, int64)
    else
        VAL = LONG_ZER
    end if
    !  
  END FUNCTION
  !
  PURE FUNCTION KURT_SAMPLE_SG_int32(st, typ) result(val)
    CLASS(ONLINE_STATS), intent(in):: st
    INTEGER(int32),      intent(in):: typ
    INTEGER(int32):: val
    REAL(qp):: tmp, norm
    !
    tmp = st%M(2)*st%M(2)
    if( tmp > tol ) then
        norm = real(st%N-1, qp)/real(st%N-2, qp)              ! norm = n(n+1)(n-1) / (n-2)(n-3)
        norm = norm * real(st%N, qp)/real(st%N-3, qp)
        norm = norm * real(st%N+1, qp)
        !
        tmp = st%M(4) / tmp
        tmp = tmp * norm
        !
        norm = real(st%N-1, qp)/real(st%N-2, qp)              ! norm = (n-1)^2 / (n-2)(n-3)
        norm = norm * real(st%N-1, qp)/real(st%N-3, qp)
        !
        tmp = tmp - q3*norm
        val = int(tmp, int32)
    else
        VAL = 0_int32
    end if
    !  
  END FUNCTION
  !
  PURE FUNCTION KURT_SAMPLE_SG_qp(st, typ) result(val)
    CLASS(ONLINE_STATS), intent(in):: st
    REAL(qp),            intent(in):: typ
    REAL(qp):: val
    REAL(qp):: norm
    !
    val = st%M(2)*st%M(2)
    if( val > tol ) then
        norm = real(st%N-1, qp)/real(st%N-2, qp)              ! norm = n(n+1)(n-1) / (n-2)(n-3)
        norm = norm * real(st%N, qp)/real(st%N-3, qp)
        norm = norm * real(st%N+1, qp)
        !
        val = st%M(4) / val
        val = val * norm
        !
        norm = real(st%N-1, qp)/real(st%N-2, qp)              ! norm = (n-1)^2 / (n-2)(n-3)
        norm = norm * real(st%N-1, qp)/real(st%N-3, qp)
        !
        val = val - q3*norm
    else
        VAL = ZER
    end if
    !  
  END FUNCTION
    
    
    
    
    
  !
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  ! Sorting Rotines
  !#########################################################################################################################
  ! 1D Insertion Sort       (ISORT) and 
  ! 1D Dual Pivot Quicksort (QSORT) Routines 
  !
  PURE SUBROUTINE ISORT_1D_REL64(DIM,A)
    INTEGER,                       INTENT(IN   ):: DIM
    REAL   (REL64), DIMENSION(DIM),INTENT(INOUT):: A
    REAL   (REL64):: TMP
    INTEGER:: I, J
    !
    DO I=2, DIM
        TMP = A(I)
        J   = I - 1
        DO WHILE ( J >= 1 )
                   IF( TMP >= A(J) ) EXIT
                   A(J+1) = A(J)
                   J = J - 1
        END DO
        A(J+1) = TMP
    END DO
    !
  END SUBROUTINE 
  !
  !##########################################################################################################################
  ! QSORT base routine
  !
  RECURSIVE PURE SUBROUTINE QSORT_1D_REL64(DIM, A, L, R, D)
    INTEGER,                        INTENT(IN   ):: DIM
    REAL   (REL64), DIMENSION(DIM), INTENT(INOUT):: A
    INTEGER,                        INTENT(IN   ):: L, R   ! Lower Bound, Upper Bound -> Starting left index and right index
    INTEGER,                        INTENT(IN   ):: D      ! Depth recursion limit, once zero switch to Heap Sort
    REAL   (REL64):: TMP, P, Q       ! Pivot Values
    INTEGER:: I, J, K
    !
    K = R - L + 1                        ! Length of QSORT Partition
    IF( K  < 2 ) RETURN                  ! No Partition -> Return
    IF( K == 2 ) THEN                    ! Simple Flip Check
                 IF( A(L) > A(R) ) THEN
                                   TMP = A(L);  A(L) = A(R);  A(R) = TMP   !Swap A(L) and A(R)
                 END IF
                 RETURN
    END IF
    !
    IF( K <= QSORT_PARTITION_MIN) THEN   ! => Do Insertion Sort and Return   -- Side note: (R - L + 1 <= 16)
       DO I=L+1, R
           TMP = A(I)
           J   = I - 1
           DO WHILE ( J >= 1 )
                      IF( TMP >= A(J) ) EXIT
                      A(J+1) = A(J)
                      J = J - 1
           END DO
           A(J+1) = TMP
       END DO
       RETURN
    END IF
    !
    IF( D < 1 ) THEN   ! No more recursion allowed, switch to Heap Sort
       CALL HEAPSORT_1D_REL64( K, A(L:R) )
       RETURN
    END IF
    !
    !  A(L) => Pivot 1 -> Lower Branch
    !  A(R) => Pivot 2 -> Upper Branch
    !
    IF( A(L) > A(R) ) THEN
                      TMP = A(L);  A(L) = A(R);  A(R) = TMP   !Swap A(L) and A(R)
    END IF
    !
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ! Partition Code
    ! 
    P = A(L)
    Q = A(R)
    I = L + 1
    J = R - 1
    !
    ! Pivolt 1 and 2   -> A(L), A(R)
    ! Partion Location -> A(I), A(J)         => L < I < J < R
    !
    K = I
    DO WHILE ( K <= J )         
        !
        IF    ( A(K)  < P ) THEN  ! Less than the left pivot 
                            TMP = A(K);  A(K) = A(I);  A(I) = TMP   ! Swap A(K) and A(I)
                            I = I + 1
        ELSEIF( A(K) >= Q ) THEN  ! Greater than the right pivot 
                            DO WHILE ( K < J .AND. A(J) > Q )
                               J = J - 1
                            END DO
                            TMP = A(K);  A(K) = A(J);  A(J) = TMP   ! Swap A(K) and A(J)
                            J = J - 1
                            !
                            IF ( A(K) < P ) THEN  ! Less than the left pivot 
                                            TMP = A(K);  A(K) = A(I);  A(I) = TMP   ! Swap A(K) and A(I)
                                            I = I + 1
                            END IF 
        END IF
        !
        K = K + 1
        !
    END DO
    !
    ! Move Pivot to Partion Location
    I = I - 1
    J = J + 1
    TMP = A(L);  A(L) = A(I);  A(I) = TMP   !Swap A(L) and A(I)
    TMP = A(J);  A(J) = A(R);  A(R) = TMP   !Swap A(J) and A(R)
    !
    ! End Partition Code
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    !
    !  A(I) => Pivot 1 -> Lower Branch
    !  A(J) => Pivot 2 -> Upper Branch
    !
    CALL QSORT_1D_REL64(DIM, A, L  , I-1, D-1) 
    CALL QSORT_1D_REL64(DIM, A, I+1, J-1, D-1) 
    CALL QSORT_1D_REL64(DIM, A, J+1,   R, D-1) 
    !
  END SUBROUTINE
  !
  !##########################################################################################################################
  ! HEAPSORT base routines for when QSORT has too many recursive calls
  !
  PURE SUBROUTINE HEAPSORT_1D_REL64(DIM, A)
    INTEGER,                        INTENT(IN   ):: DIM
    REAL   (REL64), DIMENSION(DIM), INTENT(INOUT):: A
    REAL   (REL64):: TMP
    INTEGER:: I, P, C
    !
    HEAPIFY: DO I=DIM/2, 1, -1
       P = I    ! Parent
       C = 2*I  ! Child
       DO WHILE ( C <= DIM )   ! Siftdown the heap
           !
           IF( C < DIM) THEN
                   IF ( A(C) < A(C+1) ) C = C + 1  ! 2nd child is bigger
           END IF
           !
           IF( A(P) >= A(C) ) CYCLE HEAPIFY
           !
           TMP  = A(P)
           A(P) = A(C)
           A(C) = TMP
           !
           P = C
           C = 2*P
       END DO
    END DO HEAPIFY
    !
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    !
    POPPER: DO I=DIM-1, 1, -1
       !
       TMP    = A(1)
       A(1)   = A(I+1)  !Pop the last value
       A(I+1) = TMP
       !
       P = 1    ! Parent
       C = 2    ! Child
       DO WHILE ( C <= I )   ! Siftdown the heap
           !
           IF( C < I) THEN
                   IF ( A(C) < A(C+1) ) C = C + 1  ! 2nd child is bigger
           END IF
           !
           IF( A(P) >= A(C) ) CYCLE POPPER
           !
           TMP  = A(P)
           A(P) = A(C)
           A(C) = TMP
           !
           P = C
           C = 2*P
       END DO
       !
    END DO POPPER
    !
  END SUBROUTINE
  !
  !--------------------------------------------------------------------------------------------------------------------------
  ! Function that returns the max depth/recursion level that QSORT allows
  !
  PURE FUNCTION MAX_RECUR(DIM) RESULT(MX)
    INTEGER, value:: DIM
    INTEGER:: MX
    !
    MX = 0
    DO WHILE (DIM > 0)
              DIM = SHIFTR(DIM, 1)
              MX  = MX + 1
    END DO
    MX = MX * 2  ! ~= 2*ceil(log2(DIM+1)) -> Initial guess
    !
    IF(MX < 32   )  MX = 32
    IF(MX > 10240)  MX = 10240
    !
  END FUNCTION
  !
  !--------------------------------------------------------------------------------------------------------------------------
  ! Determine sort option to use
  !
  PURE FUNCTION SORT_TYPE(DIM, A) RESULT(ANS)           ! ANS = 0 -> Already Sorted
    INTEGER,                     intent( in):: DIM   ! ANS = 1 -> Small or Well Sorted => ISORT
    REAL(rel64), dimension(DIM), intent( in):: A     ! ANS = 2 -> Default Sort         => QSORT
    INTEGER:: ANS                                      
    INTEGER:: I, CNT, LIM                              
    !                                                  
    ANS = 2                                             ! Use QSORT
    IF ( DIM <= ISORT_SMALL_ARRAY_SIZE ) ANS = 1        ! Small Array, use ISORT
    !
    IF(ANS == 2 .AND. 2*DIM > QSORT_WELL_SORTED_LIM) THEN
                  LIM = DIM/QSORT_WELL_SORTED_LIM
                  CNT = 0
                  DO I=1, DIM-1
                               IF( A(I) > A(I+1) )  CNT = CNT + 1  ! Is zero if perfectly sorted, and DIM/2 if completely not-sorted
                  END DO
                  IF(CNT <= LIM) ANS = 1     ! Well sorted array has increasing values less than LIM % of the time.
                  IF(CNT ==   0) ANS = 0     ! Array already sorted
    END IF
  END FUNCTION
  !
END MODULE