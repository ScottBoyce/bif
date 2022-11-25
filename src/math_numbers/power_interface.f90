!
! CODE DEVELOPED BY SCOTT E BOYCE 
!                   CONTACT <seboyce@usgs.gov> or <Boyce@engineer.com>
!  
!  base^x routines
!    For example pow10(x) is an optimized fucntion that returns 10^x
!
MODULE POWER_INTERFACE
  !  
  USE, INTRINSIC:: ISO_FORTRAN_ENV, ONLY: REL64=>REAL64
  USE, INTRINSIC:: IEEE_ARITHMETIC, ONLY: IEEE_VALUE, IEEE_QUIET_NAN
  !
  IMPLICIT NONE
  ! 
  PRIVATE
  !
  PUBLIC:: pow10                ! pow10( x )         -> Returns 10**x
  PUBLIC:: FloorPowerOfTwo      ! FloorPowerOfTwo(n) -> Returns the closest power of 2 to provided number
  !
  INTERFACE pow10
    MODULE PROCEDURE pow10_DBLE      ! pow10( x )                overflow set to 1e128
    MODULE PROCEDURE pow10_DBLE_over ! pow10( x, overflow_lim )  Override 1E128 overflow limit
    MODULE PROCEDURE pow10_DBLE_nan  ! pow10( x, overflow_nan )  logical to use nan or not
  END INTERFACE
  !
  ! Constants used internally to module ----------------------------------------------------
  ! 
  REAL(REL64), PARAMETER, DIMENSION(127)::  powTen = [ & 
          1.e+01_rel64,  1.e+02_rel64,  1.e+03_rel64,  1.e+04_rel64,  1.e+05_rel64, &
          1.e+06_rel64,  1.e+07_rel64,  1.e+08_rel64,  1.e+09_rel64,  1.e+10_rel64, &
          1.e+11_rel64,  1.e+12_rel64,  1.e+13_rel64,  1.e+14_rel64,  1.e+15_rel64, &
          1.e+16_rel64,  1.e+17_rel64,  1.e+18_rel64,  1.e+19_rel64,  1.e+20_rel64, &
          1.e+21_rel64,  1.e+22_rel64,  1.e+23_rel64,  1.e+24_rel64,  1.e+25_rel64, &
          1.e+26_rel64,  1.e+27_rel64,  1.e+28_rel64,  1.e+29_rel64,  1.e+30_rel64, &
          1.e+31_rel64,  1.e+32_rel64,  1.e+33_rel64,  1.e+34_rel64,  1.e+35_rel64, &
          1.e+36_rel64,  1.e+37_rel64,  1.e+38_rel64,  1.e+39_rel64,  1.e+40_rel64, &
          1.e+41_rel64,  1.e+42_rel64,  1.e+43_rel64,  1.e+44_rel64,  1.e+45_rel64, &
          1.e+46_rel64,  1.e+47_rel64,  1.e+48_rel64,  1.e+49_rel64,  1.e+50_rel64, &
          1.e+51_rel64,  1.e+52_rel64,  1.e+53_rel64,  1.e+54_rel64,  1.e+55_rel64, &
          1.e+56_rel64,  1.e+57_rel64,  1.e+58_rel64,  1.e+59_rel64,  1.e+60_rel64, &
          1.e+61_rel64,  1.e+62_rel64,  1.e+63_rel64,  1.e+64_rel64,  1.e+65_rel64, &
          1.e+66_rel64,  1.e+67_rel64,  1.e+68_rel64,  1.e+69_rel64,  1.e+70_rel64, &
          1.e+71_rel64,  1.e+72_rel64,  1.e+73_rel64,  1.e+74_rel64,  1.e+75_rel64, &
          1.e+76_rel64,  1.e+77_rel64,  1.e+78_rel64,  1.e+79_rel64,  1.e+80_rel64, &
          1.e+81_rel64,  1.e+82_rel64,  1.e+83_rel64,  1.e+84_rel64,  1.e+85_rel64, &
          1.e+86_rel64,  1.e+87_rel64,  1.e+88_rel64,  1.e+89_rel64,  1.e+90_rel64, &
          1.e+91_rel64,  1.e+92_rel64,  1.e+93_rel64,  1.e+94_rel64,  1.e+95_rel64, &
          1.e+96_rel64,  1.e+97_rel64,  1.e+98_rel64,  1.e+99_rel64, 1.e+100_rel64, &
         1.e+101_rel64, 1.e+102_rel64, 1.e+103_rel64, 1.e+104_rel64, 1.e+105_rel64, &
         1.e+106_rel64, 1.e+107_rel64, 1.e+108_rel64, 1.e+109_rel64, 1.e+110_rel64, &
         1.e+111_rel64, 1.e+112_rel64, 1.e+113_rel64, 1.e+114_rel64, 1.e+115_rel64, &
         1.e+116_rel64, 1.e+117_rel64, 1.e+118_rel64, 1.e+119_rel64, 1.e+120_rel64, &
         1.e+121_rel64, 1.e+122_rel64, 1.e+123_rel64, 1.e+124_rel64, 1.e+125_rel64, &
         1.e+126_rel64,  1.e+127_rel64                                              &
         ]
  REAL(REL64), PARAMETER, DIMENSION(46)::  rootTen = [ &
         1.33352143216332_rel64, 1.15478198468946_rel64, 1.07460782832132_rel64, 1.03663292843770_rel64, &
         1.01815172171818_rel64, 1.00903504484145_rel64, 1.00450736425446_rel64, 1.00225114829291_rel64, &
         1.00112494139988_rel64, 1.00056231260221_rel64, 1.00028111678778_rel64, 1.00014054851695_rel64, &
         1.00007027178941_rel64, 1.00003513527746_rel64, 1.00001756748442_rel64, 1.00000878370363_rel64, &
         1.00000439184217_rel64, 1.00000219591868_rel64, 1.00000109795874_rel64, 1.00000054897922_rel64, &
         1.00000027448957_rel64, 1.00000013724478_rel64, 1.00000006862239_rel64, 1.00000003431119_rel64, &
         1.00000001715560_rel64, 1.00000000857780_rel64, 1.00000000428890_rel64, 1.00000000214445_rel64, &
         1.00000000107222_rel64, 1.00000000053611_rel64, 1.00000000026806_rel64, 1.00000000013403_rel64, &
         1.00000000006701_rel64, 1.00000000003351_rel64, 1.00000000001675_rel64, 1.00000000000838_rel64, &
         1.00000000000419_rel64, 1.00000000000209_rel64, 1.00000000000105_rel64, 1.00000000000052_rel64, &
         1.00000000000026_rel64, 1.00000000000013_rel64, 1.00000000000007_rel64, 1.00000000000003_rel64, &
         1.00000000000002_rel64, 1.00000000000001_rel64                                                  &
         ]
  REAL(REL64), PARAMETER:: inf         =  1.0E+128_rel64
  REAL(REL64), PARAMETER:: underflowP  =  1.0E-128_rel64
  REAL(REL64), PARAMETER:: underflowN  = -1.0E-128_rel64
  REAL(REL64), PARAMETER:: nearOneP  =  1.0000000000001_rel64 ! 1.0_rel64 + epsilon(inf)*100.0_rel64  ! ~= 1.00000000000002
  REAL(REL64), PARAMETER:: nearOneN  =  0.9999999999999_rel64 ! 1.0_rel64 - epsilon(inf)*100.0_rel64  ! ~= 0.99999999999997
  REAL(REL64), PARAMETER:: nearZero  =  5.25E-13_rel64        ! lower lim of rootTen
  !  
  ! ----------------------------------------------------------------------------------------
  !
  CONTAINS
  !	
  ! ------------------------------------------------------------------------------------------------------------------------------------------------------------
  !	Returns the closest power of 2 to provided number
  !
  PURE FUNCTION FloorPowerOfTwo(N)  result(P)
    INTEGER, intent(in):: N
    INTEGER:: P
    INTEGER:: r, nbit
    !
    IF( N < 2 ) THEN
        P = 0
    ELSE
        r = 1
        P = IOR(N, SHIFTR( N, r  ) )
        !
        nbit = BIT_SIZE(N) - 1  ! Last bit is the sign bit, so ignore it
        !
        DO WHILE(r < nbit)
            r = SHIFTL(r, 1)             ! Powers of 2, so r = 2, 4, 8, 16, ...
            P = IOR(P, SHIFTR( P, r  ) )
        END DO
        !
        P = IEOR(P, SHIFTR( P, 1  )) ! P = P - SHIFTR( P, 1  )
    END IF
  END FUNCTION
  !	
  ! ------------------------------------------------------------------------------------------------------------------------------------------------------------
  !	pow10 Routines
  !
  PURE FUNCTION pow10_DBLE_nan( x, overflow_nan ) RESULT(y)
    REAL(REL64), intent(in) :: x
    LOGICAL,     intent(in) :: overflow_nan
    REAL(REL64):: y
    !
    y = pow10_DBLE( x )
    !
    if(overflow_nan .and. y == inf)  y = ieee_value(y, ieee_quiet_nan)
    !
  END FUNCTION
  !	
  ! ------------------------------------------------------------------------------------------------------------------------------------------------------------
  !	
  PURE FUNCTION pow10_DBLE_over( x, overflow_lim ) RESULT(y)
    REAL(REL64), intent(in) :: x, overflow_lim
    REAL(REL64):: y
    INTEGER:: b
    !
    IF( underflowN < X .AND. X < underflowP) THEN           ! Assumed Zero
                                             y = 1.0_rel64
                                             RETURN
    END IF
    IF(   nearOneN < X .AND. X < nearOneP  ) THEN           ! Assumed One
                                             y = 10.0_rel64
                                             RETURN
    END IF
    !
    b = int(x)
    IF( b < -127 ) THEN
                   y = underflowP ! 1e-128
                   RETURN
    END IF
    !
    y = pow10frac(x - real(b, REL64))
    !
    IF( b >  307 .and. y > 1.79769313486_rel64) THEN  !Note max number is: 1.797693134862316E+308  ->  1.79769313486 * 10**308 ~= HUGE(y)
                   y = overflow_lim
                   RETURN
    END IF
    !
    IF    ( b > 0 ) THEN
                        y = y * powTen(b)
    ELSEIF( b < 0 ) THEN
                        b = -b
                        y = 1.0_rel64 / y
                        y = y * ( 1.0_rel64 / powTen(b) )
    END IF
    !
    IF( y > overflow_lim ) y = overflow_lim
    !
  END FUNCTION
  !	
  ! ------------------------------------------------------------------------------------------------------------------------------------------------------------
  !	
  PURE FUNCTION pow10_DBLE( x ) RESULT(y)
    REAL(REL64), intent(in) :: x
    REAL(REL64):: y
    INTEGER:: b
    !
    IF( underflowN < X .AND. X < underflowP) THEN           ! Assumed Zero
                                             y = 1.0_rel64
                                             RETURN
    END IF
    IF(   nearOneN < X .AND. X < nearOneP  ) THEN           ! Assumed One
                                             y = 10.0_rel64
                                             RETURN
    END IF
    !
    b = int(x)
    !
    IF( b >  127 ) THEN
                   y = inf        ! 1e128
                   RETURN
    END IF
    IF( b < -127 ) THEN
                   y = underflowP ! 1e-128
                   RETURN
    END IF
    !
    y = pow10frac(x - real(b, REL64))
    !
    IF    ( b > 0 ) THEN
                        y = y * powTen(b)
    ELSEIF( b < 0 ) THEN
                        b = -b
                        y = 1.0_rel64 / y
                        y = y * ( 1.0_rel64 / powTen(b) )
    END IF
    !
  END FUNCTION
  !	
  ! ------------------------------------------------------------------------------------------------------------------------------------------------------------
  !	
  PURE FUNCTION pow10frac( frac ) RESULT(y)
    REAL(REL64), intent(in):: frac
    REAL(REL64):: y
    REAL(REL64):: L, H, M
    INTEGER:: I
    !
    if    ( frac  < nearZero ) then
                        y = 1.00_rel64
                        return
    elseif( frac  > nearOneN ) then
                        y = 10.0_rel64
                        return
    elseif( frac == 0.5_rel64 ) then
                        y = 3.16227766016838_rel64     ! sqrt(10)
                        return
    elseif( frac < 0.5_rel64 ) then
                        L = 0.00_rel64
                        M = 0.25_rel64
                        H = 0.50_rel64
                        y = 1.77827941003892_rel64
    else
                        L = 0.50_rel64
                        M = 0.75_rel64
                        H = 1.00_rel64
                        y = 5.62341325190349_rel64  ! sqrt(sqrt(10))
    end if
    !
    DO I=1, 46
        if    (M == frac) then
                              EXIT
        elseif(M <  frac) then
                              L = M
                              y = y * rootTen(I)
        else                  
                              H = M
                              y = y * (1.0_rel64 / rootTen(I))
        end if
        M = (L+H)*0.5_rel64
    END DO
    !
  END FUNCTION
END MODULE
    
    

  !REAL(REL64),   PARAMETER:: dn1 = -1.0_rel64
  !REAL(REL64),   PARAMETER:: d0  = 0.0_rel64
  !REAL(REL64),   PARAMETER:: d1  = 1.0_rel64
  !REAL(REL64),   PARAMETER:: d2  = 2.0_rel64
  !REAL(REL64),   PARAMETER:: dv2 = 0.5_rel64
  !REAL(REL64),   PARAMETER:: ten = 10.0_rel64
  !
  !PURE FUNCTION pow10frac( frac ) RESULT(y)
  !  REAL(REL64), intent(in):: frac
  !  REAL(REL64):: y
  !  REAL(REL64):: L, H, M, tmp
  !  !
  !  L = 0.0_rel64
  !  H = 1.0_rel64
  !  M = 0.5_rel64
  !  !
  !  tmp = 3.16227766016838_rel64 ! sqrt(10)
  !  y   = tmp
  !  !
  !  DO WHILE( abs(M - frac) > 1E-10_rel64)
  !      !
  !      tmp = sqrt(tmp)
  !      !
  !      if(M < frac) THEN
  !          L = M
  !          y = y * tmp
  !      else
  !          H = M
  !          y = y * (1.0_rel64 / tmp)
  !      end if
  !      M = (L+H)*0.5_rel64
  !  END DO
  !  !
  !END FUNCTION
  
  !
  !RECURSIVE PURE FUNCTION double( x ) RESULT(y)
  !  REAL(REL64), intent(in):: x
  !  REAL(REL64):: y
  !  y = x*x
  !END FUNCTION
  !!
  !RECURSIVE PURE FUNCTION powBase10( raise, prec ) RESULT(y)
  !  REAL(REL64), intent(in):: raise, prec
  !  REAL(REL64):: y
  !  !
  !  if    (raise <   d0 ) then
  !                        y = d1 / powBase10( -raise, prec )
  !  elseif(raise >= ten ) then
  !                        y = double( powBase10( raise*dv2, prec*dv2 ) )
  !  elseif(raise >=  d1 ) then
  !                        y =  ten * powBase10( raise + dn1, prec )
  !  elseif(prec  >=  d1 ) then
  !                        y = 3.16227766016838_rel64
  !  else
  !      y = sqrt( powBase10( d2*raise, d2*prec ) )
  !  end if
  !END FUNCTION
  !!
  !RECURSIVE PURE FUNCTION pow( base, raise, prec ) RESULT(y)
  !  REAL(REL64), intent(in):: base, raise, prec
  !  REAL(REL64):: y
  !  !
  !  if    (raise <   d0 ) then
  !                        y = d1 / pow( base, -raise, prec )
  !  elseif(raise >= ten ) then
  !                        y = double( pow( base, raise*dv2, prec*dv2 ) )
  !  elseif(raise >=  d1 ) then
  !                        y =  base * pow( base, raise + dn1, prec )
  !  elseif(prec  >=  d1 ) then
  !                        y = sqrt( base )
  !  else
  !      y = sqrt( pow( base, d2*raise, d2*prec ) )
  !  end if
  !END FUNCTION
  !!
  !PURE FUNCTION power( base, raise ) RESULT(y)
  !  REAL(REL64), intent(in):: base, raise
  !  REAL(REL64):: y
  !  !
  !  y = pow( base, raise, 0.000001_rel64 )
  !  !
  !END FUNCTION
  !
  !RECURSIVE PURE FUNCTION pow( base, raise, prec ) RESULT(y)
  !  REAL(REL64), intent(in):: base, raise, prec
  !  REAL(REL64):: y
  !  !
  !  if    (raise <   0 ) then
  !                       y = 1 / pow( base, -raise, prec )
  !  elseif(raise >= 10 ) then
  !                       y = double( pow( base, raise/2, prec/2 ) )
  !  elseif(raise >=  1 ) then
  !                       y = base * pow( base, raise-1, prec )
  !  elseif(prec  >=  1 ) then
  !                       y = sqrt( base )
  !  else
  !      y = sqrt( pow( base, 2*raise, 2*prec ) )
  !  end if
  !END FUNCTION