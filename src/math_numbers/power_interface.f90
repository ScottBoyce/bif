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
  ! Constants used internally to module ----------------------------------------------------
  ! 
  REAL(REL64),   PARAMETER, DIMENSION(127)::  powTen = [ REAL(REL64):: 1.E+01, 1.E+02, 1.E+03, 1.E+04, 1.E+05, 1.E+06, 1.E+07, 1.E+08, 1.E+09, 1.E+10, 1.E+11, 1.E+12, 1.E+13, 1.E+14, 1.E+15, 1.E+16, 1.E+17, 1.E+18, 1.E+19, 1.E+20, 1.E+21, 1.E+22, 1.E+23, 1.E+24, 1.E+25, 1.E+26, 1.E+27, 1.E+28, 1.E+29, 1.E+30, 1.E+31, 1.E+32, 1.E+33, 1.E+34, 1.E+35, 1.E+36, 1.E+37, 1.E+38, 1.E+39, 1.E+40, 1.E+41, 1.E+42, 1.E+43, 1.E+44, 1.E+45, 1.E+46, 1.E+47, 1.E+48, 1.E+49, 1.E+50, 1.E+51, 1.E+52, 1.E+53, 1.E+54, 1.E+55, 1.E+56, 1.E+57, 1.E+58, 1.E+59, 1.E+60, 1.E+61, 1.E+62, 1.E+63, 1.E+64, 1.E+65, 1.E+66, 1.E+67, 1.E+68, 1.E+69, 1.E+70, 1.E+71, 1.E+72, 1.E+73, 1.E+74, 1.E+75, 1.E+76, 1.E+77, 1.E+78, 1.E+79, 1.E+80, 1.E+81, 1.E+82, 1.E+83, 1.E+84, 1.E+85, 1.E+86, 1.E+87, 1.E+88, 1.E+89, 1.E+90, 1.E+91, 1.E+92, 1.E+93, 1.E+94, 1.E+95, 1.E+96, 1.E+97, 1.E+98, 1.E+99, 1.E+100, 1.E+101, 1.E+102, 1.E+103, 1.E+104, 1.E+105, 1.E+106, 1.E+107, 1.E+108, 1.E+109, 1.E+110, 1.E+111, 1.E+112, 1.E+113, 1.E+114, 1.E+115, 1.E+116, 1.E+117, 1.E+118, 1.E+119, 1.E+120, 1.E+121, 1.E+122, 1.E+123, 1.E+124, 1.E+125, 1.E+126, 1.E+127 ]
  REAL(REL64),   PARAMETER, DIMENSION(46)::  rootTen = [ REAL(REL64):: 1.33352143216332, 1.15478198468946, 1.07460782832132, 1.03663292843770, 1.01815172171818, 1.00903504484145, 1.00450736425446, 1.00225114829291, 1.00112494139988, 1.00056231260221, 1.00028111678778, 1.00014054851695, 1.00007027178941, 1.00003513527746, 1.00001756748442, 1.00000878370363, 1.00000439184217, 1.00000219591868, 1.00000109795874, 1.00000054897922, 1.00000027448957, 1.00000013724478, 1.00000006862239, 1.00000003431119, 1.00000001715560, 1.00000000857780, 1.00000000428890, 1.00000000214445, 1.00000000107222, 1.00000000053611, 1.00000000026806, 1.00000000013403, 1.00000000006701, 1.00000000003351, 1.00000000001675, 1.00000000000838, 1.00000000000419, 1.00000000000209, 1.00000000000105, 1.00000000000052, 1.00000000000026, 1.00000000000013, 1.00000000000007, 1.00000000000003, 1.00000000000002, 1.00000000000001 ]
  !
  REAL(REL64),   PARAMETER:: inf         =  1.0E+128_REL64
  REAL(REL64),   PARAMETER:: underflowP  =  1.0E-128_REL64
  REAL(REL64),   PARAMETER:: underflowN  = -1.0E-128_REL64
  REAL(REL64),   PARAMETER:: nearOneP  =  1.0000000000001_rel64 ! 1.0_REL64 + epsilon(inf)*100.0_REL64  ! ~= 1.00000000000002
  REAL(REL64),   PARAMETER:: nearOneN  =  0.9999999999999_rel64 ! 1.0_REL64 - epsilon(inf)*100.0_REL64  ! ~= 0.99999999999997
  REAL(REL64),   PARAMETER:: nearZero  =  5.25E-13              ! lower lim of rootTen
  !  ! ----------------------------------------------------------------------------------------
  ! 
  PRIVATE
  !
  PUBLIC:: pow10
  PUBLIC:: FloorPowerOfTwo
  !
  INTERFACE pow10
    MODULE PROCEDURE pow10_DBLE      ! pow10( x )                overflow set to 1e128
    MODULE PROCEDURE pow10_DBLE_over ! pow10( x, overflow_lim )  Override 1E128 overflow limit
    MODULE PROCEDURE pow10_DBLE_nan  ! pow10( x, overflow_nan )  logical to use nan or not
  END INTERFACE
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
                                             y = 10.0_REL64
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
    IF( b >  307 .and. y > 1.79769313486_REL64) THEN  !Note max number is: 1.797693134862316E+308  ->  1.79769313486 * 10**308 ~= HUGE(y)
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
                                             y = 10.0_REL64
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
    
    

  !REAL(REL64),   PARAMETER:: dn1 = -1.0_REL64
  !REAL(REL64),   PARAMETER:: d0  = 0.0_REL64
  !REAL(REL64),   PARAMETER:: d1  = 1.0_REL64
  !REAL(REL64),   PARAMETER:: d2  = 2.0_REL64
  !REAL(REL64),   PARAMETER:: dv2 = 0.5_REL64
  !REAL(REL64),   PARAMETER:: ten = 10.0_REL64
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
  !  y = pow( base, raise, 0.000001_REL64 )
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