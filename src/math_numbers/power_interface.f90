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
  REAL(REL64), PARAMETER, DIMENSION(127)::  powTen = [ REAL(REL64):: 1.d+01, 1.d+02, 1.d+03, 1.d+04, 1.d+05, 1.d+06, 1.d+07, 1.d+08, 1.d+09, 1.d+10, 1.d+11, 1.d+12, 1.d+13, 1.d+14, 1.d+15, 1.d+16, 1.d+17, 1.d+18, 1.d+19, 1.d+20, 1.d+21, 1.d+22, 1.d+23, 1.d+24, 1.d+25, 1.d+26, 1.d+27, 1.d+28, 1.d+29, 1.d+30, &
                                                                     1.d+31, 1.d+32, 1.d+33, 1.d+34, 1.d+35, 1.d+36, 1.d+37, 1.d+38, 1.d+39, 1.d+40, 1.d+41, 1.d+42, 1.d+43, 1.d+44, 1.d+45, 1.d+46, 1.d+47, 1.d+48, 1.d+49, 1.d+50, 1.d+51, 1.d+52, 1.d+53, 1.d+54, 1.d+55, 1.d+56, 1.d+57, 1.d+58, 1.d+59, 1.d+60, &
                                                                     1.d+61, 1.d+62, 1.d+63, 1.d+64, 1.d+65, 1.d+66, 1.d+67, 1.d+68, 1.d+69, 1.d+70, 1.d+71, 1.d+72, 1.d+73, 1.d+74, 1.d+75, 1.d+76, 1.d+77, 1.d+78, 1.d+79, 1.d+80, 1.d+81, 1.d+82, 1.d+83, 1.d+84, 1.d+85, 1.d+86, 1.d+87, 1.d+88, 1.d+89, 1.d+90, &
                                                                     1.d+91, 1.d+92, 1.d+93, 1.d+94, 1.d+95, 1.d+96, 1.d+97, 1.d+98, 1.d+99, 1.d+100, 1.d+101, 1.d+102, 1.d+103, 1.d+104, 1.d+105, 1.d+106, 1.d+107, 1.d+108, 1.d+109, 1.d+110, 1.d+111, 1.d+112, 1.d+113, 1.d+114, 1.d+115, 1.d+116, 1.d+117, &
                                                                     1.d+118, 1.d+119, 1.d+120, 1.d+121, 1.d+122, 1.d+123, 1.d+124, 1.d+125, 1.d+126, 1.d+127 ]
  REAL(REL64), PARAMETER, DIMENSION(46)::  rootTen = [ REAL(REL64):: 1.33352143216332, 1.15478198468946, 1.07460782832132, 1.03663292843770, 1.01815172171818, 1.00903504484145, 1.00450736425446, 1.00225114829291, 1.00112494139988, 1.00056231260221, 1.00028111678778, 1.00014054851695, 1.00007027178941, &
                                                                     1.00003513527746, 1.00001756748442, 1.00000878370363, 1.00000439184217, 1.00000219591868, 1.00000109795874, 1.00000054897922, 1.00000027448957, 1.00000013724478, 1.00000006862239, 1.00000003431119, 1.00000001715560, 1.00000000857780, &
                                                                     1.00000000428890, 1.00000000214445, 1.00000000107222, 1.00000000053611, 1.00000000026806, 1.00000000013403, 1.00000000006701, 1.00000000003351, 1.00000000001675, 1.00000000000838, 1.00000000000419, 1.00000000000209, 1.00000000000105, &
                                                                     1.00000000000052, 1.00000000000026, 1.00000000000013, 1.00000000000007, 1.00000000000003, 1.00000000000002, 1.00000000000001 ]
  !            
  REAL(REL64), PARAMETER:: inf         =  1.0E+128_rel64
  REAL(REL64), PARAMETER:: underflowP  =  1.0E-128_rel64
  REAL(REL64), PARAMETER:: underflowN  = -1.0E-128_rel64
  REAL(REL64), PARAMETER:: nearOneP  =  1.0000000000001_rel64 ! 1.0_rel64 + epsilon(inf)*100.0_rel64  ! ~= 1.00000000000002
  REAL(REL64), PARAMETER:: nearOneN  =  0.9999999999999_rel64 ! 1.0_rel64 - epsilon(inf)*100.0_rel64  ! ~= 0.99999999999997
  REAL(REL64), PARAMETER:: nearZero  =  5.25E-13_rel64        ! lower lim of rootTen
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