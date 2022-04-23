! CODE DEVELOPED BY SCOTT E BOYCE 
!                   CONTACT <seboyce@usgs.gov> or <Boyce@engineer.com>
!
!      CODE THAT PROVIDES FUNCTIONS FOR CALENDAR DATES, DECIMAL YEARS, AND JULIAN DAY NUMBERS
!
! COMMON VARIABLE NAMES
!    JDN  = Julian Date Number from 1 to 365 or 366
!    DYEAR= Decimal year where the mantissa is the JDN divided by either 365 or 366
!    FRAC = Fraction of a day; 0.0 <= FRAC < 1.0
!    mm   = String reprsentation of the month (e.g. for January '01/15/2015' or '1/15/2015'
!    dd   = String reprsentation of the day of month (e.g. for January 5th '01/05/2015' or '1/5/2015'
!    YYYY = String reprsentation of the year with four digits
!
!    DAY, MONTH, YEAR = SAME AS MEANING in mm/dd/YYYY
!
!
! FUNCTION LISTING:
!    
!    FUNCTION   ISLEAPYEAR(YEAR)                                              # TEST IF YEAR IS LEAP YEAR, RETURNS TRUE/FALSE
!    FUNCTION   MONTHDAYS(MONTH,YEAR,[LEAP])                                  # RETURNS NUMBER OF DAYS IN MONTH GIVEN YEAR, IF LEAP IS PROVIDED THEN YEAR IS IGNORED AND RETURNS THE NUMEBR OF DAYS FOR A LEAP/NONLEAP
!    FUNCTION   JULIANDAY(DAY, MONTH, YEAR)                                   # RETURNS NUMBER OF JULIAN DAY NUMBER (JDN=1 is January 1st)
!    SUBROUTINE JULIANDAY_TO_DATE(JDN_IN, YEAR_IN, DAY, MONTH, YEAR, [JDN])   # GET DATE GIVEN A JULIAN DAY NUMBER AND YEAR. JDN_IN CAN BE <1 and >366. 
!    SUBROUTINE DYEAR_TO_DATE(DYEAR, DAY, MONTH, YEAR, [FRAC], [JDN])         # GET DATE GIVEN DECIMAL YEAR
!    FUNCTION   DATE_TO_DYEAR(DAY, MONTH, YEAR, FRAC)                         # RETURNS DECIMAL YEAR GIVEN DATE
!    FUNCTION   JDN_TO_DYEAR(JDN, YEAR, [FRAC])                               # RETURNS DECIMAL YEAR GIVEN JULIAN DATE
!    
!
!
! MODULE CALENDAR_FUNCTIONS
!    FUNCTIONS THAT HANDLE BASIC CALENDER OPERATIONS
!                                                 VERSION 1.0 [3/20/2015] ORIGINAL VERSION
!
MODULE CALENDAR_FUNCTIONS
  USE, INTRINSIC:: IEEE_ARITHMETIC, ONLY: IEEE_VALUE, IEEE_QUIET_NAN
  USE, INTRINSIC:: ISO_FORTRAN_ENV, ONLY: REAL32, REAL64               ! REAL32 = SINGLE PRECISION, REAL64 = DOUBLE PRECISION
  IMPLICIT NONE(TYPE, EXTERNAL)
  PRIVATE:: IEEE_VALUE, IEEE_QUIET_NAN, REAL32, REAL64
  !PRIVATE:: TRUE, FALSE, Z, ONE, DZ, UNO, IDIFF
  PRIVATE:: LEAP_CHECK
  !
  LOGICAL,      PARAMETER:: TRUE  = .TRUE.
  LOGICAL,      PARAMETER:: FALSE = .FALSE.
  INTEGER,      PARAMETER:: IDIFF=ICHAR('a')-ICHAR('A')
  INTEGER,      PARAMETER:: Z     = 0
  INTEGER,      PARAMETER:: ONE   = 1
  REAL(REAL64), PARAMETER:: DZ  = 0.0_real64 
  REAL(REAL64), PARAMETER:: UNO = 1.0_real64 
  !
  REAL(REAL64), PARAMETER:: DAY2SEC = 86400.0_real64  ! ~ 1 day in seconds
  REAL(REAL64), PARAMETER:: DAY2MIN =  1440.0_real64  ! ~ 1 day in minutes
  REAL(REAL64), PARAMETER:: DAY2HR  =    24.0_real64  ! ~ 1 day in hour hours
  !
  REAL(REAL64), PARAMETER:: SEC2DAY = 1.1574074074074074074D-5  ! ~ 1 second in days
  REAL(REAL64), PARAMETER:: MIN2DAY = 6.9444444444444444444D-4  ! ~ 1 minute in days
  REAL(REAL64), PARAMETER::  HR2DAY = 0.0416666666666666666D0   ! ~ 1 hour   in days 
  !
  INTERFACE YEAR_FRACTION
    MODULE PROCEDURE YEAR_FRACTION_DYEAR
    MODULE PROCEDURE YEAR_FRACTION_DMY
  END INTERFACE
  !
  INTERFACE TIME_TO_DAY_FRACTION ! (HOUR, MIN, SEC)
     MODULE PROCEDURE TIME_TO_DAY_FRACTION_INT_INT_INT
     MODULE PROCEDURE TIME_TO_DAY_FRACTION_INT_INT_DBL
     MODULE PROCEDURE TIME_TO_DAY_FRACTION_INT_DBL_DBL
     MODULE PROCEDURE TIME_TO_DAY_FRACTION_DBL_DBL_DBL
     MODULE PROCEDURE STR_TIME_TO_DAY_FRACTION       !(STR)
  END INTERFACE
  !
  CONTAINS
  !
  ELEMENTAL PURE FUNCTION ISLEAPYEAR(YEAR) 
    INTEGER, INTENT(IN):: YEAR
    LOGICAL::ISLEAPYEAR
    !
    IF(   (MOD(YEAR,4).EQ.Z .AND. MOD(YEAR,100).NE.Z) .OR. MOD(YEAR,400).EQ.Z )THEN  !LEAP YEAR IF YEAR IS DIVISABLE BY 4 AND NOT 100 OR IF YEAR IS DIVISABLE BY 400
        ISLEAPYEAR=TRUE
    ELSE
        ISLEAPYEAR=FALSE
    END IF     
    !
  END FUNCTION
  !  
  ELEMENTAL PURE FUNCTION LEAP_CHECK(YEAR, LEAP) RESULT(LEAPYEAR)  !NECESSARY FOR SUBROUTINES THAT MAY NOT INCLUDE YEAR BUT SPECIFY LEAP VARIABLE
    INTEGER, INTENT(IN), OPTIONAL:: YEAR
    LOGICAL, INTENT(IN), OPTIONAL:: LEAP
    LOGICAL:: LEAPYEAR
    !
    IF(PRESENT(YEAR)) THEN
        IF(YEAR.NE.Z) THEN
                      LEAPYEAR = ISLEAPYEAR(YEAR)
        ELSEIF(PRESENT(LEAP)) THEN
                      LEAPYEAR = LEAP   ! YEAR=0, YES LEAP
        ELSE
                      LEAPYEAR = TRUE   ! YEAR=0, NO LEAP
        END IF
    ELSE
        IF(PRESENT(LEAP)) THEN
                      LEAPYEAR = LEAP   ! NO YEAR, YES LEAP
        ELSE
                      LEAPYEAR = FALSE  ! NO YEAR, NO LEAP
        END IF
    END IF
    !
  END FUNCTION
  !  
  ELEMENTAL PURE FUNCTION MONTHDAYS(MONTH, YEAR, LEAP) 
    INTEGER, INTENT(IN):: MONTH
    INTEGER, INTENT(IN), OPTIONAL:: YEAR
    LOGICAL, INTENT(IN), OPTIONAL:: LEAP
    INTEGER:: MONTHDAYS
    !
    MONTHDAYS = Z
    !
    IF(MONTH==1 .OR. MONTH==3 .OR. MONTH==5 .OR. MONTH==7 .OR. MONTH==8 .OR. MONTH==10 .OR. MONTH==12) THEN
        MONTHDAYS = 31
    ELSEIF(MONTH==4 .OR. MONTH==6 .OR. MONTH==9 .OR. MONTH==11) THEN
        MONTHDAYS = 30
    ELSEIF( LEAP_CHECK(YEAR, LEAP) ) THEN !MONTH = 2
        MONTHDAYS = 29
    ELSE                                  !MONTH = 2
        MONTHDAYS = 28
    END IF
    !
  END FUNCTION
  !  
  ELEMENTAL PURE FUNCTION JULIANDAY(DAY, MONTH, YEAR, LEAP) 
    !
    INTEGER, INTENT(IN):: DAY, MONTH 
    INTEGER, INTENT(IN), OPTIONAL:: YEAR
    LOGICAL, INTENT(IN), OPTIONAL:: LEAP
    INTEGER:: JULIANDAY
    !
    IF( MONTH > 2 ) THEN
        IF(LEAP_CHECK(YEAR, LEAP)) THEN
            JULIANDAY = DAY + ONE
        ELSE
            JULIANDAY = DAY
        END IF
    ELSE
            JULIANDAY = DAY
    END IF
    !
    IF    (MONTH == 2) THEN
        JULIANDAY = JULIANDAY + 31
    ELSEIF(MONTH == 3) THEN
        JULIANDAY = JULIANDAY + 59
    ELSEIF(MONTH == 4) THEN
        JULIANDAY = JULIANDAY + 90
    ELSEIF(MONTH == 5) THEN
        JULIANDAY = JULIANDAY + 120
    ELSEIF(MONTH == 6) THEN
        JULIANDAY = JULIANDAY + 151
    ELSEIF(MONTH == 7) THEN
        JULIANDAY = JULIANDAY + 181
    ELSEIF(MONTH == 8) THEN
        JULIANDAY = JULIANDAY + 212
    ELSEIF(MONTH == 9) THEN
        JULIANDAY = JULIANDAY + 243
    ELSEIF(MONTH == 10) THEN
        JULIANDAY = JULIANDAY + 273
    ELSEIF(MONTH == 11) THEN
        JULIANDAY = JULIANDAY + 304
    ELSEIF(MONTH == 12) THEN
        JULIANDAY = JULIANDAY + 334
    END IF
    !
  END FUNCTION
  !  
!!!  ELEMENTAL PURE FUNCTION MONTHDAYS(MONTH, YEAR, LEAP) 
!!!    INTEGER, INTENT(IN):: MONTH
!!!    INTEGER, INTENT(IN), OPTIONAL:: YEAR
!!!    LOGICAL, INTENT(IN), OPTIONAL:: LEAP
!!!    INTEGER:: MONTHDAYS
!!!    !
!!!    MONTHDAYS = Z
!!!    !
!!!    SELECT CASE (MONTH)
!!!    CASE (1, 3, 5, 7, 8, 10, 12)
!!!        MONTHDAYS = 31
!!!    CASE (4, 6, 9, 11)
!!!        MONTHDAYS = 30
!!!    CASE (2)
!!!        IF( LEAP_CHECK(YEAR, LEAP) ) THEN
!!!            MONTHDAYS = 29
!!!        ELSE
!!!            MONTHDAYS = 28
!!!        END IF
!!!    END SELECT
!!!    !
!!!  END FUNCTION
!!!  !  
!!!  ELEMENTAL PURE FUNCTION JULIANDAY(DAY, MONTH, YEAR, LEAP) 
!!!    !
!!!    INTEGER, INTENT(IN):: DAY, MONTH 
!!!    INTEGER, INTENT(IN), OPTIONAL:: YEAR
!!!    LOGICAL, INTENT(IN), OPTIONAL:: LEAP
!!!    INTEGER:: JULIANDAY
!!!    INTEGER:: M
!!!    !
!!!    JULIANDAY = Z
!!!    !
!!!    DO M=1, MONTH - 1
!!!        JULIANDAY = JULIANDAY + MONTHDAYS(M,YEAR,LEAP)
!!!    END DO
!!!    !
!!!    JULIANDAY = JULIANDAY + DAY
!!!    !
!!!  END FUNCTION
  !  
!  ELEMENTAL PURE FUNCTION JULIANDAY(DAY, MONTH, YEAR) 
!    ! https://en.wikipedia.org/wiki/Julian_day#Converting_Julian_or_Gregorian_calendar_date_to_Julian_day_number
!    INTEGER, INTENT(IN):: DAY, MONTH, YEAR
!    INTEGER:: JULIANDAY
!    INTEGER:: A, Y, M
!    !
!    A = (14 - MONTH)/12
!    Y = YEAR + 4800 - A
!    M = MONTH + 12*A - 3
!    !
!    IF ( YEAR<=1582 .AND. MONTH<=10 .AND. DAY<=15) THEN         !IF TRUE THEN CAN NOT CALCULATE RETURN FLAG
!        JULIANDAY=-1 
!    ELSE
!        JULIANDAY = DAY + (153*M+2)/5 + 365*Y + Y/4 - Y/100 + Y/400 - 32045
!    END IF
!    !
!  END FUNCTION
  !  
  ELEMENTAL PURE SUBROUTINE JULIANDAY_TO_DATE(JDN_IN, YEAR_IN, DAY, MONTH, YEAR, JDN, LEAP) 
    INTEGER, INTENT(IN):: JDN_IN, YEAR_IN
    INTEGER, INTENT(OUT):: DAY, MONTH, YEAR
    INTEGER, INTENT(OUT), OPTIONAL:: JDN
    LOGICAL, INTENT(IN ), OPTIONAL:: LEAP
    INTEGER:: DOY, MDAY
    LOGICAL:: LEAPYEAR, ZERO_YEAR
    !
    DOY  = JDN_IN
    YEAR = YEAR_IN
    !
    ZERO_YEAR = YEAR == Z
    !
    DO
       LEAPYEAR = LEAP_CHECK(YEAR, LEAP) ! TODO check what happens if JDN_IN >> 366
       !
       IF     ( DOY > 366 .AND.      LEAPYEAR ) THEN
                                                            DOY  = DOY - 366
                                                            YEAR = YEAR + 1
       ELSEIF ( DOY > 365 .AND. .NOT.LEAPYEAR ) THEN     
                                                            DOY  = DOY - 365
                                                            YEAR = YEAR + 1
       ELSEIF ( DOY < 1   .AND.      LEAPYEAR) THEN
                                                            DOY  = DOY + 366
                                                            YEAR = YEAR - 1
       ELSEIF ( DOY < 1) THEN       
                                                            DOY  = DOY + 365
                                                            YEAR = YEAR - 1
       ELSE
           EXIT
       END IF
    END DO
    !
    IF(ZERO_YEAR) THEN
                      IF(PRESENT(LEAP)) YEAR = Z !PREVENT INCREMENT OF YEAR BECAUSE IT IS BEING TREATED AS A FRACTOIN OF YEAR
    END IF
    !
    IF(PRESENT(JDN)) JDN = DOY
    !
    DO MONTH=1, 12
        MDAY= MONTHDAYS(MONTH,YEAR,LEAP)
        DOY = DOY - MDAY
        IF (DOY < 1) THEN
            DOY = DOY + MDAY
            EXIT
        END IF
    END DO
    !
    DAY = DOY
    !
  END SUBROUTINE  
  !  
  ELEMENTAL PURE SUBROUTINE DYEAR_TO_DATE(DYEAR, DAY, MONTH, YEAR, FRAC, JDN, LEAP) 
    REAL(REAL64), INTENT(IN ):: DYEAR
    INTEGER,      INTENT(OUT):: DAY, MONTH, YEAR
    REAL(REAL64), INTENT(OUT), OPTIONAL:: FRAC
    INTEGER,      INTENT(OUT), OPTIONAL:: JDN
    LOGICAL,      INTENT(IN ), OPTIONAL:: LEAP
    INTEGER:: DOY, YR
    REAL(REAL64)::TEMP
    LOGICAL:: LEAPYEAR
    !
    YR = INT(DYEAR)
    !
    LEAPYEAR = LEAP_CHECK(YR, LEAP)
    !
    TEMP = DYEAR - DBLE(YR)
    IF ( LEAPYEAR ) THEN
      DOY = INT(366.0_real64*TEMP)
      TEMP=     366.0_real64*TEMP
    ELSE
      DOY = INT(365.0_real64*TEMP)
      TEMP=     365.0_real64*TEMP
    END IF
    !
    TEMP = TEMP - DBLE(DOY)  !FRACTION OF DAY REMAINING
    !
    IF(UNO - TEMP < 0.5D-8) THEN  !MUST BE WITHIN 0.5 milisecond
        TEMP = DZ
        DOY = DOY + ONE
    END IF
    !
    IF (PRESENT(FRAC)) FRAC = TEMP
    !
    DOY = DOY + ONE                   !MOVE FROM ZERO START TO 1 START
    IF  (PRESENT(JDN)) JDN = DOY
    !
    CALL JULIANDAY_TO_DATE(DOY, YR, DAY, MONTH, YEAR, LEAP=LEAP)
    !
  END SUBROUTINE
  !  
  ELEMENTAL PURE FUNCTION DATE_TO_DYEAR(DAY, MONTH, YEAR, FRAC, LEAP) 
    INTEGER, INTENT(IN):: DAY, MONTH, YEAR
    REAL(REAL64), INTENT(IN), OPTIONAL:: FRAC
    LOGICAL,      INTENT(IN), OPTIONAL:: LEAP
    REAL(REAL64):: DATE_TO_DYEAR
    INTEGER:: JDN
    !
    JDN = JULIANDAY(DAY, MONTH, YEAR, LEAP) 
    !
    DATE_TO_DYEAR = JDN_TO_DYEAR(JDN, YEAR, FRAC, LEAP) 
    !
  END FUNCTION
  !  
  ELEMENTAL PURE FUNCTION JDN_TO_DYEAR(JDN, YEAR, FRAC, LEAP) 
    INTEGER, INTENT(IN):: JDN
    INTEGER,      INTENT(IN), OPTIONAL:: YEAR
    REAL(REAL64), INTENT(IN), OPTIONAL:: FRAC
    LOGICAL,      INTENT(IN), OPTIONAL:: LEAP
    REAL(REAL64):: JDN_TO_DYEAR
    REAL(REAL64):: YR, DOY
    LOGICAL:: LEAPYEAR
    !
    IF(PRESENT(YEAR)) THEN
                          YR = DBLE(YEAR)
    ELSE
                          YR = Z
    END IF
    !
    DOY=DBLE(JDN) - UNO            !MOVE TO MIDNIGHT WHEN THE DAY FIRST STARTS   !  <=========================
    !
    LEAPYEAR = LEAP_CHECK(YEAR, LEAP)
    !
    IF ( LEAPYEAR ) THEN
      JDN_TO_DYEAR = YR + DOY/366.0_real64
    ELSE
      JDN_TO_DYEAR = YR + DOY/365.0_real64 
    END IF
    !
    IF(PRESENT(FRAC)) THEN
      IF(LEAPYEAR) THEN
        JDN_TO_DYEAR = JDN_TO_DYEAR + FRAC/366.0_real64
      ELSE
        JDN_TO_DYEAR = JDN_TO_DYEAR + FRAC/365.0_real64
      END IF
    END IF    
    !
  END FUNCTION
  !  
  ELEMENTAL PURE FUNCTION YEAR_DAY_COUNT(YEAR, LEAP) 
    INTEGER, INTENT(IN), OPTIONAL:: YEAR
    LOGICAL, INTENT(IN), OPTIONAL:: LEAP
    INTEGER:: YEAR_DAY_COUNT
    !
    IF(LEAP_CHECK(YEAR, LEAP))THEN
        YEAR_DAY_COUNT = 366
    ELSE
        YEAR_DAY_COUNT = 365
    END IF
    !
  END FUNCTION
  !  
  ELEMENTAL PURE FUNCTION YEAR_DAY_COUNT_DBLE(YEAR, LEAP) 
    INTEGER, INTENT(IN), OPTIONAL:: YEAR
    LOGICAL, INTENT(IN), OPTIONAL:: LEAP
    REAL(REAL64):: YEAR_DAY_COUNT_DBLE
    !
    IF(LEAP_CHECK(YEAR, LEAP))THEN
        YEAR_DAY_COUNT_DBLE = 366.0_real64
    ELSE
        YEAR_DAY_COUNT_DBLE = 365.0_real64
    END IF
    !
  END FUNCTION
  !
  ELEMENTAL PURE FUNCTION YEAR_FRACTION_DYEAR(DYEAR) RESULT(YFRAC)
    REAL(REAL64), INTENT(IN):: DYEAR
    REAL(REAL64):: YFRAC
    INTEGER:: YEAR
    !
    YEAR = INT(DYEAR)
    IF(YEAR.NE.Z) THEN
        YFRAC = DYEAR - DBLE(YEAR)
    ELSE
        YFRAC = DYEAR
    END IF
    !
  END FUNCTION
  !
  ELEMENTAL PURE FUNCTION YEAR_FRACTION_DMY(DAY, MONTH, YEAR, FRAC, LEAP) RESULT(YFRAC)
    INTEGER,                INTENT(IN):: YEAR, MONTH, DAY
    REAL(REAL64), OPTIONAL, INTENT(IN):: FRAC
    LOGICAL,      OPTIONAL, INTENT(IN):: LEAP
    REAL(REAL64):: YFRAC
    REAL(REAL64):: JDN
    !
    IF(PRESENT(LEAP)) THEN
        JDN = DBLE( JULIANDAY(DAY, MONTH, Z, LEAP) - ONE )
        IF(LEAP) THEN
            YFRAC = JDN/366.0_real64
            IF(PRESENT(FRAC)) YFRAC = YFRAC + FRAC/366.0_real64
        ELSE
            YFRAC = JDN/365.0_real64
            IF(PRESENT(FRAC)) YFRAC = YFRAC + FRAC/365.0_real64
        END IF
    ELSE
        JDN = DBLE( JULIANDAY(DAY, MONTH, YEAR) - ONE )
        !
        IF(ISLEAPYEAR(YEAR)) THEN
            YFRAC = JDN/366.0_real64
            IF(PRESENT(FRAC)) YFRAC = YFRAC + FRAC/366.0_real64
        ELSE
            YFRAC = JDN/365.0_real64
            IF(PRESENT(FRAC)) YFRAC = YFRAC + FRAC/365.0_real64
        END IF
    END IF
    !
  END FUNCTION
  !
  PURE FUNCTION MONTHNAME(MONTHNUM,FULLNAME)  RESULT(MON)
    INTEGER,           INTENT(IN):: MONTHNUM
    LOGICAL, OPTIONAL, INTENT(IN):: FULLNAME
    CHARACTER(:), ALLOCATABLE:: MON
    LOGICAL::FULL
    !
    FULL = FALSE
    IF(PRESENT(FULLNAME)) THEN; FULL = FULLNAME
    END IF
    !
    SELECT CASE(MONTHNUM)
    CASE( 1)
            IF(FULL) THEN
                        MON = 'January'
            ELSE
                        MON = 'JAN'
            END IF
    CASE( 2)
            IF(FULL) THEN
                        MON = 'February'
            ELSE
                        MON = 'FEB'
            END IF
    CASE( 3)
            IF(FULL) THEN
                        MON = 'March'
            ELSE
                        MON = 'MAR'
            END IF
    CASE( 4)
            IF(FULL) THEN
                        MON = 'April'
            ELSE
                        MON = 'APR'
            END IF
    CASE( 5)
            IF(FULL) THEN
                        MON = 'May'
            ELSE
                        MON = 'MAY'
            END IF
    CASE( 6)
            IF(FULL) THEN
                        MON = 'June'
            ELSE
                        MON = 'JUN'
            END IF
    CASE( 7)
            IF(FULL) THEN
                        MON = 'July'
            ELSE
                        MON = 'JUL'
            END IF
    CASE( 8)
            IF(FULL) THEN
                        MON = 'August'
            ELSE
                        MON = 'AUG'
            END IF
    CASE( 9)
            IF(FULL) THEN
                        MON = 'September'
            ELSE
                        MON = 'SEP'
            END IF
    CASE(10)
            IF(FULL) THEN
                        MON = 'October'
            ELSE
                        MON = 'OCT'
            END IF
    CASE(11)
            IF(FULL) THEN
                        MON = 'November'
            ELSE
                        MON = 'NOV'
            END IF
    CASE(12)
            IF(FULL) THEN
                        MON = 'December'
            ELSE
                        MON = 'DEC'
            END IF
    CASE DEFAULT
                        MON = '???'
    END SELECT
    !
  END FUNCTION
  !
  PURE FUNCTION MONTHNUM(MONTHNAME)  RESULT(MON) !RETURN MONTHNUMBER OF -1
    CHARACTER(*),  INTENT(IN):: MONTHNAME
    INTEGER:: MON
    CHARACTER(3):: NAM
    !
    NAM = ADJUSTL(MONTHNAME)
    !
    IF(NAM(1:1).GE.'a' .AND. NAM(1:1).LE.'z') NAM(1:1) = CHAR(ICHAR(NAM(1:1))-IDIFF)
    IF(NAM(2:2).GE.'a' .AND. NAM(2:2).LE.'z') NAM(2:2) = CHAR(ICHAR(NAM(2:2))-IDIFF)
    IF(NAM(3:3).GE.'a' .AND. NAM(3:3).LE.'z') NAM(3:3) = CHAR(ICHAR(NAM(3:3))-IDIFF)
    !
    SELECT CASE(NAM)
    CASE('JAN'); MON = 1 !NOTE THIS WILL ALSO WORK FOR January
    CASE('FEB'); MON = 2 
    CASE('MAR'); MON = 3 
    CASE('APR'); MON = 4 
    CASE('MAY'); MON = 5 
    CASE('JUN'); MON = 6 
    CASE('JUL'); MON = 7 
    CASE('AUG'); MON = 8 
    CASE('SEP'); MON = 9 
    CASE('OCT'); MON = 10
    CASE('NOV'); MON = 11
    CASE('DEC'); MON = 12
    CASE DEFAULT
                 MON = -1
    END SELECT
    !
  END FUNCTION
  !
  PURE ELEMENTAL FUNCTION TIME_TO_DAY_FRACTION_INT_INT_INT(HOUR, MIN, SEC) RESULT(FRAC)
    INTEGER, INTENT(IN):: HOUR, MIN, SEC
    REAL(REAL64):: FRAC
    !
    FRAC = DBLE(HOUR)*HR2DAY + DBLE(MIN)*MIN2DAY + DBLE(SEC)*SEC2DAY
    !FRAC = DBLE(HOUR)/24.  + DBLE(MIN)/1440.  + DBLE(SEC)/86400.
    !
  END FUNCTION
  !
  PURE ELEMENTAL FUNCTION TIME_TO_DAY_FRACTION_INT_INT_DBL(HOUR, MIN, SEC) RESULT(FRAC)
    INTEGER,      INTENT(IN):: HOUR, MIN
    REAL(REAL64), INTENT(IN):: SEC
    REAL(REAL64):: FRAC
    !
    FRAC = DBLE(HOUR)*HR2DAY + DBLE(MIN)*MIN2DAY + SEC*SEC2DAY
    !FRAC = DBLE(HOUR)/24.  + DBLE(MIN)/1440.  + SEC/86400.
    !
  END FUNCTION
  !
  PURE ELEMENTAL FUNCTION TIME_TO_DAY_FRACTION_INT_DBL_DBL(HOUR, MIN, SEC) RESULT(FRAC)
    INTEGER,      INTENT(IN):: HOUR
    REAL(REAL64), INTENT(IN):: SEC, MIN
    REAL(REAL64):: FRAC
    !
    FRAC = DBLE(HOUR)*HR2DAY + MIN*MIN2DAY + SEC*SEC2DAY
    !FRAC = DBLE(HOUR)/24.  + MIN/1440.  + SEC/86400.
    !
  END FUNCTION
  !
  PURE ELEMENTAL FUNCTION TIME_TO_DAY_FRACTION_DBL_DBL_DBL(HOUR, MIN, SEC) RESULT(FRAC)
    REAL(REAL64), INTENT(IN):: HOUR, SEC, MIN
    REAL(REAL64):: FRAC
    !
    FRAC = HOUR*HR2DAY + MIN*MIN2DAY + SEC*SEC2DAY
    !FRAC = HOUR/24.  + MIN/1440.  + SEC/86400.
    !
  END FUNCTION
  !
  ELEMENTAL PURE FUNCTION STR_TIME_TO_DAY_FRACTION(TIME) RESULT(FRAC)
    CHARACTER(*), INTENT(IN):: TIME
    REAL(REAL64):: FRAC
    INTEGER:: I,A,B,C,IERR
    INTEGER:: HOUR, MIN, SEC
    !
    I = SCAN(TIME,'Tt') + ONE
    A = INDEX(TIME,':')
    B = INDEX(TIME,':',TRUE)
    C = LEN_TRIM(TIME)
    !
    IF(C > B + 2) C = B + 2
    IF( A == B  ) B = C
    !
    READ(TIME(I:A-1), *, IOSTAT=IERR) HOUR
    !
    IF(IERR == Z) READ(TIME(A+1:B-1),*, IOSTAT=IERR) MIN
    !
    IF( B == C ) THEN
                          SEC = Z
    ELSEIF(IERR == Z) THEN
                          READ(TIME(B+1:C)  ,*, IOSTAT=IERR) SEC
    END IF
    !
    IF(IERR.NE.Z) THEN
        FRAC = IEEE_VALUE(FRAC, IEEE_QUIET_NAN)
    ELSE
        FRAC = DBLE(HOUR)*HR2DAY + DBLE(MIN)*MIN2DAY + DBLE(SEC)*SEC2DAY
        !FRAC = DBLE(HOUR)/24. + DBLE(MIN)/1440. + DBLE(SEC)/86400.
    END IF
    !
  END FUNCTION
  !
END MODULE
!
!