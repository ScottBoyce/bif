! CODE DEVELOPED BY SCOTT E BOYCE 
!                   CONTACT <seboyce@usgs.gov> or <Boyce@engineer.com>
!
!      CODE THAT PROVIDES A DATE DATA TYPE THAT SUPPORTS DATE OPERATIONS, COMPARISONS, AND INTERPOLATION
!
!                         INCLUDES OPERATIONS FOR CALENDAR DATES, DECIMAL YEARS, AND JULIAN DAY NUMBERS
!
! COMMON VARIABLE NAMES
!    JDN  = Julian Date Number from 1 to 365 or 366
!    DYEAR= Decimal year where the mantissa is the JDN divided by either 365 or 366
!    FRAC = Fraction of a day; 0.0 <= FRAC < 1.0
!    mm   = String representation of the month (e.g. for January '01/15/2015' or '1/15/2015'
!    dd   = String representation of the day of month (e.g. for January 5th '01/05/2015' or '1/5/2015'
!    YYYY = String representation of the year with four digits
!
!    DAY, MONTH, YEAR = SAME AS MEANING in mm/dd/YYYY
!
! MODULE DATE_OPERATOR_INSTRUCTION
!    DERIVED DATE TYPE, DATE_OPERATOR,
!    AND ASSOCIATED FUNBCTIONS THAT HANDLE DATE OPERATIONS
!                                                 VERSION 1.0 [3/20/2015] ORIGINAL VERSION
!
!    EXAMPLE:
!             TYPE(DATE_OPERATOR):: DT
!
!    TO INITIALIZE THE DATE_OPERATOR EITHER IT AS A FUNCTION OR ITS BUILT IN INIT ROUTINE.
!
!    DT = DATE_OPERATOR(INPUT)   OR   CALL DT%INIT(INPUT)
!
!    WHERE (INPUT) CAN BE ONE OF THE FOLLOWING OPTIONS:
!
!       ('mm/dd/YYYY')   or   (DAY, MONTH, YEAR)   or   (JDN, YEAR)   or   (DYEAR)
!       ('yyyy-mm-dd')   or   (DAY, MONTH, YEAR, HOUR, MIN, SEC)
! 
!    SUPPORTED OPERATIONS 
!                        DT+5.0            # ADD      FIVE DAYS TO DT
!                        DT-5.0            # SUBTRACT FIVE DAYS TO DT
!                        DT-DT2            # RETURN NUMBER OF DAYS BETWEEN DT AND DT2
!                        DT=DT2            # SET DT EQUAL TO DT2
!                        DT==DT2           # LOGICAL EQUIVALENCE
!                        DT<DT2; DT<=DT2   # LOGICAL COMPARISONS
!                        DT>DT2; DT>=DT2   # LOGICAL COMPARISONS
!                        WRITE(*,*) DT     # WRITES TO COMMAND PROMPT A PRITTY PRINT OF THE DATE
! 
! FUNCTION LISTING:
!
!     DT%INTERPOLATE(DT1,DT2,DAT1,DAT2)    # LINEAR INTERPOLATE TO DATE, DT, USING (DATE, VALUE) => (DT1, DAT1) and (DT2,DAT2)
!    
!     DT%CONTAIN_MD(M,D,DT2,[INCLUSIVE])   # RETURN TRUE IF MONTH M and DAY D IS STRICTLY BETWEEN DT and DT2; IF INCLUSE IS SET TO TRUE, THEN IT IS AN INCLUSIVE COMPARISION
!     DT%CONTAIN_MD(DT3,DT2,[INCLUSIVE])   # RETURN TRUE IF MONTH AND DAY WITHIN DT3 IS STRICTLY BETWEEN DT and DT2; IF INCLUSE IS SET TO TRUE, THEN IT IS AN INCLUSIVE COMPARISION
!
!     DT%DAYS_TO_MD(M, D)                  # RETURN NUMBER OF DAYS FROM DT TO GIVEN MONTH M AND DAY D
!     DT%DAYS_TO_MD(MD)                    # RETURN NUMBER OF DAYS FROM DT TO GIVEN DATE STORED IN MD
!     
!     DT%STR([SEP],[ISO])                  # RETURN DATE AS A STRING. IF CHAR: SEP IS PRESENT THEN INCLUDES TIME WITH SEP BEING THE SEPARTING BETWEEN DATE AND TIME. ISO IS SET TO TRUE FOR ISO FORMAT OR FALSE FOR AMERICAN
!     DT%STR_MONTHYEAR([FULLNAME])         # RETURN DATE AS THE FORM MMM-YYYY, IF FULLNAME IS SET TO TRUE THEN PRINT AS MMMMMMMM-YYYY (JANAURY-2015) 
!     DT%STR_YEARMONTH([SEP])              # RETURN DATE AS YYYY_MM; IF CHAR: SEP IS SPECIFIED, THEN IT REPLACES THE _ WITH WHATEVER IS PASSED IN 
!     DT%PRETTYPRINT                       # RETURN FULL DATE AS STRING IN THE FORM OF "APRIL 23, 1979"
!     DT%STR_DYEAR([NDEC])                 # RETURN DECIMAL YEAR AS A STRING; IF INTEGER NDEC SPECIFIED, THEN IT SPECIFIES THE NUMBER OF DECIMAL PLACES TO INCLUDE. 
!     
!     DT%DIFF(DT2)                         # RETURN NUMBER OF DAYS BETWEEN DT AND DT2, EQUIVALENT TO DT-DT2
!     DT%TIME()                            # RETURN CHARACTER(8) THAT CONTAINS THE 24HR CLOCK TIME IN DT IN THE FORM "HH:MM:SS"  --THIS CONVERTS DT%FRAC TO CLOCK TIME
!     DT%ISLEAPYEAR()                      # RETURN TRUE/FALSE IF DT IS IN A LEAP YEAR
!     DT%MONTHDAYS()                       # RETURN THE TOTAL NUMBER OF DAYS FOR MONTH IN DT
!     DT%IS_SET()                          # RETURN TRUE IF DATE HAS BEEN SET
!     DT%NOT_SET()
!     DT%NO_YEAR()
!     DT%YEAR_DAY_COUNT(Y)                 # RETURN NUMBEROF DAYS IN YEAR
!     DT%YEAR_FRACTION()                   # RETURN FRACTION OF YEAR FROM 0 to 1, with 0 BEING Jan 1 at midnight
!
! SUBROUTINE LISTING:
!     DT%NOW()                             # SET DT TO CURRENT DATE AND TIME
!    
!     DT%ADD_DAY(DAY,[FRAC])               # NEW DATE_OPERATOR WITH ADDED DAYS AND OPTIONALLY ADDED FRAC
!     DT%ADD_MONTH                         # NEW DATE_OPERATOR WITH ADDED MONTHS
!     DT%ADD_YEAR                          # NEW DATE_OPERATOR WITH ADDED YEARS
!     DT%SET_YEAR (Y)                      # CHANGE DATES YEAR  TO Y
!     DT%SET_MONTH(M)                      # CHANGE DATES MONTH TO M
!     DT%SET_DAY  (D)                      # CHANGE DATES DAY   TO D
!     DT%ZERO_YEAR()                       # REMOVE YEAR FROM DATE AND MAKE IT SO IT JUST IS A MONTH-DAY DATE
!     DT%DESTROY()                         # DEALLOCATE AND RESET DATE (SETS TO -999)
!     DT%SET_DATE_STRING()                 # REBUILD INTERNAL DATE STRING
!  
MODULE DATE_OPERATOR_INSTRUCTION!, ONLY: DATE_OPERATOR
  USE CALENDAR_FUNCTIONS
  USE, INTRINSIC:: IEEE_ARITHMETIC, ONLY: IEEE_VALUE, IEEE_QUIET_NAN
  USE, INTRINSIC:: ISO_FORTRAN_ENV, ONLY: REAL32, REAL64
  IMPLICIT NONE
  PRIVATE
  PUBLIC:: DATE_OPERATOR, DATE_STR_TO_DYEAR!(DATE_STRING)
  !
  ! Date input strings accepted:
  ! yyyy-mm                  mm/yyyy
  ! yyyy-mm-dd               mm/dd/yyyy
  ! yyyy-mm-ddThh:MM:ss      mm/dd/yyyyThh:MM:ss
  ! 
  ! For loading only month and day and set year to zero
  !
  ! mm\dd
  ! mmm    --Three letter month or full month name viz. FEB or Feburary
  ! mmm-dd
  ! mmm/dd
  !
  ! Main Functions for TYPE(DATE_OPERATOR):: DT, DT2
  !
  !   Assignment      DT2 = DT1  or DT2 = DATE_OPERATOR(...)   see INTERFACE DATE_OPERATOR
  !
  !   Arithmetic      DT2 = DT1 + 5 or DT2 = DT1 - 5   ==> Add/Subrtract 5 days
  !
  !                   DIFF = DT%DIFF(DT2) => DT-DT2 in days
  !                   DIFF = DT - DT2     => DT-DT2 in days
  !                   SUMA = DT + DT2     => DT+DT2 in days
  !
  !   Basic Logical   DT == DT2, DT <= DT2, DT >= DT2, DT > DT2, DT < DT2
  !
  !   Increment 
  !              CALL DT%ADD_DAY  (...)  => Adjust date by specified number of days  , fraction of days   are prorated appropriately
  !              CALL DT%ADD_MONTH(...)  => Adjust date by specified number of months, fraction of months are prorated appropriately
  !              CALL DT%ADD_YEAR (...)  => Adjust date by specified number of years , fraction of years  are prorated appropriately
  !   Functions
  !             DT%CONTAIN_MD(MON,DAY,DT2,[INCLUSIVE])  return true if Month and Day are within date range DT : DT2, INCLUSIVE=True means that if MON\DAY == DT2 then return true
  !             DT%CONTAIN_MD(DT3,    DT2,[INCLUSIVE])  same as above, but use Month and Day stored in DT3
  !
  !             DT%INTERPOLATE(DT1,DT2,Y1,Y2)  Return result of interpolating to DT given dates DT1 and DT2 and corresponding values
  !
  !             DT%STR([SEP], [ISO]) Return string representation of date. If SEP provided then it will include time and be separated by SEP. If ISO=False then it will print American style
  !             DT%PRETTYPRINT()      Return full date as string in the form of "April 23, 1979"
  !
  ! Inherited from USE CALENDAR_FUNCTIONS:
  !
  !LOGICAL,      PARAMETER:: TRUE  = .TRUE.
  !LOGICAL,      PARAMETER:: FALSE = .FALSE.
  !INTEGER,      PARAMETER:: IDIFF=ICHAR('a')-ICHAR('A')
  !INTEGER,      PARAMETER:: Z     = 0
  !INTEGER,      PARAMETER:: ONE   = 1
  !REAL(REAL64), PARAMETER:: DZ  = 0.0_REAL64 
  !REAL(REAL64), PARAMETER:: UNO = 1.0_REAL64 
  !
  INTEGER,      PARAMETER:: NINER   = -999
  CHARACTER(7), PARAMETER:: NO_DATE ='NO_DATE'
  !
  CHARACTER,    PARAMETER:: BLNK   = ' '
  CHARACTER,    PARAMETER:: SLASH  = '/'
  CHARACTER,    PARAMETER:: BSLASH = '\'
  CHARACTER,    PARAMETER:: TAB    =  CHAR(9)
  CHARACTER,    PARAMETER:: MINUS  = '-'
  CHARACTER,    PARAMETER:: COL    = ':'
  INTEGER,      PARAMETER:: NEG    = -1
  INTEGER,      PARAMETER:: TWO    = 2
  INTEGER,      PARAMETER:: FIVE   = 5
  REAL(REAL64), PARAMETER:: DNEG   = -1.0_REAL64
  !
  REAL(REAL64), PARAMETER:: YEARTOL = 3.50000E-8_REAL64  ! ~ 1.1 seconds in years
  REAL(REAL64), PARAMETER:: DAYTOL  = 1.27315E-5_REAL64  ! ~ 1.1 seconds in days
  REAL(REAL64), PARAMETER:: HALFSEC = 5.78704E-6_REAL64  ! ~ 0.5 seconds in days
  !
  TYPE DATE_OPERATOR
     CHARACTER(10):: DATE = NO_DATE
     INTEGER:: YEAR  = NINER
     INTEGER:: MONTH = NINER
     INTEGER:: DAY   = NINER
     INTEGER:: JDN   = NINER
     REAL(REAL64):: FRAC  = DZ
     REAL(REAL64):: DYEAR = DZ  
     LOGICAL:: MONTH_DAY = FALSE    !TODO CHANGE YR==Z CHECK TO MONTH_DAY
     !
     CONTAINS
     !
     GENERIC            :: INIT         => INITIALIZE_DATE_OPERATOR_STR,        & !(DATE_STR, [FRAC], [LEAP], [FOUND_DATE], [ONLY_DYEAR], [TIME_SPACE])
                                           INITIALIZE_DATE_OPERATOR_DMY,        & !(DAY, MONTH, YEAR, [FRAC])
                                           INITIALIZE_DATE_OPERATOR_DMYHMS,     & !(DAY, MONTH, YEAR, HOUR, MIN, SEC) HMS int
                                           INITIALIZE_DATE_OPERATOR_DMYHMSdbl,  & !(DAY, MONTH, YEAR, HOUR, MIN, SEC) S is dble
                                           INITIALIZE_DATE_OPERATOR_JY,         & !(JDN, YEAR, [FRAC])
                                           INITIALIZE_DATE_OPERATOR_JYdbl,      & !(JDN, YEAR,       )  JDN is dbl
                                           INITIALIZE_DATE_OPERATOR_DYEAR_DBLE, & !(DYEAR)
                                           INITIALIZE_DATE_OPERATOR_DYEAR_SNGL, & !(DYEAR)
                                           DESTROY_DATE_OPERATOR
     PROCEDURE, PASS(DT):: NOW          => INITIALIZE_DATE_OPERATOR_CURRENT    !()
     PROCEDURE, PASS(DT):: INIT_STR     => INITIALIZE_DATE_OPERATOR_STR        !(DT, DATE_STRING,      [FRAC])
     GENERIC            :: INIT_DMY     => INITIALIZE_DATE_OPERATOR_DMY, INITIALIZE_DATE_OPERATOR_DMYHMS, INITIALIZE_DATE_OPERATOR_DMYHMSdbl
     GENERIC            :: INIT_JY      => INITIALIZE_DATE_OPERATOR_JY, INITIALIZE_DATE_OPERATOR_JYdbl
     GENERIC            :: INIT_DYEAR   => INITIALIZE_DATE_OPERATOR_DYEAR_DBLE, INITIALIZE_DATE_OPERATOR_DYEAR_SNGL 
     GENERIC            :: INTERPOLATE  => DATE_OPERATOR_INTERPOLATE_DBLE, DATE_OPERATOR_INTERPOLATE_SNGL
     PROCEDURE, PASS(DT):: DYEAR_MAKE_DATE !([DYEAR]) --useful if date is only holding dyear and not full date properties, otherwise rebuilds date with passed DYEAR
     PROCEDURE, PASS(DT):: ONLY_DYEAR      !Return true if date only holds DYEAR and nothing else
     PROCEDURE, PASS(DT):: IS_SET       => DATE_HAS_BEEN_SET
     PROCEDURE, PASS(DT):: NOT_SET      => DATE_HAS_NOT_BEEN_SET
     PROCEDURE, PASS(DT):: NO_YEAR      => DATE_HAS_ZERO_YEAR
     GENERIC            :: CONTAIN_MD   => DATE_CONTAINS_MD, DATE_CONTAINS_DATE_MD           !(DT, M, D, DT2, INCLUSIVE); (DT, MD, DT2, INCLUSIVE)  --MD is DATE_OPERATOR
     !
     GENERIC            :: ADD_DAY      => ADD_DAY_INT,   ADD_DAY_DBLE  !(DAY,  FRAC, LEAP)
     GENERIC            :: ADD_MONTH    => ADD_MONTH_INT, ADD_MONTH_DBL !(MON,  LEAP)
     GENERIC            :: ADD_YEAR     => ADD_YEAR_INT,  ADD_YEAR_DBL  !(YEAR, LEAP)
     GENERIC            :: ADD_SEC      => ADD_SEC_INT, ADD_SEC_DBLE    !(SEC,  LEAP)
     GENERIC            :: ADD_MIN      => ADD_MIN_INT, ADD_MIN_DBLE    !(MIN,  LEAP)
     GENERIC            :: ADD_HOUR     => ADD_HOUR_INT, ADD_HOUR_DBLE  !(HOUR, LEAP)
     !
     GENERIC            :: DAYS_TO_MD   => DATE_DAY_COUNT_DATE_TO_MD,   DATE_DAY_COUNT_TO_MD   !(DT, M, D); (DT, MD)  --MD is DATE_OPERATOR
     GENERIC            :: DAYS_FROM_MD => DATE_DAY_COUNT_DATE_FROM_MD, DATE_DAY_COUNT_FROM_MD !(DT, M, D); (DT, MD)  --MD is DATE_OPERATOR
     GENERIC            :: DAY_COUNT_MD => DATE_DAY_COUNT_DATE_MD,      DATE_DAY_COUNT_MD      !(DT, M, D, LOOK_FORWARD); (DT, MD, LOOK_FORWARD)  --MD is DATE_OPERATOR
     !
     PROCEDURE, PASS(DT):: SET_YEAR
     PROCEDURE, PASS(DT):: SET_MONTH
     PROCEDURE, PASS(DT):: SET_DAY
     PROCEDURE, PASS(DT):: ZERO_YEAR
     PROCEDURE, PASS(DT):: MD                 => DATE_MD_COMPARE !(DT, OP, DT2) => DT op DT2 where op = ">", ">=", "<", "<=", or  "=="
     PROCEDURE, PASS(DT):: YEAR_DAY_COUNT     => DATE_YEAR_DAY_COUNT
     PROCEDURE, PASS(DT):: YEAR_DAY_COUNT_DBLE=> DATE_YEAR_DAY_COUNT_DBLE
     PROCEDURE, PASS(DT):: YEAR_FRACTION      => DATE_YEAR_FRACTION
     PROCEDURE, PASS(DT):: DYEAR_FRACTION     => DATE_DYEAR_FRACTION
     PROCEDURE, PASS(DT):: TIME         => DATE_OPERATOR_FRAC_TO_TIME
     PROCEDURE,   NOPASS:: TIME_TO_FRAC => DATE_OPERATOR_TIME_TO_FRAC
     PROCEDURE, PASS(DT):: STR          => DATE_OPERATOR_STRING_REPRESENTATION
     PROCEDURE, PASS(DT):: STR_MONTHYEAR=> DATE_OPERATOR_PRINT_MONTHYEAR
     PROCEDURE, PASS(DT):: STR_YEARMONTH=> DATE_OPERATOR_PRINT_YEAR_MONTH
     PROCEDURE, PASS(DT):: STR_DYEAR    => DATE_OPERATOR_PRINT_DYEAR
     PROCEDURE, PASS(DT):: STR_ELAPSED  => DATE_OPERATOR_PRINT_DIF !(DT, DT2, [UNIT])  UNIT => 0 = all units, 1 = sec, 2 = min, 3 = hour, 4 = day, 5 = largest Unit
     PROCEDURE, PASS(DT):: PRETTYPRINT   => DATE_OPERATOR_PRETTYPRINT
     PROCEDURE, PASS(DT):: ISLEAPYEAR   => ISLEAPYEAR_DATE_OPERATOR  
     GENERIC            :: MONTHDAYS    => MONTHDAYS_DATE_OPERATOR, MONTHDAYS_OPERATOR! ([LEAP]) or (MONTH, YEAR, [LEAP]) => Number of days in date's month or specified MONTH/YEAR
     PROCEDURE, PASS(DT):: DIFF         => DATE_OPERATOR_DAY_DIFFERENCE!(DT, DT2) => DIFF = DT-DT2, result in days
     PROCEDURE, PASS(DT):: DYEAR_IS_YEAR=> DATE_DYEAR_IS_YEAR
     PROCEDURE, PASS(DT):: DESTROY      => DESTROY_DATE_OPERATOR
     PROCEDURE, PASS(DT):: SET_DATE_STRING
     !
     GENERIC            :: READ(FORMATTED)  => DATE_OPERATOR_FMTREAD                                                !fortran User-Defined Derived-Type I/O
     GENERIC            :: READ(UNFORMATTED)=> DATE_OPERATOR_BINREAD
     GENERIC            :: WRITE(FORMATTED) => DATE_OPERATOR_FMTWRITE
     GENERIC            :: OPERATOR(+)      => DATE_OPERATOR_ADD_INT, DATE_OPERATOR_ADD_DBLE, DATE_OPERATOR_ADD_SNGL, DATE_OPERATOR_ADD_INT_REV, DATE_OPERATOR_ADD_DBLE_REV, DATE_OPERATOR_ADD_SNGL_REV
     GENERIC            :: OPERATOR(-)      => DATE_OPERATOR_SUB_INT, DATE_OPERATOR_SUB_DBLE, DATE_OPERATOR_SUB_SNGL, DATE_OPERATOR_SUB
     GENERIC            :: ASSIGNMENT(=)    => COPY_DATE_OPERATOR, STR_TO_DATE_OPERATOR
     GENERIC            :: OPERATOR(==)     => DATE_OPERATOR_EQUALITY
     GENERIC            :: OPERATOR(<)      => DATE_OPERATOR_LESS_THAN
     GENERIC            :: OPERATOR(<=)     => DATE_OPERATOR_LESS_THAN_EQUAL
     GENERIC            :: OPERATOR(>)      => DATE_OPERATOR_GREATER_THAN
     GENERIC            :: OPERATOR(>=)     => DATE_OPERATOR_GREATER_THAN_EQUAL
     !
     PROCEDURE, PASS(DT),PRIVATE:: INITIALIZE_DATE_OPERATOR_CURRENT
     PROCEDURE, PASS(DT),PRIVATE:: INITIALIZE_DATE_OPERATOR_STR        !(DATE_STR,         [FRAC], [LEAP], [FOUND_DATE], [ONLY_DYEAR], [TIME_SPACE])
     PROCEDURE, PASS(DT),PRIVATE:: INITIALIZE_DATE_OPERATOR_DMY        !(DAY, MONTH, YEAR, [FRAC])
     PROCEDURE, PASS(DT),PRIVATE:: INITIALIZE_DATE_OPERATOR_DMYHMS     !(DAY, MONTH, YEAR, HOUR, MIN, SEC, [LEAP]) HMS int
     PROCEDURE, PASS(DT),PRIVATE:: INITIALIZE_DATE_OPERATOR_DMYHMSdbl  !(DAY, MONTH, YEAR, HOUR, MIN, SEC, [LEAP]) S is dble
     PROCEDURE, PASS(DT),PRIVATE:: INITIALIZE_DATE_OPERATOR_JY         !(JDN,        YEAR, [FRAC],         [LEAP])
     PROCEDURE, PASS(DT),PRIVATE:: INITIALIZE_DATE_OPERATOR_JYdbl      !(JDN,        YEAR,                 [LEAP]) JDN is double
     PROCEDURE, PASS(DT),PRIVATE:: INITIALIZE_DATE_OPERATOR_DYEAR_DBLE !(DYEAR)
     PROCEDURE, PASS(DT),PRIVATE:: INITIALIZE_DATE_OPERATOR_DYEAR_SNGL !(DYEAR)
     PROCEDURE, PASS(DT),PRIVATE:: ADD_DAY_INT                        !(DAY,  FRAC, LEAP)
     PROCEDURE, PASS(DT),PRIVATE:: ADD_DAY_DBLE                       !(DAY,  FRAC, LEAP)
     PROCEDURE, PASS(DT),PRIVATE:: ADD_MONTH_INT                      !(MON,  LEAP)
     PROCEDURE, PASS(DT),PRIVATE:: ADD_MONTH_DBL                      !(MON,  LEAP)
     PROCEDURE, PASS(DT),PRIVATE:: ADD_YEAR_INT                       !(YEAR, LEAP)
     PROCEDURE, PASS(DT),PRIVATE:: ADD_YEAR_DBL                       !(YEAR, LEAP)
     PROCEDURE, PASS(DT),PRIVATE:: ADD_SEC_INT                        !(SEC,  LEAP)
     PROCEDURE, PASS(DT),PRIVATE:: ADD_SEC_DBLE                       !(SEC,  LEAP)
     PROCEDURE, PASS(DT),PRIVATE:: ADD_MIN_INT                        !(MIN,  LEAP)
     PROCEDURE, PASS(DT),PRIVATE:: ADD_MIN_DBLE                       !(MIN,  LEAP)
     PROCEDURE, PASS(DT),PRIVATE:: ADD_HOUR_INT                       !(HOUR, LEAP)
     PROCEDURE, PASS(DT),PRIVATE:: ADD_HOUR_DBLE                      !(HOUR, LEAP)
     PROCEDURE, PASS(DT),PRIVATE:: DATE_OPERATOR_INTERPOLATE_DBLE     !(DT1, DT2, DAT1, DAT2)
     PROCEDURE, PASS(DT),PRIVATE:: DATE_OPERATOR_INTERPOLATE_SNGL     !(DT1, DT2, DAT1, DAT2)
     PROCEDURE, PASS(DT),PRIVATE:: DATE_CONTAINS_DATE_MD              !(MD, DT2, INCLUSIVE)  --MD is DATE_OPERATOR
     PROCEDURE, PASS(DT),PRIVATE:: DATE_CONTAINS_MD                   !(M, D, DT2, INCLUSIVE)
     PROCEDURE, PASS(DT),PRIVATE:: DATE_DAY_COUNT_DATE_MD             !(MD,   LOOK_FORWARD)  --MD is DATE_OPERATOR, LOOK_FORWARD = TRUE indicates date MD is ahead of DATE, FALSE, default then MD is behind DATE
     PROCEDURE, PASS(DT),PRIVATE:: DATE_DAY_COUNT_MD                  !(M, D, LOOK_FORWARD)
     PROCEDURE, PASS(DT),PRIVATE:: DATE_DAY_COUNT_DATE_TO_MD          !(MD)  --MD is DATE_OPERATOR
     PROCEDURE, PASS(DT),PRIVATE:: DATE_DAY_COUNT_TO_MD               !(M, D)
     PROCEDURE, PASS(DT),PRIVATE:: DATE_DAY_COUNT_DATE_FROM_MD        !(MD)  --MD is DATE_OPERATOR
     PROCEDURE, PASS(DT),PRIVATE:: DATE_DAY_COUNT_FROM_MD             !(M, D)
     PROCEDURE, PASS(DT),PRIVATE:: DATE_OPERATOR_STRING_REPRESENTATION
     PROCEDURE,          PRIVATE:: DATE_OPERATOR_ADD_DBLE
     PROCEDURE,          PRIVATE:: DATE_OPERATOR_ADD_SNGL
     PROCEDURE,          PRIVATE:: DATE_OPERATOR_FMTREAD
     PROCEDURE,          PRIVATE:: DATE_OPERATOR_BINREAD
     PROCEDURE,          PRIVATE:: DATE_OPERATOR_FMTWRITE
     PROCEDURE,          PRIVATE:: DATE_OPERATOR_ADD_INT
     PROCEDURE,          PRIVATE:: DATE_OPERATOR_SUB_DBLE
     PROCEDURE,          PRIVATE:: DATE_OPERATOR_SUB_SNGL
     PROCEDURE,          PRIVATE:: DATE_OPERATOR_SUB_INT
     PROCEDURE,          PRIVATE:: DATE_OPERATOR_SUB
     PROCEDURE,          PRIVATE:: COPY_DATE_OPERATOR
     PROCEDURE,          PRIVATE:: STR_TO_DATE_OPERATOR
     PROCEDURE,          PRIVATE:: DATE_OPERATOR_EQUALITY
     PROCEDURE,          PRIVATE:: DATE_OPERATOR_LESS_THAN
     PROCEDURE,          PRIVATE:: DATE_OPERATOR_LESS_THAN_EQUAL
     PROCEDURE,          PRIVATE:: DATE_OPERATOR_GREATER_THAN
     PROCEDURE,          PRIVATE:: DATE_OPERATOR_GREATER_THAN_EQUAL
     PROCEDURE, PASS(DT),PRIVATE:: DATE_HAS_BEEN_SET
     PROCEDURE, PASS(DT),PRIVATE:: DATE_HAS_NOT_BEEN_SET
     PROCEDURE, PASS(DT),PRIVATE:: DATE_HAS_ZERO_YEAR
     PROCEDURE, PASS(DT),PRIVATE:: DATE_YEAR_FRACTION
     PROCEDURE, PASS(DT),PRIVATE:: DATE_DYEAR_FRACTION
     PROCEDURE, PASS(DT),PRIVATE:: DATE_OPERATOR_FRAC_TO_TIME
     PROCEDURE, NOPASS,  PRIVATE:: DATE_OPERATOR_TIME_TO_FRAC
     PROCEDURE, PASS(DT),PRIVATE:: ISLEAPYEAR_DATE_OPERATOR  
     PROCEDURE, PASS(DT),PRIVATE:: MONTHDAYS_DATE_OPERATOR 
     PROCEDURE, NOPASS,  PRIVATE:: MONTHDAYS_OPERATOR
     PROCEDURE, PASS(DT),PRIVATE:: DATE_OPERATOR_DAY_DIFFERENCE
     PROCEDURE, PASS(DT),PRIVATE:: DESTROY_DATE_OPERATOR
     !
     PROCEDURE, PASS(DT_IN), PRIVATE:: DATE_OPERATOR_ADD_INT_REV
     PROCEDURE, PASS(DT_IN), PRIVATE:: DATE_OPERATOR_ADD_DBLE_REV
     PROCEDURE, PASS(DT_IN), PRIVATE:: DATE_OPERATOR_ADD_SNGL_REV
  END TYPE
  !
  INTERFACE DATE_OPERATOR
         MODULE PROCEDURE START_OBJECT_DATE_OPERATOR_STR        !(DATE_STRING,      [FRAC])
         MODULE PROCEDURE START_OBJECT_DATE_OPERATOR_DMY        !(DAY, MONTH, YEAR, [FRAC])
         MODULE PROCEDURE START_OBJECT_DATE_OPERATOR_DMYHMS     !(DAY, MONTH, YEAR, HOUR, MIN, SEC, [LEAP])
         MODULE PROCEDURE START_OBJECT_DATE_OPERATOR_DMYHMSdbl  !(DAY, MONTH, YEAR, HOUR, MIN, SEC, [LEAP])
         MODULE PROCEDURE START_OBJECT_DATE_OPERATOR_JY         !(JDN,        YEAR, [FRAC])
         MODULE PROCEDURE START_OBJECT_DATE_OPERATOR_JYdbl      !(JDN,        YEAR)
         MODULE PROCEDURE START_OBJECT_DATE_OPERATOR_DYEAR_DBLE !(DYEAR)
         MODULE PROCEDURE START_OBJECT_DATE_OPERATOR_DYEAR_SNGL !(DYEAR)
         MODULE PROCEDURE START_OBJECT_DATE_OPERATOR_CURRENT    !()
  END INTERFACE
  !
  CONTAINS
  !
  PURE FUNCTION DATE_STR_TO_DYEAR(DATE_STRING) RESULT(DYEAR)
    CHARACTER(*), INTENT(IN):: DATE_STRING
    TYPE(DATE_OPERATOR):: DT
    REAL(REAL64):: DYEAR
    !
    CALL INITIALIZE_DATE_OPERATOR_STR(DT, DATE_STRING, ONLY_DYEAR = TRUE)
    !
    DYEAR = DT%DYEAR
    !
  END FUNCTION
  !
  ELEMENTAL PURE SUBROUTINE DESTROY_DATE_OPERATOR(DT)
    CLASS(DATE_OPERATOR), INTENT(INOUT):: DT    
    DT%DATE  = NO_DATE
    DT%YEAR  = NINER
    DT%MONTH = NINER
    DT%DAY   = NINER
    DT%JDN   = NINER
    DT%FRAC  = DZ
    DT%DYEAR = DZ !IEEE_VALUE(DT%DYEAR, IEEE_QUIET_NAN)
  END SUBROUTINE
  !
  !ELEMENTAL PURE SUBROUTINE FINALIZE_DATE_OPERATOR(DT) --NOT REALLY NEEDED
  !  TYPE(DATE_OPERATOR):: DT    
  !  DT%DATE = 'NO_DATE'
  !  DT%YEAR  = NINER
  !  DT%MONTH = NINER
  !  DT%DAY   = NINER
  !  DT%JDN   = NINER
  !  DT%FRAC  = DZ
  !  DT%DYEAR = DZ
  !END SUBROUTINE
  !
  ELEMENTAL PURE FUNCTION START_OBJECT_DATE_OPERATOR_STR(DATE_STRING, FRAC, LEAP) RESULT(DT)
    CHARACTER(*), INTENT(IN):: DATE_STRING
    REAL(REAL64), INTENT(IN), OPTIONAL:: FRAC
    LOGICAL,      INTENT(IN), OPTIONAL:: LEAP
    TYPE(DATE_OPERATOR):: DT
    !
    CALL INITIALIZE_DATE_OPERATOR_STR(DT, DATE_STRING, FRAC, LEAP)
    !
  END FUNCTION
  !
  ELEMENTAL PURE FUNCTION START_OBJECT_DATE_OPERATOR_DMY(DAY, MONTH, YEAR, FRAC, LEAP) RESULT(DT)
    INTEGER,      INTENT(IN):: DAY, MONTH, YEAR
    REAL(REAL64), INTENT(IN), OPTIONAL:: FRAC
    LOGICAL,      INTENT(IN), OPTIONAL:: LEAP
    TYPE(DATE_OPERATOR):: DT
    !
    CALL INITIALIZE_DATE_OPERATOR_DMY(DT, DAY, MONTH, YEAR, FRAC, LEAP)
    !
  END FUNCTION
  !
  ELEMENTAL PURE FUNCTION START_OBJECT_DATE_OPERATOR_DMYHMS(DAY, MONTH, YEAR, HOUR, MIN, SEC, LEAP) RESULT(DT)
    INTEGER,              INTENT(IN):: DAY, MONTH, YEAR, HOUR, MIN, SEC
    LOGICAL,              INTENT(IN), OPTIONAL:: LEAP
    TYPE(DATE_OPERATOR):: DT
    REAL(REAL64):: FRAC
    
    FRAC = TIME_TO_DAY_FRACTION(HOUR, MIN, SEC)
    !
    CALL INITIALIZE_DATE_OPERATOR_DMY(DT, DAY, MONTH, YEAR, FRAC, LEAP)
    !
  END FUNCTION
  !
  ELEMENTAL PURE FUNCTION START_OBJECT_DATE_OPERATOR_DMYHMSdbl(DAY, MONTH, YEAR, HOUR, MIN, SEC, LEAP) RESULT(DT)
    INTEGER,      INTENT(IN):: DAY, MONTH, YEAR, HOUR, MIN
    REAL(REAL64), INTENT(IN):: SEC
    LOGICAL,      INTENT(IN), OPTIONAL:: LEAP
    TYPE(DATE_OPERATOR):: DT
    REAL(REAL64):: FRAC
    
    FRAC = TIME_TO_DAY_FRACTION(HOUR, MIN, SEC)
    !
    CALL INITIALIZE_DATE_OPERATOR_DMY(DT, DAY, MONTH, YEAR, FRAC, LEAP)
    !
  END FUNCTION
  !
  ELEMENTAL PURE FUNCTION START_OBJECT_DATE_OPERATOR_JY(JDN, YEAR, FRAC, LEAP) RESULT(DT)
    INTEGER,      INTENT(IN   ):: JDN, YEAR
    REAL(REAL64), INTENT(IN   ), OPTIONAL:: FRAC
    LOGICAL,      INTENT(IN), OPTIONAL:: LEAP
    TYPE(DATE_OPERATOR):: DT
    !
    CALL INITIALIZE_DATE_OPERATOR_JY(DT, JDN, YEAR, FRAC, LEAP)
    !
  END FUNCTION
  !
  ELEMENTAL PURE FUNCTION START_OBJECT_DATE_OPERATOR_JYdbl(JDN, YEAR, LEAP) RESULT(DT)
    REAL(REAL64), INTENT(IN   ):: JDN 
    INTEGER,      INTENT(IN   ):: YEAR
    LOGICAL,      INTENT(IN), OPTIONAL:: LEAP
    TYPE(DATE_OPERATOR):: DT
    INTEGER:: JD
    REAL(REAL64):: FRAC
    !
    JD = INT(JDN)
    FRAC = JDN - DBLE(JD)
    IF(ABS(FRAC) < DAYTOL*0.01D0) FRAC = DZ  !less then 0.01 sec
    !
    CALL INITIALIZE_DATE_OPERATOR_JY(DT, JD, YEAR, FRAC, LEAP)
    !
  END FUNCTION
  !
  ELEMENTAL PURE FUNCTION START_OBJECT_DATE_OPERATOR_DYEAR_DBLE(DYEAR, LEAP) RESULT(DT)
    REAL(REAL64),      INTENT(IN):: DYEAR
    LOGICAL, OPTIONAL, INTENT(IN):: LEAP
    TYPE(DATE_OPERATOR):: DT
    !
    CALL INITIALIZE_DATE_OPERATOR_DYEAR_DBLE(DT, DYEAR, LEAP)
    !
  END FUNCTION
  !
  ELEMENTAL PURE FUNCTION START_OBJECT_DATE_OPERATOR_DYEAR_SNGL(DYEAR, LEAP) RESULT(DT)
    REAL(REAL32),         INTENT(IN):: DYEAR
    LOGICAL, OPTIONAL,    INTENT(IN):: LEAP
    TYPE(DATE_OPERATOR):: DT
    !
    CALL INITIALIZE_DATE_OPERATOR_DYEAR_SNGL(DT, DYEAR, LEAP)
    !
  END FUNCTION
  !
  ELEMENTAL IMPURE FUNCTION START_OBJECT_DATE_OPERATOR_CURRENT() RESULT(DT)
    TYPE(DATE_OPERATOR):: DT
    !
    CALL INITIALIZE_DATE_OPERATOR_CURRENT(DT)
    !
  END FUNCTION
  !
  !!!PURE FUNCTION REMOVEBLANK(LN) RESULT(RES)
  !!!  CHARACTER(*),INTENT(IN)::LN
  !!!  CHARACTER(LEN(LN)):: T
  !!!  CHARACTER(:), ALLOCATABLE:: RES
  !!!  INTEGER::I, J
  !!!  !
  !!!  T=BLNK
  !!!  J=1
  !!!  DO I=1, LEN_TRIM(LN)
  !!!     IF(LN(I:I).NE.' ' .AND. LN(I:I).NE.TAB) THEN   !IF NOT BLANK AND NOT TAB THEN ADD CHARACTER
  !!!        T(J:J)=LN(I:I)  
  !!!        J=J+1
  !!!     END IF
  !!!  END DO
  !!!  !
  !!!  ALLOCATE(RES,SOURCE=TRIM(T))
  !!!  !
  !!!END FUNCTION
  !
  !
  ELEMENTAL IMPURE SUBROUTINE INITIALIZE_DATE_OPERATOR_CURRENT(DT, UTC)
    CLASS(DATE_OPERATOR), INTENT(INOUT):: DT
    LOGICAL, OPTIONAL,    INTENT(IN   ):: UTC
    INTEGER, DIMENSION(8):: I
    REAL(REAL64):: FRAC
    !
    CALL DATE_AND_TIME(VALUES = I)  !GET CURRENT TIME VALUES
    !
    !           HOUR                MIN                  SEC                  MS
    FRAC = DBLE(I(5))*HR2DAY + DBLE(I(6))*MIN2DAY + DBLE(I(7))*SEC2DAY + DBLE(I(8))/8.64D7
    !
    IF(PRESENT(UTC)) THEN; IF(UTC) FRAC = FRAC - DBLE(I(4))*MIN2DAY  !Transform from current to UTC
    END IF
    !
    CALL INITIALIZE_DATE_OPERATOR_DMY(DT, I(3), I(2), I(1), FRAC)
    !
  END SUBROUTINE
  !
  ELEMENTAL PURE SUBROUTINE INITIALIZE_DATE_OPERATOR_STR(DT, DATE_STR, FRAC, LEAP, FOUND_DATE, ONLY_DYEAR)
    CLASS(DATE_OPERATOR), INTENT(INOUT):: DT
    CHARACTER(*),         INTENT(IN   ):: DATE_STR
    REAL(REAL64),         INTENT(IN   ), OPTIONAL:: FRAC
    LOGICAL,              INTENT(IN   ), OPTIONAL:: LEAP
    LOGICAL,              INTENT(  OUT), OPTIONAL:: FOUND_DATE ! Returns true if a date was found -- no decimal year or bad dates
    LOGICAL,              INTENT(IN   ), OPTIONAL:: ONLY_DYEAR ! IF TRUE then if found only decimal year input only store that rather then full date --speed purposes
    !
    INTEGER:: I,J,A,B,C,D, YEAR, MONTH, DAY, IERR
    LOGICAL:: ISO, MONTH_STR, HAS_TIME
    !
    ! I = START OF DATE STRING
    ! A = LOCATION OF 1st  / or - or \     --SET TO ZERO IF DECIMAL YEAR
    ! B = LOCATION OF 2nd  / or -          --SET TO ZERO IF NOT FOUND
    ! C = LOCATION OF END OF CALENDAR DATE --e.g. last  d in yyyy-mm-dd or last yyyy-mm-ddThh:MM:ss
    ! D = LOCATION OF START OF WALL CLOCK  --e.g. first h in hh:MM:ss   or first h in yyyy-mm-ddThh:MM:ss; SET TO END OF DATE STRING IF NO CLOCK
    ! J = END OF DATE STRING
    !
    IF(PRESENT(FOUND_DATE)) FOUND_DATE = FALSE
    !
    IF(DATE_STR==NO_DATE) THEN
                              DT%DATE  = NO_DATE
                              DT%YEAR  = Z
                              DT%MONTH = NEG
                              DT%DAY   = NEG
                              DT%JDN   = Z
                              DT%DYEAR = IEEE_VALUE(DT%DYEAR, IEEE_QUIET_NAN)
                              RETURN
    END IF
    !
    IF(EMPTY_STRING(DATE_STR)) THEN
                         DT%DATE  = NO_DATE
                         DT%YEAR  = NINER
                         DT%MONTH = NINER
                         DT%DAY   = NINER
                         DT%JDN   = NINER
                         DT%DYEAR = IEEE_VALUE(DT%DYEAR, IEEE_QUIET_NAN)
                         RETURN
    END IF
    !
    I = INDEX(DATE_STR,'{')
    IF(I > Z) THEN
        I = I + ONE
        J = INDEX(DATE_STR,'}') - ONE
        IF(J < ONE) THEN                             !ERROR FINDING MATCHING { }
                         DT%DATE  = TRIM(DATE_STR)
                         DT%YEAR  = NINER
                         DT%MONTH = NINER
                         DT%DAY   = NINER
                         DT%JDN   = NINER
                         DT%DYEAR = IEEE_VALUE(DT%DYEAR, IEEE_QUIET_NAN)
                         RETURN
        END IF
    ELSE
        I = ONE
        J = LEN_TRIM(DATE_STR)
    END IF
    !
    YEAR  = NEG
    MONTH = NEG
    DAY   = NEG
    !
    A = INDEX(DATE_STR,BSLASH)
    DT%MONTH_DAY = A > Z
    !
    IF(DT%MONTH_DAY) THEN
        B = Z
    ELSE
        A = INDEX(DATE_STR,SLASH)
        !
        IF(A==Z) THEN
            A=INDEX(DATE_STR,MINUS)
            IF(I == A .OR. A == Z) THEN                ! MINUS IS LOCATED AT START OF STRING SO IT MUST BE A NUMBER
                         A = Z
                         B = Z
            ELSE
                         B=INDEX(DATE_STR,MINUS,TRUE)
            END IF
            !
            IF(A > ONE) THEN  !CHECK TO MAKE SURE IT IS NOT A NUMBER OF THE FORM 1E-1 OR 1D-1
                  IF(INDEX("EeDe",DATE_STR(A-1:A-1)) > Z) THEN
                      A = Z
                      B = Z
                  END IF
            END IF
            ISO = TRUE
        ELSE
            B=INDEX(DATE_STR,SLASH,TRUE)
            ISO = FALSE
        END IF
    END IF
    !
    MONTH     = MONTHNUM(DATE_STR(I:J))  !CHECK IF USIGN MONTHNAME e.g. "JAN"
    MONTH_STR = MONTH > Z
    !
    IF(MONTH_STR .OR. A > Z) THEN !IT IS --NOT-- A DECIMAL YEAR
        !
        D = INDEX(DATE_STR,':')  !DERMINE IF THERE IS A WALL CLOCK
        !
        IF( D > Z ) THEN
            HAS_TIME = TRUE  ! Thh:mm:ss  D = first : adjust to first h or T
            D = D - TWO
        ELSE
            HAS_TIME = FALSE
            D = J
        END IF
        !
        IF (A < B) THEN  ! C IS THE END OF THE CALENDAR DATE PART --e.g last y in mm/dd/yyyy
            C = B + ONE
        ELSE
            C = A + ONE
        END IF
        !
        DO WHILE (C < D)
            IF(DATE_STR(C:C).NE.BLNK .AND. DATE_STR(C:C).NE.TAB) EXIT     !FIND FIRST NONBLANK 
            C = C + ONE
        END DO
        !
        DO WHILE (C < D)
            IF(DATE_STR(C:C)==BLNK .OR. DATE_STR(C:C)==TAB) EXIT        !FIND FIRST BLNK
            C = C + ONE
        END DO
        IF(C < J) C = C - ONE
        !
        IF(HAS_TIME) THEN; IF(DATE_STR(C:C)=='T' .OR. DATE_STR(C:C)=='t') C = C - ONE
        END IF
    END IF
    !
    IF(MONTH_STR) THEN                                                               ! mmm or mmm-dd or mmm-dd-yyyy
                ! 
                IF    (A==Z) THEN
                                  DT%MONTH_DAY = TRUE
                                  DAY  = ONE
                                  YEAR = Z
                                  IERR = Z
                ELSEIF(A==B) THEN
                                  DT%MONTH_DAY = TRUE
                                  READ(DATE_STR(A+1:C  ),*, IOSTAT=IERR) DAY
                                  YEAR  = Z
                ELSE
                                  READ(DATE_STR(A+1:B-1),*, IOSTAT=IERR) DAY
                    IF(IERR == Z) READ(DATE_STR(B+1:C  ),*, IOSTAT=IERR) YEAR
                END IF
                !
    ELSEIF(A==Z) THEN  !ASSUME ITS A DECIMAL YEAR
                !
                READ(DATE_STR(I:J),*, IOSTAT=IERR) DT%DYEAR
                IF(IERR==Z) THEN
                               !
                               IF(PRESENT(ONLY_DYEAR)) THEN
                                   ISO = .NOT. ONLY_DYEAR
                               ELSE
                                   ISO = TRUE
                               END IF
                               !
                               IF(DT%DYEAR.NE.DT%DYEAR) THEN
                                      DT%DATE  = TRIM(DATE_STR)
                                      DT%YEAR  = NINER
                                      DT%MONTH = NINER
                                      DT%DAY   = NINER
                                      DT%JDN   = NINER
                                      !DT%DYEAR = IEEE_VALUE(DT%DYEAR, IEEE_QUIET_NAN)
                               ELSEIF(ISO) THEN !BUILD DATE FROM DYEAR
                                           CALL DYEAR_TO_DATE(DT%DYEAR, DT%DAY, DT%MONTH, DT%YEAR, DT%FRAC, DT%JDN, LEAP) 
                                           CALL DT%SET_DATE_STRING(DT%DAY, DT%MONTH, DT%YEAR)   !SET DT%DATE
                               ELSE !ONYL STORE DYEAR
                                      DT%DATE  = 'DYEAR'
                                      DT%YEAR  = Z
                                      DT%MONTH = NEG
                                      DT%DAY   = NEG
                                      DT%JDN   = Z
                               END IF
                               !
                               RETURN
                END IF
                !
    ELSEIF(DT%MONTH_DAY) THEN                                                                !mm\dd
                YEAR  = Z
                MONTH = NEG
                DAY   = NEG
                !
                READ(DATE_STR(  I:A-1),*, IOSTAT=IERR) MONTH
                !
                IF(IERR == Z) READ(DATE_STR(A+1:C),*, IOSTAT=IERR) DAY
                !
                IF( DAY > MONTHDAYS(MONTH, YEAR, TRUE) ) IERR = ONE  !BAD DAY OF MONTH
                !
    ELSEIF(ISO) THEN                                                                         !yyyy-mm-dd
                READ(                               DATE_STR(  I:A-1),*, IOSTAT=IERR) YEAR
                !
                IF(IERR == Z                 ) READ(DATE_STR(A+1:B-1),*, IOSTAT=IERR) MONTH
                !
                IF(A==B) THEN
                             DAY = ONE
                ELSEIF(IERR == Z .AND. DAY == NEG) THEN
                                               READ(DATE_STR(B+1:C  ),*, IOSTAT=IERR) DAY
                END IF
    ELSEIF(A==B) THEN                                                                        !mm/yyyy
                READ(                               DATE_STR(  I:A-1),*, IOSTAT=IERR) MONTH
                !
                DAY = ONE
                !
                IF(IERR == Z                 ) READ(DATE_STR(B+1:C  ),*, IOSTAT=IERR) YEAR
    ELSE                                                                                     !mm/dd/yyyy
                READ(                               DATE_STR(  I:A-1),*, IOSTAT=IERR) MONTH
                !
                IF(IERR == Z .AND. DAY == NEG) READ(DATE_STR(A+1:B-1),*, IOSTAT=IERR) DAY
                !
                IF(IERR == Z                 ) READ(DATE_STR(B+1:C  ),*, IOSTAT=IERR) YEAR
    END IF
    !
    IF(     IERR.NE.Z                                                     &  !BAD DATE FOUND --NOTE IF DATE_STR='' THEN DAY AND MONTH WILL EQUAL ZER0
       .OR. MONTH< ONE .OR. MONTH > 12                                    &
       .OR. DAY  < ONE                                                    &
       .OR. (DAY > MONTHDAYS(MONTH, YEAR, LEAP) .AND. .NOT. DT%MONTH_DAY) &
      ) THEN  
       DT%DATE  = TRIM(DATE_STR)
       DT%YEAR  = NINER
       DT%MONTH = NINER
       DT%DAY   = NINER
       DT%JDN   = NINER
       DT%DYEAR = IEEE_VALUE(DT%DYEAR, IEEE_QUIET_NAN)
       RETURN
    END IF
    !
    DT%DAY   = DAY
    DT%MONTH = MONTH
    DT%YEAR  = YEAR
    !
    IF(HAS_TIME) THEN
        !
        DT%FRAC = TIME_TO_DAY_FRACTION(DATE_STR(D:J))
        !
        IF(DT%FRAC.NE.DT%FRAC) THEN
                                 DT%DATE  = TRIM(DATE_STR)
                                 DT%YEAR  = NINER
                                 DT%MONTH = NINER
                                 DT%DAY   = NINER
                                 DT%JDN   = NINER
                                 DT%DYEAR = IEEE_VALUE(DT%DYEAR, IEEE_QUIET_NAN)
                                 RETURN
        END IF
    ELSE
        DT%FRAC = DZ
    END IF
    !
    IF(PRESENT(FRAC))  DT%FRAC = DT%FRAC + FRAC
    !
    DT%JDN = JULIANDAY(DAY, MONTH, YEAR, LEAP)
    !
    IF(DT%FRAC > UNO .OR. DT%FRAC < DZ) THEN
        CALL ADD_DAY_INT(DT, LEAP=LEAP)  !NOTE THIS WILL JUST NORMALIZE FRAC CAUSE THERE IS NO ADDED DAY NOR FRAC --NOTE AUTO SETS DYEAR AND STRING
    ELSE
        DT%DYEAR = JDN_TO_DYEAR(DT%JDN, YEAR, DT%FRAC, LEAP) 
        !
        CALL DT%SET_DATE_STRING(DAY, MONTH, YEAR)   !SET DT%DATE
    END IF
    !
    IF(PRESENT(FOUND_DATE)) FOUND_DATE = TRUE
    !
  END SUBROUTINE
  !
!  ELEMENTAL PURE SUBROUTINE INITIALIZE_DATE_OPERATOR_STR(DT, DATE_STRING, FRAC, LEAP, FOUND_DATE, ONLY_DYEAR)
!    CLASS(DATE_OPERATOR), INTENT(INOUT):: DT
!    CHARACTER(*),         INTENT(IN   ):: DATE_STRING
!    REAL(REAL64),     INTENT(IN   ), OPTIONAL:: FRAC
!    LOGICAL,              INTENT(IN   ), OPTIONAL:: LEAP
!    LOGICAL,              INTENT(  OUT), OPTIONAL:: FOUND_DATE
!    LOGICAL,              INTENT(IN   ), OPTIONAL:: ONLY_DYEAR
!    CHARACTER(:),ALLOCATABLE:: DATE_STR
!    INTEGER:: I,J,A,B,C, YEAR, MONTH, DAY, IERR
!    LOGICAL:: ISO, MONTH_DAY
!    !
!    IF(PRESENT(FOUND_DATE)) FOUND_DATE = FALSE
!    !
!    IF(DATE_STRING==NO_DATE) THEN
!                              DT%DATE  = NO_DATE
!                              DT%YEAR  = 0
!                              DT%MONTH = 1
!                              DT%DAY   = 1
!                              DT%JDN   = 1
!                              DT%DYEAR = DZ
!                              RETURN
!    END IF
!    !
!    ALLOCATE(DATE_STR, SOURCE = REMOVEBLANK(DATE_STRING))
!    !
!    A = INDEX(DATE_STR,BSLASH)
!    MONTH_DAY = A > Z
!    !
!    IF(.NOT. MONTH_DAY) THEN
!                            A=INDEX(DATE_STR,SLASH)
!                            B=INDEX(DATE_STR,SLASH, TRUE)
!                            !
!                            IF(A==Z .AND. B==Z) THEN
!                                A=INDEX(DATE_STR,MINUS)
!                                B=INDEX(DATE_STR,MINUS,TRUE)
!                                ISO = TRUE
!                            ELSE
!                                ISO = FALSE
!                            END IF
!                            !
!                            IF(A==B) THEN
!                                DAY = ONE
!                            ELSE
!                                DAY = NEG
!                            END IF
!                            !
!                            MONTH = NEG
!    END IF
!    !
!    C=SCAN(DATE_STR,"Tt",TRUE)  !CHECK IF THERE IS A 24 HOUR CLOCK
!    !
!    IF(C==Z) C = LEN(DATE_STR)+ONE
!    !
!    IF(A==Z.AND.B==Z) THEN  !ASSUME ITS A DECIMAL YEAR
!                !
!                READ(DATE_STR,*, IOSTAT=IERR) DT%DYEAR
!                IF(IERR==Z) THEN
!                               !
!                               IF(PRESENT(ONLY_DYEAR)) THEN
!                                   ISO = .NOT. ONLY_DYEAR
!                               ELSE
!                                   ISO = TRUE
!                               END IF
!                               !
!                               IF(ISO) THEN !BUILD DATE FROM DYEAR
!                                           CALL DYEAR_TO_DATE(DT%DYEAR, DT%DAY, DT%MONTH, DT%YEAR, DT%FRAC, DT%JDN, LEAP) 
!                                           CALL DT%SET_DATE_STRING(DT%DAY, DT%MONTH, DT%YEAR)   !SET DT%DATE
!                               ELSE !ONYL STORE DYEAR
!                                      DT%DATE  = TRIM(DATE_STRING)
!                                      DT%YEAR  = Z
!                                      DT%MONTH = NEG
!                                      DT%DAY   = NEG
!                                      DT%JDN   = Z
!                               END IF
!                               !
!                               RETURN
!                END IF
!                !
!    ELSEIF(MONTH_DAY) THEN
!                YEAR  = Z
!                MONTH = NEG
!                DAY   = NEG
!                !
!                READ(DATE_STR(   :A-1),*, IOSTAT=IERR) MONTH
!                !
!                IF(IERR == Z) READ(DATE_STR(A+1:C-1),*, IOSTAT=IERR) DAY
!                !
!    ELSEIF(ISO) THEN
!                READ(                               DATE_STR(   :A-1),*, IOSTAT=IERR) YEAR
!                !
!                IF(IERR == Z                 ) READ(DATE_STR(A+1:B-1),*, IOSTAT=IERR) MONTH
!                !
!                IF(IERR == Z .AND. DAY == NEG) READ(DATE_STR(B+1:C-1),*, IOSTAT=IERR) DAY
!    ELSE
!                READ(                               DATE_STR(   :A-1),*, IOSTAT=IERR) MONTH
!                !
!                IF(IERR == Z .AND. DAY == NEG) READ(DATE_STR(A+1:B-1),*, IOSTAT=IERR) DAY
!                !
!                IF(IERR == Z                 ) READ(DATE_STR(B+1:C-1),*, IOSTAT=IERR) YEAR
!    END IF
!    !
!    IF(IERR .NE. Z .OR. DAY < ONE .OR. MONTH < ONE .OR. MONTH>12) THEN  !BAD DATE FOUND --NOTE IF DATE_STR='' THEN DAY AND MONTH WILL EQUAL ZER0
!       DT%DATE  = TRIM(DATE_STRING)
!       DT%YEAR  = NINER
!       DT%MONTH = NINER
!       DT%DAY   = NINER
!       DT%JDN   = NINER
!       DT%DYEAR = IEEE_VALUE(DT%DYEAR, IEEE_QUIET_NAN)
!       RETURN
!    END IF
!    !
!    DT%DAY   = DAY
!    DT%MONTH = MONTH
!    DT%YEAR  = YEAR
!    IF(PRESENT(FRAC)) THEN
!        DT%FRAC = FRAC
!    ELSE
!        DT%FRAC = DT%TIME_TO_FRAC(DATE_STR)
!        !
!        IF(.NOT. DT%FRAC==DT%FRAC) DT%FRAC = DZ
!    END IF
!    !
!    DT%JDN = JULIANDAY(DAY, MONTH, YEAR, LEAP)
!    !
!    DT%DYEAR = JDN_TO_DYEAR(DT%JDN, YEAR, DT%FRAC, LEAP)
!    !
!    CALL DT%SET_DATE_STRING(DAY, MONTH, YEAR)   !SET DT%DATE
!    !
!    IF(PRESENT(FOUND_DATE)) FOUND_DATE = TRUE
!    !
!  END SUBROUTINE
  !
  ELEMENTAL PURE SUBROUTINE INITIALIZE_DATE_OPERATOR_DMYHMS(DT, DAY, MONTH, YEAR, HOUR, MIN, SEC, LEAP)
    CLASS(DATE_OPERATOR), INTENT(INOUT):: DT
    INTEGER,              INTENT(IN   ):: DAY, MONTH, YEAR, HOUR, MIN, SEC
    LOGICAL,              INTENT(IN   ), OPTIONAL:: LEAP
    REAL(REAL64):: FRAC
    !
    FRAC = TIME_TO_DAY_FRACTION(HOUR, MIN, SEC)
    CALL INITIALIZE_DATE_OPERATOR_DMY(DT, DAY, MONTH, YEAR, FRAC, LEAP)
    !
  END SUBROUTINE
  !
  ELEMENTAL PURE SUBROUTINE INITIALIZE_DATE_OPERATOR_DMYHMSdbl(DT, DAY, MONTH, YEAR, HOUR, MIN, SEC, LEAP)
    CLASS(DATE_OPERATOR), INTENT(INOUT):: DT
    INTEGER,              INTENT(IN   ):: DAY, MONTH, YEAR, HOUR, MIN
    REAL(REAL64),         INTENT(IN   ):: SEC
    LOGICAL,              INTENT(IN   ), OPTIONAL:: LEAP
    REAL(REAL64):: FRAC
    !
    FRAC = TIME_TO_DAY_FRACTION(HOUR, MIN, SEC)
    CALL INITIALIZE_DATE_OPERATOR_DMY(DT, DAY, MONTH, YEAR, FRAC, LEAP)
    !
  END SUBROUTINE
  !
  ELEMENTAL PURE SUBROUTINE INITIALIZE_DATE_OPERATOR_DMY(DT, DAY, MONTH, YEAR, FRAC, LEAP)
    CLASS(DATE_OPERATOR), INTENT(INOUT):: DT
    INTEGER,              INTENT(IN   ):: DAY, MONTH, YEAR
    REAL(REAL64),         INTENT(IN   ), OPTIONAL:: FRAC
    LOGICAL,              INTENT(IN   ), OPTIONAL:: LEAP
    !
    IF(DAY < ONE .OR. MONTH < ONE .OR. MONTH>12) THEN  !BAD DATE FOUND
       CALL DT%SET_DATE_STRING(DAY, MONTH, YEAR)   !SET DT%DATE
       DT%YEAR  = NINER
       DT%MONTH = NINER
       DT%DAY   = NINER
       DT%DYEAR = IEEE_VALUE(DT%DYEAR, IEEE_QUIET_NAN)
       DT%FRAC  = DZ
       RETURN
    END IF
    !
    DT%DAY   = DAY
    DT%MONTH = MONTH
    DT%YEAR  = YEAR
    IF(PRESENT(FRAC)) THEN
        DT%FRAC = FRAC
    ELSE
        DT%FRAC = DZ
    END IF
    !
    DT%JDN = JULIANDAY(DAY, MONTH, YEAR, LEAP)
    !
    IF(DT%FRAC > UNO .OR. DT%FRAC < DZ) THEN
        CALL ADD_DAY_INT(DT, LEAP=LEAP)  !NOTE THIS WILL JUST NORMALIZE FRAC CAUSE THERE IS NO ADDED DAY NOR FRAC --NOTE AUTO SETS DYEAR AND STRING
    ELSE
        DT%DYEAR = JDN_TO_DYEAR(DT%JDN, YEAR, DT%FRAC, LEAP) 
        !
        CALL DT%SET_DATE_STRING(DAY, MONTH, YEAR)   !SET DT%DATE
    END IF
    !
    END SUBROUTINE
    !
  SUBROUTINE INITIALIZE_DATE_OPERATOR_JYdbl(DT, JDN, YEAR, LEAP)
    CLASS(DATE_OPERATOR), INTENT(INOUT):: DT
    REAL(REAL64),         INTENT(IN   ):: JDN
    INTEGER,              INTENT(IN   ):: YEAR
    LOGICAL,              INTENT(IN   ), OPTIONAL:: LEAP
    INTEGER:: JD
    REAL(REAL64):: FRAC
    !
    JD = INT(JDN)
    FRAC = JDN - DBLE(JD)
    IF(ABS(FRAC) < DAYTOL*0.01D0) FRAC = DZ  !less then 0.01 sec
    !
    CALL INITIALIZE_DATE_OPERATOR_JY(DT, JD, YEAR, FRAC, LEAP)
    !
  END SUBROUTINE
  !
  ELEMENTAL PURE SUBROUTINE INITIALIZE_DATE_OPERATOR_JY(DT, JDN, YEAR, FRAC, LEAP)
    CLASS(DATE_OPERATOR), INTENT(INOUT):: DT
    INTEGER,              INTENT(IN   ):: JDN, YEAR
    REAL(REAL64),         INTENT(IN   ), OPTIONAL:: FRAC
    LOGICAL,              INTENT(IN   ), OPTIONAL:: LEAP
    INTEGER:: JD
    !
    JD = JDN
    !
    IF( PRESENT(FRAC) ) THEN
        DT%FRAC = FRAC
        IF ( DT%FRAC > UNO .OR. DT%FRAC < DZ ) THEN
                                                     JD  = JD  + INT(DT%FRAC)     
                                                     DT%FRAC = DT%FRAC - INT(DT%FRAC)
        END IF
        !
        IF ( DT%FRAC < DZ ) THEN
                                                     JD  = JD  - ONE    
                                                     DT%FRAC = DT%FRAC + UNO
        END IF
        !
        IF ( DT%FRAC == UNO ) THEN
                                                     JD  = JD  + ONE     
                                                     DT%FRAC = DZ
        END IF
    ELSE
        DT%FRAC = DZ
    END IF
    !
    CALL JULIANDAY_TO_DATE(JD, YEAR, DT%DAY, DT%MONTH, DT%YEAR, DT%JDN, LEAP)
    !
    DT%MONTH_DAY = YEAR == Z
    !
    DT%DYEAR = JDN_TO_DYEAR(DT%JDN, DT%YEAR, DT%FRAC, LEAP) 
    !
    CALL DT%SET_DATE_STRING(DT%DAY, DT%MONTH, DT%YEAR)   !SET DT%DATE
    !
  END SUBROUTINE
  !
  ELEMENTAL PURE SUBROUTINE INITIALIZE_DATE_OPERATOR_DYEAR_DBLE(DT, DYEAR, LEAP)
    CLASS(DATE_OPERATOR), INTENT(INOUT):: DT
    REAL(REAL64),         INTENT(IN   ):: DYEAR
    LOGICAL, OPTIONAL,    INTENT(IN   ):: LEAP
    !
    DT%DYEAR = DYEAR
    !
    IF(DT%DYEAR.NE.DT%DYEAR) THEN
           DT%DATE  = NO_DATE
           DT%YEAR  = NINER
           DT%MONTH = NINER
           DT%DAY   = NINER
           DT%JDN   = NINER
           !DT%DYEAR = IEEE_VALUE(DT%DYEAR, IEEE_QUIET_NAN)
           DT%MONTH_DAY = FALSE
    ELSE
        CALL DYEAR_TO_DATE(DT%DYEAR, DT%DAY, DT%MONTH, DT%YEAR, DT%FRAC, DT%JDN, LEAP) 
        !
        CALL DT%SET_DATE_STRING(DT%DAY, DT%MONTH, DT%YEAR)   !SET DT%DATE
        !
        DT%MONTH_DAY = DT%YEAR == Z
    END IF
    !
  END SUBROUTINE
  !
  ELEMENTAL PURE SUBROUTINE INITIALIZE_DATE_OPERATOR_DYEAR_SNGL(DT, DYEAR, LEAP)
    CLASS(DATE_OPERATOR), INTENT(INOUT):: DT
    REAL(REAL32),         INTENT(IN   ):: DYEAR
    LOGICAL, OPTIONAL,    INTENT(IN   ):: LEAP
    !
    DT%DYEAR = DBLE(DYEAR)
    !
    IF(DT%DYEAR.NE.DT%DYEAR) THEN
        DT%DATE  = NO_DATE
        DT%YEAR  = NINER
        DT%MONTH = NINER
        DT%DAY   = NINER
        DT%JDN   = NINER
        !DT%DYEAR = IEEE_VALUE(DT%DYEAR, IEEE_QUIET_NAN)
        DT%MONTH_DAY = FALSE
    ELSE
        CALL DYEAR_TO_DATE(DT%DYEAR, DT%DAY, DT%MONTH, DT%YEAR, DT%FRAC, DT%JDN, LEAP) 
        !
        CALL DT%SET_DATE_STRING(DT%DAY, DT%MONTH, DT%YEAR)   !SET DT%DATE
        !
        DT%MONTH_DAY = DT%YEAR == Z
    END IF
    !
  END SUBROUTINE
  !
  ELEMENTAL PURE SUBROUTINE SET_DATE_STRING(DT, DAY, MONTH, YEAR, ISO) 
    CLASS(DATE_OPERATOR), INTENT(INOUT):: DT
    INTEGER,              INTENT(IN   ):: DAY, MONTH, YEAR
    LOGICAL, OPTIONAL,    INTENT(IN   ):: ISO
    !
    IF(PRESENT(ISO)) THEN
         IF(ISO) THEN
                     WRITE(DT%DATE,'(I4.4, A ,I2.2, A ,I2.2)') YEAR, MINUS, MONTH, MINUS, DAY   !ISO STANDARD
         ELSE
                     WRITE(DT%DATE,'(I2.2, A ,I2.2, A ,I4.4)') MONTH, SLASH, DAY, SLASH, YEAR  !USA Standard
         END IF
    ELSE
                     WRITE(DT%DATE,'(I4.4, A ,I2.2, A ,I2.2)') YEAR, MINUS, MONTH, MINUS, DAY   !ISO STANDARD
    END IF
    !
  END SUBROUTINE
  !
  SUBROUTINE DATE_OPERATOR_FMTREAD(DT, UNIT, IOTYPE, V_LIST, IOSTAT, IOMSG)
    CLASS(DATE_OPERATOR), INTENT(INOUT):: DT
    INTEGER,              INTENT(IN   ):: UNIT
    CHARACTER(*),         INTENT(IN   ):: IOTYPE
    INTEGER,              INTENT(IN   ):: V_LIST (:)
    INTEGER,              INTENT(OUT  ):: IOSTAT
    CHARACTER(*),         INTENT(INOUT):: IOMSG
    CHARACTER(19):: DATE_TIME
    !
    ! This is the child I/O that gets performed when the procedure
    ! is called from a parent I/O - it uses list-directed input to read
    ! the array K
    !
    READ (UNIT, FMT='(A)', IOSTAT=IOSTAT, IOMSG=IOMSG) DATE_TIME   !SCOTT MAY HAVE TO CHANGE '(A)' TO *
    !
    IF(IOSTAT==Z) THEN
        CALL DT%INIT( DATE_TIME )
        IF( DT%NOT_SET() ) THEN
            IOSTAT=123
            IOMSG='DATE_OPERATOR_ERROR'
        END IF
    END IF
    !
  END SUBROUTINE 
  !
  SUBROUTINE DATE_OPERATOR_BINREAD(DT, UNIT, IOSTAT, IOMSG)
    CLASS(DATE_OPERATOR), INTENT(INOUT):: DT
    INTEGER,              INTENT(IN   ):: UNIT
    INTEGER,              INTENT(OUT  ):: IOSTAT
    CHARACTER(*),         INTENT(INOUT):: IOMSG
    INTEGER:: POS
    CHARACTER(19):: DATE_TIME
    !
    ! This is the child I/O that gets performed when the procedure
    ! is called from a parent I/O - it uses list-directed input to read
    ! the array K
    !
    INQUIRE(UNIT=UNIT, POS=POS, ACCESS=DATE_TIME)
    !
    IF(DATE_TIME=='STREAM') THEN
        DATE_TIME=BLNK
        READ (UNIT, IOSTAT=IOSTAT, IOMSG=IOMSG, POS=POS) DATE_TIME !yyyy-mm-ddThh:mm:ss
        IF(IOSTAT.NE.Z .OR. SCAN(DATE_TIME,'Tt')==Z) THEN
            DATE_TIME=BLNK
            READ (UNIT, IOSTAT=IOSTAT, IOMSG=IOMSG, POS=POS) DATE_TIME(:10)  !yyyy-mm-dd
        END IF
    ELSE
            IOSTAT=666
            IF(LEN(IOMSG)>FIVE) IOMSG='STREAM_BINARY_ONLY'
    END IF
    !
    IF(IOSTAT==Z) THEN
        CALL DT%INIT( DATE_TIME )
        IF( DT%NOT_SET() ) THEN
            IOSTAT=123
            IOMSG='DATE_OPERATOR_ERROR'
        END IF
    END IF
    !
  END SUBROUTINE 
  !
  SUBROUTINE DATE_OPERATOR_FMTWRITE(DT, UNIT, IOTYPE, V_LIST, IOSTAT, IOMSG)  !* or '(DT"IOTYPE")' or '(DT)'
    CLASS(DATE_OPERATOR), INTENT(IN   ):: DT
    INTEGER,              INTENT(IN   ):: UNIT
    CHARACTER(*),         INTENT(IN   ):: IOTYPE
    INTEGER,              INTENT(IN   ):: V_LIST (:)
    INTEGER,              INTENT(OUT  ):: IOSTAT
    CHARACTER(*),         INTENT(INOUT):: IOMSG
    INTEGER:: N, M
    !
    ! This is the child I/O that gets performed when the procedure
    ! is called from a parent I/O - it uses list-directed input to read
    ! the array K
    !
    N = LEN_TRIM(IOTYPE)
    M = N
    IF(N>=7) M = 7
    !
    SELECT CASE(IOTYPE(:M))
    CASE('DTDYEAR','DTdyear','DTDYear','DTDyear') ! "DTDYEAR F15.4"
                M=M+1
                IF(N>=7 .AND. N-M > ONE) THEN
                    WRITE(UNIT, FMT='('//IOTYPE(M:N)//')', IOSTAT=IOSTAT, IOMSG=IOMSG) DT%DYEAR
                ELSE
                    WRITE(UNIT, FMT='(F17.12)', IOSTAT=IOSTAT, IOMSG=IOMSG) DT%DYEAR
                END IF
    CASE DEFAULT
         !
         SELECT CASE(IOTYPE)
         CASE('DTISO','DTiso','DTIso')
                     WRITE(UNIT, FMT='(A)', IOSTAT=IOSTAT, IOMSG=IOMSG) DT%STR('T')   !SCOTT MAY HAVE TO CHANGE '(A)' TO *
         CASE('DTNOISO','DTnoiso','DTNoiso','DTNoIso')
                     WRITE(UNIT, FMT='(A)', IOSTAT=IOSTAT, IOMSG=IOMSG) DT%STR('T',FALSE)
         CASE('DTNOTIME','DTnotime','DTNotime','DTNoTime')
                     WRITE(UNIT, FMT='(A)', IOSTAT=IOSTAT, IOMSG=IOMSG) DT%STR()
         CASE('LISTDIRECTED')
                     WRITE(UNIT, FMT=*,     IOSTAT=IOSTAT, IOMSG=IOMSG) DT%STR('T')   
         CASE('NAMELIST')
                     WRITE(UNIT, FMT='(A)', IOSTAT=IOSTAT, IOMSG=IOMSG) DT%STR('T')
         CASE DEFAULT
              IF(LEN(IOTYPE) > TWO) THEN
                    WRITE(UNIT, FMT='(A)', IOSTAT=IOSTAT, IOMSG=IOMSG) DT%STR(IOTYPE(3:))   
              ELSE
                     WRITE(UNIT, FMT='(A)', IOSTAT=IOSTAT, IOMSG=IOMSG) DT%STR('T')   
              END IF
         END SELECT
    END SELECT
    !
  END SUBROUTINE 
  !
  ELEMENTAL PURE SUBROUTINE DYEAR_MAKE_DATE(DT, DYEAR) 
    CLASS(DATE_OPERATOR),       INTENT(INOUT):: DT
    REAL(REAL64),     OPTIONAL, INTENT(IN   ):: DYEAR
    !
    IF(DT%DATE .NE. NO_DATE .AND. DT%DYEAR==DT%DYEAR) THEN
        !
        !BUILD DATE FROM DYEAR
        CALL DYEAR_TO_DATE(DT%DYEAR, DT%DAY, DT%MONTH, DT%YEAR, DT%FRAC, DT%JDN) 
        CALL DT%SET_DATE_STRING(DT%DAY, DT%MONTH, DT%YEAR)   !SET DT%DATE
        !
    ELSEIF(PRESENT(DYEAR)) THEN
        !
        CALL INITIALIZE_DATE_OPERATOR_DYEAR_DBLE(DT,DYEAR) 
        !
    ELSEIF(DT%DAY == NINER .OR. DT%DATE == NO_DATE) THEN
         DT%DATE  = NO_DATE
         DT%YEAR  = NINER
         DT%MONTH = NINER
         DT%DAY   = NINER
         DT%JDN   = NINER
         !DT%DYEAR = DZ
         DT%DYEAR = IEEE_VALUE(DT%DYEAR, IEEE_QUIET_NAN)
    END IF
    !
  END SUBROUTINE
  !
  ELEMENTAL PURE FUNCTION ISLEAPYEAR_DATE_OPERATOR(DT) 
    CLASS(DATE_OPERATOR), INTENT(IN):: DT
    LOGICAL::ISLEAPYEAR_DATE_OPERATOR
    !
    ISLEAPYEAR_DATE_OPERATOR = ISLEAPYEAR(DT%YEAR) 
    !
  END FUNCTION
  !
  ELEMENTAL PURE FUNCTION MONTHDAYS_DATE_OPERATOR(DT,LEAP) 
    CLASS(DATE_OPERATOR), INTENT(IN):: DT
    LOGICAL,              INTENT(IN), OPTIONAL:: LEAP
    INTEGER::MONTHDAYS_DATE_OPERATOR
    !
    MONTHDAYS_DATE_OPERATOR = MONTHDAYS(DT%MONTH,DT%YEAR,LEAP) 
    !
  END FUNCTION
  !
  ELEMENTAL PURE FUNCTION MONTHDAYS_OPERATOR(MONTH,YEAR,LEAP) 
    INTEGER,  INTENT(IN):: MONTH, YEAR
    LOGICAL,  INTENT(IN), OPTIONAL:: LEAP
    INTEGER:: MONTHDAYS_OPERATOR
    !
    MONTHDAYS_OPERATOR = MONTHDAYS(MONTH,YEAR,LEAP) 
    !
  END FUNCTION
  !
  ELEMENTAL PURE SUBROUTINE ADD_DAY_DBLE(DT, DAY, FRAC, LEAP)
    CLASS(DATE_OPERATOR), INTENT(INOUT):: DT
    REAL(REAL64),         INTENT(IN   ):: DAY
    REAL(REAL64),         INTENT(IN   ), OPTIONAL:: FRAC
    LOGICAL,              INTENT(IN   ), OPTIONAL:: LEAP
    INTEGER:: IDAY
    REAL(REAL64):: FFRAC
    !
    IDAY = INT(DAY)
    FFRAC= DAY - DBLE(IDAY)
    !
    IF( PRESENT(FRAC) ) FFRAC = FFRAC + FRAC
    !
    CALL ADD_DAY_INT(DT, IDAY, FFRAC, LEAP)
    !
  END SUBROUTINE
  !
  ELEMENTAL PURE SUBROUTINE ADD_DAY_INT(DT, DAY, FRAC, LEAP)
    CLASS(DATE_OPERATOR), INTENT(INOUT):: DT
    INTEGER,              INTENT(IN   ), OPTIONAL:: DAY
    REAL(REAL64),         INTENT(IN   ), OPTIONAL:: FRAC
    LOGICAL,              INTENT(IN   ), OPTIONAL:: LEAP
    INTEGER:: DOY, YEAR
    !
    IF(PRESENT(DAY)) THEN
        DOY= DT%JDN + DAY
    ELSE
        DOY= DT%JDN
    END IF
    !
    YEAR = DT%YEAR
    !
    IF( PRESENT(FRAC) ) DT%FRAC = DT%FRAC + FRAC
    !
    IF ( DT%FRAC > UNO .OR. DT%FRAC < DZ ) THEN
                                                 DOY     = DOY  + INT(DT%FRAC)     
                                                 DT%FRAC = DT%FRAC - INT(DT%FRAC)
    END IF
    !
    IF ( DT%FRAC < DZ ) THEN
                                                 DOY     = DOY  - ONE    
                                                 DT%FRAC = DT%FRAC + UNO
    END IF
    !
    IF ( DT%FRAC == UNO ) THEN
                                                 DOY     = DOY  + ONE     
                                                 DT%FRAC = DZ
    END IF
    !
    CALL JULIANDAY_TO_DATE(DOY, YEAR, DT%DAY, DT%MONTH, DT%YEAR, DT%JDN, LEAP)
    !
    DT%DYEAR = JDN_TO_DYEAR(DT%JDN, DT%YEAR, DT%FRAC, LEAP) 
    !
    CALL DT%SET_DATE_STRING(DT%DAY, DT%MONTH, DT%YEAR)   !SET DT%DATE
    !
  END SUBROUTINE
  !
  ELEMENTAL PURE SUBROUTINE ADD_MONTH_INT(DT, MONTH, LEAP)
    CLASS(DATE_OPERATOR), INTENT(INOUT):: DT
    INTEGER,              INTENT(IN   ):: MONTH
    LOGICAL, OPTIONAL,    INTENT(IN   ):: LEAP
    LOGICAL:: ZERO_YEAR
    !
    ZERO_YEAR = DT%YEAR == Z
    !
    DT%MONTH = DT%MONTH + MONTH
    DO
      IF (DT%MONTH > 12) THEN
          DT%MONTH = DT%MONTH - 12
          DT%YEAR = DT%YEAR + ONE
      ELSEIF (DT%MONTH < ONE) THEN
          DT%MONTH = DT%MONTH + 12
          DT%YEAR = DT%YEAR - ONE
      ELSE
          EXIT
      END IF
    END DO
    !
    IF(ZERO_YEAR) THEN
                      IF(PRESENT(LEAP)) DT%YEAR = Z !PREVENT INCREMENT OF YEAR BECAUSE IT IS BEING TREATED AS A FRACTOIN OF YEAR
    END IF
    !
    DT%JDN= JULIANDAY(DT%DAY, DT%MONTH, DT%YEAR, LEAP) 
    !
    DT%DYEAR = JDN_TO_DYEAR(DT%JDN, DT%YEAR, DT%FRAC, LEAP) 
    !
    CALL DT%SET_DATE_STRING(DT%DAY, DT%MONTH, DT%YEAR)   !SET DT%DATE
    !
  END SUBROUTINE
  !
  ELEMENTAL PURE SUBROUTINE ADD_MONTH_DBL(DT, MONTH, LEAP)
    CLASS(DATE_OPERATOR), INTENT(INOUT):: DT
    REAL(REAL64),         INTENT(IN   ):: MONTH
    LOGICAL, OPTIONAL,    INTENT(IN   ):: LEAP
    INTEGER:: MON
    REAL(REAL64):: MFRAC
    LOGICAL:: ZERO_YEAR
    !
    ZERO_YEAR = DT%YEAR == Z
    !
    MON = INT(MONTH)
    MFRAC = MONTH - DBLE(MON)
    !
    DT%MONTH = DT%MONTH + MON
    DO
      IF (DT%MONTH > 12) THEN
          DT%MONTH = DT%MONTH - 12
          DT%YEAR = DT%YEAR + ONE
      ELSEIF (DT%MONTH < ONE) THEN
          DT%MONTH = DT%MONTH + 12
          DT%YEAR = DT%YEAR - ONE
      ELSE
          EXIT
      END IF
    END DO
    !
    IF(ABS(MFRAC) > YEARTOL) THEN !NOTE USING YEAR TOLERANCE RATHER THEN "YEARTOL*0.08" ~ YEARTOL/12 --MONTHS HAVE MORE ERRORS SO HAVE LOOSER TOLERANCE
                                  !
                                  MON = MONTHDAYS(DT%MONTH,DT%YEAR)
                                  MFRAC = MFRAC * DBLE(MON)
                                  CALL ADD_DAY_INT(DT, FRAC=MFRAC, LEAP = LEAP)
    END IF
    !
    IF(ZERO_YEAR) THEN
                      IF(PRESENT(LEAP)) DT%YEAR = Z !PREVENT INCREMENT OF YEAR BECAUSE IT IS BEING TREATED AS A FRACTOIN OF YEAR
    END IF
    !
    DT%JDN= JULIANDAY(DT%DAY, DT%MONTH, DT%YEAR, LEAP) 
    !
    DT%DYEAR = JDN_TO_DYEAR(DT%JDN, DT%YEAR, DT%FRAC, LEAP) 
    !
    CALL DT%SET_DATE_STRING(DT%DAY, DT%MONTH, DT%YEAR)   !SET DT%DATE
    !
  END SUBROUTINE
  !
  ELEMENTAL PURE SUBROUTINE ADD_YEAR_INT(DT, YEAR, LEAP)
    CLASS(DATE_OPERATOR), INTENT(INOUT):: DT
    INTEGER,              INTENT(IN   ):: YEAR
    LOGICAL, OPTIONAL,    INTENT(IN   ):: LEAP
    !
    IF(.NOT. PRESENT(LEAP)) THEN
         !
         DT%YEAR = DT%YEAR + YEAR
         !
         DT%JDN= JULIANDAY(DT%DAY, DT%MONTH, DT%YEAR) 
         !
         DT%DYEAR = JDN_TO_DYEAR(DT%JDN, DT%YEAR, DT%FRAC) 
         !
         CALL DT%SET_DATE_STRING(DT%DAY, DT%MONTH, DT%YEAR)   !SET DT%DATE
         !
    END IF
  END SUBROUTINE
  !
  ELEMENTAL PURE SUBROUTINE ADD_YEAR_DBL(DT, YEAR, LEAP)
    CLASS(DATE_OPERATOR), INTENT(INOUT):: DT
    REAL(REAL64),         INTENT(IN   ):: YEAR
    LOGICAL, OPTIONAL,    INTENT(IN   ):: LEAP
    REAL(REAL64):: YFRAC
    INTEGER:: YR
    !
    IF(.NOT. PRESENT(LEAP)) THEN
         !
         YR = INT(YEAR)
         YFRAC = YEAR - DBLE(YR)
         !
         IF(ABS(YFRAC) <= YEARTOL) THEN
                                       CALL ADD_YEAR_INT(DT, YR)
         ELSE
                                       DT%YEAR  = DT%YEAR + YR
                                       DT%JDN   = JULIANDAY(DT%DAY, DT%MONTH, DT%YEAR) 
                                       !
                                       IF(ISLEAPYEAR(DT%YEAR)) THEN
                                           YFRAC = YFRAC * 366.0_real64
                                       ELSE
                                           YFRAC = YFRAC * 365.0_real64
                                       END IF
                                       !
                                       CALL ADD_DAY_INT(DT, FRAC=YFRAC)
         END IF
         !
    END IF
  END SUBROUTINE
  !
  ELEMENTAL PURE SUBROUTINE ADD_SEC_DBLE(DT, SEC, LEAP)
    CLASS(DATE_OPERATOR), INTENT(INOUT):: DT
    REAL(REAL64),         INTENT(IN   ):: SEC
    LOGICAL,              INTENT(IN   ), OPTIONAL:: LEAP
    !
    DT%FRAC = DT%FRAC + SEC*SEC2DAY
    !
    IF ( DT%FRAC > UNO .OR. DT%FRAC < DZ ) THEN
         CALL ADD_DAY_INT(DT, LEAP=LEAP)
    ELSE
         DT%DYEAR = JDN_TO_DYEAR(DT%JDN, DT%YEAR, DT%FRAC, LEAP)
    END IF
    !
  END SUBROUTINE
  !
  ELEMENTAL PURE SUBROUTINE ADD_SEC_INT(DT, SEC, LEAP)
    CLASS(DATE_OPERATOR), INTENT(INOUT):: DT
    INTEGER,              INTENT(IN   ):: SEC
    LOGICAL,              INTENT(IN   ), OPTIONAL:: LEAP
    !
    DT%FRAC = DT%FRAC + DBLE(SEC)*SEC2DAY
    !
    IF ( DT%FRAC > UNO .OR. DT%FRAC < DZ ) THEN
         CALL ADD_DAY_INT(DT, LEAP=LEAP)
    ELSE
         DT%DYEAR = JDN_TO_DYEAR(DT%JDN, DT%YEAR, DT%FRAC, LEAP)
    END IF
    !
  END SUBROUTINE
  !
  ELEMENTAL PURE SUBROUTINE ADD_MIN_DBLE(DT, MIN, LEAP)
    CLASS(DATE_OPERATOR), INTENT(INOUT):: DT
    REAL(REAL64),         INTENT(IN   ):: MIN
    LOGICAL,              INTENT(IN   ), OPTIONAL:: LEAP
    !
    DT%FRAC = DT%FRAC + MIN*MIN2DAY
    !
    IF ( DT%FRAC > UNO .OR. DT%FRAC < DZ ) THEN
         CALL ADD_DAY_INT(DT, LEAP=LEAP)
    ELSE
         DT%DYEAR = JDN_TO_DYEAR(DT%JDN, DT%YEAR, DT%FRAC, LEAP)
    END IF
    !
  END SUBROUTINE
  !
  ELEMENTAL PURE SUBROUTINE ADD_MIN_INT(DT, MIN, LEAP)
    CLASS(DATE_OPERATOR), INTENT(INOUT):: DT
    INTEGER,              INTENT(IN   ):: MIN
    LOGICAL,              INTENT(IN   ), OPTIONAL:: LEAP
    !
    DT%FRAC = DT%FRAC + DBLE(MIN)*MIN2DAY
    !
    IF ( DT%FRAC > UNO .OR. DT%FRAC < DZ ) THEN
         CALL ADD_DAY_INT(DT, LEAP=LEAP)
    ELSE
         DT%DYEAR = JDN_TO_DYEAR(DT%JDN, DT%YEAR, DT%FRAC, LEAP)
    END IF
    !
  END SUBROUTINE
  !
  ELEMENTAL PURE SUBROUTINE ADD_HOUR_DBLE(DT, HOUR, LEAP)
    CLASS(DATE_OPERATOR), INTENT(INOUT):: DT
    REAL(REAL64),         INTENT(IN   ):: HOUR
    LOGICAL,              INTENT(IN   ), OPTIONAL:: LEAP
    !
    DT%FRAC = DT%FRAC + HOUR*HR2DAY
    !
    IF ( DT%FRAC > UNO .OR. DT%FRAC < DZ ) THEN
         CALL ADD_DAY_INT(DT, LEAP=LEAP)
    ELSE
         DT%DYEAR = JDN_TO_DYEAR(DT%JDN, DT%YEAR, DT%FRAC, LEAP)
    END IF
    !
  END SUBROUTINE
  !
  ELEMENTAL PURE SUBROUTINE ADD_HOUR_INT(DT, HOUR, LEAP)
    CLASS(DATE_OPERATOR), INTENT(INOUT):: DT
    INTEGER,              INTENT(IN   ):: HOUR
    LOGICAL,              INTENT(IN   ), OPTIONAL:: LEAP
    !
    DT%FRAC = DT%FRAC + DBLE(HOUR)*HR2DAY
    !
    IF ( DT%FRAC > UNO .OR. DT%FRAC < DZ ) THEN
         CALL ADD_DAY_INT(DT, LEAP=LEAP)
    ELSE
         DT%DYEAR = JDN_TO_DYEAR(DT%JDN, DT%YEAR, DT%FRAC, LEAP)
    END IF
    !
  END SUBROUTINE
  !
  ELEMENTAL PURE SUBROUTINE ZERO_YEAR(DT, LEAP)
    CLASS(DATE_OPERATOR), INTENT(INOUT):: DT
    LOGICAL, INTENT(IN), OPTIONAL:: LEAP
    !
    IF(DT%YEAR.NE.Z) THEN
       !
       DT%YEAR = Z
       !
       DT%JDN= JULIANDAY(DT%DAY, DT%MONTH, DT%YEAR, LEAP) 
       !
       DT%DYEAR = JDN_TO_DYEAR(DT%JDN, DT%YEAR, DT%FRAC, LEAP) 
       !
       CALL DT%SET_DATE_STRING(DT%DAY, DT%MONTH, DT%YEAR)   !SET DT%DATE
       !
       DT%DATE = 'ZERO_YEAR'
    END IF
    !
  END SUBROUTINE
  !
  ELEMENTAL PURE FUNCTION DATE_MD_COMPARE(DT, OP, DT2) RESULT(RES) !DT op DT2 where op = ">", ">=", "<", "<=", or  "=="
    CLASS(DATE_OPERATOR), INTENT(IN):: DT
    CHARACTER(*),         INTENT(IN):: OP
    TYPE(DATE_OPERATOR),  INTENT(IN):: DT2
    LOGICAL:: RES
    !
    SELECT CASE(OP)
    CASE('>' ); RES = DT%MONTH >  DT2%MONTH .AND. DT%DAY >  DT2%DAY
    CASE('>='); RES = DT%MONTH >= DT2%MONTH .AND. DT%DAY >= DT2%DAY
    CASE('<' ); RES = DT%MONTH <  DT2%MONTH .AND. DT%DAY <  DT2%DAY
    CASE('<='); RES = DT%MONTH <= DT2%MONTH .AND. DT%DAY <= DT2%DAY
    CASE('=='); RES = DT%MONTH == DT2%MONTH .AND. DT%DAY == DT2%DAY
    CASE DEFAULT
                RES = FALSE
    END SELECT
    !
  END FUNCTION
  !
  ELEMENTAL PURE FUNCTION DATE_CONTAINS_MD(DT, M, D, DT2, INCLUSIVE) RESULT(RES)
    CLASS(DATE_OPERATOR), INTENT(IN):: DT
    INTEGER,              INTENT(IN):: M,D
    TYPE(DATE_OPERATOR),  INTENT(IN):: DT2
    LOGICAL, OPTIONAL,    INTENT(IN):: INCLUSIVE
    LOGICAL:: RES
    !
    INTEGER:: JD
    LOGICAL:: INCL
    !
    INCL = FALSE
    IF(PRESENT(INCLUSIVE)) INCL = INCLUSIVE
    !
    IF(DT%DYEAR.NE.DT%DYEAR .OR. DT2%DYEAR.NE.DT2%DYEAR) THEN
        RES = FALSE
    ELSEIF(DT%DYEAR > DT2%DYEAR) THEN
        RES = FALSE
    ELSEIF(DT2%YEAR - DT%YEAR > ONE) THEN
        RES = TRUE
    ELSEIF(DT%YEAR == DT2%YEAR) THEN
        JD = JULIANDAY(D, M, DT%YEAR)
        IF(INCL) THEN
                     RES = DT%JDN <= JD .AND. JD <= DT2%JDN
        ELSE
                     RES = DT%JDN <= JD .AND. JD <  DT2%JDN
        END IF
    ELSE
        IF(INCL) THEN
                     RES = JULIANDAY(D, M, DT%YEAR) >= DT%JDN .OR. JULIANDAY(D, M, DT2%YEAR) <= DT2%JDN
        ELSE
                     RES = JULIANDAY(D, M, DT%YEAR) >= DT%JDN .OR. JULIANDAY(D, M, DT2%YEAR) <  DT2%JDN
        END IF
    END IF
    !
  END FUNCTION
  !
  ELEMENTAL PURE FUNCTION DATE_CONTAINS_DATE_MD(DT, MD, DT2, INCLUSIVE) RESULT(RES)
    CLASS(DATE_OPERATOR), INTENT(IN):: DT
    TYPE(DATE_OPERATOR),  INTENT(IN):: MD
    TYPE(DATE_OPERATOR),  INTENT(IN):: DT2
    LOGICAL, OPTIONAL,    INTENT(IN):: INCLUSIVE
    LOGICAL:: RES
    !
    RES = DATE_CONTAINS_MD(DT, MD%MONTH, MD%DAY, DT2, INCLUSIVE)
    !
  END FUNCTION
  !
  ELEMENTAL PURE FUNCTION DATE_DAY_COUNT_MD(DT, M, D, LOOK_FORWARD) RESULT(NDAY)
    CLASS(DATE_OPERATOR), INTENT(IN):: DT
    INTEGER,              INTENT(IN):: M,D
    LOGICAL, OPTIONAL,    INTENT(IN):: LOOK_FORWARD
    TYPE(DATE_OPERATOR):: DT2
    REAL(REAL64):: DAYDIFF
    INTEGER:: NDAY
    INTEGER:: YR
    LOGICAL:: LOOK_BACK
    !
    IF(PRESENT(LOOK_FORWARD)) THEN
                       LOOK_BACK = .NOT. LOOK_FORWARD
    ELSE
                       LOOK_BACK = TRUE
    END IF
    !
    IF(LOOK_BACK) THEN
        IF( M <= DT%MONTH .AND. D <= DT%DAY ) THEN  !Same YR
            YR = DT%YEAR
        ELSE
            YR = DT%YEAR - ONE
        END IF
    ELSE
        IF( M <= DT%MONTH .AND. D < DT%DAY ) THEN  !Same YR
            YR = DT%YEAR + ONE
        ELSE
            YR = DT%YEAR
        END IF
    END IF
    !
    CALL INITIALIZE_DATE_OPERATOR_DMY(DT2, D, M, YR)
    !
    IF(LOOK_BACK) THEN
                  DAYDIFF = DATE_OPERATOR_DAY_DIFFERENCE(DT, DT2)
    ELSE
                  DAYDIFF = DATE_OPERATOR_DAY_DIFFERENCE(DT2, DT)
    END IF
    !
    NDAY = INT(DAYDIFF)
    !
  END FUNCTION
  !
  ELEMENTAL PURE FUNCTION DATE_DAY_COUNT_DATE_MD(DT, MD, LOOK_FORWARD) RESULT(NDAY)
    CLASS(DATE_OPERATOR), INTENT(IN):: DT
    TYPE(DATE_OPERATOR),  INTENT(IN):: MD
    LOGICAL, OPTIONAL,    INTENT(IN):: LOOK_FORWARD
    INTEGER:: NDAY
    !
    NDAY = DATE_DAY_COUNT_MD(DT, DT%MONTH, MD%DAY, LOOK_FORWARD)
    !
  END FUNCTION
  !
  ELEMENTAL PURE FUNCTION DATE_DAY_COUNT_DATE_TO_MD(DT, MD) RESULT(NDAY)
    CLASS(DATE_OPERATOR), INTENT(IN):: DT
    TYPE(DATE_OPERATOR),  INTENT(IN):: MD
    INTEGER:: NDAY
    !
    NDAY = DATE_DAY_COUNT_MD(DT, DT%MONTH, MD%DAY, TRUE)
    !
  END FUNCTION
  !
  ELEMENTAL PURE FUNCTION DATE_DAY_COUNT_TO_MD(DT, M, D) RESULT(NDAY)
    CLASS(DATE_OPERATOR), INTENT(IN):: DT
    INTEGER,              INTENT(IN):: M, D
    INTEGER:: NDAY
    !
    NDAY = DATE_DAY_COUNT_MD(DT, M, D, TRUE)
    !
  END FUNCTION
  !
  ELEMENTAL PURE FUNCTION DATE_DAY_COUNT_DATE_FROM_MD(DT, MD) RESULT(NDAY)
    CLASS(DATE_OPERATOR), INTENT(IN):: DT
    TYPE(DATE_OPERATOR),  INTENT(IN):: MD
    INTEGER:: NDAY
    !
    NDAY = DATE_DAY_COUNT_MD(DT, DT%MONTH, MD%DAY, FALSE)
    !
  END FUNCTION
  !
  ELEMENTAL PURE FUNCTION DATE_DAY_COUNT_FROM_MD(DT, M, D) RESULT(NDAY)
    CLASS(DATE_OPERATOR), INTENT(IN):: DT
    INTEGER,              INTENT(IN):: M, D
    INTEGER:: NDAY
    !
    NDAY = DATE_DAY_COUNT_MD(DT, M, D, FALSE)
    !
  END FUNCTION
  !
  ELEMENTAL PURE FUNCTION DATE_YEAR_DAY_COUNT(DT, YEAR, LEAP) RESULT(NDAY)
    CLASS(DATE_OPERATOR), INTENT(IN):: DT
    INTEGER, OPTIONAL,    INTENT(IN):: YEAR
    LOGICAL, OPTIONAL,    INTENT(IN):: LEAP
    INTEGER:: NDAY
    !
    IF   (PRESENT(YEAR))     THEN
                                 NDAY = YEAR_DAY_COUNT(YEAR, LEAP)
    ELSEIF(DT%YEAR.NE.NINER) THEN
                                 NDAY = YEAR_DAY_COUNT(DT%YEAR, LEAP)
    ELSE
                                 NDAY = 365
    END IF
    !
  END FUNCTION
  !
  ELEMENTAL PURE FUNCTION DATE_YEAR_DAY_COUNT_DBLE(DT, YEAR, LEAP) RESULT(NDAY)
    CLASS(DATE_OPERATOR), INTENT(IN):: DT
    INTEGER, OPTIONAL,    INTENT(IN):: YEAR
    LOGICAL, OPTIONAL,    INTENT(IN):: LEAP
    REAL(REAL64):: NDAY
    !
    IF   (PRESENT(YEAR))     THEN
                                 NDAY = YEAR_DAY_COUNT_DBLE(YEAR, LEAP)
    ELSEIF(DT%YEAR.NE.NINER) THEN
                                 NDAY = YEAR_DAY_COUNT_DBLE(DT%YEAR, LEAP)
    ELSE
                                 NDAY = 365.0_real64
    END IF
    !
  END FUNCTION
  !
  ELEMENTAL PURE FUNCTION DATE_YEAR_FRACTION(DT, LEAP) RESULT(YFRAC)
    CLASS(DATE_OPERATOR), INTENT(IN):: DT
    LOGICAL,    OPTIONAL, INTENT(IN):: LEAP
    REAL(REAL64):: YFRAC
    !
    YFRAC = YEAR_FRACTION_DMY(DT%DAY, DT%MONTH, DT%YEAR, DT%FRAC, LEAP)
    !
  END FUNCTION
  !
  ELEMENTAL PURE FUNCTION DATE_DYEAR_FRACTION(DT) RESULT(YFRAC)
    CLASS(DATE_OPERATOR), INTENT(IN):: DT
    REAL(REAL64):: YFRAC
    !
    IF(DT%DYEAR==DT%DYEAR) THEN
        YFRAC = YEAR_FRACTION_DYEAR(DT%DYEAR)
    ELSE
        YFRAC = DT%DYEAR
    END IF
    !
  END FUNCTION
  !
  ELEMENTAL PURE FUNCTION DATE_DYEAR_IS_YEAR(DT) RESULT(DYEAR_IS_YEAR)
    CLASS(DATE_OPERATOR), INTENT(IN):: DT
    LOGICAL:: DYEAR_IS_YEAR
    !
    IF(DT%DYEAR==DT%DYEAR) THEN
        DYEAR_IS_YEAR = DT%DYEAR - DBLE(DT%YEAR) < YEARTOL
    ELSE
        DYEAR_IS_YEAR = FALSE
    END IF
    
    !
  END FUNCTION
  !
  ELEMENTAL PURE SUBROUTINE SET_YEAR(DT, YEAR, LEAP)
    CLASS(DATE_OPERATOR), INTENT(INOUT):: DT
    INTEGER, INTENT(IN):: YEAR
    LOGICAL, INTENT(IN), OPTIONAL:: LEAP
    !
    DT%YEAR = YEAR
    !
    DT%JDN= JULIANDAY(DT%DAY, DT%MONTH, DT%YEAR, LEAP) 
    !
    DT%DYEAR = JDN_TO_DYEAR(DT%JDN, DT%YEAR, DT%FRAC, LEAP) 
    !
    CALL DT%SET_DATE_STRING(DT%DAY, DT%MONTH, DT%YEAR)   !SET DT%DATE
    !
  END SUBROUTINE
  !
  ELEMENTAL PURE SUBROUTINE SET_MONTH(DT, MONTH, LEAP)
    CLASS(DATE_OPERATOR), INTENT(INOUT):: DT
    LOGICAL, OPTIONAL,    INTENT(IN   ):: LEAP
    INTEGER, INTENT(IN):: MONTH
    !
    DT%MONTH = MONTH
    !
    DT%JDN= JULIANDAY(DT%DAY, DT%MONTH, DT%YEAR, LEAP) 
    !
    DT%DYEAR = JDN_TO_DYEAR(DT%JDN, DT%YEAR, DT%FRAC, LEAP) 
    !
    CALL DT%SET_DATE_STRING(DT%DAY, DT%MONTH, DT%YEAR)   !SET DT%DATE
    !
  END SUBROUTINE
  !
  ELEMENTAL PURE SUBROUTINE SET_DAY(DT, DAY, LEAP)
    CLASS(DATE_OPERATOR), INTENT(INOUT):: DT
    LOGICAL, OPTIONAL,    INTENT(IN   ):: LEAP
    INTEGER, INTENT(IN):: DAY
    !
    DT%DAY = DAY
    !
    DT%JDN= JULIANDAY(DT%DAY, DT%MONTH, DT%YEAR, LEAP) 
    !
    DT%DYEAR = JDN_TO_DYEAR(DT%JDN, DT%YEAR, DT%FRAC, LEAP) 
    !
    CALL DT%SET_DATE_STRING(DT%DAY, DT%MONTH, DT%YEAR)   !SET DT%DATE
    !
  END SUBROUTINE
  !
  ELEMENTAL PURE SUBROUTINE COPY_DATE_OPERATOR(DT_OUT,DT_IN)
    CLASS(DATE_OPERATOR), INTENT(IN   ):: DT_IN
    CLASS(DATE_OPERATOR), INTENT(INOUT):: DT_OUT
    !
    DT_OUT%DATE  = DT_IN%DATE
    DT_OUT%DAY   = DT_IN%DAY
    DT_OUT%MONTH = DT_IN%MONTH
    DT_OUT%YEAR  = DT_IN%YEAR
    DT_OUT%JDN   = DT_IN%JDN
    DT_OUT%FRAC  = DT_IN%FRAC
    DT_OUT%DYEAR = DT_IN%DYEAR
    !
  END SUBROUTINE
  !
  ELEMENTAL PURE SUBROUTINE STR_TO_DATE_OPERATOR(DT,STR)
    CHARACTER(*),         INTENT(IN   ):: STR
    CLASS(DATE_OPERATOR), INTENT(INOUT):: DT
    !
    CALL INITIALIZE_DATE_OPERATOR_STR(DT, STR)
    !
  END SUBROUTINE
  !
  ELEMENTAL PURE FUNCTION DATE_OPERATOR_ADD_DBLE(DT_IN, DAY) RESULT(DT)
    CLASS(DATE_OPERATOR), INTENT(IN):: DT_IN
    REAL(REAL64),         INTENT(IN):: DAY
    TYPE(DATE_OPERATOR)             :: DT
    REAL(REAL64):: FRAC
    INTEGER:: IDAY
    !
    IDAY = INT(DAY)
    FRAC = DAY - DBLE(IDAY)
    !
    DT%JDN = DT_IN%JDN
    DT%YEAR= DT_IN%YEAR
    DT%FRAC= DT_IN%FRAC
    !
    CALL ADD_DAY_INT(DT,IDAY,FRAC)
    !
  END FUNCTION
  !
  ELEMENTAL PURE FUNCTION DATE_OPERATOR_ADD_DBLE_REV(DAY,DT_IN) RESULT(DT)
    CLASS(DATE_OPERATOR), INTENT(IN):: DT_IN
    REAL(REAL64),         INTENT(IN):: DAY
    TYPE(DATE_OPERATOR)             :: DT
    !
    DT = DATE_OPERATOR_ADD_DBLE(DT_IN, DAY)
    !
  END FUNCTION
  !
  ELEMENTAL PURE FUNCTION DATE_OPERATOR_ADD_SNGL(DT_IN, DAY) RESULT(DT)
    CLASS(DATE_OPERATOR), INTENT(IN):: DT_IN
    REAL(REAL32),         INTENT(IN):: DAY
    TYPE(DATE_OPERATOR)             :: DT
    REAL(REAL64):: FRAC
    INTEGER:: IDAY
    !
    IDAY = INT(DAY)
    FRAC = DBLE(DAY - REAL(IDAY,REAL32))
    FRAC = DBLE(INT(FRAC*1D7))/1D7                !ENSURE TRAILING ZEROS ARE ZERO
    !
    DT%JDN = DT_IN%JDN
    DT%YEAR= DT_IN%YEAR
    DT%FRAC= DT_IN%FRAC
    !
    CALL ADD_DAY_INT(DT,IDAY,FRAC)
    !
  END FUNCTION
  !
  ELEMENTAL PURE FUNCTION DATE_OPERATOR_ADD_SNGL_REV(DAY, DT_IN) RESULT(DT)
    CLASS(DATE_OPERATOR), INTENT(IN):: DT_IN
    REAL(REAL32),         INTENT(IN):: DAY
    TYPE(DATE_OPERATOR)             :: DT
    REAL(REAL64):: FRAC
    !
    DT = DATE_OPERATOR_ADD_SNGL(DT_IN, DAY)
    !
  END FUNCTION
  !
  ELEMENTAL PURE FUNCTION DATE_OPERATOR_ADD_INT(DT_IN, DAY) RESULT(DT)
    CLASS(DATE_OPERATOR), INTENT(IN):: DT_IN
    INTEGER,              INTENT(IN):: DAY
    TYPE(DATE_OPERATOR)             :: DT
    !
    DT%JDN = DT_IN%JDN
    DT%YEAR= DT_IN%YEAR
    DT%FRAC= DT_IN%FRAC
    !
    CALL ADD_DAY_INT(DT,DAY)
    !
  END FUNCTION
  !
  ELEMENTAL PURE FUNCTION DATE_OPERATOR_ADD_INT_REV(DAY, DT_IN) RESULT(DT)
    CLASS(DATE_OPERATOR), INTENT(IN):: DT_IN
    INTEGER,              INTENT(IN):: DAY
    TYPE(DATE_OPERATOR)             :: DT
    !
    DT = DATE_OPERATOR_ADD_INT(DT_IN, DAY)
    !
  END FUNCTION
  !
  ELEMENTAL PURE FUNCTION DATE_OPERATOR_SUB_DBLE(DT_IN, DAY) RESULT(DT)  !DAY INCOMING IS POSITIVE
    CLASS(DATE_OPERATOR), INTENT(IN):: DT_IN
    REAL(REAL64),         INTENT(IN):: DAY
    TYPE(DATE_OPERATOR)             :: DT
    REAL(REAL64):: FRAC
    INTEGER:: IDAY
    !
    IDAY =  NEG * INT(DAY)
    FRAC = DNEG * ( DAY + DBLE(IDAY) )  ! DAY + (-INT(DAY))  
    !
    DT%JDN = DT_IN%JDN
    DT%YEAR= DT_IN%YEAR
    DT%FRAC= DT_IN%FRAC
    !
    CALL ADD_DAY_INT(DT,IDAY,FRAC)
    !
  END FUNCTION
  !
  ELEMENTAL PURE FUNCTION DATE_OPERATOR_SUB_SNGL(DT_IN, DAY) RESULT(DT)
    CLASS(DATE_OPERATOR), INTENT(IN):: DT_IN
    REAL(REAL32),         INTENT(IN):: DAY
    TYPE(DATE_OPERATOR)             :: DT
    REAL(REAL64):: FRAC
    INTEGER:: IDAY
    !
    IDAY = NEG  * INT(DAY)
    FRAC = DBLE( -1. * ( DAY + REAL(IDAY,REAL32) ) )
    FRAC = DBLE(INT(FRAC*1D7))/1D7                !ENSURE TRAILING ZEROS ARE ZERO
    !
    DT%JDN = DT_IN%JDN
    DT%YEAR= DT_IN%YEAR
    DT%FRAC= DT_IN%FRAC
    !
    CALL ADD_DAY_INT(DT,IDAY,FRAC)
    !
  END FUNCTION
  !
  ELEMENTAL PURE FUNCTION DATE_OPERATOR_SUB_INT(DT_IN, DAY) RESULT(DT)
    CLASS(DATE_OPERATOR), INTENT(IN):: DT_IN
    INTEGER,              INTENT(IN):: DAY
    TYPE(DATE_OPERATOR)             :: DT
    INTEGER:: DAY_SUB
    !
    DAY_SUB = NEG * DAY
    !
    DT%JDN = DT_IN%JDN
    DT%YEAR= DT_IN%YEAR
    DT%FRAC= DT_IN%FRAC
    !
    CALL ADD_DAY_INT(DT,DAY_SUB)
    !
  END FUNCTION
  !
  ELEMENTAL PURE FUNCTION DATE_OPERATOR_SUB(DT1, DT2) RESULT(DAYDIFF)
    CLASS(DATE_OPERATOR), INTENT(IN):: DT1, DT2
    REAL(REAL64):: DAYDIFF
    !
    DAYDIFF = DATE_OPERATOR_DAY_DIFFERENCE(DT1, DT2)
    !
  END FUNCTION
  !
  ELEMENTAL PURE FUNCTION DATE_OPERATOR_DAY_DIFFERENCE(DT, DT2) RESULT(DIFF)
    CLASS(DATE_OPERATOR), INTENT(IN):: DT, DT2
    REAL(REAL64):: DIFF
    INTEGER:: YEAR
    !
    DIFF = DBLE(DT%JDN - DT2%JDN) + DT%FRAC - DT2%FRAC
    !
    IF (DT%YEAR < DT2%YEAR) THEN
        !DIFF= DBLE(DT%JDN - DT2%JDN) + DT%FRAC - DT2%FRAC
        YEAR = DT2%YEAR - ONE
        DO WHILE (YEAR >= DT%YEAR)
            IF ( ISLEAPYEAR(YEAR) ) THEN
                DIFF=DIFF-366.0_real64
            ELSE
                DIFF=DIFF-365.0_real64
            END IF
            YEAR = YEAR - ONE
        END DO
    ELSEIF (DT%YEAR > DT2%YEAR) THEN
        !DIFF= -1.D0*DBLE(DT2%JDN) - DT2%FRAC
        YEAR = DT2%YEAR 
        DO WHILE (YEAR <  DT%YEAR)
            IF ( ISLEAPYEAR(YEAR) ) THEN
                DIFF=DIFF+366.0_real64
            ELSE
                DIFF=DIFF+365.0_real64
            END IF
            YEAR = YEAR + ONE
        END DO
    END IF
    !
  END FUNCTION
  !
  ELEMENTAL PURE FUNCTION DATE_OPERATOR_INTERPOLATE_DBLE(DT,DT1,DT2,DAT1,DAT2) RESULT (RES)
  CLASS(DATE_OPERATOR), INTENT(IN):: DT
  CLASS(DATE_OPERATOR), INTENT(IN):: DT1,  DT2
  REAL(REAL64),         INTENT(IN):: DAT1, DAT2
  REAL(REAL64):: RES
  
  IF (ABS(DT2%DYEAR - DT1%DYEAR) < YEARTOL) THEN
      RES = DAT1
  ELSE
      !RES = (DT%DYEAR - DT1%DYEAR)/(DT2%DYEAR - DT1%DYEAR)
      !RES = DAT1 + (DAT2-DAT1)*RES
      RES = DAT1 + (DAT2-DAT1)*( DT%DIFF(DT1) / DT2%DIFF(DT1) )
  END IF
  !  
  END FUNCTION
  !
  ELEMENTAL PURE FUNCTION DATE_OPERATOR_INTERPOLATE_SNGL(DT,DT1,DT2,DAT1,DAT2) RESULT (RES)
  CLASS(DATE_OPERATOR), INTENT(IN):: DT
  CLASS(DATE_OPERATOR), INTENT(IN):: DT1,  DT2
  REAL(REAL32),         INTENT(IN):: DAT1, DAT2
  REAL(REAL32):: RES
  
  IF (ABS(DT2%DYEAR - DT1%DYEAR) < YEARTOL) THEN
      RES = DAT1
  ELSE
      !RES = (DT%DYEAR - DT1%DYEAR)/(DT2%DYEAR - DT1%DYEAR)
      !RES = DAT1 + (DAT2-DAT1)*RES
      RES = DAT1 + (DAT2-DAT1)*REAL( DT%DIFF(DT1) / DT2%DIFF(DT1) , REAL32)
  END IF
  !  
  END FUNCTION
  !
  ELEMENTAL PURE FUNCTION DATE_OPERATOR_EQUALITY(DT1, DT2) RESULT(EQ)
    CLASS(DATE_OPERATOR), INTENT(IN):: DT1, DT2
    LOGICAL:: EQ
    !
    EQ = ABS(DT1%DYEAR-DT2%DYEAR) < YEARTOL   !within 1.1 second of each other   --- EQ = ( DT1%YEAR == DT2%YEAR ) .AND. ( DT1%JDN == DT2%JDN ) .AND. ( ABS(DT1%FRAC - DT2%FRAC) < DAYTOL )
    !
    END FUNCTION
    !
  ELEMENTAL PURE FUNCTION DATE_OPERATOR_LESS_THAN(DT1, DT2) RESULT(LT)
    CLASS(DATE_OPERATOR), INTENT(IN):: DT1, DT2
    LOGICAL:: LT
    !
    LT = DT1%DYEAR < DT2%DYEAR .AND. ABS(DT1%DYEAR-DT2%DYEAR) > YEARTOL
    !
  END FUNCTION
  !
  ELEMENTAL PURE FUNCTION DATE_OPERATOR_LESS_THAN_EQUAL(DT1, DT2) RESULT(LE)
    CLASS(DATE_OPERATOR), INTENT(IN):: DT1, DT2
    LOGICAL:: LE
    !
    LE = DT1%DYEAR <= DT2%DYEAR .OR. ABS(DT1%DYEAR-DT2%DYEAR) < YEARTOL   !CHECK IF WITHIN 1.1 second of each other
    !
  END FUNCTION
  !
  ELEMENTAL PURE FUNCTION DATE_OPERATOR_GREATER_THAN(DT1, DT2) RESULT(GT)
    CLASS(DATE_OPERATOR), INTENT(IN):: DT1, DT2
    LOGICAL:: GT
    !
    GT = DT1%DYEAR > DT2%DYEAR .AND. ABS(DT1%DYEAR-DT2%DYEAR) > YEARTOL
    !
  END FUNCTION
  !
  ELEMENTAL PURE FUNCTION DATE_OPERATOR_GREATER_THAN_EQUAL(DT1, DT2) RESULT(GE)
    CLASS(DATE_OPERATOR), INTENT(IN):: DT1, DT2
    LOGICAL:: GE
    !
    GE = DT1%DYEAR >= DT2%DYEAR .OR. ABS(DT1%DYEAR-DT2%DYEAR) < YEARTOL   !CHECK IF WITHIN 1.1 second of each other
    !
  END FUNCTION
  !
  ELEMENTAL PURE FUNCTION DATE_OPERATOR_FRAC_TO_TIME(DT) RESULT(TIME)
    CLASS(DATE_OPERATOR), INTENT(IN):: DT
    CHARACTER(8):: TIME
    INTEGER:: HOUR, MIN, SEC
    REAL(REAL64):: TEMP
    !
    IF(ABS(DT%FRAC-UNO) <= HALFSEC ) THEN
        TIME = "24:00:00"
    ELSEIF(ABS(DT%FRAC) <= HALFSEC ) THEN
        TIME = "00:00:00"
    ELSEIF(ABS(DT%FRAC-0.5D0) <= HALFSEC ) THEN
        TIME = "12:00:00"
    ELSE
        HOUR= INT( 24.0_real64 * DT%FRAC)
        !
        TEMP = (24.0_real64 * DT%FRAC) - DBLE(HOUR)
        MIN =INT( 60.0_real64*TEMP )
        !
        TEMP = 60.0_real64*TEMP - DBLE(MIN)
        SEC = INT( 60.0_real64*TEMP )
        IF(SEC < Z) SEC = Z
        !
        IF( SEC==60) THEN
            SEC = 0
            MIN = MIN + ONE
        END IF
        !
        IF( MIN==60) THEN
            MIN = 0
            HOUR = HOUR + ONE
        END IF
        !
        WRITE(TIME,'(I2.2, A ,I2.2, A ,I2.2)') HOUR, COL, MIN, COL, SEC
    END IF
    !
  END FUNCTION
  !
  ELEMENTAL PURE FUNCTION DATE_OPERATOR_TIME_TO_FRAC(TIME) RESULT(FRAC)
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
    IF(C > B + TWO) C = B + TWO
    !
    READ(TIME(I:A-1), *, IOSTAT=IERR) HOUR
    !
    IF(IERR == Z) READ(TIME(A+1:B-1),*, IOSTAT=IERR) MIN
    !
    IF(IERR == Z) READ(TIME(B+1:C)  ,*, IOSTAT=IERR) SEC
    !
    IF(IERR.NE.Z) THEN
        FRAC = IEEE_VALUE(FRAC, IEEE_QUIET_NAN)
    ELSE
        FRAC = DBLE(HOUR)*HR2DAY + DBLE(MIN)*MIN2DAY + DBLE(SEC)*SEC2DAY
    END IF
    !
  END FUNCTION
  !
  ELEMENTAL PURE FUNCTION ONLY_DYEAR(DT) 
    CLASS(DATE_OPERATOR), INTENT(IN):: DT
    LOGICAL:: ONLY_DYEAR
    !
    ONLY_DYEAR = DT%DATE == 'DYEAR'
    !
  END FUNCTION
  !
  ELEMENTAL PURE FUNCTION DATE_HAS_BEEN_SET(DT) RESULT(DATE_SET)
    CLASS(DATE_OPERATOR), INTENT(IN):: DT
    LOGICAL:: DATE_SET
    !
    DATE_SET = DT%DAY .NE. NINER .AND. DT%DATE .NE. NO_DATE
    !
  END FUNCTION
  !
  ELEMENTAL PURE FUNCTION DATE_HAS_NOT_BEEN_SET(DT) RESULT(DATE_NOT_SET)
    CLASS(DATE_OPERATOR), INTENT(IN):: DT
    LOGICAL:: DATE_NOT_SET
    !
    DATE_NOT_SET = DT%DAY == NINER .OR. DT%DATE == NO_DATE
    !
  END FUNCTION
  !
  ELEMENTAL PURE FUNCTION DATE_HAS_ZERO_YEAR(DT) RESULT(ZERO_YEAR)
    CLASS(DATE_OPERATOR), INTENT(IN):: DT
    LOGICAL:: ZERO_YEAR
    !
    ZERO_YEAR = DT%YEAR == Z
    !
  END FUNCTION
  !  
  !FUNCTION DATE_OPERATOR_STRING_REPRESENTATION(DT,SEP) RESULT(DATETIME)
  !  CLASS(DATE_OPERATOR), INTENT(IN):: DT
  !  CHARACTER, OPTIONAL,  INTENT(IN):: SEP
  !  CHARACTER(19):: DATETIME
  !  !
  !  IF(PRESENT(SEP)) THEN
  !      DATETIME=DT%DATE//SEP //DT%TIME()
  !  ELSE
  !      DATETIME=DT%DATE//BLNK//DT%TIME()
  !  END IF
  !  !
  !END FUNCTION
  !
  PURE FUNCTION DATE_OPERATOR_STRING_REPRESENTATION(DT, SEP, ISO) RESULT (DATE)
    CLASS(DATE_OPERATOR),   INTENT(IN):: DT
    CHARACTER(*), OPTIONAL, INTENT(IN):: SEP  !IF PROVIDED THEN THE CLOCK IS INCLUDED
    LOGICAL,      OPTIONAL, INTENT(IN):: ISO
    CHARACTER(:), ALLOCATABLE:: DATE
    !
    IF(PRESENT(SEP)) THEN
                         ALLOCATE(CHARACTER(18+LEN(SEP)):: DATE)  !mm/dd/yyyyThh:mm:ss
                         DATE(11:)=SEP//DATE_OPERATOR_FRAC_TO_TIME(DT)
    ELSE
                         ALLOCATE(CHARACTER(10):: DATE)  !mm/dd/yyyy
    END IF
    !
    IF(PRESENT(ISO)) THEN
         IF(ISO) THEN
                     WRITE(DATE(:10),'(I4.4, A ,I2.2, A ,I2.2)') DT%YEAR, MINUS, DT%MONTH, MINUS, DT%DAY  !ISO STANDARD
         ELSE
                     WRITE(DATE(:10),'(I2.2, A ,I2.2, A ,I4.4)') DT%MONTH, SLASH, DT%DAY, SLASH, DT%YEAR  !USA Standard
         END IF
    ELSE
                     WRITE(DATE(:10),'(I4.4, A ,I2.2, A ,I2.2)') DT%YEAR, MINUS, DT%MONTH, MINUS, DT%DAY  !ISO STANDARD
    END IF
    !
  END FUNCTION
  !
  PURE FUNCTION DATE_OPERATOR_PRINT_MONTHYEAR(DT, FULLNAME) RESULT (MONTHYEAR)
    CLASS(DATE_OPERATOR), INTENT(IN):: DT
    LOGICAL,  OPTIONAL,   INTENT(IN):: FULLNAME
    CHARACTER(:), ALLOCATABLE:: MONTHYEAR
    CHARACTER(4):: YEAR
    !
    WRITE(YEAR,'(I4.4)') DT%YEAR
    !
    MONTHYEAR = MONTHNAME(DT%MONTH,FULLNAME)//'-'//YEAR
    !
  END FUNCTION
  !
  PURE FUNCTION DATE_OPERATOR_PRINT_YEAR_MONTH(DT,SEP) RESULT (MONTHYEAR)
    CLASS(DATE_OPERATOR), INTENT(IN):: DT
    CHARACTER(1), OPTIONAL, INTENT(IN):: SEP  !IF PROVIDED THEN THE CLOCK IS INCLUDED
    CHARACTER(:),ALLOCATABLE:: MONTHYEAR
    INTEGER:: N
    !
    IF(PRESENT(SEP)) THEN
        N = 6 + LEN(SEP)
        ALLOCATE(CHARACTER(N)::MONTHYEAR)
        WRITE(MONTHYEAR,'(I4.4,A,I2.2)') DT%YEAR,SEP,DT%MONTH
    ELSE
        ALLOCATE(CHARACTER(7)::MONTHYEAR)
        WRITE(MONTHYEAR,'(I4.4,A1,I2.2)') DT%YEAR,'_',DT%MONTH
    END IF
    !
  END FUNCTION
  !
  PURE FUNCTION DATE_OPERATOR_PRINT_DYEAR(DT, NDEC) RESULT (DYEAR)
    CLASS(DATE_OPERATOR), INTENT(IN):: DT
    INTEGER,  OPTIONAL,   INTENT(IN):: NDEC
    CHARACTER(:),ALLOCATABLE:: DYEAR
    CHARACTER(10):: FMT !(F10.10)
    INTEGER:: DIM
    !
    IF(PRESENT(NDEC)) THEN
        !
        DIM = NDEC
        IF(DIM>9) DIM = 9
        !
        ALLOCATE(CHARACTER(DIM+5):: DYEAR)
        IF(DIM+5 > 10) THEN
             WRITE(FMT,'("(F",I2,".",I1,")")') DIM+5,DIM
        ELSE
             WRITE(FMT,'("(F",I1,".",I1,")")') DIM+5,DIM
        END IF
        WRITE(DYEAR, FMT) DT%DYEAR
    ELSE
        ALLOCATE(CHARACTER(9):: DYEAR) 
        WRITE(DYEAR,'(F9.4)') DT%DYEAR
    END IF
    !
  END FUNCTION
  !
  PURE FUNCTION DATE_OPERATOR_PRINT_DIF(DT, DT2, UNIT) RESULT (PPRINT)
    CLASS(DATE_OPERATOR), INTENT(IN):: DT, DT2
    INTEGER, OPTIONAL,    INTENT(IN):: UNIT  != 1 => sec
    CHARACTER(:), ALLOCATABLE:: PPRINT
    !
    ! UNIT DEFINES OUTPUT STRUCTURE
    ! UNIT =
    !       0 => units as needed
    !       1 => seconds
    !       2 => minutes
    !       3 => hours
    !       4 => days
    !       5 => largest whole unit
    !
    CHARACTER(15):: NUM 
    INTEGER:: IPRT
    REAL(REAL64):: DIFF
    !
    DIFF = DATE_OPERATOR_DAY_DIFFERENCE(DT, DT2)
    !
    IF(DIFF < DZ) DIFF = DNEG*DIFF
    !
    IPRT = Z
    IF(PRESENT(UNIT)) IPRT = UNIT
    !
    IF(IPRT == 5) THEN            !Find the largest unit 
        IF    (DIFF > UNO) THEN
                                   IPRT = 4
        ELSEIF(DIFF > HR2DAY ) THEN        !HR2DAY = 1/24
                                   IPRT = 3
        ELSEIF(DIFF > MIN2DAY) THEN        !MIN2DAY = 1/1440
                                   IPRT = 2
        ELSE
                                   IPRT = 1
        END IF
    END IF
    !
    IF    (IPRT  < ONE .OR. 5 < IPRT) THEN  !Build using all time values. Note IPRT is now TMP variable
                    !
                    IF(DIFF > UNO) THEN
                            !
                            IPRT    = INT(DIFF)
                            DIFF = DIFF - DBLE(IPRT)
                            !
                            WRITE(NUM,'(I14)') IPRT
                            NUM = ADJUSTL(NUM)
                            !
                            IF(IPRT==ONE) THEN
                                PPRINT = TRIM(NUM)//' day, '
                            ELSE
                                PPRINT = TRIM(NUM)//' days, '
                            END IF
                    ELSE
                            PPRINT = ''
                            NUM = BLNK  !Flag to indicate that number has not been written
                    END IF
                    !
                    DIFF = DIFF * DAY2HR  !Now in Hours
                    !
                    IF(DIFF > UNO) THEN
                            !
                            IPRT    = INT(DIFF)
                            DIFF = DIFF - DBLE(IPRT)
                            !
                            WRITE(NUM,'(I14)') IPRT
                            NUM = ADJUSTL(NUM)
                            !
                            IF(IPRT==ONE) THEN
                                PPRINT = PPRINT//TRIM(NUM)//' hour, '
                            ELSE
                                PPRINT = PPRINT//TRIM(NUM)//' hours, '
                            END IF
                            !
                    ELSEIF(NUM .NE. BLNK) THEN
                            PPRINT = PPRINT//TRIM('0')//' hours, '
                    END IF
                    !
                    DIFF = DIFF * 60.0_real64  !Now in minutes
                    !
                    IF(DIFF > UNO) THEN
                            !
                            IPRT    = INT(DIFF)
                            DIFF = DIFF - DBLE(IPRT)
                            !
                            WRITE(NUM,'(I14)') IPRT
                            NUM = ADJUSTL(NUM)
                            !
                            IF(IPRT==ONE) THEN
                                PPRINT = PPRINT//TRIM(NUM)//' minute, '
                            ELSE
                                PPRINT = PPRINT//TRIM(NUM)//' minutes, '
                            END IF
                            !
                    ELSEIF(NUM .NE. BLNK) THEN
                            PPRINT = PPRINT//TRIM('0')//' minutes, '
                    END IF
                    !
                    DIFF = DIFF * 60.0_real64  !Now in seconds
                    !
                    IF(DIFF > UNO .AND. NUM .NE. BLNK ) THEN !>1sec and have written a previous time unit
                            !
                            IPRT    = NINT(DIFF)
                            !
                            WRITE(NUM,'(I14)') IPRT
                            NUM = ADJUSTL(NUM)
                            !
                            IF(IPRT==ONE) THEN
                                PPRINT = PPRINT//TRIM(NUM)//' second'
                            ELSE
                                PPRINT = PPRINT//TRIM(NUM)//' seconds'
                            END IF
                            !
                    ELSEIF(DIFF > 0.0005D0) THEN  !No previous time units written and >1ms
                            !
                            WRITE(NUM,'(F14.3)') DIFF
                            NUM = ADJUSTL(NUM)
                            !
                            PPRINT = PPRINT//TRIM(NUM)//' seconds'
                    ELSE
                            PPRINT = PPRINT//TRIM('0')//' seconds'
                    END IF
                    !
    ELSEIF(IPRT == ONE) THEN
                            DIFF = DIFF*DAY2SEC
                            !
                            IF(DIFF > 10000.0_real64) THEN
                                  WRITE(NUM,'(F15.0)') DIFF
                            ELSE
                                  WRITE(NUM,'(F15.3)') DIFF
                            END IF
                            NUM = ADJUSTL(NUM)
                            !
                            PPRINT = TRIM(NUM)//' seconds'
    ELSEIF(IPRT == TWO) THEN
                            DIFF = DIFF*DAY2MIN
                            !
                            IF(DIFF > 10000.0_real64) THEN
                                  WRITE(NUM,'(F15.0)') DIFF
                            ELSE
                                  WRITE(NUM,'(F15.3)') DIFF
                            END IF
                            WRITE(NUM,'(F15.3)') DIFF
                            NUM = ADJUSTL(NUM)
                            !
                            PPRINT = TRIM(NUM)//' minutes'
    ELSEIF(IPRT ==   3) THEN
                            DIFF = DIFF*DAY2HR
                            !
                            !
                            IF(DIFF > 10000.0_real64) THEN
                                  WRITE(NUM,'(F15.1)') DIFF
                            ELSE
                                  WRITE(NUM,'(F15.5)') DIFF
                            END IF
                            !
                            NUM = ADJUSTL(NUM)
                            PPRINT = TRIM(NUM)//' hours'
    ELSEIF(IPRT ==   4) THEN
                            !
                            IF(DIFF > 10000.0_real64) THEN
                                  WRITE(NUM,'(F15.2)') DIFF
                            ELSE
                                  WRITE(NUM,'(F15.5)') DIFF
                            END IF
                            NUM = ADJUSTL(NUM)
                            PPRINT = TRIM(NUM)//' days'
    END IF
    !
  END FUNCTION
  !
  PURE FUNCTION DATE_OPERATOR_PRETTYPRINT(DT, SEP) RESULT (PPRINT)
    CLASS(DATE_OPERATOR),   INTENT(IN):: DT
    CHARACTER(*), OPTIONAL, INTENT(IN):: SEP  !IF PROVIDED THEN THE CLOCK IS INCLUDED
    CHARACTER(:), ALLOCATABLE:: PPRINT
    CHARACTER(4):: YEAR
    CHARACTER(2):: DAY
    !
    WRITE(YEAR,'(I4.4)') DT%YEAR
    WRITE(DAY, '(I2.2)') DT%DAY
    !
    IF(PRESENT(SEP)) THEN
        PPRINT = MONTHNAME(DT%MONTH,TRUE)//' '//DAY//', '//YEAR//SEP//DATE_OPERATOR_FRAC_TO_TIME(DT)
    ELSE
        PPRINT = MONTHNAME(DT%MONTH,TRUE)//' '//DAY//', '//YEAR
    END IF
    !
  END FUNCTION
  !
  PURE FUNCTION EMPTY_STRING(LN) RESULT(EMPTY)
    CHARACTER(*),      INTENT(IN):: LN
    LOGICAL:: EMPTY
    INTEGER:: I
    !
    EMPTY = TRUE
    DO I=ONE, LEN_TRIM(LN)
        IF(LN(I:I).NE.BLNK .AND. LN(I:I).NE.TAB ) THEN
            EMPTY = FALSE
            EXIT
        END IF
    END DO
    !
  END FUNCTION
  !  
END MODULE
!
!