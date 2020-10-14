!
! CODE DEVELOPED BY SCOTT E BOYCE 
!                   CONTACT <seboyce@usgs.gov> or <Boyce@engineer.com>
!
MODULE TIMER_INSTRUCTION!, ONLY: SIMPLE_TIMER, CPU_TIMER, GET_SYSTEM_DATE_AND_TIME
  USE, INTRINSIC:: ISO_FORTRAN_ENV, ONLY: i32 => INT32,  i64 => INT64,  DBL => REAL64, stdout => OUTPUT_UNIT
  USE, INTRINSIC:: IEEE_ARITHMETIC, ONLY: IEEE_VALUE, IEEE_QUIET_NAN
  ! 
  IMPLICIT NONE
  ! 
  PUBLIC:: SIMPLE_TIMER    ! Data Type that uses Wall Clock to determine time delta -> Wall Time => not accurate for very short time intervals; < 1sec
  PUBLIC:: CPU_TIMER       ! Data Type that uses CPU Cycles to determine time delta -> CPU  Time
  PUBLIC:: GET_SYSTEM_DATE_AND_TIME !(JDN, YR)
  !
  PRIVATE
  !
  !-----------------------------------------------\
  INTEGER(i32), PARAMETER:: Z   =  0_i32          !
  INTEGER(i64), PARAMETER:: ZER =  0_i64          !
  REAL   (DBL), PARAMETER:: DZ  =  0_dbl          !
  !-----------------------------------------------/
  !
  TYPE CPU_TIMER
      INTEGER(i64) :: EPOCH      = ZER
      INTEGER(i64) :: FINISH     = ZER
      REAL   (DBL) :: ClockRate  = -999_DBL
      LOGICAL      :: NO_RATE    = .TRUE.
      !
      REAL(DBL),   DIMENSION(:), ALLOCATABLE :: LAPS
      INTEGER(i32),              ALLOCATABLE :: LAP_NUM
      !
      CONTAINS
      !
      PROCEDURE, PASS(TIMER):: INIT      =>      INIT_CPU_TIMER ! CALL TIM%INIT      ()                      - Optional, sets up ClockRate
      PROCEDURE, PASS(TIMER):: START     =>     START_CPU_TIMER ! CALL TIM%START     ()                      - Start the timer
      PROCEDURE, PASS(TIMER):: ELAPSED   =>   ELAPSED_CPU_TIMER ! S =  TIM%ELAPSED   ()                      - Returns the number of seconds since %START()
      PROCEDURE, PASS(TIMER):: SAVE_LAP  =>  SAVE_LAP_CPU_TIMER ! CALL TIM%SAVE_LAP  ()                      - Get lap time and store in TIMER%LAPS
      PROCEDURE, PASS(TIMER):: LAP       =>       LAP_CPU_TIMER ! CALL TIM%LAP       (SEC, [SAVE_LAP])       - SEC is set to the number of seconds since start of last lap, if SAVE_LAP=TRUE, then calls SAVE_LAP_CPU_TIMER
      PROCEDURE, PASS(TIMER):: PRINT_LAP => PRINT_LAP_CPU_TIMER ! CALL TIM%PRINT_LAP ([IOUT], [TXT], [HED]) - write to IOUT or prompt the elapsed time in seconds since start of last lap
      PROCEDURE, PASS(TIMER):: PRINT_LAPS=> PRINT_LAPS_CPU_TIMER! CALL TIM%PRINT_LAPS([IOUT], [HED])         - write to IOUT or prompt all values stored in LAPS
      PROCEDURE, NOPASS     :: PRINT     =>   PRETTY_PRINT_TIMER! CALL TIM%PRINT     (SEC,[LEVEL])           - Return string with nice formatting of sec. Default is to format based on highest supported unit, LEVEL overrides this for a set number of terms, such that 
      !                                                                                                        LEVEL = 1: sec
      !                                                                                                                2: min, sec
      !                                                                                                                3: hr, min, sec
      !                                                                                                                4: day, hr, min, sec
      FINAL:: FINAL_CPU_TIMER
      !                                                                                                                                                                                                                                                        
  END TYPE 
  !
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !#####################################################################################################
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !
  TYPE SIMPLE_TIMER
      REAL   (DBL) :: EPOCH_JDN  = DZ
      INTEGER(i32) :: EPOCH_YR   = Z
      !
      REAL   (DBL) :: FINISH_JDN = DZ
      INTEGER(i32) :: FINISH_YR  = Z
      !
      REAL(DBL),   DIMENSION(:), ALLOCATABLE :: LAPS
      INTEGER(i32),              ALLOCATABLE :: LAP_NUM
      !
      CONTAINS
      !
      PROCEDURE, PASS(TIMER):: START     =>     START_SIMPLE_TIMER ! CALL START     ()                      - Start the timer
      PROCEDURE, PASS(TIMER):: ELAPSED   =>   ELAPSED_SIMPLE_TIMER ! S =  ELAPSED   ()                      - Returns the number of seconds since %START()
      PROCEDURE, PASS(TIMER):: SAVE_LAP  =>  SAVE_LAP_SIMPLE_TIMER ! CALL SAVE_LAP  ()                      - Get lap time and store in TIMER%LAPS
      PROCEDURE, PASS(TIMER):: LAP       =>       LAP_SIMPLE_TIMER ! CALL LAP       (SEC, [SAVE_LAP])       - SEC is set to the number of seconds since start of last lap, if SAVE_LAP=TRUE, then calls SAVE_LAP_SIMPLE_TIMER
      PROCEDURE, PASS(TIMER):: PRINT_LAP => PRINT_LAP_SIMPLE_TIMER ! CALL PRINT_LAP ([IOUT], [TXT], [HED])  - write to IOUT or prompt the elapsed time in seconds since start of last lap
      PROCEDURE, PASS(TIMER):: PRINT_LAPS=> PRINT_LAPS_SIMPLE_TIMER! CALL PRINT_LAPS([IOUT], [HED])         - write to IOUT or prompt all values stored in LAPS
      PROCEDURE, NOPASS     :: PRINT     =>   PRETTY_PRINT_TIMER   ! CALL PRINT     (SEC,[LEVEL])           - Return string with nice formatting of sec. Default is to format based on highest supported unit, LEVEL overrides this for a set number of terms, such that 
      !                                                                                                       LEVEL = 1: sec
      !                                                                                                               2: min, sec
      !                                                                                                               3: hr, min, sec
      !                                                                                                               4: day, hr, min, sec
      FINAL:: FINAL_SIMPLE_TIMER
      !                                                                                                                                                                                                                                                        
  END TYPE 
  !
  !--------------------------------------------------------------------------------------------------------------------------
  REAL(DBL), PARAMETER:: msec2day = 1.1574074074074074074D-8  ! ~ 1 milisecond in days
  REAL(DBL), PARAMETER::  sec2day = 1.1574074074074074074D-5  ! ~ 1 second in days
  REAL(DBL), PARAMETER::  min2day = 6.9444444444444444444D-4  ! ~ 1 minute in days
  REAL(DBL), PARAMETER::   hr2day = 0.0416666666666666666D0   ! ~ 1 hour   in days 
  !
  REAL(DBL), PARAMETER::  day2sec = 86400_dbl   
  REAL(DBL), PARAMETER::   hr2sec =  3600_dbl
  REAL(DBL), PARAMETER::  min2sec =    60_dbl
  !--------------------------------------------------------------------------------------------------------------------------
  !
  !
  CONTAINS
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !#####################################################################################################
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !
  SUBROUTINE INIT_CPU_TIMER(TIMER)
    CLASS(CPU_TIMER), INTENT(INOUT):: TIMER
    INTEGER(i64):: ClockRate
    INTEGER     :: I
    !
    TIMER%EPOCH  = ZER
    TIMER%FINISH = ZER
    !
    IF(ALLOCATED(TIMER%LAPS)   ) DEALLOCATE(TIMER%LAPS)
    IF(ALLOCATED(TIMER%LAP_NUM)) DEALLOCATE(TIMER%LAP_NUM)
    !
    CALL SYSTEM_CLOCK(COUNT_RATE=ClockRate)
    !
    IF(ClockRate <= ZER) THEN
                             I = 1     !  -- Spin a milisecond to prevent mis-fire of subroutine (compiler bug)
                             DO WHILE (I < 100000 .AND. ClockRate <= ZER)    
                                       CALL SYSTEM_CLOCK(COUNT_RATE=ClockRate)
                                       I = I + 1
                             END DO
                             IF(ClockRate <= ZER) CALL SYSTEM_CLOCK(COUNT_RATE=ClockRate)
    END IF
    !
    TIMER%NO_RATE = ClockRate <= ZER
    !
    IF( TIMER%NO_RATE ) THEN
                       TIMER%ClockRate = IEEE_VALUE(TIMER%ClockRate, IEEE_QUIET_NAN)
    ELSE
                       TIMER%ClockRate = 1_DBL / REAL(ClockRate, DBL)
    END IF
    !
  END SUBROUTINE
  !
  !--------------------------------------------------------------------------------------------------------------------------
  !
  SUBROUTINE GET_SYSTEM_CLOCK_COUNT(TIMER, IVAL)
    CLASS(CPU_TIMER), INTENT(IN   ):: TIMER
    INTEGER(i64),     INTENT(INOUT):: IVAL
    INTEGER:: I
    !
    IF(TIMER%NO_RATE) THEN
                      IVAL = TIMER%EPOCH
    ELSE
        CALL SYSTEM_CLOCK(COUNT=IVAL)
        !
        IF(IVAL <= ZER) THEN
                        I = 1     !  -- Spin a milisecond to prevent mis-fire of subroutine (compiler bug)
                        DO WHILE (I < 100000 .AND. IVAL <= ZER)    
                                  CALL SYSTEM_CLOCK(COUNT=IVAL)
                                  I = I + 1
                        END DO
                        IF(IVAL <= ZER) CALL SYSTEM_CLOCK(COUNT=IVAL)
        END IF
    END IF
    !
  END SUBROUTINE
  !
  !--------------------------------------------------------------------------------------------------------------------------
  !
  SUBROUTINE START_CPU_TIMER(TIMER)
    CLASS(CPU_TIMER), INTENT(INOUT):: TIMER
    INTEGER(i64):: IVAL
    !
    IF(TIMER%NO_RATE) CALL INIT_CPU_TIMER(TIMER)
    !
    CALL GET_SYSTEM_CLOCK_COUNT(TIMER, IVAL)
    !
    TIMER%EPOCH  = IVAL
    TIMER%FINISH = IVAL
    !
  END SUBROUTINE
  !
  !--------------------------------------------------------------------------------------------------------------------------
  !
  FUNCTION ELAPSED_CPU_TIMER(TIMER)  RESULT(SEC)
    CLASS(CPU_TIMER), INTENT(IN):: TIMER
    REAL   (DBL):: SEC
    INTEGER(i64):: FINISH
    !
    CALL GET_SYSTEM_CLOCK_COUNT(TIMER, FINISH)
    !
    IF(FINISH <= TIMER%EPOCH) THEN
        SEC = 0_DBL
    ELSE
        SEC = REAL( FINISH-TIMER%EPOCH, DBL ) * TIMER%ClockRate
    END IF
    !
  END FUNCTION
  !
  !--------------------------------------------------------------------------------------------------------------------------
  !
  SUBROUTINE LAP_CPU_TIMER(TIMER, SEC, SAVE_LAP)
    CLASS(CPU_TIMER),    INTENT(INOUT):: TIMER
    REAL(DBL),           INTENT(INOUT):: SEC
    LOGICAL,   OPTIONAL, INTENT(IN   ):: SAVE_LAP
    INTEGER(i64):: LAP
    !
    CALL GET_SYSTEM_CLOCK_COUNT(TIMER, LAP)
    !
    IF(LAP <= TIMER%EPOCH) THEN
                SEC = 0_DBL
                TIMER%FINISH = TIMER%EPOCH
    ELSE
                SEC = REAL( LAP-TIMER%FINISH, DBL ) * TIMER%ClockRate
                TIMER%FINISH = LAP
    END IF
    !
    IF(PRESENT(SAVE_LAP)) THEN
            IF(SAVE_LAP) CALL ADD_LAP_TIM(SEC, TIMER%LAP_NUM, TIMER%LAPS)
    END IF
    !
  END SUBROUTINE
  !
  !--------------------------------------------------------------------------------------------------------------------------
  !
  SUBROUTINE SAVE_LAP_CPU_TIMER(TIMER)
    CLASS(CPU_TIMER), INTENT(INOUT):: TIMER
    REAL(DBL):: SEC
    INTEGER(i64):: LAP
    !
    CALL GET_SYSTEM_CLOCK_COUNT(TIMER, LAP)
    !
    IF(LAP <= TIMER%EPOCH) THEN
                TIMER%FINISH = TIMER%EPOCH
                SEC = 0_DBL
    ELSE
                SEC = REAL( LAP-TIMER%FINISH, DBL ) * TIMER%ClockRate
                TIMER%FINISH = LAP
    END IF
    !
    CALL ADD_LAP_TIM(SEC, TIMER%LAP_NUM, TIMER%LAPS)
    !
  END SUBROUTINE
  !
  !--------------------------------------------------------------------------------------------------------------------------
  !
  SUBROUTINE PRINT_LAP_CPU_TIMER(TIMER, IOUT, TXT, HED)
    CLASS(CPU_TIMER),           INTENT(INOUT):: TIMER
    INTEGER,             OPTIONAL, INTENT(IN   ):: IOUT
    CHARACTER(*),        OPTIONAL, INTENT(IN   ):: TXT, HED
    REAL(DBL):: SEC
    INTEGER:: IU
    !
    CALL LAP_CPU_TIMER(TIMER, SEC)
    !
    IF(PRESENT(IOUT)) THEN
        IU = IOUT
    ELSE
        IU = stdout
    END IF
    !
    IF(PRESENT(HED)) WRITE(IU,'(A)') HED
    IF(PRESENT(TXT)) THEN
                        WRITE(IU,'(A,F16.2)') TXT, SEC
    ELSE
                        WRITE(IU,'(F16.2)') SEC
    END IF
    !
  END SUBROUTINE
  !
  !--------------------------------------------------------------------------------------------------------------------------
  !
  SUBROUTINE PRINT_LAPS_CPU_TIMER(TIMER, IOUT, HED)
    CLASS(CPU_TIMER),           INTENT(INOUT):: TIMER
    INTEGER,             OPTIONAL, INTENT(IN   ):: IOUT
    CHARACTER(*),        OPTIONAL, INTENT(IN   ):: HED
    INTEGER:: I, IU
    CHARACTER(5):: L
    !
    IF(PRESENT(IOUT)) THEN
        IU = IOUT
    ELSE
        IU = stdout
    END IF
    !
    IF(PRESENT(HED)) WRITE(IU,'(A)') HED
    WRITE(IU,'(A)') 'LAP        SECONDS'
    !
    IF(ALLOCATED(TIMER%LAPS) ) THEN
       DO I=1, TIMER%LAP_NUM
               WRITE(L,'(I5)') I
               L = ADJUSTL(L)
               WRITE(IU,'(A, F16.2)') L, TIMER%LAPS(I)
       END DO
    END IF
    !
  END SUBROUTINE
  !
  !--------------------------------------------------------------------------------------------------------------------------
  !
  PURE SUBROUTINE DESTROY_CPU_TIMER(TIMER)
    CLASS(CPU_TIMER), INTENT(INOUT):: TIMER
    !
    TIMER%EPOCH     = ZER
    TIMER%FINISH    = ZER
    TIMER%ClockRate = -999_DBL
    !
    TIMER%NO_RATE = .TRUE.
    !
    IF(ALLOCATED(TIMER%LAPS)   ) DEALLOCATE(TIMER%LAPS)
    IF(ALLOCATED(TIMER%LAP_NUM)) DEALLOCATE(TIMER%LAP_NUM)
    !
  END SUBROUTINE
  !
  !--------------------------------------------------------------------------------------------------------------------------
  !
  PURE SUBROUTINE FINAL_CPU_TIMER(TIMER)
    TYPE(CPU_TIMER), INTENT(INOUT):: TIMER
    !
    CALL DESTROY_CPU_TIMER(TIMER)
    !
  END SUBROUTINE
  !
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !#####################################################################################################
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !
  SUBROUTINE START_SIMPLE_TIMER(TIMER)
    CLASS(SIMPLE_TIMER), INTENT(INOUT):: TIMER
    !
    IF(ALLOCATED(TIMER%LAPS)   ) DEALLOCATE(TIMER%LAPS)
    IF(ALLOCATED(TIMER%LAP_NUM)) DEALLOCATE(TIMER%LAP_NUM)
    !
    CALL GET_SYSTEM_DATE_AND_TIME(TIMER%EPOCH_JDN, TIMER%EPOCH_YR)
    !
    TIMER%FINISH_JDN = TIMER%EPOCH_JDN
    TIMER%FINISH_YR  = TIMER%EPOCH_YR
    !
  END SUBROUTINE
  !
  !--------------------------------------------------------------------------------------------------------------------------
  !
  FUNCTION ELAPSED_SIMPLE_TIMER(TIMER) RESULT(SEC)
    CLASS(SIMPLE_TIMER), INTENT(IN):: TIMER
    REAL(DBL):: SEC
    REAL   (DBL):: FINISH_JDN
    INTEGER(i32):: FINISH_YR
    !
    CALL GET_SYSTEM_DATE_AND_TIME(FINISH_JDN, FINISH_YR)
    !
    SEC = ELAPSED_TIME(TIMER%EPOCH_JDN, TIMER%EPOCH_YR, FINISH_JDN, FINISH_YR)
    !
  END FUNCTION
  !
  !--------------------------------------------------------------------------------------------------------------------------
  !
  SUBROUTINE LAP_SIMPLE_TIMER(TIMER, SEC, SAVE_LAP)
    CLASS(SIMPLE_TIMER), INTENT(INOUT):: TIMER
    REAL(DBL),           INTENT(INOUT):: SEC
    LOGICAL,   OPTIONAL, INTENT(IN   ):: SAVE_LAP
    REAL   (DBL):: LAP_JDN
    INTEGER(i32):: LAP_YR
    !
    CALL GET_SYSTEM_DATE_AND_TIME(LAP_JDN, LAP_YR)
    !
    SEC = ELAPSED_TIME(TIMER%FINISH_JDN, TIMER%FINISH_YR, LAP_JDN, LAP_YR)
    !
    TIMER%FINISH_JDN = LAP_JDN
    TIMER%FINISH_YR  = LAP_YR
    !
    IF(PRESENT(SAVE_LAP)) THEN
            IF(SAVE_LAP) CALL ADD_LAP_TIM(SEC, TIMER%LAP_NUM, TIMER%LAPS)
    END IF
    !
  END SUBROUTINE
  !
  !--------------------------------------------------------------------------------------------------------------------------
  !
  SUBROUTINE SAVE_LAP_SIMPLE_TIMER(TIMER)
    CLASS(SIMPLE_TIMER), INTENT(INOUT):: TIMER
    REAL   (DBL):: SEC
    REAL   (DBL):: LAP_JDN
    INTEGER(i32):: LAP_YR
    !
    CALL GET_SYSTEM_DATE_AND_TIME(LAP_JDN, LAP_YR)
    !
    SEC = ELAPSED_TIME(TIMER%FINISH_JDN, TIMER%FINISH_YR, LAP_JDN, LAP_YR)
    !
    TIMER%FINISH_JDN = LAP_JDN
    TIMER%FINISH_YR  = LAP_YR
    !
    CALL ADD_LAP_TIM(SEC, TIMER%LAP_NUM, TIMER%LAPS)
    !
  END SUBROUTINE
  !
  !--------------------------------------------------------------------------------------------------------------------------
  !
  SUBROUTINE PRINT_LAP_SIMPLE_TIMER(TIMER, IOUT, TXT, HED)
    CLASS(SIMPLE_TIMER),           INTENT(INOUT):: TIMER
    INTEGER,             OPTIONAL, INTENT(IN   ):: IOUT
    CHARACTER(*),        OPTIONAL, INTENT(IN   ):: TXT, HED
    REAL(DBL):: SEC
    INTEGER:: IU
    !
    CALL LAP_SIMPLE_TIMER(TIMER, SEC)
    !
    IF(PRESENT(IOUT)) THEN
        IU = IOUT
    ELSE
        IU = stdout
    END IF
    !
    IF(PRESENT(HED)) WRITE(IU,'(A)') HED
    IF(PRESENT(TXT)) THEN
                        WRITE(IU,'(A,F16.2)') TXT, SEC
    ELSE
                        WRITE(IU,'(F16.2)') SEC
    END IF
    !
  END SUBROUTINE
  !
  !--------------------------------------------------------------------------------------------------------------------------
  !
  SUBROUTINE PRINT_LAPS_SIMPLE_TIMER(TIMER, IOUT, HED)
    CLASS(SIMPLE_TIMER),           INTENT(INOUT):: TIMER
    INTEGER,             OPTIONAL, INTENT(IN   ):: IOUT
    CHARACTER(*),        OPTIONAL, INTENT(IN   ):: HED
    INTEGER:: I, IU
    CHARACTER(5):: L
    !
    IF(PRESENT(IOUT)) THEN
        IU = IOUT
    ELSE
        IU = stdout
    END IF
    !
    IF(PRESENT(HED)) WRITE(IU,'(A)') HED
    WRITE(IU,'(A)') 'LAP        SECONDS'
    !
    IF(ALLOCATED(TIMER%LAPS) ) THEN
       DO I=1, TIMER%LAP_NUM
               WRITE(L,'(I5)') I
               L = ADJUSTL(L)
               WRITE(IU,'(A, F16.2)') L, TIMER%LAPS(I)
       END DO
    END IF
    !
  END SUBROUTINE
  !
  !--------------------------------------------------------------------------------------------------------------------------
  !
  PURE SUBROUTINE DESTROY_SIMPLE_TIMER(TIMER)
    CLASS(SIMPLE_TIMER), INTENT(INOUT):: TIMER
    !
    TIMER%EPOCH_JDN  = DZ
    TIMER%EPOCH_YR   = Z
    !
    TIMER%FINISH_JDN = DZ
    TIMER%FINISH_YR  = Z
    !
    IF(ALLOCATED(TIMER%LAPS)   ) DEALLOCATE(TIMER%LAPS)
    IF(ALLOCATED(TIMER%LAP_NUM)) DEALLOCATE(TIMER%LAP_NUM)
    !
  END SUBROUTINE
  !
  !--------------------------------------------------------------------------------------------------------------------------
  !
  PURE SUBROUTINE FINAL_SIMPLE_TIMER(TIMER)
    TYPE(SIMPLE_TIMER), INTENT(INOUT):: TIMER
    !
    CALL DESTROY_SIMPLE_TIMER(TIMER)
    !
  END SUBROUTINE
  !
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !#####################################################################################################
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !
  SUBROUTINE GET_SYSTEM_DATE_AND_TIME(JDN, YR)
    REAL   (DBL), INTENT(INOUT) :: JDN
    INTEGER(i32), INTENT(INOUT) :: YR 
    INTEGER(i32),  DIMENSION(8) :: DT  !Used by CALL DATE_AND_TIME to get current time stamp
    !
    CALL DATE_AND_TIME(VALUES = DT)
    !                YEAR, MONTH,   DAY,   HR,    MIN,   SEC,  MSEC
    JDN = JULIANDAY(DT(1), DT(2), DT(3), DT(5), DT(6), DT(7), DT(8))
    YR  = DT(1)
    !
  END SUBROUTINE
  !
  !--------------------------------------------------------------------------------------------------------------------------
  !
  PURE SUBROUTINE ADD_LAP_TIM(SEC, LAP_NUM, LAPS)
    REAL(DBL),                              INTENT(IN   ):: SEC
    INTEGER(i32),              ALLOCATABLE, INTENT(INOUT):: LAP_NUM
    REAL(DBL),   DIMENSION(:), ALLOCATABLE, INTENT(INOUT):: LAPS
    REAL(DBL), DIMENSION(:), ALLOCATABLE:: TMP
    INTEGER:: I
    !
    IF(.NOT. ALLOCATED(LAP_NUM)) ALLOCATE(LAP_NUM, SOURCE=0_i32)
    IF(.NOT. ALLOCATED(LAPS)   ) ALLOCATE(LAPS(16))
    !
    LAP_NUM = LAP_NUM + 1_i32
    !
    I = SIZE(LAPS)
    IF(LAP_NUM > I) THEN
        CALL MOVE_ALLOC(LAPS, TMP)
        !
        I = I * 2_i32
        ALLOCATE(LAPS(I))
        !
        DO CONCURRENT (I=1:SIZE(TMP))
                           LAPS(I) = TMP(I)
        END DO
    END IF
    !
    LAPS(LAP_NUM) = SEC
    !
  END SUBROUTINE
  !
  !--------------------------------------------------------------------------------------------------------------------------
  !
  PURE FUNCTION ELAPSED_TIME(START_JDN, START_YR, FINISH_JDN, FINISH_YR) RESULT(SEC)
    REAL   (DBL), INTENT(IN):: START_JDN, FINISH_JDN
    INTEGER(i32), INTENT(IN):: START_YR,  FINISH_YR
    REAL   (DBL):: SEC
    INTEGER(i32):: YEAR
    !
    SEC = (FINISH_JDN - START_JDN) * day2sec
    !
    IF (FINISH_YR > START_YR) THEN
        !
        YEAR = START_YR 
        !
        DO WHILE (YEAR < FINISH_YR)
                         IF ( (MOD(YEAR,4_i32) == Z .AND. MOD(YEAR,100_i32) /= Z) .OR. MOD(YEAR,400_i32) == Z ) THEN
                             SEC = SEC + 3.162240d7   ! 366D * 86400
                         ELSE
                             SEC = SEC + 3.153600d7   ! 365D * 86400
                         END IF
                         !
                         YEAR = YEAR + 1_i32
        END DO
    END IF
    !
    IF( SEC < DZ ) SEC = DZ
    !
  END FUNCTION
  !
  !--------------------------------------------------------------------------------------------------------------------------
  !
  PURE FUNCTION JULIANDAY(YEAR, MONTH, DAY, HR, MIN, SEC, MSEC)  ! Function sets 1/1/YYYY to have a JDN = 0
    !
    INTEGER(i32), INTENT(IN):: DAY, MONTH, YEAR, HR, MIN, SEC, MSEC
    REAL(DBL):: JULIANDAY
    !
    IF    (MONTH ==  1_i32) THEN
                            JULIANDAY = DZ
    ELSEIF(MONTH ==  2_i32) THEN
                            JULIANDAY = 31_dbl
    ELSEIF(MONTH ==  3_i32) THEN
                            JULIANDAY = 59_dbl
    ELSEIF(MONTH ==  4_i32) THEN
                            JULIANDAY = 90_dbl
    ELSEIF(MONTH ==  5_i32) THEN
                            JULIANDAY = 120_dbl
    ELSEIF(MONTH ==  6_i32) THEN
                            JULIANDAY = 151_dbl
    ELSEIF(MONTH ==  7_i32) THEN
                            JULIANDAY = 181_dbl
    ELSEIF(MONTH ==  8_i32) THEN
                            JULIANDAY = 212_dbl
    ELSEIF(MONTH ==  9_i32) THEN
                            JULIANDAY = 243_dbl
    ELSEIF(MONTH == 10_i32) THEN
                            JULIANDAY = 273_dbl
    ELSEIF(MONTH == 11_i32) THEN
                            JULIANDAY = 304_dbl
    ELSE!IF(MONTH == 12_i32) THEN
                            JULIANDAY = 334_dbl
    END IF
    !
    IF( MONTH > 2_i32 ) THEN ! Leap year if year is divisable by 4 and not 100 or if year is divisable by 400 -> If leap and > Feb add 1 day
        !
        IF( (MOD(YEAR,4_i32) == Z .AND. MOD(YEAR,100_i32) /= Z) .OR. MOD(YEAR,400_i32) == Z ) JULIANDAY = JULIANDAY + 1_dbl
    END IF
    !
    JULIANDAY = JULIANDAY + REAL( DAY-1_i32, dbl) + REAL( HR, dbl)*HR2DAY + REAL( MIN, dbl)*MIN2DAY + REAL( SEC, dbl)*SEC2DAY + REAL( MSEC, dbl)*MSEC2DAY
    !
  END FUNCTION
  !
  !--------------------------------------------------------------------------------------------------------------------------
  !
  PURE FUNCTION PRETTY_PRINT_TIMER(SEC, LEVEL) RESULT(TIM)
    REAL(DBL), VALUE:: SEC
    INTEGER,  OPTIONAL, INTENT(IN   ):: LEVEL  ! of output 1: sec; 2: min, sec; 3: hr, min, sec; 4: day, hr, min, sec
    CHARACTER(:), ALLOCATABLE:: TIM
    CHARACTER(:), ALLOCATABLE:: DY, HR, MN
    CHARACTER(32):: TMP
    INTEGER:: ITMP, LVL
    !
    IF(PRESENT(LEVEL)) THEN
        LVL = LEVEL
    ELSE
        LVL = -1
    END IF
    !
    IF( (SEC > day2sec .AND. LVL < 0) .OR. LVL == 4 ) THEN
        ITMP = INT(SEC / day2sec)
        WRITE(TMP,'(I32)') ITMP
        DY = TRIM(ADJUSTL(TMP))//' day, '
        !
        SEC = SEC - day2sec*REAL(ITMP, dbl)
        !
        ITMP = INT(SEC / hr2sec)
        WRITE(TMP,'(I32)') ITMP
        HR = TRIM(ADJUSTL(TMP))//' hr, '
        !
        SEC = SEC - hr2sec*REAL(ITMP, dbl)
        !
        ITMP = INT(SEC / min2sec)
        WRITE(TMP,'(I32)') ITMP
        DY = TRIM(ADJUSTL(TMP))//' min, '
        !
        SEC = SEC - min2sec*REAL(ITMP, dbl)
        !
        ITMP = INT(SEC)
        WRITE(TMP,'(I32)') ITMP
        TIM = DY//HR//MN//TRIM(ADJUSTL(TMP))//' sec'
        !
    ELSEIF( (SEC > hr2sec .AND. LVL < 0) .OR. LVL == 3 ) THEN
        !
        ITMP = INT(SEC / hr2sec)
        WRITE(TMP,'(I32)') ITMP
        HR = TRIM(ADJUSTL(TMP))//' hr, '
        !
        SEC = SEC - hr2sec*REAL(ITMP, dbl)
        !
        ITMP = INT(SEC / min2sec)
        WRITE(TMP,'(I32)') ITMP
        DY = TRIM(ADJUSTL(TMP))//' min, '
        !
        SEC = SEC - min2sec*REAL(ITMP, dbl)
        !
        ITMP = INT(SEC)
        WRITE(TMP,'(I32)') ITMP
        TIM = DY//HR//MN//TRIM(ADJUSTL(TMP))//' sec'
        !
    ELSEIF( (SEC > min2sec .AND. LVL < 0) .OR. LVL == 2  ) THEN
        !
        ITMP = INT(SEC / min2sec)
        WRITE(TMP,'(I32)') ITMP
        DY = TRIM(ADJUSTL(TMP))//' min, '
        !
        SEC = SEC - min2sec*REAL(ITMP, dbl)
        !
        WRITE(TMP,'(F32.1)') SEC
        TIM = DY//HR//MN//TRIM(ADJUSTL(TMP))//' sec'
        !
    ELSE
        WRITE(TMP,'(F32.2)') SEC
        TIM = TRIM(ADJUSTL(TMP))//' sec'
    END IF
    !
  END FUNCTION
  !
END MODULE