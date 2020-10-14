MODULE SLEEP_INTERFACE!, ONLY: SLEEP, OS_SLEEP
  USE, INTRINSIC:: ISO_FORTRAN_ENV, ONLY: INT32, INT64, REL32 => REAL32, REL64 => REAL64
  ! 
  IMPLICIT NONE
  ! 
  ! *** Cation -> There is no upper limit to sleeping. 
  !                     => Accuracy may degrade if SEC exceeds 1 year ( ~ 3e7 sec )
  !                     => Times < 1 sec are not accurate
  ! 
  PUBLIC::     SLEEP  ! CALL    SLEEP(SEC) -> Puts CPU thread on infinite loop for specified number of seconds
  PUBLIC::  OS_SLEEP  ! CALL OS_SLEEP(SEC) -> Halts program execution and calls OS sleep function for specified number of seconds - Only accurate within 1 sec and holds CPU_TIME() subroutine from counting forward
  !
  PRIVATE  
  !
  INTERFACE SLEEP
    MODULE PROCEDURE SLEEP_INT32 ! CALL SLEEP(SEC)
    MODULE PROCEDURE SLEEP_INT64 ! CALL SLEEP(SEC)
    MODULE PROCEDURE SLEEP_REL32 ! CALL SLEEP(SEC)
    MODULE PROCEDURE SLEEP_REL64 ! CALL SLEEP(SEC)
  END INTERFACE
  !
  INTERFACE OS_SLEEP
    MODULE PROCEDURE OS_SLEEP_INT32 ! CALL SLEEP(SEC)
    MODULE PROCEDURE OS_SLEEP_INT64 ! CALL SLEEP(SEC)
    MODULE PROCEDURE OS_SLEEP_REL32 ! CALL SLEEP(SEC)
    MODULE PROCEDURE OS_SLEEP_REL64 ! CALL SLEEP(SEC)
  END INTERFACE
  !
  !--------------------------------------------------------------------------------------------------\
  INTEGER(INT32), PARAMETER:: ZER =  0_INT64                                                         !
  !                                                                                                  !
  INTEGER(INT32), PARAMETER:: WIN_SLEEP_LIM_INT32 = 99999_INT32                                      !
  INTEGER(INT64), PARAMETER:: WIN_SLEEP_LIM_INT64 = 99999_INT64                                      !
  REAL   (REL32), PARAMETER:: WIN_SLEEP_LIM_REL32 = 99999_REL32                                      !
  REAL   (REL64), PARAMETER:: WIN_SLEEP_LIM_REL64 = 99999_REL64                                      !
  !--------------------------------------------------------------------------------------------------/
  !
  CONTAINS
  !
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !#####################################################################################################
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !
  SUBROUTINE SLEEP_INT32(SEC)
    INTEGER(INT32), INTENT(IN):: SEC
    REAL   (REL32):: TIME, TIME_STOP
    !
    CALL CPU_TIME(TIME_STOP)
    !
    IF( TIME_STOP > -0.1_REL32 ) THEN
        !
        TIME_STOP = TIME_STOP + REAL(SEC, REL32)
        !
        CALL CPU_TIME(TIME)
        !
        DO WHILE (TIME < TIME_STOP)
                                   CALL CPU_TIME(TIME)
        END DO
    END IF
  END SUBROUTINE
  !
  SUBROUTINE SLEEP_INT64(SEC)
    INTEGER(INT64), INTENT(IN):: SEC
    REAL   (REL32):: TIME, TIME_STOP
    !
    CALL CPU_TIME(TIME_STOP)
    !
    IF( TIME_STOP > -0.1_REL32 ) THEN
        !
        TIME_STOP = TIME_STOP + REAL(SEC, REL32)
        !
        CALL CPU_TIME(TIME)
        !
        DO WHILE (TIME < TIME_STOP)
                                   CALL CPU_TIME(TIME)
        END DO
    END IF
  END SUBROUTINE
  !
  SUBROUTINE SLEEP_REL32(SEC)
    REAL(REL32), INTENT(IN):: SEC
    REAL(REL32):: TIME, TIME_STOP
    !
    CALL CPU_TIME(TIME_STOP)
    !
    IF( TIME_STOP > -0.1_REL32 ) THEN
        !
        TIME_STOP = TIME_STOP + REAL(SEC, REL32)
        !
        CALL CPU_TIME(TIME)
        !
        DO WHILE (TIME < TIME_STOP)
                                   CALL CPU_TIME(TIME)
        END DO
    END IF
  END SUBROUTINE
  !
  SUBROUTINE SLEEP_REL64(SEC)
    REAL(REL64), INTENT(IN):: SEC
    REAL(REL32):: TIME, TIME_STOP
    !
    CALL CPU_TIME(TIME_STOP)
    !
    IF( TIME_STOP > -0.1_REL32 ) THEN
        !
        TIME_STOP = TIME_STOP + REAL(SEC, REL32)
        !
        CALL CPU_TIME(TIME)
        !
        DO WHILE (TIME < TIME_STOP)
                                   CALL CPU_TIME(TIME)
        END DO
    END IF
  END SUBROUTINE
  !
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !#####################################################################################################
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !
  SUBROUTINE OS_SLEEP_INT32(SEC)
    INTEGER(INT32), VALUE:: SEC
    CHARACTER( 8):: CMD, NUM
    CHARACTER(20):: OPT
    !
    CALL GET_ENVIRONMENT_VARIABLE ( "OS", value=OPT )
    !                         If any Windows variant then "OS" exists and equals Windows_NT
    IF(OPT == 'Windows_NT') THEN
         CMD = 'timeout '
         OPT = ' /nobreak >nul 2>nul'
    ELSE
         CMD = 'sleep   '
         OPT = ' >/dev/null 2>&1    '
    END IF
    !
    DO WHILE(SEC > WIN_SLEEP_LIM_INT32) 
                         CALL EXECUTE_COMMAND_LINE( CMD//'99999'//OPT )
                         SEC = SEC - WIN_SLEEP_LIM_INT32
    END DO
    !
    WRITE(NUM,'(I8)') INT(SEC)
    !
    CALL EXECUTE_COMMAND_LINE( CMD//NUM//OPT )
    !
  END SUBROUTINE
  !
  SUBROUTINE OS_SLEEP_INT64(SEC)
    INTEGER(INT64), VALUE:: SEC
    CHARACTER( 8):: CMD, NUM
    CHARACTER(20):: OPT
    !
    CALL GET_ENVIRONMENT_VARIABLE ( "OS", value=OPT )
    !                         If any Windows variant then "OS" exists and equals Windows_NT
    IF(OPT == 'Windows_NT') THEN
         CMD = 'timeout '
         OPT = ' /nobreak >nul 2>nul'
    ELSE
         CMD = 'sleep   '
         OPT = ' >/dev/null 2>&1    '
    END IF
    !
    DO WHILE(SEC > WIN_SLEEP_LIM_INT64) 
                         CALL EXECUTE_COMMAND_LINE( CMD//'99999'//OPT )
                         SEC = SEC - WIN_SLEEP_LIM_INT64
    END DO
    !
    WRITE(NUM,'(I8)') INT(SEC)
    !
    CALL EXECUTE_COMMAND_LINE( CMD//NUM//OPT )
    !
  END SUBROUTINE
  !
  SUBROUTINE OS_SLEEP_REL32(SEC)
    REAL(REL32), VALUE:: SEC
    CHARACTER( 8):: CMD, NUM
    CHARACTER(20):: OPT
    !
    CALL GET_ENVIRONMENT_VARIABLE ( "OS", value=OPT )
    !                         If any Windows variant then "OS" exists and equals Windows_NT
    IF(OPT == 'Windows_NT') THEN
         CMD = 'timeout '
         OPT = ' /nobreak >nul 2>nul'
    ELSE
         CMD = 'sleep   '
         OPT = ' >/dev/null 2>&1    '
    END IF
    !
    DO WHILE(SEC > WIN_SLEEP_LIM_REL32) 
                         CALL EXECUTE_COMMAND_LINE( CMD//'99999'//OPT )
                         SEC = SEC - WIN_SLEEP_LIM_REL32
    END DO
    !
    WRITE(NUM,'(I8)') INT(SEC)
    !
    CALL EXECUTE_COMMAND_LINE( CMD//NUM//OPT )
    !
  END SUBROUTINE
  !
  SUBROUTINE OS_SLEEP_REL64(SEC)
    REAL(REL64), VALUE:: SEC
    CHARACTER( 8):: CMD, NUM
    CHARACTER(20):: OPT
    !
    CALL GET_ENVIRONMENT_VARIABLE ( "OS", value=OPT )
    !                         If any Windows variant then "OS" exists and equals Windows_NT
    IF(OPT == 'Windows_NT') THEN
         CMD = 'timeout '
         OPT = ' /nobreak >nul 2>nul'
    ELSE
         CMD = 'sleep   '
         OPT = ' >/dev/null 2>&1    '
    END IF
    !
    DO WHILE(SEC > WIN_SLEEP_LIM_REL64) 
                         CALL EXECUTE_COMMAND_LINE( CMD//'99999'//OPT )
                         SEC = SEC - WIN_SLEEP_LIM_REL64
    END DO
    !
    WRITE(NUM,'(I8)') INT(SEC)
    !
    CALL EXECUTE_COMMAND_LINE( CMD//NUM//OPT )
    !
  END SUBROUTINE
  !
END MODULE
!
!!!MODULE C_SLEEP_INTERFACE !-> Case error for BIND(C, NAME="SLEEP") s BIND(C, NAME="sleep") on windows vs unix due to different header files (windows.h vs unistd.h)
!!!  USE, INTRINSIC :: ISO_C_BINDING,  ONLY: C_INT
!!!  USE, INTRINSIC:: ISO_FORTRAN_ENV, ONLY: INT32, INT64, REL32 => REAL32, REL64 => REAL64
!!!  ! 
!!!  IMPLICIT NONE
!!!  ! 
!!!  PRIVATE
!!!  PUBLIC:: SLEEP
!!!  !
!!!  INTERFACE SLEEP
!!!    MODULE PROCEDURE SLEEP_INT32 ! CALL SLEEP(SEC)
!!!    MODULE PROCEDURE SLEEP_INT64 ! CALL SLEEP(SEC)
!!!    MODULE PROCEDURE SLEEP_REL32 ! CALL SLEEP(SEC)
!!!    MODULE PROCEDURE SLEEP_REL64 ! CALL SLEEP(SEC)
!!!  END INTERFACE
!!!  !
!!!  INTEGER(C_INT), PARAMETER:: LIM_C_INT = 2147472384_C_INT
!!!  INTEGER(INT64), PARAMETER:: LIM_INT64 = 2147472384_INT64
!!!  REAL   (REL64), PARAMETER:: LIM_REL64 = 2147472384_REL64
!!!  !
!!!  REAL(REL32),    PARAMETER:: LIM_REL32       = 2147472000_REL32
!!!  INTEGER(C_INT), PARAMETER:: LIM_REL32_C_INT = 2147472000_C_INT
!!!  !
!!!  !
!!!  INTERFACE 
!!!          FUNCTION C_SLEEP(SEC) BIND(C, NAME="sleep")
!!!             USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT
!!!             !
!!!             INTEGER(C_INT), INTENT(IN), VALUE :: SEC
!!!             INTEGER(C_INT) :: C_SLEEP
!!!          END FUNCTION
!!!  END INTERFACE
!!!  !
!!!  CONTAINS
!!!  !
!!!  SUBROUTINE SLEEP_INT32(SEC)
!!!    INTEGER(INT32), INTENT(IN):: SEC
!!!    INTEGER(C_INT):: S, U
!!!    !
!!!    S = INT(SEC, C_INT)
!!!    U = C_SLEEP(S)
!!!  END SUBROUTINE
!!!  !
!!!  SUBROUTINE SLEEP_INT64(SEC)
!!!    INTEGER(INT64), VALUE:: SEC
!!!    INTEGER(C_INT):: S, U
!!!    !
!!!    DO WHILE(SEC > LIM_INT64) 
!!!                         U = C_SLEEP(LIM_C_INT)
!!!                         SEC = SEC - LIM_INT64
!!!    END DO
!!!    !
!!!    S = INT(SEC, C_INT)
!!!    U = C_SLEEP(S)
!!!  END SUBROUTINE
!!!  !
!!!  SUBROUTINE SLEEP_REL32(SEC)
!!!    REAL(REL32), VALUE:: SEC
!!!    INTEGER(C_INT):: S, U
!!!    !
!!!    DO WHILE(SEC > LIM_REL32) 
!!!                         U = C_SLEEP(LIM_REL32_C_INT)
!!!                         SEC = SEC - LIM_REL32
!!!    END DO
!!!    !
!!!    S = INT(SEC, C_INT)
!!!    U = C_SLEEP(S)
!!!  END SUBROUTINE
!!!  !
!!!  SUBROUTINE SLEEP_REL64(SEC)
!!!    REAL(REL64), VALUE:: SEC
!!!    INTEGER(C_INT):: S, U
!!!    !
!!!    DO WHILE(SEC > LIM_REL64) 
!!!                         U = C_SLEEP(LIM_C_INT)
!!!                         SEC = SEC - LIM_REL64
!!!    END DO
!!!    !
!!!    S = INT(SEC, C_INT)
!!!    U = C_SLEEP(S)
!!!  END SUBROUTINE
!!!  !
!!!END MODULE

    
    
    
    
  !!!   Very inaccurate for sleeping program
  !!!INTEGER(INT32), PARAMETER:: Z   =  0_INT32                                                         !
  !!!INTEGER(INT32), PARAMETER:: ONE =  1_INT32                                                         !
  !!!INTEGER(INT32), PARAMETER:: NEG = -1_INT32                                                         !
  !!!                                                                                                   !
  !!!INTEGER(INT32), PARAMETER:: i4  =  4_INT32                                                         !
  !!!INTEGER(INT32), PARAMETER:: i100=  100_INT32                                                       !
  !!!INTEGER(INT32), PARAMETER:: i400=  400_INT32                                                       !
  !!!                                                                                                   !
  !!!REAL   (REL64), PARAMETER:: msec2day = 1.1574074074074074074e-8_REL64  ! ~ 1 milisecond in days    !
  !!!REAL   (REL64), PARAMETER::  sec2day = 1.1574074074074074074e-5_REL64  ! ~ 1 second in days        !
  !!!REAL   (REL64), PARAMETER::  min2day = 6.9444444444444444444e-4_REL64  ! ~ 1 minute in days        !
  !!!REAL   (REL64), PARAMETER::   hr2day = 0.0416666666666666666666_REL64  ! ~ 1 hour   in days        !
  !!!!                                                                                                  !
  !!!REAL   (REL64), PARAMETER:: d365 = 365_REL64                                                       !
  !!!REAL   (REL64), PARAMETER:: d366 = 366_REL64                                                       !
  !!!!
  !!!!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !!!!#####################################################################################################
  !!!!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !!!!
  !!!SUBROUTINE SLEEP_INT32(SEC)
  !!!  INTEGER(INT32), INTENT(IN):: SEC
  !!!  REAL   (REL64):: JDN_STOP, JDN
  !!!  INTEGER(INT32)::  YR_STOP, YR
  !!!  !
  !!!  CALL GET_SYSTEM_DATE_AND_TIME(JDN_STOP, YR_STOP)
  !!!  !
  !!!  JDN_STOP = JDN_STOP + REAL(SEC, REL64)*sec2day
  !!!  !
  !!!  IF( JDN_STOP > d365) THEN
  !!!      !
  !!!      IF( (MOD(YR_STOP, i4) == Z .AND. MOD(YR_STOP, i100) /= Z) .OR. MOD(YR_STOP, i400) == Z ) THEN
  !!!          IF( JDN_STOP > d366) THEN
  !!!            DO WHILE (JDN_STOP > d366)
  !!!                      YR_STOP = YR_STOP + ONE
  !!!                      JDN_STOP = JDN_STOP - d366
  !!!            END DO
  !!!          END IF
  !!!      ELSE
  !!!          DO WHILE (JDN_STOP > d365)
  !!!                    YR_STOP = YR_STOP + ONE
  !!!                    JDN_STOP = JDN_STOP - d365
  !!!          END DO
  !!!      END IF
  !!!  END IF
  !!!  !
  !!!  CALL GET_SYSTEM_DATE_AND_TIME(JDN, YR)
  !!!  !
  !!!  DO WHILE (JDN < JDN_STOP .AND. YR <= YR_STOP)
  !!!                                 CALL GET_SYSTEM_DATE_AND_TIME(JDN, YR)
  !!!  END DO
  !!!  !
  !!!END SUBROUTINE
  !!!!
  !!!SUBROUTINE SLEEP_INT64(SEC)
  !!!  INTEGER(INT64), INTENT(IN):: SEC
  !!!  REAL   (REL64):: JDN_STOP, JDN
  !!!  INTEGER(INT32)::  YR_STOP, YR
  !!!  !
  !!!  CALL GET_SYSTEM_DATE_AND_TIME(JDN_STOP, YR_STOP)
  !!!  !
  !!!  JDN_STOP = JDN_STOP + REAL(SEC, REL64)*sec2day
  !!!  !
  !!!  IF( JDN_STOP > d365) THEN
  !!!      !
  !!!      IF( (MOD(YR_STOP, i4) == Z .AND. MOD(YR_STOP, i100) /= Z) .OR. MOD(YR_STOP, i400) == Z ) THEN
  !!!          IF( JDN_STOP > d366) THEN
  !!!            DO WHILE (JDN_STOP > d366)
  !!!                      YR_STOP = YR_STOP + ONE
  !!!                      JDN_STOP = JDN_STOP - d366
  !!!            END DO
  !!!          END IF
  !!!      ELSE
  !!!          DO WHILE (JDN_STOP > d365)
  !!!                    YR_STOP = YR_STOP + ONE
  !!!                    JDN_STOP = JDN_STOP - d365
  !!!          END DO
  !!!      END IF
  !!!  END IF
  !!!  !
  !!!  CALL GET_SYSTEM_DATE_AND_TIME(JDN, YR)
  !!!  !
  !!!  DO WHILE (JDN < JDN_STOP .AND. YR <= YR_STOP)
  !!!                                 CALL GET_SYSTEM_DATE_AND_TIME(JDN, YR)
  !!!  END DO
  !!!  !
  !!!END SUBROUTINE
  !!!!
  !!!SUBROUTINE SLEEP_REL32(SEC)
  !!!  REAL(REL32), INTENT(IN):: SEC
  !!!  REAL   (REL64):: JDN_STOP, JDN
  !!!  INTEGER(INT32)::  YR_STOP, YR
  !!!  !
  !!!  CALL GET_SYSTEM_DATE_AND_TIME(JDN_STOP, YR_STOP)
  !!!  !
  !!!  JDN_STOP = JDN_STOP + REAL(SEC, REL64)*sec2day
  !!!  !
  !!!  IF( JDN_STOP > d365) THEN
  !!!      !
  !!!      IF( (MOD(YR_STOP, i4) == Z .AND. MOD(YR_STOP, i100) /= Z) .OR. MOD(YR_STOP, i400) == Z ) THEN
  !!!          IF( JDN_STOP > d366) THEN
  !!!            DO WHILE (JDN_STOP > d366)
  !!!                      YR_STOP = YR_STOP + ONE
  !!!                      JDN_STOP = JDN_STOP - d366
  !!!            END DO
  !!!          END IF
  !!!      ELSE
  !!!          DO WHILE (JDN_STOP > d365)
  !!!                    YR_STOP = YR_STOP + ONE
  !!!                    JDN_STOP = JDN_STOP - d365
  !!!          END DO
  !!!      END IF
  !!!  END IF
  !!!  !
  !!!  CALL GET_SYSTEM_DATE_AND_TIME(JDN, YR)
  !!!  !
  !!!  DO WHILE (JDN < JDN_STOP .AND. YR <= YR_STOP)
  !!!                                 CALL GET_SYSTEM_DATE_AND_TIME(JDN, YR)
  !!!  END DO
  !!!  !
  !!!END SUBROUTINE
  !!!!
  !!!SUBROUTINE SLEEP_REL64(SEC)
  !!!  REAL(REL64), INTENT(IN):: SEC
  !!!  REAL   (REL64):: JDN_STOP, JDN
  !!!  INTEGER(INT32)::  YR_STOP, YR
  !!!  !
  !!!  CALL GET_SYSTEM_DATE_AND_TIME(JDN_STOP, YR_STOP)
  !!!  !
  !!!  JDN_STOP = JDN_STOP + SEC*sec2day
  !!!  !
  !!!  IF( JDN_STOP > d365) THEN
  !!!      !
  !!!      IF( (MOD(YR_STOP, i4) == Z .AND. MOD(YR_STOP, i100) /= Z) .OR. MOD(YR_STOP, i400) == Z ) THEN
  !!!          IF( JDN_STOP > d366) THEN
  !!!            DO WHILE (JDN_STOP > d366)
  !!!                      YR_STOP = YR_STOP + ONE
  !!!                      JDN_STOP = JDN_STOP - d366
  !!!            END DO
  !!!          END IF
  !!!      ELSE
  !!!          DO WHILE (JDN_STOP > d365)
  !!!                    YR_STOP = YR_STOP + ONE
  !!!                    JDN_STOP = JDN_STOP - d365
  !!!          END DO
  !!!      END IF
  !!!  END IF
  !!!  !
  !!!  CALL GET_SYSTEM_DATE_AND_TIME(JDN, YR)
  !!!  !
  !!!  DO WHILE (JDN < JDN_STOP .AND. YR <= YR_STOP)
  !!!                                 CALL GET_SYSTEM_DATE_AND_TIME(JDN, YR)
  !!!  END DO
  !!!  !
  !!!END SUBROUTINE
  !!!!
  !!!!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !!!!#####################################################################################################
  !!!!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !!!!
  !!!SUBROUTINE GET_SYSTEM_DATE_AND_TIME(JDN, YR)
  !!!  REAL   (REL64), INTENT(INOUT) :: JDN
  !!!  INTEGER(INT32), INTENT(INOUT) :: YR 
  !!!  INTEGER(INT32),  DIMENSION(8) :: DT  !Used by CALL DATE_AND_TIME to get current time stamp
  !!!  !
  !!!  CALL DATE_AND_TIME(VALUES = DT)
  !!!  !                YEAR, MONTH,   DAY,   HR,    MIN,   SEC,  MSEC
  !!!  JDN = JULIANDAY(DT(1), DT(2), DT(3), DT(5), DT(6), DT(7), DT(8))
  !!!  YR  = DT(1)
  !!!  !
  !!!END SUBROUTINE
  !!!!
  !!!!--------------------------------------------------------------------------------------------------------------------------
  !!!!
  !!!PURE FUNCTION JULIANDAY(YEAR, MONTH, DAY, HR, MIN, SEC, MSEC)   ! Function sets 1/1/YYYY to have a JDN = 0
  !!!  !
  !!!  INTEGER(INT32), INTENT(IN):: DAY, MONTH, YEAR, HR, MIN, SEC, MSEC
  !!!  REAL(REL64):: JULIANDAY
  !!!  !
  !!!  IF    (MONTH ==  1_INT32) THEN
  !!!                          JULIANDAY = 0_REL64
  !!!  ELSEIF(MONTH ==  2_INT32) THEN
  !!!                          JULIANDAY = 31_REL64
  !!!  ELSEIF(MONTH ==  3_INT32) THEN
  !!!                          JULIANDAY = 59_REL64
  !!!  ELSEIF(MONTH ==  4_INT32) THEN
  !!!                          JULIANDAY = 90_REL64
  !!!  ELSEIF(MONTH ==  5_INT32) THEN
  !!!                          JULIANDAY = 120_REL64
  !!!  ELSEIF(MONTH ==  6_INT32) THEN
  !!!                          JULIANDAY = 151_REL64
  !!!  ELSEIF(MONTH ==  7_INT32) THEN
  !!!                          JULIANDAY = 181_REL64
  !!!  ELSEIF(MONTH ==  8_INT32) THEN
  !!!                          JULIANDAY = 212_REL64
  !!!  ELSEIF(MONTH ==  9_INT32) THEN
  !!!                          JULIANDAY = 243_REL64
  !!!  ELSEIF(MONTH == 10_INT32) THEN
  !!!                          JULIANDAY = 273_REL64
  !!!  ELSEIF(MONTH == 11_INT32) THEN
  !!!                          JULIANDAY = 304_REL64
  !!!  ELSE!IF(MONTH == 12_INT32) THEN
  !!!                          JULIANDAY = 334_REL64
  !!!  END IF
  !!!  !
  !!!  IF( MONTH > 2_INT32 ) THEN ! Leap year if year is divisable by 4 and not 100 or if year is divisable by 400 -> If leap and > Feb add 1 day
  !!!      !
  !!!      IF( (MOD(YEAR, i4) == Z .AND. MOD(YEAR, i100) /= Z) .OR. MOD(YEAR, i400) == Z ) JULIANDAY = JULIANDAY + 1_REL64
  !!!  END IF
  !!!  !
  !!!  JULIANDAY = JULIANDAY + REAL( DAY + NEG, REL64) + REAL( HR, REL64)*HR2DAY + REAL( MIN, REL64)*MIN2DAY + REAL( SEC, REL64)*SEC2DAY + REAL( MSEC, REL64)*MSEC2DAY
  !!!  !
  !!!END FUNCTION