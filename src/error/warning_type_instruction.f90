!
! CODE DEVELOPED BY SCOTT E BOYCE 
!                   CONTACT <seboyce@usgs.gov> or <Boyce@engineer.com>
!
!
MODULE WARNING_TYPE_INSTRUCTION!, ONLY: WARNING_TYPE
  !
  USE CONSTANTS,        ONLY: TRUE, FALSE, NL, Z, ONE, TWO, QUIN
  USE ERROR_INTERFACE,  ONLY: STOP_ERROR, WARNING_MESSAGE
  IMPLICIT NONE
  PRIVATE
  PUBLIC:: WARNING_TYPE, WARNING_TYPE_DEPOINT    ! XX_DEPOINT only necessary as workaround for gfortran compiler error
  !
  TYPE:: WARNING_TYPE
      LOGICAL:: RAISED  = FALSE
      INTEGER:: MAX_ADD = QUIN                    !Note limit of 500 message adds
      INTEGER:: CNT     = Z
      !
      CHARACTER(:), ALLOCATABLE::STR
      CONTAINS
      PROCEDURE, PASS(WRN):: INIT  => INITIALIZE_WARNING_TYPE !([SET_STR], [MAX])
      PROCEDURE, PASS(WRN):: ADD   => ADD_WARNING_MESSAGE     !(LINE)
      PROCEDURE, PASS(WRN):: CHECK => CHECK_IF_RAISE_WARNING  !(HED, INFILE, OUTPUT, INLINE, CMD_PRINT, KILL, TAIL, NO_NL, INIT, SET_STR)  <-- all optional
      FINAL:: FINAL_WARNING_TYPE
  END TYPE
  !
  CONTAINS
  !
  ELEMENTAL PURE SUBROUTINE INITIALIZE_WARNING_TYPE(WRN, SET_STR, MAX)
    CLASS(WARNING_TYPE),    INTENT(INOUT):: WRN
    CHARACTER(*), OPTIONAL, INTENT(IN   ):: SET_STR
    INTEGER,      OPTIONAL, INTENT(IN   ):: MAX
    !
    WRN%CNT = Z
    !
    WRN%RAISED=FALSE
    !
    IF(ALLOCATED(WRN%STR)) DEALLOCATE(WRN%STR)
    !
    IF(PRESENT(SET_STR)) THEN;  ALLOCATE(WRN%STR, SOURCE=SET_STR)
    ELSE;                       ALLOCATE(WRN%STR, SOURCE=NL     )
    END IF
    !
    IF(PRESENT(MAX)) THEN
        WRN%MAX_ADD = MAX
    ELSE
        WRN%MAX_ADD = QUIN
    END IF
    !
    WRN%CNT = Z
    !
  END SUBROUTINE
  !
  ELEMENTAL PURE SUBROUTINE ADD_WARNING_MESSAGE(WRN,LINE)
    CLASS(WARNING_TYPE),   INTENT(INOUT):: WRN
    CHARACTER(*),          INTENT(IN   ):: LINE
    !
    WRN%RAISED=TRUE
    !
    WRN%CNT = WRN%CNT + ONE
    !
    IF(WRN%CNT <= WRN%MAX_ADD .AND. LEN(WRN%STR) < 50000)  THEN
        IF(ALLOCATED(WRN%STR)) THEN
                                   WRN%STR = WRN%STR//LINE
        ELSE
                          ALLOCATE(WRN%STR, SOURCE=LINE)
        END IF
    END IF
    !
  END SUBROUTINE
  !
  SUBROUTINE CHECK_IF_RAISE_WARNING(WRN, HED, INFILE, OUTPUT, INLINE, CMD_PRINT, KILL, TAIL, NO_NL, INIT, SET_STR)
    CLASS(WARNING_TYPE),    INTENT(INOUT):: WRN
    CHARACTER(*), OPTIONAL, INTENT(IN   ):: HED
    CHARACTER(*), OPTIONAL, INTENT(IN   ):: TAIL
    INTEGER, OPTIONAL,      INTENT(IN   ):: OUTPUT
    INTEGER, OPTIONAL,      INTENT(IN   ):: INFILE
    LOGICAL, OPTIONAL,      INTENT(IN   ):: INLINE       !IF TRUE, WARNING IS  WRITTEN TO ONE LINE
    LOGICAL, OPTIONAL,      INTENT(IN   ):: CMD_PRINT    !IF TRUE, WARNING IS  WRITTEN TO CMD PROMPT
    LOGICAL, OPTIONAL,      INTENT(IN   ):: KILL         !IF TRUE, WARNING IS CANGED TO STOP PROGRAM
    LOGICAL, OPTIONAL,      INTENT(IN   ):: NO_NL        !IF TRUE, THE NL AT THE START OF THE WARNING STRING IS REMOVED.
    LOGICAL, OPTIONAL,      INTENT(IN   ):: INIT
    CHARACTER(*), OPTIONAL, INTENT(IN   ):: SET_STR
    LOGICAL:: KILL_PROG
    INTEGER:: MAX, I
    !
    IF(WRN%RAISED)  THEN
         !
         KILL_PROG = FALSE
         IF(PRESENT(KILL)) KILL_PROG = KILL
         !
         MAX = WRN%MAX_ADD
         !
         IF(WRN%CNT > WRN%MAX_ADD .OR. LEN(WRN%STR) >= 50000)  THEN
             WRN%STR = WRN%STR//NL//'   ***Note that warning message was truncated do to excessive length***'//NL
         END IF
         !
         I = ONE
         IF(PRESENT(NO_NL)) THEN
             IF(NO_NL .AND. WRN%STR(ONE:ONE)==NL .AND. LEN(WRN%STR) > ONE) I = TWO
         END IF
         !
         IF(PRESENT(HED) .AND. PRESENT(TAIL)) THEN
             IF(KILL_PROG) THEN
                                CALL STOP_ERROR(INFILE=INFILE,OUTPUT=OUTPUT,MSG=HED//NL//WRN%STR(I:)//TAIL)
             ELSE
                                CALL WARNING_MESSAGE(INFILE=INFILE,OUTPUT=OUTPUT,MSG=HED//NL//WRN%STR(I:)//TAIL, INLINE=INLINE, CMD_PRINT=CMD_PRINT)
             END IF
         ELSEIF(PRESENT(HED)) THEN
             IF(KILL_PROG) THEN
                                CALL STOP_ERROR(INFILE=INFILE,OUTPUT=OUTPUT,MSG=HED//NL//WRN%STR(I:)//NL)
             ELSE
                                CALL WARNING_MESSAGE(INFILE=INFILE,OUTPUT=OUTPUT,MSG=HED//NL//WRN%STR(I:), INLINE=INLINE, CMD_PRINT=CMD_PRINT)
             END IF
         ELSEIF(PRESENT(TAIL)) THEN
             IF(KILL_PROG) THEN
                                CALL STOP_ERROR(INFILE=INFILE,OUTPUT=OUTPUT,MSG=WRN%STR(I:)//TAIL)
             ELSE
                                CALL WARNING_MESSAGE(INFILE=INFILE,OUTPUT=OUTPUT,MSG=WRN%STR(I:)//TAIL, INLINE=INLINE, CMD_PRINT=CMD_PRINT)
             END IF
         ELSE
             IF(KILL_PROG) THEN
                                CALL STOP_ERROR(INFILE=INFILE,OUTPUT=OUTPUT,MSG=WRN%STR(I:))
             ELSE
                                CALL WARNING_MESSAGE(INFILE=INFILE,OUTPUT=OUTPUT,MSG=WRN%STR(I:), INLINE=INLINE, CMD_PRINT=CMD_PRINT)
             END IF
         END IF
         !
         IF    (PRESENT(SET_STR)) THEN
                                       CALL INITIALIZE_WARNING_TYPE(WRN,SET_STR,MAX)
         ELSEIF(PRESENT(INIT   )) THEN 
                              IF(INIT) CALL INITIALIZE_WARNING_TYPE(WRN,SET_STR,MAX)
         END IF
         !
    END IF
    !
  END SUBROUTINE
  !
  ELEMENTAL PURE SUBROUTINE FINAL_WARNING_TYPE(WRN)
    TYPE(WARNING_TYPE), INTENT(INOUT):: WRN
    !
    !!!WRN%RAISED  = FALSE  -- unnessary cause its not used again
    !!!WRN%MAX_ADD = QUIN 
    !!!WRN%CNT     = Z
    !
    IF(ALLOCATED(WRN%STR)) DEALLOCATE(WRN%STR)
    !
  END SUBROUTINE
  !
  PURE SUBROUTINE WARNING_TYPE_DEPOINT(WRN)  ! gfortran work around when DEALLOCATE(WRN) raises an internal compiler error when trying to compile the FINAL routine.
    TYPE(WARNING_TYPE),POINTER,INTENT(INOUT):: WRN
    INTEGER:: I
    !
    DEALLOCATE(WRN, STAT=I)
    NULLIFY(WRN)
    !
  END SUBROUTINE
  !
END MODULE
!