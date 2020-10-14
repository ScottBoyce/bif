!
! CODE DEVELOPED BY SCOTT E BOYCE 
!                   CONTACT <seboyce@usgs.gov> or <Boyce@engineer.com>
!
!
MODULE INTEGER_QUEUE_INSTRUCTION
  IMPLICIT NONE
  !
  PUBLIC:: INTEGER_QUEUE
  PRIVATE
  !
  TYPE QUEUE
      INTEGER:: I
      TYPE(QUEUE), ALLOCATABLE:: NXT
  END TYPE
  !
  TYPE INTEGER_QUEUE
      INTEGER:: SIZ = 0
      TYPE(QUEUE):: DAT
      TYPE(QUEUE), POINTER:: CUR => NULL()
      TYPE(QUEUE), POINTER:: STP => NULL()
      !
      CONTAINS
      !
      PROCEDURE, PASS(P):: INIT     !([IVAL]) - WILL RESET QUEUE
      PROCEDURE, PASS(P):: PUSH     !(IVAL)
      PROCEDURE, PASS(P):: PULL     !()     - FUNCTION
      PROCEDURE, PASS(P):: PEEK     !()     - FUNCTION
      PROCEDURE, PASS(P):: PEEK_MIN !(P2)   - FUNCTION
      PROCEDURE, PASS(P):: SIZE     !()     - FUNCTION
      FINAL:: DEALLOCATE_QUEUE
  END TYPE
  !
  CONTAINS
  !
  SUBROUTINE INIT(P, IVAL)                 !Optional to call - Defaults will auto-append - Can use to reset size
    CLASS(INTEGER_QUEUE), INTENT(INOUT):: P
    INTEGER,   OPTIONAL, INTENT(IN   ):: IVAL
    !
    IF(PRESENT(IVAL)) THEN
                      P%SIZ      = 1
                      P%DAT%I    = IVAL
                      CALL SET_PNT(P%CUR,P%DAT)
                      CALL SET_PNT(P%STP,P%DAT)
    ELSE
                      P%SIZ      = 0
    END IF
  END SUBROUTINE
  !
  SUBROUTINE PUSH(P, IVAL)
    CLASS(INTEGER_QUEUE), INTENT(INOUT):: P
    INTEGER,                    INTENT(IN   ):: IVAL
    !
    IF( P%SIZ < 1 ) THEN
        P%SIZ   = 1
        P%DAT%I = IVAL
        CALL SET_PNT(P%CUR,P%DAT)
        CALL SET_PNT(P%STP,P%DAT)
    ELSE
        IF(.NOT. ALLOCATED(P%STP%NXT)) ALLOCATE(P%STP%NXT)
        !
        P%SIZ   =  P%SIZ + 1
        P%STP   => P%STP%NXT
        P%STP%I =  IVAL
    END IF
  END SUBROUTINE
  !
  FUNCTION PULL(P) RESULT(VAL)
    CLASS(INTEGER_QUEUE), INTENT(INOUT):: P
    INTEGER:: VAL
    IF( P%SIZ < 1 ) THEN
        VAL = HUGE(0)
    ELSE
        P%SIZ = P%SIZ - 1
        VAL   = P%CUR%I
        IF(P%SIZ > 0) P%CUR => P%CUR%NXT
    END IF
  END FUNCTION
  !
  FUNCTION PEEK(P) RESULT(VAL)
    CLASS(INTEGER_QUEUE), INTENT(INOUT):: P
    INTEGER:: VAL
    IF( P%SIZ < 1 ) THEN
        VAL = HUGE(0)
    ELSE
        VAL   = P%CUR%I
    END IF
  END FUNCTION
  !
  FUNCTION SIZE(P) RESULT(SIZ)
    CLASS(INTEGER_QUEUE), INTENT(INOUT):: P
    INTEGER:: SIZ
    SIZ = P%SIZ
  END FUNCTION
  !
  SUBROUTINE DEALLOCATE_QUEUE(P)
    TYPE(INTEGER_QUEUE), INTENT(INOUT):: P
    !
    P%SIZ =  0
    P%CUR => NULL()
    P%STP => NULL()
    !
    IF(ALLOCATED(P%DAT%NXT)) DEALLOCATE(P%DAT%NXT)
    !
  END SUBROUTINE
  !
  FUNCTION PEEK_MIN(P,P2) RESULT(VAL)
    CLASS(INTEGER_QUEUE), INTENT(IN):: P,P2
    INTEGER:: VAL
    IF( P%SIZ < 1 .AND. P2%SIZ < 1) THEN
                                        VAL = HUGE(0)
    ELSEIF( P%SIZ > 0 .AND. P2%SIZ < 1) THEN
                                        VAL = P%CUR%I
    ELSEIF( P%SIZ < 1 .AND. P2%SIZ > 0) THEN
                                        VAL = P2%CUR%I        
    ELSE
        IF( P%CUR%I < P2%CUR%I ) THEN
            VAL = P%CUR%I
        ELSE
            VAL = P2%CUR%I 
        END IF
    END IF
  END FUNCTION
  !
  SUBROUTINE SET_PNT(PNT, TARG)
    TYPE(QUEUE), POINTER, INTENT(INOUT):: PNT
    TYPE(QUEUE), TARGET , INTENT(IN   ):: TARG
    PNT => TARG
  END SUBROUTINE
  !
END MODULE
!
!