!
! CODE DEVELOPED BY SCOTT E BOYCE
!                   CONTACT <seboyce@usgs.gov> or <Boyce@engineer.com>
!
! MODULE LISTING:
!   UTIL_INTERFACE
!                           FUNCTIONS
!                                    IS_ASCII                     (LN)
!                           SUBROUTINES
!                                    ASCII_CHECK(LN, OUTPUT, [ERRMSG])
!
!
!
!
MODULE IS_ASCII_INTERFACE
  !
  USE, INTRINSIC:: ISO_FORTRAN_ENV, ONLY: INT8
  !
  USE CONSTANTS,      ONLY: ONE, BLNK, NL, BLN, TRUE, FALSE, FOUR
  USE ERROR_INTERFACE,ONLY: STOP_ERROR
  !
  IMPLICIT NONE
  !
  PRIVATE
  !
  PUBLIC:: IS_ASCII    !(LN)
  PUBLIC:: ASCII_CHECK
  !
  INTERFACE  ASCII_CHECK
    MODULE PROCEDURE ASCII_CHECK_LOGICAL!(LN, IS_ASCII)
    MODULE PROCEDURE ASCII_CHECK_STOP   !(LN, IOUT, [MSG])
    MODULE PROCEDURE ASCII_CHECK_CHAR   !(LN, NON_ASCII)
  END INTERFACE
  !
  CONTAINS
  !
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !
  !  ASCII routines
  !
  PURE ELEMENTAL FUNCTION IS_ASCII(LN)  ! Assumes ICHAR is inline with ASCII for the first 126 numbers
      CHARACTER(*),INTENT(IN):: LN
      LOGICAL:: IS_ASCII
      INTEGER::I
      !
      IS_ASCII = TRUE
      !
      DO I = ONE, LEN_TRIM(LN)
                  IF( BTEST(TRANSFER(LN(I:I), 0_INT8), 7) ) THEN  ! Check if the MSB is set (MSB = Most Significant Bit), if set then >127
                      IS_ASCII = FALSE
                      RETURN
                  END IF
      END DO
      !
  END FUNCTION
  !
  !#########################################################################################################################
  !
  PURE SUBROUTINE ASCII_CHECK_LOGICAL(LN, IS_ASCII)
      CHARACTER(*),INTENT(IN   ):: LN
      LOGICAL,     INTENT(INOUT):: IS_ASCII
      INTEGER::I
      !
      IS_ASCII = TRUE
      !
      DO I = ONE, LEN_TRIM(LN)
                  IF( BTEST(TRANSFER(LN(I:I), 0_INT8), 7) ) THEN  ! Check if the MSB is set (MSB = Most Significant Bit), if set then >127
                      IS_ASCII = FALSE
                      RETURN
                  END IF
      END DO
      !
  END SUBROUTINE
  !
  !#############################################################################################################################################################
  !
  SUBROUTINE ASCII_CHECK_STOP(LN, IOUT, MSG)
      CHARACTER(*),           INTENT(IN):: LN
      INTEGER,                INTENT(IN):: IOUT
      CHARACTER(*), OPTIONAL, INTENT(IN):: MSG  ! Supplemental messages to write in error
      INTEGER::I
      !
      DO I = ONE, LEN_TRIM(LN)
         IF( BTEST(TRANSFER(LN(I:I), 0_INT8), 7) ) CALL STOP_ERROR( LINE=LN, OUTPUT=IOUT, MSG= &
             'Found NON-ASCII character in line.'//BLN//                                       &
             'This input only allows for standard English ASCII characters'//NL//              &
             '(ASCII code range is from 0 TO 127)'//NL//                                       &
             'The character that is a problem is starts where the X is placed.'//NL//          &
             REPEAT(' ', I)//'X'//NL//                                                         &
             '"'//LN//'"'//BLN//                                                               &
             'This can happen if you copy/past from an advanced editor like MS-WORD.'//NL//    &
             'An easy fix is to rewrite your equation in an ASCII/UNICODE basic text editor.', &
             MSG2=MSG )
      END DO
      !
  END SUBROUTINE
  !
  !#############################################################################################################################################################
  !
  PURE SUBROUTINE ASCII_CHECK_CHAR(LN, NON_ASCII)
      CHARACTER(*),                        INTENT(IN   ):: LN
      CHARACTER(:),           ALLOCATABLE, INTENT(INOUT):: NON_ASCII
      LOGICAL:: NOT_ASCII
      INTEGER::I, J, N
      !
      IF(ALLOCATED(NON_ASCII)) DEALLOCATE(NON_ASCII)
      NON_ASCII = BLNK
      !
      NOT_ASCII = FALSE
      !
      N = LEN_TRIM(LN)
      I = 0
      !
      DO WHILE (I < N)
          I = I + 1
          IF( BTEST(TRANSFER(LN(I:I), 0_INT8), 7) )  THEN
              !
              NOT_ASCII = TRUE
              !
              J = UTF_BIT_HEADER(LN(I:I))
              !
              NON_ASCII = NON_ASCII//', "'//LN(I:I+J-1)//'"'
              !
              I = I + J
          END IF
      END DO
      !
      IF(NOT_ASCII) THEN
          NON_ASCII = NON_ASCII(FOUR:)
      ELSE
          DEALLOCATE(NON_ASCII)
      END IF
      !
  END SUBROUTINE
  !
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !
  PURE FUNCTION UTF_BIT_HEADER(CH) RESULT(NBIT)  !Clone of Routine from UNICODE_INTERFACE
    CHARACTER, INTENT(IN):: CH
    INTEGER:: NBIT
    INTEGER(INT8 ):: BYT
    INTEGER:: K
    !
    NBIT = 0
    BYT = TRANSFER(CH, BYT)
    !                                                                   76543210 <-POS
    IF(BYT < ONE) THEN ! If negative, then leftmost bit is set. That is 1xxxxxxx <-Negative Bit
       NBIT = 1
       DO K = 6, 0, -1
                       IF(.NOT. BTEST(BYT,K)) EXIT
                       NBIT = NBIT + ONE
       END DO
    END IF
    !
  END FUNCTION
  !
END MODULE
!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!