MODULE CONSOLE_COMMANDER  !Note you can also just write CR to reset line
  USE ISO_FORTRAN_ENV,   ONLY: stdout => OUTPUT_UNIT  
  USE ISO_C_BINDING,     ONLY: C_BACKSPACE
  IMPLICIT NONE(TYPE, EXTERNAL)
  !
  PRIVATE
  PUBLIC:: CMD_PRINT, CMD_EXTEND, CMD_CLEAR
  !
  ! Constants used internally to module ----------------------------------------------------
  ! 
  INTEGER, PARAMETER:: Z   = 0
  INTEGER, PARAMETER:: ONE = 1
  LOGICAL, PARAMETER:: FALSE = .FALSE.
  CHARACTER(2), PARAMETER:: NO = "NO"
  CHARACTER(3), PARAMETER:: YES = "YES"
  CHARACTER(3), PARAMETER:: FMT = "(A)"
  !
  ! ----------------------------------------------------------------------------------------
  ! 
  CONTAINS
  !
  SUBROUTINE CMD_PRINT(LINE, SIZ, ADVANCE)
     CHARACTER(*),      INTENT(IN   ):: LINE
     INTEGER,           INTENT(INOUT):: SIZ
     LOGICAL, OPTIONAL, INTENT(IN   ):: ADVANCE
     LOGICAL:: ADV
     ! SIZ    0  - First Write
     !       >0  - Holds size of previous write
     !
     ! ADVANCE = .TRUE. to add a new line after writing LINE and reset SIZ=0
     !
     IF(PRESENT(ADVANCE)) THEN
         ADV = ADVANCE
     ELSE
         ADV = FALSE
     END IF
     !
     IF(SIZ > Z) THEN
         WRITE(stdout, FMT, ADVANCE = NO) repeat(C_BACKSPACE, SIZ)
         FLUSH(stdout)
         WRITE(stdout, FMT, ADVANCE = NO) repeat(" ", SIZ)
         FLUSH(stdout)
         WRITE(stdout, FMT, ADVANCE = NO) repeat(C_BACKSPACE, SIZ)
         FLUSH(stdout)
     END IF
     !
     SIZ = LEN_TRIM(LINE)
     !
     IF(ADV) THEN
         WRITE(stdout, FMT, ADVANCE = YES) LINE(:SIZ)
         SIZ = Z
     ELSE
         WRITE(stdout, FMT, ADVANCE = NO) LINE(:SIZ)
     END IF
     FLUSH(stdout)
     !
  END SUBROUTINE
  !
  SUBROUTINE CMD_EXTEND(LINE, SIZ, ADVANCE)
     CHARACTER(*),      INTENT(IN   ):: LINE
     INTEGER,           INTENT(INOUT):: SIZ
     LOGICAL, OPTIONAL, INTENT(IN   ):: ADVANCE
     LOGICAL:: ADV
     INTEGER:: OLD
     ! SIZ    0  - First Write
     !       >0  - Holds size of previous write
     !
     ! ADVANCE = .TRUE. to add a new line after writing LINE and reset SIZ=0
     !
     IF(PRESENT(ADVANCE)) THEN
         ADV = ADVANCE
     ELSE
         ADV = FALSE
     END IF
     !
     OLD = SIZ
     SIZ = LEN_TRIM(LINE)
     !
     IF(ADV) THEN
         WRITE(stdout, FMT, ADVANCE = YES) LINE(:SIZ)
         SIZ = Z
     ELSE
         WRITE(stdout, FMT, ADVANCE =  NO) LINE(:SIZ)
         SIZ = SIZ + OLD
     END IF
     FLUSH(stdout)
     !
  END SUBROUTINE
  !
  SUBROUTINE CMD_CLEAR(SIZ, ADVANCE)
     INTEGER,           INTENT(INOUT):: SIZ
     LOGICAL, OPTIONAL, INTENT(IN   ):: ADVANCE
     LOGICAL:: ADV
     ! SIZ    0  - First Write
     !       >0  - Holds size of previous write
     !
     ! ADVANCE = .TRUE. to add a new line after writing LINE and reset SIZ=0
     !
     IF(PRESENT(ADVANCE)) THEN
         ADV = ADVANCE
     ELSE
         ADV = FALSE
     END IF
     !
     IF(SIZ > Z) THEN
         WRITE(stdout, FMT, ADVANCE = NO) repeat(C_BACKSPACE, SIZ)
         FLUSH(stdout)
         WRITE(stdout, FMT, ADVANCE = NO) repeat(" ", SIZ)
         FLUSH(stdout)
         WRITE(stdout, FMT, ADVANCE = NO) repeat(C_BACKSPACE, SIZ)
         FLUSH(stdout)
     END IF
     !
     IF(ADV) THEN
             WRITE(stdout, FMT) ""
             FLUSH(stdout)
     END IF
     !
     SIZ = Z
     !
  END SUBROUTINE
  !
END MODULE