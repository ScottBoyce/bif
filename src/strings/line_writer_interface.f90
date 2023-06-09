MODULE LINE_WRITER_INTERFACE!, ONLY: LINE_WRITER
  USE, INTRINSIC:: ISO_FORTRAN_ENV, ONLY: stdout => OUTPUT_UNIT, &
                                             SNG => REAL32, DBL => REAL64
  USE CONSTANTS,         ONLY: NEG, Z, ONE, TWO, BLNK, TRUE, FALSE, NL, NO
  USE NUM2STR_INTERFACE, ONLY: NUM2STR
  USE ARRAY_DATA_TYPES, ONLY: CHARACTER_BUF_TYPE
  IMPLICIT NONE
  !
  PUBLIC:: LINE_WRITER
  PRIVATE
  !
  !Build a line by specifying the position and value to write.
  !
  ! P is the position in the line that the word is placed  -Word is either W or NUM
  !  If negative, then the end of the word is placed at P
  !
  !
  TYPE, EXTENDS(CHARACTER_BUF_TYPE):: LINE_WRITER
      INTEGER:: IU     = Z                        !If set to zero on init then writes to command prompt
      INTEGER:: BS     = Z                        !Last Line Index (Back Space)
      LOGICAL:: BINARY = FALSE
      !
      CONTAINS
      !
      PROCEDURE, PASS(LW):: INIT     => INIT_LINE_WRITER       !(IU)
      GENERIC::             WRITE    => WRITE_P_W_LINE_WRITER, WRITE_P_W_LINE_WRITER_INT, WRITE_P_W_LINE_WRITER_DBL, WRITE_P_W_LINE_WRITER_SNG, WRITE_P_W_LINE_WRITER_DBL_FMT, WRITE_P_W_LINE_WRITER_SNG_FMT, WRITE_P_W_LINE_WRITER_INT_FMT
      GENERIC::             MWRITE=> WRITE_LINE_WRITER         !Calls WRITE_P_W_LINE_WRITER
      PROCEDURE, PASS(LW):: BACKSPACE=> BACKSPACE_LINE_WRITER
      PROCEDURE, PASS(LW):: RESET    => RESET_LINE_WRITER      !()
      PROCEDURE, PASS(LW):: FLUSH    => FLUSH_LINE_WRITER      !()
      PROCEDURE, PASS(CH):: DESTROY  => DESTROY_LINE_WRITER    !()
      PROCEDURE, PASS(LW):: NEW_LINE => NEW_LINE_TO_LINE_WRITER!()  !Also %NL() works
      PROCEDURE, PRIVATE, PASS(LW):: WRITE_LINE_WRITER         !(CR, [P, W])  [P,W] can be repeated
      PROCEDURE, PRIVATE, PASS(LW):: WRITE_P_W_LINE_WRITER     !(P, W, [CR])
      PROCEDURE, PRIVATE, PASS(LW):: WRITE_P_W_LINE_WRITER_DBL !(P, NUM, [CR])
      PROCEDURE, PRIVATE, PASS(LW):: WRITE_P_W_LINE_WRITER_SNG !(P, NUM, [CR])
      PROCEDURE, PRIVATE, PASS(LW):: WRITE_P_W_LINE_WRITER_INT !(P, NUM, [CR])
      PROCEDURE, PRIVATE, PASS(LW):: WRITE_P_W_LINE_WRITER_DBL_FMT !(P, NUM, FMT, [CR])
      PROCEDURE, PRIVATE, PASS(LW):: WRITE_P_W_LINE_WRITER_SNG_FMT !(P, NUM, FMT, [CR])
      PROCEDURE, PRIVATE, PASS(LW):: WRITE_P_W_LINE_WRITER_INT_FMT !(P, NUM, FMT, [CR])
  END TYPE
  !
  CONTAINS
  !
  IMPURE ELEMENTAL SUBROUTINE INIT_LINE_WRITER(LW, IU)
    CLASS(LINE_WRITER), INTENT(INOUT):: LW
    INTEGER,            INTENT(IN   ):: IU
    CHARACTER(12):: FORM_CHK
    !
    IF( IU == Z ) THEN
        LW%BINARY = FALSE
        LW%IU     = stdout
    ELSE
        INQUIRE(IU,FORM=FORM_CHK)
        LW%BINARY = FORM_CHK .NE. 'FORMATTED'
        LW%IU     = IU
    END IF
    !
    LW%N  = Z
    LW%BS = Z
    CALL LW%SET_SIZE(128)
  END SUBROUTINE
  !
  PURE ELEMENTAL SUBROUTINE WRITE_P_W_LINE_WRITER(LW, P, W, CR)
    CLASS(LINE_WRITER),     INTENT(INOUT):: LW
    INTEGER,                INTENT(IN   ):: P
    CHARACTER(*),           INTENT(IN   ):: W
    LOGICAL,      OPTIONAL, INTENT(IN   ):: CR
    INTEGER:: N
    !
    IF (LW%N == Z) THEN
        IF (P > Z) THEN
            CALL LW%SPACE(P-1)
            CALL LW%ADD(W)
        ELSE
            N = -1*(LEN(W) + P)   !Equiv to: ABS(P) - LEN(W) or N = -1*P - LEN(W)
            CALL LW%SPACE(N)
            CALL LW%ADD(W)
        END IF
    ELSE
        IF (P > Z) THEN
            N = P - (LW%N - LW%BS) - ONE
            CALL LW%SPACE(N)
            CALL LW%ADD(W)
        ELSE
            N = -1*P - (LW%N - LW%BS) - LEN(W)
            CALL LW%SPACE(N)
            CALL LW%ADD(W)
        END IF
    END IF
    !
    IF(PRESENT(CR)) THEN
        IF(CR) THEN
               CALL LW%NL()
               LW%BS = LW%N
        END IF
    END IF
    !
  END SUBROUTINE
  !
  PURE ELEMENTAL SUBROUTINE BACKSPACE_LINE_WRITER(LW)
    CLASS(LINE_WRITER),     INTENT(INOUT):: LW
    INTEGER:: I, N
    !
    LW%N = LW%BS
    LW%BS = Z
    !
    N = LW%N - ONE
    DO I=1, N
           IF (LW%STR(I:I) == NL) LW%BS = I
    END DO
    !
  END SUBROUTINE
  !
  !PURE ELEMENTAL SUBROUTINE FIND_BS_LINE_WRITER(LW)
  !  CLASS(LINE_WRITER),     INTENT(INOUT):: LW
  !  INTEGER:: I, N
  !  !
  !  LW%BS = Z
  !  N = LW%N
  !  IF (LW%STR(N:N) == NL) N = N - ONE
  !  !
  !  DO I=1, N
  !         IF (LW%STR(I:I) == NL) LW%BS = I
  !  END DO
  !  !
  !END SUBROUTINE
  !
  IMPURE ELEMENTAL SUBROUTINE FLUSH_LINE_WRITER(LW,CR) !Assumes that INIT has been run to set the LW%IU
    CLASS(LINE_WRITER),     INTENT(INOUT):: LW
    LOGICAL,      OPTIONAL, INTENT(IN   ):: CR
    !
    IF(PRESENT(CR)) THEN
            IF(CR) CALL LW%NL()
    END IF
    !
    IF(LW%BINARY) THEN
        WRITE(LW%IU) LW%STR(1:LW%N)
    ELSE
        WRITE(LW%IU, "(A)", ADVANCE=NO) LW%STR(1:LW%N)
    END IF
    !
    LW%N  = Z
    LW%BS = Z
    !
  END SUBROUTINE
  !
  PURE ELEMENTAL SUBROUTINE WRITE_P_W_LINE_WRITER_INT(LW, P, NUM, CR)
    CLASS(LINE_WRITER),        INTENT(INOUT):: LW
    INTEGER,                   INTENT(IN   ):: P
    INTEGER,                   INTENT(IN   ):: NUM
    LOGICAL,      OPTIONAL,    INTENT(IN   ):: CR
    CHARACTER(:), ALLOCATABLE:: W
    !
    W = NUM2STR(NUM)
    !
    CALL WRITE_P_W_LINE_WRITER(LW, P, W, CR)
    !
  END SUBROUTINE
  !
  IMPURE ELEMENTAL SUBROUTINE WRITE_P_W_LINE_WRITER_DBL(LW, P, NUM, CR)
    CLASS(LINE_WRITER),        INTENT(INOUT):: LW
    INTEGER,                   INTENT(IN   ):: P
    REAL(DBL),                 INTENT(IN   ):: NUM
    LOGICAL,      OPTIONAL,    INTENT(IN   ):: CR
    CHARACTER(:), ALLOCATABLE:: W
    !
    W = NUM2STR(NUM)
    !
    CALL WRITE_P_W_LINE_WRITER(LW, P, W, CR)
    !
  END SUBROUTINE
  !
  IMPURE ELEMENTAL SUBROUTINE WRITE_P_W_LINE_WRITER_SNG(LW, P, NUM, CR)
    CLASS(LINE_WRITER),        INTENT(INOUT):: LW
    INTEGER,                   INTENT(IN   ):: P
    REAL(SNG),                 INTENT(IN   ):: NUM
    LOGICAL,      OPTIONAL,    INTENT(IN   ):: CR
    CHARACTER(:), ALLOCATABLE:: W
    !
    W = NUM2STR(NUM)
    !
    CALL WRITE_P_W_LINE_WRITER(LW, P, W, CR)
    !
  END SUBROUTINE
  !
  PURE ELEMENTAL SUBROUTINE WRITE_LINE_WRITER(LW, CR, P, W,                                            &
                                              P1,  W1,  P2,  W2,  P3,  W3,  P4,  W4,  P5,  W5, &
                                              P6,  W6,  P7,  W7,  P8,  W8,  P9,  W9, P10, W10, &
                                             P11, W11, P12, W12, P13, W13, P14, W14, P15, W15, &
                                             P16, W16, P17, W17, P18, W18, P19, W19, P20, W20, &
                                             P21, W21, P22, W22, P23, W23, P24, W24)
    CLASS(LINE_WRITER),     INTENT(INOUT):: LW
    LOGICAL,                INTENT(IN   ):: CR
    INTEGER,      OPTIONAL, INTENT(IN   ):: P, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20, P21, P22, P23, P24
    CHARACTER(*), OPTIONAL, INTENT(IN   ):: W, W1, W2, W3, W4, W5, W6, W7, W8, W9, W10, W11, W12, W13, W14, W15, W16, W17, W18, W19, W20, W21, W22, W23, W24
    !
    IF(PRESENT(P  )) CALL WRITE_P_W_LINE_WRITER(LW, P, W)
    !
    IF(PRESENT(P1 )) CALL WRITE_P_W_LINE_WRITER(LW, P1 , W1 )
    IF(PRESENT(P2 )) CALL WRITE_P_W_LINE_WRITER(LW, P2 , W2 )
    IF(PRESENT(P3 )) CALL WRITE_P_W_LINE_WRITER(LW, P3 , W3 )
    IF(PRESENT(P4 )) CALL WRITE_P_W_LINE_WRITER(LW, P4 , W4 )
    IF(PRESENT(P5 )) CALL WRITE_P_W_LINE_WRITER(LW, P5 , W5 )
    IF(PRESENT(P6 )) CALL WRITE_P_W_LINE_WRITER(LW, P6 , W6 )
    IF(PRESENT(P7 )) CALL WRITE_P_W_LINE_WRITER(LW, P7 , W7 )
    IF(PRESENT(P8 )) CALL WRITE_P_W_LINE_WRITER(LW, P8 , W8 )
    IF(PRESENT(P9 )) CALL WRITE_P_W_LINE_WRITER(LW, P9 , W9 )
    IF(PRESENT(P10)) CALL WRITE_P_W_LINE_WRITER(LW, P10, W10)
    IF(PRESENT(P11)) CALL WRITE_P_W_LINE_WRITER(LW, P11, W11)
    IF(PRESENT(P12)) CALL WRITE_P_W_LINE_WRITER(LW, P12, W12)
    IF(PRESENT(P13)) CALL WRITE_P_W_LINE_WRITER(LW, P13, W13)
    IF(PRESENT(P14)) CALL WRITE_P_W_LINE_WRITER(LW, P14, W14)
    IF(PRESENT(P15)) CALL WRITE_P_W_LINE_WRITER(LW, P15, W15)
    IF(PRESENT(P16)) CALL WRITE_P_W_LINE_WRITER(LW, P16, W16)
    IF(PRESENT(P17)) CALL WRITE_P_W_LINE_WRITER(LW, P17, W17)
    IF(PRESENT(P18)) CALL WRITE_P_W_LINE_WRITER(LW, P18, W18)
    IF(PRESENT(P19)) CALL WRITE_P_W_LINE_WRITER(LW, P19, W19)
    IF(PRESENT(P20)) CALL WRITE_P_W_LINE_WRITER(LW, P20, W20)
    IF(PRESENT(P21)) CALL WRITE_P_W_LINE_WRITER(LW, P21, W21)
    IF(PRESENT(P22)) CALL WRITE_P_W_LINE_WRITER(LW, P22, W22)
    IF(PRESENT(P23)) CALL WRITE_P_W_LINE_WRITER(LW, P23, W23)
    IF(PRESENT(P24)) CALL WRITE_P_W_LINE_WRITER(LW, P24, W24)
    !
    IF(CR) CALL LW%NL()
    !
  END SUBROUTINE
  !
  SUBROUTINE WRITE_P_W_LINE_WRITER_INT_FMT(LW, P, NUM, FMT, CR)
    CLASS(LINE_WRITER),        INTENT(INOUT):: LW
    INTEGER,                   INTENT(IN   ):: P
    INTEGER,                   INTENT(IN   ):: NUM
    CHARACTER(*),              INTENT(IN   ):: FMT
    LOGICAL,      OPTIONAL,    INTENT(IN   ):: CR
    CHARACTER(64), ALLOCATABLE:: W
    INTEGER:: I
    !
    WRITE(W,FMT) NUM
    W = ADJUSTL(W)
    I = LEN_TRIM(W)
    !
    CALL WRITE_P_W_LINE_WRITER(LW, P, W(:I), CR)
    !
  END SUBROUTINE
  !
  SUBROUTINE WRITE_P_W_LINE_WRITER_DBL_FMT(LW, P, NUM, FMT, CR)
    CLASS(LINE_WRITER),        INTENT(INOUT):: LW
    INTEGER,                   INTENT(IN   ):: P
    REAL(DBL),                 INTENT(IN   ):: NUM
    CHARACTER(*),              INTENT(IN   ):: FMT
    LOGICAL,      OPTIONAL,    INTENT(IN   ):: CR
    CHARACTER(64), ALLOCATABLE:: W
    INTEGER:: I
    !
    WRITE(W,FMT) NUM
    W = ADJUSTL(W)
    I = LEN_TRIM(W)
    !
    CALL WRITE_P_W_LINE_WRITER(LW, P, W(:I), CR)
    !
  END SUBROUTINE
  !
  SUBROUTINE WRITE_P_W_LINE_WRITER_SNG_FMT(LW, P, NUM, FMT, CR)
    CLASS(LINE_WRITER),        INTENT(INOUT):: LW
    INTEGER,                   INTENT(IN   ):: P
    REAL(SNG),                 INTENT(IN   ):: NUM
    CHARACTER(*),              INTENT(IN   ):: FMT
    LOGICAL,      OPTIONAL,    INTENT(IN   ):: CR
    CHARACTER(64), ALLOCATABLE:: W
    INTEGER:: I
    !
    WRITE(W,FMT) NUM
    W = ADJUSTL(W)
    I = LEN_TRIM(W)
    !
    CALL WRITE_P_W_LINE_WRITER(LW, P, W(:I), CR)
    !
  END SUBROUTINE
  !
  PURE ELEMENTAL SUBROUTINE NEW_LINE_TO_LINE_WRITER(LW)
    CLASS(LINE_WRITER), INTENT(INOUT):: LW
    !
    CALL LW%NL()
    LW%BS = LW%N
    !
  END SUBROUTINE
  !
  PURE ELEMENTAL SUBROUTINE RESET_LINE_WRITER(LW)
    CLASS(LINE_WRITER), INTENT(INOUT):: LW
    !
    LW%N  = Z
    LW%BS = Z
    !
  END SUBROUTINE
  !
  PURE ELEMENTAL SUBROUTINE DESTROY_LINE_WRITER(CH)
    CLASS(LINE_WRITER), INTENT(INOUT):: CH
    CH%IU = Z
    CALL CH%CHARACTER_BUF_TYPE%DESTROY()
  END SUBROUTINE
  !
END MODULE
