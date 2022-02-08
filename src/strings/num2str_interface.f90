!
! CODE DEVELOPED BY SCOTT E BOYCE
!                   CONTACT <seboyce@usgs.gov> or <Boyce@engineer.com>
!
MODULE NUM2STR_INTERFACE!, ONLY: NUM2STR, NUM2STR7, INTFMT, NUMFMT
  !
  ! STR = NUM2STR(VAL) => convert real, integer, or logical to string.
  !                       Attempts pretty formatting, such as 3.14, is retured as '3.14' instead of '3.1400000'
  !
  ! Data Types Supported:
  !   IVEC  - Integer type, 1D Array Structure
  !   IVAL  - Integer type, scalar
  !   RVEC  - Real    type, 1D Array Structure
  !   RVAL  - Real    type, scalar
  !   LVAL  - Logical type, scalar
 
  ! NUM2STR(IVEC, [PAD], [SEP], [ZPAD])
  ! NUM2STR(IVAL, [PAD], [ZPAD],    [LS], [RS])
  !
  ! NUM2STR(RVEC, [SEP], [PAD], [GENERAL])
  ! NUM2STR(RVAL, [PAD], [GENERAL], [LS], [RS])
  !
  ! Description:
  !   PAD     - INTEGER      - Reserved minimum space for number.
  !                              If the resulting string length is less than ABS(PAD),
  !                              then blank spaces are used to make sure the length is equal to PAD
  !                                 if PAD > 0, then number is right justified, 
  !                                 if PAD < 0, then number is left justified
  !   SEP     - CHARACTER(*) - Used as separate between stringed values in vector.
  !                              If not present, then a single space is used to separate numbers.
  !   ZPAD    - LOGICAL      - If .TRUE., then padding uses zeros rather than blank spaces.
  !   LS      - LOGICAL      - If .TRUE., then if the start of the restuling string is not a blank space, then one space is prepended.
  !   RS      - LOGICAL      - If .TRUE., then if the end   of the restuling string is not a blank space, then one space is postpended.
  !   GENERAL - LOGICAL      - If .TRUE., then number is not auto-formatted and uses "ES16.6".
  !
  !
  ! NUM2STR(RVAL, PAD, IPREC)
  ! NUM2STR(RVAL, DIGIT)
  !
  !   IPREC   - INTEGER      - If set to > 0, then resulting string uses high precision output (>7 digits)
  !   DIGIT   - CHARACTER(*) - Is set to an integer number, such as '12' and represents the number of significant digits to preserve. Results in TRIM(ADJUSTL("ES40."'//DIG))
  !
  ! NUM2STR(LVAL, FMT)
  ! NUM2STR(LVAL, FMT, PAD)
  ! NUM2STR(LVAL, PAD, FMT)
  ! NUM2STR(LVAL, PAD)
  !
  !   FMT  - CHARACTER(*) - Indicates output format for logical type.
  !                         If not present, then result is to print .false. => 'F'; .true. => 'T'
  !                         Otherwise the following options are supported:
  !                         fmt =
  !                              'T'  or 'F'  to ouptut for .true. => 'T'     and  .false. => 'F'; (default)
  !                              't'  or 'f'  to ouptut for .true. => 't'     and  .false. => 'f'
  !                              '0'  or '1'  to ouptut for .true. => '1'     and  .false. => '0'
  !                              'tr' or 'fa' to ouptut for .true. => 'true'  and  .false. => 'false'
  !                              'Tr' or 'Fa' to ouptut for .true. => 'True'  and  .false. => 'False'
  !                              'TR' or 'FA' to ouptut for .true. => 'TRUE'  and  .false. => 'FALSE'
  !
  USE, INTRINSIC:: ISO_FORTRAN_ENV, ONLY: INT8, INT16, INT32, INT64, &
                                          rel32 => REAL32, rel64 => REAL64
  !
  IMPLICIT NONE
  !
  PRIVATE
  !
  PUBLIC:: NUM2STR, NUM2STR7, INTFMT, NUMFMT, SEQ2STR
  !
  INTERFACE NUM2STR
    MODULE PROCEDURE INT08_VEC2STR   !(IVAL,[PAD], [SEP],[ZPAD])  --ZPAD is logical to indicate padding with 000
    MODULE PROCEDURE INT16_VEC2STR   !
    MODULE PROCEDURE INT32_VEC2STR   !
    MODULE PROCEDURE INT64_VEC2STR   !
    MODULE PROCEDURE INT08_2STR      !(IVAL,[PAD], [ZPAD], [LS], [RS])
    MODULE PROCEDURE INT16_2STR      !
    MODULE PROCEDURE INT32_2STR      !
    MODULE PROCEDURE INT64_2STR      !
    MODULE PROCEDURE REL32_2STR      !(RVAL,[PAD], [GENERAL], [LS], [RS])
    MODULE PROCEDURE REL64_2STR      !
    MODULE PROCEDURE REL32_VEC2STR   !(DVAL,[SEP], [PAD], [GENERAL])
    MODULE PROCEDURE REL64_VEC2STR   !
    MODULE PROCEDURE REL32_2STRDIG   !(DVAL, DIGIT, [PAD], [LS], [RS])
    MODULE PROCEDURE REL64_2STRDIG   !
    MODULE PROCEDURE REL32_PAD2STR   !(DVAL, PAD, IPREC, [LS], [RS])  --High Precision Printout -- set PAD=0 to autosize, IPREC > 0 for high precision
    MODULE PROCEDURE REL64_PAD2STR   !
    MODULE PROCEDURE TF_2STR_BASE    !(LVAL)
    MODULE PROCEDURE TF_2STR_FMT     !(LVAL, FMT)          --> fmt indicates output format such as fmt='1' for '0' or '1' output, or fmt='TR' to get 'TRUE' 'FALSE' output, or fmt='Tr' to get 'True' 'False' output
    MODULE PROCEDURE TF_2STR_FMT_PAD !(LVAL, FMT, PADDING) --> PADDING <-> PAD, to allow routing to work with interface (otherwise routine is identical to TF_2STR_FMT_PAD)
    MODULE PROCEDURE TF_2STR_PAD_FMT !(LVAL, PAD, FMT)
    MODULE PROCEDURE TF_2STR_PAD     !(LVAL, PAD)
  END INTERFACE
  !
  INTERFACE NUM2STR7
    MODULE PROCEDURE REL32_2STR7     !(RVAL,[PAD], [GENERAL], [LS], [RS])
    MODULE PROCEDURE REL64_2STR7     !
    !
    MODULE PROCEDURE INT08_VEC2STR   !(IVAL,[PAD],  [SEP],[ZPAD])  --ZPAD is logical to indicate padding with 000
    MODULE PROCEDURE INT16_VEC2STR   !
    MODULE PROCEDURE INT32_VEC2STR   !
    MODULE PROCEDURE INT64_VEC2STR   !
    MODULE PROCEDURE INT08_2STR      !(IVAL,[PAD],  [ZPAD], [LS], [RS])
    MODULE PROCEDURE INT16_2STR      !
    MODULE PROCEDURE INT32_2STR      !
    MODULE PROCEDURE INT64_2STR      !
    MODULE PROCEDURE TF_2STR_BASE    !(LVAL)
    MODULE PROCEDURE TF_2STR_FMT     !(LVAL, FMT)          --> fmt indicates output format such as fmt='1' for '0' or '1' output, or fmt='TR' to get 'TRUE' 'FALSE' output, or fmt='Tr' to get 'True' 'False' output
    MODULE PROCEDURE TF_2STR_FMT_PAD !(LVAL, FMT, PADDING) --> PADDING <-> PAD, to allow routing to work with interface (otherwise routine is identical to TF_2STR_FMT_PAD)
    MODULE PROCEDURE TF_2STR_PAD_FMT !(LVAL, PAD, FMT)
    MODULE PROCEDURE TF_2STR_PAD     !(LVAL, PAD)
  END INTERFACE
  !
  INTERFACE SEQ2STR
    !                                PRE: Base Name that is Post-Pendended with sequential number
    !                            SEQ_END: Sequence is from 1 to SEQ_END
    !                                SEQ: Vector of integers to post-pend to PRE
    !                              WIDTH: Space reserved for PRE_SEQ, so WIDTH = 10 -> "     PRE_1", WID<0 is left justified
    !                                SEP: Separator between each PRE_SEQ written entry
    !                              START: Sequence is from START to SEQ_END; PAD flow
    !                                PAD: Zero Pad squence number, so PAD=3 would result in 001, then 002
    MODULE PROCEDURE SEQ2STR_INT32_RNG  !(PRE, SEQ_END, [WIDTH], [SEP], [START], [PAD])
    MODULE PROCEDURE SEQ2STR_INT32_VEC  !(PRE, SEQ,     [WIDTH], [SEP],          [PAD])
  END INTERFACE
  !
  INTERFACE REAL2STR
    MODULE PROCEDURE REL32_2STR   !(RVAL,[PAD], [GENERAL], [LS], [RS])
    MODULE PROCEDURE REL64_2STR   !
  END INTERFACE 
  !
  INTERFACE INT2STR
    MODULE PROCEDURE INT08_2STR   !(IVAL,[PAD], [ZPAD], [LS], [RS])
    MODULE PROCEDURE INT16_2STR   !
    MODULE PROCEDURE INT32_2STR   !
    MODULE PROCEDURE INT64_2STR   !
  END INTERFACE 
  !
  ! Constants used internally to module ----------------------------------------------------
  !
  REAL(rel64),     PARAMETER:: DNEG   = -1.0_rel64
  REAL(rel64),     PARAMETER:: DZ     =  0.0_rel64
  REAL(rel64),     PARAMETER:: UNO    =  1.0_rel64
  REAL(rel64),     PARAMETER:: inf    =  HUGE(DZ)*0.99_rel64
  REAL(rel64),     PARAMETER:: ninf   = -inf
  REAL(rel32),     PARAMETER::  inf_R =  HUGE(1.0_rel32)*0.99_rel32
  REAL(rel32),     PARAMETER:: ninf_R = -inf_R
  LOGICAL,         PARAMETER:: TRUE   = .TRUE.
  LOGICAL,         PARAMETER:: FALSE  = .FALSE.
  CHARACTER,       PARAMETER:: BLNK   = " "
  !
  ! ----------------------------------------------------------------------------------------
  !
  !
  CONTAINS
  !
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !  
  !  Format Builder Routines
  !
  FUNCTION INTFMT(LINE)
    CHARACTER(*):: LINE
    CHARACTER(8):: INTFMT
    INTEGER:: W
    !  '(I10000)'
    W = LEN_TRIM(ADJUSTL(LINE))
    INTFMT = '(I' // TRIM(INT2STR(W)) // ')'
    !
  END FUNCTION
  !
  !#########################################################################################################################
  !
  FUNCTION NUMFMT(LINE)
    CHARACTER(*):: LINE
    CHARACTER(10):: NUMFMT
    INTEGER:: W
    !  '(F10000.0)'
    W = LEN_TRIM(ADJUSTL(LINE))
    NUMFMT = '(F' // TRIM(INT2STR(W)) // '.0)'
    !
  END FUNCTION
  !
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !  
  !  Logical To String - Default Logical (only supported option)
  !
  !##########################################################################################################################
  ! 
  ! Base Routine that other functions pass too
  !
  PURE SUBROUTINE TF_2STR_SUB(STR, LVAL, PAD, FMT)
    CHARACTER(:), ALLOCATABLE, INTENT(OUT):: STR
    LOGICAL,                   INTENT(IN ):: LVAL
    INTEGER,         OPTIONAL, INTENT(IN ):: PAD
    CHARACTER(*),    OPTIONAL, INTENT(IN ):: FMT     ! fmt indicates output format such as fmt='1' for '0' or '1' output, or fmt='TR' to get 'TRUE' 'FALSE' output, or fmt='Tr' to get 'True' 'False' output
    CHARACTER(5)::TF
    INTEGER:: N
    !
    TF = ''
    IF(PRESENT(fmt)) THEN
       N = LEN_TRIM(fmt)
       IF( N > 1 ) THEN
          SELECT CASE(fmt(1:2))
          CASE('Tr','Fa')
                          IF(LVAL) THEN
                              TF = 'True'
                          ELSE
                              TF = 'False'
                          END IF
          CASE('tr','fa')
                          IF(LVAL) THEN
                              TF = 'true'
                          ELSE
                              TF = 'false'
                          END IF
          CASE('TR','FA')
                          IF(LVAL) THEN
                              TF = 'TRUE'
                          ELSE
                              TF = 'FALSE'
                          END IF
          END SELECT
       END IF
       !
       IF( N == 1 .or. TF == '' ) THEN
          SELECT CASE(fmt(1:1))
          CASE('1','0')
                    IF(LVAL) THEN
                        TF = '1'
                    ELSE
                        TF = '0'
                    END IF
          CASE('t','f')
                    IF(LVAL) THEN
                        TF = 't'
                    ELSE
                        TF = 'f'
                    END IF
          CASE('T','F')
                    IF(LVAL) THEN
                        TF = 'T'
                    ELSE
                        TF = 'F'
                    END IF
          END SELECT
       END IF
    END IF
    !
    if( TF == '') then
        IF(LVAL) THEN
            TF = 'T'
        ELSE
            TF = 'F'
        END IF
    end if
    !
    IF(PRESENT(PAD)) THEN
        !
        TF = ADJUSTL(TF)
        !
        IF( LEN_TRIM(TF) < ABS(PAD)) THEN
                IF (PAD>0) THEN
                           STR = TRIM( REPEAT(' ',PAD-LEN_TRIM(TF))//TF )
                ELSE
                           STR = TRIM(TF)//REPEAT(' ',ABS(PAD)-LEN_TRIM(TF))
                END IF
        ELSE
                           STR = TRIM(TF)
        END IF
    ELSE
        STR = TRIM(ADJUSTL(TF))
    END IF
    !
  END SUBROUTINE
  !
  !##########################################################################################################################
  ! 
  ! Only Logical specified routine
  !
  PURE FUNCTION TF_2STR_BASE(LVAL) RESULT(STR)
    LOGICAL,       INTENT(IN):: LVAL
    CHARACTER(:), ALLOCATABLE:: STR
    CALL TF_2STR_SUB(STR, LVAL)
  END FUNCTION
  !
  !##########################################################################################################################
  ! 
  ! PAD and FMT Specified routines
  !
  PURE FUNCTION TF_2STR_FMT_PAD(LVAL, FMT, PADDING) RESULT(STR)
    LOGICAL,       INTENT(IN):: LVAL
    CHARACTER(*),  INTENT(IN):: FMT
    INTEGER,       INTENT(IN):: PADDING
    CHARACTER(:), ALLOCATABLE:: STR
    CALL TF_2STR_SUB(STR, LVAL, PADDING, FMT)
  END FUNCTION
  !
  PURE FUNCTION TF_2STR_PAD_FMT(LVAL, PAD, FMT) RESULT(STR)
    LOGICAL,       INTENT(IN):: LVAL
    INTEGER,       INTENT(IN):: PAD
    CHARACTER(*),  INTENT(IN):: FMT
    CHARACTER(:), ALLOCATABLE:: STR
    CALL TF_2STR_SUB(STR, LVAL, PAD, FMT)
  END FUNCTION
  !
  !##########################################################################################################################
  ! 
  ! PAD Specified routines
  !
  !
  PURE FUNCTION TF_2STR_PAD(LVAL, PAD) RESULT(STR)
    LOGICAL,       INTENT(IN):: LVAL
    INTEGER,       INTENT(IN):: PAD
    CHARACTER(:), ALLOCATABLE:: STR
    CALL TF_2STR_SUB(STR, LVAL, PAD)
  END FUNCTION
  !
  !##########################################################################################################################
  ! 
  ! PAD and FMT Specified routines
  !
  !
  PURE FUNCTION TF_2STR_FMT(LVAL,FMT) RESULT(STR)
    LOGICAL,       INTENT(IN):: LVAL
    CHARACTER(*),  INTENT(IN):: FMT
    CHARACTER(:), ALLOCATABLE:: STR
    CALL TF_2STR_SUB(STR, LVAL, FMT=FMT)
  END FUNCTION
  !
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !  
  !  Integer Vecetor To String
  !
  !##########################################################################################################################
  ! INT8 Vector To String
  !
  PURE FUNCTION INT08_VEC2STR(IVAL,PAD,SEP,ZPAD) RESULT(STR)
    INTEGER(INT8), DIMENSION(:), INTENT(IN):: IVAL
    INTEGER,            OPTIONAL, INTENT(IN):: PAD
    CHARACTER(*),       OPTIONAL, INTENT(IN):: SEP
    LOGICAL,            OPTIONAL, INTENT(IN):: ZPAD
    CHARACTER(:),   ALLOCATABLE:: STR
    CHARACTER(:),   ALLOCATABLE:: SEPOR
    INTEGER:: I, N
    !
    IF(PRESENT(SEP)) THEN
        ALLOCATE(SEPOR, SOURCE=SEP)
    ELSE
        ALLOCATE(SEPOR, SOURCE=" ")
    END IF
    !
    N = SIZE(IVAL)
    !
    IF(N < 1) THEN
        STR = ''
    ELSE
        STR = INT2STR(IVAL(1),PAD,ZPAD)
    END IF
    !
    IF(N>1) THEN
          DO I=2, N
              STR=STR//SEPOR//INT2STR(IVAL(I),PAD,ZPAD)
          END DO
    END IF
    !
  END FUNCTION
  !
  !##########################################################################################################################
  ! INT16 Vector To String
  !
  PURE FUNCTION INT16_VEC2STR(IVAL,PAD,SEP,ZPAD) RESULT(STR)
    INTEGER(INT16), DIMENSION(:), INTENT(IN):: IVAL
    INTEGER,            OPTIONAL, INTENT(IN):: PAD
    CHARACTER(*),       OPTIONAL, INTENT(IN):: SEP
    LOGICAL,            OPTIONAL, INTENT(IN):: ZPAD
    CHARACTER(:),   ALLOCATABLE:: STR
    CHARACTER(:),   ALLOCATABLE:: SEPOR
    INTEGER:: I, N
    !
    IF(PRESENT(SEP)) THEN
        ALLOCATE(SEPOR, SOURCE=SEP)
    ELSE
        ALLOCATE(SEPOR, SOURCE=" ")
    END IF
    !
    N = SIZE(IVAL)
    !
    IF(N < 1) THEN
        STR = ''
    ELSE
        STR = INT2STR(IVAL(1),PAD,ZPAD)
    END IF
    !
    IF(N>1) THEN
          DO I=2, N
              STR=STR//SEPOR//INT2STR(IVAL(I),PAD,ZPAD)
          END DO
    END IF
    !
  END FUNCTION
  !
  !##########################################################################################################################
  ! INT32 Vector To String
  !
  PURE FUNCTION INT32_VEC2STR(IVAL,PAD,SEP,ZPAD) RESULT(STR)
    INTEGER(INT32), DIMENSION(:), INTENT(IN):: IVAL
    INTEGER,            OPTIONAL, INTENT(IN):: PAD
    CHARACTER(*),       OPTIONAL, INTENT(IN):: SEP
    LOGICAL,            OPTIONAL, INTENT(IN):: ZPAD
    CHARACTER(:),   ALLOCATABLE:: STR
    CHARACTER(:),   ALLOCATABLE:: SEPOR
    INTEGER:: I, N
    !
    IF(PRESENT(SEP)) THEN
        ALLOCATE(SEPOR, SOURCE=SEP)
    ELSE
        ALLOCATE(SEPOR, SOURCE=" ")
    END IF
    !
    N = SIZE(IVAL)
    !
    IF(N < 1) THEN
        STR = ''
    ELSE
        STR = INT2STR(IVAL(1),PAD,ZPAD)
    END IF
    !
    IF(N>1) THEN
          DO I=2, N
              STR=STR//SEPOR//INT2STR(IVAL(I),PAD,ZPAD)
          END DO
    END IF
    !
  END FUNCTION
  !
  !##########################################################################################################################
  ! INT64 Vector To String
  !
  PURE FUNCTION INT64_VEC2STR(IVAL,PAD,SEP,ZPAD) RESULT(STR)
    INTEGER(INT64), DIMENSION(:), INTENT(IN):: IVAL
    INTEGER,            OPTIONAL, INTENT(IN):: PAD
    CHARACTER(*),       OPTIONAL, INTENT(IN):: SEP
    LOGICAL,            OPTIONAL, INTENT(IN):: ZPAD
    CHARACTER(:),   ALLOCATABLE:: STR
    CHARACTER(:),   ALLOCATABLE:: SEPOR
    INTEGER:: I, N
    !
    IF(PRESENT(SEP)) THEN
        ALLOCATE(SEPOR, SOURCE=SEP)
    ELSE
        ALLOCATE(SEPOR, SOURCE=" ")
    END IF
    !
    N = SIZE(IVAL)
    !
    IF(N < 1) THEN
        STR = ''
    ELSE
        STR = INT2STR(IVAL(1),PAD,ZPAD)
    END IF
    !
    IF(N>1) THEN
          DO I=2, N
              STR=STR//SEPOR//INT2STR(IVAL(I),PAD,ZPAD)
          END DO
    END IF
    !
  END FUNCTION
  !
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !  
  !  Integer To String
  !
  !##########################################################################################################################
  ! INT08 To String
  !
  PURE FUNCTION INT08_2STR(IVAL,PAD,ZPAD,LS,RS) RESULT(STR)
    INTEGER(INT8),   INTENT(IN):: IVAL
    INTEGER,OPTIONAL,INTENT(IN):: PAD
    LOGICAL,OPTIONAL,INTENT(IN):: ZPAD
    LOGICAL,OPTIONAL,INTENT(IN):: LS  ! If TRUE and there is no space on the front, add 1 space to end
    LOGICAL,OPTIONAL,INTENT(IN):: RS  ! If TRUE and there is no space on the end,   add 1 space to end
    CHARACTER(:),   ALLOCATABLE:: STR
    CHARACTER(32)::NUM
    !
    WRITE(NUM,'(I32)') IVAL
    !
    NUM=ADJUSTL(NUM)
    IF(PRESENT(PAD)) THEN
          !
          IF(LEN_TRIM(NUM) < ABS(PAD)) THEN
                IF (PAD>0) THEN
                           IF(PRESENT(ZPAD)) THEN
                               IF(ZPAD) THEN
                                   STR = TRIM( REPEAT('0',PAD-LEN_TRIM(NUM))//NUM )
                               ELSE
                                   STR = TRIM( REPEAT(' ',PAD-LEN_TRIM(NUM))//NUM )
                               END IF
                           ELSE
                                   STR = TRIM( REPEAT(' ',PAD-LEN_TRIM(NUM))//NUM )
                           END IF
                ELSE
                                   STR = TRIM(NUM)//REPEAT(' ',ABS(PAD)-LEN_TRIM(NUM))
                END IF
          ELSE
                                   STR = TRIM(NUM)
          END IF
    ELSE
                                   STR = TRIM(NUM)
    END IF
    !
    IF(PRESENT(LS)) THEN
            IF(LS .AND. STR(1:1) /= BLNK) STR = BLNK//STR
    END IF
    !
    IF(PRESENT(RS)) THEN
            IF(RS .AND. STR(LEN(STR):LEN(STR)) /= BLNK) STR = STR//BLNK
    END IF
    !
  END FUNCTION
  !
  !##########################################################################################################################
  ! INT16 To String
  !
  PURE FUNCTION INT16_2STR(IVAL,PAD,ZPAD,LS,RS) RESULT(STR)
    INTEGER(INT16),  INTENT(IN):: IVAL
    INTEGER,OPTIONAL,INTENT(IN):: PAD
    LOGICAL,OPTIONAL,INTENT(IN):: ZPAD
    LOGICAL,OPTIONAL,INTENT(IN):: LS  ! If TRUE and there is no space on the front, add 1 space to end
    LOGICAL,OPTIONAL,INTENT(IN):: RS  ! If TRUE and there is no space on the end,   add 1 space to end
    CHARACTER(:),   ALLOCATABLE:: STR
    CHARACTER(32)::NUM
    !
    WRITE(NUM,'(I32)') IVAL
    !
    NUM=ADJUSTL(NUM)
    IF(PRESENT(PAD)) THEN
          !
          IF(LEN_TRIM(NUM) < ABS(PAD)) THEN
                IF (PAD>0) THEN
                           IF(PRESENT(ZPAD)) THEN
                               IF(ZPAD) THEN
                                   STR = TRIM( REPEAT('0',PAD-LEN_TRIM(NUM))//NUM )
                               ELSE
                                   STR = TRIM( REPEAT(' ',PAD-LEN_TRIM(NUM))//NUM )
                               END IF
                           ELSE
                                   STR = TRIM( REPEAT(' ',PAD-LEN_TRIM(NUM))//NUM )
                           END IF
                ELSE
                                   STR = TRIM(NUM)//REPEAT(' ',ABS(PAD)-LEN_TRIM(NUM))
                END IF
          ELSE
                                   STR = TRIM(NUM)
          END IF
    ELSE
                                   STR = TRIM(NUM)
    END IF
    !
    IF(PRESENT(LS)) THEN
            IF(LS .AND. STR(1:1) /= BLNK) STR = BLNK//STR
    END IF
    !
    IF(PRESENT(RS)) THEN
            IF(RS .AND. STR(LEN(STR):LEN(STR)) /= BLNK) STR = STR//BLNK
    END IF
    !
  END FUNCTION
  !
  !##########################################################################################################################
  ! INT32 To String
  !
  PURE FUNCTION INT32_2STR(IVAL,PAD,ZPAD,LS,RS) RESULT(STR)
    INTEGER(INT32),  INTENT(IN):: IVAL
    INTEGER,OPTIONAL,INTENT(IN):: PAD
    LOGICAL,OPTIONAL,INTENT(IN):: ZPAD
    LOGICAL,OPTIONAL,INTENT(IN):: LS  ! If TRUE and there is no space on the front, add 1 space to end
    LOGICAL,OPTIONAL,INTENT(IN):: RS  ! If TRUE and there is no space on the end,   add 1 space to end
    CHARACTER(:),   ALLOCATABLE:: STR
    CHARACTER(32)::NUM
    !
    WRITE(NUM,'(I32)') IVAL
    !
    NUM=ADJUSTL(NUM)
    IF(PRESENT(PAD)) THEN
          !
          IF(LEN_TRIM(NUM) < ABS(PAD)) THEN
                IF (PAD>0) THEN
                           IF(PRESENT(ZPAD)) THEN
                               IF(ZPAD) THEN
                                   STR = TRIM( REPEAT('0',PAD-LEN_TRIM(NUM))//NUM )
                               ELSE
                                   STR = TRIM( REPEAT(' ',PAD-LEN_TRIM(NUM))//NUM )
                               END IF
                           ELSE
                                   STR = TRIM( REPEAT(' ',PAD-LEN_TRIM(NUM))//NUM )
                           END IF
                ELSE
                                   STR = TRIM(NUM)//REPEAT(' ',ABS(PAD)-LEN_TRIM(NUM))
                END IF
          ELSE
                                   STR = TRIM(NUM)
          END IF
    ELSE
                                   STR = TRIM(NUM)
    END IF
    !
    IF(PRESENT(LS)) THEN
            IF(LS .AND. STR(1:1) /= BLNK) STR = BLNK//STR
    END IF
    !
    IF(PRESENT(RS)) THEN
            IF(RS .AND. STR(LEN(STR):LEN(STR)) /= BLNK) STR = STR//BLNK
    END IF
    !
  END FUNCTION
  !
  !##########################################################################################################################
  ! INT64 To String
  !
  PURE FUNCTION INT64_2STR(IVAL,PAD,ZPAD,LS,RS) RESULT(STR)
    INTEGER(INT64),  INTENT(IN):: IVAL
    INTEGER,OPTIONAL,INTENT(IN):: PAD
    LOGICAL,OPTIONAL,INTENT(IN):: ZPAD
    LOGICAL,OPTIONAL,INTENT(IN):: LS  ! If TRUE and there is no space on the front, add 1 space to end
    LOGICAL,OPTIONAL,INTENT(IN):: RS  ! If TRUE and there is no space on the end,   add 1 space to end
    CHARACTER(:),   ALLOCATABLE:: STR
    CHARACTER(32)::NUM
    !
    WRITE(NUM,'(I32)') IVAL
    !
    NUM=ADJUSTL(NUM)
    IF(PRESENT(PAD)) THEN
          !
          IF(LEN_TRIM(NUM) < ABS(PAD)) THEN
                IF (PAD>0) THEN
                           IF(PRESENT(ZPAD)) THEN
                               IF(ZPAD) THEN
                                   STR = TRIM( REPEAT('0',PAD-LEN_TRIM(NUM))//NUM )
                               ELSE
                                   STR = TRIM( REPEAT(' ',PAD-LEN_TRIM(NUM))//NUM )
                               END IF
                           ELSE
                                   STR = TRIM( REPEAT(' ',PAD-LEN_TRIM(NUM))//NUM )
                           END IF
                ELSE
                                   STR = TRIM(NUM)//REPEAT(' ',ABS(PAD)-LEN_TRIM(NUM))
                END IF
          ELSE
                                   STR = TRIM(NUM)
          END IF
    ELSE
                                   STR = TRIM(NUM)
    END IF
    !
    IF(PRESENT(LS)) THEN
            IF(LS .AND. STR(1:1) /= BLNK) STR = BLNK//STR
    END IF
    !
    IF(PRESENT(RS)) THEN
            IF(RS .AND. STR(LEN(STR):LEN(STR)) /= BLNK) STR = STR//BLNK
    END IF
    !
  END FUNCTION
  !
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !  
  !  REAL Vector To String
  !
  !##########################################################################################################################
  ! REAL(rel32) To String
  !
  PURE FUNCTION REL32_VEC2STR(VEC,SEP,PAD,GENERAL) RESULT(STR)
    REAL(rel32), DIMENSION(:), INTENT(IN):: VEC
    CHARACTER(*),    OPTIONAL, INTENT(IN):: SEP
    INTEGER,         OPTIONAL, INTENT(IN):: PAD
    LOGICAL,         OPTIONAL, INTENT(IN):: GENERAL
    !LOGICAL,OPTIONAL,INTENT(IN):: RIGHT
    CHARACTER(:),   ALLOCATABLE:: STR
    CHARACTER(:),   ALLOCATABLE:: SEPOR
    INTEGER:: I, N
    !
    IF(PRESENT(SEP)) THEN
        ALLOCATE(SEPOR, SOURCE=SEP)
    ELSE
        ALLOCATE(SEPOR, SOURCE=" ")
    END IF
    !
    N = SIZE(VEC)
    !
    IF(N < 1) THEN
        STR = ''
    ELSE
        STR = REAL2STR(VEC(1),PAD,GENERAL)
    END IF
    !
    IF(N>1) THEN
          DO I=2, N
              STR=STR//SEPOR//REAL2STR(VEC(I),PAD,GENERAL)
          END DO
    END IF
    !
  END FUNCTION
  !
  !##########################################################################################################################
  ! REAL(rel64) To String
  !
  PURE FUNCTION REL64_VEC2STR(VEC,SEP,PAD,GENERAL) RESULT(STR)
    REAL(rel64), DIMENSION(:), INTENT(IN):: VEC
    CHARACTER(*),    OPTIONAL, INTENT(IN):: SEP
    INTEGER,         OPTIONAL, INTENT(IN):: PAD
    LOGICAL,         OPTIONAL, INTENT(IN):: GENERAL
    !LOGICAL,OPTIONAL,INTENT(IN):: RIGHT
    CHARACTER(:),   ALLOCATABLE:: STR
    CHARACTER(:),   ALLOCATABLE:: SEPOR
    INTEGER:: I, N
    !
    IF(PRESENT(SEP)) THEN
        ALLOCATE(SEPOR, SOURCE=SEP)
    ELSE
        ALLOCATE(SEPOR, SOURCE=" ")
    END IF
    !
    N = SIZE(VEC)
    !
    IF(N < 1) THEN
        STR = ''
    ELSE
        STR = REAL2STR(VEC(1),PAD,GENERAL)
    END IF
    !
    IF(N>1) THEN
          DO I=2, N
              STR=STR//SEPOR//REAL2STR(VEC(I),PAD,GENERAL)
          END DO
    END IF
    !
  END FUNCTION
  !
  !
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !  
  !  REAL To String
  !
  !##########################################################################################################################
  ! REAL(rel32) To String
  !
  PURE FUNCTION rel32_2STR(RVAL,PAD,GENERAL,LS,RS) RESULT(STR)
    REAL(rel32),       INTENT(IN):: RVAL
    INTEGER, OPTIONAL, INTENT(IN):: PAD
    LOGICAL, OPTIONAL, INTENT(IN):: GENERAL
    LOGICAL, OPTIONAL, INTENT(IN):: LS  ! If TRUE and there is no space on the front, add 1 space to end
    LOGICAL, OPTIONAL, INTENT(IN):: RS  ! If TRUE and there is no space on the end,   add 1 space to end
    CHARACTER(:),  ALLOCATABLE :: STR
    REAL(rel32):: RVAL1C, RVAL10, RVAL1K
    CHARACTER(16)::NUM !LARGEST POSSIBLE NUMBER IS 14 CHARACTERS
    LOGICAL::GEN
    !
    GEN=FALSE; IF(PRESENT(GENERAL))GEN=GENERAL
    !
    NUM=''
    RVAL10 = 10._rel32*RVAL;   RVAL1C = 100._rel32*RVAL;   RVAL1K = 1000._rel32*RVAL
    !
    IF(RVAL /= RVAL) THEN
        NUM='NaN'
    ELSEIF(RVAL >= inf_R) THEN
        NUM = 'inf'
    ELSEIF(RVAL <= ninf_R) THEN
        NUM = '-inf'
    ELSEIF(.NOT. GEN) THEN
    !
    IF(RVAL==0.E0_rel32)                 THEN
       WRITE(NUM,'(F3.1)') RVAL
    ELSEIF(RVAL>=1.E10_rel32 .OR. RVAL<=-1.E10_rel32)         THEN
       WRITE(NUM,'(ES16.7E2)') RVAL
    ELSEIF( RVAL10 == AINT(RVAL10) .AND. (RVAL10>=1._rel32.OR.RVAL10<=-1._rel32) ) THEN
       WRITE(NUM,'(F16.1)') RVAL
    ELSEIF( RVAL1C == AINT(RVAL1C) .AND. (RVAL1C>=1._rel32.OR.RVAL1C<=-1._rel32) ) THEN
       WRITE(NUM,'(F16.2)') RVAL
    ELSEIF( RVAL1K == AINT(RVAL1K) .AND. (RVAL1K>=1._rel32.OR.RVAL1K<=-1._rel32) ) THEN
       WRITE(NUM,'(F16.3)') RVAL
    ELSEIF(RVAL>=1.E6_rel32 .OR. RVAL<=-1.E6_rel32)          THEN
       WRITE(NUM,'(ES16.7E1)') RVAL
    ELSEIF(RVAL>=1.E2_rel32 .OR. RVAL<=-1.E2_rel32)          THEN
       WRITE(NUM,'(F16.5)') RVAL
    ELSEIF(RVAL>=0.00099E0_rel32 .OR. RVAL<=-0.00099E0_rel32 )  THEN
       WRITE(NUM,'(F16.7)') RVAL
    ELSEIF(RVAL>=1.E-9_rel32 .OR. RVAL<=-1.E-9_rel32)         THEN
       WRITE(NUM,'(ES16.7E1)') RVAL
    ELSEIF(RVAL>0.E0_rel32 .OR. RVAL<0.E0_rel32)              THEN
       WRITE(NUM,'(ES16.7E3)') RVAL
    END IF
    !
    ELSE
        WRITE(NUM,'(ES16.6)') RVAL
    END IF
    !
    IF(PRESENT(PAD)) THEN
        !
        NUM = ADJUSTL(NUM)
        !
        IF( LEN_TRIM(NUM) < ABS(PAD)) THEN
                IF (PAD>0) THEN
                           STR = TRIM( REPEAT(' ',PAD-LEN_TRIM(NUM))//NUM )
                ELSE
                           STR = TRIM(NUM)//REPEAT(' ',ABS(PAD)-LEN_TRIM(NUM))
                END IF
        ELSE
                           STR = TRIM(NUM)
        END IF
    ELSE
        STR = TRIM(ADJUSTL(NUM))
    END IF
    !
    IF(PRESENT(LS)) THEN
            IF(LS .AND. STR(1:1) /= BLNK) STR = BLNK//STR
    END IF
    !
    IF(PRESENT(RS)) THEN
            IF(RS .AND. STR(LEN(STR):LEN(STR)) /= BLNK) STR = STR//BLNK
    END IF
    !
  END FUNCTION
  !
  !##########################################################################################################################
  ! REAL(rel64) To String
  !
  PURE FUNCTION REL64_2STR(DVAL,PAD,GENERAL,LS,RS) RESULT(STR)
    REAL(rel64),       INTENT(IN):: DVAL
    INTEGER, OPTIONAL, INTENT(IN):: PAD
    LOGICAL, OPTIONAL, INTENT(IN):: GENERAL
    LOGICAL, OPTIONAL, INTENT(IN):: LS  ! If TRUE and there is no space on the front, add 1 space to end
    LOGICAL, OPTIONAL, INTENT(IN):: RS  ! If TRUE and there is no space on the end,   add 1 space to end
    CHARACTER(:),  ALLOCATABLE :: STR
    REAL(rel64):: DVAL1C, DVAL10, DVAL1K
    CHARACTER(16)::NUM !LARGEST POSSIBLE NUMBER IS 14 CHARACTERS
    LOGICAL::GEN
    !
    GEN=FALSE; IF(PRESENT(GENERAL)) GEN=GENERAL
    !
    NUM=''
    DVAL10 = 10._rel64*DVAL;   DVAL1C = 100._rel64*DVAL;   DVAL1K = 1000._rel64*DVAL
    !
    IF(DVAL /= DVAL) THEN
        NUM='NaN'
    ELSEIF(DVAL >= inf) THEN
        NUM = 'inf'
    ELSEIF(DVAL <= ninf) THEN
        NUM = '-inf'
    ELSEIF(.NOT. GEN) THEN
    !
    IF(DVAL==DZ)                 THEN
       WRITE(NUM,'(F3.1)') DVAL
    ELSEIF(DVAL>=1.D100 .OR. DVAL<=-1.D100)       THEN
       WRITE(NUM,'(ES16.7E3)') DVAL
    ELSEIF(DVAL>=1.D10 .OR. DVAL<=-1.D10)         THEN
       WRITE(NUM,'(ES16.7E2)') DVAL
    ELSEIF( DVAL10 == AINT(DVAL10) .AND. (DVAL10>=UNO.OR.DVAL10<=DNEG) ) THEN
       WRITE(NUM,'(F16.1)') DVAL
    ELSEIF( DVAL1C == AINT(DVAL1C) .AND. (DVAL1C>=UNO.OR.DVAL1C<=DNEG) ) THEN
       WRITE(NUM,'(F16.2)') DVAL
    ELSEIF( DVAL1K == AINT(DVAL1K) .AND. (DVAL1K>=UNO.OR.DVAL1K<=DNEG) ) THEN
       WRITE(NUM,'(F16.3)') DVAL
    ELSEIF(DVAL>=1.D6 .OR. DVAL<=-1.D6)            THEN
       WRITE(NUM,'(ES16.7E1)') DVAL
    ELSEIF(DVAL>=1.D2 .OR. DVAL<=-1.D2 )           THEN
       WRITE(NUM,'(F16.5)') DVAL
    ELSEIF(DVAL>=0.00099D0 .OR. DVAL<=-0.00099D0 ) THEN
       WRITE(NUM,'(F16.7)') DVAL
    ELSEIF(DVAL>=1.D-9 .OR. DVAL<=-1.D-9)          THEN
       WRITE(NUM,'(ES16.7E1)') DVAL
    ELSEIF(DVAL>=1.D-99 .OR. DVAL<=-1.D-99)        THEN
       WRITE(NUM,'(ES16.7E2)') DVAL
    ELSEIF(DVAL>DZ .OR. DVAL<DZ)                   THEN
       WRITE(NUM,'(ES16.7E3)') DVAL
    END IF
    !
    ELSE
        WRITE(NUM,'(ES16.6)') DVAL
    END IF
    !
    IF(PRESENT(PAD)) THEN
        !
        NUM = ADJUSTL(NUM)
        !
        IF( LEN_TRIM(NUM) < ABS(PAD)) THEN
                IF (PAD>0) THEN
                           STR = TRIM( REPEAT(' ',PAD-LEN_TRIM(NUM))//NUM )
                ELSE
                           STR = TRIM(NUM)//REPEAT(' ',ABS(PAD)-LEN_TRIM(NUM))
                END IF
        ELSE
                           STR = TRIM(NUM)
        END IF
    ELSE
        STR = TRIM(ADJUSTL(NUM))
    END IF
    !
    IF(PRESENT(LS)) THEN
            IF(LS .AND. STR(1:1) /= BLNK) STR = BLNK//STR
    END IF
    !
    IF(PRESENT(RS)) THEN
            IF(RS .AND. STR(LEN(STR):LEN(STR)) /= BLNK) STR = STR//BLNK
    END IF
    !
  END FUNCTION
  !
  !
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !  
  !  REAL To String and specify by No. of Digits
  !
  !##########################################################################################################################
  ! REAL(rel32) To String and specify by No. of Digits
  !
  PURE FUNCTION REL32_2STRDIG(RVAL,DIGIT,PAD,LS,RS) RESULT(STR)
    REAL(rel32),      INTENT(IN):: RVAL
    CHARACTER(*),     INTENT(IN):: DIGIT
    INTEGER,OPTIONAL, INTENT(IN):: PAD
    LOGICAL,OPTIONAL, INTENT(IN):: LS  ! If TRUE and there is no space on the front, add 1 space to end
    LOGICAL,OPTIONAL, INTENT(IN):: RS  ! If TRUE and there is no space on the end,   add 1 space to end
    CHARACTER(:),    ALLOCATABLE:: STR
    REAL(rel64):: DVAL
    !
    IF(RVAL /= RVAL) THEN
        DVAL = REAL(RVAL, rel64)
    ELSEIF(RVAL >= inf_R) THEN
        DVAL = inf
    ELSEIF(RVAL <= ninf_R) THEN
        DVAL = ninf
    ELSEIF(RVAL==0.0_rel32)    THEN
        DVAL = DZ
    ELSEIF(RVAL==1.0_rel32)   THEN
        DVAL = UNO
    ELSE
        DVAL = REAL(RVAL, rel64)
    END IF
    !
    STR = REL64_2STRDIG(DVAL, DIGIT, PAD, LS, RS)
    !
  END FUNCTION
  !
  !##########################################################################################################################
  ! REAL(rel64) To String and specify by No. of Digits
  !
  PURE FUNCTION REL64_2STRDIG(DVAL,DIGIT,PAD,LS,RS) RESULT(STR)
    REAL(rel64),      INTENT(IN):: DVAL
    CHARACTER(*),     INTENT(IN):: DIGIT
    INTEGER,OPTIONAL, INTENT(IN):: PAD
    LOGICAL,OPTIONAL, INTENT(IN):: LS  ! If TRUE and there is no space on the front, add 1 space to end
    LOGICAL,OPTIONAL, INTENT(IN):: RS  ! If TRUE and there is no space on the end,   add 1 space to end
    CHARACTER(:),    ALLOCATABLE:: STR
    CHARACTER(:),    ALLOCATABLE:: DIG
    REAL(rel64)::  DVAL100
    CHARACTER(41 )::NUM
    !
    NUM=''
    ALLOCATE(DIG, SOURCE=TRIM(ADJUSTL(DIGIT)))
    !DVAL10 = DIEZ*DVAL !FOR CHECKING IF THE SAME TO THE NEARIEST
    DVAL100=  100._rel64*DVAL
    !
    IF(DVAL /= DVAL) THEN
        NUM='NaN'
    ELSEIF(DVAL >= inf) THEN
        NUM = 'inf'
    ELSEIF(DVAL <= ninf) THEN
        NUM = '-inf'
    ELSEIF(DVAL==DZ .OR. DVAL==UNO)             THEN
       WRITE(NUM,'(F3.1)') DVAL
    ELSEIF(DVAL>=1D100 .OR. DVAL<=-1D100)       THEN
       WRITE(NUM,'(ES40.'//DIG//'E3)') DVAL
    ELSEIF(DVAL>=1.D10 .OR. DVAL<=-1.D10)         THEN
       WRITE(NUM,'(ES40.'//DIG//'E2)') DVAL
    !ELSEIF( DVAL10 == AINT(DVAL10) .AND. (DVAL10>=UNO .OR. DVAL10<=DNEG) ) THEN
    !   WRITE(NUM,'(F40.1)') DVAL
    ELSEIF( DVAL100 == AINT(DVAL100) .AND. (DVAL100>=UNO .OR. DVAL100<=DNEG) ) THEN
       WRITE(NUM,'(F40.2)') DVAL
    ELSEIF(DVAL>=1.D6 .OR. DVAL<=-1.D6)           THEN
       WRITE(NUM,'(ES40.'//DIG//'E1)') DVAL
    ELSEIF(DVAL>=1.D2 .OR. DVAL<=-1.D2 )          THEN
       WRITE(NUM,'(F40.5)') DVAL
    ELSEIF(DVAL>=0.00099D0 .OR. DVAL<=-0.00099D0 )  THEN
       WRITE(NUM,'(F40.'//DIG//')') DVAL
    ELSEIF(DVAL>=1.D-9 .OR. DVAL<=-1.D-9)         THEN
       WRITE(NUM,'(ES40.'//DIG//'E1)') DVAL
    ELSEIF(DVAL>=1.D-99 .OR. DVAL<=-1.D-99)       THEN
       WRITE(NUM,'(ES40.'//DIG//'E2)') DVAL
    ELSEIF(DVAL>DZ .OR. DVAL<DZ)              THEN
       WRITE(NUM,'(ES40.'//DIG//'E3)') DVAL
    END IF
    !
    IF(PRESENT(PAD)) THEN
        !
        NUM = ADJUSTL(NUM)
        !
        IF( LEN_TRIM(NUM) < ABS(PAD)) THEN
                IF (PAD>0) THEN
                           STR = TRIM( REPEAT(' ',PAD-LEN_TRIM(NUM))//NUM )
                ELSE
                           STR = TRIM(NUM)//REPEAT(' ',ABS(PAD)-LEN_TRIM(NUM))
                END IF
        ELSE
                           STR = TRIM(NUM)
        END IF
    ELSE
        STR = TRIM(ADJUSTL(NUM))
    END IF
    !
    IF(PRESENT(LS)) THEN
            IF(LS .AND. STR(1:1) /= BLNK) STR = BLNK//STR
    END IF
    !
    IF(PRESENT(RS)) THEN
            IF(RS .AND. STR(LEN(STR):LEN(STR)) /= BLNK) STR = STR//BLNK
    END IF
    !
  END FUNCTION
  !
  !
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !  
  !  REAL To String with High Precision Output
  !
  !##########################################################################################################################
  ! REAL(rel32) To String with High Precision Output
  !
  PURE FUNCTION REL32_PAD2STR(RVAL, PAD, IPREC, LS, RS) RESULT(STR)   !IPREC > 0  => HIGH PRECISION PRINT OUT
    REAL(rel32),       INTENT(IN):: RVAL
    INTEGER,           INTENT(IN):: PAD, IPREC
    LOGICAL, OPTIONAL, INTENT(IN):: LS  ! If TRUE and there is no space on the front, add 1 space to end
    LOGICAL, OPTIONAL, INTENT(IN):: RS  ! If TRUE and there is no space on the end,   add 1 space to end
    CHARACTER(:), ALLOCATABLE:: STR
    REAL(rel64):: DVAL
    !
    IF(RVAL /= RVAL) THEN
        DVAL = REAL(RVAL, rel64)
    ELSEIF(RVAL >= inf_R) THEN
        DVAL = inf
    ELSEIF(RVAL <= ninf_R) THEN
        DVAL = ninf
    ELSEIF(RVAL==0.0_rel32)    THEN
        DVAL = DZ
    ELSEIF(RVAL==1.0_rel32)   THEN
        DVAL = UNO
    ELSE
        DVAL = REAL(RVAL, rel64)
    END IF
    !
    STR = REL64_PAD2STR(DVAL, PAD, IPREC, LS, RS)
    !
  END FUNCTION
  !
  !##########################################################################################################################
  ! REAL(rel64) To String with High Precision Output
  !
  PURE FUNCTION REL64_PAD2STR(DVAL, PAD, IPREC, LS, RS) RESULT(STR)   !IPREC > 0  => HIGH PRECISION PRINT OUT
    REAL(rel64),       INTENT(IN):: DVAL
    INTEGER,           INTENT(IN):: PAD, IPREC
    LOGICAL, OPTIONAL, INTENT(IN):: LS  ! If TRUE and there is no space on the front, add 1 space to end
    LOGICAL, OPTIONAL, INTENT(IN):: RS  ! If TRUE and there is no space on the end,   add 1 space to end
    CHARACTER(:),  ALLOCATABLE :: STR
    REAL(rel64):: DVAL100
    CHARACTER(18)::NUM
    !
    IF( IPREC < 1) THEN
        STR = REAL2STR(DVAL,PAD,LS,RS)
    ELSE
        NUM=''
        !DVAL10 = DIEZ*DVAL !FOR CHECKING IF THE SAME TO THE NEARIEST
        DVAL100= 100._rel64*DVAL
        !
        IF(DVAL /= DVAL) THEN
            NUM='NaN'
        ELSEIF(DVAL >= inf) THEN
            NUM = 'inf'
        ELSEIF(DVAL <= ninf) THEN
            NUM = '-inf'
        ELSEIF(DVAL==DZ .OR. DVAL==UNO)         THEN
           WRITE(NUM,'(F3.1)') DVAL             
        ELSEIF(DVAL>=1.D100 .OR. DVAL<=-1.D100) THEN
           WRITE(NUM,'(ES18.10E3)') DVAL        
        ELSEIF(DVAL>=1.D10 .OR. DVAL<=-1.D10)   THEN
           WRITE(NUM,'(ES18.11E2)') DVAL
        !ELSEIF( DVAL10 == AINT(DVAL10) .AND. (DVAL10>=UNO .OR. DVAL10<=DNEG) ) THEN
        !   WRITE(NUM,'(F18.1)') DVAL
        ELSEIF( DVAL100 == AINT(DVAL100) .AND. (DVAL100>=UNO.OR.DVAL100<=DNEG) ) THEN
           WRITE(NUM,'(F18.2)') DVAL
        ELSEIF(DVAL>=1.D6 .OR. DVAL<=-1.D6)             THEN
           WRITE(NUM,'(ES18.12E1)') DVAL                
        ELSEIF(DVAL>=1.D2 .OR. DVAL<=-1.D2 )            THEN
           WRITE(NUM,'(F18.8)') DVAL
        ELSEIF(DVAL>=0.00099D0 .OR. DVAL<=-0.00099D0 )  THEN
           WRITE(NUM,'(F18.8)') DVAL
        ELSEIF(DVAL>=1.D-9 .OR. DVAL<=-1.D-9)           THEN
           WRITE(NUM,'(ES18.12E1)') DVAL                
        ELSEIF(DVAL>=1.D-99 .OR. DVAL<=-1.D-99)         THEN
           WRITE(NUM,'(ES18.11E2)') DVAL
        ELSEIF(DVAL>DZ .OR. DVAL<DZ)                    THEN
           WRITE(NUM,'(ES18.10E3)') DVAL
        END IF
        !
        NUM = ADJUSTL(NUM)
        !
        IF( LEN_TRIM(NUM) < ABS(PAD)) THEN
                IF (PAD>0) THEN
                           STR = TRIM( REPEAT(' ',PAD-LEN_TRIM(NUM))//NUM )
                ELSE
                           STR = TRIM(NUM)//REPEAT(' ',ABS(PAD)-LEN_TRIM(NUM))
                END IF
        ELSE
                           STR = TRIM(NUM)
        END IF
    END IF
    !
    IF(PRESENT(LS)) THEN
            IF(LS .AND. STR(1:1) /= BLNK) STR = BLNK//STR
    END IF
    !
    IF(PRESENT(RS)) THEN
            IF(RS .AND. STR(LEN(STR):LEN(STR)) /= BLNK) STR = STR//BLNK
    END IF
    !
  END FUNCTION
  !
  !
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !  
  !  REAL To String with 7 digit output
  !
  !##########################################################################################################################
  ! REAL(rel32) To String with 7 digit output
  !
  PURE FUNCTION REL32_2STR7(RVAL,PAD,LS,RS) RESULT(STR)
    REAL(rel32),       INTENT(IN):: RVAL
    INTEGER, OPTIONAL, INTENT(IN):: PAD
    LOGICAL, OPTIONAL, INTENT(IN):: LS  ! If TRUE and there is no space on the front, add 1 space to end
    LOGICAL, OPTIONAL, INTENT(IN):: RS  ! If TRUE and there is no space on the end,   add 1 space to end
    CHARACTER(:)   ,  ALLOCATABLE:: STR
    REAL(rel64):: DVAL
    !
    IF(RVAL /= RVAL) THEN
        DVAL = REAL(RVAL, rel64)
    ELSEIF(RVAL >= inf_R) THEN
        DVAL = inf
    ELSEIF(RVAL <= ninf_R) THEN
        DVAL = ninf
    ELSEIF(RVAL==0.0_rel32)    THEN
        DVAL = DZ
    ELSEIF(RVAL==1.0_rel32)   THEN
        DVAL = UNO
    ELSE
        DVAL = REAL(RVAL, rel64)
    END IF
    !
    STR = REL64_2STR7(DVAL,PAD,LS,RS)
    !
  END FUNCTION
  !
  !##########################################################################################################################
  ! REAL(rel64) To String with 7 digit output
  !
  PURE FUNCTION REL64_2STR7(DVAL,PAD,LS,RS) RESULT(STR)
    REAL(rel64),       INTENT(IN):: DVAL
    INTEGER, OPTIONAL, INTENT(IN):: PAD
    LOGICAL, OPTIONAL, INTENT(IN):: LS  ! If TRUE and there is no space on the front, add 1 space to end
    LOGICAL, OPTIONAL, INTENT(IN):: RS  ! If TRUE and there is no space on the end,   add 1 space to end
    CHARACTER(:)   ,  ALLOCATABLE:: STR
    CHARACTER(16)::NUM !LARGEST POSSIBLE NUMBER IS 14 CHARACTERS
    !
    NUM=''
    !DVAL10 = DIEZ*DVAL !FOR CHECKING IF THE SAME TO THE NEARIEST
    !
    IF(DVAL /= DVAL) THEN
        NUM='NaN'
    ELSEIF(DVAL >= inf) THEN
        NUM = 'inf'
    ELSEIF(DVAL <= ninf) THEN
        NUM = '-inf'
    ELSE
    !
    IF(DVAL==DZ .OR. DVAL==UNO)                 THEN
       WRITE(NUM,'(F16.7)') DVAL
    ELSEIF(DVAL>=1.D100 .OR. DVAL<=-1.D100)     THEN
       WRITE(NUM,'(ES16.7E3)') DVAL
    ELSEIF(DVAL>=1.D10 .OR. DVAL<=-1.D10)       THEN
       WRITE(NUM,'(ES16.7E2)') DVAL
    !ELSEIF( DVAL10 == AINT(DVAL10) .AND. (DVAL10>=UNO .OR. DVAL10<=DNEG) ) THEN
    !   WRITE(NUM,'(F16.1)') DVAL
    ELSEIF(DVAL>=1.D5 .OR. DVAL<=-1.D5)            THEN
       WRITE(NUM,'(ES16.7E1)') DVAL
    ELSEIF(DVAL>=1.D2 .OR. DVAL<=-1.D2 )           THEN
       WRITE(NUM,'(F16.7)') DVAL
    ELSEIF(DVAL>=0.00099D0 .OR. DVAL<=-0.00099D0 ) THEN
       WRITE(NUM,'(F16.7)') DVAL
    ELSEIF(DVAL>=1.D-9 .OR. DVAL<=-1.D-9)          THEN
       WRITE(NUM,'(ES16.7E1)') DVAL
    ELSEIF(DVAL>=1.D-99 .OR. DVAL<=-1.D-99)        THEN
       WRITE(NUM,'(ES16.7E2)') DVAL
    ELSE!IF(DVAL>DZ .OR. DVAL<DZ)              THEN
       WRITE(NUM,'(ES16.7E3)') DVAL
    END IF
    END IF
    !
    IF(PRESENT(PAD)) THEN
        !
        NUM = ADJUSTL(NUM)
        !
        IF( LEN_TRIM(NUM) < ABS(PAD)) THEN
                IF (PAD>0) THEN
                           STR = TRIM( REPEAT(' ',PAD-LEN_TRIM(NUM))//NUM )
                ELSE
                           STR = TRIM(NUM)//REPEAT(' ',ABS(PAD)-LEN_TRIM(NUM))
                END IF
        ELSE
                           STR = TRIM(NUM)
        END IF
    ELSE
        STR = TRIM(ADJUSTL(NUM))
    END IF
    !
    IF(PRESENT(LS)) THEN
            IF(LS .AND. STR(1:1) /= BLNK) STR = BLNK//STR
    END IF
    !
    IF(PRESENT(RS)) THEN
            IF(RS .AND. STR(LEN(STR):LEN(STR)) /= BLNK) STR = STR//BLNK
    END IF
    !
  END FUNCTION
  !
  !
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !  
  !  String with base word that gets appended with sequential numbers
  !
  !  such as PRE="BASE_" and SEQ_END=3 would return: "BASE_1BASE_2BASE_3"
  !          and if SEP=" "            would return: "BASE_1 BASE_2 BASE_3"
  !
  !##########################################################################################################################
  ! REAL(rel32) To String with 7 digit output
  !
  PURE FUNCTION SEQ2STR_INT32_RNG(PRE,SEQ_END,WIDTH,SEP,START,PAD) RESULT(STR)
    CHARACTER(*),          INTENT(IN):: PRE     ! Base Name that is Post-Pendended with sequential number
    INTEGER,               INTENT(IN):: SEQ_END ! Sequence is from 1 to SEQ_END
    INTEGER,     OPTIONAL, INTENT(IN):: WIDTH   ! Space reserved for PRE_SEQ, so WIDTH = 10 -> "     PRE_1", WID<0 is left justified
    CHARACTER(*),OPTIONAL, INTENT(IN):: SEP     ! Separator between each PRE_SEQ written entry
    INTEGER,     OPTIONAL, INTENT(IN):: START   ! Sequence is from START to SEQ_END; PAD flow
    INTEGER,     OPTIONAL, INTENT(IN):: PAD     ! Zero Pad squence number, so PAD=3 would result in 001, then 002
    !
    CHARACTER(:),   ALLOCATABLE:: STR
    CHARACTER(:),   ALLOCATABLE:: WORD
    INTEGER:: I, N, ISTR, W, NSEP
    LOGICAL:: HAS_SEP
    !
    HAS_SEP = PRESENT(SEP)
    !
    IF(PRESENT(START)) THEN
        ISTR = START
    ELSE
        ISTR = 1
    END IF
    !
    IF(PRESENT(WIDTH)) THEN
           W = WIDTH
    ELSE
           W = 0
    END IF
    !
    IF(HAS_SEP) THEN
        NSEP = LEN(SEP)
    ELSE
        NSEP = 0
    END IF

    !
    N = SEQ_END - ISTR + 1
    !
    IF(N < 1) THEN
        STR = PRE
    ELSE
        WORD = PRE // INT2STR(ISTR,PAD,TRUE)
        !
        IF     (W>0) THEN
                            WORD = TRIM( REPEAT(' ',W-LEN_TRIM(WORD)-NSEP)//WORD )
        ELSEIF (W<0) THEN
                            WORD = TRIM(WORD)//REPEAT(' ',ABS(W)-LEN_TRIM(WORD)-NSEP)
        END IF
        !
        STR  = WORD
        ISTR = ISTR + 1
    END IF
    !
    IF(N>1) THEN
          DO I=ISTR, SEQ_END
              !
              WORD = PRE // INT2STR(I,PAD,TRUE)
              !
              IF     (W>0) THEN
                                  WORD = TRIM( REPEAT(' ',W-LEN_TRIM(WORD)-NSEP)//WORD )
              ELSEIF (W<0) THEN
                                  WORD = TRIM(WORD)//REPEAT(' ',ABS(W)-LEN_TRIM(WORD)-NSEP)
              END IF
              !
              IF(HAS_SEP) WORD=SEP//WORD
              !
              STR = STR//WORD
              !
          END DO
    END IF
    !
  END FUNCTION
  !
  !#########################################################################################################################
  !
  PURE FUNCTION SEQ2STR_INT32_VEC(PRE, SEQ, WIDTH, SEP, PAD) RESULT(STR)
    CHARACTER(*),                      INTENT(IN):: PRE   ! Base Name that is Post-Pendended with sequential number
    INTEGER, DIMENSION(:), CONTIGUOUS, INTENT(IN):: SEQ   ! Vector of integers to post-pend to PRE
    INTEGER,     OPTIONAL,             INTENT(IN):: WIDTH ! Space reserved for PRE_SEQ, so WIDTH = 10 -> "     PRE_1", WID<0 is left justified
    CHARACTER(*),OPTIONAL,             INTENT(IN):: SEP   ! Separator between each PRE_SEQ written entry
    INTEGER,     OPTIONAL,             INTENT(IN):: PAD   ! Zero Pad squence number, so PAD=3 would result in 001, then 002
    !
    CHARACTER(:),   ALLOCATABLE:: STR
    CHARACTER(:),   ALLOCATABLE:: WORD
    INTEGER:: I, N, W, NSEP
    LOGICAL:: HAS_SEP
    !
    HAS_SEP = PRESENT(SEP)
    !
    IF(PRESENT(WIDTH)) THEN
           W = WIDTH
    ELSE
           W = 0
    END IF
    !
    IF(HAS_SEP) THEN
        NSEP = LEN(SEP)
    ELSE
        NSEP = 0
    END IF
    !
    N = SIZE(SEQ)
    !
    IF(N < 1) THEN
        STR = PRE
    ELSE
        WORD = PRE // INT2STR(SEQ(1),PAD,TRUE)
        !
        IF     (W>0) THEN
                            WORD = TRIM( REPEAT(' ',W-LEN_TRIM(WORD)-NSEP)//WORD )
        ELSEIF (W<0) THEN
                            WORD = TRIM(WORD)//REPEAT(' ',ABS(W)-LEN_TRIM(WORD)-NSEP)
        END IF
        !
        STR  = WORD
    END IF
    !
    IF(N>1) THEN
          DO I=2, N
              !
              WORD = PRE // INT2STR(SEQ(I),PAD,TRUE)
              !
              IF     (W>0) THEN
                                  WORD = TRIM( REPEAT(' ',W-LEN_TRIM(WORD)-NSEP)//WORD )
              ELSEIF (W<0) THEN
                                  WORD = TRIM(WORD)//REPEAT(' ',ABS(W)-LEN_TRIM(WORD)-NSEP)
              END IF
              !
              IF(HAS_SEP) WORD=SEP//WORD
              !
              STR = STR//WORD
              !
          END DO
    END IF
    !
  END FUNCTION
END MODULE
!
!