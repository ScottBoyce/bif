!
! CODE DEVELOPED BY SCOTT E BOYCE 
!                   CONTACT <seboyce@usgs.gov> or <Boyce@engineer.com>
!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
! 
MODULE IXJ_INSTRUCTION!, ONLY: IXJ_STRUCTURE, IXJ_SINGLE_ENTRY
  !OPENS AN FILE FOR WRITING. PROVIDES GENERIC INTERFACING FOR WRITTING TO FILE. IF OPEN/CLOSE AUOTMATICALLY CLOSES FILES
  !
  USE, INTRINSIC:: IEEE_ARITHMETIC, ONLY: IEEE_VALUE, IEEE_QUIET_NAN
  USE, INTRINSIC:: ISO_FORTRAN_ENV, ONLY: SNG => REAL32, DBL => REAL64
  USE CONSTANTS,                        ONLY: DZ, Z, ONE, TWO, FOUR, SIX, EIGHT, TEN, TRUE, FALSE, BLNK, NL, BLN
  USE STRINGS,                          ONLY: GET
  USE ERROR_INTERFACE,                  ONLY: STOP_ERROR
  USE NUM2STR_INTERFACE,                ONLY: NUM2STR
  USE ARRAY_DATA_TYPES,                 ONLY: COMPRESSED_VALUE_STORAGE, COMPRESSED_LOCATION_STORAGE
  USE ALLOC_INTERFACE,                  ONLY: ALLOC
  USE GENERIC_BLOCK_READER_INSTRUCTION, ONLY: GENERIC_BLOCK_READER
  USe GENERIC_INPUT_FILE_INSTRUCTION,   ONLY: GENERIC_INPUT_FILE
  !
  IMPLICIT NONE
  PRIVATE
  PUBLIC:: IXJ_STRUCTURE, IXJ_SINGLE_ENTRY
  !
  TYPE IXJ_SINGLE_ENTRY
      INTEGER,   DIMENSION(:), ALLOCATABLE:: I
      REAL(DBL), DIMENSION(:), ALLOCATABLE:: X
      INTEGER,   DIMENSION(:), ALLOCATABLE:: J
      !
  CONTAINS
      !
      GENERIC             :: ALLOC         => ALLOCATE_IXJ_SINGLE_ENTRY, ALLOCATE_IXJ_SINGLE_ENTRY_ELM
      GENERIC             :: GET_DIM       => GET_IXJ_SINGLE_ENTRY_DIM,      &  !(DIM)
                                              GET_IXJ_SINGLE_ENTRY_DIM_I_X_J    !([I],[X],[J])
      PROCEDURE, PASS(DAT):: MOVE          => MOVE_IXJ_SINGLE_ENTRY! (DAT_TO)
      PROCEDURE, PASS(DAT):: DESTROY       => DESTROY_IXJ_SINGLE_ENTRY
      GENERIC             :: ASSIGNMENT(=) => COPY_IXJ_SINGLE_ENTRY
      GENERIC             :: OPERATOR(==)  => EQUALITY_IXJ_SINGLE_ENTRY
      !
      PROCEDURE, PASS(DAT),PRIVATE:: ALLOCATE_IXJ_SINGLE_ENTRY
      PROCEDURE, PASS(DAT),PRIVATE:: ALLOCATE_IXJ_SINGLE_ENTRY_ELM
      PROCEDURE,           PRIVATE:: COPY_IXJ_SINGLE_ENTRY
      PROCEDURE,           PRIVATE:: EQUALITY_IXJ_SINGLE_ENTRY
      PROCEDURE, PASS(DAT),PRIVATE:: GET_IXJ_SINGLE_ENTRY_DIM
      PROCEDURE, PASS(DAT),PRIVATE:: GET_IXJ_SINGLE_ENTRY_DIM_I_X_J
      !
      FINAL:: FINAL_IXJ_SINGLE_ENTRY  !-- Causes compile error in GFORTRAN
      !
  END TYPE
  !
  TYPE IXJ_STRUCTURE
      !
      INTEGER:: N    = Z   !Count that is currently loaded
      INTEGER:: SIZ  = Z   !Size of DAT that is currently allocated
      !
      LOGICAL:: IS_CONSTANT = FALSE  !IF TRUE, THEN N = 1 and all intries equal CONSTANT VALUE
      !
      INTEGER:: P = ONE !SCRATCH VAR FOR POSITION
      INTEGER:: IOUT = Z
      !
      INTEGER, DIMENSION(3):: DIM = Z !HOLDS DIM of I, X, J
      !
      TYPE(IXJ_SINGLE_ENTRY), DIMENSION(:), ALLOCATABLE:: DAT
      !
      CHARACTER(:), ALLOCATABLE:: ERRMSG
      !
      CONTAINS
      !
      GENERIC             :: INIT     => INIT_IXJ_STRUCTURE,  &      !INIT(DIM1, DIM2, DIM3, OUTPUT, [SIZ], [EXACT])
                                         INIT_IXJ_STRUCTURE_DIMVEC   !INIT(DIM,              OUTPUT, [SIZ], [EXACT])
      GENERIC             :: LOAD     => LOAD_IXJ_STRUCTURE_LINE,  & !LOAD(LINE, LLOC, [INFILE  ], [OUTPUT], [SCALE], [NOSTOP], [MSG])
                                         LOAD_IXJ_STRUCTURE_IU       !LOAD(IU,         [ERROR_IU], [OUTPUT], [NOSTOP], [CONST], [FIRST_LINE], [MSG])
      !
      PROCEDURE, PASS(IXJ):: STR      => PRINT_IXJ_STRUCTURE         !STR([P], [PAD_INT], [PAD_DBL]) 
      !
      PROCEDURE, PASS(IXJ):: COUNT_ID => COUNT_INT_IXJ_STRUCTURE     !COUNT_ID(ID, ICOL, CNT)
      PROCEDURE, PASS(IXJ):: GET_VAL  => GET_POS_P_VAL_PART          !GET_VAL(ICOL) RESULT(VAL)
      PROCEDURE, PASS(IXJ):: GET_VALUE_IXJ                           !GET_VALUE_IXJ(VAL, IVAL, ID, ID_COL, ROW, IROW, COL, ICOL, FOUND, FROM_START)
      !
      PROCEDURE, PASS(IXJ):: MOVE_POS_P_TO_RECORD          !(FOUND, FROM_START, ID, ID_COLNUM, ROW, IROW, COL, ICOL)
      !
      PROCEDURE, PASS(IXJ):: START    => SET_POS_P_TO_START    !([EOF])
      PROCEDURE, PASS(IXJ):: NEXT     => MOVE_POS_P_TO_NEXT    !([EOF])
      !
      PROCEDURE, PASS(IXJ):: START_ID => SET_POS_P_TO_START_ID !START_ID(ID, ICOL, [EOF]) 
      PROCEDURE, PASS(IXJ):: NEXT_ID  => MOVE_POS_P_TO_NEXT_ID !NEXT_ID (ID, ICOL, [EOF])
      ! 
      PROCEDURE, PASS(IXJ):: MATCH_INT=> MATCH_POS_P_INT_PART     !(ID1, C1, [ID2, C2], [ID3, C3], [ID4, C4])
      !
      GENERIC             :: SORT_BY_I=> SORT_BY_INT_COLVEC,   &  ! SORT_BY_I(IXJ, ICOL)   ICOL is DIM(:)
                                         SORT_BY_INT_COL,      &  ! SORT_BY_I(IXJ, ICOL) 
                                         SORT_BY_INT_COL1_COL2    ! SORT_BY_I(IXJ, ICOL1, ICOL2)
      !
      GENERIC             :: TO_ARRAY     => COPY_TO_2D_ARRAY_DBL, & !TO_ARRAY(IROW, ICOL, VPOS, DIM1, DIM2, ARR, ERROR)
                                             COPY_TO_2D_ARRAY_INT    !TO_ARRAY(IROW, ICOL, IPOS, DIM1, DIM2, ARR, ERROR)
      !
      GENERIC             :: TO_CVS      => BUILD_COMPRESSED_VALUE_STORAGE_TYPE_DIM2, & !TO_CVS(IDIM1, IDIM2, VPOS, CVS)
                                            BUILD_COMPRESSED_VALUE_STORAGE_TYPE_DIM3    !TO_CVS(IDIM1, IDIM2, IDIM3, VPOS, CVS)
      !
      PROCEDURE, PASS(IXJ):: TO_LAC      => BUILD_COMPRESSED_LOCATION_STORAGE_TYPE      !(IDIM1, IDIM2, IDIM_ID, ID, LAC) 
      PROCEDURE, PASS(IXJ):: TO_LAC_BYDIM=> BUILD_COMPRESSED_LOCATION_STORAGE_TYPE_BYDIM!(IDIM1, IDIM2, IDIM_ID,     LAC)
      !
      PROCEDURE, PASS(IXJ):: DESTROY  => DESTROY_IXJ_STRUCTURE !(IU)
      !
      PROCEDURE, PASS(IXJ), PRIVATE:: LOAD_IXJ_STRUCTURE_IU  !(IU, [ERROR_IU], [OUTPUT], [NOSTOP], [CONST], [FIRST_LINE], [MSG])
      PROCEDURE, PASS(IXJ), PRIVATE:: LOAD_IXJ_STRUCTURE_LINE!(LINE, LLOC, [INFILE], [OUTPUT], [SCALE], [NOSTOP], [MSG])
      !
      PROCEDURE, PASS(IXJ), PRIVATE:: INIT_IXJ_STRUCTURE
      PROCEDURE, PASS(IXJ), PRIVATE:: INIT_IXJ_STRUCTURE_DIMVEC
      !
      PROCEDURE, PASS(IXJ), PRIVATE:: SORT_BY_INT_COLVEC   !(IXJ, ICOL)
      PROCEDURE, PASS(IXJ), PRIVATE:: SORT_BY_INT_COL      !(IXJ, ICOL) 
      PROCEDURE, PASS(IXJ), PRIVATE:: SORT_BY_INT_COL1_COL2!(IXJ, ICOL1, ICOL2)
      !
      PROCEDURE, PASS(IXJ), PRIVATE:: COPY_TO_2D_ARRAY_DBL !(IROW, ICOL, VPOS, DIM1, DIM2, ARR, ERROR)
      PROCEDURE, PASS(IXJ), PRIVATE:: COPY_TO_2D_ARRAY_INT !(IROW, ICOL, IPOS, DIM1, DIM2, ARR, ERROR)
      !
      PROCEDURE, PASS(IXJ), PRIVATE:: BUILD_COMPRESSED_VALUE_STORAGE_TYPE_DIM2 !(IDIM1, IDIM2, VPOS, CVS)
      PROCEDURE, PASS(IXJ), PRIVATE:: BUILD_COMPRESSED_VALUE_STORAGE_TYPE_DIM3 !(IDIM1, IDIM2, IDIM3, VPOS, CVS)
      !
      !!!PROCEDURE, PASS(IXJ):: GET_J_IXJ                   !(J,   IVAL, ID, ID_COL, ROW, IROW, COL, ICOL, FOUND, FROM_START)
      !
      FINAL::                           FINAL_DESTROY_IXJ_STRUCTURE
  END TYPE
  !
  CONTAINS
  !
  ! #############################################################################################################################
  ! #############################################################################################################################
  ! #############################################################################################################################
  !
  PURE FUNCTION PRINT_IXJ_STRUCTURE(IXJ, P, PAD_INT, PAD_DBL) RESULT(STR)
    CLASS(IXJ_STRUCTURE), INTENT(IN):: IXJ
    INTEGER,    OPTIONAL, INTENT(IN):: P, PAD_INT, PAD_DBL
    CHARACTER(:),        ALLOCATABLE:: STR
    INTEGER:: I
    !
    IF(PRESENT(P)) THEN
                    STR = PRINT_IXJ_STRUCTURE_BY_P(IXJ, P, PAD_INT, PAD_DBL)
    ELSE
        STR = PRINT_IXJ_STRUCTURE_BY_P(IXJ, ONE, PAD_INT, PAD_DBL)
        !
        DO I=TWO, IXJ%N
                  STR = STR//NL//PRINT_IXJ_STRUCTURE_BY_P(IXJ, I, PAD_INT, PAD_DBL)
        END DO
    END IF
    !
  END FUNCTION
  !
  PURE FUNCTION PRINT_IXJ_STRUCTURE_BY_P(IXJ, P, PAD_INT, PAD_DBL) RESULT(STR)
    CLASS(IXJ_STRUCTURE), INTENT(IN):: IXJ
    INTEGER,              INTENT(IN):: P
    INTEGER,    OPTIONAL, INTENT(IN):: PAD_INT, PAD_DBL
    CHARACTER(:),        ALLOCATABLE:: STR
    INTEGER:: I, PD1, PD2
    !
    STR = ' '
    !
    IF(SUM(IXJ%DIM) > Z) THEN
        !
        PD1 = SIX
        PD2 = TEN
        IF(PRESENT(PAD_INT)) PD1 = PAD_INT
        IF(PRESENT(PAD_DBL)) PD2 = PAD_DBL
        !
        DO I=ONE, IXJ%DIM(1); STR = STR//NUM2STR(IXJ%DAT(P)%I(I),PD1)//', '
        END DO
        !
        DO I=ONE, IXJ%DIM(2); STR = STR//NUM2STR(IXJ%DAT(P)%X(I),PD2)//', '
        END DO
        !
        DO I=ONE, IXJ%DIM(3); STR = STR//NUM2STR(IXJ%DAT(P)%J(I),PD1)//', '
        END DO
        !
        I = LEN(STR) - TWO
        !
        STR = STR(TWO:I)
    END IF
    !
  END FUNCTION
  !
  ! -----------------------------------------------------------------------------------------------------------------------------
  !
  PURE SUBROUTINE INIT_IXJ_STRUCTURE_DIMVEC(IXJ, DIM, OUTPUT, SIZ, EXACT)
    CLASS(IXJ_STRUCTURE),      INTENT(INOUT):: IXJ
    INTEGER,    DIMENSION(3),  INTENT(IN   ):: DIM
    INTEGER,         OPTIONAL, INTENT(IN   ):: OUTPUT
    INTEGER,         OPTIONAL, INTENT(IN   ):: SIZ
    LOGICAL,         OPTIONAL, INTENT(IN   ):: EXACT
    !
    CALL INIT_IXJ_STRUCTURE(IXJ, DIM(1), DIM(2), DIM(3), OUTPUT, SIZ, EXACT)
    !
  END SUBROUTINE
  !
  ! -----------------------------------------------------------------------------------------------------------------------------
  !
  PURE SUBROUTINE INIT_IXJ_STRUCTURE(IXJ, DIM1, DIM2, DIM3, OUTPUT, SIZ, EXACT)
    CLASS(IXJ_STRUCTURE), INTENT(INOUT):: IXJ
    INTEGER,              INTENT(IN   ):: DIM1, DIM2, DIM3
    INTEGER,    OPTIONAL, INTENT(IN   ):: OUTPUT
    INTEGER,    OPTIONAL, INTENT(IN   ):: SIZ
    LOGICAL,    OPTIONAL, INTENT(IN   ):: EXACT
    !
    IF(PRESENT(OUTPUT)) THEN;  IF(OUTPUT.NE.Z) IXJ%IOUT = OUTPUT
    END IF
    !
    IXJ%N = Z
    IXJ%P = ONE
    !
    IXJ%ERRMSG = BLNK
    !
    IXJ%IS_CONSTANT = FALSE
    !
    IF(IXJ%DIM(1).NE.DIM1  .OR. IXJ%DIM(2).NE.DIM2  .OR. IXJ%DIM(3).NE.DIM3) THEN
        !
        CALL DESTROY_IXJ_STRUCTURE_DAT(IXJ)
        !
        IXJ%DIM(1) = DIM1
        IXJ%DIM(2) = DIM2
        IXJ%DIM(3) = DIM3
        !
        IF( IXJ%SIZ > Z ) CALL ALLOCATE_IXJ_SINGLE_ENTRY_ELM(IXJ%DAT, IXJ%DIM(1), IXJ%DIM(2), IXJ%DIM(3))
    END IF
    !
    IF(PRESENT(SIZ)) CALL ALLOCATE_IXJ_DAT(IXJ, SIZ, EXACT)
    ! 
  END SUBROUTINE
  !
  ! -----------------------------------------------------------------------------------------------------------------------------
  !
  PURE SUBROUTINE ALLOCATE_IXJ_DAT(IXJ, SIZ, EXACT, NEW_ALLOC)
    CLASS(IXJ_STRUCTURE),               INTENT(INOUT):: IXJ
    INTEGER,                            INTENT(IN   ):: SIZ
    LOGICAL,                  OPTIONAL, INTENT(IN   ):: EXACT
    LOGICAL,                  OPTIONAL, INTENT(OUT  ):: NEW_ALLOC
    LOGICAL:: EXACT_DIM, NEW_ALLOCATION
    !
    NEW_ALLOCATION = FALSE
    EXACT_DIM      = FALSE
    IF(PRESENT(EXACT)) EXACT_DIM = EXACT
    !
    IF(SIZ > Z) THEN
                  IF( IXJ%SIZ > Z ) THEN
                      IF( IXJ%SIZ < SIZ .OR. (EXACT_DIM .AND. IXJ%SIZ.NE.SIZ) ) THEN
                                                                                 NEW_ALLOCATION = TRUE
                                                                                 IXJ%SIZ        = SIZ
                                                                                 DEALLOCATE(IXJ%DAT)
                                                                                   ALLOCATE(IXJ%DAT(SIZ))
                      END IF
                  ELSE
                      NEW_ALLOCATION = TRUE
                      IXJ%SIZ        = SIZ
                      ALLOCATE(IXJ%DAT(SIZ))
                  END IF
    ELSEIF(EXACT_DIM) THEN
                  IF(ALLOCATED(IXJ%DAT)) THEN
                                         NEW_ALLOCATION = TRUE
                                         IXJ%SIZ        = Z
                                         DEALLOCATE(IXJ%DAT)
                  END IF
    END IF
    !
    IF(NEW_ALLOCATION) THEN
                       IXJ%N = Z
                       CALL ALLOCATE_IXJ_SINGLE_ENTRY_ELM(IXJ%DAT, IXJ%DIM(1), IXJ%DIM(2), IXJ%DIM(3))
    END IF
    !
    IF(PRESENT(NEW_ALLOC)) NEW_ALLOC=NEW_ALLOCATION
    !
  END SUBROUTINE
  !
  ! -----------------------------------------------------------------------------------------------------------------------------
  !
  PURE SUBROUTINE MOVE_ALLOCATE_IXJ_DAT(DAT_FROM, DAT_TO)                           !Internal utility routine
    TYPE(IXJ_SINGLE_ENTRY), DIMENSION(:), ALLOCATABLE, INTENT(INOUT):: DAT_FROM
    TYPE(IXJ_SINGLE_ENTRY), DIMENSION(:), ALLOCATABLE, INTENT(INOUT):: DAT_TO
    !
    INTEGER:: SIZ
    !
    SIZ = SIZE(DAT_FROM)
    IF    (.NOT. ALLOCATED(DAT_FROM)) THEN
                                      SIZ = Z
                                      IF(ALLOCATED(DAT_TO)) DEALLOCATE(DAT_TO)
    ELSEIF(ALLOCATED(DAT_TO)) THEN
        IF(SIZ.NE.SIZE(DAT_TO)) THEN
                                           DEALLOCATE(DAT_TO)
                                             ALLOCATE(DAT_TO(SIZ))
        END IF
    ELSE
        ALLOCATE(DAT_TO(SIZ))
    END IF
    !
    IF(SIZ > Z) CALL MOVE_IXJ_SINGLE_ENTRY(DAT_FROM, DAT_TO)
    !
    IF(ALLOCATED(DAT_FROM)) DEALLOCATE(DAT_FROM)
    !
  END SUBROUTINE
  !
  ! -----------------------------------------------------------------------------------------------------------------------------
  !
  PURE SUBROUTINE CHECK_SIZE_AND_REALLOCATE_IXJ_DAT(IXJ, SIZ, EXACT, NEW_ALLOC)     !Internal utility routine
    CLASS(IXJ_STRUCTURE),               INTENT(INOUT):: IXJ
    INTEGER,                            INTENT(IN   ):: SIZ
    LOGICAL,                  OPTIONAL, INTENT(IN   ):: EXACT
    LOGICAL,                  OPTIONAL, INTENT(OUT  ):: NEW_ALLOC
    TYPE(IXJ_SINGLE_ENTRY), DIMENSION(:), ALLOCATABLE:: TMP
    LOGICAL:: EXACT_DIM, NEW_ALLOCATION
    !
    NEW_ALLOCATION = FALSE
    EXACT_DIM      = FALSE
    IF(PRESENT(EXACT)) EXACT_DIM = EXACT
    !
    IF(SIZ > Z) THEN
                  IF( IXJ%SIZ > Z ) THEN
                      IF( IXJ%SIZ < SIZ .OR. (EXACT_DIM .AND. IXJ%SIZ.NE.SIZ) ) THEN
                                                                                 NEW_ALLOCATION = TRUE
                                                                                 CALL MOVE_ALLOCATE_IXJ_DAT(IXJ%DAT, TMP)
                                                                                 !
                                                                                 ALLOCATE(IXJ%DAT(SIZ))
                                                                                 !
                                                                                 CALL MOVE_IXJ_SINGLE_ENTRY(TMP, IXJ%DAT(:IXJ%SIZ))
                                                                                 !
                                                                                 CALL ALLOCATE_IXJ_SINGLE_ENTRY_ELM(IXJ%DAT(IXJ%SIZ+1:), IXJ%DIM(1), IXJ%DIM(2), IXJ%DIM(3))
                                                                                 IXJ%SIZ = SIZ
                      END IF
                  ELSE
                      NEW_ALLOCATION = TRUE
                      IXJ%SIZ        = SIZ
                      ALLOCATE(IXJ%DAT(SIZ))
                      CALL ALLOCATE_IXJ_SINGLE_ENTRY_ELM(IXJ%DAT, IXJ%DIM(1), IXJ%DIM(2), IXJ%DIM(3))
                  END IF
    ELSEIF(EXACT_DIM) THEN
                  IF(ALLOCATED(IXJ%DAT)) THEN
                                         NEW_ALLOCATION = TRUE
                                         IXJ%SIZ        = Z
                                         DEALLOCATE(IXJ%DAT)
                  END IF
    END IF
    !
    IF(PRESENT(NEW_ALLOC)) NEW_ALLOC=NEW_ALLOCATION
    !
  END SUBROUTINE
  !
  ! -----------------------------------------------------------------------------------------------------------------------------
  !
  SUBROUTINE LOAD_IXJ_STRUCTURE_LINE(IXJ, LINE, LLOC, INFILE, OUTPUT, SCALE, NOSTOP, MSG, DIM)
    CLASS(IXJ_STRUCTURE),    INTENT(INOUT):: IXJ
    CHARACTER(*),            INTENT(IN   ):: LINE
    INTEGER,                 INTENT(INOUT):: LLOC
    INTEGER,      OPTIONAL,  INTENT(IN   ):: INFILE
    REAL(DBL),    OPTIONAL,  INTENT(INOUT):: SCALE
    INTEGER,      OPTIONAL,  INTENT(IN   ):: OUTPUT
    LOGICAL,      OPTIONAL,  INTENT(IN   ):: NOSTOP
    CHARACTER(*), OPTIONAL,  INTENT(IN   ):: MSG
    INTEGER,      OPTIONAL,  DIMENSION(3), INTENT(IN):: DIM
    TYPE(GENERIC_INPUT_FILE):: FL
    LOGICAL:: NO_SCALE
    !
    IF(PRESENT(DIM)) THEN;  IF( ANY(DIM.NE.IXJ%DIM) ) CALL IXJ%INIT(DIM)
    END IF
    !
    NO_SCALE = .not. PRESENT(SCALE)
    !
    CALL FL%OPEN(LINE, LLOC, OUTPUT, INFILE, NO_SCALE=NO_SCALE)
    !
    IF    (FL%NULL_FILE) THEN
                    IXJ%N = Z
    ELSEIF(FL%IS_CONSTANT) THEN
                           CALL LOAD_IXJ_STRUCTURE_IU(IXJ, FL%IU, INFILE, OUTPUT, NOSTOP, FL%CONST, MSG=MSG)
    ELSE
        CALL LOAD_IXJ_STRUCTURE_IU(IXJ, FL%IU, INFILE, OUTPUT, NOSTOP, MSG=MSG)
    END IF    
    !
    IF(PRESENT(SCALE)) SCALE = SCALE*FL%SCALE
    !
  END SUBROUTINE
  !
  ! -----------------------------------------------------------------------------------------------------------------------------
  !
  SUBROUTINE LOAD_IXJ_STRUCTURE_IU(IXJ, IU, ERROR_IU, OUTPUT, NOSTOP, CONST, FIRST_LINE, MSG, DIM)
    CLASS(IXJ_STRUCTURE),    INTENT(INOUT):: IXJ
    INTEGER,                 INTENT(IN   ):: IU
    INTEGER,      OPTIONAL,  INTENT(IN   ):: ERROR_IU, OUTPUT
    LOGICAL,      OPTIONAL,  INTENT(IN   ):: NOSTOP
    REAL(DBL),    OPTIONAL,  INTENT(IN   ):: CONST
    CHARACTER(*), OPTIONAL,  INTENT(IN   ):: FIRST_LINE, MSG
    INTEGER,      OPTIONAL,  DIMENSION(3), INTENT(IN   ):: DIM  !Resets IXJ%DIM if necessary
    TYPE(GENERIC_BLOCK_READER):: BL
    INTEGER:: LINE_LEN, ERR_IU
    INTEGER:: LLOC, ISTART, ISTOP, I
    LOGICAL:: END_NOT_FOUND, ALLOW_ERROR, HAS_ERROR
    CHARACTER(FOUR):: WORD
    !
    IF(PRESENT(DIM)) THEN;  IF( ANY(DIM.NE.IXJ%DIM) ) CALL IXJ%INIT(DIM)
    END IF
    !
    IF(PRESENT(OUTPUT)) THEN;  IF(OUTPUT.NE.Z) IXJ%IOUT = OUTPUT
    END IF
    !
    IF(PRESENT(FIRST_LINE)) THEN
                            LLOC = ONE
                            CALL GET(FIRST_LINE,LLOC,ISTART,ISTOP,WORD)
    ELSE;                                                         WORD = BLNK
    END IF
    !
    IXJ%IS_CONSTANT = FALSE
    !
    IF(PRESENT(CONST)) THEN
             !
             IXJ%IS_CONSTANT = TRUE
             !
             CALL CHECK_SIZE_AND_REALLOCATE_IXJ_DAT(IXJ, ONE)
             !
             IXJ%N = ONE
             I = INT(CONST)
             !
             IF(IXJ%DIM(1) > Z) IXJ%DAT(1)%I = I        ! I = INT(CONST)
             IF(IXJ%DIM(2) > Z) IXJ%DAT(1)%X = CONST
             IF(IXJ%DIM(3) > Z) IXJ%DAT(1)%J = I        ! I = INT(CONST)
             !
    ELSEIF(WORD.NE.'STOP' .AND. IU .NE. Z) THEN
             IF(PRESENT(ERROR_IU)) THEN
                                   ERR_IU = ERROR_IU
             ELSE
                                   ERR_IU = IU
             END IF
             !
             IF(PRESENT(NOSTOP)) THEN
                 ALLOW_ERROR = .NOT. NOSTOP
                 !
                 IF(NOSTOP) IXJ%ERRMSG = BLNK
             ELSE
                 ALLOW_ERROR = TRUE
             END IF
             !
             LINE_LEN = IXJ%DIM(1)*EIGHT + IXJ%DIM(1)*21 + IXJ%DIM(3)*EIGHT + SUM(IXJ%DIM)*TWO
             !
             IF( LINE_LEN < 96 ) LINE_LEN = 96
             !
             CALL BL%INIT(IU, IXJ%IOUT, LINE_LEN)
             !
             CALL BL%INNER('STOP', END_NOT_FOUND=END_NOT_FOUND)
             !
             IF(PRESENT(FIRST_LINE)) CALL BL%ADD_LINE(FIRST_LINE, ONE)
             !
             CALL CHECK_SIZE_AND_REALLOCATE_IXJ_DAT(IXJ, BL%NLINE)
             !
             IXJ%N = BL%NLINE
             !
             CALL BL%START()
             !
             HAS_ERROR = FALSE
             DO I=ONE, BL%NLINE
                       LLOC = ONE
                       !
                       IF(IXJ%DIM(1) > Z                      ) CALL GET(BL%LINE,LLOC,ISTART,ISTOP,IXJ%IOUT,ERR_IU,IXJ%DAT(I)%I,HAS_ERROR=HAS_ERROR)
                       IF(IXJ%DIM(2) > Z .AND. .NOT. HAS_ERROR) CALL GET(BL%LINE,LLOC,ISTART,ISTOP,IXJ%IOUT,ERR_IU,IXJ%DAT(I)%X,HAS_ERROR=HAS_ERROR)
                       IF(IXJ%DIM(3) > Z .AND. .NOT. HAS_ERROR) CALL GET(BL%LINE,LLOC,ISTART,ISTOP,IXJ%IOUT,ERR_IU,IXJ%DAT(I)%J,HAS_ERROR=HAS_ERROR)
                       !
                       IF(HAS_ERROR) EXIT
                       !
                       CALL BL%NEXT() !MOVE TO NEXT LINE
             END DO
             ! 
             IF(HAS_ERROR) THEN
                 IXJ%ERRMSG = 'IXJ STYLE INPUT FAILED TO LOAD AN INPUT'
                 !
                 IF(.NOT. ALLOW_ERROR) IXJ%ERRMSG = IXJ%ERRMSG//' ON THE FOLLOWING LINE:'//BLN//'"'//BL%LINE//'"'
                 !
                 IXJ%ERRMSG = IXJ%ERRMSG//BLN//'Input expects to read '//NUM2STR(IXJ%DIM(1))//' I integers, '//NUM2STR(IXJ%DIM(2))//' X numbers, and '//NUM2STR(IXJ%DIM(3))//' J integers' 
                 !
                 IF(END_NOT_FOUND) IXJ%ERRMSG = IXJ%ERRMSG//BLN//'NOTE THAT THE KEYWORD "STOP IXJ" WAS NOT FOUND, SO THIS ERROR MAYBE BECAUSE IXJ LOADED TO THE END OF THE FILE RATHER THAN THE END OF THE ACTUAL IXJ INPUT.'
                 !
                 IF(ALLOW_ERROR) CALL STOP_ERROR(BL%LINE, INFILE=ERR_IU, OUTPUT=IXJ%IOUT, MSG=IXJ%ERRMSG, MSG2=MSG)
                 !
             END IF
    ELSE
        CALL ALLOCATE_IXJ_DAT(IXJ, Z)
        IXJ%ERRMSG = BLNK
    END IF
    ! 
  END SUBROUTINE
  !
  ! -----------------------------------------------------------------------------------------------------------------------------
  !
  PURE SUBROUTINE COUNT_INT_IXJ_STRUCTURE(IXJ, ID, ICOL, CNT)
    CLASS(IXJ_STRUCTURE),  INTENT(IN   ):: IXJ
    INTEGER,               INTENT(IN   ):: ID, ICOL
    INTEGER,               INTENT(INOUT):: CNT
    INTEGER:: I
    !
    CNT   = Z
    !
    DO I=ONE, IXJ%N;  IF(IXJ%DAT(I)%I(ICOL) == ID) CNT = CNT + ONE
    END DO
    !
  END SUBROUTINE
  !
  ! -----------------------------------------------------------------------------------------------------------------------------
  !
  PURE SUBROUTINE SET_POS_P_TO_START(IXJ, EOF)
    CLASS(IXJ_STRUCTURE), INTENT(INOUT):: IXJ
    LOGICAL,    OPTIONAL, INTENT(  OUT):: EOF
    !
    IXJ%P = ONE
    !
    IF(PRESENT(EOF)) EOF = IXJ%P > IXJ%N
    !
  END SUBROUTINE
  !
  ! -----------------------------------------------------------------------------------------------------------------------------
  !
  PURE SUBROUTINE MOVE_POS_P_TO_NEXT(IXJ, EOF)
    CLASS(IXJ_STRUCTURE), INTENT(INOUT):: IXJ
    LOGICAL,    OPTIONAL, INTENT(  OUT):: EOF
    !
    IXJ%P = IXJ%P + ONE
    !
    IF(PRESENT(EOF)) EOF = IXJ%P > IXJ%N
    !
  END SUBROUTINE
  !
  ! -----------------------------------------------------------------------------------------------------------------------------
  !
  PURE SUBROUTINE SET_POS_P_TO_START_ID(IXJ, ID, ICOL, EOF)
    CLASS(IXJ_STRUCTURE), INTENT(INOUT):: IXJ
    INTEGER,              INTENT(IN   ):: ID, ICOL
    LOGICAL,    OPTIONAL, INTENT(  OUT):: EOF
    INTEGER:: I
    !
    IXJ%P = IXJ%N + ONE
    !
    DO I=ONE, IXJ%N
           IF(IXJ%DAT(I)%I(ICOL) == ID) THEN
                                        IXJ%P = I
                                        EXIT
           END IF
    END DO
    !
    IF(PRESENT(EOF)) EOF = IXJ%P > IXJ%N
    !
  END SUBROUTINE
  !
  ! -----------------------------------------------------------------------------------------------------------------------------
  !
  PURE SUBROUTINE MOVE_POS_P_TO_NEXT_ID(IXJ, ID, ICOL, EOF)
    CLASS(IXJ_STRUCTURE), INTENT(INOUT):: IXJ
    INTEGER,              INTENT(IN   ):: ID, ICOL
    LOGICAL,    OPTIONAL, INTENT(  OUT):: EOF
    INTEGER:: I, P
    !
    P = IXJ%P + ONE
    !
    IXJ%P = IXJ%N + ONE
    !
    DO I=P, IXJ%N
           IF(IXJ%DAT(I)%I(ICOL) == ID) THEN
                                        IXJ%P = I
                                        EXIT
           END IF
    END DO
    !
    IF(PRESENT(EOF)) EOF = IXJ%P > IXJ%N
    !
  END SUBROUTINE
  !
  ! -----------------------------------------------------------------------------------------------------------------------------
  !
  PURE FUNCTION MATCH_POS_P_INT_PART(IXJ, ID1, C1, ID2, C2, ID3, C3, ID4, C4) RESULT(MATCH)
    CLASS(IXJ_STRUCTURE), INTENT(IN):: IXJ
    INTEGER,              INTENT(IN):: ID1, C1
    INTEGER,    OPTIONAL, INTENT(IN):: ID2, C2, ID3, C3, ID4, C4
    LOGICAL:: MATCH
    !
    IF(IXJ%N == Z) THEN
                   MATCH = FALSE
    ELSE
        ASSOCIATE( IVEC => IXJ%DAT(IXJ%P)%I )
                   !
                   MATCH = ID1 == IVEC(C1)
                   !
                   IF(MATCH .AND. PRESENT(ID2)) THEN
                       !
                       MATCH = ID2 == IVEC(C2)
                       !
                       IF(MATCH .AND. PRESENT(ID3)) THEN
                           !
                           MATCH = ID3 == IVEC(C3)
                           !
                           IF(MATCH .AND. PRESENT(ID4)) THEN
                               !
                               MATCH = ID4 == IVEC(C4)
                               !
                           END IF
                       END IF
                   END IF
        END ASSOCIATE
    END IF
    !
  END FUNCTION
  !
  ! -----------------------------------------------------------------------------------------------------------------------------
  !
  PURE ELEMENTAL FUNCTION GET_POS_P_VAL_PART(IXJ, ICOL) RESULT(VAL)
    CLASS(IXJ_STRUCTURE), INTENT(IN):: IXJ
    INTEGER,    OPTIONAL, INTENT(IN):: ICOL
    REAL(DBL):: VAL
    !
    IF(Z < IXJ%P .AND. IXJ%P <= IXJ%N .AND. IXJ%DIM(2)>Z) THEN
       IF(PRESENT(ICOL)) THEN
           VAL = IXJ%DAT(IXJ%P)%X(ICOL)
       ELSE
           VAL = IXJ%DAT(IXJ%P)%X(ONE)
       END IF
    ELSE
        VAL = IEEE_VALUE(VAL, IEEE_QUIET_NAN)
    END IF
    !
  END FUNCTION
  !
  ! -----------------------------------------------------------------------------------------------------------------------------
  !
  PURE SUBROUTINE MOVE_POS_P_TO_RECORD(IXJ, FOUND, FROM_START, ID, ID_COLNUM, ROW, IROW, COL, ICOL)
    CLASS(IXJ_STRUCTURE),      INTENT(INOUT):: IXJ
    LOGICAL,                   INTENT(OUT  ):: FOUND
    LOGICAL,                   INTENT(IN   ):: FROM_START
    INTEGER,       OPTIONAL,   INTENT(IN   ):: ID, ID_COLNUM, ROW, IROW, COL, ICOL
    INTEGER:: ID_COL, IRR, ICC
    LOGICAL:: EOF
    !
    IF(IXJ%IS_CONSTANT) THEN
        FOUND = TRUE
        IXJ%P = ONE
    ELSE
       IF(PRESENT(ID) .AND. PRESENT(ID_COLNUM)) THEN
           ID_COL = ID_COLNUM
       ELSE
           ID_COL = Z
       END IF
       !
       IF(PRESENT(ROW) .AND. PRESENT(COL)) THEN
           IRR = ROW
           ICC = COL
       ELSE
           IRR = Z
           ICC = Z
       END IF
       !
       IF(ID_COL > Z) THEN
           !
           IF(FROM_START) THEN
                              CALL SET_POS_P_TO_START_ID(IXJ, ID, ID_COL, EOF)
           ELSE
                              CALL MOVE_POS_P_TO_NEXT_ID(IXJ, ID, ID_COL, EOF)
           END IF
           !
           IF(.NOT. EOF .AND. IRR > Z) THEN
               DO WHILE ( .NOT. EOF .AND. .NOT. ( ROW == IXJ%DAT(IXJ%P)%I(IRR) .AND. &
                                                  COL == IXJ%DAT(IXJ%P)%I(ICC))       )
                   !
                   CALL MOVE_POS_P_TO_NEXT_ID(IXJ, ID, ID_COL, EOF)
               END DO
           END IF
           !
       ELSEIF(IRR > Z) THEN
           !
           IF(FROM_START) THEN
                              CALL SET_POS_P_TO_START_ID(IXJ, ROW, IRR, EOF)
           ELSE
                              CALL MOVE_POS_P_TO_NEXT_ID(IXJ, ROW, IRR, EOF)
           END IF
           !
           DO WHILE (.NOT. EOF .AND. COL .NE. IXJ%DAT(IXJ%P)%I(ICC))
                 !
                 CALL MOVE_POS_P_TO_NEXT_ID(IXJ, ROW, IRR, EOF)
           END DO
       END IF
       !
       FOUND = .NOT. EOF
    END IF
    !
  END SUBROUTINE
  !
  ! -----------------------------------------------------------------------------------------------------------------------------
  !
  PURE SUBROUTINE GET_VALUE_IXJ(IXJ, VAL, IVAL, ID, ID_COL, ROW, IROW, COL, ICOL, FOUND, FROM_START)
    CLASS(IXJ_STRUCTURE), INTENT(INOUT):: IXJ
    REAL(DBL),            INTENT(OUT):: VAL
    INTEGER,              INTENT(IN ):: IVAL
    INTEGER,  OPTIONAL,   INTENT(IN ):: ID, ID_COL, ROW, IROW, COL, ICOL
    LOGICAL,  OPTIONAL,   INTENT(OUT):: FOUND
    LOGICAL,  OPTIONAL,   INTENT(IN ):: FROM_START
    LOGICAL:: FND, FRM_STRT
    !
    IF(IXJ%IS_CONSTANT) THEN
                         FND = TRUE
                         VAL = IXJ%DAT(ONE)%X(IVAL)
    ELSE
        IF(PRESENT(FROM_START)) THEN
            FRM_STRT = FROM_START
        ELSE
            FRM_STRT = TRUE
        END IF
        !
        CALL MOVE_POS_P_TO_RECORD(IXJ, FND, FRM_STRT, ID, ID_COL, ROW, IROW, COL, ICOL)
        !
        IF(FND) THEN
            VAL = IXJ%DAT(IXJ%P)%X(IVAL)
        ELSE
            VAL = IEEE_VALUE(VAL, IEEE_QUIET_NAN)
        END IF
    END IF
    !
    IF(PRESENT(FOUND)) FOUND = FND
    !
  END SUBROUTINE
  !
  ! -----------------------------------------------------------------------------------------------------------------------------
  !
  !!!PURE SUBROUTINE GET_J_IXJ(IXJ, J, IVAL, ID, ID_COL, ROW, IROW, COL, ICOL, FOUND, FROM_START)
  !!!  CLASS(IXJ_STRUCTURE), INTENT(INOUT):: IXJ
  !!!  INTEGER,                   INTENT(OUT  ):: J
  !!!  INTEGER,                   INTENT(IN   ):: IVAL
  !!!  INTEGER,       OPTIONAL,   INTENT(IN   ):: ID, ID_COL, ROW, IROW, COL, ICOL
  !!!  LOGICAL,       OPTIONAL,   INTENT(OUT  ):: FOUND
  !!!  LOGICAL,       OPTIONAL,   INTENT(IN   ):: FROM_START
  !!!  LOGICAL:: FND, FRM_STRT
  !!!  !
  !!!  IF(PRESENT(FROM_START)) THEN
  !!!      FRM_STRT = FROM_START
  !!!  ELSE
  !!!      FRM_STRT = TRUE
  !!!  END IF
  !!!  !
  !!!  CALL MOVE_POS_P_TO_RECORD(IXJ, FND, FRM_STRT, ID, ID_COL, ROW, IROW, COL, ICOL)
  !!!  !
  !!!  IF(FND) THEN
  !!!      J = IXJ%CUR%J(IVAL)
  !!!  ELSE
  !!!      J = Z
  !!!  END IF
  !!!  !
  !!!  IF(PRESENT(FOUND)) FOUND = FND
  !!!  !
  !!!END SUBROUTINE
  !
  ! -----------------------------------------------------------------------------------------------------------------------------
  ! 
  PURE SUBROUTINE COPY_TO_2D_ARRAY_DBL(IXJ, IROW, ICOL, VPOS, DIM1, DIM2, ARR, ERROR, NULL_VAL)
    CLASS(IXJ_STRUCTURE),             INTENT(IN   ):: IXJ
    INTEGER,                          INTENT(IN   ):: IROW, ICOL, VPOS, DIM1, DIM2
    REAL(DBL), DIMENSION(DIM1, DIM2), INTENT(INOUT):: ARR
    LOGICAL,                          INTENT(  OUT):: ERROR
    REAL(DBL),              OPTIONAL, INTENT(IN   ):: NULL_VAL
    REAL(DBL):: INIT
    INTEGER  :: N, I, J
    !
    IF(PRESENT(NULL_VAL)) THEN
        INIT = NULL_VAL
    ELSE
        INIT = DZ
    END IF
    !
    ERROR = FALSE
    !
    IF(IXJ%IS_CONSTANT) THEN
                   BLOCK
                        REAL(DBL):: DTMP
                        DTMP = IXJ%DAT(ONE)%X( VPOS )
                        DO CONCURRENT(I=1:DIM1, J=1:DIM2); ARR(I,J) = DTMP
                        END DO
                   END BLOCK
    ELSE
        DO CONCURRENT(I=1:DIM1, J=1:DIM2); ARR(I,J) = INIT
        END DO
        !
        DO N = ONE, IXJ%N
            !
            I = IXJ%DAT(I)%I(IROW)
            J = IXJ%DAT(I)%I(ICOL)
            !
            IF(I < ONE .OR. DIM1 < I .OR. J < ONE .OR. DIM2 < J) THEN
                ERROR = TRUE
            ELSE
                ARR(I,J) = IXJ%DAT(I)%X( VPOS )
            END IF
            !
        END DO
    END IF
    !
  END SUBROUTINE
  !
  ! -----------------------------------------------------------------------------------------------------------------------------
  !
  PURE SUBROUTINE COPY_TO_2D_ARRAY_INT(IXJ, IROW, ICOL, IPOS, DIM1, DIM2, ARR, ERROR, NULL_VAL)
    CLASS(IXJ_STRUCTURE),           INTENT(IN   ):: IXJ
    INTEGER,                        INTENT(IN   ):: IROW, ICOL, IPOS, DIM1, DIM2
    INTEGER, DIMENSION(DIM1, DIM2), INTENT(INOUT):: ARR
    LOGICAL,                        INTENT(  OUT):: ERROR
    INTEGER,              OPTIONAL, INTENT(IN   ):: NULL_VAL
    INTEGER:: N, I, J, INIT
    !
    IF(PRESENT(NULL_VAL)) THEN
        INIT = NULL_VAL
    ELSE
        INIT = Z
    END IF
    !
    ERROR = FALSE
    !
    IF(IXJ%IS_CONSTANT) THEN
                        N = IXJ%DAT(I)%I( IPOS )
                        DO CONCURRENT(I=1:DIM1, J=1:DIM2); ARR(I,J) = N
                        END DO
    ELSE
        DO CONCURRENT(I=1:DIM1, J=1:DIM2); ARR(I,J) = INIT
        END DO
        !
        DO N = ONE, IXJ%N
            !
            I = IXJ%DAT(I)%I(IROW)
            J = IXJ%DAT(I)%I(ICOL)
            !
            IF(I < ONE .OR. DIM1 < I .OR. J < ONE .OR. DIM2 < J) THEN
                ERROR = TRUE
            ELSE
                ARR(I,J) = IXJ%DAT(I)%I( IPOS )
            END IF
            !
        END DO
    END IF
    !
  END SUBROUTINE
  !
  ! -----------------------------------------------------------------------------------------------------------------------------
  !
  PURE SUBROUTINE BUILD_COMPRESSED_VALUE_STORAGE_TYPE_DIM2(IXJ, IDIM1, IDIM2, VPOS, CVS)
    CLASS(IXJ_STRUCTURE),           INTENT(IN   ):: IXJ
    INTEGER,                        INTENT(IN   ):: IDIM1, IDIM2, VPOS
    TYPE(COMPRESSED_VALUE_STORAGE), INTENT(INOUT):: CVS
    INTEGER:: K
    !
    CVS%IS_CONSTANT = FALSE
    !
    IF(IXJ%IS_CONSTANT) THEN
                       CALL CVS%ALLOC(ONE,TWO)
                       CVS%IS_CONSTANT = TRUE
                       !
                       K=1
                       CVS%DIM(ONE,K) = IXJ%DAT(ONE)%I(IDIM1)
                       CVS%DIM(TWO,K) = IXJ%DAT(ONE)%I(IDIM2)
                       CVS%VAL(K)     = IXJ%DAT(ONE)%X(VPOS )
    ELSEIF (IXJ%N > Z) THEN
                       CALL CVS%ALLOC(IXJ%N,TWO)
                       !
                       DO K =1, IXJ%N
                                CVS%DIM(ONE,K) = IXJ%DAT(K)%I(IDIM1)
                                CVS%DIM(TWO,K) = IXJ%DAT(K)%I(IDIM2)
                                CVS%VAL(K)     = IXJ%DAT(K)%X(VPOS )
                       END DO
    ELSE
                       CALL CVS%DESTROY()
    END IF
    !
  END SUBROUTINE
  !
  ! -----------------------------------------------------------------------------------------------------------------------------
  !
  PURE SUBROUTINE BUILD_COMPRESSED_VALUE_STORAGE_TYPE_DIM3(IXJ, IDIM1, IDIM2, IDIM3, VPOS, CVS)
    CLASS(IXJ_STRUCTURE),           INTENT(IN   ):: IXJ
    INTEGER,                        INTENT(IN   ):: IDIM1, IDIM2, IDIM3, VPOS
    TYPE(COMPRESSED_VALUE_STORAGE), INTENT(INOUT):: CVS
    INTEGER:: K
    !
    CVS%IS_CONSTANT = FALSE
    !
    IF(IXJ%IS_CONSTANT) THEN
                       CALL CVS%ALLOC(ONE,3)
                       CVS%IS_CONSTANT = TRUE
                       !
                       K=1
                       CVS%DIM(ONE, K) = IXJ%DAT(K)%I(IDIM1)
                       CVS%DIM(TWO, K) = IXJ%DAT(K)%I(IDIM2)
                       CVS%DIM(  3, K) = IXJ%DAT(K)%I(IDIM3)
                       CVS%VAL(K)      = IXJ%DAT(K)%X(VPOS )
    ELSEIF (IXJ%N > Z) THEN
                       CALL CVS%ALLOC(IXJ%N, 3)
                       !
                       DO K =1, IXJ%N
                                CVS%DIM(ONE, K) = IXJ%DAT(K)%I(IDIM1)
                                CVS%DIM(TWO, K) = IXJ%DAT(K)%I(IDIM2)
                                CVS%DIM(  3, K) = IXJ%DAT(K)%I(IDIM3)
                                CVS%VAL(K)      = IXJ%DAT(K)%X(VPOS )
                       END DO
                       !
    ELSE
                       CALL CVS%DESTROY()
    END IF
    !
  END SUBROUTINE
  !
  ! -----------------------------------------------------------------------------------------------------------------------------
  !
  PURE SUBROUTINE BUILD_COMPRESSED_LOCATION_STORAGE_TYPE(IXJ, IDIM1, IDIM2, IDIM_ID, ID, LAC) 
    CLASS(IXJ_STRUCTURE),              INTENT(IN   ):: IXJ
    INTEGER,                           INTENT(IN   ):: IDIM1, IDIM2, IDIM_ID, ID
    TYPE(COMPRESSED_LOCATION_STORAGE), INTENT(INOUT):: LAC
    INTEGER:: K, I
    !
    LAC%IS_CONSTANT = FALSE
    !
    LAC%ID = ID
    !
    IF(IXJ%IS_CONSTANT) THEN
                       CALL LAC%ALLOC(ONE, TWO)
                       LAC%IS_CONSTANT = TRUE
    ELSEIF (IXJ%N > Z) THEN
                       CALL COUNT_INT_IXJ_STRUCTURE(IXJ, ID, IDIM_ID, K)  !K is number of IDs found
                       !
                       CALL LAC%ALLOC(K, TWO)
                       LAC%N = K
                       !
                       I = Z
                       DO K =1, IXJ%N
                           !
                           IF(IXJ%DAT(K)%I(IDIM_ID) == ID) THEN
                               I = I + ONE
                               LAC%DIM(ONE,I) = IXJ%DAT(K)%I(IDIM1)
                               LAC%DIM(TWO,I) = IXJ%DAT(K)%I(IDIM2)
                           END IF
                       END DO
    ELSE
                       LAC%N = Z
    END IF
    !
  END SUBROUTINE
  !
  ! -----------------------------------------------------------------------------------------------------------------------------
  !
  SUBROUTINE BUILD_COMPRESSED_LOCATION_STORAGE_TYPE_BYDIM(IXJ, IDIM1, IDIM2, IDIM_ID, NID, LAC)
    CLASS(IXJ_STRUCTURE),                              INTENT(IN   ):: IXJ
    INTEGER,                                           INTENT(IN   ):: IDIM1, IDIM2, IDIM_ID, NID
    TYPE(COMPRESSED_LOCATION_STORAGE), DIMENSION(NID), INTENT(INOUT):: LAC
    INTEGER:: K, I
    !
    IF (IXJ%N > Z) THEN
                       DO CONCURRENT(I=ONE:NID); LAC(I)%N = Z  !Use %N as a counter
                       END DO
                       !
                       DO K=1, IXJ%N
                           !
                           I = IXJ%DAT(K)%I(IDIM_ID)
                           !
                           IF(I > Z .AND. I <= NID) LAC(I)%N = LAC(I)%N + ONE
                           !
                       END DO
                       !
                       DO I=ONE, NID
                                 K  = LAC(I)%N
                                 CALL LAC(I)%ALLOC(K, TWO)
                                 !
                                 LAC(I)%N = K
                                 !
                                 LAC(I)%IS_CONSTANT = FALSE
                       END DO
                       !
                       DO CONCURRENT(I=ONE:NID); LAC(I)%N = Z
                       END DO
                       !
                       DO CONCURRENT(I=ONE:NID); LAC(I)%ID = I
                       END DO
                       !
                       DO K =1, IXJ%N
                                     I = IXJ%DAT(K)%I(IDIM_ID)
                                     !
                                     IF(I > Z .AND. I <= NID) THEN
                                                              LAC(I)%N  = LAC(I)%N + ONE
                                                              LAC(I)%DIM(ONE,LAC(I)%N) = IXJ%DAT(K)%I(IDIM1)
                                                              LAC(I)%DIM(TWO,LAC(I)%N) = IXJ%DAT(K)%I(IDIM2)
                                     END IF
                       END DO
                       !
                       IF(IXJ%IS_CONSTANT) THEN
                                  DO I=ONE, NID
                                            IF(LAC(I)%N > Z) LAC(I)%IS_CONSTANT = TRUE
                                  END DO
                       END IF
    ELSE
                       LAC%N = Z
                       DO I=ONE, NID
                                 LAC(I)%IS_CONSTANT = FALSE
                       END DO
    END IF
    !
  END SUBROUTINE
  !
  ! -----------------------------------------------------------------------------------------------------------------------------
  !
  PURE SUBROUTINE DESTROY_IXJ_STRUCTURE_DAT(IXJ)
    CLASS(IXJ_STRUCTURE), INTENT(INOUT):: IXJ
    INTEGER:: I
    !
    DO I=ONE, IXJ%SIZ
        IF(ALLOCATED(IXJ%DAT(I)%I)) DEALLOCATE(IXJ%DAT(I)%I)
        IF(ALLOCATED(IXJ%DAT(I)%X)) DEALLOCATE(IXJ%DAT(I)%X)
        IF(ALLOCATED(IXJ%DAT(I)%J)) DEALLOCATE(IXJ%DAT(I)%J)
    END DO
    !
    IXJ%N    = Z
    IXJ%P    = ONE
    ! 
  END SUBROUTINE
  !
  ! -----------------------------------------------------------------------------------------------------------------------------
  !
  PURE SUBROUTINE DESTROY_IXJ_STRUCTURE(IXJ)
    CLASS(IXJ_STRUCTURE), INTENT(INOUT):: IXJ
    INTEGER:: I
    !
    DO I=ONE, IXJ%SIZ
        IF(ALLOCATED(IXJ%DAT(I)%I)) DEALLOCATE(IXJ%DAT(I)%I)
        IF(ALLOCATED(IXJ%DAT(I)%X)) DEALLOCATE(IXJ%DAT(I)%X)
        IF(ALLOCATED(IXJ%DAT(I)%J)) DEALLOCATE(IXJ%DAT(I)%J)
    END DO
    !
    IF(ALLOCATED(IXJ%DAT)) DEALLOCATE(IXJ%DAT)
    IF(ALLOCATED(IXJ%ERRMSG)) DEALLOCATE(IXJ%ERRMSG)
    !
    IXJ%SIZ  = Z
    IXJ%N    = Z
    IXJ%P    = ONE
    IXJ%IOUT = Z
    IXJ%DIM  = Z
    !
    IXJ%IS_CONSTANT = FALSE
    ! 
  END SUBROUTINE
  !
  ! -----------------------------------------------------------------------------------------------------------------------------
  !
  PURE SUBROUTINE FINAL_DESTROY_IXJ_STRUCTURE(IXJ)
    TYPE(IXJ_STRUCTURE), INTENT(INOUT):: IXJ
    !
    CALL DESTROY_IXJ_STRUCTURE(IXJ)
    ! 
  END SUBROUTINE
  !
  ! #############################################################################################################################
  ! #############################################################################################################################
  ! #############################################################################################################################
  !
  PURE SUBROUTINE ALLOCATE_IXJ_SINGLE_ENTRY(DAT,DIM)
    CLASS(IXJ_SINGLE_ENTRY), INTENT(INOUT):: DAT
    INTEGER,   DIMENSION(3), INTENT(IN   ):: DIM
    !
    CALL ALLOC(DAT%I, DIM(1))
    CALL ALLOC(DAT%X, DIM(2))
    CALL ALLOC(DAT%J, DIM(3))
    !
  END SUBROUTINE
  !
  ! -----------------------------------------------------------------------------------------------------------------------------
  !
  ELEMENTAL PURE SUBROUTINE ALLOCATE_IXJ_SINGLE_ENTRY_ELM(DAT,D1,D2,D3)
    CLASS(IXJ_SINGLE_ENTRY), INTENT(INOUT):: DAT
    INTEGER,                 INTENT(IN   ):: D1,D2,D3
    !
    CALL ALLOC(DAT%I, D1)
    CALL ALLOC(DAT%X, D2)
    CALL ALLOC(DAT%J, D3)
    !
  END SUBROUTINE
  !
  ! -----------------------------------------------------------------------------------------------------------------------------
  !
  PURE SUBROUTINE GET_IXJ_SINGLE_ENTRY_DIM(DAT,DIM)
    CLASS(IXJ_SINGLE_ENTRY), INTENT(IN ):: DAT
    INTEGER,   DIMENSION(3), INTENT(OUT):: DIM
    !
    DIM(1) = SIZE(DAT%I)
    DIM(2) = SIZE(DAT%X)
    DIM(3) = SIZE(DAT%J)
    !
    IF(.NOT. ALLOCATED(DAT%I)) DIM(1) = Z
    IF(.NOT. ALLOCATED(DAT%X)) DIM(2) = Z
    IF(.NOT. ALLOCATED(DAT%J)) DIM(3) = Z
    !
  END SUBROUTINE
  !
  ! -----------------------------------------------------------------------------------------------------------------------------
  !
  PURE SUBROUTINE GET_IXJ_SINGLE_ENTRY_DIM_I_X_J(DAT,I,X,J)
    CLASS(IXJ_SINGLE_ENTRY), INTENT(IN   ):: DAT
    INTEGER,       OPTIONAL, INTENT(INOUT):: I,X,J
    !
    IF(PRESENT(I)) THEN
        IF(ALLOCATED(DAT%I)) THEN; I = SIZE(DAT%I)
        ELSE;                      I = Z
        END IF
    END IF
    !
    IF(PRESENT(X)) THEN
        IF(ALLOCATED(DAT%X)) THEN; X = SIZE(DAT%X)
        ELSE;                      X = Z
        END IF
    END IF
    !
    IF(PRESENT(J)) THEN
        IF(ALLOCATED(DAT%J)) THEN; J = SIZE(DAT%J)
        ELSE;                      J = Z
        END IF
    END IF
    !
  END SUBROUTINE
  !
  ! -----------------------------------------------------------------------------------------------------------------------------
  !
  ELEMENTAL PURE SUBROUTINE MOVE_IXJ_SINGLE_ENTRY(DAT, DAT_TO)
    CLASS(IXJ_SINGLE_ENTRY), INTENT(INOUT):: DAT, DAT_TO
    !
    IF    (ALLOCATED(DAT   %I)) THEN; CALL MOVE_ALLOC(DAT%I, DAT_TO%I)
    ELSEIF(ALLOCATED(DAT_TO%I)) THEN;             DEALLOCATE(DAT_TO%I)
    END IF
    !
    IF    (ALLOCATED(DAT   %X)) THEN; CALL MOVE_ALLOC(DAT%X, DAT_TO%X)
    ELSEIF(ALLOCATED(DAT_TO%X)) THEN;             DEALLOCATE(DAT_TO%X)
    END IF
    !
    IF    (ALLOCATED(DAT   %J)) THEN; CALL MOVE_ALLOC(DAT%J, DAT_TO%J)
    ELSEIF(ALLOCATED(DAT_TO%J)) THEN;             DEALLOCATE(DAT_TO%J)
    END IF
    !
  END SUBROUTINE
  !
  ! -----------------------------------------------------------------------------------------------------------------------------
  !
  PURE SUBROUTINE COPY_IXJ_SINGLE_ENTRY(DAT_OUT,DAT_IN)
    CLASS(IXJ_SINGLE_ENTRY), INTENT(IN   ):: DAT_IN
    CLASS(IXJ_SINGLE_ENTRY), INTENT(INOUT):: DAT_OUT
    INTEGER, DIMENSION(3):: DIM1, DIM2 
    !
    CALL  DAT_IN%GET_DIM(DIM1)
    CALL DAT_OUT%GET_DIM(DIM2)
    !
    IF(ANY(DIM1.NE.DIM2)) THEN
        IF(ALLOCATED(DAT_OUT%I)) DEALLOCATE(DAT_OUT%I)
        IF(ALLOCATED(DAT_OUT%X)) DEALLOCATE(DAT_OUT%X)
        IF(ALLOCATED(DAT_OUT%J)) DEALLOCATE(DAT_OUT%J)
        !
        IF(DIM1(1) > Z) ALLOCATE( DAT_OUT%I, SOURCE = DAT_IN%I )
        IF(DIM1(2) > Z) ALLOCATE( DAT_OUT%X, SOURCE = DAT_IN%X )
        IF(DIM1(3) > Z) ALLOCATE( DAT_OUT%J, SOURCE = DAT_IN%J )
    ELSE
        IF(DIM1(1) > Z) DAT_OUT%I = DAT_IN%I
        IF(DIM1(2) > Z) DAT_OUT%X = DAT_IN%X
        IF(DIM1(3) > Z) DAT_OUT%J = DAT_IN%J
    END IF
    !
  END SUBROUTINE
  !
  ! -----------------------------------------------------------------------------------------------------------------------------
  !
  PURE SUBROUTINE FAST_COPY_IXJ_SINGLE_ENTRY(DIM, FROM, TOO)  !assumes I, X, and J are already allcocated
    INTEGER, DIMENSION(3),  INTENT(IN   ):: DIM
    TYPE(IXJ_SINGLE_ENTRY), INTENT(IN   ):: FROM
    TYPE(IXJ_SINGLE_ENTRY), INTENT(INOUT):: TOO
    !
    IF(DIM(1) > Z) TOO%I = FROM%I
    IF(DIM(2) > Z) TOO%X = FROM%X
    IF(DIM(3) > Z) TOO%J = FROM%J
    !
  END SUBROUTINE
  !
  ! -----------------------------------------------------------------------------------------------------------------------------
  !                                 
  ELEMENTAL PURE FUNCTION EQUALITY_IXJ_SINGLE_ENTRY(DAT1, DAT2) RESULT(EQ)
    CLASS(IXJ_SINGLE_ENTRY), INTENT(IN):: DAT1, DAT2
    LOGICAL:: EQ
    INTEGER, DIMENSION(3):: DIM1, DIM2 
    REAL(DBL):: TOL
    !
    TOL = 1D-12
    !
    CALL DAT1%GET_DIM(DIM1)
    CALL DAT2%GET_DIM(DIM2)
    !
    EQ = ALL(DIM1==DIM2)
    !
    IF(EQ .AND. DIM1(1) > Z) EQ = ALL( DAT1%I == DAT2%I )
    IF(EQ .AND. DIM1(2) > Z) EQ = ALL( ABS(DAT1%X - DAT2%X) <  DAT1%X*TOL)
    IF(EQ .AND. DIM1(3) > Z) EQ = ALL( DAT1%J == DAT2%J )
    !
  END FUNCTION
  !
  ! -----------------------------------------------------------------------------------------------------------------------------
  !
  ELEMENTAL PURE SUBROUTINE DESTROY_IXJ_SINGLE_ENTRY(DAT)
    CLASS(IXJ_SINGLE_ENTRY),         INTENT(INOUT):: DAT
    !
    IF(ALLOCATED(DAT%I)) DEALLOCATE(DAT%I)
    IF(ALLOCATED(DAT%X)) DEALLOCATE(DAT%X)
    IF(ALLOCATED(DAT%J)) DEALLOCATE(DAT%J)
    !
  END SUBROUTINE
  !
  ! -----------------------------------------------------------------------------------------------------------------------------
  !
  PURE RECURSIVE SUBROUTINE FINAL_IXJ_SINGLE_ENTRY(DAT)
    TYPE(IXJ_SINGLE_ENTRY),        INTENT(INOUT):: DAT
    !
    IF(ALLOCATED(DAT%I)) DEALLOCATE(DAT%I)
    IF(ALLOCATED(DAT%X)) DEALLOCATE(DAT%X)
    IF(ALLOCATED(DAT%J)) DEALLOCATE(DAT%J)
    !
  END SUBROUTINE
  !
  ! #############################################################################################################################
  ! #############################################################################################################################
  ! #############################################################################################################################
  !
  PURE SUBROUTINE SORT_BY_INT_COLVEC(IXJ, ICOL)
    CLASS(IXJ_STRUCTURE),              INTENT(INOUT):: IXJ
    INTEGER, DIMENSION(:), CONTIGUOUS, INTENT(IN   ):: ICOL
    !
    INTEGER:: N
    !
    N = SIZE(ICOL)
    !
    IF(IXJ%N > ONE) THEN                                                      !Silly to sort when size is 1
        IF    (N == ONE) THEN
                         CALL SORT_BY_INT_COL(IXJ, ICOL(1))                   !Optimized routine for sorting by 1 column reference
        ELSEIF(N == TWO) THEN
                         CALL SORT_BY_INT_COL1_COL2(IXJ, ICOL(1), ICOL(2))    !Optimized routine for sorting by 2 column reference
        ELSEIF(N ==   3) THEN
                         CALL SORT_BY_INT_COL1_COL2_COL3(IXJ, ICOL(1), ICOL(2), ICOL(3))   !Optimized routine for sorting by 3 column reference
        ELSE
                         CALL SORT_BY_INT_COLVEC_UNLIMITED(IXJ, N, ICOL)
        END IF
    END IF
    !
  END SUBROUTINE
  !
  ! -----------------------------------------------------------------------------------------------------------------------------
  !
  PURE SUBROUTINE SORT_BY_INT_COL(IXJ, ICOL)
    CLASS(IXJ_STRUCTURE), INTENT(INOUT):: IXJ
    INTEGER,              INTENT(IN   ):: ICOL
    INTEGER:: I, J, P, ICHK
    TYPE(IXJ_SINGLE_ENTRY):: TMP
    !
    CALL TMP%ALLOC(IXJ%DIM)
    IXJ%P = ONE
    !
    DO I=ONE, IXJ%N
       !
       ICHK = IXJ%DAT(I)%I(ICOL)
       P    = Z
       !
       DO J=I+ONE, IXJ%N
           IF(IXJ%DAT(J)%I(ICOL) < ICHK) THEN
                                             ICHK = IXJ%DAT(J)%I(ICOL)
                                             P    = J
           END IF
       END DO
       !
       IF(P > Z) THEN                                          ! FROM  TOO
                 CALL FAST_COPY_IXJ_SINGLE_ENTRY( IXJ%DIM, IXJ%DAT(I),        TMP )
                 CALL FAST_COPY_IXJ_SINGLE_ENTRY( IXJ%DIM, IXJ%DAT(P), IXJ%DAT(I) )
                 CALL FAST_COPY_IXJ_SINGLE_ENTRY( IXJ%DIM,        TMP, IXJ%DAT(P) )
                 !                       
                 !TMP        = IXJ%DAT(I)
                 !IXJ%DAT(I) = IXJ%DAT(P)
                 !IXJ%DAT(P) = TMP    
       END IF
       !
    END DO
    !
  END SUBROUTINE
  !
  ! -----------------------------------------------------------------------------------------------------------------------------
  !
  PURE SUBROUTINE SORT_BY_INT_COL1_COL2(IXJ, ICOL1, ICOL2)
    CLASS(IXJ_STRUCTURE), INTENT(INOUT):: IXJ
    INTEGER,              INTENT(IN   ):: ICOL1, ICOL2
    INTEGER:: I, J, P, ICHK1, ICHK2
    TYPE(IXJ_SINGLE_ENTRY):: TMP
    !
    CALL TMP%ALLOC(IXJ%DIM)
    IXJ%P = ONE
    !
    DO I=ONE, IXJ%N
       !
       ICHK1 = IXJ%DAT(I)%I(ICOL1)
       ICHK2 = IXJ%DAT(I)%I(ICOL2)
       !
       P    = Z
       !
       DO J=I+ONE, IXJ%N
           IF(IXJ%DAT(J)%I(ICOL1) == ICHK1 .AND. IXJ%DAT(J)%I(ICOL2) < ICHK2) THEN
                                                    !
                                                    ICHK1 = IXJ%DAT(J)%I(ICOL1)
                                                    ICHK2 = IXJ%DAT(J)%I(ICOL2)
                                                    P     = J
           ELSEIF(IXJ%DAT(J)%I(ICOL1) < ICHK1) THEN
                                                    ICHK1 = IXJ%DAT(J)%I(ICOL1)
                                                    ICHK2 = IXJ%DAT(J)%I(ICOL2)
                                                    P     = J
           END IF
       END DO
       !
       IF(P > Z) THEN                                          ! FROM  TOO
                 CALL FAST_COPY_IXJ_SINGLE_ENTRY( IXJ%DIM, IXJ%DAT(I),        TMP )
                 CALL FAST_COPY_IXJ_SINGLE_ENTRY( IXJ%DIM, IXJ%DAT(P), IXJ%DAT(I) )
                 CALL FAST_COPY_IXJ_SINGLE_ENTRY( IXJ%DIM,        TMP, IXJ%DAT(P) )
       END IF
       !
    END DO
    !
  END SUBROUTINE
  !
  ! -----------------------------------------------------------------------------------------------------------------------------
  !
  PURE SUBROUTINE SORT_BY_INT_COL1_COL2_COL3(IXJ, ICOL1, ICOL2, ICOL3)
    CLASS(IXJ_STRUCTURE), INTENT(INOUT):: IXJ
    INTEGER,              INTENT(IN   ):: ICOL1, ICOL2, ICOL3
    INTEGER:: I, J, P, ICHK1, ICHK2, ICHK3
    TYPE(IXJ_SINGLE_ENTRY):: TMP
    !
    CALL TMP%ALLOC(IXJ%DIM)
    IXJ%P = ONE
    !
    DO I=ONE, IXJ%N
       !
       ICHK1 = IXJ%DAT(I)%I(ICOL1)
       ICHK2 = IXJ%DAT(I)%I(ICOL2)
       ICHK3 = IXJ%DAT(I)%I(ICOL3)
       !
       P    = Z
       !
       DO J=I+ONE, IXJ%N
           IF    (IXJ%DAT(J)%I(ICOL1) == ICHK1 .AND. IXJ%DAT(J)%I(ICOL2) == ICHK2 .AND. IXJ%DAT(J)%I(ICOL3) < ICHK3) THEN
                                                    !
                                                    ICHK1 = IXJ%DAT(J)%I(ICOL1)
                                                    ICHK2 = IXJ%DAT(J)%I(ICOL2)
                                                    ICHK3 = IXJ%DAT(I)%I(ICOL3)
                                                    P     = J
                                                    !
           ELSEIF(IXJ%DAT(J)%I(ICOL1) == ICHK1 .AND. IXJ%DAT(J)%I(ICOL2) < ICHK2) THEN
                                                    !
                                                    ICHK1 = IXJ%DAT(J)%I(ICOL1)
                                                    ICHK2 = IXJ%DAT(J)%I(ICOL2)
                                                    ICHK3 = IXJ%DAT(I)%I(ICOL3)
                                                    P     = J
           ELSEIF(IXJ%DAT(J)%I(ICOL1) < ICHK1) THEN
                                                    ICHK1 = IXJ%DAT(J)%I(ICOL1)
                                                    ICHK2 = IXJ%DAT(J)%I(ICOL2)
                                                    ICHK3 = IXJ%DAT(I)%I(ICOL3)
                                                    P     = J
           END IF
       END DO
       !
       IF(P > Z) THEN                                          ! FROM  TOO
                 CALL FAST_COPY_IXJ_SINGLE_ENTRY( IXJ%DIM, IXJ%DAT(I),        TMP )
                 CALL FAST_COPY_IXJ_SINGLE_ENTRY( IXJ%DIM, IXJ%DAT(P), IXJ%DAT(I) )
                 CALL FAST_COPY_IXJ_SINGLE_ENTRY( IXJ%DIM,        TMP, IXJ%DAT(P) )
       END IF
       !
    END DO
    !
  END SUBROUTINE
  !
  ! -----------------------------------------------------------------------------------------------------------------------------
  !
  PURE SUBROUTINE SORT_BY_INT_COLVEC_UNLIMITED(IXJ, N, ICOL)
    CLASS(IXJ_STRUCTURE),  INTENT(INOUT):: IXJ
    INTEGER,               INTENT(IN   ):: N
    INTEGER, DIMENSION(N), INTENT(IN   ):: ICOL
    !
    INTEGER, DIMENSION(N):: ICHK
    TYPE(IXJ_SINGLE_ENTRY):: TMP
    INTEGER:: I, J, K, P
    LOGICAL:: REPOINT, SPIN
    !
    CALL TMP%ALLOC(IXJ%DIM)
    IXJ%P = ONE
    !
    DO I=ONE, IXJ%N
       !
       DO K=ONE, N;   ICHK(K) = IXJ%DAT(I)%I( ICOL(K) )
       END DO
       !
       P = Z
       !
       DO J=I+ONE, IXJ%N
           !
           REPOINT = IXJ%DAT(J)%I( ICOL(1) ) <= ICHK(1)  !Flag to indicate P must be updated
           SPIN    = IXJ%DAT(J)%I( ICOL(1) ) == ICHK(1)  !Flag to indicate that ICOL(2:) must be checked
           !
           K=TWO
           DO WHILE (K <= N .AND. SPIN)                         !Note that alogrithm will REPOINT a full matching %I, but this is a negligable speed hit compared to include an addition check for this within the while loop.
                     !
                     REPOINT = IXJ%DAT(J)%I( ICOL(K) ) <= ICHK(K)
                     SPIN    = IXJ%DAT(J)%I( ICOL(K) ) == ICHK(K)
                     !
                     K = K + ONE
           END DO
           !
           IF(REPOINT) THEN
                           P = J
                           DO K=ONE, N;   ICHK(K) = IXJ%DAT(P)%I( ICOL(K) )
                           END DO
           END IF
       END DO
       !
       IF(P > Z) THEN                                          ! FROM  TOO
                 CALL FAST_COPY_IXJ_SINGLE_ENTRY( IXJ%DIM, IXJ%DAT(I),        TMP )
                 CALL FAST_COPY_IXJ_SINGLE_ENTRY( IXJ%DIM, IXJ%DAT(P), IXJ%DAT(I) )
                 CALL FAST_COPY_IXJ_SINGLE_ENTRY( IXJ%DIM,        TMP, IXJ%DAT(P) )
                 !                       
                 !TMP        = IXJ%DAT(I)
                 !IXJ%DAT(I) = IXJ%DAT(P)
                 !IXJ%DAT(P) = TMP    
       END IF
       !
    END DO
    
  END SUBROUTINE
  !
  ! #############################################################################################################################
  ! #############################################################################################################################
  ! #############################################################################################################################
  !
END MODULE
!
!