!
! CODE DEVELOPED BY SCOTT E BOYCE
!                   CONTACT <seboyce@usgs.gov> or <Boyce@engineer.com>
!
! MODULE LISTING:
!
!   ULOAD_AND_SFAC_INTERFACE
!
MODULE ULOAD_AND_SFAC_INTERFACE
  ! THIS ROUTINE DOES *NOT* CHECK FOR SFAC, IT ONLY LOADS A LIST OF DATA.  --CHECK FOR SFAC FIRST WITH SFAC MODULE
  !
  ! PROVIDES A GENERIC INERFACE THAT LOADS IN EITHER AN ARRAY, VECTOR, OR SINGLE VALUE (SCALAR)
  ! PASSES IN THE UNIT NUMBER OF THE FILE AS INFILE.
  !      IF THIS IS SET FOR ZERO
  !      IT WILL CHECK FOR KEYWORD (INTERNAL,EXTERNAL, OPEN/CLOSE) AND IF THAT IS NOT FOUND THEN IT ASSUMES DATA IS WITHING "LN".
  !      IF NONZERO INFILE THEN IT WILL LOAD FROM THAT FILE UNIT NUMBER.
  !
  ! ASSUMES FILE STRUCTURE IS AS FOLLOWS:
  ! FOR ARRAY:
  ! ID_11  VAL_11  VAL_12 ... VAL_1N
  ! ID_21  VAL_21  VAL_22 ... VAL_2N
  ! ...
  ! ID_N1  VAL_N1  VAL_N2 ... VAL_NN
  !
  ! FOR VECTOR:
  ! ID_1  VAL_1
  ! ID_2  VAL_2
  ! ...
  ! ID_N  VAL_N
  !
  ! FOR SCALAR:
  ! ID_1  VAL_1
  ! ID_1  VAL_1
  ! ...
  ! ID_1  VAL_1
  !
  ! WHERE "ID" can be anything is skipped by the code. (typically used as a Farm ID or Crop ID)
  ! VAL represent the values that are either loaded into the provided array, vector or scalar.
  !
  ! SFAC IS:
  ! SKIPS PAST ONE WORD (TYPICALLY SFAC) AND THEN READS IN OPTIONAL KEYWORDS AND EITHER LOADS 1 VALUE OR A VECTOR
  ! CAN INTERPRET KEYWORDS THAT ARE PASSED WITH PASSED VECTOR SIZES
  ! ACCEPTS OPTIONS SUCH AS "OPEN/CLOSE" AND "EXTERNAL" OR ASSUMES INFORMAT IS LOCATED WITHIN PASSED CHARACTER STRING "LN"
  !
  USE, INTRINSIC:: ISO_FORTRAN_ENV, ONLY: REAL32, REAL64, REAL128 !CHARACTER_STORAGE_SIZE
  !
  USE CONSTANTS,                        ONLY: NEG, Z, ONE, TWO, THREE, inf_I, DZ, UNO, NL, BLN, BLNK, COM, TRUE, FALSE, lowerCHAR, upperCHAR
  USE ARRAY_DATA_TYPES,                 ONLY: INTEGER_VECTOR, CHARACTER_TYPE
  USE SET_ARRAY_INTERFACE,              ONLY: SET_SEQUENCE
  USE IS_ROUTINES,                      ONLY: IS_BLANK
  USE POST_KEY_SUB,                     ONLY: CHECK_FOR_POST_KEY
  USE FILE_IO_INTERFACE,                ONLY: READ_TO_DATA, COMMENT_INDEX
  USE PARSE_WORD_INTERFACE,             ONLY: PARSE_WORD, PARSE_WORD_UP, RET_WORD
  USE STRINGS,                          ONLY: GET_DATE, GET_NUMBER, GET_INTEGER, GET_WORD
  USE ERROR_INTERFACE,                  ONLY: STOP_ERROR, FILE_IO_ERROR, WARNING_MESSAGE
  USE NUM2STR_INTERFACE,                ONLY: NUM2STR
  USE ALLOC_INTERFACE,                  ONLY: ALLOC
  USE DATE_OPERATOR_INSTRUCTION,        ONLY: DATE_OPERATOR
  USE GENERIC_INPUT_FILE_INSTRUCTION,   ONLY: GENERIC_INPUT_FILE
  USE GENERIC_OPEN_INTERFACE,           ONLY: UTF8_BOM_OFFSET_REWIND
  USE TIME_SERIES_INSTRUCTION,          ONLY: TIME_SERIES_FILE
  USE LOOKUP_TABLE_INSTRUCTION,         ONLY: LOOKUP_TABLE_TYPE
  USE IXJ_INSTRUCTION,                  ONLY: IXJ_STRUCTURE
  USE GENERIC_BLOCK_READER_INSTRUCTION, ONLY: GENERIC_BLOCK_READER
  IMPLICIT NONE
  PRIVATE
  PUBLIC:: SFAC_DATA, ULOAD !, ULOAD_ARRAY, ULOAD_VECTOR, ULOAD_SCALAR
  !
  !
  INTERFACE ULOAD
                      MODULE PROCEDURE ULOAD_SCALAR    !(VAR, LLOC, LN, IOUT, IN, IU, [ NOID, BINARY, ID,                     NOSTOP, SFAC, EX1_WORD, EX1_DIM, EX2_WORD, EX2_DIM, EX3_WORD, EX3_DIM,               SCRATCH, ENTIRE_LINE, NO_INTERNAL, NO_BINARY, NO_REPEAT, OLD_IU, TEXT, MSG)
                      MODULE PROCEDURE ULOAD_VECTOR    !(VAR, LLOC, LN, IOUT, IN, IU, [ NOID, BINARY, ID, ROW_WORD,           NOSTOP, SFAC, EX1_WORD, EX1_DIM, EX2_WORD, EX2_DIM, EX3_WORD, EX3_DIM,               SCRATCH, ENTIRE_LINE, NO_INTERNAL, NO_BINARY, NO_REPEAT, OLD_IU, TEXT, MSG)
                      MODULE PROCEDURE ULOAD_ARRAY     !(VAR, LLOC, LN, IOUT, IN, IU, [ NOID, BINARY, ID, ROW_WORD, COL_WORD, NOSTOP, SFAC, EX1_WORD, EX1_DIM, EX2_WORD, EX2_DIM, EX3_WORD, EX3_DIM, READ_BY_DIM2, SCRATCH,              NO_INTERNAL, NO_BINARY, NO_REPEAT, OLD_IU, MSG)
                      !
                      MODULE PROCEDURE ULOAD_BLOCK_READER_VECTOR !(VAR, LLOC, BL, IU, [ NOID, BINARY, ID, ROW_WORD,           NOSTOP, SFAC, EX1_WORD, EX1_DIM, EX2_WORD, EX2_DIM, EX3_WORD, EX3_DIM,               SCRATCH, ENTIRE_LINE, NO_INTERNAL, NO_BINARY, NO_REPEAT, OLD_IU, TEXT, MSG)
                      MODULE PROCEDURE ULOAD_BLOCK_READER_ARRAY  !(VAR, LLOC, BL, IU, [ NOID, BINARY, ID, ROW_WORD, COL_WORD, NOSTOP, SFAC, EX1_WORD, EX1_DIM, EX2_WORD, EX2_DIM, EX3_WORD, EX3_DIM, READ_BY_DIM2, SCRATCH,              NO_INTERNAL, NO_BINARY, NO_REPEAT, OLD_IU, MSG)
  END INTERFACE
  !
  TYPE SFAC_DATA
      !LOGICAL:: HAS_EXTERNAL
      !
      LOGICAL:: HAS_ALL = FALSE
      LOGICAL:: HAS_ROW = FALSE
      LOGICAL:: HAS_COL = FALSE
      LOGICAL:: HAS_EX1 = FALSE
      LOGICAL:: HAS_EX2 = FALSE
      LOGICAL:: HAS_EX3 = FALSE
      REAL(REAL64),              ALLOCATABLE:: ALL
      REAL(REAL64), DIMENSION(:),ALLOCATABLE:: ROW
      REAL(REAL64), DIMENSION(:),ALLOCATABLE:: COL
      REAL(REAL64), DIMENSION(:),ALLOCATABLE:: EX1
      REAL(REAL64), DIMENSION(:),ALLOCATABLE:: EX2
      REAL(REAL64), DIMENSION(:),ALLOCATABLE:: EX3
      !INTEGER,                       ALLOCATABLE:: IU_ALL, IU_ROW, IU_COL, IU_EX1, IU_EX2
      !LOGICAL                                   :: NEW_ALL, NEW_ROW, NEW_COL, NEW_EX1, NEW_EX2
      !
      CONTAINS
      !
      PROCEDURE, PASS(SFAC):: INIT   => SFAC_INITIALIZE
      PROCEDURE, PASS(SFAC):: LOAD   => SFAC_READ_AND_RETURN   !(LN, IN, IOUT, ROW_WORD, ROW_DIM, COL_WORD, COL_DIM, EX1_WORD, EX1_DIM, EX2_WORD, EX2_DIM, EX3_WORD, EX3_DIM, SKIP_SFAC, SCRATCH, NO_INTERNAL) !MUST INCLUDE ROW_WORD AND ROW_DIM TOGETHER AND/OR COL_WORD AND COL_DIM TOGETHER WHEN CALLLING
      PROCEDURE, PASS(SFAC):: SET_ALL=> SFAC_SET_ANOTHER_ALL
      !PROCEDURE, PASS(SFAC):: INIT_ALL=> SFAC_INIT_ALL         !(SCALE) --redundant subroutine
      !PROCEDURE, PASS(SFAC):: NEXT   => SFAC_READ_NEXT_EXTERNAL!(IOUT)
      GENERIC::               APPLY  => SFAC_APPLY_TO_SCALAR, SFAC_APPLY_TO_VECTOR, SFAC_APPLY_TO_ARRAY!, SFAC_APPLY_TO_SCALAR_REAL, SFAC_APPLY_TO_VECTOR_REAL, SFAC_APPLY_TO_ARRAY_REAL
      GENERIC::        ASSIGNMENT(=) => COPY_SFAC_TO_SFAC
      GENERIC::        OPERATOR(*)   => PRODUCT_SFAC_TO_SFAC
      GENERIC::        OPERATOR(/)   => DIVISOR_SFAC_TO_SFAC
      !
      PROCEDURE, PRIVATE, PASS(SFAC)::  SFAC_APPLY_TO_SCALAR
      PROCEDURE, PRIVATE, PASS(SFAC)::  SFAC_APPLY_TO_VECTOR
      PROCEDURE, PRIVATE, PASS(SFAC)::  SFAC_APPLY_TO_ARRAY
      !PROCEDURE, PRIVATE, PASS(SFAC)::  SFAC_APPLY_TO_SCALAR_REAL
      !PROCEDURE, PRIVATE, PASS(SFAC)::  SFAC_APPLY_TO_VECTOR_REAL
      !PROCEDURE, PRIVATE, PASS(SFAC)::  SFAC_APPLY_TO_ARRAY_REAL
      PROCEDURE, PRIVATE            ::  COPY_SFAC_TO_SFAC
      PROCEDURE, PRIVATE            ::  PRODUCT_SFAC_TO_SFAC
      PROCEDURE, PRIVATE            ::  DIVISOR_SFAC_TO_SFAC
      !
      PROCEDURE, PASS(SFAC):: DESTROY=> SFAC_INITIALIZE      !INIT DOES THE SAME AS DESTROY
      FINAL::                           FINAL_SFAC_DEALLOCATE
  END TYPE
  !
  TYPE SFAC_DATA_TRANSIENT     !Data type not yet complete
      TYPE(SFAC_DATA)::SF_NEW
      TYPE(SFAC_DATA)::SF_OLD
      INTEGER:: IU
      LOGICAL:: TRANSIENT
      LOGICAL:: OPENCLOSE
  END TYPE
  !
  CONTAINS
  !
  ! -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  !
  PURE SUBROUTINE UPPER(DIM, LN)     !UPPER only for internal use to MODULE ULOAD_AND_SFAC_INTERFACE --> coerce compiler to inline calls in module 
    INTEGER,        INTENT(IN   ):: DIM
    CHARACTER(DIM), INTENT(INOUT):: LN
    INTEGER:: I, N
    !
    DO I=1, DIM
        N = INDEX( lowerCHAR, LN(I:I))
        !
        IF(N > 0) LN(I:I) = upperCHAR(N:N)
    END DO
  END SUBROUTINE
  !
  ! -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  !
  PURE ELEMENTAL SUBROUTINE COPY_SFAC_TO_SFAC(SFAC_OUT, SFAC_IN)
    CLASS(SFAC_DATA),INTENT(INOUT)::SFAC_OUT
    CLASS(SFAC_DATA),INTENT(IN   )::SFAC_IN
    !
    CALL SFAC_INITIALIZE(SFAC_OUT)
    !
    !SFAC_OUT%HAS_EXTERNAL = SFAC_IN%HAS_EXTERNAL
    !
    SFAC_OUT%HAS_ALL = SFAC_IN%HAS_ALL
    SFAC_OUT%HAS_ROW = SFAC_IN%HAS_ROW
    SFAC_OUT%HAS_COL = SFAC_IN%HAS_COL
    SFAC_OUT%HAS_EX1 = SFAC_IN%HAS_EX1
    SFAC_OUT%HAS_EX2 = SFAC_IN%HAS_EX2
    SFAC_OUT%HAS_EX3 = SFAC_IN%HAS_EX3
    !SFAC_OUT%NEW_ALL = SFAC_IN%NEW_ALL
    !SFAC_OUT%NEW_ROW = SFAC_IN%NEW_ROW
    !SFAC_OUT%NEW_COL = SFAC_IN%NEW_COL
    !SFAC_OUT%NEW_EX1 = SFAC_IN%NEW_EX1
    !SFAC_OUT%NEW_EX2 = SFAC_IN%NEW_EX2
    !
    IF( SFAC_IN%HAS_ALL         ) ALLOCATE(SFAC_OUT%ALL   , SOURCE = SFAC_IN%ALL   )
    IF( SFAC_IN%HAS_ROW         ) ALLOCATE(SFAC_OUT%ROW   , SOURCE = SFAC_IN%ROW   )
    IF( SFAC_IN%HAS_COL         ) ALLOCATE(SFAC_OUT%COL   , SOURCE = SFAC_IN%COL   )
    IF( SFAC_IN%HAS_EX1         ) ALLOCATE(SFAC_OUT%EX1   , SOURCE = SFAC_IN%EX1   )
    IF( SFAC_IN%HAS_EX2         ) ALLOCATE(SFAC_OUT%EX2   , SOURCE = SFAC_IN%EX2   )
    IF( SFAC_IN%HAS_EX3         ) ALLOCATE(SFAC_OUT%EX3   , SOURCE = SFAC_IN%EX3   )
    !IF(ALLOCATED(SFAC_IN%IU_ALL)) ALLOCATE(SFAC_OUT%IU_ALL, SOURCE = SFAC_IN%IU_ALL)
    !IF(ALLOCATED(SFAC_IN%IU_ROW)) ALLOCATE(SFAC_OUT%IU_ROW, SOURCE = SFAC_IN%IU_ROW)
    !IF(ALLOCATED(SFAC_IN%IU_COL)) ALLOCATE(SFAC_OUT%IU_COL, SOURCE = SFAC_IN%IU_COL)
    !IF(ALLOCATED(SFAC_IN%IU_EX1)) ALLOCATE(SFAC_OUT%IU_EX1, SOURCE = SFAC_IN%IU_EX1)
    !IF(ALLOCATED(SFAC_IN%IU_EX2)) ALLOCATE(SFAC_OUT%IU_EX2, SOURCE = SFAC_IN%IU_EX2)
    !
  END SUBROUTINE
  !
  ELEMENTAL PURE FUNCTION PRODUCT_SFAC_TO_SFAC(SF1, SF2) RESULT(SFAC)
    CLASS(SFAC_DATA),INTENT(IN)::SF1
    CLASS(SFAC_DATA),INTENT(IN)::SF2
    TYPE(SFAC_DATA):: SFAC
    !
    CALL COPY_SFAC_TO_SFAC(SFAC, SF1)
    !
    IF(.NOT. SF1%HAS_ALL     ) SFAC%HAS_ALL = SF2%HAS_ALL
    IF(.NOT. SF1%HAS_ROW     ) SFAC%HAS_ROW = SF2%HAS_ROW
    IF(.NOT. SF1%HAS_COL     ) SFAC%HAS_COL = SF2%HAS_COL
    IF(.NOT. SF1%HAS_EX1     ) SFAC%HAS_EX1 = SF2%HAS_EX1
    IF(.NOT. SF1%HAS_EX2     ) SFAC%HAS_EX2 = SF2%HAS_EX2
    IF(.NOT. SF1%HAS_EX3     ) SFAC%HAS_EX3 = SF2%HAS_EX3
    !
    IF(ALLOCATED(SFAC%ALL   ) .AND. SF2%HAS_ALL) THEN
                                                     SFAC%ALL = SFAC%ALL * SF2%ALL
    ELSEIF(SF2%HAS_ALL) THEN
                                                     ALLOCATE(SFAC%ALL   , SOURCE = SF2%ALL   )
    END IF
    !
    IF(ALLOCATED(SFAC%ROW   ) .AND. SF2%HAS_ROW) THEN
                                                     SFAC%ROW = SFAC%ROW * SF2%ROW
    ELSEIF(SF2%HAS_ROW) THEN
                                                     ALLOCATE(SFAC%ROW   , SOURCE = SF2%ROW   )
    END IF
    !
    IF(ALLOCATED(SFAC%COL   ) .AND. SF2%HAS_COL) THEN
                                                     SFAC%COL = SFAC%COL * SF2%COL
    ELSEIF(SF2%HAS_COL) THEN
                                                     ALLOCATE(SFAC%COL   , SOURCE = SF2%COL   )
    END IF
    !
    IF(ALLOCATED(SFAC%EX1   ) .AND. SF2%HAS_EX1) THEN
                                                     SFAC%EX1 = SFAC%EX1 * SF2%EX1
    ELSEIF(SF2%HAS_EX1) THEN
                                                     ALLOCATE(SFAC%EX1   , SOURCE = SF2%EX1   )
    END IF
    !
    IF(ALLOCATED(SFAC%EX2   ) .AND. SF2%HAS_EX2) THEN
                                                     SFAC%EX2 = SFAC%EX2 * SF2%EX2
    ELSEIF(SF2%HAS_EX2) THEN
                                                     ALLOCATE(SFAC%EX2   , SOURCE = SF2%EX2   )
    END IF
    !
    IF(ALLOCATED(SFAC%EX3   ) .AND. SF2%HAS_EX3) THEN
                                                     SFAC%EX3 = SFAC%EX3 * SF2%EX3
    ELSEIF(SF2%HAS_EX3) THEN
                                                     ALLOCATE(SFAC%EX3   , SOURCE = SF2%EX3   )
    END IF
    !
    !IF(ALLOCATED(SFAC%IU_ALL)) DEALLOCATE(SFAC%IU_ALL)
    !IF(ALLOCATED(SFAC%IU_ROW)) DEALLOCATE(SFAC%IU_ROW)
    !IF(ALLOCATED(SFAC%IU_COL)) DEALLOCATE(SFAC%IU_COL)
    !IF(ALLOCATED(SFAC%IU_EX1)) DEALLOCATE(SFAC%IU_EX1)
    !IF(ALLOCATED(SFAC%IU_EX2)) DEALLOCATE(SFAC%IU_EX2)
    !
  END FUNCTION
  !
  ELEMENTAL PURE FUNCTION DIVISOR_SFAC_TO_SFAC(SF1, SF2) RESULT(SFAC)
    CLASS(SFAC_DATA),INTENT(IN)::SF1
    CLASS(SFAC_DATA),INTENT(IN)::SF2
    TYPE(SFAC_DATA):: SFAC
    !
    CALL COPY_SFAC_TO_SFAC(SFAC, SF1)
    !
    IF(.NOT. SF1%HAS_ALL     ) SFAC%HAS_ALL = SF2%HAS_ALL
    IF(.NOT. SF1%HAS_ROW     ) SFAC%HAS_ROW = SF2%HAS_ROW
    IF(.NOT. SF1%HAS_COL     ) SFAC%HAS_COL = SF2%HAS_COL
    IF(.NOT. SF1%HAS_EX1     ) SFAC%HAS_EX1 = SF2%HAS_EX1
    IF(.NOT. SF1%HAS_EX2     ) SFAC%HAS_EX2 = SF2%HAS_EX2
    IF(.NOT. SF1%HAS_EX3     ) SFAC%HAS_EX3 = SF2%HAS_EX3
    !
    IF(ALLOCATED(SFAC%ALL   ) .AND. SF2%HAS_ALL) THEN
                                                     IF(SF2%ALL /= DZ) THEN
                                                              SFAC%ALL = SFAC%ALL / SF2%ALL
                                                     ELSE
                                                              SFAC%ALL = DZ
                                                     END IF
    ELSEIF(SF2%HAS_ALL) THEN
                                                     IF(SF2%ALL /= DZ) THEN
                                                              ALLOCATE(SFAC%ALL   , SOURCE = UNO / SF2%ALL   )
                                                     ELSE
                                                              ALLOCATE(SFAC%ALL   , SOURCE = DZ   )
                                                     END IF
    END IF
    !
    IF(ALLOCATED(SFAC%ROW   ) .AND. SF2%HAS_ROW) THEN
                                                     WHERE(SF2%ROW /= DZ)
                                                              SFAC%ROW = SFAC%ROW / SF2%ROW
                                                     ELSEWHERE
                                                              SFAC%ROW = DZ
                                                     END WHERE
    ELSEIF(SF2%HAS_ROW) THEN
                                                     ALLOCATE(SFAC%ROW,  MOLD = SF2%ROW   )
                                                     WHERE(SF2%ROW /= DZ)
                                                              SFAC%ROW = UNO / SF2%ROW
                                                     ELSEWHERE
                                                              SFAC%ROW = DZ
                                                     END WHERE
    END IF
    !
    IF(ALLOCATED(SFAC%COL   ) .AND. SF2%HAS_COL) THEN
                                                     WHERE(SF2%COL /= DZ)
                                                              SFAC%COL = SFAC%COL / SF2%COL
                                                     ELSEWHERE
                                                              SFAC%COL = DZ
                                                     END WHERE
    ELSEIF(SF2%HAS_COL) THEN
                                                     ALLOCATE(SFAC%COL,  MOLD = SF2%COL   )
                                                     WHERE(SF2%COL /= DZ)
                                                              SFAC%COL = UNO / SF2%COL
                                                     ELSEWHERE
                                                              SFAC%COL = DZ
                                                     END WHERE
    END IF
    !
    IF(ALLOCATED(SFAC%EX1   ) .AND. SF2%HAS_EX1) THEN
                                                     WHERE(SF2%EX1 /= DZ)
                                                              SFAC%EX1 = SFAC%EX1 / SF2%EX1
                                                     ELSEWHERE
                                                              SFAC%EX1 = DZ
                                                     END WHERE
    ELSEIF(SF2%HAS_EX1) THEN
                                                     ALLOCATE(SFAC%EX1,  MOLD = SF2%EX1   )
                                                     WHERE(SF2%EX1 /= DZ)
                                                              SFAC%EX1 = UNO / SF2%EX1
                                                     ELSEWHERE
                                                              SFAC%EX1 = DZ
                                                     END WHERE
    END IF
    !
    IF(ALLOCATED(SFAC%EX2   ) .AND. SF2%HAS_EX2) THEN
                                                     WHERE(SF2%EX2 /= DZ)
                                                              SFAC%EX2 = SFAC%EX2 / SF2%EX2
                                                     ELSEWHERE
                                                              SFAC%EX2 = DZ
                                                     END WHERE
    ELSEIF(SF2%HAS_EX2) THEN
                                                     ALLOCATE(SFAC%EX2,  MOLD = SF2%EX2   )
                                                     WHERE(SF2%EX2 /= DZ)
                                                              SFAC%EX2 = UNO / SF2%EX2
                                                     ELSEWHERE
                                                              SFAC%EX2 = DZ
                                                     END WHERE
    END IF
    !
    IF(ALLOCATED(SFAC%EX3   ) .AND. SF2%HAS_EX3) THEN
                                                     WHERE(SF2%EX3 /= DZ)
                                                              SFAC%EX3 = SFAC%EX3 / SF2%EX3
                                                     ELSEWHERE
                                                              SFAC%EX3 = DZ
                                                     END WHERE
    ELSEIF(SF2%HAS_EX3) THEN
                                                     ALLOCATE(SFAC%EX3,  MOLD = SF2%EX3   )
                                                     WHERE(SF2%EX3 /= DZ)
                                                              SFAC%EX3 = UNO / SF2%EX3
                                                     ELSEWHERE
                                                              SFAC%EX3 = DZ
                                                     END WHERE
    END IF
    !
    !IF(ALLOCATED(SFAC%IU_ALL)) DEALLOCATE(SFAC%IU_ALL)
    !IF(ALLOCATED(SFAC%IU_ROW)) DEALLOCATE(SFAC%IU_ROW)
    !IF(ALLOCATED(SFAC%IU_COL)) DEALLOCATE(SFAC%IU_COL)
    !IF(ALLOCATED(SFAC%IU_EX1)) DEALLOCATE(SFAC%IU_EX1)
    !IF(ALLOCATED(SFAC%IU_EX2)) DEALLOCATE(SFAC%IU_EX2)
    !
  END FUNCTION
  !
  PURE ELEMENTAL SUBROUTINE SFAC_INITIALIZE(SFAC)
    CLASS(SFAC_DATA),INTENT(INOUT)::SFAC
    !
    !SFAC%HAS_EXTERNAL =FALSE
    !
    SFAC%HAS_ALL = FALSE
    SFAC%HAS_ROW = FALSE
    SFAC%HAS_COL = FALSE
    SFAC%HAS_EX1 = FALSE
    SFAC%HAS_EX2 = FALSE
    SFAC%HAS_EX3 = FALSE
    !SFAC%NEW_ALL = FALSE
    !SFAC%NEW_ROW = FALSE
    !SFAC%NEW_COL = FALSE
    !SFAC%NEW_EX1 = FALSE
    !SFAC%NEW_EX2 = FALSE
    !
    IF(ALLOCATED(SFAC%ALL   )) DEALLOCATE(SFAC%ALL)
    IF(ALLOCATED(SFAC%ROW   )) DEALLOCATE(SFAC%ROW)
    IF(ALLOCATED(SFAC%COL   )) DEALLOCATE(SFAC%COL)
    IF(ALLOCATED(SFAC%EX1   )) DEALLOCATE(SFAC%EX1)
    IF(ALLOCATED(SFAC%EX2   )) DEALLOCATE(SFAC%EX2)
    IF(ALLOCATED(SFAC%EX3   )) DEALLOCATE(SFAC%EX3)
    !IF(ALLOCATED(SFAC%IU_ALL)) DEALLOCATE(SFAC%IU_ALL)
    !IF(ALLOCATED(SFAC%IU_ROW)) DEALLOCATE(SFAC%IU_ROW)
    !IF(ALLOCATED(SFAC%IU_COL)) DEALLOCATE(SFAC%IU_COL)
    !IF(ALLOCATED(SFAC%IU_EX1)) DEALLOCATE(SFAC%IU_EX1)
    !IF(ALLOCATED(SFAC%IU_EX2)) DEALLOCATE(SFAC%IU_EX2)
    !
  END SUBROUTINE
  !
  PURE ELEMENTAL SUBROUTINE FINAL_SFAC_DEALLOCATE(SFAC)
    TYPE(SFAC_DATA),INTENT(INOUT)::SFAC
    !
    CALL SFAC_INITIALIZE(SFAC)
    !
  END SUBROUTINE
  !
  RECURSIVE SUBROUTINE SFAC_READ_AND_RETURN(SFAC, LN, IN, IOUT, ROW_WORD, ROW_DIM, COL_WORD, COL_DIM, EX1_WORD, EX1_DIM, EX2_WORD, EX2_DIM, EX3_WORD, EX3_DIM, SKIP_SFAC, SCRATCH, NO_INTERNAL) !MUST INCLUDE ROW_WORD AND ROW_DIM TOGETHER AND/OR COL_WORD AND COL_DIM TOGETHER WHEN CALLLING
    !
    CLASS(SFAC_DATA),INTENT(INOUT)::SFAC
    CHARACTER(*),    INTENT(INOUT):: LN
    INTEGER,         INTENT(IN   ):: IN, IOUT
    CHARACTER(*),    INTENT(IN   ), OPTIONAL:: ROW_WORD, COL_WORD
    INTEGER,         INTENT(IN   ), OPTIONAL:: ROW_DIM,  COL_DIM
    !
    CHARACTER(*),    INTENT(IN   ), OPTIONAL:: EX1_WORD, EX2_WORD, EX3_WORD
    INTEGER,         INTENT(IN   ), OPTIONAL:: EX1_DIM,  EX2_DIM,  EX3_DIM
    INTEGER,         INTENT(IN   ), OPTIONAL:: SCRATCH
    !
    LOGICAL,         INTENT(IN   ),OPTIONAL:: SKIP_SFAC, NO_INTERNAL
    INTEGER:: IU
    INTEGER:: LLOC, ISTART, ISTOP, OLDLOC, INFILE
    LOGICAL:: INCLUDE_SFAC, HAS_ROW, HAS_COL, HAS_ALL, HAS_EX1, HAS_EX2, HAS_EX3, EOL
    CHARACTER(:), ALLOCATABLE:: RWORD, CWORD, EXKEY1, EXKEY2, EXKEY3
    REAL(REAL64), DIMENSION(:), ALLOCATABLE:: TMP
    REAL(REAL64):: TMP_ALL
    !
    HAS_ROW      = FALSE
    HAS_COL      = FALSE
    HAS_ALL      = FALSE
    HAS_EX1      = FALSE
    HAS_EX2      = FALSE
    HAS_EX3      = FALSE
    !
    IF(PRESENT(SCRATCH)) THEN
        IF(SCRATCH /= Z) THEN
                              INFILE = SCRATCH
        ELSE
                              INFILE = IN
        END IF
    ELSE
        INFILE = IN
    END IF
    !
    IF(PRESENT(ROW_WORD)) THEN
        ALLOCATE(RWORD, SOURCE=ROW_WORD)
    ELSE
        ALLOCATE(RWORD, SOURCE='nR')
    END IF
    !
    IF(PRESENT(COL_WORD)) THEN
        ALLOCATE(CWORD, SOURCE=COL_WORD)
    ELSE
        ALLOCATE(CWORD, SOURCE='nC')
    END IF
    !
    IF(PRESENT(EX1_WORD)) THEN
        ALLOCATE(EXKEY1, SOURCE=EX1_WORD)
    ELSE
        ALLOCATE(EXKEY1, SOURCE='n1')
    END IF
    !
    IF(PRESENT(EX2_WORD)) THEN
        ALLOCATE(EXKEY2, SOURCE=EX2_WORD)
    ELSE
        ALLOCATE(EXKEY2, SOURCE='n2')
    END IF
    !
    IF(PRESENT(EX3_WORD)) THEN
        ALLOCATE(EXKEY3, SOURCE=EX3_WORD)
    ELSE
        ALLOCATE(EXKEY3, SOURCE='n3')
    END IF
    !
    IF(PRESENT(SKIP_SFAC)) THEN
        INCLUDE_SFAC = .NOT. SKIP_SFAC
    ELSE
        INCLUDE_SFAC = TRUE
    END IF
    !
    !
    LLOC = ONE
    IF(INCLUDE_SFAC) CALL PARSE_WORD(LN,LLOC,ISTART,ISTOP,TRUE,TRUE)  !BYPASS SFAC (or any keyword that started this subroutine)
    !
    CALL PARSE_WORD_UP(LN,LLOC,ISTART,ISTOP,TRUE, OLD_LOC=OLDLOC, EOL=EOL)
    !
    IF (IS_BLANK( LN(ISTART:ISTOP) ) .OR. EOL) THEN
                                                 CALL READ_TO_DATA(LN,IU,IOUT)
                                                 LLOC   = ONE
                                                 OLDLOC = ONE
                                                 CALL PARSE_WORD_UP(LN,LLOC,ISTART,ISTOP,TRUE)
    END IF
    !
    IF     (LN(ISTART:ISTOP)== RWORD) THEN
                                                  HAS_ROW = TRUE
    ELSEIF (LN(ISTART:ISTOP)== CWORD) THEN
                                                  HAS_COL = TRUE
    ELSEIF (LN(ISTART:ISTOP)== "ALL") THEN
                                                  HAS_ALL = TRUE
    ELSEIF (LN(ISTART:ISTOP)== EXKEY1) THEN
                                                  HAS_EX1 = TRUE
    ELSEIF (LN(ISTART:ISTOP)== EXKEY2) THEN
                                                  HAS_EX2 = TRUE
    ELSEIF (LN(ISTART:ISTOP)== EXKEY3) THEN
                                                  HAS_EX3 = TRUE
    ELSE
            HAS_ALL = TRUE
            LLOC = OLDLOC
    END IF
    !
    !IF (HAS_ROW .AND. SFAC%HAS_ROW) CALL FILE_IO_ERROR( Z, LINE=LN, INFILE=IN, OUTPUT=IOUT, MSG= 'SFAC FOUND WITH KEYWORD '//RWORD//' THAT WAS SPECIFIED TWICE WITHIN BLOCK OR STRESS PERIOD.')
    !IF (HAS_COL .AND. SFAC%HAS_COL) CALL FILE_IO_ERROR( Z, LINE=LN, INFILE=IN, OUTPUT=IOUT, MSG= 'SFAC FOUND WITH KEYWORD '//CWORD//' THAT WAS SPECIFIED TWICE WITHIN BLOCK OR STRESS PERIOD.')
    !IF (HAS_ALL .AND. SFAC%HAS_ALL) CALL FILE_IO_ERROR( Z, LINE=LN, INFILE=IN, OUTPUT=IOUT, MSG= 'SFAC FOUND THAT IS APPLIED TO ALL PROPERTIES HAS BEEN CALLED TWICE WITHIN BLOCK OR STRESS PERIOD.')
    !IF (HAS_EX1 .AND. SFAC%HAS_EX1) CALL FILE_IO_ERROR( Z, LINE=LN, INFILE=IN, OUTPUT=IOUT, MSG= 'SFAC FOUND WITH KEYWORD '//EXKEY1//' THAT WAS SPECIFIED TWICE WITHIN BLOCK OR STRESS PERIOD.')
    !IF (HAS_EX2 .AND. SFAC%HAS_EX2) CALL FILE_IO_ERROR( Z, LINE=LN, INFILE=IN, OUTPUT=IOUT, MSG= 'SFAC FOUND WITH KEYWORD '//EXKEY2//' THAT WAS SPECIFIED TWICE WITHIN BLOCK OR STRESS PERIOD.')
    !IF (HAS_EX3 .AND. SFAC%HAS_EX3) CALL FILE_IO_ERROR( Z, LINE=LN, INFILE=IN, OUTPUT=IOUT, MSG= 'SFAC FOUND WITH KEYWORD '//EXKEY3//' THAT WAS SPECIFIED TWICE WITHIN BLOCK OR STRESS PERIOD.')
    !
    IU = Z
    IF     (HAS_ROW) THEN
                         ALLOCATE( TMP(ROW_DIM) )
                         CALL ULOAD_VECTOR(TMP, LLOC, LN, IOUT, IN, IU, SCRATCH=SCRATCH, NO_INTERNAL=NO_INTERNAL)
                         IF(SFAC%HAS_ROW) THEN
                             SFAC%ROW = SFAC%ROW * TMP
                         ELSE
                             CALL MOVE_ALLOC(TMP,SFAC%ROW)
                             SFAC%HAS_ROW = TRUE
                         END IF
                         !
                         !!!WHERE(SFAC%ROW == DZ) SFAC%ROW = UNO
                         !IF(IU /= Z) ALLOCATE(SFAC%IU_ROW, SOURCE=IU)
                         !SFAC%NEW_ROW = TRUE
                         !
    ELSEIF (HAS_COL) THEN
                         ALLOCATE( TMP(COL_DIM) )
                         CALL ULOAD_VECTOR(TMP, LLOC, LN, IOUT, IN, IU, SCRATCH=SCRATCH, NO_INTERNAL=NO_INTERNAL)
                         IF(SFAC%HAS_COL) THEN
                             SFAC%COL = SFAC%COL * TMP
                         ELSE
                             CALL MOVE_ALLOC(TMP,SFAC%COL)
                             SFAC%HAS_COL = TRUE
                         END IF
                         !
                         !!!WHERE(SFAC%COL == DZ) SFAC%COL = UNO
                         !IF(IU /= Z) ALLOCATE(SFAC%IU_COL, SOURCE=IU)
                         !SFAC%NEW_COL = TRUE
                         !
    ELSEIF (HAS_ALL) THEN
                         CALL ULOAD_SCALAR(TMP_ALL, LLOC, LN, IOUT, IN, IU, SCRATCH=SCRATCH, NO_INTERNAL=NO_INTERNAL)
                         IF(SFAC%HAS_ALL) THEN
                             SFAC%ALL = SFAC%ALL * TMP_ALL
                         ELSE
                             ALLOCATE(SFAC%ALL, SOURCE=TMP_ALL)
                             SFAC%HAS_ALL = TRUE
                         END IF
                         !
                         !!!IF(SFAC%ALL == DZ) SFAC%ALL = UNO
                         !IF(IU /= Z) ALLOCATE(SFAC%IU_ALL, SOURCE=IU)
                         !SFAC%NEW_ALL = TRUE
    ELSEIF (HAS_EX1) THEN
                         ALLOCATE( TMP(EX1_DIM) )
                         CALL ULOAD_VECTOR(TMP, LLOC, LN, IOUT, IN, IU, SCRATCH=SCRATCH, NO_INTERNAL=NO_INTERNAL)
                         IF(SFAC%HAS_EX1) THEN
                             SFAC%EX1 = SFAC%EX1 * TMP
                         ELSE
                             CALL MOVE_ALLOC(TMP,SFAC%EX1)
                             SFAC%HAS_EX1 = TRUE
                         END IF
                         !
                         !!!WHERE(SFAC%EX1 == DZ) SFAC%EX1 = UNO
                         !IF(IU /= Z) ALLOCATE(SFAC%IU_EX1, SOURCE=IU)
                         !SFAC%NEW_EX1 = TRUE
                         !
    ELSEIF (HAS_EX2) THEN
                         ALLOCATE( TMP(EX2_DIM) )
                         CALL ULOAD_VECTOR(TMP, LLOC, LN, IOUT, IN, IU, SCRATCH=SCRATCH, NO_INTERNAL=NO_INTERNAL)
                         IF(SFAC%HAS_EX2) THEN
                             SFAC%EX2 = SFAC%EX2 * TMP
                         ELSE
                             CALL MOVE_ALLOC(TMP,SFAC%EX2)
                             SFAC%HAS_EX2 = TRUE
                         END IF
                         !
                         !!!WHERE(SFAC%EX2 == DZ) SFAC%EX2 = UNO
                         !IF(IU /= Z) ALLOCATE(SFAC%IU_EX2, SOURCE=IU)
                         !SFAC%NEW_EX2 = TRUE
                         !
    ELSEIF (HAS_EX3) THEN
                         ALLOCATE( TMP(EX3_DIM) )
                         CALL ULOAD_VECTOR(TMP, LLOC, LN, IOUT, IN, IU, SCRATCH=SCRATCH, NO_INTERNAL=NO_INTERNAL)
                         IF(SFAC%HAS_EX3) THEN
                             SFAC%EX3 = SFAC%EX3 * TMP
                         ELSE
                             CALL MOVE_ALLOC(TMP,SFAC%EX3)
                             SFAC%HAS_EX3 = TRUE
                         END IF
                         !
                         !!!WHERE(SFAC%EX3 == DZ) SFAC%EX3 = UNO
                         !IF(IU /= Z) ALLOCATE(SFAC%IU_EX3, SOURCE=IU)
                         !SFAC%NEW_EX3 = TRUE
                         !
    END IF
    !
    !IF(IU /= Z) SFAC%HAS_EXTERNAL = TRUE
    IF(ALLOCATED(TMP)) DEALLOCATE(TMP)
    !
    END SUBROUTINE
  !
!  SUBROUTINE SFAC_READ_NEXT_EXTERNAL(SFAC, REMOVE_OLD, IOUT, NOSTOP)
!    CLASS(SFAC_DATA),INTENT(INOUT):: SFAC
!    LOGICAL,         INTENT(IN   ):: REMOVE_OLD, NOSTOP
!    INTEGER,         INTENT(IN   ):: IOUT
!    INTEGER:: LLOC, Z
!    CHARACTER(:), ALLOCATABLE:: LN
!    DOUBLE PRECISION, DIMENSION(:),ALLOCATABLE:: DIV
!    DOUBLE PRECISION:: DIV_ALL, DZ
!    !
!    SFAC%NEW_ALL = .FALSE.
!    SFAC%NEW_ROW = .FALSE.
!    SFAC%NEW_COL = .FALSE.
!    SFAC%NEW_EX1 = .FALSE.
!    SFAC%NEW_EX2 = .FALSE.
!    !
!    IF(SFAC%HAS_EXTERNAL) THEN
!          !
!          Z = 0
!          DZ = 0D0
!          ALLOCATE(CHARACTER(768)::LN)
!          !
!          IF (ALLOCATED(SFAC%IU_ROW)) THEN
!                                         IF(REMOVE_OLD) ALLOCATE(DIV, SOURCE=SFAC%ROW)
!                                         LLOC = 1
!                                         CALL ULOAD_VECTOR(SFAC%ROW, LLOC, LN, IOUT, SFAC%IU_ROW, SFAC%IU_ROW, NOSTOP=NOSTOP)
!                                         !
!                                         IF(SFAC%IU_ROW /= Z) SFAC%NEW_ROW = .TRUE.
!                                         IF(SFAC%IU_ROW == Z) THEN  !FAILED TO LOAD NEXT EXTERNAL, KEEP OLD SFAC
!                                                      SFAC%ROW =  DIV
!                                         ELSEIF(REMOVE_OLD) THEN
!                                                      WHERE ( DIV /= DZ) SFAC%ROW = SFAC%ROW / DIV
!                                                      DEALLOCATE(DIV)
!                                         END IF
!          END IF
!          !
!          IF (ALLOCATED(SFAC%IU_COL)) THEN
!                                         IF(REMOVE_OLD) ALLOCATE(DIV, SOURCE=SFAC%COL)
!                                         LLOC = 1
!                                         CALL ULOAD_VECTOR(SFAC%COL, LLOC, LN, IOUT, SFAC%IU_COL, SFAC%IU_COL, NOSTOP=NOSTOP)
!                                         !
!                                         IF(SFAC%IU_COL /= Z) SFAC%NEW_COL = .TRUE.
!                                         IF(SFAC%IU_COL == Z) THEN  !FAILED TO LOAD NEXT EXTERNAL, KEEP OLD SFAC
!                                                      SFAC%COL =  DIV
!                                         ELSEIF(REMOVE_OLD) THEN
!                                                      WHERE ( DIV /= DZ) SFAC%COL = SFAC%COL / DIV
!                                                      DEALLOCATE(DIV)
!                                         END IF
!          END IF
!          !
!          IF (ALLOCATED(SFAC%IU_ALL)) THEN
!                                         IF(REMOVE_OLD) DIV_ALL = SFAC%ALL
!                                         LLOC = 1
!                                         CALL ULOAD_SCALAR(SFAC%ALL, LLOC, LN, IOUT, SFAC%IU_ALL, SFAC%IU_ALL, NOSTOP=NOSTOP)
!                                         !
!                                         IF(SFAC%IU_ALL /= Z) SFAC%NEW_ALL = .TRUE.
!                                         IF(SFAC%IU_ALL == Z ) THEN
!                                             SFAC%ALL = DIV_ALL
!                                         ELSEIF(REMOVE_OLD .AND. DIV_ALL /= DZ) THEN
!                                             SFAC%ALL = SFAC%ALL / DIV_ALL
!                                         END IF
!          END IF
!          !
!          IF (ALLOCATED(SFAC%IU_EX1)) THEN
!                                         IF(REMOVE_OLD) ALLOCATE(DIV, SOURCE=SFAC%EX1)
!                                         LLOC = 1
!                                         CALL ULOAD_VECTOR(SFAC%EX1, LLOC, LN, IOUT, SFAC%IU_EX1, SFAC%IU_EX1, NOSTOP=NOSTOP)
!                                         !
!                                         IF(SFAC%IU_EX1 /= Z) SFAC%NEW_EX1 = .TRUE.
!                                         IF(SFAC%IU_EX1 == Z) THEN  !FAILED TO LOAD NEXT EXTERNAL, KEEP OLD SFAC
!                                                      SFAC%EX1 =  DIV
!                                         ELSEIF(REMOVE_OLD) THEN
!                                                      WHERE ( DIV /= DZ) SFAC%EX1 = SFAC%EX1 / DIV
!                                                      DEALLOCATE(DIV)
!                                         END IF
!          END IF
!          !
!          IF (ALLOCATED(SFAC%IU_EX2)) THEN
!                                         IF(REMOVE_OLD) ALLOCATE(DIV, SOURCE=SFAC%EX2)
!                                         LLOC = 1
!                                         CALL ULOAD_VECTOR(SFAC%EX2, LLOC, LN, IOUT, SFAC%IU_EX2, SFAC%IU_EX2, NOSTOP=NOSTOP)
!                                         !
!                                         IF(SFAC%IU_EX2 /= Z) SFAC%NEW_EX2 = .TRUE.
!                                         IF(SFAC%IU_EX2 == Z) THEN  !FAILED TO LOAD NEXT EXTERNAL, KEEP OLD SFAC
!                                                      SFAC%EX2 =  DIV
!                                         ELSEIF(REMOVE_OLD) THEN
!                                                      WHERE ( DIV /= DZ) SFAC%EX2 = SFAC%EX2 / DIV
!                                                      DEALLOCATE(DIV)
!                                         END IF
!          END IF
!          DEALLOCATE(LN)
!    END IF
!    !
!  END SUBROUTINE
  !
  PURE SUBROUTINE SFAC_SET_ANOTHER_ALL(SFAC, SCALE)
    CLASS(SFAC_DATA), INTENT(INOUT):: SFAC
    REAL(REAL64),     INTENT(IN   ):: SCALE
    !
    IF(SCALE /= UNO) THEN
                      IF(SFAC%HAS_ALL) THEN
                          SFAC%ALL = SFAC%ALL * SCALE
                      ELSE
                          ALLOCATE(SFAC%ALL, SOURCE=SCALE)
                          SFAC%HAS_ALL = TRUE
                      END IF
    END IF
    !
  END SUBROUTINE
  !
  !PURE SUBROUTINE SFAC_INIT_ALL(SFAC, SCALE)
  !  CLASS(SFAC_DATA), INTENT(INOUT):: SFAC
  !  REAL(REAL64),     INTENT(IN   ):: SCALE
  !  !
  !  IF(SFAC%HAS_ALL .AND. SCALE /= UNO) THEN
  !      SFAC%ALL = SFAC%ALL * SCALE
  !  ELSE
  !      ALLOCATE(SFAC%ALL, SOURCE=SCALE)
  !      SFAC%HAS_ALL = TRUE
  !  END IF
  !  !
  !END SUBROUTINE
  !

  !
  PURE SUBROUTINE SFAC_APPLY_TO_SCALAR(SFAC, VAR)
    CLASS(SFAC_DATA), INTENT(INOUT):: SFAC
    CLASS(*),     INTENT(INOUT):: VAR
    !
    SELECT TYPE(VAR)
    TYPE IS (REAL(REAL64 ))  ! DOUBLE PRECISION
        IF (SFAC%HAS_ALL) VAR = VAR * SFAC%ALL
        !
        IF (SFAC%HAS_ROW) VAR = VAR * PRODUCT(SFAC%ROW) !GET PRODUCT
        !
        IF (SFAC%HAS_COL) VAR = VAR * PRODUCT(SFAC%COL)
    TYPE IS (REAL(REAL32 ))  ! SINGLE PRECISION
        IF (SFAC%HAS_ALL) VAR = VAR * SFAC%ALL
        !
        IF (SFAC%HAS_ROW) VAR = VAR * PRODUCT(SFAC%ROW) !GET PRODUCT
        !
        IF (SFAC%HAS_COL) VAR = VAR * PRODUCT(SFAC%COL)
    TYPE IS (REAL(REAL128))  ! QUAD   PRECISION
        IF (SFAC%HAS_ALL) VAR = VAR * SFAC%ALL
        !
        IF (SFAC%HAS_ROW) VAR = VAR * PRODUCT(SFAC%ROW) !GET PRODUCT
        !
        IF (SFAC%HAS_COL) VAR = VAR * PRODUCT(SFAC%COL)
    END SELECT
    !
  END SUBROUTINE
  !
  PURE SUBROUTINE SFAC_APPLY_TO_VECTOR(SFAC, VAR)
    CLASS(SFAC_DATA),                          INTENT(INOUT):: SFAC
    CLASS(*),        DIMENSION(:), CONTIGUOUS, INTENT(INOUT):: VAR
    !
    SELECT TYPE(VAR)
    TYPE IS (REAL(REAL64 ))  ! DOUBLE PRECISION
        IF (SFAC%HAS_ALL) VAR = VAR * SFAC%ALL
        !
        IF (SFAC%HAS_ROW) VAR = VAR * SFAC%ROW
        !
        IF (SFAC%HAS_COL) VAR = VAR * SFAC%COL  !ASSUMES COL IS SAME SIZE AS VAR
    TYPE IS (REAL(REAL32 ))  ! SINGLE PRECISION
        IF (SFAC%HAS_ALL) VAR = VAR * SFAC%ALL
        !
        IF (SFAC%HAS_ROW) VAR = VAR * SFAC%ROW
        !
        IF (SFAC%HAS_COL) VAR = VAR * SFAC%COL  !ASSUMES COL IS SAME SIZE AS VAR
    TYPE IS (REAL(REAL128))  ! QUAD   PRECISION
        IF (SFAC%HAS_ALL) VAR = VAR * SFAC%ALL
        !
        IF (SFAC%HAS_ROW) VAR = VAR * SFAC%ROW
        !
        IF (SFAC%HAS_COL) VAR = VAR * SFAC%COL  !ASSUMES COL IS SAME SIZE AS VAR
    END SELECT
    !
  END SUBROUTINE
  !
  PURE SUBROUTINE SFAC_APPLY_TO_ARRAY(SFAC, VAR, ROW_ON_DIM1)
    CLASS(SFAC_DATA),                     INTENT(INOUT):: SFAC
    CLASS(*), DIMENSION(:,:), CONTIGUOUS, INTENT(INOUT):: VAR
    LOGICAL,          OPTIONAL,           INTENT(IN   ):: ROW_ON_DIM1
    LOGICAL:: ROW_DIM1
    INTEGER:: I
    !
    ROW_DIM1 = FALSE
    IF(PRESENT(ROW_ON_DIM1)) ROW_DIM1 = ROW_ON_DIM1
    !
    !
    SELECT TYPE(VAR)
    TYPE IS (REAL(REAL64 ))  ! DOUBLE PRECISION
        !
        IF (SFAC%HAS_ALL) VAR = VAR * SFAC%ALL
        !
        IF (SFAC%HAS_ROW) THEN
              IF(ROW_DIM1) THEN
                   DO I=1, SIZE(VAR,1)
                       VAR(:,I) = VAR(:,I) * SFAC%ROW
                   END DO
              ELSE
                   DO I=1, UBOUND(VAR,1)
                       VAR(I,:) = VAR(I,:) * SFAC%ROW
                   END DO
              END IF
        END IF
        !
        IF (SFAC%HAS_COL) THEN
              IF(ROW_DIM1) THEN
                   DO I=1, UBOUND(VAR,2)
                       VAR(I,:) = VAR(I,:) * SFAC%COL
                   END DO
              ELSE
                   DO I=1, SIZE(VAR,2)
                       VAR(:,I) = VAR(:,I) * SFAC%COL
                   END DO
              END IF
        END IF
    TYPE IS (REAL(REAL32 ))  ! SINGLE PRECISION
        !
        IF (SFAC%HAS_ALL) VAR = VAR * SFAC%ALL
        !
        IF (SFAC%HAS_ROW) THEN
              IF(ROW_DIM1) THEN
                   DO I=1, SIZE(VAR,1)
                       VAR(:,I) = VAR(:,I) * SFAC%ROW
                   END DO
              ELSE
                   DO I=1, UBOUND(VAR,1)
                       VAR(I,:) = VAR(I,:) * SFAC%ROW
                   END DO
              END IF
        END IF
        !
        IF (SFAC%HAS_COL) THEN
              IF(ROW_DIM1) THEN
                   DO I=1, UBOUND(VAR,2)
                       VAR(I,:) = VAR(I,:) * SFAC%COL
                   END DO
              ELSE
                   DO I=1, SIZE(VAR,2)
                       VAR(:,I) = VAR(:,I) * SFAC%COL
                   END DO
              END IF
        END IF
    TYPE IS (REAL(REAL128))  ! QUAD   PRECISION
        !
        IF (SFAC%HAS_ALL) VAR = VAR * SFAC%ALL
        !
        IF (SFAC%HAS_ROW) THEN
              IF(ROW_DIM1) THEN
                   DO I=1, SIZE(VAR,1)
                       VAR(:,I) = VAR(:,I) * SFAC%ROW
                   END DO
              ELSE
                   DO I=1, UBOUND(VAR,1)
                       VAR(I,:) = VAR(I,:) * SFAC%ROW
                   END DO
              END IF
        END IF
        !
        IF (SFAC%HAS_COL) THEN
              IF(ROW_DIM1) THEN
                   DO I=1, UBOUND(VAR,2)
                       VAR(I,:) = VAR(I,:) * SFAC%COL
                   END DO
              ELSE
                   DO I=1, SIZE(VAR,2)
                       VAR(:,I) = VAR(:,I) * SFAC%COL
                   END DO
              END IF
        END IF
    END SELECT
    !
  END SUBROUTINE
  !
!!!  PURE SUBROUTINE SFAC_APPLY_TO_SCALAR(SFAC, VAR)
!!!    CLASS(SFAC_DATA), INTENT(INOUT):: SFAC
!!!    DOUBLE PRECISION, INTENT(INOUT):: VAR
!!!    !
!!!    IF (SFAC%HAS_ALL) VAR = VAR * SFAC%ALL
!!!    !
!!!    IF (SFAC%HAS_ROW) VAR = VAR * PRODUCT(SFAC%ROW) !GET PRODUCT
!!!    !
!!!    IF (SFAC%HAS_COL) VAR = VAR * PRODUCT(SFAC%COL)
!!!    !
!!!  END SUBROUTINE
!!!  !
!!!  PURE SUBROUTINE SFAC_APPLY_TO_VECTOR(SFAC, VAR)
!!!    CLASS(SFAC_DATA),                           INTENT(INOUT):: SFAC
!!!    DOUBLE PRECISION, DIMENSION(:), CONTIGUOUS, INTENT(INOUT):: VAR
!!!    !
!!!    IF (SFAC%HAS_ALL) VAR = VAR * SFAC%ALL
!!!    !
!!!    IF (SFAC%HAS_ROW) VAR = VAR * SFAC%ROW
!!!    !
!!!    IF (SFAC%HAS_COL) VAR = VAR * SFAC%COL  !ASSUMES COL IS SAME SIZE AS VAR
!!!    !
!!!  END SUBROUTINE
!!!  !
!!!  PURE SUBROUTINE SFAC_APPLY_TO_ARRAY(SFAC, VAR, ROW_ON_DIM1)
!!!    CLASS(SFAC_DATA),                             INTENT(INOUT):: SFAC
!!!    DOUBLE PRECISION, DIMENSION(:,:), CONTIGUOUS, INTENT(INOUT):: VAR
!!!    LOGICAL,          OPTIONAL,                   INTENT(IN   ):: ROW_ON_DIM1
!!!    LOGICAL:: ROW_DIM1
!!!    INTEGER:: I
!!!    !
!!!    ROW_DIM1 = FALSE
!!!    IF(PRESENT(ROW_ON_DIM1)) ROW_DIM1 = ROW_ON_DIM1
!!!    !
!!!    IF (SFAC%HAS_ALL) VAR = VAR * SFAC%ALL
!!!    !
!!!    IF (SFAC%HAS_ROW) THEN
!!!          IF(ROW_DIM1) THEN
!!!               DO I=1, SIZE(VAR,1)
!!!                   VAR(:,I) = VAR(:,I) * SFAC%ROW
!!!               END DO
!!!          ELSE
!!!               DO I=1, UBOUND(VAR,1)
!!!                   VAR(I,:) = VAR(I,:) * SFAC%ROW
!!!               END DO
!!!          END IF
!!!    END IF
!!!    !
!!!    IF (SFAC%HAS_COL) THEN
!!!          IF(ROW_DIM1) THEN
!!!               DO I=1, UBOUND(VAR,2)
!!!                   VAR(I,:) = VAR(I,:) * SFAC%COL
!!!               END DO
!!!          ELSE
!!!               DO I=1, SIZE(VAR,2)
!!!                   VAR(:,I) = VAR(:,I) * SFAC%COL
!!!               END DO
!!!          END IF
!!!    END IF
!!!    !
!!!  END SUBROUTINE
!!!  !
!!!  PURE SUBROUTINE SFAC_APPLY_TO_SCALAR_REAL(SFAC, VAR)
!!!    CLASS(SFAC_DATA),INTENT(INOUT):: SFAC
!!!    REAL(REAL32),    INTENT(INOUT):: VAR
!!!    !
!!!    IF (SFAC%HAS_ALL) VAR = VAR * SNGL(SFAC%ALL)
!!!    !
!!!    IF (SFAC%HAS_ROW) VAR = VAR * SNGL(PRODUCT(SFAC%ROW)) !GET PRODUCT
!!!    !
!!!    IF (SFAC%HAS_COL) VAR = VAR * SNGL(PRODUCT(SFAC%COL))
!!!    !
!!!  END SUBROUTINE
!!!  !
!!!  PURE SUBROUTINE SFAC_APPLY_TO_VECTOR_REAL(SFAC, VAR)
!!!    CLASS(SFAC_DATA),                       INTENT(INOUT):: SFAC
!!!    REAL(REAL32), DIMENSION(:), CONTIGUOUS, INTENT(INOUT):: VAR
!!!    !
!!!    IF (SFAC%HAS_ALL) VAR = VAR * SNGL(SFAC%ALL)
!!!    !
!!!    IF (SFAC%HAS_ROW) VAR = VAR * SNGL(SFAC%ROW)
!!!    !
!!!    IF (SFAC%HAS_COL) VAR = VAR * SNGL(SFAC%COL)  !ASSUMES COL IS SAME SIZE AS VAR
!!!    !
!!!  END SUBROUTINE
!!!  !
!!!  PURE SUBROUTINE SFAC_APPLY_TO_ARRAY_REAL(SFAC, VAR, ROW_ON_DIM1)
!!!    CLASS(SFAC_DATA),                         INTENT(INOUT):: SFAC
!!!    REAL(REAL32), DIMENSION(:,:), CONTIGUOUS, INTENT(INOUT):: VAR
!!!    LOGICAL,                      OPTIONAL,   INTENT(IN   ):: ROW_ON_DIM1
!!!    LOGICAL:: ROW_DIM1
!!!    INTEGER:: I
!!!    !
!!!    ROW_DIM1 = FALSE
!!!    IF(PRESENT(ROW_ON_DIM1)) ROW_DIM1 = ROW_ON_DIM1
!!!    !
!!!    IF (SFAC%HAS_ALL) VAR = VAR * SFAC%ALL
!!!    !
!!!    IF (SFAC%HAS_ROW) THEN
!!!          IF(ROW_DIM1) THEN
!!!               DO I=1, SIZE(VAR,1)
!!!                   VAR(:,I) = VAR(:,I) * SNGL(SFAC%ROW)
!!!               END DO
!!!          ELSE
!!!               DO I=1, UBOUND(VAR,1)
!!!                   VAR(I,:) = VAR(I,:) * SNGL(SFAC%ROW)
!!!               END DO
!!!          END IF
!!!    END IF
!!!    !
!!!    IF (SFAC%HAS_COL) THEN
!!!          IF(ROW_DIM1) THEN
!!!               DO I=1, UBOUND(VAR,2)
!!!                   VAR(I,:) = VAR(I,:) * SNGL(SFAC%COL)
!!!               END DO
!!!          ELSE
!!!               DO I=1, SIZE(VAR,2)
!!!                   VAR(:,I) = VAR(:,I) * SNGL(SFAC%COL)
!!!               END DO
!!!          END IF
!!!    END IF
!!!    !
!!!  END SUBROUTINE
  !
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !
  RECURSIVE SUBROUTINE ULOAD_SCALAR(VAR, LLOC, LN, IOUT, IN, IU, NOID, BINARY, ID, NOSTOP, SFAC, EX1_WORD, EX1_DIM, EX2_WORD, EX2_DIM, EX3_WORD, EX3_DIM, SCRATCH, ENTIRE_LINE, NO_INTERNAL, NO_BINARY, NO_REPEAT, OLD_IU, TEXT, MSG)
    CLASS(*),                  INTENT(INOUT):: VAR       !Variable to set by ULOAD
    INTEGER,                   INTENT(INOUT):: LLOC      !SET TO -1 IF CHARACTER::VAR IS NOT LONG ENOUGH TO HOLD INPUT
    CHARACTER(*),              INTENT(INOUT):: LN        !Temp line to holds read input that is processed
    INTEGER,                   INTENT(IN   ):: IN, IOUT  !File that input originated from; error output location
    INTEGER,                   INTENT(INOUT):: IU        ! If IU=0 input is dermined from directives in IN or SCRATCH, IU /= 0 indicates input is on that file unit. Same as IN having a LN = "EXTERNAL IU"
    !
    LOGICAL,         OPTIONAL, INTENT(IN   ):: NOID, BINARY, NOSTOP
    INTEGER,         OPTIONAL, INTENT(INOUT):: ID
    TYPE(SFAC_DATA), OPTIONAL, INTENT(  OUT):: SFAC
    !
    CHARACTER(*),    OPTIONAL, INTENT(IN   ):: EX1_WORD, EX2_WORD, EX3_WORD
    INTEGER,         OPTIONAL, INTENT(IN   ):: EX1_DIM,  EX2_DIM,  EX3_DIM
    INTEGER,         OPTIONAL, INTENT(IN   ):: SCRATCH
    LOGICAL,         OPTIONAL, INTENT(IN   ):: ENTIRE_LINE, NO_INTERNAL, NO_BINARY, NO_REPEAT
    INTEGER,         OPTIONAL, INTENT(INOUT):: OLD_IU
    CHARACTER(*),    OPTIONAL, INTENT(IN   ):: TEXT
    CHARACTER(*),    OPTIONAL, INTENT(IN   ):: MSG
    !
    TYPE(SFAC_DATA):: SF!1, SF2
    !
    INTEGER:: IERR, ISTART, ISTOP, I, N, INFILE, ERROR_IU, LINELEN
    TYPE(GENERIC_INPUT_FILE), ALLOCATABLE:: FL
    LOGICAL:: KEEP_IU, READ_ID, BIN, ALLOW_ERROR, CLEAR_IU, READ_FULL_LINE, NO_INTERN, NEG_LLOC, NO_MAIN_KEY
    REAL(REAL64):: SFAC_FILE, CONST ! ENSURE ITS ALWAYS DOUBLE PRECISION
    CHARACTER(12):: EXT, FORM_CHK   ! Note 12 is passed in at CALL UPPER(12, EXT)
    !
    IERR = Z
    NEG_LLOC  = FALSE
    SFAC_FILE = UNO
    ERROR_IU = IN
    NO_MAIN_KEY=FALSE
    !
    IF(PRESENT(SCRATCH)) THEN
        IF(SCRATCH /= Z) THEN
                              INFILE = SCRATCH
        ELSE
                              INFILE = IN
        END IF
    ELSE
        INFILE = IN
    END IF
    !
    IF(PRESENT(ID)) ID = Z
    !
    IF(PRESENT(NOID)) THEN
        READ_ID = .NOT. NOID
    ELSE
        READ_ID = TRUE
    END IF
    !
    IF(PRESENT(BINARY)) THEN
        BIN = BINARY
    ELSE
        BIN = FALSE
    END IF
    !
    IF(PRESENT(NOSTOP)) THEN
        ALLOW_ERROR = .NOT. NOSTOP
    ELSE
        ALLOW_ERROR = TRUE
    END IF
    !
    IF(PRESENT(ENTIRE_LINE)) THEN
        READ_FULL_LINE = ENTIRE_LINE
    ELSE
        READ_FULL_LINE = FALSE
    END IF
    !
    IF(PRESENT(NO_INTERNAL)) THEN
        NO_INTERN = NO_INTERNAL
    ELSE
        NO_INTERN = FALSE
    END IF
    !
    SELECT TYPE (VAR)
    TYPE IS (CHARACTER(*));  LINELEN = LEN(VAR)
    TYPE IS (IXJ_STRUCTURE); READ_ID = FALSE
    END SELECT
    !
    CLEAR_IU = FALSE
    IF(IU /= Z) THEN
                    INQUIRE(IU,FORM=FORM_CHK, OPENED=KEEP_IU)
                    IF(.NOT. KEEP_IU) CALL STOP_ERROR( LINE=LN, INFILE=IN, OUTPUT=IOUT, MSG= 'ULOAD ERROR: RECIEVED UNIT NUMBER "'//NUM2STR(IU)//'" WHICH IS NOT ASSOCIATED WITH A FILE. THIS COULD BE THE RESULT OF USING "EXTERNAL" OR "DATAUNIT" WITH A UNIT THAT IS NOT OPENED BY THE NAME FILE.', MSG2=MSG)
                    BIN = FORM_CHK /= 'FORMATTED'
                    !
                    ERROR_IU = IU
                    KEEP_IU  = TRUE
                    LLOC     = ONE
                    IF(.NOT. ALLOW_ERROR) CLEAR_IU = TRUE
                    IF(.NOT. BIN) CALL READ_TO_DATA(LN,IU,IOUT)  !UNIT IS BEING PASSED IN SO READ IN LINE FROM IT
    ELSE
                    KEEP_IU = FALSE
    END IF
    !
    CALL SF%INIT()
    IF(PRESENT(SFAC)) CALL SFAC%INIT()
    !
    IF(.NOT. BIN) THEN
                      DO !CHECK FOR KEYWORD OF SFAC BEFORE LIST
                           N = LLOC
                           CALL GET_WORD(LN, LLOC, ISTART, ISTOP, EXT, COM_STOP=TRUE)
                           !
                           IF (EXT == 'SFAC') THEN
                               IF(IU==Z) THEN
                                             CALL SFAC_READ_AND_RETURN(SF, LN(N:), IN, IOUT, EX1_WORD=EX1_WORD, EX1_DIM=EX1_DIM, EX2_WORD=EX2_WORD, EX2_DIM=EX2_DIM, EX3_WORD=EX3_WORD, EX3_DIM=EX3_DIM, SCRATCH=SCRATCH)
                                             CALL READ_TO_DATA(LN,INFILE,IOUT)
                               ELSE
                                             CALL SFAC_READ_AND_RETURN(SF, LN(N:), IU, IOUT, EX1_WORD=EX1_WORD, EX1_DIM=EX1_DIM, EX2_WORD=EX2_WORD, EX2_DIM=EX2_DIM, EX3_WORD=EX3_WORD, EX3_DIM=EX3_DIM)
                                             CALL READ_TO_DATA(LN,IU,IOUT)
                               END IF
                               LLOC = ONE
                           ELSEIF(EXT==BLNK) THEN  ! READ NEXT LINE IF BLANK
                               IF(IU==Z) THEN
                                             CALL READ_TO_DATA(LN,INFILE,IOUT)
                                             IF(LN==BLNK) CALL STOP_ERROR(INFILE=IN,OUTPUT=IOUT,MSG = 'ULOAD FAILED TO LOAD INPUT LINE.'//NL//'THIS MOST LIKELY OCCURED BECAUSE THE END OF FILE WAS REACHED WHILE LOADING INPUT.'//BLN//'POSSIBLE CAUSES ARE NOT ENOUGH INPUT LINES, AN EMPTY FILE,'//NL//'THE INPUT LINE IS EMPTY WHEN A NUMBER WAS EXPECTED,'//NL//'OR YOU SPECIFIED TRANSIENT WHEN YOU MEANT STATIC FOR A LIST-ARRAY INPUT.', MSG2=MSG)
                              ELSE
                                             CALL READ_TO_DATA(LN,IU,IOUT)
                                             IF(LN==BLNK) CALL STOP_ERROR(INFILE=IU,OUTPUT=IOUT,MSG = 'ULOAD FAILED TO LOAD INPUT LINE.'//NL//'THIS MOST LIKELY OCCURED BECAUSE THE END OF FILE WAS REACHED WHILE LOADING INPUT.'//BLN//'POSSIBLE CAUSES ARE NOT ENOUGH INPUT LINES, AN EMPTY FILE,'//NL//'THE INPUT LINE IS EMPTY WHEN A NUMBER WAS EXPECTED,'//NL//'OR YOU SPECIFIED TRANSIENT WHEN YOU MEANT STATIC FOR A LIST-ARRAY INPUT.', MSG2=MSG)
                               END IF
                               LLOC = ONE
                               NO_MAIN_KEY=TRUE
                           ELSE
                               LLOC = N
                               EXIT
                           END IF
                      END DO
    ELSE
                      EXT='BINARY'
    END IF
    !
    SELECT CASE (EXT)
    CASE( 'SKIP', 'NULL', 'NUL' )     !-----------------------------------------------------------------------------------------------------------------------
                       SELECT TYPE (VAR)
                       TYPE IS (REAL(REAL64));     VAR = 0.0_REAL64  ! DOUBLE PRECISION
                       TYPE IS (REAL(REAL32));     VAR = 0.0_REAL32  ! SINGLE PRECISION
                       TYPE IS (INTEGER);          VAR = 0
                       TYPE IS (CHARACTER(*));     VAR = BLNK
                       TYPE IS (CHARACTER_TYPE);   VAR = BLNK
                       TYPE IS (GENERIC_INPUT_FILE)
                                                   CALL VAR%CLOSE() ! CALL STOP_ERROR( LINE=LN, INFILE=ERROR_IU, OUTPUT=IOUT, MSG= 'ULOAD ERROR: THIS INPUT READ UTILITY DOES NOT ALLOW FOR THE KEYWORD "SKIP". PLEASE USE A KEYWORD TO INDICATE WHERE DATA IS LOCATED (e.g INTERNAL|EXTERNAL|OPEN/CLOSE)', MSG2=MSG)
                                                   VAR%NULL_FILE = TRUE
                       TYPE IS (TIME_SERIES_FILE);  CALL VAR%INIT('SKIP')
                       TYPE IS (LOOKUP_TABLE_TYPE); CALL VAR%DESTROY()
                       TYPE IS (DATE_OPERATOR);     CALL VAR%DESTROY()
                       TYPE IS (IXJ_STRUCTURE);     CALL VAR%LOAD(Z)
                       TYPE IS (REAL(REAL128));     VAR = 0.0_REAL128 ! QUAD PRECISION
                       END SELECT
                       LLOC = ONE
                       LN = "NULL"//BLNK//LN
                       RETURN
    CASE( 'CONSTANT' ) !-----------------------------------------------------------------------------------------------------------------------
                       N = LLOC
                       CALL PARSE_WORD(LN,LLOC,ISTART,ISTOP)
                       CALL PARSE_WORD(LN,LLOC,ISTART,ISTOP,COM_STOP=TRUE)
                       !
                       SELECT TYPE (VAR)
                       TYPE IS (DATE_OPERATOR)
                                                   CALL GET_DATE(LN,LLOC,ISTART,ISTOP,IOUT,IN,VAR,NO_PARSE_WORD=TRUE,HAS_ERROR=NEG_LLOC,NO_DATE=TRUE)
                                                   !
                                                   IF(NEG_LLOC) THEN
                                                       IERR = 123
                                                       NEG_LLOC = TRUE
                                                   ELSE
                                                       IERR = Z
                                                   END IF
                       TYPE IS (GENERIC_INPUT_FILE)
                                                   CALL VAR%OPEN(LN,N, IOUT, IN, REQKEY=TRUE, KEY = EXT)
                                                   !
                                                   LLOC = ONE
                                                   LN = LN(N:)  ! RETURN THE LINE FROM "CONSTANT" ONWARD
                                                   RETURN
                       !TYPE IS (IXJ_STRUCTURE);
                       !                             CALL VAR%LOAD(Z)
                       !                             IERR = ONE
                       !                             IF(ALLOW_ERROR) CALL STOP_ERROR( LINE=LN, INFILE=ERROR_IU, OUTPUT=IOUT, MSG= 'ULOAD ERROR: IXJ STYLE INPUT DOES NOT ALLOW FOR THE KEYWORD "CONSTANT". PLEASE USE A KEYWORD TO INDICATE WHERE DATA IS LOCATED (e.g INTERNAL|EXTERNAL|OPEN/CLOSE)', MSG2=MSG)
                       CLASS DEFAULT
                                                   READ(LN(ISTART:ISTOP), *, IOSTAT=IERR) CONST
                       END SELECT
                       !
                       IF(IERR /= Z .AND. ALLOW_ERROR)   CALL FILE_IO_ERROR( IERR, UNIT=ERROR_IU, LINE=LN, INFILE=IN, OUTPUT=IOUT, MSG= 'ULOAD FOUND KEYWORD CONSTANT, BUT FAILED TO READ THE CONSTANT VALUE AFTER THE KEYWORD.', MSG2=MSG)
                       IF(IERR /= Z .AND. CLEAR_IU)  IU = Z
                       !
                       SELECT TYPE (VAR)
                       TYPE IS (REAL(REAL64));     VAR = CONST                ! DOUBLE PRECISION
                       TYPE IS (REAL(REAL32));     VAR = REAL(CONST,REAL32)   ! SINGLE PRECISION
                       TYPE IS (INTEGER)
                                                   READ(LN(ISTART:ISTOP), *, IOSTAT=IERR) I
                                                   VAR = I
                       TYPE IS (CHARACTER(*))
                                                   IF(READ_FULL_LINE) ISTOP = COMMENT_INDEX(LN)   !GET INDEX WHERE # IS OR LEN_TRIM
                                                   VAR = LN(ISTART:ISTOP)
                                                   IF(LINELEN<LEN_TRIM(LN(ISTART:ISTOP))) NEG_LLOC = TRUE
                       TYPE IS (CHARACTER_TYPE)
                                                   ISTOP = COMMENT_INDEX(LN)   !GET INDEX WHERE # IS OR LEN_TRIM
                                                   N = LEN_TRIM(LN(ISTART:ISTOP))
                                                   VAR = LN(ISTART:N)
                                                   !
                       TYPE IS (TIME_SERIES_FILE); CALL VAR%INIT(CONST)
                                                   !
                       TYPE IS (LOOKUP_TABLE_TYPE);CALL VAR%LOAD(CONST)
                                                   !
                       TYPE IS (IXJ_STRUCTURE);    CALL VAR%LOAD(Z, CONST=CONST)
                                                   !
                       TYPE IS (REAL(REAL128));    VAR = REAL(CONST,REAL128) ! QUAD PRECISION

                       END SELECT
                       !
                       CALL CHECK_FOR_POST_KEY(LLOC,LN,IN,IOUT,ISTART,ISTOP,SFAC_FILE) !ISTART, ISTOP ARE PLACE HOLDERS FOR BUF AND ISPLIT
                       !
                       IF(SFAC_FILE /= UNO) CALL SF%SET_ALL(SFAC_FILE)
                       !
                       IF(PRESENT(SFAC)) THEN
                                             SFAC = SF
                       ELSE
                                             SELECT TYPE (VAR)
                                             TYPE IS (REAL(REAL64));    CALL SF%APPLY(VAR)   ! DOUBLE PRECISION
                                             TYPE IS (REAL(REAL32));    CALL SF%APPLY(VAR)   ! SINGLE PRECISION
                                             TYPE IS (TIME_SERIES_FILE)
                                                                        IF(SF%HAS_ALL) VAR%FL%SCALE = VAR%FL%SCALE*SF%ALL
                                             TYPE IS (LOOKUP_TABLE_TYPE)
                                                                        IF(SF%HAS_ALL) VAR%Y = VAR%Y*SF%ALL
                                             TYPE IS (REAL(REAL128));   CALL SF%APPLY(VAR) ! QUAD PRECISION
                                             END SELECT
                       END IF
                       !
                       LLOC = ONE
                       IF(NEG_LLOC) LLOC = NEG
                       LN = EXT//BLNK//LN
                       RETURN
    CASE( 'REPEAT' )   !-----------------------------------------------------------------------------------------------------------------------
                       !
                       IF(PRESENT(NO_REPEAT)) THEN
                               IF(NO_REPEAT) CALL STOP_ERROR( LINE=LN, INFILE=ERROR_IU, OUTPUT=IOUT, MSG= 'ULOAD FOUND KEYWORD "REPEAT" BUT THIS PARTICULAR FEATURE DOES NOT SUPPORT IT.'//NL//'(NOTE APPLYING A SCALE FACTOR WITH SFAC, DOES NOT SUPPORT THIS.)'//NL//'PLEAUSE CHANGE INPUT TO USE OPEN/CLOSE TO RELOAD THE FILE INSTEAD OF REPEATING IT.', MSG2=MSG)
                       END IF
                       !
                       CALL PARSE_WORD(LN,LLOC,ISTART,ISTOP)
                       CALL CHECK_FOR_POST_KEY(LLOC,LN,IN,IOUT,ISTART,ISTOP,SFAC_FILE) !ISTART, ISTOP ARE PLACE HOLDERS FOR BUF AND ISPLIT
                       !
                       IF(SFAC_FILE /= UNO) CALL SF%SET_ALL(SFAC_FILE)
                       !
                       IF(PRESENT(SFAC)) THEN
                                             SFAC = SF
                       ELSE
                                             SELECT TYPE (VAR)
                                             TYPE IS (REAL(REAL64));    CALL SF%APPLY(VAR)   ! DOUBLE PRECISION
                                             TYPE IS (REAL(REAL32));    CALL SF%APPLY(VAR)   ! SINGLE PRECISION
                                             TYPE IS (TIME_SERIES_FILE)
                                                                        IF(SF%HAS_ALL) VAR%FL%SCALE = SF%ALL
                                             TYPE IS (GENERIC_INPUT_FILE)
                                                                        IF(SF%HAS_ALL) VAR%SCALE    = SF%ALL
                                             TYPE IS (LOOKUP_TABLE_TYPE)
                                                                        IF(SF%HAS_ALL) THEN
                                                                                      IF(SF%ALL /= UNO)CALL STOP_ERROR( LINE=LN, INFILE=ERROR_IU, OUTPUT=IOUT, MSG= 'LOOKUP TABLE LOAD ERROR - ULOAD FOUND KEYWORD "REPEAT" WITH A SCALE FACTOR THAT IS NOT EQUAL TO 1. LOOKUP TABLES IMMEDIATELY APPLY SCALE FACTORS SO THERE IS NO WAY TO KEEP THEM SEPARATE. EITHER RE-SPECIFY LOOKUP TABLE OR REMOVE SFAC OR SCALE.', MSG2=MSG)
                                                                        END IF
                                             TYPE IS (REAL(REAL128));   CALL SF%APPLY(VAR)   ! QUAD PRECISION
                                             END SELECT
                       END IF
                       !
                       LLOC = ONE
                       LN = EXT//BLNK//LN
                       RETURN
    CASE( 'RELOAD' ) !-----------------------------------------------------------------------------------------------------------------------
                       !
                       IF(IU==Z) THEN
                            IF(PRESENT(OLD_IU))  IU = OLD_IU
                            IF(ALLOW_ERROR .AND. (IU==Z .OR. .NOT. PRESENT(OLD_IU))) THEN
                                CALL STOP_ERROR( LINE=LN, INFILE=ERROR_IU, OUTPUT=IOUT, MSG= 'ULOAD FOUND KEYWORD "RELOAD" BUT THERE WAS NOT A PREVIOUSLY OPENED FILE TO USE OR THIS INPUT FEATURE DOES NOT SUPPORT THE "RELOAD" KEYWORD.'//NL//'PLEASE MAKE SURE THAT THIS ULOAD WAS PRECEDED BY AN EXTERNAL UNIT, DATAUNIT UNIT, OR DATAFILE FILE'//NL//'SO THT THE FILE CAN BE CONTINUED TO LOAD NEXT INPUT OR CHANGE KEYWORD TO ONE THAT OPENS A FILE OR POINTS TO AN EXISTING OPENED FILE.'//NL//'(e.g. THIS KEYWORD SHOULD ONLY APPEAR IN A TFR AFTER AN EXTERNAL, DATAUNIT, or DATAFILE KEYWORDS TO REUSE THEIR UNIT NUMBERS AND CONTINUE LOADING FROM THEIR FILES)', MSG2=MSG)
                            END IF
                            !
                            IF(IU /= Z) THEN
                                             CALL CHECK_FOR_POST_KEY(LLOC,LN,IN,IOUT,ISTART,ISTOP,SFAC_FILE) !ISTART, ISTOP ARE PLACE HOLDERS FOR BUF AND ISPLIT
                                             !
                                             INQUIRE(IU,FORM=FORM_CHK, OPENED=KEEP_IU)
                                             IF(.NOT. KEEP_IU) CALL STOP_ERROR(LINE=LN, INFILE=IN, OUTPUT=IOUT, MSG= 'ULOAD ERROR: RECIEVED UNIT NUMBER "'//NUM2STR(IU)//'" WHICH IS NOT ASSOCIATED WITH A FILE. THIS COULD BE THE RESULT OF USING "EXTERNAL" OR "DATAUNIT" WITH A UNIT THAT IS NOT OPENED BY THE NAME FILE.', MSG2=MSG)
                                             BIN = FORM_CHK /= 'FORMATTED'
                                             !
                                             IF(BIN) THEN
                                                 REWIND(IU) !MOVE BACK TO TOP OF FILE
                                             ELSE
                                                 CALL UTF8_BOM_OFFSET_REWIND(IU)  !Check in case file has a UTF8 BOM header, rewind to correct location
                                             END IF
                                             !
                                             ERROR_IU = IU
                                             KEEP_IU  = TRUE
                                             LLOC     = ONE
                                             IF(.NOT. ALLOW_ERROR) CLEAR_IU = TRUE
                                             IF(.NOT. BIN) CALL READ_TO_DATA(LN,IU,IOUT)  !UNIT IS BEING PASSED IN SO READ IN LINE FROM IT
                            END IF
                       ELSE
                            CALL STOP_ERROR( LINE=LN, INFILE=ERROR_IU, OUTPUT=IOUT, MSG= 'ULOAD FOUND KEYWORD "RELOAD" BUT THIS INPUT OPTION EITHER DOES NOT SUPPORT RELOAD OR THERE IS A CODE ERROR. PLEASE CHANGE RELOAD TO SOMETHING LIKE "EXTERNAL  55  REWIND"', MSG2=MSG)
                       END IF
    CASE( 'LOAD_NEXT' )!-----------------------------------------------------------------------------------------------------------------------
                       !
                       IF(IU==Z) THEN
                            IF(PRESENT(OLD_IU))  IU = OLD_IU
                            IF(ALLOW_ERROR .AND. (IU==Z .OR. .NOT. PRESENT(OLD_IU))) THEN
                                CALL STOP_ERROR( LINE=LN, INFILE=ERROR_IU, OUTPUT=IOUT, MSG= 'ULOAD FOUND KEYWORD "LOAD_NEXT" BUT THERE WAS NOT A PREVIOUSLY OPENED FILE TO USE OR THIS INPUT FEATURE DOES NOT SUPPORT THE "LOAD_NEXT" KEYWORD (viz. SFAC DOES NOT SUPPORT IT).'//NL//'PLEASE MAKE SURE THAT THIS ULOAD WAS PRECEDED BY AN EXTERNAL UNIT, DATAUNIT UNIT, OR DATAFILE FILE'//NL//'SO THT THE FILE CAN BE CONTINUED TO LOAD NEXT INPUT'//NL//'OR CHANGE KEYWORD TO ONE THAT OPENS A FILE OR POINTS TO AN EXISTING OPENED FILE.'//NL//'(e.g. THIS KEYWORD SHOULD ONLY APPEAR IN A TFR AFTER AN EXTERNAL, DATAUNIT, or DATAFILE KEYWORDS TO REUSE THEIR UNIT NUMBERS AND CONTINUE LOADING FROM THEIR FILES)', MSG2=MSG)
                            END IF
                            !
                            IF(IU /= Z) THEN
                                             CALL CHECK_FOR_POST_KEY(LLOC,LN,IN,IOUT,ISTART,ISTOP,SFAC_FILE) !ISTART, ISTOP ARE PLACE HOLDERS FOR BUF AND ISPLIT
                                             !
                                             INQUIRE(IU,FORM=FORM_CHK, OPENED=KEEP_IU)
                                             IF(.NOT. KEEP_IU) CALL STOP_ERROR(LINE=LN, INFILE=IN, OUTPUT=IOUT, MSG= 'ULOAD ERROR: RECIEVED UNIT NUMBER "'//NUM2STR(IU)//'" WHICH IS NOT ASSOCIATED WITH A FILE. THIS COULD BE THE RESULT OF USING "EXTERNAL" OR "DATAUNIT" WITH A UNIT THAT IS NOT OPENED BY THE NAME FILE.', MSG2=MSG)
                                             BIN = FORM_CHK /= 'FORMATTED'
                                             !
                                             ERROR_IU = IU
                                             KEEP_IU  = TRUE
                                             LLOC     = ONE
                                             IF(.NOT. ALLOW_ERROR) CLEAR_IU = TRUE
                                             IF(.NOT. BIN) CALL READ_TO_DATA(LN,IU,IOUT)  !UNIT IS BEING PASSED IN SO READ IN LINE FROM IT
                            END IF
                       ELSE
                            CALL STOP_ERROR( LINE=LN, INFILE=ERROR_IU, OUTPUT=IOUT, MSG= 'ULOAD FOUND KEYWORD "LOAD_NEXT" BUT THIS INPUT OPTION EITHER DOES NOT SUPPORT RELOAD OR THERE IS A CODE ERROR. PLEASE CHANGE LOAD_NEXT TO SOMETHING LIKE "DATAFILE MyFile.txt"', MSG2=MSG)
                       END IF
    END SELECT
    !
    ! CHECK IF ONLY LOADING FILE AND NOT DATA
    SELECT TYPE (VAR)
    TYPE IS (GENERIC_INPUT_FILE)
              IF(IU==Z) THEN
                    N = LLOC
                    !
                    CALL VAR%OPEN(LN, LLOC, IOUT, IN, REQKEY=TRUE, NOSTOP=TRUE, KEY_FAIL_STOP=TRUE, INTERNAL_IU=INFILE, KEY_FOUND=NEG_LLOC, BINARY=BIN, KEY = EXT, MSG=MSG)
                    !
                    IF(VAR%IS_INTERNAL .AND. NO_INTERN) CALL STOP_ERROR( LINE=LN, INFILE=ERROR_IU, OUTPUT=IOUT, MSG= 'ULOAD ERROR: THIS SPECIFIC INPUT LINE DOES NOT ALLOW FOR THE "INTERNAL" KEYWORD OR IMPLIED INTERNAL. PLEASE MOVE INTERNAL INPUT TO SEPARATE FILE AND CHANGE KEYWORD TO "OPEN/CLOSE" OR "EXTERNAL".', MSG2=MSG)
                    !
                    NEG_LLOC = .not. NEG_LLOC .or. VAR%ERROR    ! Set to true if no key is found
              ELSE
                  IF(.NOT. BIN) BACKSPACE(IU)  ! Read line to check for SFAC, but unit was passed so that will be used to read data of GENERIC_INPUT_FILE type
                  VAR%IU = IU
                  VAR%OPENCLOSE = FALSE
                  VAR%ERROR     = FALSE
              END IF
              !BIN = VAR%BINARY
              IF(.NOT. VAR%ERROR) THEN
                                        IU = VAR%IU
                                        IF(IU /= Z) ERROR_IU = IU
                                        !
                                        IF(.NOT. VAR%OPENCLOSE .AND. .NOT. VAR%IS_INTERNAL) KEEP_IU = TRUE  !FOUND EXTERNAL KEYWORD, SO PASS BACK WHAT WAS LOADED
                                        !
                                        IF(VAR%IS_INTERNAL) THEN
                                            IU = INFILE 
                                            VAR%IU = INFILE
                                            VAR%OPENCLOSE=FALSE
                                            KEEP_IU = TRUE
                                        END IF
              ELSEIF(ALLOW_ERROR) THEN
                                        CALL STOP_ERROR( LINE=LN, INFILE=IU, OUTPUT=IOUT, MSG= 'ULOAD ERROR: THIS PARTICULAR INPUT DOES NOT ALLOW AN IMPLIED INTERNAL. PLEASE USE A KEYWORD TO INDICATE WHERE DATA IS LOCATED (e.g INTERNAL|EXTERNAL|OPEN/CLOSE)', MSG2=MSG)
              END IF
              !
              IF(.NOT. KEEP_IU) IU = Z
              !
              IF(PRESENT(SFAC)) THEN
                  IF(VAR%SCALE /= UNO) CALL SF%SET_ALL(VAR%SCALE)
                  SFAC = SF
                  VAR%SCALE = UNO
              END IF
              !
              !RETURN -- FILE OBJECT LOADED SO DO NOT LOAD INFORMATION TO ALLOW CUSTOM LOADER
              !
    CLASS DEFAULT ! USE NORMAL ULOAD FOR SINGLE SCALAR OF INT, CHAR, DBLE, SNGL
                !
                IF(IU==Z) THEN
                      N = LLOC
                      ALLOCATE(FL)
                      !
                      CALL FL%OPEN(LN, LLOC, IOUT, IN, REQKEY=TRUE, NOSTOP=TRUE, KEY_FAIL_STOP=TRUE, INTERNAL_IU=INFILE, BINARY=BIN, MSG=MSG)
                      !
                      IF(FL%IS_INTERNAL .AND. NO_INTERN) CALL STOP_ERROR( LINE=LN, INFILE=ERROR_IU, OUTPUT=IOUT, MSG= 'ULOAD ERROR: THIS SPECIFIC INPUT LINE DOES NOT ALLOW FOR THE "INTERNAL" KEYWORD. PLEASE MOVE INTERNAL INPUT TO SEPARATE FILE AND CHANGE KEYWORD TO "OPEN/CLOSE" OR "EXTERNAL" OR INSTEAD USE AN IMPLIED INTERNAL BY HAVING THE INPUT DATA ITEM ON THE SAME LINE.', MSG2=MSG)
                      !
                      SFAC_FILE = FL%SCALE
                      FL%SCALE = UNO
                      !
                      BIN = FL%BINARY
                      IF(FL%ERROR) THEN ! NO KEYWORD FOUND SO DATA IS WITHIN LINE
                          LLOC = N
                          IU   = Z
                      ELSE
                          LLOC = ONE
                          IU   = FL%IU
                          !
                          IF(.NOT. FL%OPENCLOSE .AND. .NOT. FL%IS_INTERNAL) KEEP_IU = TRUE  !FOUND EXTERNAL KEYWORD, SO PASS BACK WHAT WAS LOADED
                          IF(IU /= Z) ERROR_IU = IU
                          IF(.NOT. BIN) CALL READ_TO_DATA(LN,IU,IOUT)
                      END IF
                END IF
                !
                IF(.NOT. BIN .AND. IU /= Z) THEN
                                            DO !CHECK FOR KEYWORD OF SFAC WITHIN LIST
                                                  N = LLOC
                                                  CALL GET_WORD(LN, LLOC, ISTART, ISTOP, EXT, COM_STOP=TRUE)
                                                  IF (EXT == 'SFAC') THEN
                                                      CALL SFAC_READ_AND_RETURN(SF, LN(N:), IU, IOUT, EX1_WORD=EX1_WORD, EX1_DIM=EX1_DIM, EX2_WORD=EX2_WORD, EX2_DIM=EX2_DIM, EX3_WORD=EX3_WORD, EX3_DIM=EX3_DIM)
                                                      LLOC = ONE
                                                      CALL READ_TO_DATA(LN,IU,IOUT)
                                                  ELSE
                                                      LLOC = N
                                                      EXIT
                                                  END IF
                                            END DO
                END IF
                !
                IF(BIN) THEN
                            !
                            IF(IU==INFILE)                             CALL STOP_ERROR( LINE=LN, INFILE=ERROR_IU, OUTPUT=IOUT, MSG= 'ULOAD ERROR: "INTERNAL" AND "BINARY" KEYWORD FOUND, BUT THIS IS NOT ALLOWED!', MSG2=MSG)
                            IF(PRESENT(NO_BINARY)) THEN; IF(NO_BINARY) CALL STOP_ERROR( LINE=LN, INFILE=ERROR_IU, OUTPUT=IOUT, MSG= 'ULOAD ERROR: "BINARY" KEYWORD FOUND, BUT THIS INPUT PROPERTY DOES NOT SUPPORT BINARY FILE INPUT. PLEASE USE ASCII/TEXT INPUT INSTEAD AND REMOVE "BINARY" KEYWORD.', MSG2=MSG)
                            END IF
                            !
                            SELECT TYPE (VAR)
                            TYPE IS (REAL(REAL64));     READ(IU, IOSTAT=IERR) VAR    ! DOUBLE PRECISION
                            TYPE IS (REAL(REAL32));     READ(IU, IOSTAT=IERR) VAR    ! SINGLE PRECISION
                            TYPE IS (INTEGER);          READ(IU, IOSTAT=IERR) VAR
                            TYPE IS (CHARACTER(*));     READ(IU, IOSTAT=IERR) VAR
                            TYPE IS (DATE_OPERATOR);    READ(IU, IOSTAT=IERR) VAR
                            TYPE IS (TIME_SERIES_FILE); CALL STOP_ERROR( LINE=LN, INFILE=ERROR_IU, OUTPUT=IOUT, MSG= 'ULOAD ERROR: "BINARY" KEYWORD FOUND, BUT THIS INPUT PROPERTY DOES NOT SUPPORT BINARY FILE INPUT. PLEASE USE ASCII/TEXT INPUT INSTEAD AND REMOVE "BINARY" KEYWORD.', MSG2=MSG)
                            TYPE IS (LOOKUP_TABLE_TYPE);CALL STOP_ERROR( LINE=LN, INFILE=ERROR_IU, OUTPUT=IOUT, MSG= 'ULOAD ERROR: "BINARY" KEYWORD FOUND, BUT THIS INPUT PROPERTY DOES NOT SUPPORT BINARY FILE INPUT. PLEASE USE ASCII/TEXT INPUT INSTEAD AND REMOVE "BINARY" KEYWORD.', MSG2=MSG)
                            TYPE IS (CHARACTER_TYPE);   CALL STOP_ERROR( LINE=LN, INFILE=ERROR_IU, OUTPUT=IOUT, MSG= 'ULOAD ERROR: "BINARY" KEYWORD FOUND, BUT THIS INPUT PROPERTY DOES NOT SUPPORT BINARY FILE INPUT. PLEASE USE ASCII/TEXT INPUT INSTEAD AND REMOVE "BINARY" KEYWORD.', MSG2=MSG)
                            TYPE IS (IXJ_STRUCTURE);    CALL STOP_ERROR( LINE=LN, INFILE=ERROR_IU, OUTPUT=IOUT, MSG= 'ULOAD ERROR: "BINARY" KEYWORD FOUND, BUT IXJ STYLE INPUT DOES NOT SUPPORT BINARY FILE INPUT. PLEASE USE ASCII/TEXT INPUT INSTEAD AND REMOVE "BINARY" KEYWORD.', MSG2=MSG)
                            TYPE IS (REAL(REAL128));    READ(IU, IOSTAT=IERR) VAR    ! QUAD PRECISION
                            END SELECT
                            !
                            IF(IERR /= Z .AND. ALLOW_ERROR)   CALL FILE_IO_ERROR( IERR, UNIT=ERROR_IU, LINE=LN, INFILE=IN, OUTPUT=IOUT, MSG= 'ATTEMPT MADE TO READ LIST FROM ULOAD IN A BINARY FILE, BUT IT FAILED TO LOAD INPUT DATA.', MSG2=MSG)
                            IF(IERR /= Z .AND. CLEAR_IU)  IU = Z
                ELSE
                    IF(IU /= Z) THEN  !KEYWORD FOUND SO LOAD IN NEXT LINE
                                      IF(READ_ID) THEN
                                            CALL PARSE_WORD(LN,LLOC,ISTART,ISTOP,COM_STOP=TRUE)
                                            IF(PRESENT(ID)) THEN
                                                           READ(LN(ISTART:ISTOP), *, IOSTAT=IERR)      ID
                                                                                         IF(IERR /= Z) ID = Z
                                            END IF
                                      END IF
                                      CALL PARSE_WORD(LN,LLOC,ISTART,ISTOP,COM_STOP=TRUE)
                    ELSE
                                      CALL PARSE_WORD(LN,LLOC,ISTART,ISTOP,COM_STOP=TRUE)
                    END IF
                    !
                    IERR = Z
                    SELECT TYPE (VAR)
                    TYPE IS (REAL(REAL64));     READ(LN(ISTART:ISTOP), *, IOSTAT=IERR) VAR   ! DOUBLE PRECISION
                    TYPE IS (REAL(REAL32));     READ(LN(ISTART:ISTOP), *, IOSTAT=IERR) VAR   ! SINGLE PRECISION
                    TYPE IS (INTEGER);          READ(LN(ISTART:ISTOP), *, IOSTAT=IERR) VAR
                    TYPE IS (CHARACTER(*));
                                                IF(READ_FULL_LINE) ISTOP = COMMENT_INDEX(LN)   !GET INDEX WHERE # IS OR LEN_TRIM
                                                VAR = LN(ISTART:ISTOP)
                                                IF(LINELEN<LEN_TRIM(LN(ISTART:ISTOP))) NEG_LLOC = TRUE
                    TYPE IS (CHARACTER_TYPE)
                                                ISTOP = COMMENT_INDEX(LN)   !GET INDEX WHERE # IS OR LEN_TRIM
                                                N = LEN_TRIM(LN(ISTART:ISTOP))
                                                VAR = LN(ISTART:N)
                    TYPE IS (DATE_OPERATOR)
                                                CALL GET_DATE(LN,LLOC,ISTART,ISTOP,IOUT,IU,VAR,NO_PARSE_WORD=TRUE,HAS_ERROR=NEG_LLOC,NO_DATE=TRUE)
                                                !CALL VAR%INIT(LN(ISTART:ISTOP))
                                                IF(NEG_LLOC) IERR = 123
                    TYPE IS (TIME_SERIES_FILE)
                                                CALL VAR%INIT(LN, LLOC, IOUT, IN, FALSE, DEFAULT_OPT=TEXT)
                    TYPE IS (LOOKUP_TABLE_TYPE)
                                                CALL VAR%LOAD(LLOC,ISTART,ISTOP,LN,IU,IOUT,FALSE)
                                                !
                    TYPE IS (IXJ_STRUCTURE);
                                                IF(IU /= Z) THEN
                                                    CALL VAR%LOAD(IU, ERROR_IU, IOUT, TRUE, FIRST_LINE=LN)
                                                    !
                                                    IF(VAR%ERRMSG /= BLNK) IERR = ONE
                                                ELSE
                                                    CALL STOP_ERROR( LINE=LN, INFILE=ERROR_IU, OUTPUT=IOUT, MSG= 'ULOAD ERROR: IXJ STYLE INPUT DOES NOT ALLOW FOR "IMPLIED INTERNAL" LOAD. PLEASE USE A KEYWORD TO INDICATE WHERE DATA IS LOCATED (e.g INTERNAL|EXTERNAL|OPEN/CLOSE)', MSG2=MSG)
                                                END IF
                                                !
                    TYPE IS (REAL(REAL128));    READ(LN(ISTART:ISTOP), *, IOSTAT=IERR) VAR   ! QUAD PRECISION
                    END SELECT
                    !
                    IF(LN(ISTART:ISTOP) == '') IERR = -1234
                    !
                    IF(IERR /= Z .AND. ALLOW_ERROR ) THEN
                           IF(NO_MAIN_KEY) THEN
                                CALL FILE_IO_ERROR( IERR, UNIT=ERROR_IU, LINE=LN, INFILE=IN, OUTPUT=IOUT, MSG= 'ATTEMPT MADE TO READ LIST FROM ULOAD ON LINE, BUT IT FAILED TO LOAD THE FOLLOWING VALUE "'//LN(ISTART:ISTOP)//'"'//BLN//'NOTE THAT NO KEYWORD WAS FOUND ON THE PREVIOUS LINE, SO CODE AUTOMOVED DOWNWARD TO NEXT LINE.'//NL//'YOU MAYBE MISSING A KEYWORD (viz. INTERNAL, EXTERNAL, OPEN/CLOSE, DATEFILE, DATAUNIT).', MSG2=MSG)
                           ELSE
                                SELECT TYPE (VAR)
                                TYPE IS (DATE_OPERATOR);           CALL FILE_IO_ERROR( IERR, UNIT=ERROR_IU, LINE=LN, INFILE=IN, OUTPUT=IOUT, MSG= 'ATTEMPT MADE TO READ A DATE WITH ULOAD ON LINE, BUT IT FAILED TO CONVERT THE FOLLOWING TO A CALENDAR DATE "'//LN(ISTART:ISTOP)//'"'//BLN//'THE ACCEPTED DATE FORMATS ARE "mm/dd/YYYY" OR "YYYY-mm-dd" IF YOU WANT TO ADD A 24-HOUR TIME TO IT YOU MUST ADD TO THE DATE "Thh:mm:ss" (e.g. "YYYY-mm-ddThh:mm:ss")', MSG2=MSG)
                                TYPE IS (GENERIC_INPUT_FILE);      CALL FILE_IO_ERROR( IERR, UNIT=ERROR_IU, LINE=LN, INFILE=IN, OUTPUT=IOUT, MSG= 'ATTEMPT MADE TO OPEN FILE WITH ULOAD, BUT IT FAILED TO EITHER OPEN, FIND THE FILE, OR IS NOT SET UP CORRECTLY. THE FOLLOWING IS THE FILE THAT WAS ATTEMPTED TO BE OPENED "'//LN(ISTART:ISTOP)//'".', MSG2=MSG)
                                TYPE IS (IXJ_STRUCTURE);           CALL STOP_ERROR( LINE=LN, INFILE=ERROR_IU, OUTPUT=IOUT, MSG= 'ULOAD ERROR OCCURRED WILE LOADING IXJ INPUT. THE FOLLOWING IS AN ERROR MESSAGE FROM THE IXJ LOADER:'//BLN//VAR%ERRMSG, MSG2=MSG)
                                CLASS DEFAULT
                                                                   CALL STOP_ERROR( LINE=LN, INFILE=ERROR_IU, OUTPUT=IOUT, MSG= 'ULOAD ATTEMPTED TO READ A SINGLE VALUE WITHIN THE FOLLOWING "'//LN(ISTART:ISTOP)//'", BUT IT FAILED TO LOAD INTO OneWater. PLEASE CHECK INPUT TO MAKE SURE FORMAT IS WHAT IS EXPECTED.', MSG2=MSG)
                                END SELECT
                           END IF
                    ELSEIF(IERR /= Z .AND. CLEAR_IU) THEN
                           IU = Z
                    END IF
                    !
                END IF
                !
                IF(SFAC_FILE /= UNO) CALL SF%SET_ALL(SFAC_FILE)
                !
                IF(PRESENT(SFAC)) THEN
                                       SFAC = SF
                ELSE
                                       SELECT TYPE (VAR)
                                       TYPE IS (REAL(REAL64));    CALL SF%APPLY(VAR)  ! DOUBLE PRECISION
                                       TYPE IS (REAL(REAL32));    CALL SF%APPLY(VAR)  ! SINGLE PRECISION
                                       TYPE IS (TIME_SERIES_FILE)
                                                                  IF(SF%HAS_ALL) VAR%FL%SCALE = VAR%FL%SCALE*SF%ALL
                                       TYPE IS (LOOKUP_TABLE_TYPE)
                                                                  IF(SF%HAS_ALL) VAR%Y = VAR%Y*SF%ALL
                                       TYPE IS (REAL(REAL128));   CALL SF%APPLY(VAR)   ! QUAD PRECISION
                                       END SELECT
                END IF
                !
                IF(.NOT. KEEP_IU) IU = Z
                !
                IF(ALLOCATED(FL))  DEALLOCATE(FL)
    END SELECT
    !
    IF(NEG_LLOC) LLOC = NEG  ! SET TO NEG IF CHARACTER VAR IS NOT LARGE ENOUGH OR IF GENERIC_INPUT IS MISSING KEYWORD
    !
    IF(PRESENT(OLD_IU))  OLD_IU = IU
    !
  END SUBROUTINE
  !
  RECURSIVE SUBROUTINE ULOAD_VECTOR(VAR, LLOC, LN, IOUT, IN, IU, NOID, BINARY, ID, ROW_WORD, NOSTOP, SFAC, EX1_WORD, EX1_DIM, EX2_WORD, EX2_DIM, EX3_WORD, EX3_DIM, SCRATCH, ENTIRE_LINE, NO_INTERNAL, NO_BINARY, NO_REPEAT, OLD_IU, TEXT, MSG)
    CLASS(*),         DIMENSION(:), CONTIGUOUS,      INTENT(INOUT):: VAR       !Variable to set by ULOAD
    INTEGER,                                         INTENT(INOUT):: LLOC      !SET TO -1 IF CHARACTER::VAR IS NOT LONG ENOUGH TO HOLD INPUT
    CHARACTER(*),                                    INTENT(INOUT):: LN        !Temp line to holds read input that is processed
    INTEGER,                                         INTENT(IN   ):: IN, IOUT  !File that input originated from; error output location
    INTEGER,                                         INTENT(INOUT):: IU        ! If IU=0 input is dermined from directives in IN or SCRATCH, IU /= 0 indicates input is on that file unit. Same as IN having a LN = "EXTERNAL IU"
    !
    LOGICAL,     OPTIONAL,                           INTENT(IN   ):: NOID, BINARY, NOSTOP
    INTEGER,     OPTIONAL, DIMENSION(:), ALLOCATABLE,INTENT(INOUT):: ID                   !Note that ID must be ALLOCATABLE -- This is done to check if it is read
    CHARACTER(*),OPTIONAL,                           INTENT(IN   ):: ROW_WORD
    TYPE(SFAC_DATA),OPTIONAL,                        INTENT(  OUT):: SFAC
    !
    CHARACTER(*), OPTIONAL,                          INTENT(IN   ):: EX1_WORD, EX2_WORD, EX3_WORD
    INTEGER,      OPTIONAL,                          INTENT(IN   ):: EX1_DIM,  EX2_DIM,  EX3_DIM
    INTEGER,      OPTIONAL,                          INTENT(IN   ):: SCRATCH
    LOGICAL,      OPTIONAL,                          INTENT(IN   ):: ENTIRE_LINE, NO_INTERNAL, NO_BINARY, NO_REPEAT
    INTEGER,      OPTIONAL,                          INTENT(INOUT):: OLD_IU
    CHARACTER(*), OPTIONAL,                          INTENT(IN   ):: TEXT
    CHARACTER(*), OPTIONAL,                          INTENT(IN   ):: MSG
    !
    TYPE(SFAC_DATA):: SF!1, SF2
    !
    INTEGER:: IERR, ISTART, ISTOP, I, N, DIM1, INFILE, ERROR_IU, LINELEN
    TYPE(GENERIC_INPUT_FILE), ALLOCATABLE:: FL
    LOGICAL:: KEEP_IU, READ_ID, BIN, ALLOW_ERROR, CLEAR_IU, READ_FULL_LINE, NO_INTERN, NEG_LLOC, NO_MAIN_KEY, IMPLIED_INTERNAL, SET_ID
    REAL(REAL64):: SFAC_FILE, CONST  !ENSURE IT IS ALWAYS DOUBLE PRECISION
    CHARACTER(12):: EXT, FORM_CHK    ! Note 12 is passed in at CALL UPPER(12, EXT)
    !
       DIM1 = UBOUND(VAR, 1)
    IF(DIM1 == Z) RETURN
    !
    !
    IERR = Z
    NEG_LLOC  = FALSE
    SFAC_FILE = UNO
    ERROR_IU = IN
    NO_MAIN_KEY = FALSE
    IMPLIED_INTERNAL = FALSE
    !
    SELECT TYPE (VAR)
    TYPE IS (CHARACTER(*)      ); LINELEN = LEN(VAR)
    TYPE IS (GENERIC_INPUT_FILE); VAR%SCALE = UNO
    END SELECT
    !
    IF(PRESENT(SCRATCH)) THEN
        IF(SCRATCH /= Z) THEN
                              INFILE = SCRATCH
        ELSE
                              INFILE = IN
        END IF
    ELSE
        INFILE = IN
    END IF
    !
    IF(PRESENT(NOID)) THEN
        READ_ID = .NOT. NOID
    ELSE
        READ_ID = TRUE
    END IF
    !
    IF(PRESENT(BINARY)) THEN
        BIN = BINARY
    ELSE
        BIN = FALSE
    END IF
    !
    IF(PRESENT(NOSTOP)) THEN
        ALLOW_ERROR = .NOT. NOSTOP
    ELSE
        ALLOW_ERROR = TRUE
    END IF
    !
    IF(PRESENT(ENTIRE_LINE)) THEN
        READ_FULL_LINE = ENTIRE_LINE
    ELSE
        READ_FULL_LINE = FALSE
    END IF
    !
    IF(PRESENT(NO_INTERNAL)) THEN
        NO_INTERN = NO_INTERNAL
    ELSE
        NO_INTERN = FALSE
    END IF
    !
    IF(PRESENT(ID)) THEN
           SET_ID = ALLOCATED(ID)
        IF(SET_ID) THEN 
                   CALL SET_SEQUENCE(SIZE(ID), ID)
                   SET_ID = READ_ID 
        END IF
    ELSE
        SET_ID = FALSE
    END IF
    !
    CLEAR_IU = FALSE
    IF(IU /= Z) THEN
                    INQUIRE(IU,FORM=FORM_CHK, OPENED=KEEP_IU)
                    IF(.NOT. KEEP_IU) CALL STOP_ERROR( LINE=LN, INFILE=IN, OUTPUT=IOUT, MSG= 'ULOAD ERROR: RECIEVED UNIT NUMBER "'//NUM2STR(IU)//'" WHICH IS NOT ASSOCIATED WITH A FILE. THIS COULD BE THE RESULT OF USING "EXTERNAL" OR "DATAUNIT" WITH A UNIT THAT IS NOT OPENED BY THE NAME FILE.', MSG2=MSG)
                    BIN = FORM_CHK /= 'FORMATTED'
                    !
                    ERROR_IU = IU
                    KEEP_IU  = TRUE
                    LLOC     = ONE
                    IF(.NOT. ALLOW_ERROR) CLEAR_IU = TRUE
                    IF(.NOT. BIN) CALL READ_TO_DATA(LN,IU,IOUT)  !UNIT IS BEING PASSED IN SO READ IN LINE FROM IT
    ELSE
                    KEEP_IU = FALSE
    END IF
    !
    CALL SF%INIT()
    IF(PRESENT(SFAC)) CALL SFAC%INIT()
    !
    IF(.NOT. BIN) THEN
                      DO !CHECK FOR KEYWORD OF SFAC BEFORE LIST
                           N = LLOC
                           CALL GET_WORD(LN, LLOC, ISTART, ISTOP, EXT, COM_STOP=TRUE)
                           !
                           IF (EXT == 'SFAC') THEN
                               IF(IU==Z) THEN
                                             CALL SFAC_READ_AND_RETURN(SF, LN(N:), IN, IOUT, ROW_WORD, DIM1, EX1_WORD=EX1_WORD, EX1_DIM=EX1_DIM, EX2_WORD=EX2_WORD, EX2_DIM=EX2_DIM, EX3_WORD=EX3_WORD, EX3_DIM=EX3_DIM, SCRATCH=SCRATCH)
                                             CALL READ_TO_DATA(LN,INFILE,IOUT)
                               ELSE
                                             CALL SFAC_READ_AND_RETURN(SF, LN(N:), IU, IOUT, ROW_WORD, DIM1, EX1_WORD=EX1_WORD, EX1_DIM=EX1_DIM, EX2_WORD=EX2_WORD, EX2_DIM=EX2_DIM, EX3_WORD=EX3_WORD, EX3_DIM=EX3_DIM)
                                             CALL READ_TO_DATA(LN,IU,IOUT)
                               END IF
                               LLOC = ONE
                           ELSEIF(EXT=='STATIC' .OR. EXT=='LIST') THEN; CYCLE ! FOUND LAI KEYWORDS, IGNORE THEM
                           !
                           ELSEIF(EXT==BLNK) THEN  !READ NEXT LINE IF BLANK
                               IF(IU==Z) THEN
                                             CALL READ_TO_DATA(LN,INFILE,IOUT)
                                             IF(LN==BLNK) CALL STOP_ERROR(INFILE=IN,OUTPUT=IOUT,MSG = 'ULOAD FAILED TO LOAD INPUT LINE.'//NL//'THIS MOST LIKELY OCCURED BECAUSE THE END OF FILE WAS REACHED WHILE LOADING INPUT.'//BLN//'POSSIBLE CAUSES ARE NOT ENOUGH INPUT LINES, AN EMPTY FILE,'//NL//'THE INPUT LINE IS EMPTY WHEN NUMBERS WERE EXPECTED,'//NL//'OR YOU SPECIFIED TRANSIENT WHEN YOU MEANT STATIC FOR A LIST-ARRAY INPUT.', MSG2=MSG)
                               ELSE
                                             CALL READ_TO_DATA(LN,IU,IOUT)
                                             IF(LN==BLNK) CALL STOP_ERROR(INFILE=IU,OUTPUT=IOUT,MSG = 'ULOAD FAILED TO LOAD INPUT LINE.'//NL//'THIS MOST LIKELY OCCURED BECAUSE THE END OF FILE WAS REACHED WHILE LOADING INPUT.'//BLN//'POSSIBLE CAUSES ARE NOT ENOUGH INPUT LINES, AN EMPTY FILE,'//NL//'THE INPUT LINE IS EMPTY WHEN NUMBERS WERE EXPECTED,'//NL//'OR YOU SPECIFIED TRANSIENT WHEN YOU MEANT STATIC FOR A LIST-ARRAY INPUT.', MSG2=MSG)
                               END IF
                               !
                               LLOC = ONE
                               NO_MAIN_KEY = TRUE
                           ELSE
                               LLOC = N
                               EXIT
                           END IF
                      END DO
    ELSE
                      EXT='BINARY'
    END IF
    !
    SELECT CASE (EXT)
    CASE( 'SKIP', 'NULL', 'NUL' )     !-----------------------------------------------------------------------------------------------------------------------
                       SELECT TYPE (VAR)
                       TYPE IS (REAL(REAL64));     VAR = 0.0_REAL64   ! DOUBLE PRECISION
                       TYPE IS (REAL(REAL32));     VAR = 0.0_REAL32   ! SINGLE PRECISION
                       TYPE IS (INTEGER);          VAR = 0
                       TYPE IS (CHARACTER(*));     VAR = BLNK
                       TYPE IS (CHARACTER_TYPE);   VAR = BLNK
                       TYPE IS (GENERIC_INPUT_FILE);
                                                    CALL VAR%CLOSE() ! CALL STOP_ERROR( LINE=LN, INFILE=ERROR_IU, OUTPUT=IOUT, MSG= 'ULOAD ERROR: THIS INPUT READ UTILITY DOES NOT ALLOW FOR THE KEYWORD "SKIP". PLEASE USE A KEYWORD TO INDICATE WHERE DATA IS LOCATED (e.g INTERNAL|EXTERNAL|OPEN/CLOSE)', MSG2=MSG)
                                                    VAR%NULL_FILE = TRUE
                       TYPE IS (TIME_SERIES_FILE);  CALL VAR%INIT('SKIP')
                       TYPE IS (LOOKUP_TABLE_TYPE); CALL VAR%DESTROY()
                       TYPE IS (DATE_OPERATOR);     CALL VAR%DESTROY()
                       !TYPE IS (DATE_OPERATOR)
                       !                          DO I=1, DIM1
                       !                              CALL VAR(I)%DESTROY()
                       !                          END DO
                       TYPE IS (REAL(REAL128));    VAR = 0.0_REAL128   ! QUAD PRECISION
                       END SELECT
                       LN = EXT//BLNK//LN
                       RETURN
    CASE( 'CONSTANT' ) !-----------------------------------------------------------------------------------------------------------------------
                       N = LLOC
                       CALL PARSE_WORD(LN,LLOC,ISTART,ISTOP)
                       CALL PARSE_WORD(LN,LLOC,ISTART,ISTOP,COM_STOP=TRUE)
                       !
                       SELECT TYPE (VAR)
                       TYPE IS (GENERIC_INPUT_FILE)
                                                   LLOC = N
                                                   !
                                                   CALL FL%OPEN(LN,LLOC,IOUT,IN,REQKEY=TRUE,KEY=EXT)
                                                   !
                                                   IF(IU==Z) THEN
                                                         VAR = FL
                                                         CALL FL%NOCLOSE()
                                                   ELSE
                                                       VAR%IU = IU
                                                       VAR%OPENCLOSE = FALSE
                                                       VAR%ERROR     = FALSE
                                                   END IF
                                                   LLOC = ONE
                                                   LN = LN(N:)  !RETURN THE LINE FROM "CONSTANT" ONWARD
                                                   RETURN
                       TYPE IS (DATE_OPERATOR)
                                                   !VAR = DATE_OPERATOR(LN(ISTART:ISTOP))  --CAUSES COMILER ERROR
                                                   CALL GET_DATE(LN,LLOC,ISTART,ISTOP,IOUT,IN,VAR(ONE),NO_PARSE_WORD=TRUE,HAS_ERROR=NEG_LLOC,NO_DATE=TRUE)
                                                   IF(NEG_LLOC) THEN
                                                       IERR = 123
                                                   ELSE
                                                       DO I=TWO, DIM1
                                                           VAR(I) = VAR(ONE)
                                                       END DO
                                                   END IF
                       CLASS DEFAULT
                                                   READ(LN(ISTART:ISTOP), *, IOSTAT=IERR) CONST
                       END SELECT
                       !
                       IF(IERR /= Z .AND. ALLOW_ERROR)   CALL FILE_IO_ERROR( IERR, UNIT=ERROR_IU, LINE=LN, INFILE=IN, OUTPUT=IOUT, MSG= 'FOUND KEYWORD CONSTANT, BUT FAILED TO READ THE VALUE LOCATED AFTER THE KEYWORD "CONSTANT".', MSG2=MSG)
                       IF(IERR /= Z .AND. CLEAR_IU)  IU = Z
                       !
                       SELECT TYPE (VAR)
                       TYPE IS (REAL(REAL64));     VAR = CONST
                       TYPE IS (REAL(REAL32));     VAR = REAL(CONST,REAL32)
                       TYPE IS (INTEGER)
                                                   READ(LN(ISTART:ISTOP), *, IOSTAT=IERR) I
                                                   VAR = I
                       TYPE IS (CHARACTER(*))
                                                   IF(READ_FULL_LINE) ISTOP = COMMENT_INDEX(LN)   !GET INDEX WHERE # IS OR LEN_TRIM
                                                   VAR = LN(ISTART:ISTOP)
                                                   IF(LINELEN<LEN_TRIM(LN(ISTART:ISTOP))) NEG_LLOC = TRUE
                       TYPE IS (CHARACTER_TYPE)
                                                   ISTOP = COMMENT_INDEX(LN)   !GET INDEX WHERE # IS OR LEN_TRIM
                                                   N = LEN_TRIM(LN(ISTART:ISTOP))
                                                   VAR = LN(ISTART:N)
                       TYPE IS (TIME_SERIES_FILE);
                                                   DO I=1, DIM1
                                                               CALL VAR(I)%INIT(CONST)
                                                   END DO
                                                   !
                       TYPE IS (LOOKUP_TABLE_TYPE);
                                                   DO CONCURRENT (I=1:DIM1);   CALL VAR(I)%LOAD(CONST)
                                                   END DO
                                                   !
                       TYPE IS (REAL(REAL128));    VAR = REAL(CONST,REAL128)   ! QUAD PRECISION
                       END SELECT
                       !
                       CALL CHECK_FOR_POST_KEY(LLOC,LN,IN,IOUT,ISTART,ISTOP,SFAC_FILE) !ISTART, ISTOP ARE PLACE HOLDERS FOR BUF AND ISPLIT
                       !
                       IF(SFAC_FILE /= UNO) CALL SF%SET_ALL(SFAC_FILE)
                       !
                       IF(PRESENT(SFAC)) THEN
                                             SFAC = SF
                       ELSE
                                             SELECT TYPE (VAR)
                                             TYPE IS (REAL(REAL64));    CALL SF%APPLY(VAR)    ! DOUBLE PRECISION
                                             TYPE IS (REAL(REAL32));    CALL SF%APPLY(VAR)    ! SINGLE PRECISION
                                             TYPE IS (TIME_SERIES_FILE)
                                                                        IF(SF%HAS_ALL) THEN
                                                                            DO CONCURRENT (I=1:DIM1); VAR(I)%FL%SCALE = VAR(I)%FL%SCALE*SF%ALL
                                                                            END DO
                                                                        END IF
                                             TYPE IS (LOOKUP_TABLE_TYPE)
                                                                        IF(SF%HAS_ALL) THEN
                                                                           DO CONCURRENT (I=1:DIM1); VAR(I)%Y = VAR(I)%Y*SF%ALL
                                                                           END DO
                                                                        END IF
                                             TYPE IS (REAL(REAL128));   CALL SF%APPLY(VAR)   ! QUAD PRECISION
                                             END SELECT
                       END IF
                       !
                       LLOC = ONE
                       IF(NEG_LLOC) LLOC = NEG
                       LN = EXT//BLNK//LN
                       RETURN
    CASE( 'REPEAT' )   !-----------------------------------------------------------------------------------------------------------------------
                       !
                       IF(PRESENT(NO_REPEAT)) THEN
                               IF(NO_REPEAT) CALL STOP_ERROR( LINE=LN, INFILE=ERROR_IU, OUTPUT=IOUT, MSG= 'ULOAD FOUND KEYWORD "REPEAT" BUT THIS PARTICULAR FEATURE DOES NOT SUPPORT IT.'//NL//'(NOTE APPLYING A SCALE FACTOR WITH SFAC, DOES NOT SUPPORT THIS.)'//NL//'PLEAUSE CHANGE INPUT TO USE OPEN/CLOSE TO RELOAD THE FILE INSTEAD OF REPEATING IT.', MSG2=MSG)
                       END IF
                       !
                       CALL PARSE_WORD(LN,LLOC,ISTART,ISTOP)
                       CALL CHECK_FOR_POST_KEY(LLOC,LN,IN,IOUT,ISTART,ISTOP,SFAC_FILE) !ISTART, ISTOP ARE PLACE HOLDERS FOR BUF AND ISPLIT
                       !
                       IF(SFAC_FILE /= UNO) CALL SF%SET_ALL(SFAC_FILE)
                       !
                       IF(PRESENT(SFAC)) THEN
                                             SFAC = SF
                       ELSE
                                             SELECT TYPE (VAR)
                                             TYPE IS (REAL(REAL64));     CALL SF%APPLY(VAR)  ! DOUBLE PRECISION
                                             TYPE IS (REAL(REAL32));     CALL SF%APPLY(VAR)  ! SINGLE PRECISION
                                             TYPE IS (TIME_SERIES_FILE); VAR%FL%SCALE = SFAC_FILE
                                             TYPE IS (LOOKUP_TABLE_TYPE)
                                                                        IF(SF%HAS_ALL) THEN
                                                                                      IF(SF%ALL /= UNO)CALL STOP_ERROR( LINE=LN, INFILE=ERROR_IU, OUTPUT=IOUT, MSG= 'LOOKUP TABLE LOAD ERROR - ULOAD FOUND KEYWORD "REPEAT" WITH A SCALE FACTOR THAT IS NOT EQUAL TO 1. LOOKUP TABLES IMMEDIATELY APPLY SCALE FACTORS SO THERE IS NO WAY TO KEEP THEM SEPARATE. EITHER RE-SPECIFY LOOKUP TABLE OR REMOVE SFAC OR SCALE.', MSG2=MSG)
                                                                        END IF
                                             TYPE IS (REAL(REAL128));    CALL SF%APPLY(VAR)  ! QUAD PRECISION
                                             END SELECT
                       END IF
                       !
                       LN = EXT//BLNK//LN
                       RETURN
    CASE( 'RELOAD' ) !-----------------------------------------------------------------------------------------------------------------------
                     !
                     IF(IU==Z) THEN
                          IF(PRESENT(OLD_IU))  IU = OLD_IU
                          IF(ALLOW_ERROR .AND. (IU==Z .OR. .NOT. PRESENT(OLD_IU))) THEN
                              CALL STOP_ERROR( LINE=LN, INFILE=ERROR_IU, OUTPUT=IOUT, MSG= 'ULOAD FOUND KEYWORD "RELOAD" BUT THERE WAS NOT A PREVIOUSLY OPENED FILE TO USE OR THIS INPUT FEATURE DOES NOT SUPPORT THE "RELOAD" KEYWORD.'//NL//'PLEASE MAKE SURE THAT THIS ULOAD WAS PRECEDED BY AN EXTERNAL UNIT, DATAUNIT UNIT, OR DATAFILE FILE'//NL//'SO THT THE FILE CAN BE CONTINUED TO LOAD NEXT INPUT OR CHANGE KEYWORD TO ONE THAT OPENS A FILE OR POINTS TO AN EXISTING OPENED FILE.'//NL//'(e.g. THIS KEYWORD SHOULD ONLY APPEAR IN A TFR AFTER AN EXTERNAL, DATAUNIT, or DATAFILE KEYWORDS TO REUSE THEIR UNIT NUMBERS AND CONTINUE LOADING FROM THEIR FILES)', MSG2=MSG)
                          END IF
                          !
                          IF(IU /= Z) THEN
                                                CALL CHECK_FOR_POST_KEY(LLOC,LN,IN,IOUT,ISTART,ISTOP,SFAC_FILE) !ISTART, ISTOP ARE PLACE HOLDERS FOR BUF AND ISPLIT
                                                !
                                                INQUIRE(IU,FORM=FORM_CHK, OPENED=KEEP_IU)
                                                IF(.NOT. KEEP_IU) CALL STOP_ERROR(LINE=LN, INFILE=IN, OUTPUT=IOUT, MSG= 'ULOAD ERROR: RECIEVED UNIT NUMBER "'//NUM2STR(IU)//'" WHICH IS NOT ASSOCIATED WITH A FILE. THIS COULD BE THE RESULT OF USING "EXTERNAL" OR "DATAUNIT" WITH A UNIT THAT IS NOT OPENED BY THE NAME FILE.', MSG2=MSG)
                                                BIN = FORM_CHK /= 'FORMATTED'
                                                !
                                                IF(BIN) THEN
                                                    REWIND(IU) !MOVE BACK TO TOP OF FILE
                                                ELSE
                                                    CALL UTF8_BOM_OFFSET_REWIND(IU)  !Check in case file has a UTF8 BOM header, rewind to correct location
                                                END IF
                                                !
                                                ERROR_IU = IU
                                                KEEP_IU  = TRUE
                                                LLOC     = ONE
                                                IF(.NOT. ALLOW_ERROR) CLEAR_IU = TRUE
                                                IF(.NOT. BIN) CALL READ_TO_DATA(LN,IU,IOUT)  !UNIT IS BEING PASSED IN SO READ IN LINE FROM IT
                          END IF
                       ELSE
                            CALL STOP_ERROR( LINE=LN, INFILE=ERROR_IU, OUTPUT=IOUT, MSG= 'ULOAD FOUND KEYWORD "RELOAD" BUT THIS INPUT OPTION EITHER DOES NOT SUPPORT RELOAD OR THERE IS A CODE ERROR. PLEASE CHANGE RELOAD TO SOMETHING LIKE "EXTERNAL  55  REWIND"', MSG2=MSG)
                     END IF
    CASE( 'LOAD_NEXT' )!-----------------------------------------------------------------------------------------------------------------------
                       !
                       IF(IU==Z) THEN
                            IF(PRESENT(OLD_IU))  IU = OLD_IU
                            IF(ALLOW_ERROR .AND. (IU==Z .OR. .NOT. PRESENT(OLD_IU))) THEN
                                CALL STOP_ERROR( LINE=LN, INFILE=ERROR_IU, OUTPUT=IOUT, MSG= 'ULOAD FOUND KEYWORD "LOAD_NEXT" BUT THERE WAS NOT A PREVIOUSLY OPENED FILE TO USE OR THIS INPUT FEATURE DOES NOT SUPPORT THE "LOAD_NEXT" KEYWORD (viz. SFAC DOES NOT SUPPORT IT).'//NL//'PLEASE MAKE SURE THAT THIS ULOAD WAS PRECEDED BY AN EXTERNAL UNIT, DATAUNIT UNIT, OR DATAFILE FILE'//NL//'SO THT THE FILE CAN BE CONTINUED TO LOAD NEXT INPUT'//NL//'OR CHANGE KEYWORD TO ONE THAT OPENS A FILE OR POINTS TO AN EXISTING OPENED FILE.'//NL//'(e.g. THIS KEYWORD SHOULD ONLY APPEAR IN A TFR AFTER AN EXTERNAL, DATAUNIT, or DATAFILE KEYWORDS TO REUSE THEIR UNIT NUMBERS AND CONTINUE LOADING FROM THEIR FILES)', MSG2=MSG)
                            END IF
                            !
                            IF(IU /= Z) THEN
                                                  CALL CHECK_FOR_POST_KEY(LLOC,LN,IN,IOUT,ISTART,ISTOP,SFAC_FILE) !ISTART, ISTOP ARE PLACE HOLDERS FOR BUF AND ISPLIT
                                                  !
                                                  INQUIRE(IU,FORM=FORM_CHK, OPENED=KEEP_IU)
                                                  IF(.NOT. KEEP_IU) CALL STOP_ERROR(LINE=LN, INFILE=IN, OUTPUT=IOUT, MSG= 'ULOAD ERROR: RECIEVED UNIT NUMBER "'//NUM2STR(IU)//'" WHICH IS NOT ASSOCIATED WITH A FILE. THIS COULD BE THE RESULT OF USING "EXTERNAL" OR "DATAUNIT" WITH A UNIT THAT IS NOT OPENED BY THE NAME FILE.', MSG2=MSG)
                                                  BIN = FORM_CHK /= 'FORMATTED'
                                                  !
                                                  ERROR_IU = IU
                                                  KEEP_IU  = TRUE
                                                  LLOC     = ONE
                                                  IF(.NOT. ALLOW_ERROR) CLEAR_IU = TRUE
                                                  IF(.NOT. BIN) CALL READ_TO_DATA(LN,IU,IOUT)  !UNIT IS BEING PASSED IN SO READ IN LINE FROM IT
                            END IF
                       ELSE
                            CALL STOP_ERROR( LINE=LN, INFILE=ERROR_IU, OUTPUT=IOUT, MSG= 'ULOAD FOUND KEYWORD "LOAD_NEXT" BUT THIS INPUT OPTION EITHER DOES NOT SUPPORT RELOAD OR THERE IS A CODE ERROR. PLEASE CHANGE LOAD_NEXT TO SOMETHING LIKE "DATAFILE MyFile.txt"', MSG2=MSG)
                       END IF
    END SELECT
    !
    IF(IU==Z) THEN
          N = LLOC
          ALLOCATE(FL)
          !
          CALL FL%OPEN(LN,LLOC,IOUT,IN,NOSTOP=TRUE,KEY_FAIL_STOP=TRUE,REQKEY=TRUE,BINARY=BIN, MSG=MSG)
          SFAC_FILE = FL%SCALE
          FL%SCALE = UNO
          BIN = FL%BINARY
          IF(.NOT. FL%ERROR) THEN
                                 IU = FL%IU
                                 IF(IU /= Z) ERROR_IU = IU
                                 !
                                 IF(.NOT. FL%IS_INTERNAL .AND. .NOT. FL%OPENCLOSE) KEEP_IU = TRUE  !FOUND EXTERNAL KEYWORD, SO PASS BACK WHAT WAS LOADED
                                 !
                                 IF(FL%IS_INTERNAL .AND. NO_INTERN) CALL STOP_ERROR( LINE=LN, INFILE=ERROR_IU, OUTPUT=IOUT, MSG= 'ULOAD ERROR: THIS SPECIFIC INPUT LINE DOES NOT ALLOW FOR THE "INTERNAL" KEYWORD. PLEASE MOVE INTERNAL INPUT TO SEPARATE FILE AND CHANGE KEYWORD TO "OPEN/CLOSE" OR "EXTERNAL" OR INSTEAD USE AN IMPLIED INTERNAL BY HAVING ALL THE INPUT DATA ON THE SAME LINE.', MSG2=MSG)
                                 IF(FL%IS_INTERNAL) IU = INFILE
                                 !
                                 LLOC = ONE
                                 IF(.NOT. BIN) CALL READ_TO_DATA(LN,IU,IOUT)
          ELSE !NO KEYWORD FOUND SO DATA IS WITHIN LINE
              LLOC = N
              IU = Z
          END IF
    END IF
    !
    IF(.NOT. BIN .AND. IU /= Z) THEN
                                DO !CHECK FOR KEYWORD OF SFAC WITHIN LIST
                                      N = LLOC
                                      CALL GET_WORD(LN, LLOC, ISTART, ISTOP, EXT, COM_STOP=TRUE)
                                      IF (EXT == 'SFAC') THEN
                                          CALL SFAC_READ_AND_RETURN(SF, LN(N:), IU, IOUT, ROW_WORD, DIM1, EX1_WORD=EX1_WORD, EX1_DIM=EX1_DIM, EX2_WORD=EX2_WORD, EX2_DIM=EX2_DIM, EX3_WORD=EX3_WORD, EX3_DIM=EX3_DIM)
                                          LLOC = ONE
                                          CALL READ_TO_DATA(LN,IU,IOUT)
                                      ELSE
                                          LLOC = N
                                          EXIT
                                      END IF
                                END DO
    END IF
    !
    IF(BIN) THEN
                !
                IF(IU==INFILE)                             CALL STOP_ERROR( LINE=LN, INFILE=ERROR_IU, OUTPUT=IOUT, MSG= 'ULOAD ERROR: "INTERNAL" AND "BINARY" KEYWORD FOUND, BUT THIS IS NOT ALLOWED!', MSG2=MSG)
                IF(PRESENT(NO_BINARY)) THEN; IF(NO_BINARY) CALL STOP_ERROR( LINE=LN, INFILE=ERROR_IU, OUTPUT=IOUT, MSG= 'ULOAD ERROR: "BINARY" KEYWORD FOUND, BUT THIS INPUT PROPERTY DOES NOT SUPPORT BINARY FILE INPUT. PLEASE USE ASCII/TEXT INPUT INSTEAD AND REMOVE "BINARY" KEYWORD.', MSG2=MSG)
                END IF
                !
                SELECT TYPE (VAR)
                TYPE IS (REAL(REAL64));       READ(IU, IOSTAT=IERR) VAR   ! DOUBLE PRECISION
                TYPE IS (REAL(REAL32));       READ(IU, IOSTAT=IERR) VAR   ! SINGLE PRECISION
                TYPE IS (INTEGER);            READ(IU, IOSTAT=IERR) VAR
                TYPE IS (CHARACTER(*));       READ(IU, IOSTAT=IERR) VAR
                TYPE IS (DATE_OPERATOR);      READ(IU, IOSTAT=IERR) VAR
                TYPE IS (GENERIC_INPUT_FILE); CALL STOP_ERROR( LINE=LN, INFILE=ERROR_IU, OUTPUT=IOUT, MSG= 'ULOAD ERROR: "BINARY" KEYWORD FOUND, BUT THIS INPUT PROPERTY DOES NOT SUPPORT BINARY FILE INPUT. PLEASE USE ASCII/TEXT INPUT INSTEAD AND REMOVE "BINARY" KEYWORD.', MSG2=MSG)
                TYPE IS (TIME_SERIES_FILE);   CALL STOP_ERROR( LINE=LN, INFILE=ERROR_IU, OUTPUT=IOUT, MSG= 'ULOAD ERROR: "BINARY" KEYWORD FOUND, BUT THIS INPUT PROPERTY DOES NOT SUPPORT BINARY FILE INPUT. PLEASE USE ASCII/TEXT INPUT INSTEAD AND REMOVE "BINARY" KEYWORD.', MSG2=MSG)
                TYPE IS (LOOKUP_TABLE_TYPE);  CALL STOP_ERROR( LINE=LN, INFILE=ERROR_IU, OUTPUT=IOUT, MSG= 'ULOAD ERROR: "BINARY" KEYWORD FOUND, BUT THIS INPUT PROPERTY DOES NOT SUPPORT BINARY FILE INPUT. PLEASE USE ASCII/TEXT INPUT INSTEAD AND REMOVE "BINARY" KEYWORD.', MSG2=MSG)
                TYPE IS (CHARACTER_TYPE);     CALL STOP_ERROR( LINE=LN, INFILE=ERROR_IU, OUTPUT=IOUT, MSG= 'ULOAD ERROR: "BINARY" KEYWORD FOUND, BUT THIS INPUT PROPERTY DOES NOT SUPPORT BINARY FILE INPUT. PLEASE USE ASCII/TEXT INPUT INSTEAD AND REMOVE "BINARY" KEYWORD.', MSG2=MSG)
                TYPE IS (REAL(REAL128));      READ(IU, IOSTAT=IERR) VAR   ! QUAD PRECISION
                END SELECT
                !
                IF(IERR /= Z .AND. ALLOW_ERROR)   CALL FILE_IO_ERROR( IERR, UNIT=ERROR_IU, LINE=LN, INFILE=IN, OUTPUT=IOUT, MSG= 'ATTEMPT MADE TO READ LIST FROM ULOAD IN A BINARY FILE, BUT IT FAILED TO LOAD. PLEASURE ENSURE THAT BINARY FORMAT COMPLIES TO WHAT IS EXPECTED TO BE LOADED.', MSG2=MSG)
                IF(IERR /= Z .AND. CLEAR_IU)  IU = Z
    ELSE
        IF(IU == Z) THEN  !NO KEYWORD, SO ASSUME IT IS ON CURRENT LINE
              IMPLIED_INTERNAL = TRUE
              SELECT TYPE (VAR)
              TYPE IS (REAL(REAL64))
                                     CALL GET_NUMBER(LN,LLOC,ISTART,ISTOP,IOUT,IN,VAR,IOSTAT=IERR)      !READ(LN(LLOC:), *, IOSTAT=IERR) VAR  ! DOUBLE PRECISION
                                     !
                                     CALL PARSE_WORD(LN,LLOC,ISTART,ISTOP,COM_STOP=TRUE) !Check for SFAC
                                     IF(ISTOP - ISTART == THREE ) THEN
                                        EXT = LN(ISTART:ISTOP)
                                        CALL UPPER(12, EXT)
                                        IF (EXT == 'SFAC')  CALL SFAC_READ_AND_RETURN(SF, LN(LLOC:), IN, IOUT, ROW_WORD, DIM1, EX1_WORD=EX1_WORD, EX1_DIM=EX1_DIM, EX2_WORD=EX2_WORD, EX2_DIM=EX2_DIM, EX3_WORD=EX3_WORD, EX3_DIM=EX3_DIM, SCRATCH=SCRATCH, SKIP_SFAC=TRUE)
                                     END IF
              TYPE IS (REAL(REAL32))
                                     CALL GET_NUMBER(LN,LLOC,ISTART,ISTOP,IOUT,IN,VAR,IOSTAT=IERR)      !READ(LN(LLOC:), *, IOSTAT=IERR) VAR  ! SINGLE PRECISION
                                     !
                                     CALL PARSE_WORD(LN,LLOC,ISTART,ISTOP,COM_STOP=TRUE) !Check for SFAC
                                     IF(ISTOP - ISTART == THREE ) THEN
                                        EXT = LN(ISTART:ISTOP)
                                        CALL UPPER(12, EXT)
                                        IF (EXT == 'SFAC')  CALL SFAC_READ_AND_RETURN(SF, LN(LLOC:), IN, IOUT, ROW_WORD, DIM1, EX1_WORD=EX1_WORD, EX1_DIM=EX1_DIM, EX2_WORD=EX2_WORD, EX2_DIM=EX2_DIM, EX3_WORD=EX3_WORD, EX3_DIM=EX3_DIM, SCRATCH=SCRATCH, SKIP_SFAC=TRUE)
                                     END IF
                                     !
              TYPE IS (INTEGER);     READ(LN(LLOC:), *, IOSTAT=IERR) VAR
              TYPE IS (CHARACTER(*))
                                   DO I=1, DIM1
                                                  CALL PARSE_WORD(LN,LLOC,ISTART,ISTOP,COM_STOP=TRUE)
                                                  VAR(I) = LN(ISTART:ISTOP)
                                                  IF(LN(ISTART:ISTOP)=='') THEN
                                                      IERR = 69
                                                      EXIT
                                                  ELSEIF(LINELEN<LEN_TRIM(LN(ISTART:ISTOP))) THEN
                                                      NEG_LLOC = TRUE
                                                  END IF
                                   END DO
              TYPE IS (CHARACTER_TYPE)
                                       IF( DIM1 == ONE) THEN
                                          ISTOP = COMMENT_INDEX(LN)   !GET INDEX WHERE # IS OR LEN_TRIM
                                          N = LEN_TRIM(LN(ISTART:ISTOP))
                                          VAR(ONE) = LN(ISTART:N)
                                       ELSE
                                          IF(ALLOW_ERROR) THEN
                                                 CALL STOP_ERROR( LINE=LN, INFILE=IN, OUTPUT=IOUT, MSG= 'ULOAD ERROR: THIS PARTICULAR INPUT CANNOT BE LOADED WITH IMPLIED INTERNAL. PLEASE USE A KEYWORD TO INDICATE WHERE THE INPUT IS LOCATED (e.g INTERNAL|EXTERNAL|OPEN/CLOSE)', MSG2=MSG)
                                          END IF
                                          IF(CLEAR_IU) THEN
                                             IU = Z
                                             LLOC = Z
                                          END IF
                                          RETURN
                                       END IF
              TYPE IS (GENERIC_INPUT_FILE)
                                   DO I=1, DIM1
                                                  CALL VAR(I)%OPEN(LN,LLOC,IOUT,IN,NOSTOP=TRUE,NO_POSTKEY_CHECK=TRUE)
                                                  !
                                                  IF(VAR(I)%ERROR) THEN
                                                      IERR = 222
                                                      EXIT
                                                  END IF
                                   END DO
              TYPE IS (DATE_OPERATOR)
                                   DO I=1, DIM1
                                                  CALL GET_DATE(LN,LLOC,ISTART,ISTOP,IOUT,IN,VAR(I),HAS_ERROR=NEG_LLOC,NO_DATE=TRUE)
                                                  IF( NEG_LLOC ) THEN
                                                      IERR = 123
                                                      EXIT
                                                  END IF
                                   END DO
              TYPE IS (TIME_SERIES_FILE)
                                       IF( DIM1 == ONE) THEN
                                           CALL VAR(ONE)%INIT(LN, LLOC, IOUT, IN, FALSE, DEFAULT_OPT=TEXT)
                                       ELSE
                                          IF(ALLOW_ERROR) THEN
                                                 CALL STOP_ERROR( LINE=LN, INFILE=IN, OUTPUT=IOUT, MSG= 'ULOAD ERROR: TIME SERIES FILE LOADING OF MORE THEN ONE FILE CANNOT BE LOADED WITH IMPLIED INTERNAL. PLEASE USE A KEYWORD TO INDICATE WHERE TIME SERIES FILES ARE LOCATED (e.g INTERNAL|EXTERNAL|OPEN/CLOSE)', MSG2=MSG)
                                          END IF
                                          IF(CLEAR_IU) THEN
                                             IU = Z
                                             LLOC = Z
                                          END IF
                                          RETURN
                                       END IF
                                       !
              TYPE IS (LOOKUP_TABLE_TYPE)
                                       IF( DIM1 == ONE) THEN
                                           CALL VAR(ONE)%LOAD(LLOC,ISTART,ISTOP,LN,INFILE,IOUT,FALSE,NO_INTERNAL)
                                       ELSE
                                          IF(ALLOW_ERROR) THEN
                                                 CALL STOP_ERROR( LINE=LN, INFILE=IN, OUTPUT=IOUT, MSG= 'LOOKUP TABLE LOAD - ULOAD ERROR: LOOKUP TABLE LOADING OF MORE THEN ONE FILE CANNOT BE LOADED WITH IMPLIED INTERNAL. PLEASE USE A KEYWORD TO INDICATE WHERE TIME SERIES FILES ARE LOCATED (e.g INTERNAL|EXTERNAL|OPEN/CLOSE)', MSG2=MSG)
                                          END IF
                                          IF(CLEAR_IU) THEN
                                             IU = Z
                                             LLOC = Z
                                          END IF
                                          RETURN
                                       END IF
                                       !
              TYPE IS (REAL(REAL128))
                                     CALL GET_NUMBER(LN,LLOC,ISTART,ISTOP,IOUT,IN,VAR,IOSTAT=IERR)      !READ(LN(LLOC:), *, IOSTAT=IERR) VAR   ! QUAD PRECISION
                                     !
                                     CALL PARSE_WORD(LN,LLOC,ISTART,ISTOP,COM_STOP=TRUE) !Check for SFAC
                                     IF(ISTOP - ISTART == THREE ) THEN
                                        EXT = LN(ISTART:ISTOP)
                                        CALL UPPER(12, EXT)
                                        IF (EXT == 'SFAC')  CALL SFAC_READ_AND_RETURN(SF, LN(LLOC:), IN, IOUT, ROW_WORD, DIM1, EX1_WORD=EX1_WORD, EX1_DIM=EX1_DIM, EX2_WORD=EX2_WORD, EX2_DIM=EX2_DIM, EX3_WORD=EX3_WORD, EX3_DIM=EX3_DIM, SCRATCH=SCRATCH, SKIP_SFAC=TRUE)
                                     END IF
              END SELECT
        ELSE
            DO I=1, DIM1
                  IF(I>ONE) CALL READ_TO_DATA(LN,IU,IOUT)
                  LLOC   = ONE
                  IF(READ_ID) THEN
                        CALL PARSE_WORD(LN,LLOC,ISTART,ISTOP,COM_STOP=TRUE)
                        IF(SET_ID) THEN
                                       READ(LN(ISTART:ISTOP), *, IOSTAT=IERR)      ID(I)
                                                                     IF(IERR /= Z) ID(I) = Z
                        END IF
                  END IF
                  !
                  CALL PARSE_WORD(LN,LLOC,ISTART,ISTOP,COM_STOP=TRUE)
                  !
                  SELECT TYPE (VAR)
                  TYPE IS (REAL(REAL64));     READ(LN(ISTART:ISTOP), *, IOSTAT=IERR) VAR(I)  ! DOUBLE PRECISION
                  TYPE IS (REAL(REAL32));     READ(LN(ISTART:ISTOP), *, IOSTAT=IERR) VAR(I)  ! SINGLE PRECISION
                  TYPE IS (INTEGER);          READ(LN(ISTART:ISTOP), *, IOSTAT=IERR) VAR(I)
                  TYPE IS (CHARACTER(*));
                                              IF(READ_FULL_LINE) ISTOP = COMMENT_INDEX(LN)
                                              IF(LINELEN<LEN_TRIM(LN(ISTART:ISTOP))) NEG_LLOC = TRUE
                                              VAR(I) = LN(ISTART:ISTOP)
                  TYPE IS (CHARACTER_TYPE)
                                              ISTOP = COMMENT_INDEX(LN)   !GET INDEX WHERE # IS OR LEN_TRIM
                                              N = LEN_TRIM(LN(ISTART:ISTOP))
                                              VAR(I) = LN(ISTART:N)
                  TYPE IS (GENERIC_INPUT_FILE)
                                              LLOC = ISTART
                                              CALL VAR(I)%OPEN(LN,LLOC,IOUT,IN,NOSTOP=TRUE,NO_INTERNAL=TRUE,KEY=EXT)
                                              !
                                              IF(VAR(I)%NULL_FILE) THEN
                                                  VAR(I)%ERROR = FALSE
                                              ELSEIF(VAR(I)%ERROR) THEN
                                                  IERR = 222
                                                  EXIT
                                              END IF
                  TYPE IS (DATE_OPERATOR)
                                              !CALL VAR(I)%INIT(LN(ISTART:ISTOP))
                                              !IF( VAR(I)%NOT_SET() ) IERR = 123
                                              CALL GET_DATE(LN,LLOC,ISTART,ISTOP,IOUT,IN,VAR(I),NO_PARSE_WORD=TRUE,HAS_ERROR=NEG_LLOC,NO_DATE=TRUE)
                                              IF( NEG_LLOC ) IERR = 123
                  TYPE IS (TIME_SERIES_FILE)
                                              LLOC = ISTART
                                              CALL VAR(I)%INIT(LN, LLOC, IOUT, IN, FALSE, DEFAULT_OPT=TEXT)
                                              !
                   TYPE IS (LOOKUP_TABLE_TYPE)
                                              LLOC = ISTART
                                              CALL VAR(I)%LOAD(LLOC,ISTART,ISTOP,LN,IU,IOUT,FALSE,NO_INTERNAL)
                  TYPE IS (REAL(REAL128));    READ(LN(ISTART:ISTOP), *, IOSTAT=IERR) VAR(I)   ! QUAD PRECISION
                  END SELECT
                  !
                  IF(LN(ISTART:ISTOP) == '') IERR = -1234
                  IF(IERR /= Z) EXIT
            END DO
        END IF
        !
        IF(IERR /= Z .AND. ALLOW_ERROR) THEN
               IF(NO_MAIN_KEY) THEN
                    CALL FILE_IO_ERROR( IERR, UNIT=ERROR_IU, LINE=LN, INFILE=IN, OUTPUT=IOUT, MSG= 'Attempt made to read list from uload on line, but it failed to load the following value "'//LN(ISTART:ISTOP)//'"'//BLN//'Note that no keyword was found on the previous line, so code automoved downward to next line.'//NL//'You maybe missing a keyword (viz. INTERNAL, EXTERNAL, OPEN/CLOSE, DATEFILE, DATAUNIT).', MSG2=MSG)
               ELSE
                    SELECT TYPE (VAR)
                    TYPE IS (DATE_OPERATOR);           CALL FILE_IO_ERROR( IERR, UNIT=ERROR_IU, LINE=LN, INFILE=IN, OUTPUT=IOUT, MSG= 'Attempt made to read date from ULOAD list on line, but it failed to convert the following to a calendar date "'//LN(ISTART:ISTOP)//'"'//BLN//'THE ACCEPTED DATE FORMATS ARE "mm/dd/YYYY" OR "YYYY-mm-dd" IF YOU WANT TO ADD A 24-HOUR TIME TO IT YOU MUST ADD TO THE DATE "Thh:mm:ss" (e.g. "YYYY-mm-ddThh:mm:ss")', MSG2=MSG)
                    TYPE IS (GENERIC_INPUT_FILE);      CALL FILE_IO_ERROR( IERR, UNIT=ERROR_IU, LINE=LN, INFILE=IN, OUTPUT=IOUT, MSG= 'Attempt made to open file from list of files being loaded with ULOAD, but it failed to either open, find the file, or is not set up correctly. The following is the file that was attempted to be opened "'//LN(ISTART:ISTOP)//'".', MSG2=MSG)
                    CLASS DEFAULT
                                                       IF(IMPLIED_INTERNAL) THEN
                                                           CALL FILE_IO_ERROR( IERR, UNIT=ERROR_IU, LINE=LN, INFILE=IN, OUTPUT=IOUT, MSG= 'Attempt made to read list from ULOAD on line'//NL//'But it failed to identify GENERIC INPUT/ULOAD keyword (e.g. INTERNAL, OPEN/CLOSE),'//BLN//'So ULOAD attemped an IMPLIED INTERNAL load'//BLN//'ULOAD failed to read the values with IMPLIED INTERNAL load,'//BLN//'Perhaps input does not support IMPLIED INTERNAL,'//BLN//'A bad value was specified,'//BLN//'You do not have '//NUM2STR(SIZE(VAR))//' Input values specified along the line,'//BLN//'Or the preloaded line was not long enough to read entire input.'//NL//'The max line length is '//NUM2STR(LEN(LN))//' characters.'//NL//'If using a TRANSIENT FILE READER (TFR), you can increase the length of the pre-loaded line'//NL//'with the post-keyword "DIM" followed by the size of the line.'//NL//'For example:'//NL//'           "ROOT_DEPTH  TRANSIENT  LIST  OPEN/CLOSE  Input.txt  DIM 2000"'//NL//'Indicate that the max line length is 2000 characters.', MSG2=MSG)
                                                       ELSE
                                                           CALL FILE_IO_ERROR( IERR, UNIT=ERROR_IU, LINE=LN, INFILE=IN, OUTPUT=IOUT, MSG= 'Attempt made to read list from ULOAD on line, but it failed to load the following value "'//LN(ISTART:ISTOP)//'"', MSG2=MSG)
                                                       END IF
                    END SELECT

               END IF
        ELSEIF(IERR /= Z .AND. CLEAR_IU) THEN
               IU = Z
               LLOC = Z
        END IF
        !
    END IF
    !
    IF(SFAC_FILE /= UNO) CALL SF%SET_ALL(SFAC_FILE)
    !
    IF(PRESENT(SFAC)) THEN
                           SFAC = SF
    ELSE
                           SELECT TYPE (VAR)
                           TYPE IS (REAL(REAL64));    CALL SF%APPLY(VAR)   ! DOUBLE PRECISION
                           TYPE IS (REAL(REAL32));    CALL SF%APPLY(VAR)   ! SINGLE PRECISION
                           TYPE IS (TIME_SERIES_FILE)
                                                      IF(SF%HAS_ALL) THEN
                                                          DO CONCURRENT (I=1:DIM1); VAR(I)%FL%SCALE = VAR(I)%FL%SCALE*SF%ALL
                                                          END DO
                                                      END IF
                           TYPE IS (LOOKUP_TABLE_TYPE)
                                                      IF(SF%HAS_ALL) THEN
                                                         DO CONCURRENT (I=1:DIM1); VAR(I)%Y = VAR(I)%Y*SF%ALL
                                                         END DO
                                                      END IF
                           TYPE IS (GENERIC_INPUT_FILE)
                                                      IF(SF%HAS_ALL) THEN
                                                          DO CONCURRENT (I=1:DIM1); VAR(I)%SCALE = VAR(I)%SCALE*SF%ALL
                                                          END DO
                                                      END IF
                           TYPE IS (REAL(REAL128));   CALL SF%APPLY(VAR)   ! QUAD PRECISION
                           END SELECT
    END IF
    !
    IF(.NOT. KEEP_IU)    IU     = Z
    IF(NEG_LLOC)         LLOC   = NEG
    IF(PRESENT(OLD_IU))  OLD_IU = IU
    !
    IF(ALLOCATED(FL))  DEALLOCATE(FL)
    !
  END SUBROUTINE
  !
  RECURSIVE SUBROUTINE ULOAD_ARRAY(VAR, LLOC, LN, IOUT, IN, IU, NOID, BINARY, ID, ROW_WORD, COL_WORD, NOSTOP, SFAC, EX1_WORD, EX1_DIM, EX2_WORD, EX2_DIM, EX3_WORD, EX3_DIM, READ_BY_DIM2, SCRATCH, NO_INTERNAL, NO_BINARY, NO_REPEAT, OLD_IU, MSG)
    CLASS(*), DIMENSION(:,:), CONTIGUOUS,            INTENT(INOUT):: VAR       !Variable to set by ULOAD
    INTEGER,                                         INTENT(INOUT):: LLOC      !SET TO -1 IF CHARACTER::VAR IS NOT LONG ENOUGH TO HOLD INPUT
    CHARACTER(*),                                    INTENT(INOUT):: LN        !Temp line to holds read input that is processed
    INTEGER,                                         INTENT(IN   ):: IN, IOUT  !File that input originated from; error output location
    INTEGER,                                         INTENT(INOUT):: IU        ! If IU=0 input is dermined from directives in IN or SCRATCH, IU /= 0 indicates input is on that file unit. Same as IN having a LN = "EXTERNAL IU"
    !
    LOGICAL,     OPTIONAL,                           INTENT(IN   ):: NOID, BINARY, NOSTOP, READ_BY_DIM2
    INTEGER,     OPTIONAL, DIMENSION(:), ALLOCATABLE,INTENT(INOUT):: ID                   !Note that ID must be ALLOCATABLE -- This is done to check if it is read
    CHARACTER(*),OPTIONAL,                           INTENT(IN   ):: ROW_WORD, COL_WORD
    TYPE(SFAC_DATA),OPTIONAL,                        INTENT(  OUT):: SFAC
    !
    CHARACTER(*), OPTIONAL,                          INTENT(IN   ):: EX1_WORD, EX2_WORD, EX3_WORD
    INTEGER,      OPTIONAL,                          INTENT(IN   ):: EX1_DIM,  EX2_DIM,  EX3_DIM
    INTEGER,      OPTIONAL,                          INTENT(IN   ):: SCRATCH
    LOGICAL,      OPTIONAL,                          INTENT(IN   ):: NO_INTERNAL, NO_BINARY, NO_REPEAT
    INTEGER,      OPTIONAL,                          INTENT(INOUT):: OLD_IU
    CHARACTER(*), OPTIONAL,                          INTENT(IN   ):: MSG
    !
    TYPE(SFAC_DATA):: SF!1, SF2
    !
    INTEGER:: IERR, ISTART, ISTOP, I, J, N, DIM1, DIM2, INFILE, ERROR_IU, IID, LINELEN
    TYPE(GENERIC_INPUT_FILE), ALLOCATABLE:: FL
    LOGICAL:: KEEP_IU, READ_ID, BIN, ALLOW_ERROR, CLEAR_IU, READ_BY_DIM1, NO_INTERN, NEG_LLOC, NO_MAIN_KEY, SET_ID
    REAL(REAL64):: SFAC_FILE, CONST  !ENSURE IT IS ALWAYS DOUBLE PRECISION
    CHARACTER(12):: EXT, FORM_CHK    ! Note 12 is passed in at CALL UPPER(12, EXT)
    !
    DIM1 = UBOUND(VAR, 1)
    DIM2 = UBOUND(VAR, 2)
    !
    IF(DIM1==Z .OR. DIM2==Z) RETURN
    !
    IERR = Z
    NEG_LLOC  = FALSE
    SFAC_FILE = UNO
    ERROR_IU = IN
    NO_MAIN_KEY=FALSE
    !
    SELECT TYPE (VAR)
    TYPE IS (CHARACTER(*)); LINELEN = LEN(VAR)
    END SELECT
    !
    IF(PRESENT(SCRATCH)) THEN
        IF(SCRATCH /= Z) THEN
                              INFILE = SCRATCH
        ELSE
                              INFILE = IN
        END IF
    ELSE
        INFILE = IN
    END IF
    !
    IF(PRESENT(NOID)) THEN
        READ_ID = .NOT. NOID
    ELSE
        READ_ID = TRUE
    END IF
    !
    IF(PRESENT(BINARY)) THEN
        BIN = BINARY
    ELSE
        BIN = FALSE
    END IF
    !
    IF(PRESENT(NOSTOP)) THEN
        ALLOW_ERROR = .NOT. NOSTOP
    ELSE
        ALLOW_ERROR = TRUE
    END IF
    !
    IF(PRESENT(NO_INTERNAL)) THEN
        NO_INTERN = NO_INTERNAL
    ELSE
        NO_INTERN = FALSE
    END IF
    !
    IF(PRESENT(READ_BY_DIM2)) THEN
        READ_BY_DIM1 = .NOT. READ_BY_DIM2
    ELSE
        READ_BY_DIM1 = TRUE
    END IF
    !
    IF(PRESENT(ID)) THEN
           SET_ID = ALLOCATED(ID)
        IF(SET_ID) THEN 
                   CALL SET_SEQUENCE(SIZE(ID), ID)
                   SET_ID = READ_ID 
        END IF
    ELSE
        SET_ID = FALSE
    END IF
    !
    CLEAR_IU = FALSE
    IF(IU /= Z) THEN
                    INQUIRE(IU,FORM=FORM_CHK, OPENED=KEEP_IU)
                    IF(.NOT. KEEP_IU) CALL STOP_ERROR( LINE=LN, INFILE=IN, OUTPUT=IOUT, MSG= 'ULOAD ERROR: RECIEVED UNIT NUMBER "'//NUM2STR(IU)//'" WHICH IS NOT ASSOCIATED WITH A FILE. THIS COULD BE THE RESULT OF USING "EXTERNAL" OR "DATAUNIT" WITH A UNIT THAT IS NOT OPENED BY THE NAME FILE.', MSG2=MSG)
                    BIN = FORM_CHK /= 'FORMATTED'
                    !
                    ERROR_IU = IU
                    KEEP_IU  = TRUE
                    LLOC     = ONE
                    IF(.NOT. ALLOW_ERROR) CLEAR_IU = TRUE
                    IF(.NOT. BIN) CALL READ_TO_DATA(LN,IU,IOUT)  !UNIT IS BEING PASSED IN SO READ IN LINE FROM IT
    ELSE
                    KEEP_IU = FALSE
    END IF
    !
    CALL SF%INIT()
    IF(PRESENT(SFAC)) CALL SFAC%INIT()
    !
    IF(.NOT. BIN) THEN
                      DO !CHECK FOR KEYWORD OF SFAC BEFORE LIST
                           N = LLOC
                           CALL GET_WORD(LN, LLOC, ISTART, ISTOP, EXT, COM_STOP=TRUE)
                           !
                           SELECT CASE(EXT)
                           CASE ('SFAC')
                               IF(IU==Z) THEN
                                             CALL SFAC_READ_AND_RETURN(SF, LN(N:), IN, IOUT, ROW_WORD, DIM1, COL_WORD, DIM2, EX1_WORD=EX1_WORD, EX1_DIM=EX1_DIM, EX2_WORD=EX2_WORD, EX2_DIM=EX2_DIM, EX3_WORD=EX3_WORD, EX3_DIM=EX3_DIM, SCRATCH=SCRATCH)
                                             CALL READ_TO_DATA(LN,INFILE,IOUT)
                               ELSE
                                             CALL SFAC_READ_AND_RETURN(SF, LN(N:), IU, IOUT, ROW_WORD, DIM1, COL_WORD, DIM2, EX1_WORD=EX1_WORD, EX1_DIM=EX1_DIM, EX2_WORD=EX2_WORD, EX2_DIM=EX2_DIM, EX3_WORD=EX3_WORD, EX3_DIM=EX3_DIM)
                                             CALL READ_TO_DATA(LN,IU,IOUT)
                               END IF
                               LLOC = ONE
                           CASE (BLNK)   !READ NEXT LINE IF BLANK
                               IF(IU==Z) THEN
                                             CALL READ_TO_DATA(LN,INFILE,IOUT)
                                             IF(LN==BLNK) CALL STOP_ERROR(INFILE=IN,OUTPUT=IOUT,MSG = 'ULOAD FAILED TO LOAD INPUT LINE.'//NL//'THIS MOST LIKELY OCCURED BECAUSE THE END OF FILE WAS REACHED WHILE LOADING INPUT.'//BLN//'POSSIBLE CAUSES ARE NOT ENOUGH INPUT LINES, AN EMPTY FILE,'//NL//'THE INPUT LINE IS EMPTY WHEN NUMBERS WERE EXPECTED,'//NL//'OR YOU SPECIFIED TRANSIENT WHEN YOU MEANT STATIC FOR A LIST-ARRAY INPUT.', MSG2=MSG)
                               ELSE
                                             CALL READ_TO_DATA(LN,IU,IOUT)
                                             IF(LN==BLNK) CALL STOP_ERROR(INFILE=IU,OUTPUT=IOUT,MSG = 'ULOAD FAILED TO LOAD INPUT LINE.'//NL//'THIS MOST LIKELY OCCURED BECAUSE THE END OF FILE WAS REACHED WHILE LOADING INPUT.'//BLN//'POSSIBLE CAUSES ARE NOT ENOUGH INPUT LINES, AN EMPTY FILE,'//NL//'THE INPUT LINE IS EMPTY WHEN NUMBERS WERE EXPECTED,'//NL//'OR YOU SPECIFIED TRANSIENT WHEN YOU MEANT STATIC FOR A LIST-ARRAY INPUT.', MSG2=MSG)
                               END IF
                               LLOC = ONE
                               NO_MAIN_KEY = TRUE
                           CASE ('ARRAY','STATIC','TRANSIENT')   !IN CASE LIST ARRAY INPUT CALLS DIRECTLY ULOAD, BY PASS KEYWORDS
                               CYCLE
                           CASE ('LIST')    !IN CASE LIST ARRAY INPUT CALLS DIRECTLY ULOAD, ERROR ON LIST KEYWORD
                               IF(IU==Z) IU = IN
                               CALL STOP_ERROR( LINE=LN, INFILE=IU, OUTPUT=IOUT, MSG= 'ULOAD ERROR: EXPECTED TO LOAD A 2D ARRAY, BUT FOUND KEYWORD LIST. PLEASE REMOVE KEYWORD OR SWITCH IT TO "ARRAY".', MSG2=MSG)
                           CASE DEFAULT
                               LLOC = N
                               EXIT
                           END SELECT
                      END DO
    ELSE
        EXT='BINARY'
    END IF
    !
    SELECT CASE (EXT)
    CASE( 'SKIP', 'NULL', 'NUL' )     !-----------------------------------------------------------------------------------------------------------------------
                       SELECT TYPE (VAR)
                       TYPE IS (REAL(REAL64));     VAR = 0.0_REAL64  ! DOUBLE PRECISION
                       TYPE IS (REAL(REAL32));     VAR = 0.0_REAL32  ! SINGLE PRECISION
                       TYPE IS (INTEGER);          VAR = 0
                       TYPE IS (CHARACTER(*));     VAR = ''
                       TYPE IS (DATE_OPERATOR);    CALL VAR%DESTROY()
                       TYPE IS (REAL(REAL128));    VAR = 0.0_REAL128 ! QUAD PRECISION
                       END SELECT
                       LN = EXT//BLNK//LN
                       RETURN
    CASE( 'CONSTANT' ) !-----------------------------------------------------------------------------------------------------------------------
                       CALL PARSE_WORD(LN,LLOC,ISTART,ISTOP)
                       CALL PARSE_WORD(LN,LLOC,ISTART,ISTOP,COM_STOP=TRUE)
                       !
                       SELECT TYPE (VAR)
                       TYPE IS (DATE_OPERATOR)
                                                   !VAR = DATE_OPERATOR(LN(ISTART:ISTOP))  !--CAUSES COMILER ERROR
                                                   CALL GET_DATE(LN,LLOC,ISTART,ISTOP,IOUT,IN,VAR(ONE,ONE),NO_PARSE_WORD=TRUE,HAS_ERROR=NEG_LLOC,NO_DATE=TRUE)
                                                   IF(NEG_LLOC) THEN
                                                       IERR = 123
                                                   ELSE
                                                       DO CONCURRENT (J=ONE:DIM2, I=ONE:DIM1, I /= ONE.AND.J /= ONE ); VAR(I,J) = VAR(ONE,ONE)
                                                       END DO
                                                   END IF
                                                   !DO I=ONE, DIM1
                                                   !DO J=ONE, DIM2
                                                   !      CALL VAR(I,J)%INIT(LN(ISTART:ISTOP))
                                                   !      IF( VAR(I,J)%NOT_SET() ) THEN
                                                   !          IERR = 123
                                                   !          EXIT
                                                   !      END IF
                                                   !END DO
                                                   !END DO
                       CLASS DEFAULT
                                                   READ(LN(ISTART:ISTOP), *, IOSTAT=IERR) CONST
                       END SELECT
                       !
                       IF(IERR /= Z .AND. ALLOW_ERROR)   CALL FILE_IO_ERROR( IERR, UNIT=ERROR_IU, LINE=LN, INFILE=IN, OUTPUT=IOUT, MSG= 'FOUND KEYWORD CONSTANT, BUT FAILED TO READ THE VALUE LOCATED AFTER THE KEYWORD "CONSTANT".', MSG2=MSG)
                       IF(IERR /= Z .AND. CLEAR_IU)  IU = Z
                       !
                       SELECT TYPE (VAR)
                       TYPE IS (REAL(REAL64));     VAR = CONST
                       TYPE IS (REAL(REAL32));     VAR = REAL(CONST, REAL32)
                       TYPE IS (INTEGER)
                                                   READ(LN(ISTART:ISTOP), *, IOSTAT=IERR) I
                                                   VAR = I
                       TYPE IS (CHARACTER(*))
                                                   VAR = LN(ISTART:ISTOP)
                                                   IF(LINELEN<LEN_TRIM(LN(ISTART:ISTOP))) NEG_LLOC = TRUE
                                                   !
                       TYPE IS (REAL(REAL128));    VAR = REAL(CONST, REAL128)
                       END SELECT
                       !
                       CALL CHECK_FOR_POST_KEY(LLOC,LN,IN,IOUT,ISTART,ISTOP,SFAC_FILE) !ISTART, ISTOP ARE PLACE HOLDERS FOR BUF AND ISPLIT
                       !
                       IF(SFAC_FILE /= UNO) CALL SF%SET_ALL(SFAC_FILE)
                       !
                       IF(PRESENT(SFAC)) THEN
                                             SFAC = SF
                       ELSE
                                             SELECT TYPE (VAR)
                                             TYPE IS (REAL(REAL64 )); CALL SF%APPLY(VAR)  ! DOUBLE PRECISION
                                             TYPE IS (REAL(REAL32 )); CALL SF%APPLY(VAR)  ! SINGLE PRECISION
                                             TYPE IS (REAL(REAL128)); CALL SF%APPLY(VAR)  ! QUAD PRECISION
                                             END SELECT
                       END IF
                       !
                       LLOC=ONE
                       IF(NEG_LLOC) LLOC = NEG
                       LN = EXT//BLNK//LN
                       RETURN
    CASE( 'REPEAT' )   !-----------------------------------------------------------------------------------------------------------------------
                       !
                       IF(PRESENT(NO_REPEAT)) THEN
                               IF(NO_REPEAT) CALL STOP_ERROR( LINE=LN, INFILE=ERROR_IU, OUTPUT=IOUT, MSG= 'ULOAD FOUND KEYWORD "REPEAT" BUT THIS PARTICULAR FEATURE DOES NOT SUPPORT IT.'//NL//'(NOTE APPLYING A SCALE FACTOR WITH SFAC, DOES NOT SUPPORT THIS.)'//NL//'PLEAUSE CHANGE INPUT TO USE OPEN/CLOSE TO RELOAD THE FILE INSTEAD OF REPEATING IT.', MSG2=MSG)
                       END IF
                       !
                       CALL PARSE_WORD(LN,LLOC,ISTART,ISTOP)
                       CALL CHECK_FOR_POST_KEY(LLOC,LN,IN,IOUT,ISTART,ISTOP,SFAC_FILE) !ISTART, ISTOP ARE PLACE HOLDERS FOR BUF AND ISPLIT
                       !
                       IF(SFAC_FILE /= UNO) CALL SF%SET_ALL(SFAC_FILE)
                       !
                       IF(PRESENT(SFAC)) THEN
                                             SFAC = SF
                       ELSE
                                             SELECT TYPE (VAR)
                                             TYPE IS (REAL(REAL64 )); CALL SF%APPLY(VAR)  ! DOUBLE PRECISION
                                             TYPE IS (REAL(REAL32 )); CALL SF%APPLY(VAR)  ! SINGLE PRECISION
                                             TYPE IS (REAL(REAL128)); CALL SF%APPLY(VAR)  ! QUAD PRECISION
                                             END SELECT
                       END IF
                       !
                       LN = EXT//BLNK//LN
                       RETURN
    CASE( 'RELOAD' ) !-----------------------------------------------------------------------------------------------------------------------
                     !
                     CALL PARSE_WORD(LN,LLOC,ISTART,ISTOP)
                     IF(IU==Z) THEN
                          IF(PRESENT(OLD_IU))  IU = OLD_IU
                          IF(ALLOW_ERROR .AND. (IU==Z .OR. .NOT. PRESENT(OLD_IU))) THEN
                              CALL STOP_ERROR( LINE=LN, INFILE=ERROR_IU, OUTPUT=IOUT, MSG= 'ULOAD FOUND KEYWORD "RELOAD" BUT THERE WAS NOT A PREVIOUSLY OPENED FILE TO USE OR THIS INPUT FEATURE DOES NOT SUPPORT THE "RELOAD" KEYWORD.'//NL//'PLEASE MAKE SURE THAT THIS ULOAD WAS PRECEDED BY AN EXTERNAL UNIT, DATAUNIT UNIT, OR DATAFILE FILE'//NL//'SO THT THE FILE CAN BE CONTINUED TO LOAD NEXT INPUT OR CHANGE KEYWORD TO ONE THAT OPENS A FILE OR POINTS TO AN EXISTING OPENED FILE.'//NL//'(e.g. THIS KEYWORD SHOULD ONLY APPEAR IN A TFR AFTER AN EXTERNAL, DATAUNIT, or DATAFILE KEYWORDS TO REUSE THEIR UNIT NUMBERS AND CONTINUE LOADING FROM THEIR FILES)', MSG2=MSG)
                          END IF
                          !
                          IF(IU /= Z) THEN
                                                CALL CHECK_FOR_POST_KEY(LLOC,LN,IN,IOUT,ISTART,ISTOP,SFAC_FILE) !ISTART, ISTOP ARE PLACE HOLDERS FOR BUF AND ISPLIT
                                                !
                                                INQUIRE(IU,FORM=FORM_CHK, OPENED=KEEP_IU)
                                                IF(.NOT. KEEP_IU) CALL STOP_ERROR(LINE=LN, INFILE=IN, OUTPUT=IOUT, MSG= 'ULOAD ERROR: RECIEVED UNIT NUMBER "'//NUM2STR(IU)//'" WHICH IS NOT ASSOCIATED WITH A FILE. THIS COULD BE THE RESULT OF USING "EXTERNAL" OR "DATAUNIT" WITH A UNIT THAT IS NOT OPENED BY THE NAME FILE.', MSG2=MSG)
                                                BIN = FORM_CHK /= 'FORMATTED'
                                                !
                                                IF(BIN) THEN
                                                    REWIND(IU) !MOVE BACK TO TOP OF FILE
                                                ELSE
                                                    CALL UTF8_BOM_OFFSET_REWIND(IU)  !Check in case file has a UTF8 BOM header, rewind to correct location
                                                END IF
                                                !
                                                ERROR_IU = IU
                                                KEEP_IU  = TRUE
                                                LLOC     = ONE
                                                IF(.NOT. ALLOW_ERROR) CLEAR_IU = TRUE
                                                IF(.NOT. BIN) CALL READ_TO_DATA(LN,IU,IOUT)  !UNIT IS BEING PASSED IN SO READ IN LINE FROM IT
                          END IF
                       ELSE
                            CALL STOP_ERROR( LINE=LN, INFILE=ERROR_IU, OUTPUT=IOUT, MSG= 'ULOAD FOUND KEYWORD "RELOAD" BUT THIS INPUT OPTION EITHER DOES NOT SUPPORT RELOAD OR THERE IS A CODE ERROR. PLEASE CHANGE RELOAD TO SOMETHING LIKE "EXTERNAL  55  REWIND"', MSG2=MSG)
                     END IF
    CASE( 'LOAD_NEXT' )!-----------------------------------------------------------------------------------------------------------------------
                       !
                       IF(IU==Z) THEN
                            IF(PRESENT(OLD_IU))  IU = OLD_IU
                            IF(ALLOW_ERROR .AND. (IU==Z .OR. .NOT. PRESENT(OLD_IU))) THEN
                                CALL STOP_ERROR( LINE=LN, INFILE=ERROR_IU, OUTPUT=IOUT, MSG= 'ULOAD FOUND KEYWORD "LOAD_NEXT" BUT THERE WAS NOT A PREVIOUSLY OPENED FILE TO USE OR THIS INPUT FEATURE DOES NOT SUPPORT THE "LOAD_NEXT" KEYWORD (viz. SFAC DOES NOT SUPPORT IT).'//NL//'PLEASE MAKE SURE THAT THIS ULOAD WAS PRECEDED BY AN EXTERNAL UNIT, DATAUNIT UNIT, OR DATAFILE FILE'//NL//'SO THT THE FILE CAN BE CONTINUED TO LOAD NEXT INPUT'//NL//'OR CHANGE KEYWORD TO ONE THAT OPENS A FILE OR POINTS TO AN EXISTING OPENED FILE.'//NL//'(e.g. THIS KEYWORD SHOULD ONLY APPEAR IN A TFR AFTER AN EXTERNAL, DATAUNIT, or DATAFILE KEYWORDS TO REUSE THEIR UNIT NUMBERS AND CONTINUE LOADING FROM THEIR FILES)', MSG2=MSG)
                            END IF
                            !
                            IF(IU /= Z) THEN
                                                  CALL CHECK_FOR_POST_KEY(LLOC,LN,IN,IOUT,ISTART,ISTOP,SFAC_FILE) !ISTART, ISTOP ARE PLACE HOLDERS FOR BUF AND ISPLIT
                                                  !
                                                  INQUIRE(IU,FORM=FORM_CHK, OPENED=KEEP_IU)
                                                  IF(.NOT. KEEP_IU) CALL STOP_ERROR(LINE=LN, INFILE=IN, OUTPUT=IOUT, MSG= 'ULOAD ERROR: RECIEVED UNIT NUMBER "'//NUM2STR(IU)//'" WHICH IS NOT ASSOCIATED WITH A FILE. THIS COULD BE THE RESULT OF USING "EXTERNAL" OR "DATAUNIT" WITH A UNIT THAT IS NOT OPENED BY THE NAME FILE.', MSG2=MSG)
                                                  BIN = FORM_CHK /= 'FORMATTED'
                                                  !
                                                  ERROR_IU = IU
                                                  KEEP_IU  = TRUE
                                                  LLOC     = ONE
                                                  IF(.NOT. ALLOW_ERROR) CLEAR_IU = TRUE
                                                  IF(.NOT. BIN) CALL READ_TO_DATA(LN,IU,IOUT)  !UNIT IS BEING PASSED IN SO READ IN LINE FROM IT
                            END IF
                       ELSE
                            CALL STOP_ERROR( LINE=LN, INFILE=ERROR_IU, OUTPUT=IOUT, MSG= 'ULOAD FOUND KEYWORD "LOAD_NEXT" BUT THIS INPUT OPTION EITHER DOES NOT SUPPORT RELOAD OR THERE IS A CODE ERROR. PLEASE CHANGE LOAD_NEXT TO SOMETHING LIKE "DATAFILE MyFile.txt"', MSG2=MSG)
                       END IF
    END SELECT
    !
    IF(IU==Z) THEN
          ALLOCATE(FL)
          !
          CALL FL%OPEN(LN,LLOC,IOUT,IN,NOSTOP=TRUE,KEY_FAIL_STOP=TRUE,REQKEY=TRUE,BINARY=BIN, MSG=MSG)
          !
          SFAC_FILE = FL%SCALE
          FL%SCALE = UNO
          !
          BIN = FL%BINARY
          IF(.NOT. FL%ERROR) THEN
                                 IU = FL%IU
                                 IF(IU /= Z) ERROR_IU = IU
                                 !
                                 IF(.NOT. FL%IS_INTERNAL .AND. .NOT. FL%OPENCLOSE) KEEP_IU = TRUE  !FOUND EXTERNAL KEYWORD, SO PASS BACK WHAT WAS LOADED
                                 !
                                 IF(FL%IS_INTERNAL .AND. NO_INTERN) CALL STOP_ERROR( LINE=LN, INFILE=ERROR_IU, OUTPUT=IOUT, MSG= 'ULOAD ERROR: THIS SPECIFIC INPUT LINE DOES NOT ALLOW FOR THE "INTERNAL" KEYWORD. PLEASE MOVE INTERNAL INPUT TO SEPARATE FILE AND CHANGE KEYWORD TO "OPEN/CLOSE" OR "EXTERNAL".', MSG2=MSG)
                                 IF(FL%IS_INTERNAL) IU = INFILE
                                 !
                                 LLOC = ONE
                                 IF(.NOT. BIN) CALL READ_TO_DATA(LN,IU,IOUT)
          ELSE !NO KEYWORD FOUND, BUT THIS IS REQUIRED FOR 2D
              !
              IF(ALLOW_ERROR) THEN
                     CALL STOP_ERROR( LINE=LN, INFILE=IU, OUTPUT=IOUT, MSG= 'ULOAD ERROR: 2D ARRAYS CANNOT BE LOADED WITH IMPLIED INTERNAL. PLEASE USE A KEYWORD TO INDICATE WHERE DATA IS LOCATED (e.g INTERNAL|EXTERNAL|OPEN/CLOSE)', MSG2=MSG)
              ELSEIF(CLEAR_IU) THEN
                     IU = Z
                     LLOC = Z
              END IF
              RETURN
          END IF
    END IF
    !
    IF(.NOT. BIN .AND. IU /= Z) THEN
                                DO !CHECK FOR KEYWORD OF SFAC WITHIN LIST
                                      N = LLOC
                                      CALL GET_WORD(LN, LLOC, ISTART, ISTOP, EXT, COM_STOP=TRUE)
                                      IF (EXT == 'SFAC') THEN
                                          CALL SFAC_READ_AND_RETURN(SF, LN(N:), IU, IOUT, ROW_WORD, DIM1, COL_WORD, DIM2, EX1_WORD=EX1_WORD, EX1_DIM=EX1_DIM, EX2_WORD=EX2_WORD, EX2_DIM=EX2_DIM, EX3_WORD=EX3_WORD, EX3_DIM=EX3_DIM)
                                          LLOC = ONE
                                          CALL READ_TO_DATA(LN,IU,IOUT)
                                      ELSE
                                          LLOC = N
                                          EXIT
                                      END IF
                                END DO
    END IF
    !
    IF(BIN) THEN
                !
                IF(IU==INFILE)                             CALL STOP_ERROR( LINE=LN, INFILE=ERROR_IU, OUTPUT=IOUT, MSG= 'ULOAD ERROR: "INTERNAL" AND "BINARY" KEYWORD FOUND, BUT THIS IS NOT ALLOWED!', MSG2=MSG)
                IF(PRESENT(NO_BINARY)) THEN; IF(NO_BINARY) CALL STOP_ERROR( LINE=LN, INFILE=ERROR_IU, OUTPUT=IOUT, MSG= 'ULOAD ERROR: "BINARY" KEYWORD FOUND, BUT THIS INPUT PROPERTY DOES NOT SUPPORT BINARY FILE INPUT. PLEASE USE ASCII/TEXT INPUT INSTEAD AND REMOVE "BINARY" KEYWORD.', MSG2=MSG)
                END IF
                !
                SELECT TYPE (VAR)
                TYPE IS (REAL(REAL64));     READ(IU, IOSTAT=IERR) VAR   ! DOUBLE PRECISION
                TYPE IS (REAL(REAL32));     READ(IU, IOSTAT=IERR) VAR   ! SINGLE PRECISION
                TYPE IS (INTEGER);          READ(IU, IOSTAT=IERR) VAR
                TYPE IS (CHARACTER(*));     READ(IU, IOSTAT=IERR) VAR
                TYPE IS (DATE_OPERATOR);    READ(IU, IOSTAT=IERR) VAR
                TYPE IS (REAL(REAL128));    READ(IU, IOSTAT=IERR) VAR   ! QUAD PRECISION
                END SELECT
                !
                IF(IERR /= Z .AND. ALLOW_ERROR)   CALL FILE_IO_ERROR( IERR, UNIT=ERROR_IU, LINE=LN, INFILE=IN, OUTPUT=IOUT, MSG= 'ATTEMPT MADE TO READ AN ARRAY WITH ULOAD IN A BINARY FILE, BUT IT FAILED TO LOAD. PLEASE CHECK THAT INPUT FORMAT IS IN THE EXPECTED BINARY STRUCTURE AND ORDERED CORRECTLY', MSG2=MSG)
                IF(IERR /= Z .AND. CLEAR_IU)  IU = Z
    ELSE
        IF(READ_BY_DIM1) THEN
                             DO I=1, DIM2
                                            IF(I>ONE) CALL READ_TO_DATA(LN,IU,IOUT)
                                            IF(LN=='') THEN
                                                IERR = -1
                                                EXIT
                                            END IF
                                            BACKSPACE(IU)
                                            IF(READ_ID) THEN
                                                            SELECT TYPE (VAR)
                                                            TYPE IS (REAL(REAL64));     READ(IU, *, IOSTAT=IERR) IID, VAR(:,I)   ! DOUBLE PRECISION
                                                            TYPE IS (REAL(REAL32));     READ(IU, *, IOSTAT=IERR) IID, VAR(:,I)   ! SINGLE PRECISION
                                                            TYPE IS (INTEGER);          READ(IU, *, IOSTAT=IERR) IID, VAR(:,I)
                                                            TYPE IS (CHARACTER(*))
                                                                                        READ(IU, '(A)', IOSTAT=IERR) LN
                                                                                        LLOC=ONE
                                                                                        CALL GET_INTEGER(LN,LLOC,ISTART,ISTOP,IOUT,IU,IID,MSG='NOSTOP');  IF(IID==inf_I) IERR = TWO  !FLAG ERROR
                                                                                        DO J=ONE, DIM1
                                                                                             CALL PARSE_WORD(LN,LLOC,ISTART,ISTOP,COM_STOP=TRUE)
                                                                                             VAR(J,I) = LN(ISTART:ISTOP)
                                                                                             IF(LINELEN<LEN_TRIM(LN(ISTART:ISTOP))) NEG_LLOC = TRUE
                                                                                        END DO
                                                            TYPE IS (DATE_OPERATOR);    READ(IU, *, IOSTAT=IERR) IID, VAR(:,I)
                                                            TYPE IS (REAL(REAL128));    READ(IU, *, IOSTAT=IERR) IID, VAR(:,I)   ! QUAD PRECISION
                                                            END SELECT
                                                            !
                                                            IF(SET_ID)  ID(I) = IID
                                            ELSE
                                                            SELECT TYPE (VAR)
                                                            TYPE IS (REAL(REAL64));     READ(IU, *, IOSTAT=IERR) VAR(:,I)    ! DOUBLE PRECISION
                                                            TYPE IS (REAL(REAL32));     READ(IU, *, IOSTAT=IERR) VAR(:,I)    ! SINGLE PRECISION
                                                            TYPE IS (INTEGER);          READ(IU, *, IOSTAT=IERR) VAR(:,I)
                                                            TYPE IS (CHARACTER(*))
                                                                                        READ(IU, '(A)', IOSTAT=IERR) LN
                                                                                        LLOC=ONE
                                                                                        DO J=ONE, DIM1
                                                                                             CALL PARSE_WORD(LN,LLOC,ISTART,ISTOP,COM_STOP=TRUE)
                                                                                             VAR(J,I) = LN(ISTART:ISTOP)
                                                                                             IF(LINELEN<LEN_TRIM(LN(ISTART:ISTOP))) NEG_LLOC = TRUE
                                                                                        END DO
                                                            TYPE IS (DATE_OPERATOR);    READ(IU, *, IOSTAT=IERR) VAR(:,I)
                                                            TYPE IS (REAL(REAL128));    READ(IU, *, IOSTAT=IERR) VAR(:,I)   ! QUAD PRECISION
                                                            END SELECT
                                            END IF
                                            !
                                            IF(IERR /= Z)  EXIT
                             END DO
        ELSE !READ_BY_DIM2
                             DO I=1, DIM1
                                            IF(I>ONE) CALL READ_TO_DATA(LN,IU,IOUT)
                                            IF(LN=='') THEN
                                                IERR = -1
                                                EXIT
                                            END IF
                                            BACKSPACE(IU)
                                            IF(READ_ID) THEN
                                                            SELECT TYPE (VAR)
                                                            TYPE IS (REAL(REAL64));     READ(IU, *, IOSTAT=IERR) IID, VAR(I,:)  ! DOUBLE PRECISION
                                                            TYPE IS (REAL(REAL32));     READ(IU, *, IOSTAT=IERR) IID, VAR(I,:)  ! SINGLE PRECISION
                                                            TYPE IS (INTEGER);          READ(IU, *, IOSTAT=IERR) IID, VAR(I,:)
                                                            TYPE IS (CHARACTER(*))
                                                                                        READ(IU, '(A)', IOSTAT=IERR) LN
                                                                                        LLOC=ONE
                                                                                        CALL GET_INTEGER(LN,LLOC,ISTART,ISTOP,IOUT,IU,IID,MSG='NOSTOP');  IF(IID==inf_I) IERR = TWO  !FLAG ERROR
                                                                                        DO J=ONE, DIM2
                                                                                             CALL PARSE_WORD(LN,LLOC,ISTART,ISTOP,COM_STOP=TRUE)
                                                                                             VAR(I,J) = LN(ISTART:ISTOP)
                                                                                             IF(LINELEN<LEN_TRIM(LN(ISTART:ISTOP))) NEG_LLOC = TRUE
                                                                                        END DO
                                                            TYPE IS (DATE_OPERATOR);    READ(IU, *, IOSTAT=IERR) IID, VAR(I,:)
                                                            TYPE IS (REAL(REAL128));    READ(IU, *, IOSTAT=IERR) IID, VAR(I,:)   ! QUAD PRECISION
                                                            END SELECT
                                                            !
                                                            IF(SET_ID)  ID(I) = IID
                                            ELSE
                                                            SELECT TYPE (VAR)
                                                            TYPE IS (REAL(REAL64));     READ(IU, *, IOSTAT=IERR) VAR(I,:)   ! DOUBLE PRECISION
                                                            TYPE IS (REAL(REAL32));     READ(IU, *, IOSTAT=IERR) VAR(I,:)   ! SINGLE PRECISION
                                                            TYPE IS (INTEGER);          READ(IU, *, IOSTAT=IERR) VAR(I,:)
                                                            TYPE IS (CHARACTER(*))
                                                                                        READ(IU, '(A)', IOSTAT=IERR) LN
                                                                                        LLOC=ONE
                                                                                        DO J=ONE, DIM2
                                                                                             CALL PARSE_WORD(LN,LLOC,ISTART,ISTOP,COM_STOP=TRUE)
                                                                                             VAR(I,J) = LN(ISTART:ISTOP)
                                                                                             IF(LINELEN<LEN_TRIM(LN(ISTART:ISTOP))) NEG_LLOC = TRUE
                                                                                        END DO
                                                            TYPE IS (DATE_OPERATOR);    READ(IU, *, IOSTAT=IERR) VAR(I,:)
                                                            TYPE IS (REAL(REAL128));    READ(IU, *, IOSTAT=IERR) VAR(I,:)   ! QUAD PRECISION
                                                            END SELECT
                                            END IF
                                            !
                                            IF(IERR /= Z)  EXIT
                             END DO
        END IF
        !IF(READ_BY_DIM1) THEN
        !                     DO I=1, DIM2
        !                                    IF(I>ONE) CALL READ_TO_DATA(LN,IU,IOUT)
        !                                    LLOC = ONE
        !                                    IF(READ_ID) THEN
        !                                          CALL URWORD(LN,LLOC,ISTART,ISTOP,Z,N,R,IOUT,IU)
        !                                          IF(PRESENT(ID)) THEN
        !                                                         READ(LN(ISTART:ISTOP), *, IOSTAT=IERR)      ID(I)
        !                                                                                       IF(IERR /= Z) ID(I) = Z
        !                                          END IF
        !                                    END IF
        !                                    !
        !                                    SELECT TYPE (VAR)
        !                                    TYPE IS (DOUBLE PRECISION); READ(LN(LLOC:), *, IOSTAT=IERR) VAR(:,I)
        !                                    TYPE IS (REAL);             READ(LN(LLOC:), *, IOSTAT=IERR) VAR(:,I)
        !                                    TYPE IS (INTEGER);          READ(LN(LLOC:), *, IOSTAT=IERR) VAR(:,I)
        !                                    TYPE IS (CHARACTER(*))
        !                                                                DO J=ONE, DIM1
        !                                                                     CALL URWORD(LN,LLOC,ISTART,ISTOP,Z,N,R,IOUT,IU)
        !                                                                     VAR(J,I) = LN(ISTART:ISTOP)
        !                                                                END DO
        !                                    END SELECT
        !                                    IF(IERR /= Z)  EXIT
        !                     END DO
        !ELSE !READ_BY_DIM2
        !                     DO I=1, DIM1
        !                                    IF(I>ONE) CALL READ_TO_DATA(LN,IU,IOUT)
        !                                    LLOC = ONE
        !                                    IF(READ_ID) THEN
        !                                          CALL URWORD(LN,LLOC,ISTART,ISTOP,Z,N,R,IOUT,IU)
        !                                          IF(PRESENT(ID)) THEN
        !                                                         READ(LN(ISTART:ISTOP), *, IOSTAT=IERR)      ID(I)
        !                                                                                       IF(IERR /= Z) ID(I) = Z
        !                                          END IF
        !                                    END IF
        !                                    !
        !                                    SELECT TYPE (VAR)
        !                                    TYPE IS (DOUBLE PRECISION); READ(LN(LLOC:), *, IOSTAT=IERR) VAR(I,:)
        !                                    TYPE IS(REAL);              READ(LN(LLOC:), *, IOSTAT=IERR) VAR(I,:)
        !                                    TYPE IS (INTEGER);          READ(LN(LLOC:), *, IOSTAT=IERR) VAR(I,:)
        !                                    TYPE IS (CHARACTER(*))
        !                                                                DO J=ONE, DIM2
        !                                                                    CALL URWORD(LN,LLOC,ISTART,ISTOP,Z,N,R,IOUT,IU)
        !                                                                    VAR(I,J) = LN(ISTART:ISTOP)
        !                                                                END DO
        !                                    END SELECT
        !                                    IF(IERR /= Z)  EXIT
        !                     END DO
        !END IF
        !
        IF(IERR /= Z .AND. ALLOW_ERROR) THEN
               IF(NO_MAIN_KEY) THEN
                    CALL FILE_IO_ERROR( IERR, UNIT=ERROR_IU, LINE=LN, INFILE=IN, OUTPUT=IOUT, MSG= 'Attempt made to read list from uload on line, but it failed to load the following value "'//LN(ISTART:ISTOP)//'"'//BLN//'Note that no keyword was found on the previous line, so code automoved downward to next line.'//NL//'You maybe missing a keyword (viz. INTERNAL, EXTERNAL, OPEN/CLOSE, DATEFILE, DATAUNIT).', MSG2=MSG)
               ELSE
                    SELECT TYPE (VAR)
                    TYPE IS (DATE_OPERATOR);           CALL FILE_IO_ERROR( IERR, UNIT=ERROR_IU, LINE=LN, INFILE=IN, OUTPUT=IOUT, MSG= 'Attempt made to read row of dates from ULOAD ON LINE, BUT IT FAILED TO CONVERT THE FOLLOWING TO A CALENDAR DATE "'//LN(ISTART:ISTOP)//'"'//BLN//'The accepted date formats are "mm/dd/YYYY" OR "YYYY-mm-dd" if you want to add a 24-Hour time to it you must add to the date "Thh:mm:ss" (e.g. "YYYY-mm-ddThh:mm:ss")', MSG2=MSG)
                    TYPE IS (GENERIC_INPUT_FILE);      CALL FILE_IO_ERROR( IERR, UNIT=ERROR_IU, LINE=LN, INFILE=IN, OUTPUT=IOUT, MSG= 'Attempt made to open row of files with ULOAD, but it failed to either open, find the file, or is not set up correctly. The following is the file that was attempted to be opened "'//LN(ISTART:ISTOP)//'".', MSG2=MSG)
                    CLASS DEFAULT
                                                       CALL FILE_IO_ERROR( IERR, UNIT=ERROR_IU, LINE=LN, INFILE=IN, OUTPUT=IOUT, MSG= 'Attempt made to read a row of numbers from ULOAD on line, but it failed to load.'//NL//' (Note that error code -1 may indicate that you do not have enough numbers on the line'//NL//'  e.g. expects 5 numbers but only 4 numbers on line'//NL//'  or if line is empty/blank then you may have reached the end of file prematurely.)', MSG2=MSG)
                    END SELECT

               END IF
        ELSEIF(IERR /= Z .AND. CLEAR_IU) THEN
               IU = Z
               LLOC = Z
        END IF
        !
    END IF
    !
    IF(SFAC_FILE /= UNO) CALL SF%SET_ALL(SFAC_FILE)
    !
    IF(PRESENT(SFAC)) THEN
                           SFAC = SF
    ELSE
                           SELECT TYPE (VAR)
                           TYPE IS (REAL(REAL64));  CALL SF%APPLY(VAR)   ! DOUBLE PRECISION
                           TYPE IS (REAL(REAL32));  CALL SF%APPLY(VAR)   ! SINGLE PRECISION
                           TYPE IS (REAL(REAL128)); CALL SF%APPLY(VAR)   ! QUAD PRECISION
                           END SELECT
    END IF
    !
    IF(.NOT. KEEP_IU)    IU     = Z
    IF(NEG_LLOC)         LLOC   = NEG
    IF(PRESENT(OLD_IU))  OLD_IU = IU
    !
    IF(ALLOCATED(FL))  DEALLOCATE(FL)
    !
  END SUBROUTINE
  !
  SUBROUTINE ULOAD_BLOCK_READER_VECTOR(VAR, LLOC, BL, NOID, BINARY, ID, ROW_WORD, NOSTOP, SFAC, EX1_WORD, EX1_DIM, EX2_WORD, EX2_DIM, EX3_WORD, EX3_DIM, SCRATCH, ENTIRE_LINE, NO_INTERNAL, NO_BINARY, NO_REPEAT, OLD_IU, TEXT, MSG)
    CLASS(*),         DIMENSION(:), CONTIGUOUS,      INTENT(INOUT):: VAR       !Variable to set by ULOAD
    INTEGER,                                         INTENT(INOUT):: LLOC      !SET TO -1 IF CHARACTER::VAR IS NOT LONG ENOUGH TO HOLD INPUT
    TYPE(GENERIC_BLOCK_READER),                      INTENT(INOUT):: BL        !Block reader that will be read from
    !
    LOGICAL,     OPTIONAL,                           INTENT(IN   ):: NOID, BINARY, NOSTOP
    INTEGER,     OPTIONAL, DIMENSION(:), ALLOCATABLE,INTENT(INOUT):: ID                   !Note that ID must be ALLOCATABLE -- This is done to check if it is read
    CHARACTER(*),OPTIONAL,                           INTENT(IN   ):: ROW_WORD
    TYPE(SFAC_DATA),OPTIONAL,                        INTENT(  OUT):: SFAC
    !
    CHARACTER(*), OPTIONAL,                          INTENT(IN   ):: EX1_WORD, EX2_WORD, EX3_WORD
    INTEGER,      OPTIONAL,                          INTENT(IN   ):: EX1_DIM,  EX2_DIM,  EX3_DIM
    INTEGER,      OPTIONAL,                          INTENT(IN   ):: SCRATCH
    LOGICAL,      OPTIONAL,                          INTENT(IN   ):: ENTIRE_LINE, NO_INTERNAL, NO_BINARY, NO_REPEAT
    INTEGER,      OPTIONAL,                          INTENT(INOUT):: OLD_IU
    CHARACTER(*), OPTIONAL,                          INTENT(IN   ):: TEXT
    CHARACTER(*), OPTIONAL,                          INTENT(IN   ):: MSG
    INTEGER:: POS, NLINE_PRINT, IU, IU_BAK
    !
    BL%LN(:) = BL%LINE  !Copy list pointer BL%LINE to regular string for safety
    IU = Z
    !
    IF(RET_WORD(BL%LINE, LLOC) == 'INTERNAL' .AND. .NOT. PRESENT(SCRATCH)) THEN  !CHECK IF THERE IS THE INTERNAL KEYWORD (ULOAD NEEDS A FILE TO READ INTERNAL FROM)
        !
        NLINE_PRINT = UBOUND(VAR, 1)
        !
        CALL  BL%NEXT()          !Move past header line
        POS = BL%LINE_NUM()      !Get current line number to restore after searching for SFAC
        !
        DO WHILE(RET_WORD(BL%LINE) == "SFAC")
            CALL BL%NEXT()
            NLINE_PRINT = NLINE_PRINT + ONE
        END DO
        CALL BL%GOTO_LINE(POS)
        !
        IU_BAK = BL%SCRATCH
        BL%SCRATCH = Z
        CALL BL%MAKE_SCRATCH_FILE(NLINE_PRINT)
        !
        CALL ULOAD_VECTOR(VAR, LLOC, BL%LN, BL%IOUT, BL%IU, IU, NOID, BINARY, ID, ROW_WORD, NOSTOP, SFAC, EX1_WORD, EX1_DIM, EX2_WORD, EX2_DIM, EX3_WORD, EX3_DIM, BL%SCRATCH, ENTIRE_LINE, NO_INTERNAL, NO_BINARY, NO_REPEAT, OLD_IU, TEXT, MSG)
        !
        CALL BL%CLOSE_SCRATCH()
        BL%SCRATCH = IU_BAK 
    ELSE
        CALL ULOAD_VECTOR(VAR, LLOC, BL%LN, BL%IOUT, BL%IU, IU, NOID, BINARY, ID, ROW_WORD, NOSTOP, SFAC, EX1_WORD, EX1_DIM, EX2_WORD, EX2_DIM, EX3_WORD, EX3_DIM, SCRATCH, ENTIRE_LINE, NO_INTERNAL, NO_BINARY, NO_REPEAT, OLD_IU, TEXT, MSG)
    END IF
    !
  END SUBROUTINE
  !
  RECURSIVE SUBROUTINE ULOAD_BLOCK_READER_ARRAY(VAR, LLOC, BL, NOID, BINARY, ID, ROW_WORD, COL_WORD, NOSTOP, SFAC, EX1_WORD, EX1_DIM, EX2_WORD, EX2_DIM, EX3_WORD, EX3_DIM, READ_BY_DIM2, SCRATCH, NO_INTERNAL, NO_BINARY, NO_REPEAT, OLD_IU, MSG)
    CLASS(*), DIMENSION(:,:), CONTIGUOUS,            INTENT(INOUT):: VAR       !Variable to set by ULOAD
    INTEGER,                                         INTENT(INOUT):: LLOC      !SET TO -1 IF CHARACTER::VAR IS NOT LONG ENOUGH TO HOLD INPUT
    TYPE(GENERIC_BLOCK_READER),                      INTENT(INOUT):: BL        !Block reader that will be read from
    !
    LOGICAL,     OPTIONAL,                           INTENT(IN   ):: NOID, BINARY, NOSTOP, READ_BY_DIM2
    INTEGER,     OPTIONAL, DIMENSION(:), ALLOCATABLE,INTENT(INOUT):: ID                   !Note that ID must be ALLOCATABLE -- This is done to check if it is read
    CHARACTER(*),OPTIONAL,                           INTENT(IN   ):: ROW_WORD, COL_WORD
    TYPE(SFAC_DATA),OPTIONAL,                        INTENT(  OUT):: SFAC
    !
    CHARACTER(*), OPTIONAL,                          INTENT(IN   ):: EX1_WORD, EX2_WORD, EX3_WORD
    INTEGER,      OPTIONAL,                          INTENT(IN   ):: EX1_DIM,  EX2_DIM,  EX3_DIM
    INTEGER,      OPTIONAL,                          INTENT(IN   ):: SCRATCH
    LOGICAL,      OPTIONAL,                          INTENT(IN   ):: NO_INTERNAL, NO_BINARY, NO_REPEAT
    INTEGER,      OPTIONAL,                          INTENT(INOUT):: OLD_IU
    CHARACTER(*), OPTIONAL,                          INTENT(IN   ):: MSG
    INTEGER:: POS, NLINE_PRINT, IU, IU_BAK
    !
    BL%LN(:) = BL%LINE  !Copy list pointer BL%LINE to regular string for safety
    IU = Z
    !
    IF(RET_WORD(BL%LINE, LLOC) == 'INTERNAL' .AND. .NOT. PRESENT(SCRATCH)) THEN  !CHECK IF THERE IS THE INTERNAL KEYWORD (ULOAD NEEDS A FILE TO READ INTERNAL FROM)
        !
        NLINE_PRINT = UBOUND(VAR, 2)
        IF(PRESENT(READ_BY_DIM2)) THEN
                IF(READ_BY_DIM2) NLINE_PRINT = UBOUND(VAR, 1)
        END IF
        !
        CALL  BL%NEXT()          !Move past header line
        POS = BL%LINE_NUM()      !Get current line number to restore after searching for SFAC
        !
        DO WHILE(RET_WORD(BL%LINE) == "SFAC")
            CALL BL%NEXT()
            NLINE_PRINT = NLINE_PRINT + ONE
        END DO
        CALL BL%GOTO_LINE(POS)
        !
        IU_BAK = BL%SCRATCH
        BL%SCRATCH = Z
        CALL BL%MAKE_SCRATCH_FILE(NLINE_PRINT)
        !
        CALL ULOAD_ARRAY(VAR, LLOC, BL%LN, BL%IOUT, BL%IU, IU, NOID, BINARY, ID, ROW_WORD, COL_WORD, NOSTOP, SFAC, EX1_WORD, EX1_DIM, EX2_WORD, EX2_DIM, EX3_WORD, EX3_DIM, READ_BY_DIM2, BL%SCRATCH, NO_INTERNAL, NO_BINARY, NO_REPEAT, OLD_IU, MSG)
        !
        CALL BL%CLOSE_SCRATCH()
        BL%SCRATCH = IU_BAK 
    ELSE
        CALL ULOAD_ARRAY(VAR, LLOC, BL%LN, BL%IOUT, BL%IU, IU, NOID, BINARY, ID, ROW_WORD, COL_WORD, NOSTOP, SFAC, EX1_WORD, EX1_DIM, EX2_WORD, EX2_DIM, EX3_WORD, EX3_DIM, READ_BY_DIM2, SCRATCH, NO_INTERNAL, NO_BINARY, NO_REPEAT, OLD_IU, MSG)
    END IF
    !
  END SUBROUTINE
  !
END MODULE
