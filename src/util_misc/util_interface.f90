!
! CODE DEVELOPED BY SCOTT E BOYCE 
!                   CONTACT <seboyce@usgs.gov> or <Boyce@engineer.com>
!
! MODULE LISTING:
!   UTIL_INTERFACE
!                           FUNCTIONS
!                                    TO_SNGL
!                                    ZERO_OR_GREATER, ZERO_OR_LESS
!                                    NEAR_ZERO, NOT_NEAR_ZERO, SET_NEAR_ZERO
!                                    DIM_OVERFLOW
!                                    DIM_MISMATCH
!                                    
!                           SUBROUTINES
!                                    MAKE_ZERO_IF_POS, MAKE_ZERO_IF_NEG
!                                    LRC_TO_CELLID(ID, LAY, ROW, COL, NLAY, NROW, NCOL)
!                                    CELLID_TO_LRC(ID, LAY, ROW, COL, NLAY, NROW, NCOL)
!                                    VEC_ADJUST_MAXSUM
!                                    REDUCE_SUM_BY
!
MODULE UTIL_INTERFACE
  !  
  USE CONSTANTS
  USE ARRAY_DATA_TYPES,          ONLY: CHARACTER_TYPE, INTEGER_VECTOR
  !
  USE, INTRINSIC:: IEEE_ARITHMETIC, ONLY: IEEE_VALUE, IEEE_QUIET_NAN, IEEE_IS_NAN
  USE, INTRINSIC:: ISO_FORTRAN_ENV, ONLY: REAL32, REAL64, REAL128, INT8, INT16, INT32, INT64
  !
  IMPLICIT NONE
  !
  PRIVATE
  PUBLIC:: CELLID_TO_LRC, LRC_TO_CELLID
  PUBLIC:: TO_SNGL
  PUBLIC:: DIM_MISMATCH, DIM_OVERFLOW
  PUBLIC:: NEAR_ZERO, NOT_NEAR_ZERO, MAKE_ZERO_IF_POS, MAKE_ZERO_IF_NEG, ONLY_ZERO_TO_ONE_RANGE, SET_NEAR_ZERO
  PUBLIC:: ZERO_OR_GREATER, ZERO_OR_LESS
  PUBLIC:: VEC_ADJUST_MAXSUM, REDUCE_SUM_BY
  !
  !---------------------------------------------------------------------------------------------------------------------
  !
  INTERFACE DIM_MISMATCH
    MODULE PROCEDURE DIM_MISMATCH_1D !(VEC,N)
    MODULE PROCEDURE DIM_MISMATCH_2D !(ARR,N,M)
  END INTERFACE
  !
  INTERFACE DIM_OVERFLOW
    MODULE PROCEDURE DIM_OVERFLOW_1D !(VEC,N)
    MODULE PROCEDURE DIM_OVERFLOW_2D !(ARR,N,M)
  END INTERFACE
  !
  INTERFACE TO_SNGL
    MODULE PROCEDURE SNGL_TO_SNGL!(X)
    MODULE PROCEDURE DBLE_TO_SNGL!(X)
    MODULE PROCEDURE QUAD_TO_SNGL!(X)
  END INTERFACE
  !
  CONTAINS
  !
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !  
  !  Cell ID routines
  !  
  PURE ELEMENTAL SUBROUTINE CELLID_TO_LRC(ID, LAY, ROW, COL, NLAY, NROW, NCOL)
    INTEGER, INTENT(IN ):: ID
    INTEGER, INTENT(OUT):: LAY, ROW, COL
    INTEGER, INTENT(IN ):: NLAY, NROW, NCOL
    INTEGER:: RC, I
    !
    I = ID
    RC = NCOL * NROW
    !
    LAY = I / RC
    IF (MOD(I,RC) .NE. Z) LAY = LAY + ONE
    !
    I = I - RC*(LAY-ONE)
    !
    ROW = I / NCOL
    IF (MOD(I,NCOL) .NE. Z) ROW = ROW + ONE
    !
    COL = I - NCOL*(ROW-1)
    !
  END SUBROUTINE
  !
  !#############################################################################################################################################################
  !
  PURE ELEMENTAL SUBROUTINE LRC_TO_CELLID(ID, LAY, ROW, COL, NLAY, NROW, NCOL)
    INTEGER, INTENT(OUT):: ID
    INTEGER, INTENT(IN ):: LAY, ROW, COL
    INTEGER, INTENT(IN ):: NLAY, NROW, NCOL
    !
    ID = COL + NCOL*(ROW-1) + NCOL*NROW*(LAY-1)
    !
  END SUBROUTINE
  !
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !  
  !  Zero, 0-1 Range, and Close routines
  !
  PURE ELEMENTAL FUNCTION ZERO_OR_GREATER(VAR)
    DOUBLE PRECISION, INTENT(IN):: VAR
    DOUBLE PRECISION:: ZERO_OR_GREATER
    IF(VAR < DZ) THEN
        ZERO_OR_GREATER = DZ
    ELSE
        ZERO_OR_GREATER = VAR
    END IF
  END FUNCTION
  !
  PURE ELEMENTAL FUNCTION ZERO_OR_LESS(VAR)
    DOUBLE PRECISION, INTENT(IN):: VAR
    DOUBLE PRECISION:: ZERO_OR_LESS
    IF(VAR > DZ) THEN
        ZERO_OR_LESS = DZ
    ELSE
        ZERO_OR_LESS = VAR
    END IF
  END FUNCTION
  !
  !#############################################################################################################################################################
  !
  PURE ELEMENTAL SUBROUTINE MAKE_ZERO_IF_NEG(VAR)
    DOUBLE PRECISION, INTENT(INOUT):: VAR
    !
    IF(VAR < DZ)  VAR = DZ
    !
  END SUBROUTINE
  !
  PURE ELEMENTAL SUBROUTINE MAKE_ZERO_IF_POS(VAR)
    DOUBLE PRECISION, INTENT(INOUT):: VAR
    !
    IF(VAR > DZ)  VAR = DZ
    !
  END SUBROUTINE
  !
  !#############################################################################################################################################################
  !
  PURE ELEMENTAL SUBROUTINE ONLY_ZERO_TO_ONE_RANGE(VAR)
    DOUBLE PRECISION, INTENT(INOUT):: VAR
    !
    IF    (VAR < DZ) THEN
           VAR = DZ
    ELSEIF(VAR > UNO) THEN
           VAR = UNO
    END IF
    !
  END SUBROUTINE
  !
  !#############################################################################################################################################################
  !
  PURE ELEMENTAL SUBROUTINE SET_NEAR_ZERO(VAR,TOL)
    DOUBLE PRECISION,           INTENT(INOUT):: VAR
    DOUBLE PRECISION, OPTIONAL, INTENT(IN   ):: TOL
    !
    IF(NEAR_ZERO(VAR,TOL)) VAR = DZ
    !
  END SUBROUTINE
  !
  !#############################################################################################################################################################
  !
  PURE ELEMENTAL FUNCTION NEAR_ZERO(VAR,TOL)
    DOUBLE PRECISION,           INTENT(IN):: VAR
    DOUBLE PRECISION, OPTIONAL, INTENT(IN):: TOL
    LOGICAL:: NEAR_ZERO
    !
    IF(PRESENT(TOL)) THEN
        IF(VAR > TOL) THEN
            NEAR_ZERO = FALSE
        ELSEIF(VAR < TOL*DNEG) THEN
            NEAR_ZERO = FALSE
        ELSE
            NEAR_ZERO = TRUE
        END IF
    ELSE
        IF(VAR > NEARZERO_30) THEN
            NEAR_ZERO = FALSE
        ELSEIF(VAR < NEGNEARZERO_30) THEN
            NEAR_ZERO = FALSE
        ELSE
            NEAR_ZERO = TRUE
        END IF
    END IF
    !
  END FUNCTION
  !
  !#############################################################################################################################################################
  !
  PURE ELEMENTAL FUNCTION NOT_NEAR_ZERO(VAR,TOL)
    DOUBLE PRECISION,           INTENT(IN):: VAR
    DOUBLE PRECISION, OPTIONAL, INTENT(IN):: TOL
    LOGICAL:: NOT_NEAR_ZERO
    !
    IF(PRESENT(TOL)) THEN
        IF(VAR > TOL) THEN
            NOT_NEAR_ZERO = TRUE
        ELSEIF(VAR < TOL*DNEG) THEN
            NOT_NEAR_ZERO = TRUE
        ELSE
            NOT_NEAR_ZERO = FALSE
        END IF
    ELSE
        IF(VAR > NEARZERO_30) THEN
            NOT_NEAR_ZERO = TRUE
        ELSEIF(VAR < NEGNEARZERO_30) THEN
            NOT_NEAR_ZERO = TRUE
        ELSE
            NOT_NEAR_ZERO = FALSE
        END IF
    END IF
    !
  END FUNCTION
  !
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !  
  !  Vector Max Min adjustment routines
  !
  PURE SUBROUTINE VEC_ADJUST_MAXSUM(VEC, MAXSUM, ORDER)
    DOUBLE PRECISION, DIMENSION(:),CONTIGUOUS,INTENT(INOUT):: VEC
    DOUBLE PRECISION,                         INTENT(IN   ):: MAXSUM
    INTEGER, OPTIONAL,DIMENSION(:),CONTIGUOUS,INTENT(IN   ):: ORDER
    INTEGER:: I,N
    DOUBLE PRECISION:: TOT
    !
    N = SIZE(VEC)
    TOT = SUM(VEC)
    !
    IF(PRESENT(ORDER)) THEN
        DO I=ONE, N
           ASSOCIATE(J=>ORDER(I))
                IF(TOT > MAXSUM) THEN
                    !
                    VEC(J) = VEC(J) + MAXSUM - TOT
                    !
                    IF(VEC(J) < DZ) VEC(J) = DZ
                    !
                    TOT = SUM(VEC)
                ELSE
                    EXIT
                END IF
           END ASSOCIATE
        END DO
    ELSE
        DO I=ONE, N
              IF(TOT > MAXSUM) THEN
                  !
                  VEC(I) = VEC(I) + MAXSUM - TOT
                  !
                  IF(VEC(I) < DZ) VEC(I) = DZ
                  !
                  TOT = SUM(VEC)
              ELSE
                  EXIT
              END IF
        END DO
    END IF
    !
  END SUBROUTINE
  !
  !#########################################################################################################################
  !
  SUBROUTINE REDUCE_SUM_BY(VEC, REDUCER, ORDER) !REDUCES BY THE REDUCER -- If Negative, nothing happens
    DOUBLE PRECISION, DIMENSION(:),CONTIGUOUS,INTENT(INOUT):: VEC
    DOUBLE PRECISION,                         INTENT(IN   ):: REDUCER
    INTEGER, OPTIONAL,DIMENSION(:),CONTIGUOUS,INTENT(IN   ):: ORDER
    DOUBLE PRECISION:: RED
    INTEGER:: I
    !
    IF(REDUCER > DZ) THEN
        !
        RED = DNEG*REDUCER  !MAKE NEGATIVE
        !
        IF(PRESENT(ORDER)) THEN
            DO I=ONE, SIZE(VEC)
                ASSOCIATE(J=>ORDER(I))
                                     VEC(J) = VEC(J) + RED
                                     IF( VEC(J) < DZ ) THEN
                                         RED = VEC(J) 
                                         VEC(J) = DZ
                                     ELSE
                                         !RED = DZ
                                         EXIT
                                     END IF
                END ASSOCIATE
            END DO
        ELSE
            DO I=ONE, SIZE(VEC)
                VEC(I) = VEC(I) + RED
                IF( VEC(I) < DZ ) THEN
                    RED = VEC(I) 
                    VEC(I) = DZ
                ELSE
                    !RED = DZ
                    EXIT
                END IF
            END DO
        END IF
    END IF
    !
  END SUBROUTINE
  !
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !  
  !  TO_SNGL  routines
  !
  PURE ELEMENTAL FUNCTION SNGL_TO_SNGL(X) RESULT(RES)
     REAL(REAL32), INTENT(IN):: X
     REAL(REAL32):: RES
     !
     RES = X
     !
  END FUNCTION
  !
  !#########################################################################################################################
  !
  PURE ELEMENTAL FUNCTION DBLE_TO_SNGL(X) RESULT(RES)
     REAL(REAL64), INTENT(IN):: X
     REAL(REAL32):: RES
     !
     IF    (X < SNGL_ninf) THEN
                               RES = SNGL_ninf_R
     ELSEIF(SNGL_inf < X ) THEN
                               RES = SNGL_inf_R
     ELSE
                               RES = REAL(X, REAL32)
     END IF
     !
  END FUNCTION
  !
  !#########################################################################################################################
  !
  PURE ELEMENTAL FUNCTION QUAD_TO_SNGL(X) RESULT(RES)
     REAL(REAL128), INTENT(IN):: X
     REAL(REAL32):: RES
     !
     IF    (X < REAL(SNGL_ninf,REAL128)) THEN
                                             RES = SNGL_ninf_R
     ELSEIF(REAL(SNGL_inf,REAL128) < X ) THEN
                                             RES = SNGL_inf_R
     ELSE
                                             RES = REAL(X, REAL32)
     END IF
     !
  END FUNCTION
  !
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !  
  !  MISMATCH and OVERFLOW  routine
  !
  PURE FUNCTION DIM_MISMATCH_1D(VEC,N) RESULT(TF)
    CLASS(*), DIMENSION(:,:), ALLOCATABLE, INTENT(IN):: VEC
    INTEGER, INTENT(IN):: N
    LOGICAL:: TF
    !
    IF(ALLOCATED(VEC)) THEN
                         TF = SIZE(VEC,1)==N
    ELSE
                         TF = TRUE
    END IF
  END FUNCTION
  !
  !#########################################################################################################################
  !
  PURE FUNCTION DIM_MISMATCH_2D(ARR,N,M) RESULT(TF)
    CLASS(*), DIMENSION(:,:), ALLOCATABLE, INTENT(IN):: ARR
    INTEGER, INTENT(IN):: N,M
    LOGICAL:: TF
    !
    IF(ALLOCATED(ARR)) THEN
                         TF = SIZE(ARR,1)==N .AND. SIZE(ARR,2)==M
    ELSE
                         TF = TRUE
    END IF
  END FUNCTION
  !
  !#########################################################################################################################
  !
  PURE FUNCTION DIM_OVERFLOW_1D(VEC,N) RESULT(TF)
    CLASS(*), DIMENSION(:,:), ALLOCATABLE, INTENT(IN):: VEC
    INTEGER, INTENT(IN):: N
    LOGICAL:: TF
    !
    IF(ALLOCATED(VEC)) THEN
                         TF = SIZE(VEC,1) <= N
    ELSE
                         TF = TRUE
    END IF
  END FUNCTION
  !
  !#########################################################################################################################
  !
  PURE FUNCTION DIM_OVERFLOW_2D(ARR,N,M) RESULT(TF)
    CLASS(*), DIMENSION(:,:), ALLOCATABLE, INTENT(IN):: ARR
    INTEGER, INTENT(IN):: N,M
    LOGICAL:: TF
    !
    IF(ALLOCATED(ARR)) THEN
                         TF = SIZE(ARR,1) <= N .AND. SIZE(ARR,2) <= M
    ELSE
                         TF = TRUE
    END IF
  END FUNCTION
  !
END MODULE 
!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
