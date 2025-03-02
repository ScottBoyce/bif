!
! CODE DEVELOPED BY SCOTT E BOYCE 
!                   CONTACT <seboyce@usgs.gov> or <Boyce@engineer.com>
!
!
MODULE SET_ARRAY_INTERFACE
  !
  USE, INTRINSIC:: IEEE_ARITHMETIC, ONLY: IEEE_VALUE, IEEE_QUIET_NAN!, IEEE_IS_NAN
  USE, INTRINSIC:: ISO_FORTRAN_ENV, ONLY: REAL32, REAL64, REAL128, INT8, INT16, INT32, INT64
  !
  IMPLICIT NONE
  !
  PRIVATE
  !
  PUBLIC:: SET_ARRAY, SET_ZERO, SET_NAN, SET_SEQUENCE, POSITION_SET_ARRAY
  !
  !---------------------------------------------------------------------------------------------------------------------
  !
  INTERFACE SET_ARRAY
    MODULE PROCEDURE SET_ARRAY_3D3D_DBL !SET_ARRAY(DIM1, DIM2, DIM3, ARR1, ARR2)
    MODULE PROCEDURE SET_ARRAY_3D3D_SNG !
    MODULE PROCEDURE SET_ARRAY_3D3D_INT !
    MODULE PROCEDURE SET_ARRAY_0D3D_DBL !SET_ARRAY(DIM1, DIM2, DIM3, VAL,  ARR2)
    MODULE PROCEDURE SET_ARRAY_0D3D_SNG !
    MODULE PROCEDURE SET_ARRAY_0D3D_INT !
    MODULE PROCEDURE SET_ARRAY_2D2D_DBL !SET_ARRAY(DIM1, DIM2, ARR1, ARR2) 
    MODULE PROCEDURE SET_ARRAY_2D2D_SNG !
    MODULE PROCEDURE SET_ARRAY_2D2D_INT !
    MODULE PROCEDURE SET_ARRAY_0D2D_DBL !SET_ARRAY(DIM1, DIM2,  VAL, ARR2)
    MODULE PROCEDURE SET_ARRAY_0D2D_SNG !
    MODULE PROCEDURE SET_ARRAY_0D2D_INT !
    MODULE PROCEDURE SET_ARRAY_1D1D_DBL !SET_ARRAY(DIM1, ARR1, ARR2)  
    MODULE PROCEDURE SET_ARRAY_1D1D_SNG !
    MODULE PROCEDURE SET_ARRAY_1D1D_INT !
    MODULE PROCEDURE SET_ARRAY_0D1D_DBL !SET_ARRAY(DIM1,  VAL, ARR2)
    MODULE PROCEDURE SET_ARRAY_0D1D_SNG !
    MODULE PROCEDURE SET_ARRAY_0D1D_INT !
    MODULE PROCEDURE SET_ARRAY_3D3D_DBL_SNG
    MODULE PROCEDURE SET_ARRAY_3D3D_SNG_DBL
    MODULE PROCEDURE SET_ARRAY_2D2D_DBL_SNG
    MODULE PROCEDURE SET_ARRAY_2D2D_SNG_DBL
    !
    MODULE PROCEDURE SET_ARRAY_SEQUENTIAL_1D_INT      !(DIM1, ARR2)
    MODULE PROCEDURE SET_ARRAY_SEQUENTIAL_1D_INT_START!(DIM1, ARR2, START)
    MODULE PROCEDURE SET_ARRAY_SEQUENTIAL_1D_INT_INC  !(DIM1, ARR2, START, INC)
  END INTERFACE
  !
  INTERFACE SET_ZERO
    MODULE PROCEDURE SET_ZERO_3D_DBL !SET_ZERO(DIM1, DIM2, DIM3, ARR)
    MODULE PROCEDURE SET_ZERO_3D_SNG !
    MODULE PROCEDURE SET_ZERO_3D_INT !
    MODULE PROCEDURE SET_ZERO_2D_DBL !SET_ZERO(DIM1, DIM2, ARR) 
    MODULE PROCEDURE SET_ZERO_2D_SNG !
    MODULE PROCEDURE SET_ZERO_2D_INT !
    MODULE PROCEDURE SET_ZERO_1D_DBL !SET_ZERO(DIM1, ARR)  
    MODULE PROCEDURE SET_ZERO_1D_SNG !
    MODULE PROCEDURE SET_ZERO_1D_INT !
  END INTERFACE
  !
  INTERFACE SET_NAN
    MODULE PROCEDURE SET_NAN_3D_DBL !SET_ZERO(DIM1, DIM2, DIM3, ARR)
    MODULE PROCEDURE SET_NAN_3D_SNG !
    MODULE PROCEDURE SET_NAN_2D_DBL !SET_ZERO(DIM1, DIM2, ARR) 
    MODULE PROCEDURE SET_NAN_2D_SNG !
    MODULE PROCEDURE SET_NAN_1D_DBL !SET_ZERO(DIM1, ARR)  
    MODULE PROCEDURE SET_NAN_1D_SNG !
  END INTERFACE
  !
  INTERFACE SET_SEQUENCE
    MODULE PROCEDURE SET_ARRAY_SEQUENTIAL_1D_INT      !(DIM1, ARR2)
    MODULE PROCEDURE SET_ARRAY_SEQUENTIAL_1D_INT_START!(DIM1, ARR2, START)
    MODULE PROCEDURE SET_ARRAY_SEQUENTIAL_1D_INT_INC  !(DIM1, ARR2, START, INC)
  END INTERFACE
  !
  INTERFACE POSITION_SET_ARRAY
    MODULE PROCEDURE POSITION_SET_ARRAY_2D_DBL!(DIM1, DIM2, SRCH, ARR1, SET, ARR2)
  END INTERFACE
  !
  CONTAINS
  !
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !  
  !  SET Array routines
  !
  PURE SUBROUTINE POSITION_SET_ARRAY_2D_DBL(DIM1, DIM2, SRCH, ARR1, SET, ARR2)
    INTEGER,                            INTENT(IN   ):: DIM1, DIM2, SRCH
    INTEGER,      DIMENSION(DIM1,DIM2), INTENT(IN   ):: ARR1
    REAL(REAL64),                       INTENT(IN   ):: SET
    REAL(REAL64), DIMENSION(DIM1,DIM2), INTENT(  OUT):: ARR2
    INTEGER:: I,J
    !
    DO CONCURRENT(J=1:DIM2, I=1:DIM1, ARR1(I,J)==SRCH); ARR2(I,J) = SET
    END DO
    !
  END SUBROUTINE
  !
  !#############################################################################################################################################################
  !
  PURE SUBROUTINE SET_ARRAY_3D3D_DBL(DIM1, DIM2, DIM3, ARR1, ARR2)
    INTEGER,                                 INTENT(IN   ):: DIM1, DIM2, DIM3
    REAL(REAL64), DIMENSION(DIM1,DIM2,DIM3), INTENT(IN   ):: ARR1
    REAL(REAL64), DIMENSION(DIM1,DIM2,DIM3), INTENT(  OUT):: ARR2
    INTEGER:: I,J,K
    !
    DO CONCURRENT(K=1:DIM3, J=1:DIM2, I=1:DIM1); ARR2(I,J,K) = ARR1(I,J,K)
    END DO
    !
  END SUBROUTINE
  !
  !#############################################################################################################################################################
  !
  PURE SUBROUTINE SET_ARRAY_0D3D_DBL(DIM1, DIM2, DIM3, VAL, ARR2)
    INTEGER,                                 INTENT(IN   ):: DIM1, DIM2, DIM3
    REAL(REAL64),                            INTENT(IN   ):: VAL
    REAL(REAL64), DIMENSION(DIM1,DIM2,DIM3), INTENT(  OUT):: ARR2
    INTEGER:: I,J,K
    !
    DO CONCURRENT(K=1:DIM3, J=1:DIM2, I=1:DIM1); ARR2(I,J,K) = VAL
    END DO
    !
  END SUBROUTINE
  !
  !#############################################################################################################################################################
  !
  PURE SUBROUTINE SET_ARRAY_3D3D_SNG(DIM1, DIM2, DIM3, ARR1, ARR2)
    INTEGER,                                 INTENT(IN   ):: DIM1, DIM2, DIM3
    REAL(REAL32), DIMENSION(DIM1,DIM2,DIM3), INTENT(IN   ):: ARR1
    REAL(REAL32), DIMENSION(DIM1,DIM2,DIM3), INTENT(  OUT):: ARR2
    INTEGER:: I,J,K
    !
    DO CONCURRENT(K=1:DIM3, J=1:DIM2, I=1:DIM1); ARR2(I,J,K) = ARR1(I,J,K)
    END DO
    !
  END SUBROUTINE
  !
  !#############################################################################################################################################################
  !
  PURE SUBROUTINE SET_ARRAY_0D3D_SNG(DIM1, DIM2, DIM3, VAL, ARR2)
    INTEGER,                                 INTENT(IN   ):: DIM1, DIM2, DIM3
    REAL(REAL32),                            INTENT(IN   ):: VAL
    REAL(REAL32), DIMENSION(DIM1,DIM2,DIM3), INTENT(  OUT):: ARR2
    INTEGER:: I,J,K
    !
    DO CONCURRENT(K=1:DIM3, J=1:DIM2, I=1:DIM1); ARR2(I,J,K) = VAL
    END DO
    !
  END SUBROUTINE
  !
  !#############################################################################################################################################################
  !
  PURE SUBROUTINE SET_ARRAY_3D3D_INT(DIM1, DIM2, DIM3, ARR1, ARR2)
    INTEGER,                            INTENT(IN   ):: DIM1, DIM2, DIM3
    INTEGER, DIMENSION(DIM1,DIM2,DIM3), INTENT(IN   ):: ARR1
    INTEGER, DIMENSION(DIM1,DIM2,DIM3), INTENT(  OUT):: ARR2
    INTEGER:: I,J,K
    !
    DO CONCURRENT(K=1:DIM3, J=1:DIM2, I=1:DIM1); ARR2(I,J,K) = ARR1(I,J,K)
    END DO
    !
  END SUBROUTINE
  !
  !#############################################################################################################################################################
  !
  PURE SUBROUTINE SET_ARRAY_0D3D_INT(DIM1, DIM2, DIM3, VAL, ARR2)
    INTEGER,                            INTENT(IN   ):: DIM1, DIM2, DIM3
    INTEGER,                            INTENT(IN   ):: VAL
    INTEGER, DIMENSION(DIM1,DIM2,DIM3), INTENT(  OUT):: ARR2
    INTEGER:: I,J,K
    !
    DO CONCURRENT(K=1:DIM3, J=1:DIM2, I=1:DIM1); ARR2(I,J,K) = VAL
    END DO
    !
  END SUBROUTINE
  !
  !#############################################################################################################################################################
  !
  PURE SUBROUTINE SET_ARRAY_2D2D_DBL(DIM1, DIM2, ARR1, ARR2)
    INTEGER,                            INTENT(IN   ):: DIM1, DIM2
    REAL(REAL64), DIMENSION(DIM1,DIM2), INTENT(IN   ):: ARR1
    REAL(REAL64), DIMENSION(DIM1,DIM2), INTENT(  OUT):: ARR2
    INTEGER:: I,J
    !
    DO CONCURRENT(J=1:DIM2, I=1:DIM1); ARR2(I,J) = ARR1(I,J)
    END DO
    !
  END SUBROUTINE
  !
  !#############################################################################################################################################################
  !
  PURE SUBROUTINE SET_ARRAY_0D2D_DBL(DIM1, DIM2, VAL, ARR2)
    INTEGER,                            INTENT(IN   ):: DIM1, DIM2
    REAL(REAL64),                       INTENT(IN   ):: VAL
    REAL(REAL64), DIMENSION(DIM1,DIM2), INTENT(  OUT):: ARR2
    INTEGER:: I,J
    !
    DO CONCURRENT(J=1:DIM2, I=1:DIM1); ARR2(I,J) = VAL
    END DO
    !
  END SUBROUTINE
  !
  !#############################################################################################################################################################
  !
  PURE SUBROUTINE SET_ARRAY_2D2D_SNG(DIM1, DIM2, ARR1, ARR2)
    INTEGER,                            INTENT(IN   ):: DIM1, DIM2
    REAL(REAL32), DIMENSION(DIM1,DIM2), INTENT(IN   ):: ARR1
    REAL(REAL32), DIMENSION(DIM1,DIM2), INTENT(  OUT):: ARR2
    INTEGER:: I,J
    !
    DO CONCURRENT(J=1:DIM2, I=1:DIM1); ARR2(I,J) = ARR1(I,J)
    END DO
    !
  END SUBROUTINE
  !
  !#############################################################################################################################################################
  !
  PURE SUBROUTINE SET_ARRAY_0D2D_SNG(DIM1, DIM2, VAL, ARR2)
    INTEGER,                            INTENT(IN   ):: DIM1, DIM2
    REAL(REAL32),                       INTENT(IN   ):: VAL
    REAL(REAL32), DIMENSION(DIM1,DIM2), INTENT(  OUT):: ARR2
    INTEGER:: I,J
    !
    DO CONCURRENT(J=1:DIM2, I=1:DIM1); ARR2(I,J) = VAL
    END DO
    !
  END SUBROUTINE
  !
  !#############################################################################################################################################################
  !
  PURE SUBROUTINE SET_ARRAY_3D3D_SNG_DBL(DIM1, DIM2, DIM3, ARR1, ARR2)
    INTEGER,                                 INTENT(IN   ):: DIM1, DIM2, DIM3
    REAL(REAL32), DIMENSION(DIM1,DIM2,DIM3), INTENT(IN   ):: ARR1
    REAL(REAL64), DIMENSION(DIM1,DIM2,DIM3), INTENT(  OUT):: ARR2
    INTEGER:: I,J,K
    REAL(REAL64):: NaN
    NaN = IEEE_VALUE(NaN, IEEE_QUIET_NAN)
    !
    DO CONCURRENT(K=1:DIM3, J=1:DIM2, I=1:DIM1)
        IF(ARR1(I,J,K) /= ARR1(I,J,K)) THEN                ! Set to NaN
                                  ARR2(I,J,K) = NaN
        ELSE
                                  ARR2(I,J,K) = REAL(ARR1(I,J,K), REAL64)
        END IF
    END DO
    !
  END SUBROUTINE
  !
  PURE SUBROUTINE SET_ARRAY_3D3D_DBL_SNG(DIM1, DIM2, DIM3, ARR1, ARR2)
    INTEGER,                                 INTENT(IN   ):: DIM1, DIM2, DIM3
    REAL(REAL64), DIMENSION(DIM1,DIM2,DIM3), INTENT(IN   ):: ARR1
    REAL(REAL32), DIMENSION(DIM1,DIM2,DIM3), INTENT(  OUT):: ARR2
    INTEGER:: I,J,K
    REAL(REAL32):: NaN
    NaN = IEEE_VALUE(NaN, IEEE_QUIET_NAN)
    !
    DO CONCURRENT(K=1:DIM3, J=1:DIM2, I=1:DIM1)
        IF    (ARR1(I,J,K) /= ARR1(I,J,K)  ) THEN                ! Set to NaN
                                  ARR2(I,J,K) = NaN
        ELSEIF(ARR1(I,J,K) < -3.40282e38_real64) THEN                ! Lower limit of Single Precision
                                  ARR2(I,J,K) = -3.40282e38_real32
        ELSEIF(ARR1(I,J,K) >  3.40282e38_real64) THEN
                                  ARR2(I,J,K) =  3.40282e38_real32  ! Upper limit of Single Precision
        ELSE
                                  ARR2(I,J,K) = REAL(ARR1(I,J,K), REAL32)
        END IF
    END DO
    !
  END SUBROUTINE
  !
  !#############################################################################################################################################################
  !
  PURE SUBROUTINE SET_ARRAY_2D2D_SNG_DBL(DIM1, DIM2, ARR1, ARR2)
    INTEGER,                            INTENT(IN   ):: DIM1, DIM2
    REAL(REAL32), DIMENSION(DIM1,DIM2), INTENT(IN   ):: ARR1
    REAL(REAL64), DIMENSION(DIM1,DIM2), INTENT(  OUT):: ARR2
    INTEGER:: I,J
    REAL(REAL64):: NaN
    NaN = IEEE_VALUE(NaN, IEEE_QUIET_NAN)
    !
    DO CONCURRENT(J=1:DIM2, I=1:DIM1)
        IF    (ARR1(I,J) /= ARR1(I,J)  ) THEN                 ! Set to NaN
                                  ARR2(I,J) = NaN
        ELSE
                                  ARR2(I,J) = REAL(ARR1(I,J), REAL64)
        END IF
    END DO
    !
  END SUBROUTINE
  !
  PURE SUBROUTINE SET_ARRAY_2D2D_DBL_SNG(DIM1, DIM2, ARR1, ARR2)
    INTEGER,                            INTENT(IN   ):: DIM1, DIM2
    REAL(REAL64), DIMENSION(DIM1,DIM2), INTENT(IN   ):: ARR1
    REAL(REAL32), DIMENSION(DIM1,DIM2), INTENT(  OUT):: ARR2
    INTEGER:: I,J
    REAL(REAL32):: NaN
    NaN = IEEE_VALUE(NaN, IEEE_QUIET_NAN)
    !
    DO CONCURRENT(J=1:DIM2, I=1:DIM1)
        IF    (ARR1(I,J) /= ARR1(I,J)  ) THEN                 ! Set to NaN
                                  ARR2(I,J) = NaN
        ELSEIF(ARR1(I,J) < -3.40282e38_real64) THEN               ! Lower limit of Single Precision
                                  ARR2(I,J) = -3.40282e38_real32
        ELSEIF(ARR1(I,J) >  3.40282e38_real64) THEN
                                  ARR2(I,J) =  3.40282e38_real32  ! Upper limit of Single Precision
        ELSE
                                  ARR2(I,J) = REAL(ARR1(I,J), REAL32)
        END IF
    END DO
    !
  END SUBROUTINE
  !
  !#############################################################################################################################################################
  !
  PURE SUBROUTINE SET_ARRAY_2D2D_INT(DIM1, DIM2, ARR1, ARR2)
    INTEGER,                       INTENT(IN   ):: DIM1, DIM2
    INTEGER, DIMENSION(DIM1,DIM2), INTENT(IN   ):: ARR1
    INTEGER, DIMENSION(DIM1,DIM2), INTENT(  OUT):: ARR2
    INTEGER:: I,J
    !
    DO CONCURRENT(J=1:DIM2, I=1:DIM1); ARR2(I,J) = ARR1(I,J)
    END DO
    !
  END SUBROUTINE
  !
  !#############################################################################################################################################################
  !
  PURE SUBROUTINE SET_ARRAY_0D2D_INT(DIM1, DIM2, VAL, ARR2)
    INTEGER,                       INTENT(IN   ):: DIM1, DIM2
    INTEGER,                       INTENT(IN   ):: VAL
    INTEGER, DIMENSION(DIM1,DIM2), INTENT(  OUT):: ARR2
    INTEGER:: I,J
    !
    DO CONCURRENT(J=1:DIM2, I=1:DIM1); ARR2(I,J) = VAL
    END DO
    !
  END SUBROUTINE
  !
  !#############################################################################################################################################################
  !
  PURE SUBROUTINE SET_ARRAY_1D1D_DBL(DIM1, ARR1, ARR2)
    INTEGER,                       INTENT(IN   ):: DIM1
    REAL(REAL64), DIMENSION(DIM1), INTENT(IN   ):: ARR1
    REAL(REAL64), DIMENSION(DIM1), INTENT(  OUT):: ARR2
    INTEGER:: I
    !
    DO CONCURRENT(I=1:DIM1); ARR2(I) = ARR1(I)
    END DO
    !
  END SUBROUTINE
  !
  !#############################################################################################################################################################
  !
  PURE SUBROUTINE SET_ARRAY_0D1D_DBL(DIM1, VAL, ARR2)
    INTEGER,                       INTENT(IN   ):: DIM1
    REAL(REAL64),                  INTENT(IN   ):: VAL
    REAL(REAL64), DIMENSION(DIM1), INTENT(  OUT):: ARR2
    INTEGER:: I
    !
    DO CONCURRENT(I=1:DIM1); ARR2(I) = VAL
    END DO
    !
  END SUBROUTINE
  !
  !#############################################################################################################################################################
  !
  PURE SUBROUTINE SET_ARRAY_1D1D_SNG(DIM1, ARR1, ARR2)
    INTEGER,                       INTENT(IN   ):: DIM1
    REAL(REAL32), DIMENSION(DIM1), INTENT(IN   ):: ARR1
    REAL(REAL32), DIMENSION(DIM1), INTENT(  OUT):: ARR2
    INTEGER:: I
    !
    DO CONCURRENT(I=1:DIM1); ARR2(I) = ARR1(I)
    END DO
    !
  END SUBROUTINE
  !
  !#############################################################################################################################################################
  !
  PURE SUBROUTINE SET_ARRAY_0D1D_SNG(DIM1, VAL, ARR2)
    INTEGER,                       INTENT(IN   ):: DIM1
    REAL(REAL32),                  INTENT(IN   ):: VAL
    REAL(REAL32), DIMENSION(DIM1), INTENT(  OUT):: ARR2
    INTEGER:: I
    !
    DO CONCURRENT(I=1:DIM1); ARR2(I) = VAL
    END DO
    !
  END SUBROUTINE
  !
  !#############################################################################################################################################################
  !
  PURE SUBROUTINE SET_ARRAY_1D1D_INT(DIM1, ARR1, ARR2)
    INTEGER,                  INTENT(IN   ):: DIM1
    INTEGER, DIMENSION(DIM1), INTENT(IN   ):: ARR1
    INTEGER, DIMENSION(DIM1), INTENT(  OUT):: ARR2
    INTEGER:: I
    !
    DO CONCURRENT(I=1:DIM1); ARR2(I) = ARR1(I)
    END DO
    !
  END SUBROUTINE
  !
  !#############################################################################################################################################################
  !
  PURE SUBROUTINE SET_ARRAY_0D1D_INT(DIM1, VAL, ARR2)
    INTEGER,                  INTENT(IN   ):: DIM1
    INTEGER,                  INTENT(IN   ):: VAL
    INTEGER, DIMENSION(DIM1), INTENT(  OUT):: ARR2
    INTEGER:: I
    !
    DO CONCURRENT(I=1:DIM1); ARR2(I) = VAL
    END DO
    !
  END SUBROUTINE
  !
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !
  ! Set Sequential Routines
  !
  PURE SUBROUTINE SET_ARRAY_SEQUENTIAL_1D_INT(DIM1, ARR2)
    INTEGER,                  INTENT(IN   ):: DIM1
    INTEGER, DIMENSION(DIM1), INTENT(  OUT):: ARR2
    INTEGER:: I
    !
    DO CONCURRENT(I=1:DIM1); ARR2(I) = I
    END DO
    !
  END SUBROUTINE
  !
  PURE SUBROUTINE SET_ARRAY_SEQUENTIAL_1D_INT_START(DIM1, ARR2, START)
    INTEGER,                  INTENT(IN   ):: DIM1
    INTEGER,                  INTENT(IN   ):: START
    INTEGER, DIMENSION(DIM1), INTENT(  OUT):: ARR2
    INTEGER:: I, STRT
    !
    STRT = START
    DO I=1, DIM1
                ARR2(I) = STRT
                STRT = STRT + 1
    END DO
    !
  END SUBROUTINE
  !
  !#############################################################################################################################################################
  !
  PURE SUBROUTINE SET_ARRAY_SEQUENTIAL_1D_INT_INC(DIM1, ARR2, START, INC)
    INTEGER,                  INTENT(IN   ):: DIM1
    INTEGER,                  INTENT(IN   ):: START, INC
    INTEGER, DIMENSION(DIM1), INTENT(  OUT):: ARR2
    INTEGER:: I, STRT
    !
    STRT = START
    DO I=1, DIM1
                ARR2(I) = STRT
                STRT = STRT + INC
    END DO
  END SUBROUTINE
  !
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !
  ! Set to Zero Routines
  !
  PURE SUBROUTINE SET_ZERO_3D_DBL(DIM1, DIM2, DIM3, ARR)
    INTEGER,                                 INTENT(IN   ):: DIM1, DIM2, DIM3
    REAL(REAL64), DIMENSION(DIM1,DIM2,DIM3), INTENT(  OUT):: ARR
    INTEGER:: I,J,K
    !
    DO CONCURRENT(K=1:DIM3, J=1:DIM2, I=1:DIM1); ARR(I,J,K) = 0.0_REAL64
    END DO
    !
  END SUBROUTINE
  !
  !#############################################################################################################################################################
  !
  PURE SUBROUTINE SET_ZERO_3D_SNG(DIM1, DIM2, DIM3, ARR)
    INTEGER,                                 INTENT(IN   ):: DIM1, DIM2, DIM3
    REAL(REAL32), DIMENSION(DIM1,DIM2,DIM3), INTENT(  OUT):: ARR
    INTEGER:: I,J,K
    !
    DO CONCURRENT(K=1:DIM3, J=1:DIM2, I=1:DIM1); ARR(I,J,K) = 0.0_REAL32
    END DO
    !
  END SUBROUTINE
  !
  !#############################################################################################################################################################
  !
  PURE SUBROUTINE SET_ZERO_3D_INT(DIM1, DIM2, DIM3, ARR)
    INTEGER,                            INTENT(IN   ):: DIM1, DIM2, DIM3
    INTEGER, DIMENSION(DIM1,DIM2,DIM3), INTENT(  OUT):: ARR
    INTEGER:: I,J,K
    !
    DO CONCURRENT(K=1:DIM3, J=1:DIM2, I=1:DIM1); ARR(I,J,K) = 0
    END DO
    !
  END SUBROUTINE
  !
  !#############################################################################################################################################################
  !
  PURE SUBROUTINE SET_ZERO_2D_DBL(DIM1, DIM2, ARR)
    INTEGER,                            INTENT(IN   ):: DIM1, DIM2
    REAL(REAL64), DIMENSION(DIM1,DIM2), INTENT(  OUT):: ARR
    INTEGER:: I,J
    !
    DO CONCURRENT(J=1:DIM2, I=1:DIM1); ARR(I,J) = 0.0_REAL64
    END DO
    !
  END SUBROUTINE
  !
  !#############################################################################################################################################################
  !
  PURE SUBROUTINE SET_ZERO_2D_SNG(DIM1, DIM2, ARR)
    INTEGER,                            INTENT(IN   ):: DIM1, DIM2
    REAL(REAL32), DIMENSION(DIM1,DIM2), INTENT(  OUT):: ARR
    INTEGER:: I,J
    !
    DO CONCURRENT(J=1:DIM2, I=1:DIM1); ARR(I,J) = 0.0_REAL32
    END DO
    !
  END SUBROUTINE
  !
  !#############################################################################################################################################################
  !
  PURE SUBROUTINE SET_ZERO_2D_INT(DIM1, DIM2, ARR)
    INTEGER,                            INTENT(IN   ):: DIM1, DIM2
    INTEGER, DIMENSION(DIM1,DIM2), INTENT(  OUT):: ARR
    INTEGER:: I,J
    !
    DO CONCURRENT(J=1:DIM2, I=1:DIM1); ARR(I,J) = 0
    END DO
    !
  END SUBROUTINE
  !
  !#############################################################################################################################################################
  !
  PURE SUBROUTINE SET_ZERO_1D_DBL(DIM1, ARR)
    INTEGER,                       INTENT(IN   ):: DIM1
    REAL(REAL64), DIMENSION(DIM1), INTENT(  OUT):: ARR
    INTEGER:: I
    !
    DO CONCURRENT(I=1:DIM1); ARR(I) = 0.0_REAL64
    END DO
    !
  END SUBROUTINE
  !
  !#############################################################################################################################################################
  !
  PURE SUBROUTINE SET_ZERO_1D_SNG(DIM1, ARR)
    INTEGER,                       INTENT(IN   ):: DIM1
    REAL(REAL32), DIMENSION(DIM1), INTENT(  OUT):: ARR
    INTEGER:: I
    !
    DO CONCURRENT(I=1:DIM1); ARR(I) = 0.0_REAL32
    END DO
    !
  END SUBROUTINE
  !
  !#############################################################################################################################################################
  !
  PURE SUBROUTINE SET_ZERO_1D_INT(DIM1, ARR)
    INTEGER,                  INTENT(IN   ):: DIM1
    INTEGER, DIMENSION(DIM1), INTENT(  OUT):: ARR
    INTEGER:: I
    !
    DO CONCURRENT(I=1:DIM1); ARR(I) = 0
    END DO
    !
  END SUBROUTINE
  !3
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !
  ! Set to NaN Routines
  !
  PURE SUBROUTINE SET_NAN_3D_DBL(DIM1, DIM2, DIM3, ARR)
    INTEGER,                                 INTENT(IN   ):: DIM1, DIM2, DIM3
    REAL(REAL64), DIMENSION(DIM1,DIM2,DIM3), INTENT(  OUT):: ARR
    INTEGER:: I,J,K
    REAL(REAL64):: NaN
    !
    NaN = IEEE_VALUE(NaN, IEEE_QUIET_NAN)
    !
    DO CONCURRENT(K=1:DIM3, J=1:DIM2, I=1:DIM1); ARR(I,J,K) = NaN
    END DO
    !
  END SUBROUTINE
  !
  !#############################################################################################################################################################
  !
  PURE SUBROUTINE SET_NAN_3D_SNG(DIM1, DIM2, DIM3, ARR)
    INTEGER,                                 INTENT(IN   ):: DIM1, DIM2, DIM3
    REAL(REAL32), DIMENSION(DIM1,DIM2,DIM3), INTENT(  OUT):: ARR
    INTEGER:: I,J,K
    REAL(REAL32):: NaN
    !
    NaN = IEEE_VALUE(NaN, IEEE_QUIET_NAN)
    !
    DO CONCURRENT(K=1:DIM3, J=1:DIM2, I=1:DIM1); ARR(I,J,K) = NaN
    END DO
    !
  END SUBROUTINE
  !
  !#############################################################################################################################################################
  !
  PURE SUBROUTINE SET_NAN_2D_DBL(DIM1, DIM2, ARR)
    INTEGER,                            INTENT(IN   ):: DIM1, DIM2
    REAL(REAL64), DIMENSION(DIM1,DIM2), INTENT(  OUT):: ARR
    INTEGER:: I,J
    REAL(REAL64):: NaN
    !
    NaN = IEEE_VALUE(NaN, IEEE_QUIET_NAN)
    !
    DO CONCURRENT(J=1:DIM2, I=1:DIM1); ARR(I,J) = NaN
    END DO
    !
  END SUBROUTINE
  !
  !#############################################################################################################################################################
  !
  PURE SUBROUTINE SET_NAN_2D_SNG(DIM1, DIM2, ARR)
    INTEGER,                            INTENT(IN   ):: DIM1, DIM2
    REAL(REAL32), DIMENSION(DIM1,DIM2), INTENT(  OUT):: ARR
    INTEGER:: I,J
    REAL(REAL32):: NaN
    !
    NaN = IEEE_VALUE(NaN, IEEE_QUIET_NAN)
    !
    DO CONCURRENT(J=1:DIM2, I=1:DIM1); ARR(I,J) = NaN
    END DO
    !
  END SUBROUTINE
  !
  !#############################################################################################################################################################
  !
  PURE SUBROUTINE SET_NAN_1D_DBL(DIM1, ARR)
    INTEGER,                       INTENT(IN   ):: DIM1
    REAL(REAL64), DIMENSION(DIM1), INTENT(  OUT):: ARR
    INTEGER:: I
    REAL(REAL64):: NaN
    !
    NaN = IEEE_VALUE(NaN, IEEE_QUIET_NAN)
    !
    DO CONCURRENT(I=1:DIM1); ARR(I) = NaN
    END DO
    !
  END SUBROUTINE
  !
  !#############################################################################################################################################################
  !
  PURE SUBROUTINE SET_NAN_1D_SNG(DIM1, ARR)
    INTEGER,                       INTENT(IN   ):: DIM1
    REAL(REAL32), DIMENSION(DIM1), INTENT(  OUT):: ARR
    INTEGER:: I
    REAL(REAL32):: NaN
    !
    NaN = IEEE_VALUE(NaN, IEEE_QUIET_NAN)
    !
    DO CONCURRENT(I=1:DIM1); ARR(I) = NaN
    END DO
    !
  END SUBROUTINE
  !
  !#############################################################################################################################################################
  !
END MODULE 
!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!