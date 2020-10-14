!
! CODE DEVELOPED BY SCOTT E BOYCE 
!                   CONTACT <seboyce@usgs.gov> or <Boyce@engineer.com>
!
MODULE OBS_GROUP_INTERPOLATOR  !Mimcs Hob interpoation
    !
    USE, INTRINSIC:: ISO_FORTRAN_ENV, ONLY: DBL => REAL64
    USE CONSTANTS
    USE DATE_OPERATOR_INSTRUCTION,    ONLY: DATE_OPERATOR
    USE XY_GRID_COORDINATE_INTERFACE, ONLY: XY_GRID_COODINATES
    IMPLICIT NONE
    PRIVATE
    PUBLIC:: OBS_POINTS, OBS_POINT
    !
    REAL(DBL), PARAMETER:: OFF_TOL = 0.0001_dbl
    !
    TYPE OBS_POINT
        CHARACTER(:),ALLOCATABLE:: NAM
        DOUBLE PRECISION:: X, Y
        DOUBLE PRECISION:: ROFF,COFF
        DOUBLE PRECISION, DIMENSION(:),ALLOCATABLE:: RINT
        INTEGER:: LAY, ROW, COL, ID
        INTEGER:: I1, J1, IOFF, JOFF !I1,J1 ARE ADJACENT, IOFF/JOFF are off diagonal cell
        LOGICAL:: XY_INTERP
        LOGICAL:: DO_INTERP = FALSE
        LOGICAL:: ALL_TIME
        LOGICAL:: IB_ROW=FALSE, IB_COL=FALSE, IB_OFF=FALSE
        DOUBLE PRECISION:: SIM
        !DOUBLE PRECISION:: OBS
        !TYPE(DATE_OPERATOR):: DT
        !
        CONTAINS
        ! 
        PROCEDURE, PASS(OP):: INIT  => INITALIZE_OBS_POINT!(XY,NAM,X,Y,LAY,NLAY,XY_INTERP,NCHAR,ROW,COL)
        PROCEDURE, PASS(OP):: BASIS_BUILDER!(NROW,NCOL,NLAY,IBOUND,DELC,DELR)
        GENERIC::             INTERP => INTERP_3D,INTERP_2D,INTERP_1D
        PROCEDURE, PASS(OP):: INTERP_SUB!(NROW,NCOL,NDIM,SIM)
        PROCEDURE, PASS(OP):: DESTROY_OBS_POINT
        !
        PROCEDURE, PASS(OP), PRIVATE:: INTERP_3D!(NROW,NCOL,NLAY,SIM) 
        PROCEDURE, PASS(OP), PRIVATE:: INTERP_2D!(NROW,NCOL,SIM) 
        PROCEDURE, PASS(OP), PRIVATE:: INTERP_1D!(IDX,SIM)
        FINAL:: FINAL_OBS_POINT
    END TYPE
    !
    TYPE OBS_POINTS
        !
        INTEGER:: N   = Z
        INTEGER:: DIM = Z
        !
        TYPE(OBS_POINT),DIMENSION(:),ALLOCATABLE:: OP
        !
        CONTAINS
        ! 
        PROCEDURE, PASS(OBS):: INIT    => INITALIZE_OBS_POINTS!(NPOINT)
        PROCEDURE, PASS(OBS):: DESTROY => DESTROY_OBS_POINTS
        FINAL:: FINAL_OBS_POINTS
    END TYPE
  !
  CONTAINS
  !
  SUBROUTINE FINAL_OBS_POINTS(OBS)
    TYPE(OBS_POINTS), INTENT(INOUT):: OBS
    !
    CALL DESTROY_OBS_POINTS(OBS)
    !
  END SUBROUTINE
  !
  PURE SUBROUTINE DESTROY_OBS_POINTS(OBS)
    CLASS(OBS_POINTS), INTENT(INOUT):: OBS
    !
    IF(ALLOCATED(OBS%OP)) DEALLOCATE(OBS%OP)
    !
    OBS%N   = Z
    OBS%DIM = Z
    !
  END SUBROUTINE
  !
  PURE SUBROUTINE INITALIZE_OBS_POINTS(OBS,NPOINT)
    CLASS(OBS_POINTS), INTENT(INOUT):: OBS
    INTEGER,           INTENT(IN   ):: NPOINT
    !
    IF(NPOINT > Z) THEN
       !
       OBS%N   = NPOINT
       !
       IF(OBS%DIM == Z) THEN
           OBS%DIM = NPOINT
           ALLOCATE(OBS%OP(NPOINT))
       ELSEIF(OBS%DIM < NPOINT) THEN
           OBS%DIM = NPOINT
           DEALLOCATE(OBS%OP)
           ALLOCATE(OBS%OP(NPOINT))
       END IF
    ELSE
       OBS%N   = Z
    END IF
    !
  END SUBROUTINE
  !
  SUBROUTINE FINAL_OBS_POINT(OP)
    TYPE(OBS_POINT), INTENT(INOUT):: OP
    !
    CALL DESTROY_OBS_POINT(OP)
    !
  END SUBROUTINE
  !
  PURE SUBROUTINE DESTROY_OBS_POINT(OP)
    CLASS(OBS_POINT), INTENT(INOUT):: OP
    !
    IF(ALLOCATED(OP%NAM )) DEALLOCATE(OP%NAM)
    IF(ALLOCATED(OP%RINT)) DEALLOCATE(OP%RINT)
    !
    OP%ROW = Z
    OP%COL = Z
    OP%ID  = Z
    OP%IB_ROW=FALSE
    OP%IB_COL=FALSE
    OP%IB_OFF=FALSE
    OP%DO_INTERP = FALSE
    !
  END SUBROUTINE
  !
  PURE SUBROUTINE INITALIZE_OBS_POINT(OP,XY,NAM,X,Y,LAY,XY_INTERP,NCHAR,ROW,COL,ID)
    CLASS(OBS_POINT),                       INTENT(INOUT):: OP
    TYPE(XY_GRID_COODINATES),               INTENT(IN   ):: XY  !MODEL COORDINATES
    CHARACTER(*),                           INTENT(IN   ):: NAM
    DOUBLE PRECISION,                       INTENT(IN   ):: X,Y
    INTEGER,                                INTENT(IN   ):: LAY
    LOGICAL,                                INTENT(IN   ):: XY_INTERP
    INTEGER, OPTIONAL,                      INTENT(IN   ):: ID, NCHAR, ROW, COL
    !
    IF(ALLOCATED(OP%NAM)) DEALLOCATE(OP%NAM)
    IF(PRESENT(NCHAR)) THEN
        IF(NCHAR > Z) THEN
            ALLOCATE(CHARACTER(NCHAR):: OP%NAM)
            OP%NAM = NAM
        END IF
    ELSE
        ALLOCATE(OP%NAM, SOURCE = NAM)
    END IF
    !
    OP%X = X
    OP%Y = Y
    OP%LAY = LAY
    OP%XY_INTERP=XY_INTERP
    OP%DO_INTERP = FALSE
    !
    IF(PRESENT(ID)) THEN
        OP%ID = ID
    ELSE
        OP%ID = Z
    END IF
    !
    IF(PRESENT(ROW)) THEN
        OP%ROW = ROW
        OP%COL = COL
    ELSE
        CALL XY%XY2RC(X,Y,OP%ROW,OP%COL)
    END IF
    !
    IF(OP%ROW > Z) THEN
      !
      IF(XY_INTERP) THEN
          CALL XY%CENTER_OFFSET(X,Y,OP%COFF,OP%ROFF,OP%ROW,OP%COL)  !GET ROFF AND COFF AND ROW/COL
          ALLOCATE(OP%RINT(FOUR))
      ELSE
          OP%ROFF = DZ
          OP%COFF = DZ
      END IF
      !
      ASSOCIATE(I=>OP%ROW, J=>OP%COL, K=>OP%LAY,                      &
                I1=>OP%I1, J1=>OP%J1, IOFF=>OP%IOFF, JOFF=>OP%JOFF,  &
                ROFF=>OP%ROFF, COFF=>OP%COFF,                          &
                IB_ROW=>OP%IB_ROW, IB_COL=>OP%IB_COL, IB_OFF=>OP%IB_OFF)
                !
          IF (ROFF.LT.DZ) THEN
              I1   = I - ONE
              IOFF = NEG
          ELSE
              I1   = I + ONE
              IOFF = ONE
          ENDIF
          IF (COFF.LT.DZ) THEN
              J1 = J - 1
              JOFF = NEG
          ELSE
              J1   = J + ONE
              JOFF = ONE
          ENDIF
          !
          IB_ROW = FALSE
          IB_COL = FALSE
          IB_OFF = FALSE
          !
          OP%DO_INTERP = FALSE
      END ASSOCIATE
    ELSE
      CALL DESTROY_OBS_POINT(OP)  !FAILED TO FIND POINT!!!
    END IF
    !
  END SUBROUTINE
  !
  PURE SUBROUTINE BASIS_BUILDER(OP,NROW,NCOL,NLAY,IBOUND,DELC,DELR,HCHK,BOTM,LBOTM,LAYHDT)
    CLASS(OBS_POINT),                     INTENT(INOUT):: OP
    INTEGER,                              INTENT(IN   ):: NROW,NCOL,NLAY
    INTEGER,   DIMENSION(NCOL,NROW,NLAY), INTENT(IN   ):: IBOUND
    REAL,      DIMENSION(NCOL),           INTENT(IN   ):: DELR
    REAL,      DIMENSION(NROW),           INTENT(IN   ):: DELC
    !
    DOUBLE PRECISION, DIMENSION(NCOL,NROW,NLAY),     OPTIONAL, INTENT(IN):: HCHK  !IF PRESENT THEN INCLUDES HNEW<BOTM CHECK
    DOUBLE PRECISION, DIMENSION(:,:,0:), CONTIGUOUS, OPTIONAL, INTENT(IN):: BOTM
    INTEGER, DIMENSION(NLAY),                        OPTIONAL, INTENT(IN):: LBOTM, LAYHDT
    LOGICAL:: UPDATE
    !
    IF(OP%XY_INTERP .AND. OP%ROW > Z) THEN
       ASSOCIATE(I=>OP%ROW, J=>OP%COL, K=>OP%LAY,                      &
                 I1=>OP%I1, J1=>OP%J1, IOFF=>OP%IOFF, JOFF=>OP%JOFF,  &
                 ROFF=>OP%ROFF, COFF=>OP%COFF,                          &
                 IB_ROW=>OP%IB_ROW, IB_COL=>OP%IB_COL, IB_OFF=>OP%IB_OFF)
                 !
         UPDATE = FALSE
         !
         !----- IBOUND CHK -------------------------------------------------
         IF (I1.GE.ONE .AND. I1.LE.NROW) THEN
             !
             IF(IB_ROW .NEQV. IBOUND(J,I1,K).NE.Z) THEN
                 IB_ROW   =   IBOUND(J,I1,K).NE.Z
                 UPDATE   = TRUE
             END IF
         END IF
         !
         IF (J1.GE.ONE .AND. J1.LE.NCOL) THEN
             !
             IF(IB_COL .NEQV. IBOUND(J1,I,K).NE.Z) THEN
                 IB_COL   =   IBOUND(J1,I,K).NE.Z
                 UPDATE   = TRUE
             END IF
         END IF
         !
         IF (I1.GE.ONE .AND. I1.LE.NROW .AND. J1.GE.1 .AND. J1.LE.NCOL) THEN
             !
             IF(IB_OFF .NEQV. IBOUND(J1,I1,K).NE.Z) THEN
                 IB_OFF   =   IBOUND(J1,I1,K).NE.Z
                 UPDATE   = TRUE
             END IF
         END IF
         !
         !----- HCKH -------------------------------------------------
         IF(PRESENT(HCHK)) THEN
            !
           IF(LAYHDT(K).NE.Z) THEN  
            IF (IB_ROW .AND. I1.GE.ONE .AND. I1.LE.NROW) THEN
                !
                IF(IB_ROW .NEQV. HCHK(J,I1,K) > BOTM(J,I1,LBOTM(K))) THEN
                    IB_ROW   =   HCHK(J,I1,K) > BOTM(J,I1,LBOTM(K))
                    UPDATE   = TRUE
                END IF
            END IF
            !
            IF (IB_COL .AND. J1.GE.ONE .AND. J1.LE.NCOL) THEN
                !
                IF(IB_COL .NEQV. HCHK(J1,I,K) > BOTM(J1,I,LBOTM(K))) THEN
                    IB_COL   =   HCHK(J1,I,K) > BOTM(J1,I,LBOTM(K))
                    UPDATE   = TRUE
                END IF
            END IF
            !
            IF (IB_OFF .AND. I1.GE.ONE .AND. I1.LE.NROW .AND. J1.GE.1 .AND. J1.LE.NCOL) THEN
                !
                IF(IB_OFF .NEQV. HCHK(J1,I1,K) > BOTM(J1,I1,LBOTM(K))) THEN
                    IB_OFF   =   HCHK(J1,I1,K) > BOTM(J1,I1,LBOTM(K))
                    UPDATE   = TRUE
                END IF
            END IF
           END IF !( LAYHDT(K).NE.Z ) 
         END IF   !( PRESENT(HCHK)  ) 
         !----- END CHECKS -------------------------------------------------
         !
         IF (I1.LT.1 .OR. I1.GT.NROW) IB_ROW = FALSE
         IF (J1.LT.1 .OR. J1.GT.NCOL) IB_COL = FALSE
         IF (I1.LT.1 .OR. I1.GT.NROW .OR. J1.LT.1 .OR. J1.GT.NCOL) IB_OFF = FALSE
         !
         IF(UPDATE) THEN
           IF (     (ABS(ROFF) < OFF_TOL .AND. ABS(COFF) < OFF_TOL)  &
               .OR. (ABS(ROFF) < OFF_TOL .AND. .NOT. IB_ROW)       &
               .OR. (ABS(COFF) < OFF_TOL .AND. .NOT. IB_COL)       &
               .OR. (.NOT. IB_ROW      .AND. .NOT. IB_COL)        ) THEN
               ! 
               OP%DO_INTERP = FALSE
               IOFF = Z
               JOFF = Z
               OP%RINT(1) = FOURTH
               OP%RINT(2) = FOURTH
               OP%RINT(3) = FOURTH
               OP%RINT(4) = FOURTH
           ELSE
               OP%DO_INTERP = TRUE
           ENDIF
           !       
           IF (OP%DO_INTERP) THEN
               !
               IF (ROFF.LT.DZ) THEN
                   I1   = I - ONE
                   IOFF = NEG
               ELSE
                   I1   = I + ONE
                   IOFF = ONE
               ENDIF
               IF (COFF.LT.DZ) THEN
                   J1 = J - 1
                   JOFF = NEG
               ELSE
                   J1   = J + ONE
                   JOFF = ONE
               ENDIF
               !
               CALL BASIS_COEFFIENT_BUILDER(OP,NROW,NCOL,NLAY,IBOUND,DELC,DELR)
           END IF
           !
         END IF
         !
       END ASSOCIATE
    END IF     
  END SUBROUTINE 
  !
  PURE SUBROUTINE BASIS_COEFFIENT_BUILDER(OP,NROW,NCOL,NLAY,IBOUND,DELC,DELR)
    CLASS(OBS_POINT),                     INTENT(INOUT):: OP
    INTEGER,                              INTENT(IN   ):: NROW,NCOL,NLAY
    INTEGER,   DIMENSION(NCOL,NROW,NLAY), INTENT(IN   ):: IBOUND
    REAL,      DIMENSION(NCOL),           INTENT(IN   ):: DELR
    REAL,      DIMENSION(NROW),           INTENT(IN   ):: DELC
    DOUBLE PRECISION:: A, DC, DCF, DR, DRF, ROFF_A, COFF_A
    !
    IF( OP%ROW > Z ) THEN
      ASSOCIATE(I=>OP%ROW, J=>OP%COL, K=>OP%LAY,                      &
              I1=>OP%I1, J1=>OP%J1, IOFF=>OP%IOFF, JOFF=>OP%JOFF,   &
              ROFF=>OP%ROFF, COFF=>OP%COFF,         RINT=>OP%RINT,  &
              IB_ROW=>OP%IB_ROW, IB_COL=>OP%IB_COL, IB_OFF=>OP%IB_OFF)
              !
        ROFF_A = ABS(ROFF)
        COFF_A = ABS(COFF)
        !
        DRF = DZ
        DCF = DZ
        IF (ROFF_A > OFF_TOL) DCF = ROFF_A*DELC(I)
        IF (COFF_A > OFF_TOL) DRF = COFF_A*DELR(J)
        !
        IF (IB_ROW) THEN
                      DC = (DELC(I)+DELC(I1)) * HALF
        ELSE
                      DC = DELC(I) * HALF
        END IF
        IF (IB_COL) THEN
                      DR = (DELR(J)+DELR(J1)) * HALF
        ELSE
                      DR = DELR(J) * HALF
        END IF
        !
        IF (ROFF_A > OFF_TOL .AND. COFF_A > OFF_TOL) THEN
            A = UNO/(DC*DR)
        ELSE
            A = DZ
        END IF
        !
        !!!IF (ROFF_A > OFF_TOL) THEN
        !!!                        DC = (DELC(I)+DELC(I1)) * HALF
        !!!                        DCF = ROFF_A*DELC(I)
        !!!ENDIF
        !!!IF (COFF_A > OFF_TOL) THEN
        !!!                        DR = (DELR(J)+DELR(J1)) * HALF
        !!!                        DRF = COFF_A*DELR(J)
        !!!ENDIF
        !
        IF (ROFF_A > OFF_TOL .AND. COFF_A > OFF_TOL) THEN
            A = UNO/(DC*DR)
        ELSE
            A = DZ
        END IF
        ! 
        ! LINEAR INTERPOLATION
        IF (ROFF_A < OFF_TOL .OR. (.NOT. IB_ROW .AND. .NOT. IB_OFF)) THEN
            !
            IOFF = 0
            RINT(1) = 0.5*(1.-DRF/DR)
            RINT(2) = 0.5*DRF/DR
            RINT(3) = RINT(1)
            RINT(4) = RINT(2)
            !
        ELSEIF (COFF_A < OFF_TOL .OR. (.NOT. IB_COL .AND. .NOT. IB_OFF)) THEN
            !
            JOFF = 0
            RINT(1) = 0.5*(1.-DCF/DC)
            RINT(2) = RINT(1)
            RINT(3) = 0.5*DCF/DC
            RINT(4) = RINT(3)
            !
        ! CALCULATE BASIS FUNCTIONS FOR INTERPOLATION ON A RECTANGLE
        ELSEIF (IB_ROW .AND. IB_COL .AND. IB_OFF) THEN
            !
            RINT(3) = A*(DR-DRF)*DCF
            RINT(4) = A*DRF*DCF
            RINT(2) = A*DRF*(DC-DCF)
            RINT(1) = A*(DR-DRF)*(DC-DCF)
            ! 
        ! CALCULATE BASIS FUNCTIONS FOR INTERPOLATION ON A TRIANGLE
        ELSEIF (.NOT. IB_COL) THEN
            !
            RINT(1) = A*(DR*DC-DR*DCF)
            RINT(2) = 0.0
            RINT(3) = A*(DR*DCF-DC*DRF)
            RINT(4) = A*(DC*DRF)
            !
        ELSEIF (.NOT. IB_ROW) THEN
            !
            RINT(1) = A*(DR*DC-DC*DRF)
            RINT(4) = A*(DR*DCF)
            RINT(2) = A*(DC*DRF-DR*DCF)
            RINT(3) = 0.0
            !
        ELSEIF (.NOT. IB_OFF) THEN
            !
            RINT(1) = A*(DR*DC-DC*DRF-DR*DCF)
            RINT(3) = A*(DR*DCF)
            RINT(2) = A*(DC*DRF)
            RINT(4) = 0.0
        ELSE
          IOFF = Z    !SHOULD NEVER GO HERE, BUT CATCH ALL STATEMENT
          JOFF = Z
          RINT(1) = FOURTH
          RINT(2) = FOURTH
          RINT(3) = FOURTH
          RINT(4) = FOURTH
        ENDIF
      END ASSOCIATE
    END IF
    !
  END SUBROUTINE
  !
  PURE SUBROUTINE INTERP_3D(OP,NROW,NCOL,NLAY,SIM)
    CLASS(OBS_POINT),                            INTENT(INOUT):: OP
    INTEGER,                                     INTENT(IN   ):: NROW,NCOL,NLAY
    DOUBLE PRECISION, DIMENSION(NCOL,NROW,NLAY), INTENT(IN   ):: SIM
    !
    ASSOCIATE(I=>OP%ROW,     J=>OP%COL,     K=>OP%LAY,    &
              IOFF=>OP%IOFF, JOFF=>OP%JOFF, RINT=>OP%RINT  )
              !
       IF(OP%DO_INTERP) THEN
           !
           OP%SIM = RINT(1)*SIM(J,     I,     K) +  &
                    RINT(2)*SIM(J+JOFF,I,     K) +  &
                    RINT(3)*SIM(J,     I+IOFF,K) +  &
                    RINT(4)*SIM(J+JOFF,I+IOFF,K)
       ELSE
           OP%SIM = SIM(J,I,K)
       END IF
       !
    END ASSOCIATE
    !
  END SUBROUTINE
  !
  PURE SUBROUTINE INTERP_2D(OP,NROW,NCOL,SIM) 
    CLASS(OBS_POINT),                       INTENT(INOUT):: OP
    INTEGER,                                INTENT(IN   ):: NROW,NCOL
    DOUBLE PRECISION, DIMENSION(NCOL,NROW), INTENT(IN   ):: SIM
    !
    ASSOCIATE(I=>OP%ROW,     J=>OP%COL,                   &
              IOFF=>OP%IOFF, JOFF=>OP%JOFF, RINT=>OP%RINT  )
              !
       IF(OP%DO_INTERP) THEN
           !
           OP%SIM = RINT(1)*SIM(J,     I     ) +  &
                    RINT(2)*SIM(J+JOFF,I     ) +  &
                    RINT(3)*SIM(J,     I+IOFF) +  &
                    RINT(4)*SIM(J+JOFF,I+IOFF)
       ELSE
           OP%SIM = SIM(J,I)
       END IF
       !
    END ASSOCIATE
    !
  END SUBROUTINE
  !
  PURE SUBROUTINE INTERP_1D(OP,IDX,SIM)
    CLASS(OBS_POINT),               INTENT(INOUT):: OP
    INTEGER,          DIMENSION(:), INTENT(IN   ):: IDX
    DOUBLE PRECISION, DIMENSION(:), INTENT(IN   ):: SIM
    !
    ASSOCIATE(I=>OP%ROW,     J=>OP%COL,                   &
              IOFF=>OP%IOFF, JOFF=>OP%JOFF, RINT=>OP%RINT  )
              !
       IF(OP%DO_INTERP) THEN
           !
           OP%SIM = RINT(1)*SIM(IDX(1)) +  &
                    RINT(2)*SIM(IDX(2)) +  &
                    RINT(3)*SIM(IDX(3)) +  &
                    RINT(4)*SIM(IDX(4))
       ELSE
           OP%SIM = SIM(IDX(1))
       END IF
       !
    END ASSOCIATE
    !
  END SUBROUTINE
  !
  PURE SUBROUTINE INTERP_SUB(OP,NROW,NCOL,NDIM,SIM)    !NDIM = NROW*NCOL*NNDB
    CLASS(OBS_POINT),                  INTENT(INOUT):: OP
    INTEGER,                           INTENT(IN   ):: NROW,NCOL,NDIM
    DOUBLE PRECISION, DIMENSION(NDIM), INTENT(IN   ):: SIM
    INTEGER:: II, KQ
    !
    ASSOCIATE(I=>OP%ROW,     J=>OP%COL,     K=>OP%ID,      &
              IOFF=>OP%IOFF, JOFF=>OP%JOFF, RINT=>OP%RINT   )
              !
       KQ = (K-1)*NROW*NCOL
       !
       IF(OP%DO_INTERP) THEN
           !
           II = NCOL*(I-1) + J + KQ;       OP%SIM =          RINT(1)*SIM(II)
           II = II + JOFF;                 OP%SIM = OP%SIM + RINT(2)*SIM(II)   ! II = NCOL*(I-1) + J+JOFF + KQ
           II = NCOL*(I-1+IOFF) + J + KQ;  OP%SIM = OP%SIM + RINT(3)*SIM(II) 
           II = II + JOFF;                 OP%SIM = OP%SIM + RINT(4)*SIM(II)   ! II = NCOL*(I-1+IOFF) + J+JOFF + KQ
       ELSE
           OP%SIM = SIM( NCOL*(I-1) + J + KQ )
       END IF
       !
    END ASSOCIATE
    !
  END SUBROUTINE
  !  
END MODULE
!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
! 