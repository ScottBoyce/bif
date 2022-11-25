!
! CODE DEVELOPED BY SCOTT E BOYCE 
!                   CONTACT <seboyce@usgs.gov> or <Boyce@engineer.com>
!
! Generic Allocation Routine that can allocate 
!  the base Fortran data types (REAL, INTEGER, LOGICAL, CHARACTER)
!
    
MODULE ALLOC_INTERFACE!, ONLY: ALLOC, GROW, RESIZE
  !
  USE, INTRINSIC:: ISO_FORTRAN_ENV, ONLY: REAL64!, REAL32, REAL128
  !
  IMPLICIT NONE(TYPE, EXTERNAL)
  !
  PRIVATE
  !
  PUBLIC::    GROW!(VEC, N, [By])
  PUBLIC::  RESIZE!(VEC, N, [EXACT])
  PUBLIC::  ALLOC
  !
  INTERFACE ALLOC
    ! GENERIC ALLOCATION ROUTINES
    !N is DIM1 size
    !M is DIM2 size
    !DIMCHK IS THE DIMENSION TO CHECK FOR A MATCH, 0 IS EXACT MATCH ALL DIM, 1 MUST MATCH DIM 1, etc.
    !EXACT = False will not reallocate if N and M are smaller than current dimensions. Defualt is TRUE
    !SRC is what the array is set to
    MODULE PROCEDURE ALLOCATE_INT_VECTOR      !(VEC, N,              [EXACT], [SRC], [NEW_ALLOC])   -- SRC is a scalar
    MODULE PROCEDURE ALLOCATE_INT_VECTOR_SRC  !(VEC, N,              [EXACT],  SRC , [NEW_ALLOC])   -- SRC is dim N
    MODULE PROCEDURE ALLOCATE_INT_ARRAY       !(ARR, N, M, [DIMCHK], [EXACT], [SRC])                -- SRC is a scalar
    MODULE PROCEDURE ALLOCATE_INT_ARRAY_SRC   !(ARR, N, M, [DIMCHK], [EXACT],  SRC )                -- SRC is dim NxM
    MODULE PROCEDURE ALLOCATE_DBLE_VECTOR     !(VEC, N,              [EXACT], [SRC], [NEW_ALLOC])
    MODULE PROCEDURE ALLOCATE_DBLE_ARRAY      !(ARR, N, M, [DIMCHK], [EXACT], [SRC])
    MODULE PROCEDURE ALLOCATE_TF_VECTOR       !(VEC, N,              [EXACT], [SRC], [NEW_ALLOC])   -- SRC is a scalar
    MODULE PROCEDURE ALLOCATE_TF_VECTOR_SRC   !(VEC, N,              [EXACT], [SRC], [NEW_ALLOC])   -- SRC is dim N
    MODULE PROCEDURE ALLOCATE_TF_ARRAY        !(ARR, N, M)
    MODULE PROCEDURE ALLOCATE_CHAR_VECTOR     !(VEC, N, EXACT, SRC, NEW_ALLOC)
    MODULE PROCEDURE ALLOCATE_CHAR_VECTOR_SRC !(VEC, N, EXACT, SRC, NEW_ALLOC)
    MODULE PROCEDURE ALLOCATE_DIM_CHAR_VECTOR !(VEC, N, DIM, EXACT, SRC, NEW_ALLOC)
    MODULE PROCEDURE ALLOCATE_DIM_CHAR_VECTOR_SRC !(VEC, N, DIM, EXACT, SRC, NEW_ALLOC)
  END INTERFACE
  !
  INTERFACE RESIZE
    ! GENERIC ALLOCATION ROUTINES - ONLY ALLOCATE IF N<SIZE(VEC) AND PRESERVE ORIGINAL VEC VALUES
    !
    !N is desired size.
    !EXACT = False will not reallocate if N and M are smaller than current dimensions. Defualt is TRUE
    !DIM is set to SIZE(VEC)
    MODULE PROCEDURE RESIZE_INT_VECTOR      !(VEC, N, [EXACT], [DIM])
    MODULE PROCEDURE RESIZE_DBLE_VECTOR     !(VEC, N, [EXACT], [DIM])
    MODULE PROCEDURE RESIZE_TF_VECTOR       !(VEC, N, [EXACT], [DIM])
    MODULE PROCEDURE RESIZE_CHAR_VECTOR     !(VEC, N, [EXACT], [DIM])
  END INTERFACE
  !
  INTERFACE GROW
    ! INCREASE SIZE OF ARRAY BY A MULTIPLE [By] IF REQUESTED SIZE IS INSUFFIENT. DEFAULT IS TO GROW BY MULTIPLES OF 512
    ! VEC IS VECTOR TO REALLOCATE IF N<SIZE(VEC)
    ! N   IS THE DIMENSION TO CHECK AGAINST.
    ! By  IS THE MULTIPLE AT WHICH TO GROW ARRAY IF N<SIZE(VEC); DEFAULT IS 512
    ! DIM IS SET TO SIZE(VEC)
    MODULE PROCEDURE GROW_INT_VECTOR      !(VEC, N, [By], [DIM])
    MODULE PROCEDURE GROW_DBLE_VECTOR     !(VEC, N, [By], [DIM])
    MODULE PROCEDURE GROW_TF_VECTOR       !(VEC, N, [By], [DIM])
    MODULE PROCEDURE GROW_CHAR_VECTOR     !(VEC, N, [By], [DIM])
  END INTERFACE
  !
  ! Constants used internally to module ----------------------------------------------------
  ! 
  LOGICAL, PARAMETER:: TRUE  = .TRUE.
  LOGICAL, PARAMETER:: FALSE = .FALSE.
  !
  ! ----------------------------------------------------------------------------------------
  ! 
  CONTAINS
  !
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !  
  !  Allocation routines
  ! 
  PURE SUBROUTINE ALLOCATE_INT_VECTOR(VEC, N, EXACT, SRC, NEW_ALLOC)
    !VEC IS VECTOR TO ALLOCATE
    !N IS THE DIMENSION TO ALLOCATE. IF <1 THEN SUBROUTINE DOES NOTHING
    !EXACT IF SET TO TRUE TO INDICATE ALLOCATION MUST MATCH N  (DEFAULT)
    !      IF SET TO FALSE THEN IF ALREADY ALLOCATED AND ALLOCATION IS GREATER THAN N, THEN NOTHING IS DONE
    INTEGER, DIMENSION(:), ALLOCATABLE, INTENT(INOUT):: VEC
    INTEGER,                            INTENT(IN   ):: N
    LOGICAL,                  OPTIONAL, INTENT(IN   ):: EXACT
    INTEGER,                  OPTIONAL, INTENT(IN   ):: SRC
    LOGICAL,                  OPTIONAL, INTENT(OUT  ):: NEW_ALLOC
    LOGICAL:: EXACT_DIM, NEW_ALLOCATION
    !
    NEW_ALLOCATION = FALSE
    !
    IF(N > 0) THEN
                  EXACT_DIM = TRUE
                  IF(PRESENT(EXACT)) EXACT_DIM = EXACT
                  !
                  IF( ALLOCATED(VEC) ) THEN
                                       IF(EXACT_DIM .AND. SIZE(VEC).NE.N) THEN
                                                                               NEW_ALLOCATION = TRUE
                                                                               DEALLOCATE(VEC)
                                                                               IF(PRESENT(SRC)) THEN
                                                                                                    ALLOCATE(VEC(N), SOURCE=SRC)
                                                                               ELSE
                                                                                                    ALLOCATE(VEC(N))
                                                                               END IF
                                       ELSEIF(            SIZE(VEC).LT.N) THEN
                                                                               NEW_ALLOCATION = TRUE
                                                                               DEALLOCATE(VEC   )
                                                                               IF(PRESENT(SRC)) THEN
                                                                                                    ALLOCATE(VEC(N), SOURCE=SRC)
                                                                               ELSE
                                                                                                    ALLOCATE(VEC(N))
                                                                               END IF
                                       ELSEIF(PRESENT(SRC)) THEN
                                                                               VEC=SRC
                                       END IF
                  ELSE
                                                                               IF(PRESENT(SRC)) THEN
                                                                                                    ALLOCATE(VEC(N), SOURCE=SRC)
                                                                               ELSE
                                                                                                    ALLOCATE(VEC(N))
                                                                               END IF
                                                                               NEW_ALLOCATION = TRUE
                  END IF
    ELSEIF(ALLOCATED(VEC)) THEN
                  EXACT_DIM = TRUE
                  IF(PRESENT(EXACT)) EXACT_DIM = EXACT
                  IF(EXACT_DIM) DEALLOCATE(VEC)
    END IF
    !
    IF(PRESENT(NEW_ALLOC)) NEW_ALLOC=NEW_ALLOCATION
    !
  END SUBROUTINE
  !
  !#########################################################################################################################
  !
  PURE SUBROUTINE ALLOCATE_INT_VECTOR_SRC(VEC, N, EXACT, SRC, NEW_ALLOC)
    !VEC IS VECTOR TO ALLOCATE
    !N IS THE DIMENSION TO ALLOCATE. IF <1 THEN SUBROUTINE DOES NOTHING
    !EXACT IF SET TO TRUE TO INDICATE ALLOCATION MUST MATCH N  (DEFAULT)
    !      IF SET TO FALSE THEN IF ALREADY ALLOCATED AND ALLOCATION IS GREATER THAN N, THEN NOTHING IS DONE
    INTEGER, DIMENSION(:), ALLOCATABLE, INTENT(INOUT):: VEC
    INTEGER,                            INTENT(IN   ):: N
    LOGICAL,                  OPTIONAL, INTENT(IN   ):: EXACT
    INTEGER, DIMENSION(:),  CONTIGUOUS, INTENT(IN   ):: SRC
    LOGICAL,                  OPTIONAL, INTENT(OUT  ):: NEW_ALLOC
    LOGICAL:: EXACT_DIM, NEW_ALLOCATION
    !
    NEW_ALLOCATION = FALSE
    !
    IF(N > 0) THEN
                  EXACT_DIM = TRUE
                  IF(PRESENT(EXACT)) EXACT_DIM = EXACT
                  !
                  IF( ALLOCATED(VEC) ) THEN
                                       IF(EXACT_DIM .AND. SIZE(VEC).NE.N) THEN
                                                                               NEW_ALLOCATION = TRUE
                                                                               DEALLOCATE(VEC)
                                                                               ALLOCATE(VEC(N), SOURCE=SRC)
                                       ELSEIF(            SIZE(VEC).LT.N) THEN
                                                                               NEW_ALLOCATION = TRUE
                                                                               DEALLOCATE(VEC   )
                                                                               ALLOCATE(VEC(N), SOURCE=SRC)
                                       ELSE
                                                                               VEC=SRC
                                       END IF
                  ELSE
                                                                               ALLOCATE(VEC(N), SOURCE=SRC)
                                                                               NEW_ALLOCATION = TRUE
                  END IF
    ELSEIF(ALLOCATED(VEC)) THEN
                  EXACT_DIM = TRUE
                  IF(PRESENT(EXACT)) EXACT_DIM = EXACT
                  IF(EXACT_DIM) DEALLOCATE(VEC)
    END IF
    !
    IF(PRESENT(NEW_ALLOC)) NEW_ALLOC=NEW_ALLOCATION
    !
  END SUBROUTINE
  !
  !#########################################################################################################################
  !
  PURE SUBROUTINE ALLOCATE_INT_ARRAY(ARR, N, M, DIMCHK, EXACT, SRC)
    !ARR IS ARRAY TO ALLOCATE
    !N,M ARE THE DIMENSION TO ALLOCATE => ARR(N,M)
    !DIMCHK IS THE DIMENSION TO CHECK FOR A MATCH, 0 IS EXACT MATCH ALL DIM, 1 MUST MATCH DIM 1, etc.
    INTEGER, DIMENSION(:,:), ALLOCATABLE, INTENT(INOUT):: ARR
    INTEGER,                              INTENT(IN   ):: N, M
    INTEGER,                    OPTIONAL, INTENT(IN   ):: DIMCHK
    LOGICAL,                    OPTIONAL, INTENT(IN   ):: EXACT
    INTEGER,                    OPTIONAL, INTENT(IN   ):: SRC
    !
    IF(PRESENT(DIMCHK) .AND. ALLOCATED(ARR)) THEN
        CALL ALLOCATE_INT_ARRAY_DIMCHK(ARR, N, M, DIMCHK, EXACT, SRC)
    ELSE
        IF( ALLOCATED(ARR) ) DEALLOCATE(ARR)
        !
        IF(N>0 .AND. M>0) THEN
            IF( PRESENT(SRC) ) THEN
                ALLOCATE(ARR(N,M), SOURCE=SRC)
            ELSE
                ALLOCATE(ARR(N,M))
            END IF
        END IF
    END IF
    !
  END SUBROUTINE
  !
  !#########################################################################################################################
  !
  PURE SUBROUTINE ALLOCATE_INT_ARRAY_SRC(ARR, N, M, DIMCHK, EXACT, SRC)
    !ARR IS ARRAY TO ALLOCATE
    !N,M ARE THE DIMENSION TO ALLOCATE => ARR(N,M)
    !DIMCHK IS THE DIMENSION TO CHECK FOR A MATCH, 0 IS EXACT MATCH ALL DIM, 1 MUST MATCH DIM 1, etc.
    INTEGER, DIMENSION(:,:), ALLOCATABLE, INTENT(INOUT):: ARR
    INTEGER,                              INTENT(IN   ):: N, M
    INTEGER,                    OPTIONAL, INTENT(IN   ):: DIMCHK
    LOGICAL,                    OPTIONAL, INTENT(IN   ):: EXACT
    INTEGER, DIMENSION(:,:),  CONTIGUOUS, INTENT(IN   ):: SRC
    !
    IF(PRESENT(DIMCHK) .AND. ALLOCATED(ARR) .AND. N > 0 .AND. M > 0) THEN
        !
        CALL ALLOCATE_INT_ARRAY_DIMCHK_SRC(ARR, N, M, DIMCHK, EXACT, SRC)
    ELSE
        IF( ALLOCATED(ARR) ) DEALLOCATE(ARR)
        !
        IF(N>0 .AND. M>0) ALLOCATE(ARR(N,M), SOURCE=SRC)
    END IF
    !
  END SUBROUTINE
  !
  !#########################################################################################################################
  !
  PURE SUBROUTINE ALLOCATE_INT_ARRAY_DIMCHK_SRC(ARR, N, M, DIMCHK, EXACT, SRC)
    !ARR IS ARRAY TO ALLOCATE
    !N,M ARE THE DIMENSION TO ALLOCATE => ARR(N,M)
    !DIMCHK IS THE DIMENSION TO CHECK FOR A MATCH, 0 IS EXACT MATCH ALL DIM, 1 MUST MATCH DIM 1, etc.
    INTEGER, DIMENSION(:,:), ALLOCATABLE, INTENT(INOUT):: ARR
    INTEGER,                              INTENT(IN   ):: N, M, DIMCHK
    LOGICAL,                    OPTIONAL, INTENT(IN   ):: EXACT
    INTEGER, DIMENSION(N,M),              INTENT(IN   ):: SRC
    LOGICAL:: EXACT_DIM, ALOC
    !
    IF(N>0 .AND. M>0) THEN
       EXACT_DIM = TRUE
       IF(PRESENT(EXACT)) EXACT_DIM = EXACT
       !
       ALOC = TRUE
       !
       IF( ALLOCATED(ARR) ) THEN
           ALOC = FALSE
           IF(DIMCHK == 1) THEN
               IF(EXACT_DIM .AND. SIZE(ARR,1).NE.N) THEN
                                                          ALOC = TRUE
                                                          DEALLOCATE(ARR)
               ELSEIF(            SIZE(ARR,1).LT.N) THEN
                                                          ALOC = TRUE
                                                          DEALLOCATE(ARR)
               END IF
           ELSEIF(DIMCHK == 2) THEN
               IF(EXACT_DIM .AND. SIZE(ARR,2).NE.M) THEN
                                                          ALOC = TRUE
                                                          DEALLOCATE(ARR)
               ELSEIF(            SIZE(ARR,2).LT.M) THEN
                                                          ALOC = TRUE
                                                          DEALLOCATE(ARR)
               END IF
           ELSEIF(SIZE(ARR,2).NE.N .OR. SIZE(ARR,2).NE.M) THEN
                                                          ALOC = TRUE
                                                          DEALLOCATE(ARR)
           
           END IF
       END IF
       !
       IF(ALOC) THEN
           ALLOCATE(ARR(N,M), SOURCE=SRC)
       ELSE
                                ARR  = SRC
       END IF
       !
    ELSEIF(ALLOCATED(ARR)) THEN
                                DEALLOCATE(ARR)
    END IF
    !
  END SUBROUTINE
  !
  !#########################################################################################################################
  !
  PURE SUBROUTINE ALLOCATE_INT_ARRAY_DIMCHK(ARR, N, M, DIMCHK, EXACT, SRC)
    !ARR IS ARRAY TO ALLOCATE
    !N,M ARE THE DIMENSION TO ALLOCATE => ARR(N,M)
    !DIMCHK IS THE DIMENSION TO CHECK FOR A MATCH, 0 IS EXACT MATCH ALL DIM, 1 MUST MATCH DIM 1, etc.
    INTEGER, DIMENSION(:,:), ALLOCATABLE, INTENT(INOUT):: ARR
    INTEGER,                              INTENT(IN   ):: N, M, DIMCHK
    LOGICAL,                    OPTIONAL, INTENT(IN   ):: EXACT
    INTEGER,                    OPTIONAL, INTENT(IN   ):: SRC
    LOGICAL:: EXACT_DIM, ALOC
    !
    IF(N>0 .AND. M>0) THEN
       EXACT_DIM = TRUE
       IF(PRESENT(EXACT)) EXACT_DIM = EXACT
       !
       ALOC = TRUE
       !
       IF( ALLOCATED(ARR) ) THEN
           ALOC = FALSE
           IF(DIMCHK == 1) THEN
               IF(EXACT_DIM .AND. SIZE(ARR,1).NE.N) THEN
                                                          ALOC = TRUE
                                                          DEALLOCATE(ARR)
               ELSEIF(            SIZE(ARR,1).LT.N) THEN
                                                          ALOC = TRUE
                                                          DEALLOCATE(ARR)
               END IF
           ELSEIF(DIMCHK == 2) THEN
               IF(EXACT_DIM .AND. SIZE(ARR,2).NE.M) THEN
                                                          ALOC = TRUE
                                                          DEALLOCATE(ARR)
               ELSEIF(            SIZE(ARR,2).LT.M) THEN
                                                          ALOC = TRUE
                                                          DEALLOCATE(ARR)
               END IF
           ELSEIF(SIZE(ARR,2).NE.N .OR. SIZE(ARR,2).NE.M) THEN
                                                          ALOC = TRUE
                                                          DEALLOCATE(ARR)
           
           END IF
       END IF
       !
       IF(ALOC) THEN
           IF( PRESENT(SRC) ) THEN
               ALLOCATE(ARR(N,M), SOURCE=SRC)
           ELSE
               ALLOCATE(ARR(N,M))
           END IF
       ELSEIF(PRESENT(SRC)) THEN
                                ARR  = SRC
       END IF
       !
    ELSEIF(ALLOCATED(ARR)) THEN
                                DEALLOCATE(ARR)
    END IF
    !
  END SUBROUTINE
  !
  !#########################################################################################################################
  !
  PURE SUBROUTINE ALLOCATE_DBLE_VECTOR(VEC, N, EXACT, SRC, NEW_ALLOC)
    !VEC IS VECTOR TO ALLOCATE
    !N IS THE DIMENSION TO ALLOCATE
    !EXACT IF SET TO TRUE TO INDICATE ALLOCATION MUST MATCH N  (DEFAULT)
    !      IF SET TO FALSE THEN IF ALREADY ALLOCATED AND ALLOCATION IS GREATER THAN N, THEN NOTHING IS DONE
    REAL(REAL64), DIMENSION(:), ALLOCATABLE, INTENT(INOUT):: VEC
    INTEGER,                                 INTENT(IN   ):: N
    LOGICAL,                       OPTIONAL, INTENT(IN   ):: EXACT
    REAL(REAL64),                  OPTIONAL, INTENT(IN   ):: SRC
    LOGICAL,                       OPTIONAL, INTENT(OUT  ):: NEW_ALLOC
    LOGICAL:: EXACT_DIM, NEW_ALLOCATION
    !
    NEW_ALLOCATION = FALSE
    !
    IF(N > 0) THEN
                  EXACT_DIM = TRUE
                  IF(PRESENT(EXACT)) EXACT_DIM = EXACT
                  !
                  IF( ALLOCATED(VEC) ) THEN
                                       IF(EXACT_DIM .AND. SIZE(VEC).NE.N) THEN
                                                                               NEW_ALLOCATION = TRUE
                                                                               DEALLOCATE(VEC   )
                                                                               IF(PRESENT(SRC)) THEN
                                                                                                    ALLOCATE(VEC(N), SOURCE=SRC)
                                                                               ELSE
                                                                                                    ALLOCATE(VEC(N))
                                                                               END IF
                                       ELSEIF(            SIZE(VEC).LT.N) THEN
                                                                               NEW_ALLOCATION = TRUE
                                                                               DEALLOCATE(VEC   )
                                                                               IF(PRESENT(SRC)) THEN
                                                                                                    ALLOCATE(VEC(N), SOURCE=SRC)
                                                                               ELSE
                                                                                                    ALLOCATE(VEC(N))
                                                                               END IF
                                       ELSEIF(PRESENT(SRC)) THEN
                                                                               VEC=SRC
                                       END IF
                  ELSE
                                                                               IF(PRESENT(SRC)) THEN
                                                                                                    ALLOCATE(VEC(N), SOURCE=SRC)
                                                                               ELSE
                                                                                                    ALLOCATE(VEC(N))
                                                                               END IF
                                                                               NEW_ALLOCATION = TRUE
                  END IF
    ELSEIF(ALLOCATED(VEC)) THEN
                  EXACT_DIM = TRUE
                  IF(PRESENT(EXACT)) EXACT_DIM = EXACT
                  IF(EXACT_DIM) DEALLOCATE(VEC)
    END IF
    !
    IF(PRESENT(NEW_ALLOC)) NEW_ALLOC=NEW_ALLOCATION
    !
  END SUBROUTINE
  !
  !#########################################################################################################################
  !
  PURE SUBROUTINE ALLOCATE_DBLE_ARRAY(ARR, N, M, DIMCHK, EXACT, SRC)
    !ARR IS ARRAY TO ALLOCATE
    !N,M ARE THE DIMENSION TO ALLOCATE => ARR(N,M)
    !DIMCHK IS THE DIMENSION TO CHECK FOR A MATCH, 0 IS EXACT MATCH ALL DIM, 1 MUST MATCH DIM 1, etc.
    REAL(REAL64), DIMENSION(:,:), ALLOCATABLE, INTENT(INOUT):: ARR
    INTEGER,                                   INTENT(IN   ):: N, M
    INTEGER,                         OPTIONAL, INTENT(IN   ):: DIMCHK
    LOGICAL,                         OPTIONAL, INTENT(IN   ):: EXACT
    REAL(REAL64),                    OPTIONAL, INTENT(IN   ):: SRC
    !
    IF(PRESENT(DIMCHK) .AND. ALLOCATED(ARR)) THEN
        CALL ALLOCATE_DBLE_ARRAY_DIMCHK(ARR, N, M, DIMCHK, EXACT, SRC)
    ELSE
        IF( ALLOCATED(ARR) ) DEALLOCATE(ARR)
        !
        IF(N>0 .AND. M>0) THEN
            IF( PRESENT(SRC) ) THEN
                ALLOCATE(ARR(N,M), SOURCE=SRC)
            ELSE
                ALLOCATE(ARR(N,M))
            END IF
        END IF
    END IF
    !
  END SUBROUTINE
  !
  !#########################################################################################################################
  !
  PURE SUBROUTINE ALLOCATE_DBLE_ARRAY_DIMCHK(ARR, N, M, DIMCHK, EXACT, SRC)
    !ARR IS ARRAY TO ALLOCATE
    !N,M ARE THE DIMENSION TO ALLOCATE => ARR(N,M)
    !DIMCHK IS THE DIMENSION TO CHECK FOR A MATCH, 0 IS EXACT MATCH ALL DIM, 1 MUST MATCH DIM 1, etc.
    REAL(REAL64), DIMENSION(:,:), ALLOCATABLE, INTENT(INOUT):: ARR
    INTEGER,                                   INTENT(IN   ):: N, M, DIMCHK
    LOGICAL,                         OPTIONAL, INTENT(IN   ):: EXACT
    REAL(REAL64),                    OPTIONAL, INTENT(IN   ):: SRC
    LOGICAL:: EXACT_DIM, ALOC
    !
    IF(N>0 .AND. M>0) THEN
       EXACT_DIM = TRUE
       IF(PRESENT(EXACT)) EXACT_DIM = EXACT
       !
       ALOC = TRUE
       !
       IF( ALLOCATED(ARR) ) THEN
           ALOC = FALSE
           IF(DIMCHK == 1) THEN
               IF(EXACT_DIM .AND. SIZE(ARR,1).NE.N) THEN
                                                          ALOC = TRUE
                                                          DEALLOCATE(ARR)
               ELSEIF(            SIZE(ARR,1).LT.N) THEN
                                                          ALOC = TRUE
                                                          DEALLOCATE(ARR)
               END IF
           ELSEIF(DIMCHK == 2) THEN
               IF(EXACT_DIM .AND. SIZE(ARR,2).NE.M) THEN
                                                          ALOC = TRUE
                                                          DEALLOCATE(ARR)
               ELSEIF(            SIZE(ARR,2).LT.M) THEN
                                                          ALOC = TRUE
                                                          DEALLOCATE(ARR)
               END IF
           ELSEIF(SIZE(ARR,2).NE.N .OR. SIZE(ARR,2).NE.M) THEN
                                                          ALOC = TRUE
                                                          DEALLOCATE(ARR)
           
           END IF
       END IF
       !
       IF(ALOC) THEN
           IF( PRESENT(SRC) ) THEN
               ALLOCATE(ARR(N,M), SOURCE=SRC)
           ELSE
               ALLOCATE(ARR(N,M))
           END IF
       ELSEIF(PRESENT(SRC)) THEN
                                ARR  = SRC
       END IF
       !
    ELSEIF(ALLOCATED(ARR)) THEN
                                DEALLOCATE(ARR)
    END IF
    !
  END SUBROUTINE
  !
  !#########################################################################################################################
  !
  !!!PURE SUBROUTINE ALLOCATE_TF_VECTOR(VEC, N, EXACT, NEW_ALLOC)
  !!!  !VEC IS VECTOR TO ALLOCATE
  !!!  !N IS THE DIMENSION TO ALLOCATE. IF <1 THEN SUBROUTINE DOES NOTHING
  !!!  !EXACT IF SET TO TRUE TO INDICATE ALLOCATION MUST MATCH N  (DEFAULT)
  !!!  !      IF SET TO FALSE THEN IF ALREADY ALLOCATED AND ALLOCATION IS GREATER THAN N, THEN NOTHING IS DONE
  !!!  LOGICAL, DIMENSION(:), ALLOCATABLE, INTENT(INOUT):: VEC
  !!!  INTEGER,                            INTENT(IN   ):: N
  !!!  LOGICAL,                  OPTIONAL, INTENT(IN   ):: EXACT
  !!!  LOGICAL,                  OPTIONAL, INTENT(OUT  ):: NEW_ALLOC
  !!!  LOGICAL:: EXACT_DIM, NEW_ALLOCATION
  !!!  !
  !!!  NEW_ALLOCATION = FALSE
  !!!  !
  !!!  IF(N > 0) THEN
  !!!                EXACT_DIM = TRUE
  !!!                IF(PRESENT(EXACT)) EXACT_DIM = EXACT
  !!!                !
  !!!                IF( ALLOCATED(VEC) ) THEN
  !!!                                     IF(EXACT_DIM .AND. SIZE(VEC).NE.N) THEN
  !!!                                                                             NEW_ALLOCATION = TRUE
  !!!                                                                             DEALLOCATE(VEC   )
  !!!                                                                             ALLOCATE(  VEC(N))
  !!!                                     ELSEIF(            SIZE(VEC).LT.N) THEN
  !!!                                                                             NEW_ALLOCATION = TRUE
  !!!                                                                             DEALLOCATE(VEC   )
  !!!                                                                             ALLOCATE(  VEC(N))
  !!!                                     END IF
  !!!                ELSE
  !!!                                                                             ALLOCATE(  VEC(N))
  !!!                                                                             NEW_ALLOCATION = TRUE
  !!!                END IF
  !!!  ELSEIF(ALLOCATED(VEC)) THEN
  !!!                EXACT_DIM = TRUE
  !!!                IF(PRESENT(EXACT)) EXACT_DIM = EXACT
  !!!                IF(EXACT_DIM) DEALLOCATE(VEC)
  !!!  END IF
  !!!  !
  !!!  IF(PRESENT(NEW_ALLOC)) NEW_ALLOC=NEW_ALLOCATION
  !!!  !
  !!!END SUBROUTINE
  PURE SUBROUTINE ALLOCATE_TF_VECTOR(VEC, N, EXACT, SRC, NEW_ALLOC)
    !VEC IS VECTOR TO ALLOCATE
    !N IS THE DIMENSION TO ALLOCATE. IF <1 THEN SUBROUTINE DOES NOTHING
    !EXACT IF SET TO TRUE TO INDICATE ALLOCATION MUST MATCH N  (DEFAULT)
    !      IF SET TO FALSE THEN IF ALREADY ALLOCATED AND ALLOCATION IS GREATER THAN N, THEN NOTHING IS DONE
    LOGICAL, DIMENSION(:), ALLOCATABLE, INTENT(INOUT):: VEC
    INTEGER,                            INTENT(IN   ):: N
    LOGICAL,                  OPTIONAL, INTENT(IN   ):: EXACT
    LOGICAL,                  OPTIONAL, INTENT(IN   ):: SRC
    LOGICAL,                  OPTIONAL, INTENT(OUT  ):: NEW_ALLOC
    LOGICAL:: EXACT_DIM, NEW_ALLOCATION
    !
    NEW_ALLOCATION = FALSE
    !
    IF(N > 0) THEN
                  EXACT_DIM = TRUE
                  IF(PRESENT(EXACT)) EXACT_DIM = EXACT
                  !
                  IF( ALLOCATED(VEC) ) THEN
                                       IF(EXACT_DIM .AND. SIZE(VEC).NE.N) THEN
                                                                               NEW_ALLOCATION = TRUE
                                                                               DEALLOCATE(VEC)
                                                                               IF(PRESENT(SRC)) THEN
                                                                                                    ALLOCATE(VEC(N), SOURCE=SRC)
                                                                               ELSE
                                                                                                    ALLOCATE(VEC(N))
                                                                               END IF
                                       ELSEIF(            SIZE(VEC).LT.N) THEN
                                                                               NEW_ALLOCATION = TRUE
                                                                               DEALLOCATE(VEC   )
                                                                               IF(PRESENT(SRC)) THEN
                                                                                                    ALLOCATE(VEC(N), SOURCE=SRC)
                                                                               ELSE
                                                                                                    ALLOCATE(VEC(N))
                                                                               END IF
                                       ELSEIF(PRESENT(SRC)) THEN
                                                                               VEC=SRC
                                       END IF
                  ELSE
                                                                               IF(PRESENT(SRC)) THEN
                                                                                                    ALLOCATE(VEC(N), SOURCE=SRC)
                                                                               ELSE
                                                                                                    ALLOCATE(VEC(N))
                                                                               END IF
                                                                               NEW_ALLOCATION = TRUE
                  END IF
    ELSEIF(ALLOCATED(VEC)) THEN
                  EXACT_DIM = TRUE
                  IF(PRESENT(EXACT)) EXACT_DIM = EXACT
                  IF(EXACT_DIM) DEALLOCATE(VEC)
    END IF
    !
    IF(PRESENT(NEW_ALLOC)) NEW_ALLOC=NEW_ALLOCATION
    !
  END SUBROUTINE
  !
  !#########################################################################################################################
  !
  PURE SUBROUTINE ALLOCATE_TF_VECTOR_SRC(VEC, N, EXACT, SRC, NEW_ALLOC)
    !VEC IS VECTOR TO ALLOCATE
    !N IS THE DIMENSION TO ALLOCATE. IF <1 THEN SUBROUTINE DOES NOTHING
    !EXACT IF SET TO TRUE TO INDICATE ALLOCATION MUST MATCH N  (DEFAULT)
    !      IF SET TO FALSE THEN IF ALREADY ALLOCATED AND ALLOCATION IS GREATER THAN N, THEN NOTHING IS DONE
    LOGICAL, DIMENSION(:), ALLOCATABLE, INTENT(INOUT):: VEC
    INTEGER,                            INTENT(IN   ):: N
    LOGICAL,                  OPTIONAL, INTENT(IN   ):: EXACT
    LOGICAL, DIMENSION(:),  CONTIGUOUS, INTENT(IN   ):: SRC
    LOGICAL,                  OPTIONAL, INTENT(OUT  ):: NEW_ALLOC
    LOGICAL:: EXACT_DIM, NEW_ALLOCATION
    !
    NEW_ALLOCATION = FALSE
    !
    IF(N > 0) THEN
                  EXACT_DIM = TRUE
                  IF(PRESENT(EXACT)) EXACT_DIM = EXACT
                  !
                  IF( ALLOCATED(VEC) ) THEN
                                       IF(EXACT_DIM .AND. SIZE(VEC).NE.N) THEN
                                                                               NEW_ALLOCATION = TRUE
                                                                               DEALLOCATE(VEC)
                                                                               ALLOCATE(VEC(N), SOURCE=SRC)
                                       ELSEIF(            SIZE(VEC).LT.N) THEN
                                                                               NEW_ALLOCATION = TRUE
                                                                               DEALLOCATE(VEC   )
                                                                               ALLOCATE(VEC(N), SOURCE=SRC)
                                       ELSE
                                                                               VEC=SRC
                                       END IF
                  ELSE
                                                                               ALLOCATE(VEC(N), SOURCE=SRC)
                                                                               NEW_ALLOCATION = TRUE
                  END IF
    ELSEIF(ALLOCATED(VEC)) THEN
                  EXACT_DIM = TRUE
                  IF(PRESENT(EXACT)) EXACT_DIM = EXACT
                  IF(EXACT_DIM) DEALLOCATE(VEC)
    END IF
    !
    IF(PRESENT(NEW_ALLOC)) NEW_ALLOC=NEW_ALLOCATION
    !
  END SUBROUTINE
  !
  !#########################################################################################################################
  !
  PURE SUBROUTINE ALLOCATE_TF_ARRAY(ARR, N, M)
    !ARR IS ARRAY TO ALLOCATE
    !N,M ARE THE DIMENSION TO ALLOCATE => ARR(N,M)
    LOGICAL, DIMENSION(:,:), ALLOCATABLE, INTENT(INOUT):: ARR
    INTEGER,                              INTENT(IN   ):: N, M
    !
    IF( ALLOCATED(ARR) ) DEALLOCATE(ARR)
    !
    IF(N>0 .AND. M>0) ALLOCATE(ARR(N,M))
    !
  END SUBROUTINE
  !
  !#########################################################################################################################
  !
  PURE SUBROUTINE ALLOCATE_CHAR_VECTOR(VEC, N, EXACT, SRC, NEW_ALLOC) 
    !VEC IS VECTOR TO ALLOCATE
    !N IS THE DIMENSION TO ALLOCATE. IF <1 THEN SUBROUTINE DOES NOTHING
    !EXACT IF SET TO TRUE TO INDICATE ALLOCATION MUST MATCH N  (DEFAULT)
    !      IF SET TO FALSE THEN IF ALREADY ALLOCATED AND ALLOCATION IS GREATER THAN N, THEN NOTHING IS DONE
    CHARACTER(*), DIMENSION(:), ALLOCATABLE, INTENT(INOUT):: VEC
    INTEGER,                                 INTENT(IN   ):: N
    LOGICAL,                       OPTIONAL, INTENT(IN   ):: EXACT
    CHARACTER(*),                  OPTIONAL, INTENT(IN   ):: SRC
    LOGICAL,                       OPTIONAL, INTENT(OUT  ):: NEW_ALLOC
    LOGICAL:: EXACT_DIM, NEW_ALLOCATION
    !
    NEW_ALLOCATION = FALSE
    !
    IF(N > 0) THEN
                  EXACT_DIM = TRUE
                  IF(PRESENT(EXACT)) EXACT_DIM = EXACT
                  !
                  IF( ALLOCATED(VEC) ) THEN
                                       IF((EXACT_DIM .AND. SIZE(VEC).NE.N) .OR. SIZE(VEC) < N ) THEN
                                                                               NEW_ALLOCATION = TRUE
                                                                               DEALLOCATE(VEC)
                                                                               ALLOCATE(VEC(N))
                                       END IF
                  ELSE
                                                                               ALLOCATE(VEC(N))
                                                                               NEW_ALLOCATION = TRUE
                  END IF
                  IF(PRESENT(SRC)) VEC = SRC
    ELSEIF(ALLOCATED(VEC)) THEN
                  EXACT_DIM = TRUE
                  IF(PRESENT(EXACT)) EXACT_DIM = EXACT
                  IF(EXACT_DIM) DEALLOCATE(VEC)
    END IF
    !
    IF(PRESENT(NEW_ALLOC)) NEW_ALLOC=NEW_ALLOCATION
    !
  END SUBROUTINE
  !
  !#########################################################################################################################
  !
  PURE SUBROUTINE ALLOCATE_CHAR_VECTOR_SRC(VEC, N, EXACT, SRC, NEW_ALLOC) 
    !VEC IS VECTOR TO ALLOCATE
    !N IS THE DIMENSION TO ALLOCATE. IF <1 THEN SUBROUTINE DOES NOTHING
    !EXACT IF SET TO TRUE TO INDICATE ALLOCATION MUST MATCH N  (DEFAULT)
    !      IF SET TO FALSE THEN IF ALREADY ALLOCATED AND ALLOCATION IS GREATER THAN N, THEN NOTHING IS DONE
    CHARACTER(*), DIMENSION(:), ALLOCATABLE, INTENT(INOUT):: VEC
    INTEGER,                                 INTENT(IN   ):: N
    LOGICAL,                       OPTIONAL, INTENT(IN   ):: EXACT
    CHARACTER(*), DIMENSION(:),  CONTIGUOUS, INTENT(IN   ):: SRC
    LOGICAL,                       OPTIONAL, INTENT(OUT  ):: NEW_ALLOC
    LOGICAL:: EXACT_DIM, NEW_ALLOCATION
    !
    NEW_ALLOCATION = FALSE
    !
    IF(N > 0) THEN
                  EXACT_DIM = TRUE
                  IF(PRESENT(EXACT)) EXACT_DIM = EXACT
                  !
                  IF( ALLOCATED(VEC) ) THEN
                                       IF((EXACT_DIM .AND. SIZE(VEC).NE.N) .OR. SIZE(VEC) < N ) THEN
                                                                               NEW_ALLOCATION = TRUE
                                                                               DEALLOCATE(VEC)
                                                                               ALLOCATE(VEC(N))
                                       END IF
                  ELSE
                                                                               ALLOCATE(VEC(N))
                                                                               NEW_ALLOCATION = TRUE
                  END IF
                  VEC = SRC
    ELSEIF(ALLOCATED(VEC)) THEN
                  EXACT_DIM = TRUE
                  IF(PRESENT(EXACT)) EXACT_DIM = EXACT
                  IF(EXACT_DIM) DEALLOCATE(VEC)
    END IF
    !
    IF(PRESENT(NEW_ALLOC)) NEW_ALLOC=NEW_ALLOCATION
    !
  END SUBROUTINE
  !
  !#########################################################################################################################
  !
  PURE SUBROUTINE ALLOCATE_DIM_CHAR_VECTOR(VEC, N, DIM, EXACT, SRC, NEW_ALLOC)
    !VEC IS VECTOR TO ALLOCATE
    !N IS THE DIMENSION TO ALLOCATE. IF <1 THEN SUBROUTINE DOES NOTHING
    !EXACT IF SET TO TRUE TO INDICATE ALLOCATION MUST MATCH N  (DEFAULT)
    !      IF SET TO FALSE THEN IF ALREADY ALLOCATED AND ALLOCATION IS GREATER THAN N, THEN NOTHING IS DONE
    CHARACTER(:), DIMENSION(:), ALLOCATABLE, INTENT(INOUT):: VEC
    INTEGER,                                 INTENT(IN   ):: N, DIM
    LOGICAL,                       OPTIONAL, INTENT(IN   ):: EXACT
    CHARACTER(*),                  OPTIONAL, INTENT(IN   ):: SRC
    LOGICAL,                       OPTIONAL, INTENT(OUT  ):: NEW_ALLOC
    LOGICAL:: EXACT_DIM, NEW_ALLOCATION
    !
    NEW_ALLOCATION = FALSE
    !
    IF(N > 0 .AND. DIM > 0) THEN
                  EXACT_DIM = TRUE
                  IF(PRESENT(EXACT)) EXACT_DIM = EXACT
                  !
                  IF( ALLOCATED(VEC) ) THEN
                                       IF((EXACT_DIM .AND. (SIZE(VEC).NE.N .OR. LEN(VEC).NE.DIM)) .OR. &
                                          SIZE(VEC) < N  .OR. LEN(VEC) < DIM) THEN
                                                                               NEW_ALLOCATION = TRUE
                                                                               DEALLOCATE(VEC)
                                                                               ALLOCATE(CHARACTER(DIM):: VEC(N))
                                       END IF
                  ELSE
                                                                               ALLOCATE(CHARACTER(DIM):: VEC(N))
                                                                               NEW_ALLOCATION = TRUE
                  END IF
                  IF(PRESENT(SRC)) VEC = SRC
    ELSEIF(ALLOCATED(VEC)) THEN
                  EXACT_DIM = TRUE
                  IF(PRESENT(EXACT)) EXACT_DIM = EXACT
                  IF(EXACT_DIM) DEALLOCATE(VEC)
    END IF
    !
    IF(PRESENT(NEW_ALLOC)) NEW_ALLOC=NEW_ALLOCATION
    !
  END SUBROUTINE
  !
  !#########################################################################################################################
  !
  PURE SUBROUTINE ALLOCATE_DIM_CHAR_VECTOR_SRC(VEC, N, DIM, EXACT, SRC, NEW_ALLOC)
    !VEC IS VECTOR TO ALLOCATE
    !N IS THE DIMENSION TO ALLOCATE. IF <1 THEN SUBROUTINE DOES NOTHING
    !EXACT IF SET TO TRUE TO INDICATE ALLOCATION MUST MATCH N  (DEFAULT)
    !      IF SET TO FALSE THEN IF ALREADY ALLOCATED AND ALLOCATION IS GREATER THAN N, THEN NOTHING IS DONE
    CHARACTER(:), DIMENSION(:), ALLOCATABLE, INTENT(INOUT):: VEC
    INTEGER,                                 INTENT(IN   ):: N, DIM
    LOGICAL,                       OPTIONAL, INTENT(IN   ):: EXACT
    CHARACTER(*), DIMENSION(:),  CONTIGUOUS, INTENT(IN   ):: SRC
    LOGICAL,                       OPTIONAL, INTENT(OUT  ):: NEW_ALLOC
    LOGICAL:: EXACT_DIM, NEW_ALLOCATION
    !
    NEW_ALLOCATION = FALSE
    !
    IF(N > 0 .AND. DIM > 0) THEN
                  EXACT_DIM = TRUE
                  IF(PRESENT(EXACT)) EXACT_DIM = EXACT
                  !
                  IF( ALLOCATED(VEC) ) THEN
                                       IF((EXACT_DIM .AND. (SIZE(VEC).NE.N .OR. LEN(VEC).NE.DIM)) .OR. &
                                          SIZE(VEC) < N  .OR. LEN(VEC) < DIM) THEN
                                                                               NEW_ALLOCATION = TRUE
                                                                               DEALLOCATE(VEC)
                                                                               ALLOCATE(CHARACTER(DIM):: VEC(N))
                                       END IF
                  ELSE
                                                                               ALLOCATE(CHARACTER(DIM):: VEC(N))
                                                                               NEW_ALLOCATION = TRUE
                  END IF
                  VEC = SRC
    ELSEIF(ALLOCATED(VEC)) THEN
                  EXACT_DIM = TRUE
                  IF(PRESENT(EXACT)) EXACT_DIM = EXACT
                  IF(EXACT_DIM) DEALLOCATE(VEC)
    END IF
    !
    IF(PRESENT(NEW_ALLOC)) NEW_ALLOC=NEW_ALLOCATION
    !
  END SUBROUTINE
  !
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !  
  !  RESIZE routines
  ! 
  PURE SUBROUTINE RESIZE_INT_VECTOR(VEC, N, EXACT, DIM)
    !VEC IS VECTOR TO ALLOCATE
    !N IS THE DIMENSION TO ALLOCATE. IF <1 THEN SUBROUTINE DOES NOTHING
    !EXACT IF SET TO TRUE TO INDICATE ALLOCATION MUST MATCH N  (DEFAULT)
    !      IF SET TO FALSE THEN IF ALREADY ALLOCATED AND ALLOCATION IS GREATER THAN N, THEN NOTHING IS DONE
    !DIM IS THE FINAL SIZE OF VEC.
    INTEGER, DIMENSION(:), ALLOCATABLE, INTENT(INOUT):: VEC
    INTEGER,                            INTENT(IN   ):: N
    LOGICAL,                  OPTIONAL, INTENT(IN   ):: EXACT
    INTEGER,                  OPTIONAL, INTENT(  OUT):: DIM
    LOGICAL:: EXACT_DIM
    INTEGER, DIMENSION(:), ALLOCATABLE:: TMP
    INTEGER:: D1, MN
    !
                       EXACT_DIM = TRUE
    IF(PRESENT(EXACT)) EXACT_DIM = EXACT
    !
    IF(N > 0) THEN
                  IF( ALLOCATED(VEC) ) THEN
                                       D1 = SIZE(VEC)
                                       IF(D1.LT.N .OR. (EXACT_DIM .AND. D1.NE.N)) THEN
                                                                                  MN = D1
                                                                                  IF(D1 > N) MN = N
                                                                                  !
                                                                                  CALL MOVE_ALLOC(VEC,TMP)
                                                                                  ALLOCATE(VEC(N))
                                                                                  VEC(1:MN) = TMP(1:MN)
                                                                                  DEALLOCATE(TMP)
                                       END IF
                  ELSE
                                                                                  ALLOCATE(VEC(N))
                  END IF
    ELSEIF(ALLOCATED(VEC) .AND. EXACT_DIM) THEN
                                           DEALLOCATE(VEC)
    END IF
    !
    IF(PRESENT(DIM)) THEN
          IF( ALLOCATED(VEC) ) THEN
              DIM = SIZE(VEC)
          ELSE
              DIM = 0
          END IF
    END IF
    !
  END SUBROUTINE
  !
  !#########################################################################################################################
  !
  PURE SUBROUTINE RESIZE_DBLE_VECTOR(VEC, N, EXACT, DIM)
    !VEC IS VECTOR TO ALLOCATE
    !N IS THE DIMENSION TO ALLOCATE. IF <1 THEN SUBROUTINE DOES NOTHING
    !EXACT IF SET TO TRUE TO INDICATE ALLOCATION MUST MATCH N  (DEFAULT)
    !      IF SET TO FALSE THEN IF ALREADY ALLOCATED AND ALLOCATION IS GREATER THAN N, THEN NOTHING IS DONE
    !DIM IS THE FINAL SIZE OF VEC.
    REAL(REAL64), DIMENSION(:), ALLOCATABLE, INTENT(INOUT):: VEC
    INTEGER,                                 INTENT(IN   ):: N
    LOGICAL,                       OPTIONAL, INTENT(IN   ):: EXACT
    INTEGER,                       OPTIONAL, INTENT(  OUT):: DIM
    LOGICAL:: EXACT_DIM
    REAL(REAL64), DIMENSION(:), ALLOCATABLE:: TMP
    INTEGER:: D1, MN
    !
                       EXACT_DIM = TRUE
    IF(PRESENT(EXACT)) EXACT_DIM = EXACT
    !
    IF(N > 0) THEN
                  IF( ALLOCATED(VEC) ) THEN
                                       D1 = SIZE(VEC)
                                       IF(D1.LT.N .OR. (EXACT_DIM .AND. D1.NE.N)) THEN
                                                                                  MN = D1
                                                                                  IF(D1 > N) MN = N
                                                                                  !
                                                                                  CALL MOVE_ALLOC(VEC,TMP)
                                                                                  ALLOCATE(VEC(N))
                                                                                  VEC(1:MN) = TMP(1:MN)
                                                                                  DEALLOCATE(TMP)
                                       END IF
                  ELSE
                                                                                  ALLOCATE(VEC(N))
                  END IF
    ELSEIF(ALLOCATED(VEC) .AND. EXACT_DIM) THEN
                                           DEALLOCATE(VEC)
    END IF
    !
    IF(PRESENT(DIM)) THEN
          IF( ALLOCATED(VEC) ) THEN
              DIM = SIZE(VEC)
          ELSE
              DIM = 0
          END IF
    END IF
    !
  END SUBROUTINE
  !
  !#########################################################################################################################
  !
  PURE SUBROUTINE RESIZE_TF_VECTOR(VEC, N, EXACT, DIM)
    !VEC IS VECTOR TO ALLOCATE
    !N IS THE DIMENSION TO ALLOCATE. IF <1 THEN SUBROUTINE DOES NOTHING
    !EXACT IF SET TO TRUE TO INDICATE ALLOCATION MUST MATCH N  (DEFAULT)
    !      IF SET TO FALSE THEN IF ALREADY ALLOCATED AND ALLOCATION IS GREATER THAN N, THEN NOTHING IS DONE
    !DIM IS THE FINAL SIZE OF VEC.
    LOGICAL, DIMENSION(:), ALLOCATABLE, INTENT(INOUT):: VEC
    INTEGER,                            INTENT(IN   ):: N
    LOGICAL,                  OPTIONAL, INTENT(IN   ):: EXACT
    INTEGER,                  OPTIONAL, INTENT(  OUT):: DIM
    LOGICAL:: EXACT_DIM
    LOGICAL, DIMENSION(:), ALLOCATABLE:: TMP
    INTEGER:: D1, MN
    !
                       EXACT_DIM = TRUE
    IF(PRESENT(EXACT)) EXACT_DIM = EXACT
    !
    IF(N > 0) THEN
                  IF( ALLOCATED(VEC) ) THEN
                                       D1 = SIZE(VEC)
                                       IF(D1.LT.N .OR. (EXACT_DIM .AND. D1.NE.N)) THEN
                                                                                  MN = D1
                                                                                  IF(D1 > N) MN = N
                                                                                  !
                                                                                  CALL MOVE_ALLOC(VEC,TMP)
                                                                                  ALLOCATE(VEC(N))
                                                                                  VEC(1:MN) = TMP(1:MN)
                                                                                  DEALLOCATE(TMP)
                                       END IF
                  ELSE
                                                                                  ALLOCATE(VEC(N))
                  END IF
    ELSEIF(ALLOCATED(VEC) .AND. EXACT_DIM) THEN
                                           DEALLOCATE(VEC)
    END IF
    !
    IF(PRESENT(DIM)) THEN
          IF( ALLOCATED(VEC) ) THEN
              DIM = SIZE(VEC)
          ELSE
              DIM = 0
          END IF
    END IF
    !
  END SUBROUTINE
  !
  !#########################################################################################################################
  !
  PURE SUBROUTINE RESIZE_CHAR_VECTOR(VEC, N, EXACT, DIM)
    !VEC IS VECTOR TO ALLOCATE
    !N IS THE DIMENSION TO ALLOCATE. IF <1 THEN SUBROUTINE DOES NOTHING
    !EXACT IF SET TO TRUE TO INDICATE ALLOCATION MUST MATCH N  (DEFAULT)
    !      IF SET TO FALSE THEN IF ALREADY ALLOCATED AND ALLOCATION IS GREATER THAN N, THEN NOTHING IS DONE
    !DIM IS THE FINAL SIZE OF VEC.
    CHARACTER(*), DIMENSION(:), ALLOCATABLE, INTENT(INOUT):: VEC
    INTEGER,                            INTENT(IN   ):: N
    LOGICAL,                  OPTIONAL, INTENT(IN   ):: EXACT
    INTEGER,                  OPTIONAL, INTENT(  OUT):: DIM
    LOGICAL:: EXACT_DIM
    CHARACTER(LEN(VEC)), DIMENSION(:), ALLOCATABLE:: TMP
    INTEGER:: D1, MN
    !
                       EXACT_DIM = TRUE
    IF(PRESENT(EXACT)) EXACT_DIM = EXACT
    !
    IF(N > 0) THEN
                  IF( ALLOCATED(VEC) ) THEN
                                       D1 = SIZE(VEC)
                                       IF(D1.LT.N .OR. (EXACT_DIM .AND. D1.NE.N)) THEN
                                                                                  MN = D1
                                                                                  IF(D1 > N) MN = N
                                                                                  !
                                                                                  CALL MOVE_ALLOC(VEC,TMP)
                                                                                  ALLOCATE(VEC(N))
                                                                                  VEC(1:MN) = TMP(1:MN)
                                                                                  DEALLOCATE(TMP)
                                       END IF
                  ELSE
                                                                                  ALLOCATE(VEC(N))
                  END IF
    ELSEIF(ALLOCATED(VEC) .AND. EXACT_DIM) THEN
                                           DEALLOCATE(VEC)
    END IF
    !
    IF(PRESENT(DIM)) THEN
          IF( ALLOCATED(VEC) ) THEN
              DIM = SIZE(VEC)
          ELSE
              DIM = 0
          END IF
    END IF
    !
  END SUBROUTINE
  !
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !  
  !  GROW routines
  ! 
  PURE SUBROUTINE GROW_INT_VECTOR(VEC, N, By, DIM)
    !VEC IS VECTOR TO REALLOCATE IF N<SIZE(VEC)
    !N   IS THE DIMENSION TO CHECK AGAINST.
    !By  IS THE MULTIPLE AT WHICH TO GROW ARRAY IF N<SIZE(VEC); DEFAULT IS 512
    !DIM IS THE FINAL SIZE OF VEC.
    INTEGER, DIMENSION(:), ALLOCATABLE, INTENT(INOUT):: VEC
    INTEGER,                            INTENT(IN   ):: N
    INTEGER,                  OPTIONAL, INTENT(IN   ):: By
    INTEGER,                  OPTIONAL, INTENT(  OUT):: DIM
    INTEGER, DIMENSION(:), ALLOCATABLE:: TMP
    INTEGER:: GROW, D1
    !
                    GROW = 512
    IF(PRESENT(By)) GROW = By
    IF( GROW < 8  ) GROW = 8
    !
    IF(N > 0) THEN
          IF( ALLOCATED(VEC) ) THEN
                               D1 = SIZE(VEC)
                               IF(D1 < N) THEN
                                           GROW = GROW + GROW*(N/GROW)   !Ensure increment beats N by at most GROW indices
                                           CALL MOVE_ALLOC(VEC,TMP)
                                           ALLOCATE(VEC(GROW))
                                           VEC(1:D1) = TMP(1:D1)
                                           DEALLOCATE(TMP)
                               END IF
          ELSE
                                           GROW = GROW + GROW*(N/GROW)   !Ensure increment beats N by at most GROW indices
                                           ALLOCATE(VEC(GROW))
          END IF
    END IF
    !
    IF(PRESENT(DIM)) THEN
          IF( ALLOCATED(VEC) ) THEN
              DIM = SIZE(VEC)
          ELSE
              DIM = 0
          END IF
    END IF
    !
  END SUBROUTINE
  !
  !#########################################################################################################################
  !
  PURE SUBROUTINE GROW_DBLE_VECTOR(VEC, N, By, DIM)
    !VEC IS VECTOR TO REALLOCATE IF N<SIZE(VEC)
    !N   IS THE DIMENSION TO CHECK AGAINST.
    !By  IS THE MULTIPLE AT WHICH TO GROW ARRAY IF N<SIZE(VEC); DEFAULT IS 512
    !DIM IS THE FINAL SIZE OF VEC.
    REAL(REAL64), DIMENSION(:), ALLOCATABLE, INTENT(INOUT):: VEC
    INTEGER,                                 INTENT(IN   ):: N
    INTEGER,                       OPTIONAL, INTENT(IN   ):: By
    INTEGER,                       OPTIONAL, INTENT(  OUT):: DIM
    REAL(REAL64), DIMENSION(:), ALLOCATABLE:: TMP
    INTEGER:: GROW, D1
    !
                    GROW = 512
    IF(PRESENT(By)) GROW = By
    IF( GROW < 8  ) GROW = 8
    !
    IF(N > 0) THEN
          IF( ALLOCATED(VEC) ) THEN
                               D1 = SIZE(VEC)
                               IF(D1 < N) THEN
                                           GROW = GROW + GROW*(N/GROW)   !Ensure increment beats N by at most GROW indices
                                           CALL MOVE_ALLOC(VEC,TMP)
                                           ALLOCATE(VEC(GROW))
                                           VEC(1:D1) = TMP(1:D1)
                                           DEALLOCATE(TMP)
                               END IF
          ELSE
                                           GROW = GROW + GROW*(N/GROW)   !Ensure increment beats N by at most GROW indices
                                           ALLOCATE(VEC(GROW))
          END IF
    END IF
    !
    IF(PRESENT(DIM)) THEN
          IF( ALLOCATED(VEC) ) THEN
              DIM = SIZE(VEC)
          ELSE
              DIM = 0
          END IF
    END IF
    !
  END SUBROUTINE
  !
  !#########################################################################################################################
  !
  PURE SUBROUTINE GROW_TF_VECTOR(VEC, N, By, DIM)
    !VEC IS VECTOR TO REALLOCATE IF N<SIZE(VEC)
    !N   IS THE DIMENSION TO CHECK AGAINST.
    !By  IS THE MULTIPLE AT WHICH TO GROW ARRAY IF N<SIZE(VEC); DEFAULT IS 512
    !DIM IS THE FINAL SIZE OF VEC.
    LOGICAL, DIMENSION(:), ALLOCATABLE, INTENT(INOUT):: VEC
    INTEGER,                            INTENT(IN   ):: N
    INTEGER,                  OPTIONAL, INTENT(IN   ):: By
    INTEGER,                  OPTIONAL, INTENT(  OUT):: DIM
    LOGICAL, DIMENSION(:), ALLOCATABLE:: TMP
    INTEGER:: GROW, D1
    !
                    GROW = 512
    IF(PRESENT(By)) GROW = By
    IF( GROW < 8  ) GROW = 8
    !
    IF(N > 0) THEN
          IF( ALLOCATED(VEC) ) THEN
                               D1 = SIZE(VEC)
                               IF(D1 < N) THEN
                                           GROW = GROW + GROW*(N/GROW)   !Ensure increment beats N by at most GROW indices
                                           CALL MOVE_ALLOC(VEC,TMP)
                                           ALLOCATE(VEC(GROW))
                                           VEC(1:D1) = TMP(1:D1)
                                           DEALLOCATE(TMP)
                               END IF
          ELSE
                                           GROW = GROW + GROW*(N/GROW)   !Ensure increment beats N by at most GROW indices
                                           ALLOCATE(VEC(GROW))
          END IF
    END IF
    !
    IF(PRESENT(DIM)) THEN
          IF( ALLOCATED(VEC) ) THEN
              DIM = SIZE(VEC)
          ELSE
              DIM = 0
          END IF
    END IF
    !
  END SUBROUTINE
  !
  !#########################################################################################################################
  !
  PURE SUBROUTINE GROW_CHAR_VECTOR(VEC, N, By, DIM)
    !VEC IS VECTOR TO REALLOCATE IF N<SIZE(VEC)
    !N   IS THE DIMENSION TO CHECK AGAINST.
    !By  IS THE MULTIPLE AT WHICH TO GROW ARRAY IF N<SIZE(VEC); DEFAULT IS 512
    !DIM IS THE FINAL SIZE OF VEC.
    CHARACTER(*), DIMENSION(:), ALLOCATABLE, INTENT(INOUT):: VEC
    INTEGER,                                 INTENT(IN   ):: N
    INTEGER,                       OPTIONAL, INTENT(IN   ):: By
    INTEGER,                       OPTIONAL, INTENT(  OUT):: DIM
    CHARACTER(LEN(VEC)), DIMENSION(:), ALLOCATABLE:: TMP
    INTEGER:: GROW, D1
    !
                    GROW = 512
    IF(PRESENT(By)) GROW = By
    IF( GROW < 8  ) GROW = 8
    !
    IF(N > 0) THEN
          IF( ALLOCATED(VEC) ) THEN
                               D1 = SIZE(VEC)
                               IF(D1 < N) THEN
                                           GROW = GROW + GROW*(N/GROW)   !Ensure increment beats N by at most GROW indices
                                           CALL MOVE_ALLOC(VEC,TMP)
                                           ALLOCATE(VEC(GROW))
                                           VEC(1:D1) = TMP(1:D1)
                                           DEALLOCATE(TMP)
                               END IF
          ELSE
                                           GROW = GROW + GROW*(N/GROW)   !Ensure increment beats N by at most GROW indices
                                           ALLOCATE(VEC(GROW))
          END IF
    END IF
    !
    IF(PRESENT(DIM)) THEN
          IF( ALLOCATED(VEC) ) THEN
              DIM = SIZE(VEC)
          ELSE
              DIM = 0
          END IF
    END IF
    !
  END SUBROUTINE
  !
  !#########################################################################################################################
  !
END MODULE
!
!