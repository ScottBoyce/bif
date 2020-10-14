!
! CODE DEVELOPED BY SCOTT E BOYCE 
!                   CONTACT <seboyce@usgs.gov> or <Boyce@engineer.com>
!
!
MODULE ADJACENCY_LIST_INSTRUCTION!, ONLY: ADJ_LST, VERTEX_BACKPATH, VERTEX_VERTEX_PATH 
  !
  ! Construct an Adjacency list for vectex points and their connections. It may also include a weight (WT) for each connection.
  !  
  !  Can apply Dijkstra shortest path algorithm to a vertex V1 to all possible connecting points.
  !
  !     Shortest path is optimized for single starting location (V1), but many ending locations (V2)
  !     For many starting locations and advanced path options use MODULE MULTI_DIJKSTRA_SHORTEST_PATH
  !
  ! Usage:
  !   TYPE(ADJ_LST):: ADJ
  !
  !   CALL ADJ%INIT(DIM)                  DIM = Number of Vertex points that are identified from 1 to DIM (for example NSEG)
  !
  !   CALL ADJ%ADD(V1,V2,[WT],[ERR])      Add connection from Vertex V1 to V2 with optional weight WT -- WT could be Segment Length
  !
  !   CALL ADJ%SORT()                     Sorts Adjancy connections, may provide some improvement for being well ordered and clairity for output.
  !                                       For example Vertex 5 could be connected to ADJ%VER(5)%CON = [6, 2, 8]. The sort would ADJ%VER(5)%CON = [2, 6, 8]
  !
  !   CALL ADJ%GET_PATH(V1,V2,PATH,DIM)   Determines shortests path between V1 and V2. The number of Vertexs in path is DIM and hte actual vertexs is in PATH 
  !                                       INTEGER (IN ):: V1, V2
  !                                       INTEGER (OUT):: DIM                => Set to 0 if no backpath exists
  !                                       INTEGER, ALLOCATABLE:: PATH(DIM)
  !
  !   CALL ADJ%SHORTEST_PATH(V1)          Called by ADJ%GET_PATH if V1 does not equal ADJ%V1_BACK_PATH 
  !                                       Sets ADJ%V1_BACK_PATH to V1 and populates ADJ%BACKPATH(DIM) such that the index represents any vertex's next vertex backward towards V1
  USE CONSTANTS
  USE NUM2STR_INTERFACE,        ONLY: NUM2STR
  USE ALLOC_INTERFACE,          ONLY: ALLOC
  USE WARNING_TYPE_INSTRUCTION, ONLY: WARNING_TYPE
  USE SORT_INTERFACE,           ONLY: SORT           ! Calls SORT_1D_MULTI_INT32_REL64!(IVEC,DVEC)
  USE BINARY_HEAP_INSTRUCTION,  ONLY: BINARY_HEAP
  IMPLICIT NONE(TYPE, EXTERNAL)
  PRIVATE
  PUBLIC:: ADJ_LST, VERTEX_BACKPATH, VERTEX_VERTEX_PATH, BUILD_DIJKSTRA_PATH
  !
  TYPE VERTEX_BACKPATH
      LOGICAL:: GOT_BACK = FALSE
      INTEGER:: N  = Z
      INTEGER:: V1 = Z
      INTEGER, DIMENSION(:), ALLOCATABLE:: BACKPATH
      !
      CONTAINS
      !
      PROCEDURE, PASS(VTX):: INIT      => ALLOCATE_VERTEX_BACKPATH !(DIM, V1)
      PROCEDURE, PASS(VTX):: SET_V1    => ALLOCATE_VERTEX_BACKPATH !(DIM, V1)
      PROCEDURE, PASS(VTX):: IS_MATCH  => IS_VERTEX_MATCH !(V1)
      PROCEDURE, PASS(VTX):: MATCHED   => IS_VERTEX_AND_DIM_MATCH !(V1,DIM)
      PROCEDURE, PASS(VTX):: CLOSEST   => CLOSEST_COMMON_VERTEX_BACKPATH!(V2, V3, [JUNCT], [DN2], [DN3]))
      !PROCEDURE, PASS(VTX):: CLOSEST   => CLOSEST_COMMON_VERTEX_BACKPATH!(VTY,    JUNCT,  [DN2], [DN3]))
      PROCEDURE, PASS(VTX):: BACKSTEP  => BACKSTEP_VERTEX_BACKPATH      !(V2) RESULT(BACKSTEP)
      PROCEDURE, PASS(VTX):: NSTEP     => NSTEP_VERTEX_BACKPATH         !(V2)
      FINAL:: FINAL_DEALLOCATE_BINARY_HEAP
  END TYPE
  !
  ! ----------------------------------------------------------------------------------
  !
  TYPE VERTEX_VERTEX_PATH
      INTEGER:: N  = Z
      INTEGER:: V1 = Z
      INTEGER:: V2 = Z
      INTEGER, DIMENSION(:), ALLOCATABLE:: PATH
      !
      CONTAINS
      !
      PROCEDURE, PASS(VTX):: INIT   =>      SET_V_VERTEX_VERTEX_PATH  !(V1, V2)
      PROCEDURE, PASS(VTX):: BUILD  =>  MAKE_PATH_VERTEX_VERTEX_PATH  !(BACKPATH, [REBUILD])
      PROCEDURE, PASS(VTX):: SET_V1 =>     SET_V1_VERTEX_VERTEX_PATH  !(V1) If V1 does not match current V2, then PATH is deallcoated
      PROCEDURE, PASS(VTX):: SET_V2 =>     SET_V2_VERTEX_VERTEX_PATH  !(V2) If V2 does not match current V2, then PATH is deallcoated
      PROCEDURE, PASS(VTX):: ALLOC  =>      ALLOC_VERTEX_VERTEX_PATH
  END TYPE
  !
  ! ----------------------------------------------------------------------------------
  !
  TYPE ADJ_LST_ELM
      INTEGER:: N = Z
      INTEGER,          DIMENSION(:), ALLOCATABLE:: CON  !Connection Vertex
      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE:: WT   !Weight of Connection
  END TYPE
  !
  TYPE ADJ_LST
      INTEGER:: N = Z
      TYPE(ADJ_LST_ELM), DIMENSION(:), ALLOCATABLE:: VER  !Vertex ID
      !
      INTEGER:: V1_BACK_PATH = Z
      INTEGER, DIMENSION(:), ALLOCATABLE:: BACKPATH  !Holdest best backstep for any vertex -- Used by DIJKSTRA to keep track of the vertices which are currently in min HEAP.
      !
      LOGICAL, PRIVATE, DIMENSION(:), ALLOCATABLE:: SPT !Shortest Path Tree -- Used by DIJKSTRA to keep track of the vertices which are currently in min HEAP.
      !
      TYPE(BINARY_HEAP), PRIVATE:: HEAP                 !Scratch Space if GET_PATH/SHORTEST_PATH is invoked
      !
      CONTAINS
      !
      GENERIC             :: INIT           => ALLOCATE_ADJ_LST,        &  !(DIM)  -- Always destroys %VER connections
                                               RESET_CONNECTIONS_ADJ_LST   !()
      PROCEDURE, PASS(ADJ):: DIM            => GET_VER_DIM_ADJ_LST         !()
      PROCEDURE, PASS(ADJ):: ADD            => ADD_CONNECTION_ADJ_LST      !(V1,V2,[WT])  -- Reallocate VER if V1 or V2 > ADJ%N
      PROCEDURE, PASS(ADJ):: SET_CONNECTION => SET_CONNECTION_ADJ_LST      !(V1,V2,[WT])  -- Same as ADD but if V1 or V2 exceeds ADJ%N then raises an errror
      PROCEDURE, PASS(ADJ):: SET_WT         => SET_WT_ADJ_LST              !(V1,V2, WT )
      PROCEDURE, PASS(ADJ):: SCALE_WT       => SCALE_WT_ADJ_LST            !(V1,V2, SCALE )
      PROCEDURE, PASS(ADJ):: GET_WT         => GET_WT_ADJ_LST              !(V1,V2)
      GENERIC             :: SUM_WT         =>          SUM_WT_ADJ_LST, &  !(V1)
                                               SUM_WT_BACKPATH_ADJ_LST     !(V1, BACKPATH)
      PROCEDURE, PASS(ADJ):: SORT           => SORT_CONNECTIONS_ADJ_LST    !()
      GENERIC             :: CLOSEST_COMMON => CLOSEST_COMMON_V1_V2_ADJ_LST,               & ! (V1,   V2,          JUNCT, [UP1], [UP2])
                                               CLOSEST_COMMON_V1_V2_IMAP_ADJ_LST,          & ! (V1,   V2, I, IMAP, JUNCT, [UP1], [UP2]) 
                                               CLOSEST_COMMON_VERTEX_BACKPATH_ADJ_LST,     & ! (VTX, VTY,          JUNCT, [UP1], [UP2])  
                                               CLOSEST_COMMON_IMAP_VERTEX_BACKPATH_ADJ_LST   ! (VTX, VTY, I, IMAP, JUNCT, [UP1], [UP2])
      PROCEDURE, PASS(ADJ):: GET_PATH       => GET_PATH_DIJKSTRA_SHORTEST_PATH!(V1,V2,PATH,DIM)  -- Uses internal array
      GENERIC::              BUILD_BACKPATH => DIJKSTRA_SHORTEST_PATH_VERTEX,          &     ! (V1,                            [REBUILD])
                                               DIJKSTRA_SHORTEST_PATH_BACKPATH,        &     ! (V1, BACKPATH, [INTERNAL_COPY]           )
                                               DIJKSTRA_SHORTEST_PATH_VERTEX_BACKPATH, &     ! (    VTX,      [INTERNAL_COPY]           ) -- Assumes VTX%V1 is set to vertix
                                               DIJKSTRA_SHORTEST_PATH_VERTEX_VERTEX_BACKPATH ! (V1, VTX,      [INTERNAL_COPY], [REBUILD])
      !
      PROCEDURE, PASS(ADJ), PRIVATE:: ALLOCATE_ADJ_LST         !(DIM)
      PROCEDURE, PASS(ADJ), PRIVATE:: RESET_CONNECTIONS_ADJ_LST!()
      PROCEDURE, PASS(ADJ), PRIVATE:: DIJKSTRA_SHORTEST_PATH_VERTEX                 ! (V1,                            [REBUILD])
      PROCEDURE, PASS(ADJ), PRIVATE:: DIJKSTRA_SHORTEST_PATH_BACKPATH               ! (V1, BACKPATH, [INTERNAL_COPY]           )
      PROCEDURE, PASS(ADJ), PRIVATE:: DIJKSTRA_SHORTEST_PATH_VERTEX_BACKPATH        ! (    VTX,      [INTERNAL_COPY]           ) -- Assumes VTX%V1 is set to vertix
      PROCEDURE, PASS(ADJ), PRIVATE:: DIJKSTRA_SHORTEST_PATH_VERTEX_VERTEX_BACKPATH ! (V1, VTX,      [INTERNAL_COPY], [REBUILD])
      PROCEDURE, PASS(ADJ), PRIVATE:: CLOSEST_COMMON_V1_V2_ADJ_LST                  ! (V1,   V2,          JUNCT, [UP1], [UP2])
      PROCEDURE, PASS(ADJ), PRIVATE:: CLOSEST_COMMON_V1_V2_IMAP_ADJ_LST             ! (V1,   V2, I, IMAP, JUNCT, [UP1], [UP2])
      PROCEDURE, PASS(ADJ), PRIVATE:: CLOSEST_COMMON_VERTEX_BACKPATH_ADJ_LST        ! (VTX, VTY,          JUNCT, [UP1], [UP2])
      PROCEDURE, PASS(ADJ), PRIVATE:: CLOSEST_COMMON_IMAP_VERTEX_BACKPATH_ADJ_LST   ! (VTX, VTY, I, IMAP, JUNCT, [UP1], [UP2])
      PROCEDURE, PASS(ADJ), PRIVATE:: SUM_WT_ADJ_LST           !(V1)
      PROCEDURE, PASS(ADJ), PRIVATE:: SUM_WT_BACKPATH_ADJ_LST  !(V1, BACKPATH)
  END TYPE
  !
  ! ----------------------------------------------------------------------------------
  !
  INTERFACE BUILD_DIJKSTRA_PATH
      MODULE PROCEDURE BUILD_DIJKSTRA_PATH_VERTEX_VERTEX_PATH     !(VTX, VPATH)
      MODULE PROCEDURE BUILD_DIJKSTRA_PATH_V2_VERTEX_VERTEX_PATH  !(V2, VTX, VPATH)  
      MODULE PROCEDURE BUILD_DIJKSTRA_PATH_BACKPATH               !(V2, BACKPATH, PATH, DIM) 
  END INTERFACE
  !
  ! ----------------------------------------------------------------------------------
  !
  INTERFACE   ! VERTEX_VERTEX_PATH_SUBS
          !
      MODULE PURE SUBROUTINE MAKE_PATH_VERTEX_VERTEX_PATH(VTX, BACKPATH, REBUILD)
        CLASS(VERTEX_VERTEX_PATH), INTENT(INOUT):: VTX
        TYPE(VERTEX_BACKPATH),     INTENT(IN   ):: BACKPATH
        LOGICAL,         OPTIONAL, INTENT(IN   ):: REBUILD
      END SUBROUTINE
      !
      MODULE PURE SUBROUTINE EXTEND_VERTEX_VERTEX_PATH(VTX, V1, V2)
        TYPE(VERTEX_VERTEX_PATH), DIMENSION(:), ALLOCATABLE, INTENT(INOUT):: VTX
        INTEGER,                                             INTENT(IN   ):: V1, V2
      END SUBROUTINE
      !
      MODULE ELEMENTAL PURE SUBROUTINE COPY_VERTEX_VERTEX_PATH(VTX_OUT,VTX_IN)
        CLASS(VERTEX_VERTEX_PATH), INTENT(IN   ):: VTX_IN
        CLASS(VERTEX_VERTEX_PATH), INTENT(INOUT):: VTX_OUT
      END SUBROUTINE
      !
      MODULE ELEMENTAL PURE SUBROUTINE MOVE_VERTEX_VERTEX_PATH(VTX_IN, VTX_OUT)
        CLASS(VERTEX_VERTEX_PATH), INTENT(INOUT):: VTX_IN
        CLASS(VERTEX_VERTEX_PATH), INTENT(INOUT):: VTX_OUT
      END SUBROUTINE
      !
      MODULE PURE SUBROUTINE ALLOC_VERTEX_VERTEX_PATH(VTX, N)
        CLASS(VERTEX_VERTEX_PATH), INTENT(INOUT):: VTX
        INTEGER,                   INTENT(IN   ):: N
      END SUBROUTINE
      !
      MODULE ELEMENTAL PURE SUBROUTINE SET_V_VERTEX_VERTEX_PATH(VTX, V1, V2, NEW_ALLOC)
        CLASS(VERTEX_VERTEX_PATH),                     INTENT(INOUT):: VTX
        INTEGER,                                       INTENT(IN   ):: V1, V2
        LOGICAL,                             OPTIONAL, INTENT(  OUT):: NEW_ALLOC
      END SUBROUTINE
      !
      MODULE PURE SUBROUTINE SET_V1_VERTEX_VERTEX_PATH(VTX, V1, NEW_ALLOC)
        CLASS(VERTEX_VERTEX_PATH),                     INTENT(INOUT):: VTX
        INTEGER,                                       INTENT(IN   ):: V1
        LOGICAL,                             OPTIONAL, INTENT(  OUT):: NEW_ALLOC
      END SUBROUTINE
      !
      MODULE PURE SUBROUTINE SET_V2_VERTEX_VERTEX_PATH(VTX, V2, NEW_ALLOC)
        CLASS(VERTEX_VERTEX_PATH),                     INTENT(INOUT):: VTX
        INTEGER,                                       INTENT(IN   ):: V2
        LOGICAL,                             OPTIONAL, INTENT(  OUT):: NEW_ALLOC
      END SUBROUTINE
  END INTERFACE
  !
  ! ----------------------------------------------------------------------------------
  !
  INTERFACE  !VERTEX_BACKPATH_SUBS
      !
      MODULE PURE FUNCTION IS_VERTEX_MATCH(VTX, V1) RESULT(IS_MATCH)
        CLASS(VERTEX_BACKPATH), INTENT(IN):: VTX
        INTEGER,                INTENT(IN):: V1
        LOGICAL:: IS_MATCH
      END FUNCTION
      !
      MODULE PURE FUNCTION IS_VERTEX_AND_DIM_MATCH(VTX, V1, DIM) RESULT(IS_MATCH)
        CLASS(VERTEX_BACKPATH), INTENT(IN):: VTX
        INTEGER,                INTENT(IN):: V1, DIM
        LOGICAL:: IS_MATCH
      END FUNCTION
      !
      MODULE PURE SUBROUTINE ALLOCATE_VERTEX_BACKPATH(VTX, DIM, V1)
        CLASS(VERTEX_BACKPATH), INTENT(INOUT):: VTX
        INTEGER,                INTENT(IN   ):: DIM
        INTEGER,  OPTIONAL,     INTENT(IN   ):: V1
      END SUBROUTINE
      !
      MODULE PURE SUBROUTINE SET_V1_VERTEX_BACKPATH(VTX, V1)
        CLASS(VERTEX_BACKPATH), INTENT(INOUT):: VTX
        INTEGER,                INTENT(IN   ):: V1
      END SUBROUTINE
      !
      MODULE PURE SUBROUTINE CLOSEST_COMMON_VERTEX_BACKPATH(VTX, V2, V3, JUNCT, DN2, DN3)
        CLASS(VERTEX_BACKPATH), INTENT(IN   ):: VTX
        INTEGER,                INTENT(IN   ):: V2, V3
        INTEGER,     OPTIONAL,  INTENT(INOUT):: JUNCT, DN2, DN3
      END SUBROUTINE
      !
      MODULE PURE FUNCTION BACKSTEP_VERTEX_BACKPATH(VTX, V2) RESULT(BACKSTEP)
        CLASS(VERTEX_BACKPATH), INTENT(IN):: VTX
        INTEGER,                INTENT(IN):: V2
        INTEGER:: BACKSTEP
      END FUNCTION
      !
      MODULE PURE FUNCTION NSTEP_VERTEX_BACKPATH(VTX, V2) RESULT(DIM)
        CLASS(VERTEX_BACKPATH), INTENT(IN):: VTX
        INTEGER,                INTENT(IN):: V2
        INTEGER:: DIM
      END FUNCTION
      !
      MODULE PURE ELEMENTAL SUBROUTINE FINAL_DEALLOCATE_BINARY_HEAP(VTX)
        TYPE(VERTEX_BACKPATH), INTENT(INOUT):: VTX
      END SUBROUTINE
  END INTERFACE
  !
  CONTAINS
  !
  ! ----------------------------------------------------------------------------------
  !
  PURE SUBROUTINE DIJKSTRA_SHORTEST_PATH_VERTEX(ADJ, V1, REBUILD)
    CLASS(ADJ_LST),                                INTENT(INOUT):: ADJ
    INTEGER,                                       INTENT(IN   ):: V1
    LOGICAL,                             OPTIONAL, INTENT(IN   ):: REBUILD
    LOGICAL:: FORCE_REBUILD, NEW_ALLOC
    !
    IF(ADJ%N > Z .AND. V1 > Z .AND. V1 <= ADJ%N) THEN
                  !
                  IF(PRESENT(REBUILD)) THEN; FORCE_REBUILD = REBUILD
                  ELSE;                      FORCE_REBUILD = FALSE
                  END IF
                  !
                  CALL ALLOC(ADJ%SPT,      ADJ%N, NEW_ALLOC=NEW_ALLOC)
                  CALL ALLOC(ADJ%BACKPATH, ADJ%N, NEW_ALLOC=NEW_ALLOC)
                  !
                  IF(ADJ%V1_BACK_PATH .NE. V1 .OR. NEW_ALLOC .OR. FORCE_REBUILD)  CALL DIJKSTRA_SHORTEST_PATH(ADJ, V1, ADJ%BACKPATH, ADJ%SPT)
    END IF
    !
  END SUBROUTINE
  !
  PURE SUBROUTINE DIJKSTRA_SHORTEST_PATH_BACKPATH(ADJ, V1, BACKPATH, INTERNAL_COPY)
    CLASS(ADJ_LST),                     INTENT(INOUT):: ADJ
    INTEGER,                            INTENT(IN   ):: V1
    INTEGER, DIMENSION(:), ALLOCATABLE, INTENT(INOUT):: BACKPATH
    LOGICAL,                  OPTIONAL, INTENT(IN   ):: INTERNAL_COPY
    INTEGER:: V1_BAK
    !
    IF(ADJ%N > Z .AND. V1 > Z .AND. V1 <= ADJ%N) THEN
                  !
                  V1_BAK = ADJ%V1_BACK_PATH
                  !
                  CALL ALLOC(ADJ%SPT, ADJ%N)
                  !
                  CALL ALLOC(BACKPATH, ADJ%N)
                  !
                  CALL DIJKSTRA_SHORTEST_PATH(ADJ,V1,BACKPATH, ADJ%SPT)
                  !
                  ADJ%V1_BACK_PATH = V1_BAK !No local storage of path
                  !
                  IF(PRESENT(INTERNAL_COPY)) THEN
                          IF(INTERNAL_COPY) THEN
                              ADJ%V1_BACK_PATH = V1
                              CALL ALLOC(ADJ%BACKPATH, ADJ%N, SRC=BACKPATH)
                          END IF
                  END IF
    ELSE
        IF(ALLOCATED(BACKPATH)) THEN
                                 BACKPATH = Z
        ELSE
            ALLOCATE(BACKPATH(ONE), SOURCE=Z)
        END IF
    END IF
    !
  END SUBROUTINE
  !
  PURE SUBROUTINE DIJKSTRA_SHORTEST_PATH_VERTEX_VERTEX_BACKPATH(ADJ, V1, VTX, INTERNAL_COPY, REBUILD)
    CLASS(ADJ_LST),        INTENT(INOUT):: ADJ
    INTEGER,               INTENT(IN   ):: V1
    TYPE(VERTEX_BACKPATH), INTENT(INOUT):: VTX
    LOGICAL,     OPTIONAL, INTENT(IN   ):: INTERNAL_COPY, REBUILD
    INTEGER:: V1_BAK
    LOGICAL:: FORCE_REBUILD
    !
    IF(ADJ%N > Z .AND. V1 > Z .AND. V1 <= ADJ%N) THEN
                  !
                  IF(PRESENT(REBUILD)) THEN; FORCE_REBUILD = REBUILD
                  ELSE;                      FORCE_REBUILD = FALSE
                  END IF
                  !
                  IF(.NOT. IS_VERTEX_AND_DIM_MATCH(VTX, V1, ADJ%N) .OR. FORCE_REBUILD .OR. .NOT. VTX%GOT_BACK) THEN
                       !
                       V1_BAK = ADJ%V1_BACK_PATH
                       !
                       CALL VTX%INIT(ADJ%N, V1)
                       !
                       CALL ALLOC(ADJ%SPT, ADJ%N)
                       CALL DIJKSTRA_SHORTEST_PATH(ADJ, VTX%V1, VTX%BACKPATH, ADJ%SPT)
                       VTX%GOT_BACK = TRUE
                       !
                       ADJ%V1_BACK_PATH = V1_BAK
                       !
                       IF(PRESENT(INTERNAL_COPY)) THEN
                               IF(INTERNAL_COPY) THEN
                                                 ADJ%V1_BACK_PATH = VTX%V1
                                                 CALL ALLOC(ADJ%BACKPATH, ADJ%N, SRC=VTX%BACKPATH)
                               END IF
                       END IF
                  END IF
    ELSE
        CALL ALLOCATE_VERTEX_BACKPATH(VTX, Z)
    END IF
    !
  END SUBROUTINE
  !
  PURE SUBROUTINE DIJKSTRA_SHORTEST_PATH_VERTEX_BACKPATH(ADJ, VTX, INTERNAL_COPY) !assumes V1 is set
    CLASS(ADJ_LST),        INTENT(INOUT):: ADJ
    TYPE(VERTEX_BACKPATH), INTENT(INOUT):: VTX
    LOGICAL,     OPTIONAL, INTENT(IN   ):: INTERNAL_COPY
    INTEGER:: V1_BAK
    !
    IF(ADJ%N > Z .AND. VTX%V1 > Z .AND. VTX%V1 <= ADJ%N) THEN
                  !
                  V1_BAK = ADJ%V1_BACK_PATH
                  VTX%N = ADJ%N
                  !
                  CALL ALLOC(VTX%BACKPATH, ADJ%N)  !Ensure that it has been allocated
                  !
                  CALL ALLOC(ADJ%SPT, ADJ%N)
                  CALL DIJKSTRA_SHORTEST_PATH(ADJ, VTX%V1, VTX%BACKPATH, ADJ%SPT)
                  VTX%GOT_BACK = TRUE
                  !
                  ADJ%V1_BACK_PATH = V1_BAK !No local storage of path
                  !
                  IF(PRESENT(INTERNAL_COPY)) THEN
                          IF(INTERNAL_COPY) THEN
                                            ADJ%V1_BACK_PATH = VTX%V1
                                            CALL ALLOC(ADJ%BACKPATH, ADJ%N, SRC=VTX%BACKPATH)
                          END IF
                  END IF
    ELSE
        V1_BAK = VTX%V1
        CALL ALLOCATE_VERTEX_BACKPATH(VTX, Z, V1_BAK)
    END IF
    !
  END SUBROUTINE
  !
  !PURE SUBROUTINE GET_PATH_DIJKSTRA_SHORTEST_PATH_MAIN(ADJ,V1,V2,PATH,DIM,BACKPATH)
  !  CLASS(ADJ_LST),                                INTENT(INOUT):: ADJ
  !  INTEGER,                                       INTENT(IN   ):: V1,V2
  !  INTEGER,                                       INTENT(  OUT):: DIM
  !  INTEGER, DIMENSION(:), ALLOCATABLE,            INTENT(INOUT):: PATH
  !  INTEGER, DIMENSION(:), ALLOCATABLE,  OPTIONAL, INTENT(INOUT):: BACKPATH
  !  !
  !  IF(ADJ%N > Z) THEN
  !                CALL ALLOC(ADJ%SPT, ADJ%N)
  !                !
  !                IF(PRESENT(BACKPATH)) THEN
  !                                      CALL ALLOC(BACKPATH, ADJ%N)
  !                                      CALL DIJKSTRA_SHORTEST_PATH(ADJ,V1,BACKPATH, ADJ%SPT)
  !                                      !
  !                                      ADJ%V1_BACK_PATH = Z !No local storage of path
  !                ELSE
  !                                      CALL ALLOC(ADJ%BACKPATH, ADJ%N)
  !                                      CALL DIJKSTRA_SHORTEST_PATH(ADJ,V1,ADJ%BACKPATH, ADJ%SPT)
  !                END IF
  !  ELSEIF(PRESENT(BACKPATH)) THEN
  !                            IF(ALLOCATED(BACKPATH)) THEN
  !                                                     BACKPATH = Z
  !                            ELSE
  !                                ALLOCATE(BACKPATH(ONE), SOURCE=Z)
  !                            END IF
  !  END IF
  !  !
  !END SUBROUTINE
  !
  ! ----------------------------------------------------------------------------------
  !
  PURE SUBROUTINE DIJKSTRA_SHORTEST_PATH(ADJ,V1,BACKPATH, SPT)
    CLASS(ADJ_LST),            INTENT(INOUT):: ADJ
    INTEGER,                   INTENT(IN   ):: V1
    INTEGER, DIMENSION(ADJ%N), INTENT(INOUT):: BACKPATH
    LOGICAL, DIMENSION(ADJ%N), INTENT(INOUT):: SPT
    INTEGER:: I,U,V,P
    DOUBLE PRECISION:: DIST_U, DIST_V, DIST 
    !
    ADJ%V1_BACK_PATH = V1  !Keep track of where BACKPATH CURRENTLY POINTS TOO.
    !
    SPT      = TRUE   !If FALSE then point is in the tree
    BACKPATH = Z      !Non-Zero when point exists
    !
    IF( V1 > Z ) THEN
        CALL ADJ%HEAP%DIJKSTRA_INIT(V1, ADJ%N)  !Set up HEAP For DIJKSTRA Algorithm 
        !
        DO WHILE (ADJ%HEAP%SIZE() > Z)
            !
            CALL ADJ%HEAP%POP(U, DIST_U)  !GET MIN FROM HEAP
            !
            IF(U > Z .AND. DIST_U < near_inf) THEN !dist is < inf
                !
                SPT(U) = FALSE  !Point is being explored so add it to the shortest path
                !
                DO I=ONE, ADJ%VER(U)%N
                    !
                    V = ADJ%VER(U)%CON(I)
                    !
                    IF(V > Z) THEN
                       !
                       IF(SPT(V)) THEN  !Have not visited point before
                            !
                            DIST = DIST_U + ADJ%VER(U)%WT(I)
                            !
                            CALL ADJ%HEAP%GET(V,DIST_V,P)
                            !
                            IF( DIST < DIST_V) THEN
                                BACKPATH(V) = U
                                !ADJ%HEAP%CHILD(P)%PNT%X = DIST  !Update with new lower distance
                                CALL ADJ%HEAP%SET_X(P,DIST)      !Update with new lower distance
                            END IF
                       END IF
                    END IF
                END DO
            ELSE
                EXIT
            END IF
            !
        END DO
    END IF
  END SUBROUTINE
  !
  ! ----------------------------------------------------------------------------------
  !
  PURE SUBROUTINE GET_PATH_DIJKSTRA_SHORTEST_PATH(ADJ,V1,V2,PATH,DIM)
    CLASS(ADJ_LST),                      INTENT(INOUT):: ADJ
    INTEGER,                             INTENT(IN   ):: V1,V2
    INTEGER,                             INTENT(  OUT):: DIM
    INTEGER, DIMENSION(:), ALLOCATABLE,  INTENT(INOUT):: PATH
    INTEGER:: I,J
    !
    IF(ADJ%V1_BACK_PATH .NE. V1) CALL DIJKSTRA_SHORTEST_PATH_VERTEX(ADJ, V1)
    !
    IF(ADJ%BACKPATH(V2) == Z) THEN
        DIM = Z
        RETURN
    END IF
    !
    DIM = ONE
    I   = V2
    DO WHILE ( ADJ%BACKPATH(I) > Z )
            DIM = DIM + ONE
            !
            I = ADJ%BACKPATH(I)
    END DO
    !
    CALL ALLOC(PATH,DIM)
    !
    I       = V2
    J       = DIM
    DO J=DIM, ONE, NEG
            !
            PATH(J) = I
            !
            I = ADJ%BACKPATH(I)
    END DO
    !
  END SUBROUTINE
  !
  ! ----------------------------------------------------------------------------------
  !
  PURE SUBROUTINE CLOSEST_COMMON_VERTEX_BACKPATH_ADJ_LST(ADJ, VTX, VTY, JUNCT, UP1, UP2)
    CLASS(ADJ_LST),         INTENT(INOUT):: ADJ
    TYPE(VERTEX_BACKPATH),  INTENT(INOUT):: VTX, VTY
    INTEGER,                INTENT(  OUT):: JUNCT
    INTEGER,      OPTIONAL, INTENT(  OUT):: UP1, UP2
    !
    IF(VTX%N .NE. ADJ%N) CALL ADJ%BUILD_BACKPATH(VTX)
    IF(VTY%N .NE. ADJ%N) CALL ADJ%BUILD_BACKPATH(VTY)
    !
    ASSOCIATE(V1 => VTX%V1, DIM1 => VTX%N, BACK1 => VTX%BACKPATH, &
              V2 => VTY%V1, DIM2 => VTY%N, BACK2 => VTY%BACKPATH   )
             !
       IF    (V1 == V2) THEN
                                JUNCT = V1
       ELSEIF(ADJ%N <  TWO) THEN
                                JUNCT = Z
       ELSE
           JUNCT = Z
           CALL ALLOC(ADJ%SPT, ADJ%N)
           !
           IF(BACK2(V1) > Z) THEN !V1 is downstream of V2
               !
               CALL CLOSEST_COMMON_ADJ_LST(ADJ, V2, V1, BACK2, ADJ%SPT, JUNCT)
               !
           ELSE! V1 is upstream of V2 or common is somewhere downstream
               !
               CALL CLOSEST_COMMON_ADJ_LST(ADJ, V1, V2, BACK1, ADJ%SPT, JUNCT)
           END IF
           !
       END IF
       !
       IF(JUNCT > Z) THEN
                     IF(PRESENT(UP1)) UP1 = BACK1(JUNCT)
                     IF(PRESENT(UP2)) UP2 = BACK2(JUNCT)
       ELSE
                     IF(PRESENT(UP1)) UP1 = Z
                     IF(PRESENT(UP2)) UP2 = Z
       END IF
       !
    END ASSOCIATE
    !
  END SUBROUTINE
  !
  PURE SUBROUTINE CLOSEST_COMMON_IMAP_VERTEX_BACKPATH_ADJ_LST(ADJ, VTX, VTY, I, IMAP, JUNCT, UP1, UP2)
    CLASS(ADJ_LST),            INTENT(INOUT):: ADJ
    TYPE(VERTEX_BACKPATH),     INTENT(INOUT):: VTX, VTY
    INTEGER,                   INTENT(IN   ):: I
    INTEGER, DIMENSION(ADJ%N), INTENT(IN   ):: IMAP
    INTEGER,                   INTENT(  OUT):: JUNCT
    INTEGER,         OPTIONAL, INTENT(  OUT):: UP1, UP2
    !
    IF(VTX%N .NE. ADJ%N) CALL ADJ%BUILD_BACKPATH(VTX)
    IF(VTY%N .NE. ADJ%N) CALL ADJ%BUILD_BACKPATH(VTY)
    !                         
    ASSOCIATE(V1 => VTX%V1, DIM1 => VTX%N, BACK1 => VTX%BACKPATH, &
              V2 => VTY%V1, DIM2 => VTY%N, BACK2 => VTY%BACKPATH   )
             !
       IF    (V1 == V2) THEN
                                JUNCT = V1
       ELSEIF(ADJ%N <  TWO) THEN
                                JUNCT = Z
       ELSE
           JUNCT = Z
           CALL ALLOC(ADJ%SPT, ADJ%N)
           !
           IF(BACK2(V1) > Z) THEN !V1 is downstream of V2
               !
               CALL CLOSEST_COMMON_IMAP_ADJ_LST(ADJ, V2, V1, I, IMAP, BACK2, ADJ%SPT, JUNCT)
               !
           ELSE! V1 is upstream of V2 or common is somewhere downstream
               !
               CALL CLOSEST_COMMON_IMAP_ADJ_LST(ADJ, V1, V2, I, IMAP, BACK1, ADJ%SPT, JUNCT)
           END IF
           !
       END IF
       !
       IF(JUNCT > Z) THEN
                     IF(PRESENT(UP1)) UP1 = BACK1(JUNCT)
                     IF(PRESENT(UP2)) UP2 = BACK2(JUNCT)
       ELSE
                     IF(PRESENT(UP1)) UP1 = Z
                     IF(PRESENT(UP2)) UP2 = Z
       END IF
       !
    END ASSOCIATE
    !
  END SUBROUTINE
  !
  ! ----------------------------------------------------------------------------------
  !
  PURE SUBROUTINE CLOSEST_COMMON_V1_V2_ADJ_LST(ADJ, V1, V2, JUNCT, UP1, UP2)
    CLASS(ADJ_LST),         INTENT(INOUT):: ADJ
    INTEGER,                INTENT(IN   ):: V1, V2
    INTEGER,                INTENT(INOUT):: JUNCT
    INTEGER,      OPTIONAL, INTENT(INOUT):: UP1, UP2
    INTEGER:: P1, P2
    INTEGER, DIMENSION(:), ALLOCATABLE:: BACKPATH
    !
    IF    (V1 == V2) THEN
                             JUNCT = V1
    ELSEIF(ADJ%N <  TWO) THEN
                             JUNCT = Z
    ELSE
        IF(V2 == ADJ%V1_BACK_PATH) THEN
            P1 = V2
            P2 = V1
        ELSE
            P1 = V1
            P2 = V2
        END IF
        JUNCT = Z
        !
        CALL DIJKSTRA_SHORTEST_PATH_VERTEX  (ADJ, P1)
        CALL DIJKSTRA_SHORTEST_PATH_BACKPATH(ADJ, P2, BACKPATH)
        !
        IF(BACKPATH(P1) > Z) THEN !P1 is downstream of P2
                                  !
                                  CALL CLOSEST_COMMON_ADJ_LST(ADJ, P2, P1,     BACKPATH, ADJ%SPT, JUNCT)
                                  !
        ELSE! P1 is upstream of P2 or common is somewhere downstream
                                  !
                                  CALL CLOSEST_COMMON_ADJ_LST(ADJ, P1, P2, ADJ%BACKPATH, ADJ%SPT, JUNCT)
        END IF
        !
    END IF
    !
    IF(PRESENT(UP1) .OR. PRESENT(UP2)) THEN
       IF(JUNCT > Z) THEN
           IF(V2 == ADJ%V1_BACK_PATH) THEN
               P1 =     BACKPATH(JUNCT)
               P2 = ADJ%BACKPATH(JUNCT)
           ELSE
               P1 = ADJ%BACKPATH(JUNCT)
               P2 =     BACKPATH(JUNCT)
           END IF
       ELSE
           P1 = Z
           P2 = Z
       END IF
       !
       IF(PRESENT(UP1)) UP1 = P1
       IF(PRESENT(UP2)) UP2 = P2
       !
    END IF
    !
  END SUBROUTINE
  !
  !V2 cannot be upstream of V1, BACKPATH is for V1
  !Assumes that V1 /= V2 and ADJ%N > 1
  !This will return V2 if it is the closest path to V1
  PURE SUBROUTINE CLOSEST_COMMON_ADJ_LST(ADJ, V1, V2, BACKPATH, SPT, JUNCT) 
    CLASS(ADJ_LST),            INTENT(INOUT):: ADJ
    INTEGER,                   INTENT(IN   ):: V1, V2
    INTEGER, DIMENSION(ADJ%N), INTENT(IN   ):: BACKPATH
    LOGICAL, DIMENSION(ADJ%N), INTENT(INOUT):: SPT
    INTEGER,                   INTENT(INOUT):: JUNCT
    INTEGER:: I,U,V,P
    DOUBLE PRECISION:: COM_DIST
    DOUBLE PRECISION:: DIST_U, DIST_V, DIST 
    !
    JUNCT = Z
    COM_DIST = inf
    !
    SPT = TRUE
    !
    CALL ADJ%HEAP%DIJKSTRA_INIT(V2, ADJ%N)  !Set up HEAP 
    !
    DO WHILE (ADJ%HEAP%SIZE() > Z)
        !
        CALL ADJ%HEAP%POP(U, DIST_U)  !GET MIN FROM HEAP  (FIRST POP IS V2)
        !
        IF(U > Z .AND. DIST_U < near_inf) THEN !dist is < inf
            !
            IF(BACKPATH(U) > Z .AND. DIST_U < COM_DIST) THEN  !TRUE if direct path to V1
                  !
                  DIST = DIST_U + ADJ%SUM_WT(U, BACKPATH)
                  !
                  IF(DIST < COM_DIST) THEN
                      !
                      COM_DIST = DIST
                      JUNCT      = U
                  END IF
            END IF
            !
            SPT(U) = FALSE  !Point is being explored so add it to the shortest path
            !
            DO I=ONE, ADJ%VER(U)%N
                !
                V = ADJ%VER(U)%CON(I)
                !
                IF(V > Z) THEN
                   !
                   IF(SPT(V)) THEN  !Have not visited point before
                        !
                        DIST = DIST_U + ADJ%VER(U)%WT(I)
                        !
                        CALL ADJ%HEAP%GET(V,DIST_V,P)
                        !
                        IF( DIST < DIST_V)  CALL ADJ%HEAP%SET_X(P,DIST)      !Update with new lower distance
                   END IF
                END IF
            END DO
        ELSE
            EXIT
        END IF
        !
    END DO
    !
  END SUBROUTINE
  !
  PURE FUNCTION SUM_WT_ADJ_LST(ADJ, V1)  RESULT(DIST)
    CLASS(ADJ_LST),            INTENT(IN):: ADJ
    INTEGER,                   INTENT(IN):: V1
    DOUBLE PRECISION:: DIST
    !
    DIST = DZ
    !
    IF(ADJ%V1_BACK_PATH > Z) DIST = SUM_WT_BACKPATH_ADJ_LST(ADJ, V1, ADJ%BACKPATH) 
    !
  END FUNCTION
  !
  PURE FUNCTION SUM_WT_BACKPATH_ADJ_LST(ADJ, V1, BACKPATH)  RESULT(DIST)
    CLASS(ADJ_LST),            INTENT(IN):: ADJ
    INTEGER,                   INTENT(IN):: V1
    INTEGER, DIMENSION(ADJ%N), INTENT(IN):: BACKPATH
    DOUBLE PRECISION:: DIST
    INTEGER:: I, J
    !
    DIST = DZ
    !
    IF(Z < V1 .AND. V1 <= ADJ%N) THEN
        !
        J = V1
        I = BACKPATH(J)
        !
        DO WHILE(I > Z)
            !
            DIST = DIST + ADJ%GET_WT(I, J, DZ)
            !
            J = I
            I = BACKPATH(J)
            !
        END DO
    END IF
    !
  END FUNCTION
  !
  PURE FUNCTION GET_WT_ADJ_LST(ADJ, V1, V2, ERROR_VAL)  RESULT(WT)
    CLASS(ADJ_LST),             INTENT(IN):: ADJ
    INTEGER,                    INTENT(IN):: V1, V2
    DOUBLE PRECISION, OPTIONAL, INTENT(IN):: ERROR_VAL
    DOUBLE PRECISION:: WT
    INTEGER:: J
    !
    IF(PRESENT(ERROR_VAL)) THEN
          WT = ERROR_VAL
    ELSE
          WT = DNEG        ! = -1 is an error condition
    END IF
    !
    IF(  Z < V1 .AND. V1 <= ADJ%N .AND.  &
       NEG < V2 .AND. V2 <= ADJ%N .AND. ADJ%VER(V1)%N > Z ) THEN
                      !
                      DO J = ONE, ADJ%VER(V1)%N
                               IF(ADJ%VER(V1)%CON(J) == V2) WT = ADJ%VER(V1)%WT(J)
                      END DO
                      !
    END IF
    !
  END FUNCTION
  !
  ! ----------------------------------------------------------------------------------
  !
  PURE SUBROUTINE CLOSEST_COMMON_V1_V2_IMAP_ADJ_LST(ADJ, V1, V2, I, IMAP, JUNCT, UP1, UP2)
    CLASS(ADJ_LST),            INTENT(INOUT):: ADJ
    INTEGER,                   INTENT(IN   ):: V1, V2, I
    INTEGER, DIMENSION(ADJ%N), INTENT(IN   ):: IMAP
    INTEGER,                   INTENT(INOUT):: JUNCT
    INTEGER,         OPTIONAL, INTENT(INOUT):: UP1, UP2
    INTEGER:: P1, P2 
    INTEGER, DIMENSION(:), ALLOCATABLE:: BACKPATH
    !
    IF    (V1 == V2) THEN
                             JUNCT = V1
    ELSEIF(ADJ%N <  TWO) THEN
                             JUNCT = Z
    ELSE
        IF(V2 == ADJ%V1_BACK_PATH) THEN
            P1 = V2
            P2 = V1
        ELSE
            P1 = V1
            P2 = V2
        END IF
        JUNCT = Z
        !
        CALL DIJKSTRA_SHORTEST_PATH_VERTEX  (ADJ, P1)
        CALL DIJKSTRA_SHORTEST_PATH_BACKPATH(ADJ, P2, BACKPATH)
        !
        IF(BACKPATH(P1) > Z) THEN !P1 is downstream of P2
                                  !
                                  CALL CLOSEST_COMMON_IMAP_ADJ_LST(ADJ, P2, P1, I, IMAP,     BACKPATH, ADJ%SPT, JUNCT)
                                  !
        ELSE! P1 is upstream of P2 or common is somewhere downstream
                                  !
                                  CALL CLOSEST_COMMON_IMAP_ADJ_LST(ADJ, P1, P2, I, IMAP, ADJ%BACKPATH, ADJ%SPT, JUNCT)
        END IF
        !
    END IF
    !
    IF(PRESENT(UP1) .OR. PRESENT(UP2)) THEN
       IF(JUNCT > Z) THEN
           IF(V2 == ADJ%V1_BACK_PATH) THEN
               P1 =     BACKPATH(JUNCT)
               P2 = ADJ%BACKPATH(JUNCT)
           ELSE
               P1 = ADJ%BACKPATH(JUNCT)
               P2 =     BACKPATH(JUNCT)
           END IF
       ELSE
           P1 = Z
           P2 = Z
       END IF
       !
       IF(PRESENT(UP1)) UP1 = P1
       IF(PRESENT(UP2)) UP2 = P2
       !
    END IF
    !
  END SUBROUTINE
  !
  !V2 cannot be upstream of V1, BACKPATH is for V1
  !Assumes that V1 /= V2 and ADJ%N > 1
  !This will return V2 if it is the closest path to V1
  PURE SUBROUTINE CLOSEST_COMMON_IMAP_ADJ_LST(ADJ, V1, V2, ID, IMAP, BACKPATH, SPT, JUNCT) 
    CLASS(ADJ_LST),            INTENT(INOUT):: ADJ
    INTEGER,                   INTENT(IN   ):: V1, V2, ID
    INTEGER, DIMENSION(ADJ%N), INTENT(IN   ):: BACKPATH, IMAP
    LOGICAL, DIMENSION(ADJ%N), INTENT(INOUT):: SPT
    INTEGER,                   INTENT(INOUT):: JUNCT
    INTEGER:: I,U,V,P
    DOUBLE PRECISION:: COM_DIST
    DOUBLE PRECISION:: DIST_U, DIST_V, DIST 
    !
    JUNCT = Z
    COM_DIST = inf
    !
    SPT = TRUE
    !
    CALL ADJ%HEAP%DIJKSTRA_INIT(V2, ADJ%N)  !Set up HEAP 
    !
    DO WHILE (ADJ%HEAP%SIZE() > Z)
        !
        CALL ADJ%HEAP%POP(U, DIST_U)  !GET MIN FROM HEAP  (FIRST POP IS V2)
        !
        IF(U > Z .AND. DIST_U < near_inf) THEN !dist is < inf
            !
            IF(BACKPATH(U) > Z .AND. DIST_U < COM_DIST) THEN  !TRUE if direct path to V1
                  !
                  IF(ID == IMAP(U)) THEN
                                    DIST = DIST_U + ADJ%SUM_WT(U, BACKPATH)
                                    !
                                    IF(DIST < COM_DIST) THEN
                                        !
                                        COM_DIST = DIST
                                        JUNCT      = U
                                    END IF
                  END IF
            END IF
            !
            SPT(U) = FALSE  !Point is being explored so add it to the shortest path
            !
            DO I=ONE, ADJ%VER(U)%N
                !
                V = ADJ%VER(U)%CON(I)
                !
                IF(V > Z) THEN
                   !
                   IF(SPT(V)) THEN  !Have not visited point before
                        !
                        DIST = DIST_U + ADJ%VER(U)%WT(I)
                        !
                        CALL ADJ%HEAP%GET(V,DIST_V,P)
                        !
                        IF( DIST < DIST_V)  CALL ADJ%HEAP%SET_X(P,DIST)      !Update with new lower distance
                   END IF
                END IF
            END DO
        ELSE
            EXIT
        END IF
        !
    END DO
    !
  END SUBROUTINE
  !
  !!!PURE SUBROUTINE CLOSEST_COMMON_ADJ_LST(ADJ,V1,V2,JUNCT) !V1 must be upstream of V2
  !!!  CLASS(ADJ_LST),         INTENT(INOUT):: ADJ
  !!!  INTEGER,                INTENT(IN   ):: V1, V2
  !!!  INTEGER,                INTENT(INOUT):: JUNCT
  !!!  INTEGER:: I,U,V, P1, P2, P
  !!!  DOUBLE PRECISION:: COM_DIST
  !!!  DOUBLE PRECISION:: DIST_U, DIST_V, DIST 
  !!!  !
  !!!  IF    (V1 == V2) THEN
  !!!                           JUNCT = V1
  !!!  ELSEIF(ADJ%N <  TWO) THEN
  !!!                           JUNCT = Z
  !!!  ELSE
  !!!      JUNCT = Z
  !!!      COM_DIST = inf
  !!!      !
  !!!      IF(V2 == ADJ%V1_BACK_PATH) THEN
  !!!          P1 = V2
  !!!          P2 = V1
  !!!      ELSE
  !!!          P1 = V1
  !!!          P2 = V2
  !!!      END IF
  !!!      !
  !!!      CALL DIJKSTRA_SHORTEST_PATH_VERTEX(ADJ, P1)
  !!!      !
  !!!      CALL ADJ%HEAP%DIJKSTRA_INIT(P2, ADJ%N)  !Set up HEAP 
  !!!      !
  !!!      ADJ%SPT = TRUE !SET TO FALSE IF VISITED
  !!!      !
  !!!      DO WHILE (ADJ%HEAP%SIZE() > Z)
  !!!          !
  !!!          CALL ADJ%HEAP%POP(U, DIST_U)  !GET MIN FROM HEAP  (FIRST POP IS P2)
  !!!          !
  !!!          IF(U > Z .AND. DIST_U < D200) THEN !dist is < inf
  !!!              !
  !!!              IF(ADJ%BACKPATH(U) > Z .AND. DIST_U < COM_DIST) THEN  !TRUE if direct path to P1
  !!!                    COM_DIST = DIST_U
  !!!                    JUNCT      = U
  !!!              END IF
  !!!              !
  !!!              ADJ%SPT(U) = FALSE  !Point is being explored so add it to the shortest path
  !!!              !
  !!!              DO I=ONE, ADJ%VER(U)%N
  !!!                  !
  !!!                  V = ADJ%VER(U)%CON(I)
  !!!                  !
  !!!                  IF(V > Z) THEN
  !!!                     !
  !!!                     IF(ADJ%SPT(V)) THEN  !Have not visited point before
  !!!                          !
  !!!                          DIST = DIST_U + ADJ%VER(U)%WT(I)
  !!!                          !
  !!!                          CALL ADJ%HEAP%GET(V,DIST_V,P)
  !!!                          !
  !!!                          IF( DIST < DIST_V)  CALL ADJ%HEAP%SET_X(P,DIST)      !Update with new lower distance
  !!!                     END IF
  !!!                  END IF
  !!!              END DO
  !!!          ELSE
  !!!              EXIT
  !!!          END IF
  !!!          !
  !!!      END DO
  !!!  END IF
  !!!  !
  !!!END SUBROUTINE
  !
  ! ----------------------------------------------------------------------------------
  !
  PURE SUBROUTINE BUILD_DIJKSTRA_PATH_VERTEX_VERTEX_PATH(VTX, VPATH)
    TYPE(VERTEX_BACKPATH),               INTENT(IN   ):: VTX
    TYPE(VERTEX_VERTEX_PATH),            INTENT(INOUT):: VPATH
    !
    IF(VPATH%V1 == VTX%V1) THEN
                            CALL BUILD_DIJKSTRA_PATH_BACKPATH(VPATH%V2, VTX%BACKPATH, VPATH%PATH, VPATH%N)
    ELSE
                            VPATH%N = Z
    END IF
    !
  END SUBROUTINE
  !
  PURE SUBROUTINE BUILD_DIJKSTRA_PATH_V2_VERTEX_VERTEX_PATH(V2, VTX, VPATH)
    INTEGER,                             INTENT(IN   ):: V2
    TYPE(VERTEX_BACKPATH),               INTENT(IN   ):: VTX
    TYPE(VERTEX_VERTEX_PATH),            INTENT(INOUT):: VPATH
    INTEGER:: DIM
    !
    VPATH%V1 = VTX%V1
    VPATH%V2 = V2
    CALL BUILD_DIJKSTRA_PATH_BACKPATH(V2, VTX%BACKPATH, VPATH%PATH, DIM)
    VPATH%N = DIM
    !
  END SUBROUTINE
  !
  PURE SUBROUTINE BUILD_DIJKSTRA_PATH_BACKPATH(V2, BACKPATH, PATH, DIM)
    INTEGER,                             INTENT(IN   ):: V2
    INTEGER, DIMENSION(:), ALLOCATABLE,  INTENT(IN   ):: BACKPATH
    INTEGER,                             INTENT(  OUT):: DIM
    INTEGER, DIMENSION(:), ALLOCATABLE,  INTENT(INOUT):: PATH
    INTEGER:: I,J
    !
    DIM = ONE
    !
    IF(.NOT. ALLOCATED(BACKPATH)) THEN; DIM = Z
    ELSEIF(BACKPATH(V2) == Z    ) THEN; DIM = Z
    END IF
    !
    IF(DIM == ONE) THEN
                      I   = V2
                      DO WHILE ( BACKPATH(I) > Z )
                              DIM = DIM + ONE
                              !
                              I = BACKPATH(I)
                      END DO
                      !
                      CALL ALLOC(PATH,DIM)
                      !
                      I       = V2
                      J       = DIM
                      DO J=DIM, ONE, NEG
                              !
                              PATH(J) = I
                              !
                              I = BACKPATH(I)
                      END DO
    END IF
    !
  END SUBROUTINE
  !
  !!!PURE SUBROUTINE DIJKSTRA_SHORTEST_PATH(ADJ,V1)
  !!!  CLASS(ADJ_LST),                      INTENT(INOUT):: ADJ
  !!!  INTEGER,                             INTENT(IN   ):: V1
  !!!  INTEGER:: I,U,V,P
  !!!  DOUBLE PRECISION:: DIST_U, DIST_V, DIST 
  !!!  !
  !!!  IF(.NOT. ALLOCATED(ADJ%SPT      )) ALLOCATE(ADJ%SPT(ADJ%N))
  !!!  IF(.NOT. ALLOCATED(ADJ%BACKPATH)) ALLOCATE(ADJ%BACKPATH(ADJ%N))
  !!!  !
  !!!  ADJ%V1_BACK_PATH = V1
  !!!  !
  !!!  ADJ%SPT       = TRUE  !If FALSE then point is in the tree
  !!!  ADJ%BACKPATH = Z     !Non-Zero when point exists
  !!!  !
  !!!  !!!CALL ADJ%HEAP%INIT(ADJ%N)
  !!!  !
  !!!  CALL ADJ%HEAP%DIJKSTRA_INIT(V1, ADJ%N)  !Set up HEAP For DIJKSTRA Algorithm 
  !!!  !
  !!!  DO WHILE (ADJ%HEAP%SIZE() > Z)
  !!!      !
  !!!      CALL ADJ%HEAP%POP(U, DIST_U)  !GET MIN FROM HEAP
  !!!      !
  !!!      IF(U > Z .AND. DIST_U < D200) THEN !dist is < inf
  !!!          !
  !!!          ADJ%SPT(U) = FALSE  !Point is being explored so add it to the shortest path
  !!!          !
  !!!          DO I=ONE, ADJ%VER(U)%N
  !!!              !
  !!!              V = ADJ%VER(U)%CON(I)
  !!!              !
  !!!              IF(V > Z) THEN
  !!!                 !
  !!!                 IF(ADJ%SPT(V)) THEN  !Have not visited point before
  !!!                      !
  !!!                      DIST = DIST_U + ADJ%VER(U)%WT(I)
  !!!                      !
  !!!                      CALL ADJ%HEAP%GET(V,DIST_V,P)
  !!!                      !
  !!!                      IF( DIST < DIST_V) THEN
  !!!                          ADJ%BACKPATH(V) = U
  !!!                          !ADJ%HEAP%CHILD(P)%PNT%X = DIST  !Update with new lower distance
  !!!                          CALL ADJ%HEAP%SET_X(P,DIST)      !Update with new lower distance
  !!!                      END IF
  !!!                 END IF
  !!!              END IF
  !!!          END DO
  !!!      ELSE
  !!!          EXIT
  !!!      END IF
  !!!      !
  !!!  END DO
  !!!END SUBROUTINE
  !!!!
  !!!PURE SUBROUTINE GET_PATH_DIJKSTRA_SHORTEST_PATH(ADJ,V1,V2,PATH,DIM)
  !!!  CLASS(ADJ_LST),                      INTENT(INOUT):: ADJ
  !!!  INTEGER,                             INTENT(IN   ):: V1,V2
  !!!  INTEGER,                             INTENT(  OUT):: DIM
  !!!  INTEGER, DIMENSION(:), ALLOCATABLE,  INTENT(INOUT):: PATH
  !!!  INTEGER:: I,J
  !!!  !
  !!!  IF(ADJ%V1_BACK_PATH .NE. V1) CALL DIJKSTRA_SHORTEST_PATH(ADJ,V1)
  !!!  !
  !!!  IF(ADJ%BACKPATH(V2) == Z) THEN
  !!!      DIM = Z
  !!!      RETURN
  !!!  END IF
  !!!  !
  !!!  DIM = ONE
  !!!  I   = V2
  !!!  DO WHILE ( ADJ%BACKPATH(I) > Z )
  !!!          DIM = DIM + ONE
  !!!          !
  !!!          I = ADJ%BACKPATH(I)
  !!!  END DO
  !!!  !
  !!!  CALL ALLOC(PATH,DIM)
  !!!  !
  !!!  I       = V2
  !!!  J       = DIM
  !!!  DO J=DIM, ONE, NEG
  !!!          !
  !!!          PATH(J) = I
  !!!          !
  !!!          I = ADJ%BACKPATH(I)
  !!!  END DO
  !!!  !
  !!!END SUBROUTINE
  !
  PURE ELEMENTAL FUNCTION GET_VER_DIM_ADJ_LST(ADJ) RESULT(DIM)
     CLASS(ADJ_LST), INTENT(IN):: ADJ
     INTEGER:: DIM
     !
     DIM = ADJ%N
     !
  END FUNCTION
  !
  PURE ELEMENTAL SUBROUTINE ALLOCATE_ADJ_LST(ADJ,DIM)
     CLASS(ADJ_LST),INTENT(INOUT):: ADJ
     INTEGER,       INTENT(IN   ):: DIM
     !
     IF(DIM < ONE) THEN
         IF(ALLOCATED(ADJ%VER)) DEALLOCATE(ADJ%VER)
         ADJ%N=Z
     ELSEIF(ADJ%N .NE. DIM) THEN
         IF(ALLOCATED(ADJ%VER)) DEALLOCATE(ADJ%VER)
         ALLOCATE(ADJ%VER(DIM))
         ADJ%N=DIM
     ELSE
         CALL RESET_CONNECTIONS_ADJ_LST(ADJ)
     END IF
     !
  END SUBROUTINE
  !
  PURE ELEMENTAL SUBROUTINE RESET_CONNECTIONS_ADJ_LST(ADJ)
     CLASS(ADJ_LST),INTENT(INOUT):: ADJ
     INTEGER:: I
     !
    DO CONCURRENT (I=ONE:ADJ%N, ADJ%VER(I)%N > Z)
                                ADJ%VER(I)%N = Z
                                DEALLOCATE(ADJ%VER(I)%CON)
                                DEALLOCATE(ADJ%VER(I)%WT)
    END DO
    !
  END SUBROUTINE
  !
  PURE SUBROUTINE ADD_CONNECTION_ADJ_LST(ADJ,V1,V2,WT,ERR)
    CLASS(ADJ_LST),                      INTENT(INOUT):: ADJ
    INTEGER,                             INTENT(IN   ):: V1,V2
    DOUBLE PRECISION,          OPTIONAL, INTENT(IN   ):: WT
    CHARACTER(:), ALLOCATABLE, OPTIONAL, INTENT(INOUT):: ERR
    INTEGER:: DIM
    !
    IF(V1 < ONE .OR. V2 < Z) THEN
                       CALL SET_ERR_MSG('ADJACENCY_LIST_INSTRUCTION RECIEVED VERTEX-CONNECTION PAIR THAT IS INVALID. MAX SIZE OF VERTEX IS '//NUM2STR(ADJ%N)//' BUT THE VERTEX AND CONNECTION RECIEVED WERE: '//NUM2STR(V1)//' AND '//NUM2STR(V2),ERR)
    ELSE
        IF(V1>V2) THEN; DIM = V1
        ELSE;           DIM = V2
        END IF
        !
        IF(DIM > ADJ%N) CALL REALLOCATE_ADJ_LST(ADJ, DIM)
        !
        CALL SET_CONNECTION_ADJ_LST(ADJ,V1,V2,WT,ERR)
    END IF
    !
  END SUBROUTINE
  !
  PURE SUBROUTINE REALLOCATE_ADJ_LST(ADJ, DIM)
    CLASS(ADJ_LST), INTENT(INOUT):: ADJ
    INTEGER,        INTENT(IN   ):: DIM
    TYPE(ADJ_LST):: TMP
    INTEGER:: I
    !
    IF(ADJ%N < ONE) THEN
                       CALL ALLOCATE_ADJ_LST( ADJ, DIM )
    ELSE
        CALL ALLOCATE_ADJ_LST( TMP, DIM )
        !
        DO CONCURRENT (I=ONE:ADJ%N, ADJ%VER(I)%N > Z)
                                                 TMP%VER(I)%N = ADJ%VER(I)%N
                                                 !
                                                 CALL MOVE_ALLOC(ADJ%VER(I)%CON, TMP%VER(I)%CON)
                                                 CALL MOVE_ALLOC(ADJ%VER(I)%WT , TMP%VER(I)%WT )
        END DO
        !
        DEALLOCATE(ADJ%VER);  ALLOCATE(ADJ%VER(DIM))
        !
        DO CONCURRENT (I=ONE:ADJ%N, TMP%VER(I)%N > Z)
                                                 ADJ%VER(I)%N = TMP%VER(I)%N
                                                 !
                                                 CALL MOVE_ALLOC(TMP%VER(I)%CON, ADJ%VER(I)%CON)
                                                 CALL MOVE_ALLOC(TMP%VER(I)%WT , ADJ%VER(I)%WT )
        END DO
        ADJ%N=DIM
        ADJ%V1_BACK_PATH=Z
    END IF
    !
  END SUBROUTINE
  !
  PURE SUBROUTINE SET_CONNECTION_ADJ_LST(ADJ,V1,V2,WT,ERR)
    CLASS(ADJ_LST),                      INTENT(INOUT):: ADJ
    INTEGER,                             INTENT(IN   ):: V1,V2
    DOUBLE PRECISION,          OPTIONAL, INTENT(IN   ):: WT
    CHARACTER(:), ALLOCATABLE, OPTIONAL, INTENT(INOUT):: ERR
    INTEGER,          DIMENSION(:),ALLOCATABLE:: ITMP
    DOUBLE PRECISION, DIMENSION(:),ALLOCATABLE:: DTMP
    INTEGER:: I
    DOUBLE PRECISION:: WEIGHT
    !
    IF(PRESENT(WT)) THEN;  WEIGHT = WT
    ELSE;                  WEIGHT = DZ
    END IF
    !
    IF(ADJ%N < ONE) THEN
                       CALL SET_ERR_MSG('ADJACENCY_LIST_INSTRUCTION RECIEVED VERTEX-CONNECTION PAIR WHEN IT WAS NOT ALLOCATED',ERR)
                       !
    ELSEIF(V1 < ONE .OR. V2 < Z .OR. V1 > ADJ%N .OR. V2 > ADJ%N) THEN
                       CALL SET_ERR_MSG('ADJACENCY_LIST_INSTRUCTION RECIEVED VERTEX-CONNECTION PAIR THAT IS INVALID. MAX SIZE OF VERTEX IS '//NUM2STR(ADJ%N)//' BUT THE VERTEX AND CONNECTION RECIEVED WERE: '//NUM2STR(V1)//' AND '//NUM2STR(V2),ERR)
    ELSE
        ASSOCIATE (DIM => ADJ%VER(V1)%N, VER => ADJ%VER(V1))
           !
           IF(DIM == Z) THEN
               ALLOCATE(VER%CON(ONE), SOURCE=V2)
               !
               ALLOCATE(VER%WT(ONE), SOURCE=WEIGHT)
               !
               DIM = ONE
           ELSE
               IF(ALL(VER%CON .NE. V2)) THEN
                   !
                   ALLOCATE(ITMP(DIM+ONE))
                   !
                   ITMP(ONE:DIM) = VER%CON
                   ITMP(DIM+ONE) = V2
                   CALL MOVE_ALLOC(ITMP,VER%CON)
                   !
                   ALLOCATE(DTMP(DIM+ONE))
                   !
                   DTMP(ONE:DIM) = VER%WT
                   !
                   DTMP(DIM+ONE) = WEIGHT
                   !
                   CALL MOVE_ALLOC(DTMP, VER%WT)
                   !
                   DIM = DIM + ONE
                   !
               ELSEIF(PRESENT(WT)) THEN  !Update WT if passed in
                   !
                   DO I=ONE, DIM
                             IF(VER%CON(I)==V2) THEN
                                                VER%WT(I) = WT
                                                EXIT
                             END IF
                   END DO
               END IF
           END IF
        END ASSOCIATE
    END IF
    !
  END SUBROUTINE
  !
  PURE SUBROUTINE SET_WT_ADJ_LST(ADJ,V1,V2,WT,ERR)
    CLASS(ADJ_LST),                      INTENT(INOUT):: ADJ
    INTEGER,                             INTENT(IN   ):: V1,V2
    DOUBLE PRECISION,                    INTENT(IN   ):: WT
    CHARACTER(:), ALLOCATABLE, OPTIONAL, INTENT(INOUT):: ERR
    INTEGER:: I
    !
    IF(ADJ%N < ONE) THEN
                       CALL SET_ERR_MSG('ADJACENCY_LIST_INSTRUCTION RECIEVED VERTEX-CONNECTION PAIR WHEN IT WAS NOT ALLOCATED',ERR)
                       !
    ELSEIF(V1 < ONE .OR. V2 < Z .OR. V1 > ADJ%N .OR. V2 > ADJ%N) THEN
                       CALL SET_ERR_MSG('ADJACENCY_LIST_INSTRUCTION RECIEVED VERTEX-CONNECTION PAIR THAT IS INVALID. MAX SIZE OF VERTEX IS '//NUM2STR(ADJ%N)//' BUT THE VERTEX AND CONNECTION RECIEVED WERE: '//NUM2STR(V1)//' AND '//NUM2STR(V2),ERR)
    ELSE
        ASSOCIATE (DIM => ADJ%VER(V1)%N, VER => ADJ%VER(V1))
           !
           IF(DIM == Z) THEN
               CALL SET_ERR_MSG('ADJACENCY_LIST_INSTRUCTION RECIEVED VERTEX-CONNECTION-WEIGHT, BUT THE CONNECTION DOES NOT EXIST. THE VERTEX AND CONNECTION RECIEVED WERE: '//NUM2STR(V1)//' AND '//NUM2STR(V2),ERR)
           ELSE
               DO I=ONE, DIM
                   IF(VER%CON(I)==V2) THEN
                      VER%WT(I) = WT
                      EXIT
                   ELSEIF(I==DIM) THEN
                          CALL SET_ERR_MSG('ADJACENCY_LIST_INSTRUCTION RECIEVED VERTEX-CONNECTION-WEIGHT, BUT THE CONNECTION DOES NOT EXIST. THE VERTEX AND CONNECTION RECIEVED WERE: '//NUM2STR(V1)//' AND '//NUM2STR(V2),ERR)
                   END IF
               END DO
             END IF
        END ASSOCIATE
    END IF
    !
  END SUBROUTINE
  !
  PURE SUBROUTINE SCALE_WT_ADJ_LST(ADJ,V1,V2,SCALE,ERR)
    CLASS(ADJ_LST),                      INTENT(INOUT):: ADJ
    INTEGER,                             INTENT(IN   ):: V1,V2
    DOUBLE PRECISION,                    INTENT(IN   ):: SCALE
    CHARACTER(:), ALLOCATABLE, OPTIONAL, INTENT(INOUT):: ERR
    INTEGER:: I
    !
    IF(ADJ%N < ONE) THEN
                       CALL SET_ERR_MSG('ADJACENCY_LIST_INSTRUCTION RECIEVED VERTEX-CONNECTION PAIR WHEN IT WAS NOT ALLOCATED',ERR)
                       !
    ELSEIF(V1 < ONE .OR. V2 < Z .OR. V1 > ADJ%N .OR. V2 > ADJ%N) THEN
                       CALL SET_ERR_MSG('ADJACENCY_LIST_INSTRUCTION RECIEVED VERTEX-CONNECTION PAIR THAT IS INVALID. MAX SIZE OF VERTEX IS '//NUM2STR(ADJ%N)//' BUT THE VERTEX AND CONNECTION RECIEVED WERE: '//NUM2STR(V1)//' AND '//NUM2STR(V2),ERR)
    ELSE
        ASSOCIATE (DIM => ADJ%VER(V1)%N, VER => ADJ%VER(V1))
           !
           IF(DIM == Z) THEN
               CALL SET_ERR_MSG('ADJACENCY_LIST_INSTRUCTION RECIEVED VERTEX-CONNECTION-WEIGHT, BUT THE CONNECTION DOES NOT EXIST. THE VERTEX AND CONNECTION RECIEVED WERE: '//NUM2STR(V1)//' AND '//NUM2STR(V2),ERR)
           ELSE
               DO I=ONE, DIM
                   IF(VER%CON(I)==V2) THEN
                      !
                      IF(VER%WT(I) < D100) VER%WT(I) = VER%WT(I) * SCALE 
                      EXIT
                   ELSEIF(I==DIM) THEN
                          CALL SET_ERR_MSG('ADJACENCY_LIST_INSTRUCTION RECIEVED VERTEX-CONNECTION-WEIGHT, BUT THE CONNECTION DOES NOT EXIST. THE VERTEX AND CONNECTION RECIEVED WERE: '//NUM2STR(V1)//' AND '//NUM2STR(V2),ERR)
                   END IF
               END DO
             END IF
        END ASSOCIATE
    END IF
    !
  END SUBROUTINE
  !
  PURE SUBROUTINE SORT_CONNECTIONS_ADJ_LST(ADJ)
    CLASS(ADJ_LST),                      INTENT(INOUT):: ADJ
    INTEGER:: I
    !
    DO CONCURRENT (I=ONE:ADJ%N, ADJ%VER(I)%N > ONE)
          !
          CALL SORT(ADJ%VER(I)%CON, ADJ%VER(I)%WT)
    END DO
    !
  END SUBROUTINE
  !
  PURE SUBROUTINE SET_ERR_MSG(MSG,ERR)
    CHARACTER(*),                        INTENT(IN   ):: MSG
    CHARACTER(:), ALLOCATABLE, OPTIONAL, INTENT(INOUT):: ERR
    !
    IF(PRESENT(ERR)) THEN
         IF(ALLOCATED(ERR)) THEN
             ERR = ERR//MSG//NL
         ELSE
             ERR = MSG//NL
         END IF
    END IF
    !
  END SUBROUTINE
END MODULE
!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
SUBMODULE (ADJACENCY_LIST_INSTRUCTION) VERTEX_VERTEX_PATH_SUBS
  !
  IMPLICIT NONE(TYPE, EXTERNAL)
  !
  CONTAINS  ! ------------------------------------------------------------------------------------------------------------------------------------------------------
  !
  MODULE PURE SUBROUTINE MAKE_PATH_VERTEX_VERTEX_PATH(VTX, BACKPATH, REBUILD)
    CLASS(VERTEX_VERTEX_PATH), INTENT(INOUT):: VTX
    TYPE(VERTEX_BACKPATH),     INTENT(IN   ):: BACKPATH
    LOGICAL,         OPTIONAL, INTENT(IN   ):: REBUILD
    LOGICAL:: FORCE_REBUILD
    !
    IF(PRESENT(REBUILD)) THEN
                        FORCE_REBUILD = REBUILD
    ELSE
                        FORCE_REBUILD = FALSE
    END IF
    !
    IF(VTX%V1 == BACKPATH%V1 .AND. BACKPATH%N > Z) THEN
        !
        IF(VTX%N == Z .OR. FORCE_REBUILD) CALL BUILD_DIJKSTRA_PATH_VERTEX_VERTEX_PATH( BACKPATH, VTX)
        !
    END IF
    !
  END SUBROUTINE
  !
  MODULE PURE SUBROUTINE EXTEND_VERTEX_VERTEX_PATH(VTX, V1, V2)
    TYPE(VERTEX_VERTEX_PATH), DIMENSION(:), ALLOCATABLE, INTENT(INOUT):: VTX
    INTEGER,                                             INTENT(IN   ):: V1, V2
    TYPE(VERTEX_VERTEX_PATH), DIMENSION(:), ALLOCATABLE:: TMP
    INTEGER:: DIM
    !
    IF(ALLOCATED(VTX)) THEN
        DIM = SIZE(VTX)
        ALLOCATE(TMP(DIM+ONE))
        !
        CALL MOVE_VERTEX_VERTEX_PATH(VTX, TMP(:DIM))
        !
        DEALLOCATE(VTX)
        CALL MOVE_ALLOC(TMP, VTX)
    ELSE
        ALLOCATE(VTX(ONE))
        CALL SET_V_VERTEX_VERTEX_PATH(VTX(ONE), V1, V2)
        VTX%N = Z
    END IF
    !
  END SUBROUTINE
  !
  MODULE ELEMENTAL PURE SUBROUTINE COPY_VERTEX_VERTEX_PATH(VTX_OUT,VTX_IN)
    CLASS(VERTEX_VERTEX_PATH), INTENT(IN   ):: VTX_IN
    CLASS(VERTEX_VERTEX_PATH), INTENT(INOUT):: VTX_OUT
    !
    VTX_OUT%V1  = VTX_IN%V1
    VTX_OUT%V2  = VTX_IN%V2
    VTX_OUT%N   = VTX_IN%N
    CALL ALLOC(VTX_OUT%PATH, VTX_OUT%N, SRC=VTX_IN%PATH)
    !
  END SUBROUTINE
  !
  MODULE ELEMENTAL PURE SUBROUTINE MOVE_VERTEX_VERTEX_PATH(VTX_IN, VTX_OUT)
    CLASS(VERTEX_VERTEX_PATH), INTENT(INOUT):: VTX_IN
    CLASS(VERTEX_VERTEX_PATH), INTENT(INOUT):: VTX_OUT
    !
    VTX_OUT%V1  = VTX_IN%V1
    VTX_OUT%V2  = VTX_IN%V2
    VTX_OUT%N   = VTX_IN%N
    !
    IF(VTX_IN%N > Z) CALL MOVE_ALLOC(VTX_IN%PATH, VTX_OUT%PATH)
    !
    VTX_IN%V1 = Z
    VTX_IN%V2 = Z
    VTX_IN%N  = Z
    !
  END SUBROUTINE
  !
  MODULE PURE SUBROUTINE ALLOC_VERTEX_VERTEX_PATH(VTX, N)
    CLASS(VERTEX_VERTEX_PATH), INTENT(INOUT):: VTX
    INTEGER,                   INTENT(IN   ):: N
    !
    IF( N > Z) THEN
        VTX%N = N
    ELSE
        VTX%N = Z
    END IF
    !
    CALL ALLOC(VTX%PATH, N)
    !
  END SUBROUTINE
  !
  MODULE ELEMENTAL PURE SUBROUTINE SET_V_VERTEX_VERTEX_PATH(VTX, V1, V2, NEW_ALLOC)
    CLASS(VERTEX_VERTEX_PATH),                     INTENT(INOUT):: VTX
    INTEGER,                                       INTENT(IN   ):: V1, V2
    LOGICAL,                             OPTIONAL, INTENT(  OUT):: NEW_ALLOC
    !
    IF(PRESENT(NEW_ALLOC)) NEW_ALLOC=FALSE
    !
    IF(VTX%V1.NE.V1 .OR. VTX%V2.NE.V2 .OR. V1 < ONE .OR. V2 < ONE) THEN
                                                                   VTX%N = Z
                                                                   CALL ALLOC(VTX%PATH, Z, NEW_ALLOC=NEW_ALLOC)
    END IF
    !
    IF(V1 > Z) THEN
               VTX%V1 = V1
    ELSE
               VTX%V1 = Z
    END IF
    !
    IF(V2 > Z) THEN
               VTX%V2 = V2
    ELSE
               VTX%V2 = Z
    END IF
    !
  END SUBROUTINE
  !
  MODULE PURE SUBROUTINE SET_V1_VERTEX_VERTEX_PATH(VTX, V1, NEW_ALLOC)
    CLASS(VERTEX_VERTEX_PATH),                     INTENT(INOUT):: VTX
    INTEGER,                                       INTENT(IN   ):: V1
    LOGICAL,                             OPTIONAL, INTENT(  OUT):: NEW_ALLOC
    !
    IF(PRESENT(NEW_ALLOC)) NEW_ALLOC=FALSE
    !
    IF(VTX%V1.NE.V1 .OR. V1 < ONE) THEN
                                   VTX%N = Z
                                   CALL ALLOC(VTX%PATH, Z, NEW_ALLOC=NEW_ALLOC)
    END IF
    !
    IF(V1 > Z) THEN
               VTX%V1 = V1
    ELSE
               VTX%V1 = Z
    END IF
    !
  END SUBROUTINE
  !
  MODULE PURE SUBROUTINE SET_V2_VERTEX_VERTEX_PATH(VTX, V2, NEW_ALLOC)
    CLASS(VERTEX_VERTEX_PATH),                     INTENT(INOUT):: VTX
    INTEGER,                                       INTENT(IN   ):: V2
    LOGICAL,                             OPTIONAL, INTENT(  OUT):: NEW_ALLOC
    !
    IF(PRESENT(NEW_ALLOC)) NEW_ALLOC=FALSE
    !
    IF(VTX%V2.NE.V2 .OR. V2 < ONE) THEN
                                   VTX%N = Z
                                   CALL ALLOC(VTX%PATH, Z, NEW_ALLOC=NEW_ALLOC)
    END IF
    !
    IF(V2 > Z) THEN
               VTX%V2 = V2
    ELSE
               VTX%V2 = Z
    END IF
    !
  END SUBROUTINE
  !
END SUBMODULE 
!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
SUBMODULE (ADJACENCY_LIST_INSTRUCTION) VERTEX_BACKPATH_SUBS
  !
  IMPLICIT NONE(TYPE, EXTERNAL)
  !
  CONTAINS  ! ------------------------------------------------------------------------------------------------------------------------------------------------------
  !
  MODULE PURE FUNCTION IS_VERTEX_MATCH(VTX, V1) RESULT(IS_MATCH)
    CLASS(VERTEX_BACKPATH), INTENT(IN):: VTX
    INTEGER,                INTENT(IN):: V1
    LOGICAL:: IS_MATCH
    !
    IS_MATCH = VTX%V1 == V1
    !
  END FUNCTION
  !
  MODULE PURE FUNCTION IS_VERTEX_AND_DIM_MATCH(VTX, V1, DIM) RESULT(IS_MATCH)
    CLASS(VERTEX_BACKPATH), INTENT(IN):: VTX
    INTEGER,                INTENT(IN):: V1, DIM
    LOGICAL:: IS_MATCH
    !
    IS_MATCH = VTX%V1 == V1 .AND. VTX%N == DIM
    !
  END FUNCTION
  !
  MODULE PURE SUBROUTINE ALLOCATE_VERTEX_BACKPATH(VTX, DIM, V1)
    CLASS(VERTEX_BACKPATH), INTENT(INOUT):: VTX
    INTEGER,                INTENT(IN   ):: DIM
    INTEGER,  OPTIONAL,     INTENT(IN   ):: V1
    !
    IF(PRESENT(V1)) THEN
        VTX%V1 = V1
    ELSE
        VTX%V1 = Z
    END IF
    !
    VTX%GOT_BACK = FALSE
    VTX%N  = DIM
    CALL ALLOC(VTX%BACKPATH, DIM)
    !
  END SUBROUTINE
  !
  MODULE PURE SUBROUTINE SET_V1_VERTEX_BACKPATH(VTX, V1)
    CLASS(VERTEX_BACKPATH), INTENT(INOUT):: VTX
    INTEGER,                INTENT(IN   ):: V1
    !
    IF(V1 .NE. VTX%V1) THEN
               VTX%V1 = V1
               VTX%GOT_BACK = FALSE
    END IF
    !
  END SUBROUTINE
  !
  MODULE PURE SUBROUTINE CLOSEST_COMMON_VERTEX_BACKPATH(VTX, V2, V3, JUNCT, DN2, DN3)
    CLASS(VERTEX_BACKPATH), INTENT(IN   ):: VTX
    INTEGER,                INTENT(IN   ):: V2, V3
    INTEGER,     OPTIONAL,  INTENT(INOUT):: JUNCT, DN2, DN3
    INTEGER:: CON
    INTEGER:: I, J, D2, D3
    !
    D2 = V2
    D3 = V3
    CON = Z
    IF(V2 == V3) THEN
       CON = V3
    ELSEIF(V2 <= VTX%N .AND. V3 <= VTX%N) THEN
          !
          I = V2
          J = V3
V2_LOOP:  DO WHILE ( VTX%BACKPATH(I) > Z )
                !
                D2 = I
                I  = VTX%BACKPATH(I)
                !
                J = V3
                !
                DO WHILE ( VTX%BACKPATH(J) > Z )
                      !
                      D3 = J
                      J  = VTX%BACKPATH(J)
                      !
                      IF(I == J) THEN
                                 CON = I
                                 EXIT V2_LOOP
                      END IF
                END DO
          END DO V2_LOOP
    END IF
    !
    IF(PRESENT(DN2)) DN2 = D2 
    IF(PRESENT(DN3)) DN3 = D3
    IF(PRESENT(JUNCT)) JUNCT = CON 
    !
  END SUBROUTINE
  !
!!!  MODULE PURE SUBROUTINE CLOSEST_COMMON_VERTEX_BACKPATH_VERTEX_BACKPATH(VTX, VTY, JUNCT, UP1, UP2)
!!!    CLASS(VERTEX_BACKPATH), INTENT(IN   ):: VTX
!!!    CLASS(VERTEX_BACKPATH), INTENT(IN   ):: VTY
!!!    INTEGER,                INTENT(INOUT):: JUNCT
!!!    INTEGER,     OPTIONAL,  INTENT(INOUT):: UP1, UP2
!!!    INTEGER:: I, BS
!!!    LOGICAL, DIMENSION(:), ALLOCATABLE:: CHK1, CHK2
!!!    !
!!!    ASSOCIATE(V1 => VTX%V1, DIM1 => VTX%N, B1 => VTX%BACKPATH &
!!!              V2 => VTY%V1, DIM2 => VTY%N, B2 => VTY%BACKPATH  )
!!!    !
!!!    IF(V1 < ONE .OR. V2 < ONE .OR. V1 > DIM1 .OR. V1 > DIM2 .OR. V2 > DIM1 .OR. V2 > DIM2 .OR. DIM1 < ONE .OR. DIM2 < Z) THEN
!!!           JUNCT = Z
!!!    ELSEIF(V1 == V2) THEN
!!!           JUNCT = V1
!!!    ELSE
!!!          ALLOCATE(CHK1(DIM1), SOURCE=TRUE)
!!!          ALLOCATE(CHK2(DIM2), SOURCE=TRUE)
!!!          !
!!!LOOP:     DO I = ONE, DIM1
!!!                V = B1
!!!                !
!!!                D2 = I
!!!                I  = VTX%BACKPATH(I)
!!!                !
!!!                J = V3
!!!                !
!!!                DO WHILE ( VTX%BACKPATH(J) > Z )
!!!                      !
!!!                      D3 = J
!!!                      J  = VTX%BACKPATH(J)
!!!                      !
!!!                      IF(I == J) THEN
!!!                                 CON = I
!!!                                 EXIT V2_LOOP
!!!                      END IF
!!!                END DO
!!!          END DO V2_LOOP
!!!    END IF
!!!    !
!!!    IF(PRESENT(DN2)) DN2 = D2 
!!!    IF(PRESENT(DN3)) DN3 = D3
!!!    IF(PRESENT(JUNCT)) JUNCT = CON 
!!!    !
!!!  END SUBROUTINE
  !
  MODULE PURE FUNCTION BACKSTEP_VERTEX_BACKPATH(VTX, V2) RESULT(BACKSTEP)
    CLASS(VERTEX_BACKPATH), INTENT(IN):: VTX
    INTEGER,                INTENT(IN):: V2
    INTEGER:: BACKSTEP
    !
    IF(V2 < ONE .OR. VTX%N < V2) THEN
        BACKSTEP = Z
    ELSE
        BACKSTEP = VTX%BACKPATH(V2)
    END IF
    !
  END FUNCTION
  !
  MODULE PURE FUNCTION NSTEP_VERTEX_BACKPATH(VTX, V2) RESULT(DIM)
    CLASS(VERTEX_BACKPATH), INTENT(IN):: VTX
    INTEGER,                INTENT(IN):: V2
    INTEGER:: DIM
    INTEGER:: I
    !
    DIM = Z
    IF(V2 > Z .AND. V2 <= VTX%N) THEN
                                 DIM = ONE
                                 I   = V2
                                 DO WHILE ( VTX%BACKPATH(I) > Z )
                                         DIM = DIM + ONE
                                         !
                                         I = VTX%BACKPATH(I)
                                 END DO
    END IF
    !
  END FUNCTION
  !
  MODULE PURE ELEMENTAL SUBROUTINE FINAL_DEALLOCATE_BINARY_HEAP(VTX)
    TYPE(VERTEX_BACKPATH), INTENT(INOUT):: VTX
    !
    VTX%N  = Z
    VTX%V1 = Z
    VTX%GOT_BACK = FALSE
    IF(ALLOCATED(VTX%BACKPATH)) DEALLOCATE(VTX%BACKPATH)
  END SUBROUTINE
  !
END SUBMODULE 
!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
!
!
!  Module superseded by features that can be performed by ADJACENCY_LIST_INSTRUCTION with ADJ_LST and VERTEX_BACKPATH
!
!
!!!MODULE MULTI_DIJKSTRA_SHORTEST_PATH!, ONLY: DIJKSTRA_PATHS
!!!  USE CONSTANTS
!!!  USE ALLOC_INTERFACE,        ONLY: ALLOC
!!!  USE BINARY_HEAP_INSTRUCTION,    ONLY: BINARY_HEAP
!!!  USE ADJACENCY_LIST_INSTRUCTION, ONLY: ADJ_LST
!!!  IMPLICIT NONE(TYPE, EXTERNAL)
!!!  PRIVATE
!!!  PUBLIC:: DIJKSTRA_PATHS
!!!  !
!!!  TYPE DIJKSTRA_PATHS
!!!      INTEGER:: DIM = Z
!!!      INTEGER:: N   = Z !Size of BACKPATH(N,DIM)
!!!      !
!!!      TYPE(BINARY_HEAP):: HEAP
!!!      INTEGER, DIMENSION(:),   ALLOCATABLE:: V1
!!!      LOGICAL, DIMENSION(:),   ALLOCATABLE:: SPT        !Shortest Path Tree -- Used by DIJKSTRA to keep track of the vertices which are currently in min HEAP.
!!!      INTEGER, DIMENSION(:,:), ALLOCATABLE:: BACKPATH  !Holdest best backstep for any vertex -- Used by DIJKSTRA to keep track of the vertices which are currently in min HEAP.
!!!      !
!!!      CONTAINS
!!!      !
!!!      PROCEDURE, PASS(PTH):: INIT      => ALLOCATE_DIJKSTRA_PATHS               !INIT(ADJ,VERT)
!!!      PROCEDURE, PASS(PTH):: GET_PATH  => GET_PATH_DIJKSTRA_SHORTEST_PATH       !GET_PATH(V1,V2,PATH,DIM)
!!!      PROCEDURE, PASS(PTH):: GET_COMMON=> CLOSEST_COMMON_DIJKSTRA_SHORTEST_PATH !GET_COMMON(V1,V2,V3,JUNCT)
!!!  END TYPE
!!!  !
!!!  CONTAINS
!!!  !
!!!  PURE SUBROUTINE ALLOCATE_DIJKSTRA_PATHS(PTH,ADJ,VERT)
!!!    CLASS(DIJKSTRA_PATHS), INTENT(INOUT):: PTH
!!!    CLASS(ADJ_LST),        INTENT(IN   ):: ADJ
!!!    INTEGER, DIMENSION(:), INTENT(IN   ):: VERT
!!!    INTEGER:: I, J, V1,U,V,P
!!!    DOUBLE PRECISION:: DIST_U, DIST_V, DIST 
!!!    !
!!!    PTH%DIM = SIZE(VERT)
!!!    PTH%N = ADJ%N
!!!    !
!!!    CALL ALLOC(PTH%V1, PTH%N, SRC=VERT)
!!!    !
!!!    CALL ALLOC(PTH%SPT, PTH%N)
!!!    !
!!!    CALL ALLOC(PTH%BACKPATH, PTH%N, PTH%DIM)
!!!    !
!!!    PTH%BACKPATH = Z
!!!    !
!!!    CALL PTH%HEAP%INIT(PTH%N)  !Set up Heap to sort PTH%N vertices
!!!    !
!!!    DO J=ONE, PTH%DIM
!!!            V1 = VERT(J)
!!!            !
!!!            PTH%SPT = TRUE
!!!            !
!!!            CALL PTH%HEAP%DIJKSTRA_INIT(V1)  !Set up HEAP For DIJKSTRA Algorithm 
!!!            !
!!!            DO WHILE (PTH%HEAP%SIZE() > Z)
!!!                !
!!!                CALL PTH%HEAP%POP(U, DIST_U)  !GET MIN FROM HEAP
!!!                !
!!!                IF(U > Z .AND. DIST_U < near_inf) THEN !dist is < inf
!!!                    !
!!!                    PTH%SPT(U) = FALSE  !Point is being explored so add it to the shortest path
!!!                    !
!!!                    DO I=ONE, ADJ%VER(U)%N
!!!                        !
!!!                        V = ADJ%VER(U)%CON(I)
!!!                        !
!!!                        IF(V > Z) THEN
!!!                           !
!!!                           IF(PTH%SPT(V)) THEN  !Have not visited point before
!!!                                !
!!!                                DIST = DIST_U + ADJ%VER(U)%WT(I)
!!!                                !
!!!                                CALL PTH%HEAP%GET(V,DIST_V,P)
!!!                                !
!!!                                IF( DIST < DIST_V) THEN
!!!                                    PTH%BACKPATH(V,J) = U
!!!                                    !
!!!                                    CALL PTH%HEAP%SET_X(P,DIST)      !Update with new lower distance
!!!                                END IF
!!!                           END IF
!!!                        END IF
!!!                    END DO
!!!                ELSE
!!!                    EXIT
!!!                END IF
!!!                !
!!!            END DO
!!!    END DO
!!!    !
!!!  END SUBROUTINE
!!!  !
!!!  PURE SUBROUTINE GET_PATH_DIJKSTRA_SHORTEST_PATH(PTH,V1,V2,PATH,DIM)
!!!    CLASS(DIJKSTRA_PATHS),               INTENT(INOUT):: PTH
!!!    INTEGER,                             INTENT(IN   ):: V1,V2
!!!    INTEGER,                             INTENT(  OUT):: DIM
!!!    INTEGER, DIMENSION(:), ALLOCATABLE,  INTENT(INOUT):: PATH
!!!    INTEGER:: I,J,K
!!!    !
!!!    K = Z
!!!    DO J=ONE, PTH%DIM
!!!        IF(V1 == PTH%V1(J)) THEN
!!!            K = J
!!!            EXIT
!!!        END IF
!!!    END DO
!!!    !
!!!    IF(K == Z) THEN                              !-----------------------
!!!                                      DIM = NEG                          !Vertex V1 was not part of the DIJKSTRA_PATHS initialization
!!!    ELSEIF(PTH%BACKPATH(V2,K) == Z) THEN        !-----------------------
!!!                                      DIM = Z                            !No connection between V1 and V2
!!!    ELSE !---------------------------------------------------------------
!!!                                      DIM = ONE
!!!                                      I   = V2
!!!                                      DO WHILE ( PTH%BACKPATH(I,K) > Z )
!!!                                              DIM = DIM + ONE
!!!                                              !
!!!                                              I = PTH%BACKPATH(I,K)
!!!                                      END DO
!!!                                      !
!!!                                      CALL ALLOC(PATH,DIM)
!!!                                      !
!!!                                      I       = V2
!!!                                      J       = DIM
!!!                                      DO J=DIM, ONE, NEG
!!!                                              !
!!!                                              PATH(J) = I
!!!                                              !
!!!                                              I = PTH%BACKPATH(I,K)
!!!                                      END DO
!!!    END IF  !------------------------------------------------------------
!!!    !
!!!  END SUBROUTINE
!!!  !
!!!  PURE SUBROUTINE CLOSEST_COMMON_DIJKSTRA_SHORTEST_PATH(PTH,V1,V2,V3,JUNCT)   !V1 and V2 common point to V3
!!!    CLASS(DIJKSTRA_PATHS),               INTENT(INOUT):: PTH
!!!    INTEGER,                             INTENT(IN   ):: V1,V2,V3
!!!    INTEGER,                             INTENT(  OUT):: JUNCT
!!!    INTEGER:: I,J, DIM1, DIM2
!!!    INTEGER, DIMENSION(:),ALLOCATABLE:: PATH1, PATH2
!!!    !
!!!    IF(V1 == V2) THEN
!!!        JUNCT = V1
!!!        RETURN
!!!    END IF
!!!    !
!!!    CALL GET_PATH_DIJKSTRA_SHORTEST_PATH(PTH,V1,V3,PATH1,DIM1)
!!!    !
!!!    IF    (DIM1 < Z ) THEN
!!!                       JUNCT = NEG
!!!    ELSEIF(DIM1 == Z) THEN
!!!                       JUNCT = Z
!!!    END IF
!!!    !
!!!    IF(DIM1 < ONE) RETURN
!!!    !
!!!    CALL GET_PATH_DIJKSTRA_SHORTEST_PATH(PTH,V2,V3,PATH2,DIM2)
!!!    !
!!!    IF    (DIM2 < Z ) THEN
!!!                       JUNCT = NEG
!!!    ELSEIF(DIM2 == Z) THEN
!!!                       JUNCT = Z
!!!    END IF
!!!    !
!!!    IF(DIM2 < ONE) RETURN
!!!    !
!!!    LP: DO I=ONE, DIM1
!!!        DO J=ONE, DIM2
!!!                     IF( PATH1(I) == PATH2(J) ) THEN
!!!                         JUNCT = PATH1(I)
!!!                         EXIT LP
!!!                     END IF
!!!        END DO
!!!    END DO LP
!!!    !
!!!  END SUBROUTINE
!!!  !
!!!END MODULE
!