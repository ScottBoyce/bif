!
! CODE DEVELOPED BY SCOTT E BOYCE 
!                   CONTACT <seboyce@usgs.gov> or <Boyce@engineer.com>
!
MODULE BINARY_HEAP_INSTRUCTION!, ONLY:BINARY_HEAP
  !
  !   Max or Min Binary Heap
  !  
  ! Usage:
  !   TYPE(BINARY_HEAP):: BIN
  !
  !   CALL BIN%INIT(DIM, [MIN_HEAP])        DIM = Maximum size of binary heap. MIN_HEAP = True, then Min Heap (which is the default). MIN_HEAP = False is Max Heap
  !
  !   CALL BIN%ADD(ID, VAL)                 Add to Binary Heap a new ID-Val entry. If Heap is full (SIZE=DIM) then an entry is removed to make room.
  !
  !   CALL BIN%POP(ID, VAL)                 Remove to top of the Binary Heap and return its ID and VAL
  !
  !   CALL BIN%GET(ID,VAL,POS)              Search heap for ID, return its position, POS, and value VAL.
  !
  !   CALL BIN%SIZE()                       Returns the current size of the heap (BIN%N)
  !
  !   CALL BIN%GET_DIM()                    Returns the maximum size of the heap (BIN%DIM)
  !
  !   CALL BIN%DIJKSTRA_INIT(ID, [MAX_ID])  Auto-builds heap with IDs from 1 to MAX_ID with the passed ID being at the top of the heap
  !                                            This is necessary for the Dijkstra Algorithm for vertex "ID"
  !                                            If MAX_ID is not included then it will automatically use DIM
  !                                            If MAX_ID > DIM, then the binary heap will reallocate so that DIM = MAX_ID
  !                                            Note this is equivalent to:
  !                                                                       CALL BIN%INIT(MAX_ID)
  !                                                                       CALL BIN%ADD(ID, 0.0)
  !                                                                       DO I=1, MAX_ID
  !                                                                                  IF(I .NE. ID) CALL BIN%ADD(I, 1D300)
  !                                                                       END DO
  !USE CONSTANTS
  USE, INTRINSIC:: ISO_FORTRAN_ENV, ONLY: REAL64
  IMPLICIT NONE(TYPE, EXTERNAL)
  PRIVATE
  PUBLIC:: BINARY_HEAP
  !
  ! Constants used internally to module ----------------------------------------------------
  ! 
  LOGICAL, PARAMETER:: TRUE  = .TRUE.
  LOGICAL, PARAMETER:: FALSE = .FALSE.
  INTEGER, PARAMETER:: NEG = -1
  INTEGER, PARAMETER:: Z   = 0
  INTEGER, PARAMETER:: ONE = 1
  INTEGER, PARAMETER:: TWO = 2
  DOUBLE PRECISION, PARAMETER:: DZ  = 0_REAL64
  DOUBLE PRECISION, PARAMETER:: inf = huge(DZ)
  !
  ! ----------------------------------------------------------------------------------------
  ! 
  TYPE BINARY_DATA
      INTEGER         :: I  
      DOUBLE PRECISION:: X 
  END TYPE
  !
  TYPE BINARY_CHILD
      TYPE(BINARY_DATA), POINTER:: PNT => NULL()  !Must explicitly deallocate/nullify
  END TYPE
  !
  TYPE BINARY_HEAP
      PRIVATE
      INTEGER:: DIM = Z  !Max Number of Children
      INTEGER:: N = Z    !Number of Childen in use
      LOGICAL:: MIN_HEAP = TRUE
      TYPE(BINARY_CHILD), DIMENSION(:), POINTER:: CHILD => NULL()
      !
      CONTAINS
      !
      PROCEDURE, PASS(BIN):: INIT         => ALLOCATE_BINARY_HEAP       !(DIM, [MIN_HEAP])
      PROCEDURE, PASS(BIN):: ADD          => INSERT_BINARY_HEAP         !(ID,VAL)
      PROCEDURE, PASS(BIN):: POP          => POP_TOP_BINARY_HEAP        !(ID,VAL)
      PROCEDURE, PASS(BIN):: GET          => GET_BY_ID_BINARY_HEAP      !(ID,VAL,POS)
      PROCEDURE, PASS(BIN):: SET_BUBBLE   => SET_BUBBLE_BINARY_HEAP     !(POS,ID, VAL, [NO_CHECK])
      PROCEDURE, PASS(BIN):: SET_X        => SET_X_BINARY_HEAP          !(POS,    VAL, [NO_CHECK])
      PROCEDURE, PASS(BIN):: SET_BY_ID    => SET_BY_ID_BINARY_HEAP      !(    ID, VAL, [NO_CHECK])
      PROCEDURE, PASS(BIN):: SWOP         => SWOP_BUBBLES_BINARY_HEAP   !(P1,P2)
      PROCEDURE, PASS(BIN):: SIZE         => ACTIVE_SIZE_BINARY_HEAP    !()  Returns BIN%N
      PROCEDURE, PASS(BIN):: GET_DIM      => GET_DIM_BINARY_HEAP       !()  Returns BIN%DIM
      !
      PROCEDURE, PASS(BIN):: DIJKSTRA_INIT => DIJKSTRA_INIT_BINARY_HEAP !(ID)
      !
      PROCEDURE, PASS(BIN):: DESTROY  => DEALLOCATE_BINARY_HEAP
      FINAL:: FINAL_DEALLOCATE_BINARY_HEAP
  END TYPE
  !
  CONTAINS
  !
  PURE FUNCTION ACTIVE_SIZE_BINARY_HEAP(BIN) RESULT(N)
                                 CLASS(BINARY_HEAP), INTENT(IN):: BIN
                                 INTEGER:: N
                                 N = BIN%N
  END FUNCTION
  !
  PURE FUNCTION GET_DIM_BINARY_HEAP(BIN) RESULT(DIM)
                                 CLASS(BINARY_HEAP), INTENT(IN):: BIN
                                 INTEGER:: DIM
                                 DIM = BIN%DIM
  END FUNCTION
  !
  PURE SUBROUTINE ALLOCATE_BINARY_HEAP(BIN, DIM, MIN_HEAP)
     CLASS(BINARY_HEAP),INTENT(INOUT):: BIN
     INTEGER,           INTENT(IN   ):: DIM
     LOGICAL, OPTIONAL, INTENT(IN   ):: MIN_HEAP
     INTEGER:: I
     !
     IF(BIN%DIM .NE. DIM) THEN  !Start with fresh HEAP since dimension is different
         !
         CALL BIN%DESTROY()
         !
         BIN%DIM = DIM
         !
         ALLOCATE(BIN%CHILD(DIM))
         !
         DO I=ONE, DIM
                      ALLOCATE(BIN%CHILD(I)%PNT)
                      BIN%CHILD(I)%PNT%I =  Z
                      BIN%CHILD(I)%PNT%X = DZ
         END DO
         !
     END IF
     !
     BIN%N = Z
     !
     IF(PRESENT(MIN_HEAP)) THEN
         BIN%MIN_HEAP = MIN_HEAP
     ELSE
         BIN%MIN_HEAP = TRUE
     END IF
     !
  END SUBROUTINE
  !
  PURE SUBROUTINE POP_TOP_BINARY_HEAP(BIN,ID,VAL)
     CLASS(BINARY_HEAP),INTENT(INOUT):: BIN
     INTEGER,           INTENT(  OUT):: ID
     DOUBLE PRECISION,  INTENT(  OUT):: VAL
     !
     INTEGER:: I
     !
     IF( BIN%N > Z ) THEN
         !
         I = ONE
         ID  = BIN%CHILD(I)%PNT%I
         VAL = BIN%CHILD(I)%PNT%X
         !
         CALL BIN%SWOP(I,BIN%N)  !Move bottom most position to top
         !
         BIN%N = BIN%N - ONE     !Drop new bottom
         !
         IF( BIN%N > ONE ) CALL SINK_DOWN_BINARY_HEAP(BIN,I)  !Move top position downward
     ELSE
         ID  = NEG
         VAL = inf
     END IF
     !
  END SUBROUTINE
  !
  PURE SUBROUTINE INSERT_BINARY_HEAP(BIN,ID,VAL)
     CLASS(BINARY_HEAP),INTENT(INOUT):: BIN
     INTEGER,           INTENT(IN   ):: ID
     DOUBLE PRECISION,  INTENT(IN   ):: VAL
     !
     INTEGER:: I
     !
     I = Z
     IF( BIN%N < BIN%DIM ) THEN
         BIN%N = BIN%N + ONE
         I = BIN%N
         !
         BIN%CHILD(I)%PNT%I =  ID
         BIN%CHILD(I)%PNT%X = VAL
     END IF
     !
     IF(I == Z) THEN  !FULL LIST DROP LOWEST LEVEL
        I = BIN%DIM
        !
        IF(BIN%MIN_HEAP) THEN
                        IF(BIN%CHILD(I)%PNT%X > VAL) THEN
                           BIN%CHILD(I)%PNT%I =  ID
                           BIN%CHILD(I)%PNT%X = VAL
                        ELSE
                            I = Z
                        END IF
        ELSE
                        IF(BIN%CHILD(I)%PNT%X < VAL) THEN
                           BIN%CHILD(I)%PNT%I =  ID
                           BIN%CHILD(I)%PNT%X = VAL
                        ELSE
                            I = Z
                        END IF
        END IF
     END IF
     !
     IF(I > ONE) CALL BUBBLE_UP_BINARY_HEAP(BIN,I)  !TRUE IF NEW ENTRY ADDED
     !
  END SUBROUTINE
  !
  PURE SUBROUTINE DIJKSTRA_INIT_BINARY_HEAP(BIN, ID, MAX_ID)  !HEAP from 1 to MAX_ID, with ID at the top
     !
     CLASS(BINARY_HEAP),INTENT(INOUT):: BIN
     INTEGER,           INTENT(IN   ):: ID
     INTEGER, OPTIONAL, INTENT(IN   ):: MAX_ID
     INTEGER::I,K,DIM
     !
     IF(PRESENT(MAX_ID)) THEN
                              DIM = MAX_ID
                              IF( DIM > BIN%DIM ) THEN
                                                  BLOCK
                                                      LOGICAL:: MIN_HEAP
                                                      MIN_HEAP = BIN%MIN_HEAP
                                                      CALL BIN%INIT(DIM, MIN_HEAP)
                                                  END BLOCK
                              END IF
     ELSE
                              DIM = BIN%DIM
     END IF
     
     !
     BIN%CHILD(ONE)%PNT%I = ID  !Head node is recieved ID
     BIN%CHILD(ONE)%PNT%X = DZ
     !
     K = ONE
     DO I=ONE, DIM
         IF ( I.NE.ID ) THEN
                           K = K + ONE
                           BIN%CHILD(K)%PNT%I = I
                           BIN%CHILD(K)%PNT%X = inf
         END IF
     END DO
     !
     BIN%N = DIM
     !
  END SUBROUTINE
  !
  PURE SUBROUTINE BUBBLE_UP_OR_SINK_BINARY_HEAP(BIN,POS)
     !
     CLASS(BINARY_HEAP),INTENT(INOUT):: BIN
     INTEGER,           INTENT(IN   ):: POS
     !
     INTEGER:: P, I
     LOGICAL:: MOVE_UP
     !
     IF(BIN%N > ONE) THEN
           I = POS          !Bubble to check
           !
           P = I/2          !Parent Position relative to check
           !
           IF    (I == ONE) THEN
                                    MOVE_UP = FALSE
           ELSEIF(I == BIN%N) THEN
                                    MOVE_UP = TRUE
           ELSEIF(BIN%MIN_HEAP) THEN
               IF(BIN%CHILD(P)%PNT%X > BIN%CHILD(I)%PNT%X) THEN
                                                           MOVE_UP = TRUE
               ELSE
                                                           MOVE_UP = FALSE
               END IF
           ELSE
               IF(BIN%CHILD(P)%PNT%X < BIN%CHILD(I)%PNT%X) THEN
                                                           MOVE_UP = TRUE
               ELSE
                                                           MOVE_UP = FALSE
               END IF
           END IF
           !
           IF(MOVE_UP) THEN
                       CALL BUBBLE_UP_BINARY_HEAP(BIN,POS)
           ELSE
                       CALL SINK_DOWN_BINARY_HEAP(BIN,POS)
           END IF
     END IF
     !
  END SUBROUTINE
  !
  PURE SUBROUTINE SET_BUBBLE_BINARY_HEAP(BIN,POS,ID,VAL,NO_CHECK)
     !
     CLASS(BINARY_HEAP),INTENT(INOUT):: BIN
     INTEGER,           INTENT(IN   ):: POS,ID
     DOUBLE PRECISION,  INTENT(IN   ):: VAL
     LOGICAL, OPTIONAL, INTENT(IN   ):: NO_CHECK
     LOGICAL:: CHECK
     !
     IF( Z < POS .AND. POS <= BIN%DIM ) THEN
                           !
                           BIN%CHILD(POS)%PNT%I =  ID
                           BIN%CHILD(POS)%PNT%X = VAL
                           !
                           IF(BIN%N < POS) BIN%N = POS
     END IF
     !
     CHECK = TRUE
     IF(PRESENT(NO_CHECK)) CHECK = .NOT. NO_CHECK
     !
     IF(CHECK) CALL BUBBLE_UP_OR_SINK_BINARY_HEAP(BIN,POS)
     !
  END SUBROUTINE
  !
  PURE SUBROUTINE SET_X_BINARY_HEAP(BIN,POS,VAL,NO_CHECK)
     !
     CLASS(BINARY_HEAP),INTENT(INOUT):: BIN
     INTEGER,           INTENT(IN   ):: POS
     DOUBLE PRECISION,  INTENT(IN   ):: VAL
     LOGICAL, OPTIONAL, INTENT(IN   ):: NO_CHECK
     LOGICAL:: CHECK
     !
     IF( Z < POS .AND. POS <= BIN%DIM ) THEN
                           !
                           BIN%CHILD(POS)%PNT%X = VAL
                           !
                           IF(BIN%N < POS) BIN%N = POS
     END IF
     !
     CHECK = TRUE
     IF(PRESENT(NO_CHECK)) CHECK = .NOT. NO_CHECK
     !
     IF(CHECK) CALL BUBBLE_UP_OR_SINK_BINARY_HEAP(BIN,POS)
     !
  END SUBROUTINE
  !
  PURE SUBROUTINE SORT_BY_VAL_BINARY_HEAP(BIN,FULL_TREE)
     !
     CLASS(BINARY_HEAP),INTENT(INOUT):: BIN
     LOGICAL, OPTIONAL, INTENT(IN   ):: FULL_TREE
     INTEGER:: I, J, N
     !
     N = BIN%N
     IF(PRESENT(FULL_TREE)) THEN;  IF(FULL_TREE) N = BIN%DIM
     END IF
     !
     IF(BIN%MIN_HEAP) THEN
         !
         DO I=ONE, N-ONE
             DO J=I+ONE, N
                 IF( BIN%CHILD(I)%PNT%X > BIN%CHILD(J)%PNT%X) CALL BIN%SWOP(I,J)
             END DO
         END DO
     ELSE
         DO I=ONE, N-ONE
             DO J=I+ONE, N
                 IF( BIN%CHILD(I)%PNT%X < BIN%CHILD(J)%PNT%X) CALL BIN%SWOP(I,J)
             END DO
         END DO
     END IF
     !
  END SUBROUTINE
  !
  PURE SUBROUTINE SORT_BY_ID_BINARY_HEAP(BIN,FULL_TREE, REVERSE)
     !
     CLASS(BINARY_HEAP),INTENT(INOUT):: BIN
     LOGICAL, OPTIONAL, INTENT(IN   ):: FULL_TREE, REVERSE
     INTEGER:: I, J, N
     LOGICAL:: MIN_DIR
     !
     N = BIN%N
     IF(PRESENT(FULL_TREE)) THEN;  IF(FULL_TREE) N = BIN%DIM
     END IF
     !
     MIN_DIR = TRUE
     IF(PRESENT(REVERSE)) MIN_DIR = .NOT. REVERSE
     !
     IF(MIN_DIR) THEN
         !
         DO I=ONE, N-ONE
             DO J=I+ONE, N
                 IF( BIN%CHILD(I)%PNT%I > BIN%CHILD(J)%PNT%I) CALL BIN%SWOP(I,J)
             END DO
         END DO
     ELSE
         DO I=ONE, N-ONE
             DO J=I+ONE, N
                 IF( BIN%CHILD(I)%PNT%I < BIN%CHILD(J)%PNT%I) CALL BIN%SWOP(I,J)
             END DO
         END DO
     END IF
     !
  END SUBROUTINE
  !
  PURE SUBROUTINE SET_BY_ID_BINARY_HEAP(BIN,ID,VAL,NO_CHECK)
     !
     CLASS(BINARY_HEAP),INTENT(INOUT):: BIN
     INTEGER,           INTENT(IN   ):: ID
     DOUBLE PRECISION,  INTENT(IN   ):: VAL
     LOGICAL, OPTIONAL, INTENT(IN   ):: NO_CHECK
     INTEGER:: I, P
     LOGICAL:: CHECK
     !
     DO I=ONE, BIN%N
         IF(ID == BIN%CHILD(I)%PNT%I)THEN
             BIN%CHILD(I)%PNT%X = VAL
             P = I
             EXIT
         END IF
     END DO
     !
     CHECK = TRUE
     IF(PRESENT(NO_CHECK)) CHECK = .NOT. NO_CHECK
     !
     IF(CHECK) CALL BUBBLE_UP_OR_SINK_BINARY_HEAP(BIN,P)
     !
  END SUBROUTINE
  !
  PURE SUBROUTINE GET_BY_ID_BINARY_HEAP(BIN,ID,VAL,POS)
     !
     CLASS(BINARY_HEAP),INTENT(INOUT):: BIN
     INTEGER,           INTENT(IN   ):: ID
     DOUBLE PRECISION,  INTENT(  OUT):: VAL
     INTEGER,           INTENT(  OUT):: POS
     INTEGER:: I
     !
     DO I=ONE, BIN%N
         IF( ID == BIN%CHILD(I)%PNT%I)THEN
             VAL = BIN%CHILD(I)%PNT%X
             POS = I
             EXIT
         END IF
     END DO
     !
  END SUBROUTINE
  !
  PURE SUBROUTINE SWOP_BUBBLES_BINARY_HEAP(BIN,P1,P2)
     !
     CLASS(BINARY_HEAP),INTENT(INOUT):: BIN
     INTEGER,           INTENT(IN   ):: P1,P2
     !
     TYPE(BINARY_DATA), POINTER:: PNT
     !
     PNT               => BIN%CHILD(P1)%PNT
     BIN%CHILD(P1)%PNT => BIN%CHILD(P2)%PNT
     BIN%CHILD(P2)%PNT => PNT              
     PNT => NULL()
     !
  END SUBROUTINE
  !
  PURE SUBROUTINE BUBBLE_UP_BINARY_HEAP(BIN,POS)
     !
     CLASS(BINARY_HEAP),INTENT(INOUT):: BIN
     INTEGER,           INTENT(IN   ):: POS
     !
     TYPE(BINARY_DATA), POINTER:: PNT
     INTEGER:: P, I
     !
     I = POS !Child  Position
     P = I/2 !Parent Position
     IF(BIN%MIN_HEAP) THEN
          DO WHILE ( P > Z )
             IF(BIN%CHILD(P)%PNT%X > BIN%CHILD(I)%PNT%X) THEN
                !
                PNT              => BIN%CHILD(P)%PNT   !Parent-Child Swap
                BIN%CHILD(P)%PNT => BIN%CHILD(I)%PNT   !Parent points to Child
                BIN%CHILD(I)%PNT => PNT                !Child poitns to Parent
                PNT => NULL()
                I = P
                P = I/2
             ELSE
                P = Z  !For stop on next itertation
             END IF
          END DO
     ELSE
          DO WHILE ( P > Z )
             IF(BIN%CHILD(P)%PNT%X < BIN%CHILD(I)%PNT%X) THEN
                !
                PNT              => BIN%CHILD(P)%PNT
                BIN%CHILD(I)%PNT => BIN%CHILD(P)%PNT
                BIN%CHILD(P)%PNT => PNT
                PNT => NULL()
                I = P
                P = I/2
             ELSE
                P = Z  !For stop on next itertation
             END IF
          END DO
     END IF
  END SUBROUTINE   
  !
  PURE SUBROUTINE SINK_DOWN_BINARY_HEAP(BIN,POS)
     !
     CLASS(BINARY_HEAP),INTENT(INOUT):: BIN
     INTEGER,           INTENT(IN   ):: POS
     !
     TYPE(BINARY_DATA), POINTER:: PNT
     INTEGER:: P, C1, C2
     !
     P  = POS         !Parent Position
     C1 = TWO*P       !Left  Child Position
     C2 = TWO*P + ONE !Right Child Position
     !
     IF(BIN%MIN_HEAP) THEN
          DO WHILE ( C1 <= BIN%N )
             !
             IF(C2 > BIN%N) C2 = BIN%N  !Ensures that Child 2 exists, if not only check Child 1
             !
             IF(BIN%CHILD(P)%PNT%X > BIN%CHILD(C1)%PNT%X .OR. BIN%CHILD(P)%PNT%X > BIN%CHILD(C2)%PNT%X) THEN    !Parent is Larger than Children, move upward smallest child
                 IF(BIN%CHILD(C1)%PNT%X < BIN%CHILD(C2)%PNT%X) THEN !Swap with Child 1, which is the smallest child
                    !
                    PNT               => BIN%CHILD(P)%PNT   !Parent-Child Swap
                    BIN%CHILD(P )%PNT => BIN%CHILD(C1)%PNT  !Parent  points to Child 1
                    BIN%CHILD(C1)%PNT => PNT                !Child 1 poitns to Parent
                    PNT => NULL()
                    P = C1
                    C1 = TWO*P 
                    C2 = TWO*P + ONE
                 ELSE                                       !Swap with Child 2, which is the smallest child
                    PNT               => BIN%CHILD(P)%PNT   !Parent-Child Swap
                    BIN%CHILD(P )%PNT => BIN%CHILD(C2)%PNT  !Parent  points to Child 1
                    BIN%CHILD(C2)%PNT => PNT                !Child 1 poitns to Parent
                    PNT => NULL()
                    P = C2
                    C1 = TWO*P 
                    C2 = TWO*P + ONE
                 END IF
             ELSE
                 C1 = BIN%N + ONE  !Force Exit on next iteration
             END IF
          END DO
     ELSE
          DO WHILE ( C1 <= BIN%N )
             !
             IF(C2 > BIN%N) C2 = C1  !Ensures that Child 2 exists, if not only check Child 1
             !
             IF(BIN%CHILD(P)%PNT%X < BIN%CHILD(C1)%PNT%X .OR. BIN%CHILD(P)%PNT%X < BIN%CHILD(C2)%PNT%X) THEN  !Parent is Smaller than Children, move upward largest child
                 IF(BIN%CHILD(C1)%PNT%X > BIN%CHILD(C2)%PNT%X) THEN !Swap with Child 1, which is the biggest child
                    !
                    PNT               => BIN%CHILD(P)%PNT   !Parent-Child Swap
                    BIN%CHILD(P )%PNT => BIN%CHILD(C1)%PNT  !Parent  points to Child 1
                    BIN%CHILD(C1)%PNT => PNT                !Child 1 poitns to Parent
                    PNT => NULL()
                    P = C1
                    C1 = TWO*P 
                    C2 = TWO*P + ONE
                 ELSE                                       !Swap with Child 2, which is the smallest child
                    PNT               => BIN%CHILD(P)%PNT   !Parent-Child Swap
                    BIN%CHILD(P )%PNT => BIN%CHILD(C2)%PNT  !Parent  points to Child 1
                    BIN%CHILD(C2)%PNT => PNT                !Child 1 poitns to Parent
                    PNT => NULL()
                    P = C2
                    C1 = TWO*P 
                    C2 = TWO*P + ONE
                 END IF
             ELSE
                 EXIT
             END IF
          END DO
     END IF
  END SUBROUTINE 
  !
  PURE ELEMENTAL SUBROUTINE DEALLOCATE_BINARY_HEAP(BIN)
    CLASS(BINARY_HEAP), INTENT(INOUT):: BIN
    INTEGER:: I, J
    !
    IF(BIN%DIM > Z) THEN
       DO I=ONE, BIN%DIM
                        DEALLOCATE(BIN%CHILD(I)%PNT, STAT=J)
                        BIN%CHILD(I)%PNT => NULL()
       END DO
       !
       DEALLOCATE(BIN%CHILD)
    END IF
    !
    BIN%DIM = Z
    BIN%N   = Z
    BIN%MIN_HEAP = TRUE
  END SUBROUTINE
  !
  PURE ELEMENTAL SUBROUTINE FINAL_DEALLOCATE_BINARY_HEAP(BIN)
    TYPE(BINARY_HEAP), INTENT(INOUT):: BIN
    !
    CALL DEALLOCATE_BINARY_HEAP(BIN)
  END SUBROUTINE
  !
END MODULE
!
!