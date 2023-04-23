!
! CODE DEVELOPED BY SCOTT E BOYCE 
!                   CONTACT <seboyce@usgs.gov> or <Boyce@engineer.com>
!
MODULE HASH_TABLE_INSTRUCTION
  USE, INTRINSIC:: ISO_FORTRAN_ENV, ONLY: i8 => INT8, i16 => INT16,  &
                                       i32 => INT32,  i64 => INT64,  &
                                       SNG => REAL32, DBL => REAL64, &
                                                      QAD => REAL128
  USE, INTRINSIC:: IEEE_ARITHMETIC, ONLY: IEEE_VALUE, IEEE_QUIET_NAN
  USE CONSTANTS,    ONLY: Z, BLNK
  USE PRIME_FINDER, ONLY: NEXT_PRIME
  !
  IMPLICIT NONE
  !
  PUBLIC:: HASH_TABLE
  PRIVATE
  !
  ! ----Uncomment to remove dependency on CONSTANTS---
  !
  !INTEGER,   PARAMETER:: Z    = 0
  !CHARACTER, PARAMETER:: BLNK = " "
  !
  ! --------------------------------------------------
  !
  ! Flag to indicate that hash entry was previously used, but then deleted.
  !
  CHARACTER(4), PARAMETER:: NULL = ACHAR(7)//ACHAR(10)//ACHAR(13)//ACHAR(7)  ! = "BELL"//"CR"//"LF"//"BELL"
  !
  TYPE KEY_VAL
      CHARACTER(:), ALLOCATABLE:: KEY
      CHARACTER(:), ALLOCATABLE:: VAL
      !
      CONTAINS
      !
      FINAL:: DESTROY_KEY_VAL
  END TYPE
  !
  TYPE:: HASH_TABLE
      INTEGER:: DIM = Z
      INTEGER:: CAP = Z
      !
      INTEGER:: PROBE = 33  !Must be >8, best is 33 or 63
      !
      TYPE(KEY_VAL), DIMENSION(:), ALLOCATABLE:: KV
      !
      CONTAINS
      !
      PROCEDURE, PASS(H):: INIT    => INITALIZE_HASH !([CAP], [PROBE])   -- Optional, is called if HASH used without preallocation
      PROCEDURE, PASS(H):: DESTROY =>   DESTROY_HASH !()   
      !
      GENERIC:: SET => SET_STR_STR_HASH, SET_STR_DBL_HASH, SET_STR_INT_HASH, SET_DBL_DBL_HASH, SET_INT_INT_HASH  !( KEY, VAL, [FOUND_KEY])                                                 
      GENERIC:: GET => GET_STR_STR_HASH, GET_STR_DBL_HASH, GET_STR_INT_HASH, GET_DBL_DBL_HASH, GET_INT_INT_HASH  !( KEY, VAL, [ERROR] )                                                    
      GENERIC:: DEL => DEL_STR_HASH, DEL_DBL_HASH, DEL_INT_HASH                                                  !( KEY )                                                                         
      !
      PROCEDURE, PASS(H):: LEN         => DIM_HASH
      !
      GENERIC::            ITER        => ITER_HASH_ALLOC,                       & !(START, [KEY], [VAL]) ==> CHARACTER(:):: KEY, VAL
                                          ITER_DBL_HASH_ALLOC, ITER_INT_HASH_ALLOC !VAL => INT OR DBL
      !
      PROCEDURE, PASS(H):: HAS_KEY     => HAS_KEY_HASH      !(KEY) RESULT(HAS_KEY); HAS_KEY = T or F
      PROCEDURE, PASS(H):: FIND_KEY    => FIND_KEY_POS_HASH !(KEY) RESULT(POS);     POS = 0 if not found
      !
      PROCEDURE, PASS(H):: GET_ASSUME  => GET_STR_STR_HASH_ASSUME !( KEY, VAL, [ERROR] ) ==> CHARACTER(*):: KEY, VAL               
      PROCEDURE, PASS(H):: ITER_ASSUME => ITER_HASH_ASSUME        !( START, [KEY], [VAL])    ==> CHARACTER(*):: KEY, VAL
      !
      PROCEDURE, PASS(H), PRIVATE:: SET_STR_STR_HASH
      PROCEDURE, PASS(H), PRIVATE:: SET_STR_DBL_HASH
      PROCEDURE, PASS(H), PRIVATE:: SET_STR_INT_HASH
      PROCEDURE, PASS(H), PRIVATE:: SET_DBL_DBL_HASH
      PROCEDURE, PASS(H), PRIVATE:: SET_INT_INT_HASH
      !
      PROCEDURE, PASS(H), PRIVATE:: GET_STR_STR_HASH
      PROCEDURE, PASS(H), PRIVATE:: GET_STR_DBL_HASH
      PROCEDURE, PASS(H), PRIVATE:: GET_STR_INT_HASH
      PROCEDURE, PASS(H), PRIVATE:: GET_DBL_DBL_HASH
      PROCEDURE, PASS(H), PRIVATE:: GET_INT_INT_HASH
      !
      PROCEDURE, PASS(H), PRIVATE:: DEL_STR_HASH
      PROCEDURE, PASS(H), PRIVATE:: DEL_DBL_HASH
      PROCEDURE, PASS(H), PRIVATE:: DEL_INT_HASH
      !
      PROCEDURE, PASS(H), PRIVATE:: ITER_HASH_ALLOC
      PROCEDURE, PASS(H), PRIVATE:: ITER_DBL_HASH_ALLOC
      PROCEDURE, PASS(H), PRIVATE:: ITER_INT_HASH_ALLOC
      !
      FINAL:: FINAL_HASH
      !
  END TYPE
  !
  CONTAINS
  !
  ! -----------------------------------------------------------------------------------------------
  !
  !  Rabin-Karp rolling hash is based on a linear congruential generator called 
  !    Multiplicative hash function
  !  ID = INI
  !  DO i as H = MODULO(H*Aparm + KEY[i], SIZ)
  !
  PURE FUNCTION HASHER(KEY, SIZ) RESULT(H)  !Assumes SIZ > 0 and Prime
     CHARACTER(*), INTENT(IN):: KEY
     INTEGER,      INTENT(IN):: SIZ
     INTEGER:: H, I, Aparm
     Aparm = 31
     H = Z
     DO I=1, LEN_TRIM(KEY)
                     H = H*Aparm + ICHAR( KEY(I:I) )
                     !H = MODULO(H, SIZ)
     END DO
     H = MODULO(H, SIZ) + 1
     !H = H + 1  !Fortran one-based indexing
     !
  END FUNCTION
  !
  ! -----------------------------------------------------------------------------------------------
  !
  PURE SUBROUTINE INITALIZE_HASH(H, CAP, PROBE)
    CLASS(HASH_TABLE),    INTENT(INOUT):: H
    INTEGER,    OPTIONAL, INTENT(IN   ):: CAP
    INTEGER,    OPTIONAL, INTENT(IN   ):: PROBE  !Default is 33, number of keys that can have the same ID before hash size increases to reduce redundant hashing. Larger values result in smaller CAP sizes for the same keyset.
    INTEGER:: MCAP, OLD_PROBE
    !
    IF(PRESENT(CAP)) THEN
        MCAP = (13 * CAP)/10 + 1  !Increase size by 30%
        MCAP = NEXT_PRIME(MCAP)   !Make sure its Prime
    ELSE
        MCAP = 47
    END IF
    !
    OLD_PROBE = H%PROBE
    H%PROBE = 33
    IF(PRESENT(PROBE)) THEN
            IF(PROBE > 3) H%PROBE = PROBE
    END IF
    !
    H%DIM = Z
    !
    IF (     ALLOCATED(H%KV)) THEN
                              IF( H%CAP < MCAP .OR. H%PROBE /= OLD_PROBE) THEN                !Current allocation not big enough
                                                    DEALLOCATE(H%KV)
                              ELSE
                                                    CALL DESTROY_KEY_VAL(H%KV)
                              END IF
    END IF
    !
    IF(.NOT. ALLOCATED(H%KV)) THEN
                              H%CAP = MCAP
                              ALLOCATE( H%KV(H%CAP) )
    END IF
    !
  END SUBROUTINE
  !
  ! -----------------------------------------------------------------------------------------------
  !
  PURE FUNCTION DIM_HASH(H) RESULT(DIM)
    CLASS(HASH_TABLE), INTENT(IN):: H
    INTEGER:: DIM
    !
    DIM = H%DIM
    !
  END FUNCTION
  !
  ! -----------------------------------------------------------------------------------------------
  !
  PURE SUBROUTINE SET_STR_STR_HASH(H, KEY, VAL, FOUND_KEY)
    CLASS(HASH_TABLE), INTENT(INOUT):: H
    CHARACTER(*),      INTENT(IN   ):: KEY, VAL
    LOGICAL, OPTIONAL, INTENT(  OUT):: FOUND_KEY
    !
    INTEGER:: ID
    LOGICAL:: FOUND
    !
    CALL FIND_OPEN_SLOT_HASH(H, KEY, ID, FOUND)
    !
    IF( ID == Z ) THEN  !Failed to find open slot, reallocate hash table
        DO WHILE (ID == Z)
                          CALL REHASH_HASH(H)
                          CALL FIND_OPEN_SLOT_KEY_VAL(H%PROBE, H%CAP, H%KV, KEY, ID, FOUND)  !Since we know CAP > 0
        END DO
    END IF
    !
    IF(FOUND) THEN
              H%KV(ID)%VAL = VAL
    ELSE
              H%KV(ID)%VAL = VAL
              H%KV(ID)%KEY = TRIM(KEY)
              H%DIM = H%DIM + 1
              !
              IF( H%DIM > (7*H%CAP) / 10 ) CALL REHASH_HASH(H) !Hash >70% Full, rebuild
    END IF
    !
    IF(PRESENT(FOUND_KEY)) FOUND_KEY = FOUND
    !
  END SUBROUTINE
  !
  ! -----------------------------------------------------------------------------------------------
  !
  PURE SUBROUTINE FIND_OPEN_SLOT_HASH(H, KEY, POS, FOUND) 
    CLASS(HASH_TABLE), INTENT(INOUT):: H
    CHARACTER(*),      INTENT(IN   ):: KEY
    INTEGER,           INTENT(  OUT):: POS
    LOGICAL,           INTENT(  OUT):: FOUND
    !
    !
    IF( H%CAP > Z ) THEN
                    CALL FIND_OPEN_SLOT_KEY_VAL(H%PROBE, H%CAP, H%KV, KEY, POS, FOUND)
    ELSE
                    FOUND = .FALSE.
                    POS   = Z
    END IF
    !
  END SUBROUTINE
  !
  PURE SUBROUTINE FIND_OPEN_SLOT_KEY_VAL(LIM, CAP, KV, KEY, POS, FOUND) !Assumes CAP > 0 and LIM > 0
    INTEGER,                       INTENT(IN   ):: LIM
    INTEGER,                       INTENT(IN   ):: CAP
    TYPE(KEY_VAL), DIMENSION(CAP), INTENT(IN   ):: KV
    CHARACTER(*),                  INTENT(IN   ):: KEY
    INTEGER,                       INTENT(  OUT):: POS
    LOGICAL,                       INTENT(  OUT):: FOUND
    !
    INTEGER:: I, ID
    !
    FOUND = .FALSE.
    POS = Z
    !
    ID = HASHER(KEY, CAP)
    !
    DO I=1, LIM  !LIM is the number of allowable linear additions
        !
        IF (ALLOCATED(KV(ID)%KEY)) THEN
            IF      ( KV(ID)%KEY == KEY  ) THEN
                                           POS = ID
                                           FOUND = .TRUE.
                                           EXIT
            ELSEIF  ( POS == Z .AND. KV(ID)%KEY == NULL ) THEN
                                           POS = ID
            END IF
        ELSE
            IF( POS == Z ) POS = ID
            EXIT
        END IF
        !
        ID = ID + 1
        IF(ID > CAP) ID = 1
    END DO
    !
  END SUBROUTINE
  !
  ! -----------------------------------------------------------------------------------------------
  !
  PURE FUNCTION HAS_KEY_HASH(H, KEY) RESULT(HAS_KEY)
    CLASS(HASH_TABLE), INTENT(IN):: H
    CHARACTER(*),      INTENT(IN):: KEY
    LOGICAL:: HAS_KEY
    !
    IF (H%CAP > Z) THEN
                   HAS_KEY = FIND_KEY_POS_KEY_VAL(H%PROBE, H%CAP, H%KV, KEY) > 0
    ELSE
                   HAS_KEY = .FALSE.
    END IF
    !
  END FUNCTION
  !
  ! -----------------------------------------------------------------------------------------------
  !
  PURE FUNCTION FIND_KEY_POS_HASH(H, KEY) RESULT(POS)
    CLASS(HASH_TABLE), INTENT(IN):: H
    CHARACTER(*),      INTENT(IN):: KEY
    INTEGER:: POS
    !
    IF (H%CAP > Z) THEN
                   POS = FIND_KEY_POS_KEY_VAL(H%PROBE, H%CAP, H%KV, KEY)
    ELSE
                   POS = Z
    END IF
    !
  END FUNCTION
  !
  PURE FUNCTION FIND_KEY_POS_KEY_VAL(LIM, CAP, KV, KEY) RESULT(POS) !Assumes CAP > 0
    INTEGER,                       INTENT(IN):: LIM
    INTEGER,                       INTENT(IN):: CAP
    TYPE(KEY_VAL), DIMENSION(CAP), INTENT(IN):: KV
    CHARACTER(*),                  INTENT(IN):: KEY
    INTEGER:: POS
    !
    INTEGER:: ID, I
    !
    POS = Z
    ID  = HASHER(KEY, CAP)
    !
    DO I=1, LIM
            IF(.NOT. ALLOCATED(KV(ID)%KEY)) THEN
                                            EXIT
            ELSEIF(     KEY == KV(ID)%KEY ) THEN
                                            POS = ID
                                            EXIT
            END IF
            !
            ID = ID + 1
            IF(ID > CAP) ID = 1
    END DO
    !
  END FUNCTION
  !
  ! -----------------------------------------------------------------------------------------------
  !
  PURE SUBROUTINE REHASH_HASH(H)
    CLASS(HASH_TABLE), INTENT(INOUT):: H
    !
    TYPE(KEY_VAL), DIMENSION(:), ALLOCATABLE:: TMP
    INTEGER:: CAP, I, J, LIM
    LOGICAL:: FOUND
    !
    IF (.NOT. ALLOCATED(H%KV)) THEN
                              CALL INITALIZE_HASH(H)
                              RETURN
    END IF
    !
    CAP = H%CAP
    LIM = H%PROBE
    DO WHILE (.NOT. ALLOCATED(TMP)) 
        !
        CAP = (13 * CAP)/10 + 1  !Increase size by 30%
        !
        IF    (CAP <= 47) THEN          ! Low level Prime hard wire for speed
                              CAP = 47
        ELSEIF(CAP <= 127) THEN 
                              CAP = 127
        ELSE
            CAP = NEXT_PRIME(CAP)
        END IF
        !
        ALLOCATE(TMP(CAP))
        !
        ! I = Index for   H%KV
        ! J = Index for TMP%KV
        !
        I = Z
        IF( H%CAP > Z ) I = ITER_KEY_VAL(H%CAP, H%KV, I)
        !
        DO WHILE (I > Z)
            CALL FIND_OPEN_SLOT_KEY_VAL(LIM, CAP, TMP, H%KV(I)%KEY, J, FOUND)
            !
            IF(J == Z) THEN             !Failed to find open bucket, so rehash again
                         DEALLOCATE(TMP)
                         EXIT
            END IF
            !
            TMP(J)%KEY = H%KV(I)%KEY
            TMP(J)%VAL = H%KV(I)%VAL
            !
            I = ITER_KEY_VAL(H%CAP, H%KV, I)
            !
        END DO
        !
    END DO
    !
    H%CAP = CAP
    CALL MOVE_ALLOC(TMP, H%KV)
    !
  END SUBROUTINE
  !
  ! -----------------------------------------------------------------------------------------------
  !
  PURE FUNCTION ITER_KEY_VAL(CAP, KV, START) RESULT(POS) !Assumes CAP > 0, set START=0 on before first call, POS=0 if no more keys
    INTEGER,                       INTENT(IN):: CAP
    TYPE(KEY_VAL), DIMENSION(CAP), INTENT(IN):: KV
    INTEGER,                       INTENT(IN):: START
    !
    INTEGER:: I, POS
    !
    POS = -1
    DO I=START + 1, CAP  
        IF (ALLOCATED(KV(I)%KEY)) THEN
                 IF ( KV(I)%KEY /= NULL ) THEN
                                          POS = I
                                          EXIT
                 END IF
        END IF
    END DO
    !
  END FUNCTION
  !
  PURE SUBROUTINE ITER_HASH_ASSUME(H, START, KEY, VAL)
    CLASS(HASH_TABLE),      INTENT(IN   ):: H
    INTEGER,                INTENT(INOUT):: START
    CHARACTER(*), OPTIONAL, INTENT(INOUT):: KEY
    CHARACTER(*), OPTIONAL, INTENT(INOUT):: VAL
    !
    INTEGER:: POS
    !
    POS = ITER_KEY_VAL(H%CAP, H%KV, START)
    !
    IF(PRESENT(VAL))THEN
       IF (POS > Z) THEN
           KEY = H%KV(POS)%KEY
       ELSE
           KEY = BLNK
       END IF
    END IF
    !
    IF(PRESENT(VAL))THEN
       IF (POS > Z) THEN
           VAL = H%KV(POS)%VAL
       ELSE
           VAL = BLNK
       END IF
    END IF
    !
    START = POS
    !
  END SUBROUTINE
  !
  PURE SUBROUTINE ITER_HASH_ALLOC(H, START, KEY, VAL)
    CLASS(HASH_TABLE),                  INTENT(IN   ):: H
    INTEGER,                            INTENT(INOUT):: START
    CHARACTER(:),ALLOCATABLE, OPTIONAL, INTENT(INOUT):: KEY
    CHARACTER(:),ALLOCATABLE, OPTIONAL, INTENT(INOUT):: VAL
    !
    INTEGER:: POS
    !
    POS = ITER_KEY_VAL(H%CAP, H%KV, START)
    !
    IF(PRESENT(VAL))THEN
       IF (POS > Z) THEN
           KEY = H%KV(POS)%KEY
       ELSE
           KEY = BLNK
       END IF
    END IF
    !
    IF(PRESENT(VAL))THEN
       IF (POS > Z) THEN
           VAL = H%KV(POS)%VAL
       ELSE
           VAL = BLNK
       END IF
    END IF
    !
    START = POS
    !
  END SUBROUTINE
  !
  PURE SUBROUTINE ITER_DBL_HASH_ALLOC(H, START, KEY, VAL)
    CLASS(HASH_TABLE),        INTENT(IN   ):: H
    INTEGER,                  INTENT(INOUT):: START
    CHARACTER(:),ALLOCATABLE, INTENT(INOUT):: KEY
    REAL(DBL),                INTENT(INOUT):: VAL
    !
    INTEGER:: POS
    !
    POS = ITER_KEY_VAL(H%CAP, H%KV, START)
    !
    IF (POS > Z) THEN
        KEY = H%KV(POS)%KEY
        VAL = TRANSFER(H%KV(POS)%VAL, VAL)
    ELSE
        KEY = BLNK
        VAL = 0
    END IF
    !
    START = POS
    !
  END SUBROUTINE
  !
  PURE SUBROUTINE ITER_INT_HASH_ALLOC(H, START, KEY, VAL)
    CLASS(HASH_TABLE),        INTENT(IN   ):: H
    INTEGER,                  INTENT(INOUT):: START
    CHARACTER(:),ALLOCATABLE, INTENT(INOUT):: KEY
    INTEGER,                  INTENT(INOUT):: VAL
    !
    INTEGER:: POS
    !
    POS = ITER_KEY_VAL(H%CAP, H%KV, START)
    !
    IF (POS > Z) THEN
        KEY = H%KV(POS)%KEY
        VAL = TRANSFER(H%KV(POS)%VAL, VAL)
    ELSE
        KEY = BLNK
        VAL = 0
    END IF
    !
    START = POS
    !
  END SUBROUTINE
  !
  ! -----------------------------------------------------------------------------------------------
  !
  PURE SUBROUTINE SET_STR_DBL_HASH(H, KEY, VAL, ERROR)
    CLASS(HASH_TABLE), INTENT(INOUT):: H
    CHARACTER(*),      INTENT(IN   ):: KEY
    REAL(DBL),         INTENT(IN   ):: VAL
    LOGICAL, OPTIONAL, INTENT(  OUT):: ERROR
    CHARACTER(8):: CVAL
    !
    CVAL = TRANSFER(VAL,CVAL)
    CALL SET_STR_STR_HASH(H, KEY, CVAL, ERROR)
    !
  END SUBROUTINE
  !
  PURE SUBROUTINE SET_STR_INT_HASH(H, KEY, VAL, ERROR)
    CLASS(HASH_TABLE), INTENT(INOUT):: H
    CHARACTER(*),      INTENT(IN   ):: KEY
    INTEGER,           INTENT(IN   ):: VAL
    LOGICAL, OPTIONAL, INTENT(  OUT):: ERROR
    CHARACTER(4):: CVAL
    !
    CVAL = TRANSFER(VAL,CVAL)
    CALL SET_STR_STR_HASH(H, KEY, CVAL, ERROR)
    !
  END SUBROUTINE
  !
  PURE SUBROUTINE SET_DBL_DBL_HASH(H, KEY, VAL, ERROR)
    CLASS(HASH_TABLE), INTENT(INOUT):: H
    REAL(DBL),         INTENT(IN   ):: KEY
    REAL(DBL),         INTENT(IN   ):: VAL
    LOGICAL, OPTIONAL, INTENT(  OUT):: ERROR
    CHARACTER(8):: CKEY, CVAL
    !
    CKEY = TRANSFER(KEY,CKEY)
    CVAL = TRANSFER(VAL,CVAL)
    CALL SET_STR_STR_HASH(H, CKEY, CVAL, ERROR)
    !
  END SUBROUTINE
  !
  PURE SUBROUTINE SET_INT_INT_HASH(H, KEY, VAL, ERROR)
    CLASS(HASH_TABLE), INTENT(INOUT):: H
    INTEGER,           INTENT(IN   ):: KEY
    INTEGER,           INTENT(IN   ):: VAL
    LOGICAL, OPTIONAL, INTENT(  OUT):: ERROR
    CHARACTER(4):: CKEY, CVAL
    !
    CKEY = TRANSFER(KEY,CKEY)
    CVAL = TRANSFER(VAL,CVAL)
    CALL SET_STR_STR_HASH(H, CKEY, CVAL, ERROR)
    !
  END SUBROUTINE
  !
  ! -----------------------------------------------------------------------------------------------
  !
  PURE SUBROUTINE GET_STR_STR_HASH(H, KEY, VAL, ERROR)
    CLASS(HASH_TABLE),         INTENT(IN   ):: H
    CHARACTER(*),              INTENT(IN   ):: KEY
    CHARACTER(:), ALLOCATABLE, INTENT(INOUT):: VAL
    LOGICAL,         OPTIONAL, INTENT(  OUT):: ERROR
    INTEGER:: I
    !
    IF (H%CAP > Z) THEN
                   I = FIND_KEY_POS_KEY_VAL(H%PROBE, H%CAP, H%KV, KEY)
    ELSE
                   I = Z
    END IF
    !
    IF (I > Z) THEN
        VAL = H%KV(I)%VAL
        IF(PRESENT(ERROR)) ERROR = .FALSE.
    ELSE
        VAL = BLNK
        IF(PRESENT(ERROR)) ERROR = .TRUE.
    END IF
    !
  END SUBROUTINE
  !
  PURE SUBROUTINE GET_STR_STR_HASH_ASSUME(H, KEY, VAL, ERROR)
    CLASS(HASH_TABLE),     INTENT(IN   ):: H
    CHARACTER(*),          INTENT(IN   ):: KEY
    CHARACTER(*),          INTENT(INOUT):: VAL
    LOGICAL,     OPTIONAL, INTENT(  OUT):: ERROR
    INTEGER:: I
    !
    IF (H%CAP > Z) THEN
                   I = FIND_KEY_POS_KEY_VAL(H%PROBE, H%CAP, H%KV, KEY)
    ELSE
                   I = Z
    END IF
    !
    IF (I > Z) THEN
        VAL = H%KV(I)%VAL
        IF(PRESENT(ERROR)) ERROR = .FALSE.
    ELSE
        VAL = BLNK
        IF(PRESENT(ERROR)) ERROR = .TRUE.
    END IF
    !
  END SUBROUTINE
  !
  PURE SUBROUTINE GET_STR_DBL_HASH(H, KEY, VAL, ERROR)
    CLASS(HASH_TABLE), INTENT(INOUT):: H
    CHARACTER(*),      INTENT(IN   ):: KEY
    REAL(DBL),         INTENT(  OUT):: VAL
    LOGICAL, OPTIONAL, INTENT(  OUT):: ERROR
    CHARACTER(8):: CVAL
    !
    CALL GET_STR_STR_HASH_ASSUME(H, KEY, CVAL, ERROR)
    !
    IF (CVAL /= BLNK) THEN
        VAL = TRANSFER(CVAL, VAL)
    ELSE
        VAL = IEEE_VALUE(VAL, IEEE_QUIET_NAN)
    END IF
    !
  END SUBROUTINE
  !
  PURE SUBROUTINE GET_STR_INT_HASH(H, KEY, VAL, ERROR)
    CLASS(HASH_TABLE), INTENT(INOUT):: H
    CHARACTER(*),      INTENT(IN   ):: KEY
    INTEGER,           INTENT(  OUT):: VAL
    LOGICAL, OPTIONAL, INTENT(  OUT):: ERROR
    CHARACTER(4):: CVAL
    !
    CALL GET_STR_STR_HASH_ASSUME(H, KEY, CVAL, ERROR)
    !
    IF (CVAL /= BLNK) THEN
        VAL = TRANSFER(CVAL, VAL)
    ELSE
        VAL = HUGE(VAL)
    END IF
    !
  END SUBROUTINE
  !
  PURE SUBROUTINE GET_DBL_DBL_HASH(H, KEY, VAL, ERROR)
    CLASS(HASH_TABLE), INTENT(INOUT):: H
    REAL(DBL),         INTENT(IN   ):: KEY
    REAL(DBL),         INTENT(  OUT):: VAL
    LOGICAL, OPTIONAL, INTENT(  OUT):: ERROR
    CHARACTER(8):: CKEY, CVAL
    !
    CKEY = TRANSFER(KEY,CKEY)
    CALL GET_STR_STR_HASH_ASSUME(H, CKEY, CVAL, ERROR)
    !
    IF (CVAL /= BLNK) THEN
        VAL = TRANSFER(CVAL, VAL)
    ELSE
        VAL = HUGE(VAL)
    END IF
    !
  END SUBROUTINE
  !
  PURE SUBROUTINE GET_INT_INT_HASH(H, KEY, VAL, ERROR)
    CLASS(HASH_TABLE), INTENT(INOUT):: H
    INTEGER,           INTENT(IN   ):: KEY
    INTEGER,           INTENT(  OUT):: VAL
    LOGICAL, OPTIONAL, INTENT(  OUT):: ERROR
    CHARACTER(4):: CKEY, CVAL
    !
    CKEY = TRANSFER(KEY,CKEY)
    CALL GET_STR_STR_HASH_ASSUME(H, CKEY, CVAL, ERROR)
    !
    IF (CVAL /= BLNK) THEN
        VAL = TRANSFER(CVAL, VAL)
    ELSE
        VAL = HUGE(VAL)
    END IF
    !
  END SUBROUTINE
  !
  ! -----------------------------------------------------------------------------------------------
  !
  PURE SUBROUTINE DEL_STR_HASH(H, KEY)
    CLASS(HASH_TABLE), INTENT(INOUT):: H
    CHARACTER(*),      INTENT(IN   ):: KEY
    INTEGER:: I
    !
    IF (H%CAP > Z) THEN
                   I = FIND_KEY_POS_KEY_VAL(H%PROBE, H%CAP, H%KV, KEY)
                   !
                   IF( I > Z ) THEN
                               H%KV(I)%KEY = NULL
                               DEALLOCATE(H%KV(I)%VAL)
                               H%DIM = H%DIM - 1
                   END IF
    END IF
    !
  END SUBROUTINE
  !
  PURE SUBROUTINE DEL_DBL_HASH(H, KEY)
    CLASS(HASH_TABLE), INTENT(INOUT):: H
    REAL(DBL),         INTENT(IN   ):: KEY
    CHARACTER(8):: CKEY
    INTEGER:: I
    !
    IF (H%CAP > Z) THEN
                   CKEY = TRANSFER(KEY,CKEY)
                   I = FIND_KEY_POS_KEY_VAL(H%PROBE, H%CAP, H%KV, CKEY)
                   !
                   IF( I > Z ) THEN
                               H%KV(I)%KEY = NULL
                               DEALLOCATE(H%KV(I)%VAL)
                   END IF
    END IF
    !
  END SUBROUTINE
  !
  PURE SUBROUTINE DEL_INT_HASH(H, KEY)
    CLASS(HASH_TABLE), INTENT(INOUT):: H
    INTEGER,           INTENT(IN   ):: KEY
    CHARACTER(4):: CKEY
    INTEGER:: I
    !
    IF (H%CAP > Z) THEN
                   CKEY = TRANSFER(KEY,CKEY)
                   I = FIND_KEY_POS_KEY_VAL(H%PROBE, H%CAP, H%KV, CKEY)
                   !
                   IF( I > Z ) THEN
                               H%KV(I)%KEY = NULL
                               DEALLOCATE(H%KV(I)%VAL)
                   END IF
    END IF
    !
  END SUBROUTINE
  !
  ! -----------------------------------------------------------------------------------------------
  !
  PURE SUBROUTINE DESTROY_HASH(H)
    CLASS(HASH_TABLE), INTENT(INOUT):: H
    !
    H%DIM = Z
    H%CAP = Z
    H%PROBE = 33
    !
    IF (ALLOCATED(H%KV)) DEALLOCATE(H%KV)
    !
  END SUBROUTINE
  !
  PURE ELEMENTAL SUBROUTINE FINAL_HASH(H)
    TYPE(HASH_TABLE), INTENT(INOUT):: H
    !
    CALL DESTROY_HASH(H)
    !
  END SUBROUTINE
  !
  PURE ELEMENTAL SUBROUTINE DESTROY_KEY_VAL(KV)
    TYPE(KEY_VAL), INTENT(INOUT):: KV
    !
    IF(ALLOCATED(KV%KEY)) DEALLOCATE(KV%KEY)
    IF(ALLOCATED(KV%VAL)) DEALLOCATE(KV%VAL)
    !
  END SUBROUTINE
  !
    END MODULE
    

!!!    !
!!!    PURE SUBROUTINE SET_HASH(H, KEY, VAL, ERROR)
!!!      CLASS(HASH_TABLE),       INTENT(INOUT):: H
!!!      CHARACTER(*),      INTENT(IN   ):: KEY, 
!!!      CLASS(*),          INTENT(IN   ):: VAL
!!!      LOGICAL, OPTIONAL, INTENT(  OUT):: ERROR
!!!      !
!!!      CHARACTER(8):: CVAL
!!!      !
!!!      SELECT TYPE(VAL)    !CVAL = TRANSFER (source,mold[,size])
!!!      CLASS(CHARACTER(*)); CVAL = TRANSFER(VAL, CVAL())
!!!      CLASS(REAL(DBL)   ); CVAL = TRANSFER(VAL, CVAL())
!!!      CLASS(INTEGER(i32)); CVAL = TRANSFER(VAL, CVAL())
!!!      CLASS(INTEGER(i64)); CVAL = TRANSFER(VAL, CVAL())
!!!      CLASS(INTEGER(i16)); CVAL = TRANSFER(VAL, CVAL())
!!!      CLASS(INTEGER(i8) ); CVAL = TRANSFER(VAL, CVAL())
!!!      CLASS(REAL(SNG)   ); CVAL = TRANSFER(VAL, CVAL())
!!!      END SELECT
!!!      !
!!!      CALL SET_STR_STR_HASH(H, KEY, VAL, ERROR)
!!!      !
!!!    END SUBROUTINE
    
    
    

  !!!PURE FUNCTION NEXT_PRIME(N) RESULT(P)
  !!!  INTEGER, INTENT(IN):: N
  !!!  INTEGER:: P, I, M
  !!!  !
  !!!  IF ( N < 17 ) THEN
  !!!      IF     ( N < 2  ) THEN; P = 2 
  !!!      ELSEIF ( N < 3  ) THEN; P = 3 
  !!!      ELSEIF ( N < 5  ) THEN; P = 5 
  !!!      ELSEIF ( N < 7  ) THEN; P = 7 
  !!!      ELSEIF ( N < 11 ) THEN; P = 11
  !!!      ELSEIF ( N < 13 ) THEN; P = 13
  !!!      ELSE;                   P = 17   !IF ( N < 17 ) THEN
  !!!      END IF
  !!!  ELSE !-------------------------------------
  !!!      IF( MODULO(N, 2) == 0 ) THEN          ! Even number, so increment by 1
  !!!                                  P = N + 1
  !!!      ELSE
  !!!                                  P = N
  !!!      END IF
  !!!      !
  !!!      PRIME: DO 
  !!!               P = P + 1
  !!!               IF ( MODULO(P, 2) > 0 .AND. MODULO(P, 3) > 0 ) THEN
  !!!                                                              M = 0
  !!!                                                              DO I = 5, 2*P, 6
  !!!                                                                  IF ( I*I > P ) THEN
  !!!                                                                                 M = I
  !!!                                                                                 EXIT
  !!!                                                                  END IF
  !!!                                                              END DO
  !!!                                                              !
  !!!                                                              DO I = 5, M, 6
  !!!                                                              END DO
  !!!                                                              
  !!!               END IF
  !!!      END DO PRIME
  !!!  END IF
    
    
    
    
  !
  !!PURE SUBROUTINE FIND_OPEN_SLOT_HASH(H, KEY, POS, FOUND) 
  !!  CLASS(HASH_TABLE),       INTENT(INOUT):: H
  !!  CHARACTER(*),      INTENT(IN   ):: KEY
  !!  INTEGER,           INTENT(  OUT):: POS
  !!  LOGICAL,           INTENT(  OUT):: FOUND
  !!  !
  !!  INTEGER:: I, ID
  !!  !
  !!  FOUND = .FALSE.
  !!  POS = Z
  !!  !
  !!  IF( H%CAP > Z ) THEN
  !!     !
  !!     ID = HASHER(KEY, H%CAP)
  !!     !
  !!     DO I=1, H%PROBE  
  !!         !
  !!         IF (ALLOCATED(H%KV(ID)%KEY)) THEN
  !!             IF    ( H%KV(ID)%KEY == KEY      ) THEN
  !!                                             POS = ID
  !!                                             FOUND = .TRUE.
  !!                                             EXIT
  !!             ELSEIF( H%KV(ID)%KEY == NULL) THEN
  !!                                             POS = ID
  !!             END IF
  !!         ELSE
  !!             IF( POS == Z )                  POS = ID
  !!             EXIT
  !!         END IF
  !!         !
  !!         ID = ID + 1
  !!         IF(ID > H%CAP) ID = H%CAP
  !!     END DO
  !!  END IF
  !!  !
  !!END SUBROUTINE
  !!!END FUNCTION
    
    
  !
  ! -----------------------------------------------------------------------------------------------
  !
  !PURE SUBROUTINE FREE_KEY_VAL(KV)
  !  CLASS(KEY_VAL), INTENT(INOUT):: KV
  !  !
  !  IF(ALLOCATED(KV%KEY)) THEN
  !                        DEALLOCATE(KV%KEY)
  !                        KV%KEY = NULL
  !  END IF
  !  !
  !  IF(ALLOCATED(KV%VAL)) DEALLOCATE(KV%VAL)
  !  !
  !END SUBROUTINE
  !
  !PURE SUBROUTINE SET_KEY_VAL(KV, KEY, VAL)
  !  CLASS(KEY_VAL), INTENT(INOUT):: KV
  !  CHARACTER(*),   INTENT(IN   ):: KEY, VAL
  !  !
  !  KV%KEY = TRIM(KEY)
  !  KV%VAL = VAL
  !  !
  !END SUBROUTINE
  !!
  !PURE FUNCTION KEY_CHECK_KEY_VAL(KV, KEY) RESULT(ANS)
  !  CLASS(KEY_VAL), INTENT(IN):: KV
  !  CHARACTER(*),   INTENT(IN):: KEY
  !  LOGICAL:: ANS
  !  !
  !  IF(ALLOCATED(KV%KEY)) THEN
  !                         ANS = KEY == KV%KEY
  !  ELSE
  !                         ANS = .FALSE.
  !  END IF
  !  !
  !END FUNCTION
    
    
    
  !  
  !!
  !TYPE VAL_SETTER
  !    CHARACTER(:), POINTER:: VAL
  !    !
  !    CONTAINS
  !    !
  !    GENERIC:: ASSIGNMENT(=) => SET_DBL_VAL_SETTER, SET_INT_VAL_SETTER
  !    PROCEDURE:: SET_DBL_VAL_SETTER
  !    PROCEDURE:: SET_INT_VAL_SETTER
  !    !
  !    FINAL:: DESTROY_VAL_SETTER
  !END TYPE
  !  
  !!
  !! -----------------------------------------------------------------------------------------------
  !!
  !SUBROUTINE SET_TARGET_VAL_SETTER(VS, TAR)
  !  TYPE(VAL_SETTER),     INTENT(INOUT):: VS
  !  CHARACTER(*), TARGET, INTENT(IN   ):: TAR
  !  !
  !  VS%VAL => TAR
  !  !
  !END SUBROUTINE
  !!
  !PURE SUBROUTINE SET_DBL_VAL_SETTER(VS, VAL)
  !  CLASS(VAL_SETTER),     INTENT(INOUT):: VS
  !  REAL(DBL),             INTENT(IN   ):: VAL
  !  CHARACTER(8):: CVAL
  !  !
  !  VS%VAL = TRANSFER(VAL,CVAL)
  !  !
  !END SUBROUTINE
  !!
  !PURE SUBROUTINE SET_INT_VAL_SETTER(VS, VAL)
  !  CLASS(VAL_SETTER),     INTENT(INOUT):: VS
  !  INTEGER,               INTENT(IN   ):: VAL
  !  CHARACTER(4):: CVAL
  !  !
  !  VS%VAL = TRANSFER(VAL,CVAL)
  !  !
  !END SUBROUTINE
  !!
  !FUNCTION SET_STR_HASH(H, KEY) RESULT(VS)
  !  CLASS(HASH_TABLE), INTENT(IN):: H
  !  CHARACTER(*),      INTENT(IN):: KEY
  !  TYPE(VAL_SETTER):: VS
  !  !
  !  INTEGER:: ID
  !  !
  !  ID = FIND_KEY_POS_HASH(H, KEY)
  !  !
  !  IF(ID > 0) THEN
  !            CALL SET_TARGET_VAL_SETTER(VS, H%KV(ID)%VAL)
  !  ELSE
  !            ALLOCATE(CHARACTER(16):: VS%VAL)
  !  END IF
  !  !
  !END FUNCTION
  !
  !PURE ELEMENTAL SUBROUTINE DESTROY_VAL_SETTER(VS)
  !  TYPE(VAL_SETTER), INTENT(INOUT):: VS
  !  !
  !  NULLIFY(VS%VAL)
  !  !
  !END SUBROUTINE