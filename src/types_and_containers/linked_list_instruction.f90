! CODE DEVELOPED BY SCOTT E BOYCE 
!                   CONTACT <seboyce@usgs.gov> or <Boyce@engineer.com>
!
! MODULE LINKED_LIST_INSTRUCTION   ==>  USE LINKED_LIST_INSTRUCTION, ONLY: INTEGER_LINKED_LIST, CHARACTER_LINKED_LIST
!
!  PROVIDES TWO DATA TYPES THAT ALLOW LINKED LIST OPTIONS (SIMILAR TO PYTHON LIST)
!  LIST CAN BE EITHER COMPOSED OF CHARACTER STRINGS OF DIFFERENT LENGTHS OR INTEGERS.
!  CAN ARBITRARILY ADD VALUES TO LIST, REMOVE THEM AND JUMP TO DIFFERENT POSTIONS.
!     OPTION TO CONVERT LIST TO ALLOCATABLE ARRAY AND OPTIONALLY SORT THE RESULTING ARRAY FROM SMALLEST TO LARGEST    
!
!    --UNKNOWN LIMITATION:
!                        CHANGING A CHARACTER LIST VALUE SEEMS TO CAUSE A COMPILER BUG IF PASSED CHARACTER IS OF LENGTH 1 AND A SUBSET OF A CHARACTER VARIABLE > 1
!
!  VERSION 1.0 [4/23/2017] ORIGINAL VERSION THAT SUPPORTS INTEGER_LINKED_LIST AND CHARACTER_LINKED_LIST DATA TYPES
!                               BEFORE USING LIST YOU MUST "CALL LIST%INIT()" TO EMPTY ANY EXISTING CONTENTS AND INITIALIZE ITS POINTERS.
!
!                               SEE TYPE, EXTENDS(ABSTRACT_LINKED_LIST):: INTEGER_LINKED_LIST and 
!                                   TYPE, EXTENDS(ABSTRACT_LINKED_LIST):: CHARACTER_LINKED_LIST
!                                       FOR SPECIFIC SUBROUTINE OPTIONS
!
!                               SUBROUTINES AVAILIBLE TO BOTH ARE LISTED IN TYPE, ABSTRACT:: ABSTRACT_LINKED_LIST
!
MODULE LINKED_LIST_INSTRUCTION!, ONLY: INTEGER_LINKED_LIST, CHARACTER_LINKED_LIST
  IMPLICIT NONE
  PRIVATE
  PUBLIC:: INTEGER_LINKED_LIST, CHARACTER_LINKED_LIST
  !
  TYPE LINK_ELEMENT
      CLASS(*), ALLOCATABLE:: I !VALUE
      INTEGER:: P=0             !POSITION
      TYPE(LINK_ELEMENT), POINTER:: NXT=>NULL()
      TYPE(LINK_ELEMENT), POINTER:: PRV=>NULL()
      CONTAINS
      FINAL:: DEALLOCATE_LINK_ELEMENT_FINAL
  END TYPE
  !
  TYPE, ABSTRACT:: ABSTRACT_LINKED_LIST
      PRIVATE
      INTEGER:: N=0
      TYPE(LINK_ELEMENT), POINTER:: BEG=>NULL()
      TYPE(LINK_ELEMENT), POINTER:: END=>NULL()
      TYPE(LINK_ELEMENT), POINTER:: CUR=>NULL()
      !
      CONTAINS
      !
      PROCEDURE, PASS(LST):: INIT         => INITIALIZE_LINKED_LIST    ! ()     INITIALIZES TO CREATE NEW LIST, MUST RUN BEFORE USING LIST. IT ALSO RESETS LIST TO INITIAL STATE IF HAS BE ALREADY USED.
      !
      GENERIC::              ADD          => EXTEND_LIST_FROM_ENDING, ADD_VALUE_AT, EXTEND_LIST_FROM_ENDING_VECTOR  ! (VAL,[POS]) PYTHON EQUIVALENT LIST.APPEND(VAL) and LIST.INSERT(VAL,POS)
      !
      PROCEDURE, PASS(LST):: CHANGE       => CHANGE_LIST_VALUE        ! (VAL,[POS],[ERROR]) SETS CURRENT VALUE/POINTER TO PASSED VALUE. IF INTEGER POS IS PROVIDED, THEN IT WILL SET THE VALUE LOCATED AT POSITION POS. IF LOGICAL ERROR IS PROVIDED THEN IT IS SET TO TRUE IF THERE IS AN ERROR WHEN SETTING THE VALUE.
      PROCEDURE, PASS(LST):: START        => MOVE_TO_FIRST_POS        ! ()     SETS CURRENT VALUE/POINTER TO START OF LIST
      PROCEDURE, PASS(LST):: LAST         => MOVE_TO_LAST_POS         ! ()     SETS CURRENT VALUE/POINTER TO END   OF LIST
      PROCEDURE, PASS(LST):: NEXT         => MOVE_TO_NEXT_POS         ! ()     MOVE TO NEXT     VALUE/POINTER WITHIN LIST
      PROCEDURE, PASS(LST):: BACK         => MOVE_TO_PREVIOUS_POS     ! ()     MOVE TO PREVIOUS VALUE/POINTER WITHIN LIST
      PROCEDURE, PASS(LST):: POS          => GOTO_POSITION            ! (P)    GO TO POSITION P WITHIN LIST
      PROCEDURE, PASS(LST):: LEN          => RETURN_LIST_LENGTH       ! ()     RETURNS INTEGER OF CURRENT LENGTH OF LIST
      PROCEDURE, PASS(LST):: GETPOS       => RETURN_LIST_POSITION     ! ()     RETURNS INTEGER OF CURRENT POSITION OF LIST
      PROCEDURE, PASS(LST):: IS_ASSOCIATED=> CUR_IS_ASSOCIATED        ! ()     RETURNS LOGICAL IF CURRENT POSITION IS ASSOCIATED WITH A TARGET. IF NOT ASSOCIATED WOULD INDICATE THAT THE POSITION IS EITHER BEYOND THE END OF THE LIST, BEFORE THE BEGINING OF THE LSIT, OR THE LIST HAS NO VALUES IN IT. -- USEFUL FOR WHILE LOOP WITH NEXT BECAUSE WHEN NEXT MOVES BEYOND THE END OF THE LIST, IT BECOMES NO LONGER ASSOCIATED
      PROCEDURE, PASS(LST):: AT_START     => POS_AT_START_OF_LIST     ! ()     RETURNS LOGICAL IF CURRENT POSITION IS AT START OF LIST. NOTE THAT IT WILL RETURN FALSE IF BEYOND THE BEGINING OF LIST
      PROCEDURE, PASS(LST):: AT_END       => POS_AT_END_OF_LIST       ! ()     RETURNS LOGICAL IF CURRENT POSITION IS AT END   OF LIST. NOTE THAT IT WILL RETURN FALSE IF BEYOND END OF LIST
      PROCEDURE, PASS(LST):: POP          => REMOVE_VAL_AT            !([POS]) REMOVES LAST VALUE IN LIST, "POS" IS OPTIONAL, IF POS IS PRESENT THEN THE VALUE LOCATED AT POSITION POS IS REMOVED. PYTHON LIST.POP([POS])
      PROCEDURE, PASS(LST):: DESTROY      => DELETE_ALL_LINKED_LIST_ENTRIES  !() DELETES ALL VALUES WITHIN THE LIST. THIS ROUTINE IS CALLED WHEN LIST IS DEALLOCATED OR WHEN INIT IS CALLED.
      PROCEDURE, PASS(LST):: EXTEND_LIST_FROM_ENDING                  ! (VAL)      ADDS "VAL" TO END OF LIST,                     PYTHON EQUIVALENT LIST.APPEND(VAL)
      PROCEDURE, PASS(LST):: EXTEND_LIST_FROM_ENDING_VECTOR           ! (VEC)      ADDS IN ORDER THE ITEMS IN VEC TO END OF LIST, PYTHON EQUIVALENT LIST+=VEC
      PROCEDURE, PASS(LST):: ADD_VALUE_AT                             ! (VAL,POS)  ADDS "VAL" TO POSITION POS OF LIST,            PYTHON EQUIVALENT LIST.INSERT(VAL,POS)
  END TYPE
  !
  TYPE, EXTENDS(ABSTRACT_LINKED_LIST):: INTEGER_LINKED_LIST
      CONTAINS
      PROCEDURE, PASS(LST):: INT             => INTEGER_RETURN_VALUE                 ! ([POS]) RETURNS CURRENT POSITIONS INTEGER VALUE, IF POS IS PROVIDED THEN RETURNS THE POS POSITION INTEGER
      GENERIC             :: ADD_UNIQUE      => INTEGER_ADD_UNIQUE_VALUE_TO_ENDING, &! (IVAL,[POS])  ADDS IVAL TO END OF LIST ONLY IF IT IS NOT CURRENTLY PRESENT WITHIN THE LIST. POS IS OPTIONAL AND SET TO ZERO IF NOT FOUND OTHERWISE IS THE NON-UNIQUE LOCATION
                                                INTEGER_ADD_UNIQUE_VALUE_TO_ENDING_VECTOR!(VEC)
      PROCEDURE, PASS(LST):: IS_UNIQUE       => INTEGER_VALUE_UNIQUE_IN_LIST         ! (IVAL)  RETURNS LOGICAL THAT CHECKS IF IVAL IS WITHIN THE LIST. RETURNS TRUE IF IT IS NO WITHIN LIST (VIZ ITS UNIQUE).
      PROCEDURE, PASS(LST):: NOT_UNIQUE      => INTEGER_VALUE_NOT_UNIQUE_IN_LIST     ! (IVAL)  OPPOSITE OF ABOVE
      PROCEDURE, PASS(LST):: SORT            => INTEGER_SORT_LIST                    ! ([REVERSE])
      PROCEDURE, PASS(LST):: MAX             => INTEGER_LIST_MAXVAL                  ! () RETURNS MAX THE VALUE WITHIN THE LIST
      PROCEDURE, PASS(LST):: MIN             => INTEGER_LIST_MINVAL                  ! () RETURNS MIN THE VALUE WITHIN THE LIST
      PROCEDURE, PASS(LST):: REVERSE         => INTEGER_REVERSE_LIST                 ! () REVERSE THE ORDER OF THE LIST
      PROCEDURE, PASS(LST):: DROP_DUPLICATES => INTEGER_LIST_DROP_DUPLICATES         ! () REMOVES ALL DUBLICATE VALUES WTIHIN THE LIST
      GENERIC::              TOARRAY         => INTEGER_LIST_TO_ARRAY_ALLOCATABLE, & ! (ARR,[SORT]) CONVERTS LIST TO ONE DIMENSIONAL ARRAY, ARR, WHICH MUST BE DECLARED AS "INTEGER, DIMENSION(:), ALLOCATABLE" OR "INTEGER, DIMENSION(:), POINTER". 
                                                 INTEGER_LIST_TO_ARRAY_POINTER       !              [SORT] IS OPTIONAL AND WHEN PROVIDED AND SET TO TRUE WILL CAUSE THE RESULTING ARRAY TO BE SORTED FROM SMALLEST TO LARGEST.
      PROCEDURE, PASS(LST):: STR             => INTEGER_LIST_RETURN_AS_STRING        !(DELIM, PAD)
      PROCEDURE, PASS(LST), PRIVATE:: INTEGER_LIST_TO_ARRAY_ALLOCATABLE
      PROCEDURE, PASS(LST), PRIVATE:: INTEGER_LIST_TO_ARRAY_POINTER
      PROCEDURE, PASS(LST), PRIVATE:: INTEGER_ADD_UNIQUE_VALUE_TO_ENDING
      PROCEDURE, PASS(LST), PRIVATE:: INTEGER_ADD_UNIQUE_VALUE_TO_ENDING_VECTOR
      FINAL:: INTEGER_LINKED_LIST_DEALLOCATE
  END TYPE
  !
  TYPE, EXTENDS(ABSTRACT_LINKED_LIST):: CHARACTER_LINKED_LIST  !NOTE THAT LIST ENTRIES MUST BE PASSED AS ENTIRE STRING. SUBSTRINGS SEEM TO CAUSE ALLOCATION ERRORS [ ie no CALL LIST%ADD( CHAR(3:6) ) ]
      !PUBLIC:: LN
      CHARACTER(:), POINTER:: ERR => NULL()
      CHARACTER(:), POINTER:: LN  => NULL()  !WHEN SET WITH SET_LN, HOLDS THE CURRENT POSITION STRING, LIST%LN. IT IS SET TO "ERROR" WHEN THE CURRENT POSITION IS NOT ASSOCIATED. IT IS A SHORT TO ACCESS STRING DIRECTLY.
      CONTAINS
      PROCEDURE, PASS(LST):: INIT         => CHAR_INITIALIZE_LINKED_LIST      ! ()      INITIALIZES TO CREATE NEW LIST, MUST RUN BEFORE USING LIST. IT ALSO RESETS LIST TO INITIAL STATE IF HAS BE ALREADY USED.
      PROCEDURE, PASS(LST):: FIRST_LINE   => CHAR_START_LINE                  ! ()      MOVES TO FIRST POSITION AND SETS LIST%LN TO IT
      PROCEDURE, PASS(LST):: NEXT_LINE    => CHAR_NEXT_LINE                   ! ()      MOVES TO NEXT LIST ENTRY AND SETS LIST%LN TO IT
      PROCEDURE, PASS(LST):: DEL_LINE     => CHAR_DEL_LINE                    ! ()      DELETES FROM THE LIST THE CURRENT LINE AND SETS LIST%LN TO THE NEXT LINE
      PROCEDURE, PASS(LST):: SET_LN       => CHAR_SET_LN                      ! ()      SETS LIST%LN TO CURRENT STRING. IF CURRENT POSITION IS NOT ALLOCATED LIST%LN ="ERROR" (VIZ. LIST%LN => LST%CUR%I)
      PROCEDURE, PASS(LST):: CHAR         => CHAR_RETURN_VALUE                ! ([POS]) RETURNS CURRENT STRING, IF CURRENT STRING IS NOT ALLOCATED THEN RETURNS "ERROR"
      PROCEDURE, PASS(LST):: FIND         => CHAR_FIND_VALUE                  ! (STR)   RETURN POS
      PROCEDURE, PASS(LST):: ADD_TRIM     => CHAR_ADD_TRIMMED_VALUE_TO_ENDING ! (STR,POS)  ADDS STR TO END OF LIST. LINE IS TRIMMED BEFORE ADDED.
      PROCEDURE, PASS(LST):: ADD_CORE     => CHAR_ADD_CORE_VALUE_TO_ENDING    ! (STR,POS)  ADDS STR TO END OF LIST. LINE IS ADJUSTL AND TRIMMED BEFORE ADDED.
      PROCEDURE, PASS(LST):: ADD_UNIQUE   => CHAR_ADD_UNIQUE_VALUE_TO_ENDING  ! (STR,POS)  ADDS STR TO END OF LIST ONLY IF IT IS NOT CURRENTLY PRESENT WITHIN THE LIST. POS IS OPTIONAL AND SET TO ZERO IF NOT FOUND OTHERWISE IS THE NON-UNIQUE LOCATION
      PROCEDURE, PASS(LST):: IS_UNIQUE    => CHAR_VALUE_UNIQUE_IN_LIST        ! (STR)  RETURNS LOGICAL THAT CHECKS IF STR IS WITHIN THE LIST. RETURNS TRUE IF IT IS NO WITHIN LIST (VIZ ITS UNIQUE).
      PROCEDURE, PASS(LST):: TOLINE       => RETURN_CHAR_LIST_TO_LINE         ! ([SEP], [EXT], [SEP2]) RETURN SINGLE LINE COMPOSED OF ALL LIST ENTIRIES SEPARATED BY SEP
      PROCEDURE, PASS(LST):: NBACK        => RETURN_CHAR_LIST_NBACK_LINES     ! (NBACK,[SEP])          RETURN SINGLE LINE COMPOSED OF NBACK LINES BEFORE CUR SEPARATED BY SEP
      GENERIC::              TOARRAY      => CHAR_LIST_TO_ARRAY_ALLOCATABLE, &! (ARR,[SORT],[DIM])  CONVERTS LIST TO ONE DIMENSIONAL CHARACTER ARRAY, ARR, WHICH MUST BE DECLARED AS "CHARACTER(:), DIMENSION(:), ALLOCATABLE" OR "CHARACTER(:), DIMENSION(:), POINTER"
                                             CHAR_LIST_TO_ARRAY_POINTER       !      [SORT] IS OPTIONAL AND WHEN PROVIDED AND SET TO TRUE WILL CAUSE THE RESULTING ARRAY TO BE SORTED ALPHABETICALLY.
                                                                              !      [DIM]  IS OPTINOAL AND WHEN PROVIDED WILL SET THE CHARACTER LENGTH, AS CHARACTER(DIM), OTHERWISE THE DIM IS SET TO THE LENGTH OF THE LONGEST STRING IN THE LIST.
      !
      GENERIC::              TOARRAY_ASSUM=> CHAR_LIST_TO_ARRAY_ASSUME_ALLOCATABLE, & ! (ARR,[SORT]) CONVERTS LIST TO ONE DIMENSIONAL CHARACTER ARRAY, ARR, WHICH MUST BE DECLARED AS "CHARACTER(DIM), DIMENSION(:), ALLOCATABLE" OR "CHARACTER(DIM), DIMENSION(:), POINTER". DIM IS SPECIFIED OUTSIDE OF SUBROUTINE AND IS THE FINAL CHARACTER LENGTH.
                                             CHAR_LIST_TO_ARRAY_ASSUME_POINTER        !      [SORT] IS OPTIONAL AND WHEN PROVIDED AND SET TO TRUE WILL CAUSE THE RESULTING ARRAY TO BE SORTED ALPHABETICALLY.
      PROCEDURE, PASS(LST):: MAKE_LINE       => CHAR_LIST_TO_LINE_ALLOCATABLE            ! (LINE,[SEP]) MERGES LIST INTO A SINGLE CHARACTER VARIABLE, LINE, WHICH MUST BE DEFINED AS "CHARACTER(:), ALLOCATABLE". SEP IS OPTINALL AND WHEN PRESENT IS PLACED BETWEEN EACH LIST ENTRY ON LINE. (eg SEP=', ' TO MAKE IT COMMA SEPARATED)
      PROCEDURE, PASS(LST):: MAKE_LINE_ASSUM => CHAR_LIST_TO_LINE_ASSUME                 ! (LINE,[SEP]) SEE ABOVE, BUT IS FOR NON-ALLOCATABLE CHARACTERS. LST WILL BE ADDED TO LINE UNTIL IT REACHES ITS END OR LIST ENDS...WHICH EVER HAPPENS FIRST
      PROCEDURE, PASS(LST):: DESTROY      => CHAR_LINKED_LIST_DEALLOCATE              ! ()   DELETES ALL VALUES WITHIN THE LIST. THIS ROUTINE IS CALLED WHEN LIST IS DEALLOCATED OR WHEN INIT IS CALLED.
      PROCEDURE, PASS(LST), PRIVATE:: CHAR_LIST_TO_ARRAY_ALLOCATABLE
      PROCEDURE, PASS(LST), PRIVATE:: CHAR_LIST_TO_ARRAY_POINTER
      PROCEDURE, PASS(LST), PRIVATE:: CHAR_LIST_TO_ARRAY_ASSUME_ALLOCATABLE
      PROCEDURE, PASS(LST), PRIVATE:: CHAR_LIST_TO_ARRAY_ASSUME_POINTER
      FINAL:: FINAL_CHARACTER_LINKED_LIST_DEALLOCATE
  END TYPE
  !
  CONTAINS
  !
  !#######################################################################
  !
  SUBROUTINE NULLIFY_LINK_ELEMENT(LNK)  !DOES NOT DEALLOCATE MEMORY, ONLY NULLIFIES!!!
     TYPE(LINK_ELEMENT), POINTER, INTENT(INOUT):: LNK
     !
     IF(ASSOCIATED(LNK)) THEN 
            NULLIFY(LNK%PRV)
            NULLIFY(LNK%NXT)
     END IF
     !
     NULLIFY(LNK)
  END SUBROUTINE
  !
  !#######################################################################
  !
  SUBROUTINE INITIALIZE_LINKED_LIST(LST)
    CLASS(ABSTRACT_LINKED_LIST):: LST
    !
    IF(LST%N==0) THEN
       LST%CUR   => NULL()   
       LST%BEG   => NULL()                                             !BEGINING REFERENCE IS NOW LOST
       LST%END   => NULL()                                             !ENDING   REFERENCE IS NOW LOST - NOW ONLY REFERENCE IS WITH LST%CUR
    ELSE
       CALL DELETE_ALL_LINKED_LIST_ENTRIES(LST)
    END IF
    !
  END SUBROUTINE
  !
  !#######################################################################
  !
  SUBROUTINE RENUMBER_LIST_POSITIONS(LST)
    CLASS(ABSTRACT_LINKED_LIST):: LST
    INTEGER:: I
    !        
    CALL MOVE_TO_FIRST_POS(LST)
    !
    IF(ASSOCIATED(LST%CUR)) THEN
        !
        I = 1
        LST%CUR%P = I
        !
        CALL MOVE_TO_NEXT_POS(LST)
        !
        DO WHILE(ASSOCIATED(LST%CUR))
              !
              I = I + 1
              LST%CUR%P = I
              !
              CALL MOVE_TO_NEXT_POS(LST)
        END DO
    END IF
    !
  END SUBROUTINE
  !
  !#######################################################################
  !
  SUBROUTINE CHANGE_LIST_VALUE(LST,I,POS,ERROR)
    CLASS(ABSTRACT_LINKED_LIST):: LST
    CLASS(*),          INTENT(IN ):: I
    INTEGER, OPTIONAL, INTENT(IN ):: POS
    LOGICAL, OPTIONAL, INTENT(OUT):: ERROR
    !
    IF(PRESENT(ERROR)) ERROR = .TRUE.
    !.
    IF(LST%N > 0) THEN
      !
      IF(PRESENT(POS)) CALL GOTO_POSITION(LST, POS)
      !
      IF(ASSOCIATED(LST%CUR)) THEN
                                  IF(ALLOCATED(LST%CUR%I)) DEALLOCATE(LST%CUR%I)
                                  ALLOCATE( LST%CUR%I, SOURCE = I )
                                  !
                                  IF(PRESENT(ERROR)) ERROR = .FALSE.
      END IF
      !
    END IF
    !
  END SUBROUTINE
  !
  !#######################################################################
  !
  SUBROUTINE EXTEND_LIST_FROM_ENDING(LST,I)
    CLASS(ABSTRACT_LINKED_LIST):: LST
    CLASS(*),        INTENT(IN):: I
    !
    TYPE(LINK_ELEMENT),POINTER::NEW_ELM                           !THIS WILL BE THE NEW ALLOCATED SPACE
    TYPE(LINK_ELEMENT),POINTER::CONNECTOR                         !THIS WILL HOLD IN MEMORY THE CURRENT LST%END
    !
    NULLIFY(NEW_ELM  )
    NULLIFY(CONNECTOR)
    ALLOCATE( LINK_ELEMENT::NEW_ELM )                             !ALLOCATE THE SPACE FOR THE NEW ELEMENT
    ALLOCATE( NEW_ELM%I, SOURCE = I )                             !SOMETIMES THIS FAILS WHEN YOU PASS IT A CHARACTER VARIABLE AS I(5:5) -- COMPILER BUG, FIX IS TO SET II = I(5:5) AND PASS II
    !
    LST%N = LST%N + 1
    IF(LST%N == 1) THEN                                           !THE LIST HAPPENS TO BE COMPLETELY EMPTY SO SET UP POINTERS AND RETURN
      NEW_ELM%P = 1                                               ! SET THE INITIAL POSITION
      LST%BEG => NEW_ELM
      LST%END => LST%BEG
      LST%CUR => LST%BEG
    ELSE
      !
      NEW_ELM%P = LST%END%P + 1                                   !GET THE NEW POSITION
      !
      CONNECTOR=>LST%END                                          !HOLD IN MEMORY THE CURRENT END VALUE AND POINTERS
      !
      CONNECTOR%NXT=>NEW_ELM                                      !THE CURRENT ENDING WHICH IS HELD IN MEMORY NOW POITNS TO THE NEXT VALUE
      !
      LST%END      =>NEW_ELM                                      !UPDATE THE ENDING TO THE NEW ENDING LOCATION
      !
      LST%END%PRV=>CONNECTOR                                      !NOW LINK NEW LAST VALUE PREVIOUS POINTER TO SECOND TO LAST
      !
      IF( .NOT. ASSOCIATED(LST%BEG%NXT) ) LST%BEG%NXT=>CONNECTOR  !IF NOT ASOCIATED THEN THIS IS THE SECOND ELEMENT TO BE ADDED SO LINK FIRST ELEMENT WITH LAST
      !
      LST%CUR => LST%END
      !
    END IF
    !
    NEW_ELM  => NULL()                                            ! CLEAN UP DANGLING POINTERS
    CONNECTOR=> NULL()                                            ! CLEAN UP DANGLING POINTERS
    !
  END SUBROUTINE
  !
  !
  SUBROUTINE EXTEND_LIST_FROM_ENDING_VECTOR(LST,VEC)
    CLASS(ABSTRACT_LINKED_LIST):: LST
    CLASS(*), DIMENSION(:), CONTIGUOUS, INTENT(IN):: VEC
    INTEGER:: I
    !
    DO I = 1, SIZE(VEC)
        CALL EXTEND_LIST_FROM_ENDING(LST,VEC(I))
    END DO
    !
  END SUBROUTINE
  !
  !#######################################################################
  !
  SUBROUTINE ADD_VALUE_AT(LST,I,POS)
    CLASS(ABSTRACT_LINKED_LIST):: LST
    CLASS(*),        INTENT(IN):: I
    INTEGER,         INTENT(IN):: POS
    INTEGER:: P
    !
    TYPE(LINK_ELEMENT),POINTER::NEW_ELM                           !THIS WILL BE THE NEW ALLOCATED SPACE
    TYPE(LINK_ELEMENT),POINTER::CONNECTOR                         !THIS WILL HOLD IN MEMORY THE CURRENT LST%END
    !
    IF(POS > LST%N .OR. LST%N == 0) THEN
        !
        CALL EXTEND_LIST_FROM_ENDING(LST,I)
        !
    ELSEIF(LST%N == 1) THEN                                       !ONLY OPTION IS TO UPDATE LST%BEG
        NULLIFY(NEW_ELM  )
        NULLIFY(CONNECTOR)
        ALLOCATE( LINK_ELEMENT::NEW_ELM )                             !ALLOCATE THE SPACE FOR THE NEW ELEMENT
        ALLOCATE( NEW_ELM%I, SOURCE = I )                             !SOMETIMES THIS FAILS WHEN YOU PASS IT A CHARACTER VARIABLE AS I(5:5) -- COMPILER BUG, FIX IS TO SET II = I(5:5) AND PASS II
        !
        CONNECTOR     => LST%BEG                                      !HOLD IN MEMORY THE CURRENT BEGINING VALUE AND POINTERS
        CONNECTOR%PRV => NEW_ELM                                      !CONNECT FIRST ENTRYT WIH NEW FIRST ENTRY
        NEW_ELM%NXT   => CONNECTOR                                    !MOVE FIRST ENTRY TO SECOND LOCATION
        NEW_ELM%PRV   => NULL()                                       !NEW START SO NULLIFY
        NEW_ELM%P     = 1
        CONNECTOR%P   = 2
        LST%BEG       => NEW_ELM
        LST%CUR       => NEW_ELM
    ELSE
        NULLIFY(NEW_ELM  )
        NULLIFY(CONNECTOR)
        ALLOCATE( LINK_ELEMENT::NEW_ELM )                             !ALLOCATE THE SPACE FOR THE NEW ELEMENT
        ALLOCATE( NEW_ELM%I, SOURCE = I )                             !SOMETIMES THIS FAILS WHEN YOU PASS IT A CHARACTER VARIABLE AS I(5:5) -- COMPILER BUG, FIX IS TO SET II = I(5:5) AND PASS II
        !
        CALL GOTO_POSITION(LST, POS)
        !
        NEW_ELM%P     =  LST%CUR%P
        !
        IF(LST%CUR%P == 1) THEN
                                      CONNECTOR     => LST%CUR          !HOLD IN MEMORY THE CURRENT BEGINING VALUE AND POINTERS
                                      CONNECTOR%PRV => NEW_ELM          !CONNECT FIRST ENTRYT WIH NEW FIRST ENTRY
                                      NEW_ELM%NXT   => CONNECTOR        !MOVE FIRST ENTRY TO SECOND LOCATION
                                      NEW_ELM%PRV   => NULL()           !NEW START SO NULLIFY
                                      LST%BEG       => NEW_ELM
        ELSE
                                      CONNECTOR     => LST%CUR%PRV      !CONNECT TO P-1 LOCATION
                                      CONNECTOR%NXT => NEW_ELM          !MAKE NEW NEXT LOCATION THE NEW_ELM
                                      NEW_ELM%PRV   => CONNECTOR        !NEW_ELM MUST POINT BACK TO P-1 LOCATION
                                      NEW_ELM%NXT   => LST%CUR          !CONNECT NEW_ELM WITH LST%CUR
                                      LST%CUR%PRV   => NEW_ELM          !      IN BOTH DIRECTIONS
        END IF
        !
        DO P=NEW_ELM%P, LST%N
                     LST%CUR%P = LST%CUR%P + 1
                     CALL LST%NEXT()
        END DO
        !
        LST%N = LST%N + 1
        LST%CUR => NEW_ELM
        !
    END IF
    !
    NEW_ELM  => NULL()                                            ! CLEAN UP DANGLING POINTERS
    CONNECTOR=> NULL()                                            ! CLEAN UP DANGLING POINTERS
    !
  END SUBROUTINE
  !
  !#######################################################################
  !
  SUBROUTINE MOVE_TO_NEXT_POS(LST)
    CLASS(ABSTRACT_LINKED_LIST) :: LST
    !
    IF(.NOT. ASSOCIATED(LST%CUR)) THEN             !EITHER LIST IS BEYOND ENDING, LIST IS EMPTY, OR BAD CUR POINTER
                                          CONTINUE
    ELSEIF(  ASSOCIATED(LST%CUR%NXT)) THEN
                                          LST%CUR=>LST%CUR%NXT
    ELSE
                                          LST%CUR=>NULL() !REACHED END OF LIST --LST%END
    END IF
    !
  END SUBROUTINE
  !
  !#######################################################################
  !
  SUBROUTINE MOVE_TO_PREVIOUS_POS(LST)
    CLASS(ABSTRACT_LINKED_LIST) :: LST
    !
    IF(.NOT. ASSOCIATED(LST%CUR)) THEN             !EITHER LIST IS BEYOND ENDING, LIST IS EMPTY, OR BAD CUR POINTER
                                          CONTINUE
    ELSEIF(ASSOCIATED(LST%CUR%PRV)) THEN
                                          LST%CUR=>LST%CUR%PRV
    ELSE
                                          LST%CUR=>NULL() !LST%BEG
    END IF
    !
  END SUBROUTINE
  !
  !#######################################################################
  !
  SUBROUTINE MOVE_TO_FIRST_POS(LST)
    CLASS(ABSTRACT_LINKED_LIST):: LST
    IF(ASSOCIATED(LST%BEG)) THEN
        LST%CUR=>LST%BEG
    ELSE
        LST%CUR=>NULL()
    END IF
  END SUBROUTINE
  !
  !#######################################################################
  !
  SUBROUTINE MOVE_TO_LAST_POS(LST)
    CLASS(ABSTRACT_LINKED_LIST):: LST
    IF(ASSOCIATED(LST%BEG)) THEN
        LST%CUR=>LST%END
    ELSE
        LST%CUR=>NULL()
    END IF
  END SUBROUTINE
  !
  !#######################################################################
  !
  FUNCTION POS_AT_END_OF_LIST(LST) RESULT(AT_END)
    CLASS(ABSTRACT_LINKED_LIST):: LST
    LOGICAL:: AT_END
    !
    IF(ASSOCIATED(LST%CUR)) THEN
                                AT_END=LST%CUR%P == LST%N
    ELSE
                                AT_END=.FALSE.
    END IF
    !
  END FUNCTION
  !
  !#######################################################################
  !
  FUNCTION POS_AT_START_OF_LIST(LST) RESULT(AT_START)
    CLASS(ABSTRACT_LINKED_LIST):: LST
    LOGICAL:: AT_START
    !
    IF(ASSOCIATED(LST%CUR)) THEN
                                AT_START=LST%CUR%P == 1
    ELSE
                                AT_START=.FALSE.
    END IF
    !
  END FUNCTION
  !
  !#######################################################################
  !
  FUNCTION CUR_IS_ASSOCIATED(LST) RESULT(IS_ASSOCIATED)
    CLASS(ABSTRACT_LINKED_LIST):: LST
    LOGICAL:: IS_ASSOCIATED
    !
    IF(ASSOCIATED(LST%CUR)) THEN
                                IS_ASSOCIATED=.TRUE.
    ELSE
                                IS_ASSOCIATED=.FALSE.
    END IF
    !
  END FUNCTION
  !
  !#######################################################################
  !
  FUNCTION RETURN_LIST_LENGTH(LST)  RESULT(LENGTH)
    CLASS(ABSTRACT_LINKED_LIST) :: LST
    INTEGER                     :: LENGTH
    !
    LENGTH=LST%N
    !
  END FUNCTION
  !
  !#######################################################################
  !
  FUNCTION RETURN_LIST_POSITION(LST)  RESULT(POS)
    CLASS(ABSTRACT_LINKED_LIST) :: LST
    INTEGER                     :: POS
    !
    IF(ASSOCIATED(LST%CUR)) THEN
                                POS=LST%CUR%P
    ELSE
                                POS= LST%N+1  !MOVE TO BAD POSITION
    END IF
    !
  END FUNCTION
  !
  !#######################################################################
  !
  SUBROUTINE GOTO_POSITION(LST, POS)
    CLASS(ABSTRACT_LINKED_LIST) :: LST
    INTEGER, INTENT(IN)         :: POS
    INTEGER:: I, P
    !
    IF (.NOT. ASSOCIATED(LST%BEG)) RETURN
    !
    IF (POS == 0) RETURN ! GOING NO WHERE
    P = POS
    !
    IF (P <  0) P = LST%N + P + 1  !-1 IS THE END OF THE LIST
    IF (P <  1 .OR. LST%N < P) THEN
                                  LST%CUR => NULL() !REFERENCING OUTSIDE OF LIST LENGTH
                                  RETURN
    END IF
    !
    IF( ASSOCIATED(LST%CUR) ) THEN;  IF (P == LST%CUR%P) RETURN  ! ALREADY THERE
    END IF
    !
    IF    (P == 1) THEN 
                                 LST%CUR => LST%BEG 
    ELSEIF(P == LST%N) THEN
                                 LST%CUR => LST%END
    ELSEIF ( LST%N - P < P ) THEN                     !START SEARCH FROM BACK
                                 LST%CUR => LST%END      
                                 DO I=1, LST%N
                                              IF (P == LST%CUR%P) EXIT
                                              CALL LST%BACK()
                                 END DO
    ELSE
                                 LST%CUR => LST%BEG      
                                 DO I=1, LST%N
                                              IF (P == LST%CUR%P) EXIT
                                              CALL LST%NEXT()
                                 END DO
    END IF
    !
  END SUBROUTINE
  !
  !#######################################################################
  !
  SUBROUTINE REMOVE_VAL_AT(LST,POS)
    CLASS(ABSTRACT_LINKED_LIST)  :: LST
    INTEGER, OPTIONAL, INTENT(IN):: POS
    !
    INTEGER::P
    !
    IF( LST%N < 2 ) THEN                         !LIST EITHER IS EMPTY OR CONTAINS ONE VALUE AND WILL BECOME EMPTY ONCE IT IS REMOVED
        CALL DELETE_ALL_LINKED_LIST_ENTRIES(LST)
        RETURN
    END IF
    !
    P = -1
    IF(PRESENT(POS)) P = POS
    !
    CALL GOTO_POSITION(LST, P)
    !
    P = RETURN_LIST_POSITION(LST)
    !
    CALL DISCONNECT_CUR_TO_MEMORY_LEAK(LST)
    !
    LST%N = LST%N - 1
    !
    !DEALLOCATE(LST%CUR%I)                                         !CLEAN UP THE DELETED MEMORY - DELETE STORED ARRAY TO CLEAR OUT MEMORY LEAK THAT RESULTED FROM "DISCONNECT_CUR_TO_MEMORY_LEAK"
    DEALLOCATE(LST%CUR)                                           !CLEAN UP THE DELETED MEMORY - DELETE DATA TYPE
    !
    IF(P > LST%N) P = LST%N
    CALL GOTO_POSITION(LST, P)
    !
  END SUBROUTINE
  !
  !#######################################################################
  !
  SUBROUTINE DISCONNECT_CUR_TO_MEMORY_LEAK(LST)
    CLASS(ABSTRACT_LINKED_LIST):: LST
    !
    TYPE(LINK_ELEMENT),POINTER:: CUR_PRV, CUR_NXT
    !
    IF(.NOT. ASSOCIATED(LST%CUR)) THEN;   RETURN
                                          !
    ELSEIF( ASSOCIATED(LST%BEG,LST%END) ) THEN 
                                          CALL NULLIFY_LINK_ELEMENT(LST%BEG)
                                          CALL NULLIFY_LINK_ELEMENT(LST%END)
                                          LST%CUR%NXT => NULL()              !THIS IS THE LAST VALUE IN THE LIST SO ALL POINTERS ARE NULL
                                          LST%CUR%PRV => NULL()
                                          RETURN
    ELSEIF( ASSOCIATED(LST%CUR,LST%BEG) ) THEN
                                          LST%BEG => LST%BEG%NXT                       !THIS CHECKS IF THE LOCATION FOUND IS ACTUALLY AT THE BEGINING OF THE LIST
                                          !
                                          IF( ASSOCIATED(LST%BEG) ) LST%BEG%PRV => NULL()
                                          !
                                          CUR_NXT => LST%BEG
                                          DO WHILE(ASSOCIATED(CUR_NXT))
                                              CUR_NXT%P = CUR_NXT%P - 1
                                              IF(  ASSOCIATED(CUR_NXT%NXT)) THEN
                                                                                CUR_NXT => CUR_NXT%NXT
                                              ELSE
                                                                                EXIT
                                              END IF
                                          END DO
                                          CUR_NXT => NULL()
                                          RETURN
    ELSEIF( ASSOCIATED(LST%CUR,LST%END) ) THEN
                                          LST%END => LST%END%PRV !THIS CHECKS IF THE LOCATION FOUND IS ACTUALLY AT THE END      OF THE LIST
                                          !
                                          IF( ASSOCIATED(LST%END) ) LST%END%NXT => NULL()
                                          RETURN
    END IF
    !
    IF( ASSOCIATED(LST%CUR%PRV) ) THEN 
        CUR_PRV => LST%CUR%PRV    !CONNECT TO ONE LIST ENTRY BEFORE THE CURRENT
    ELSE
        CUR_PRV => NULL()
    END IF
    IF( ASSOCIATED(LST%CUR%NXT) ) THEN 
        CUR_NXT => LST%CUR%NXT    !CONNECT TO ONE LIST ENTRY AFTER  THE CURRENT
    ELSE
        CUR_NXT=>NULL()
    END IF
    !
    IF    (      ASSOCIATED(CUR_PRV) .AND.       ASSOCIATED(CUR_NXT)) THEN!IN THEREY THIS SHOULD BE THE ONYL TRUE CASE
                                                                          !
                                                                          CUR_PRV%NXT => CUR_NXT !TO THE VALUE ONE BEYOND THE CURRENT
                                                                          CUR_NXT%PRV => CUR_PRV !TO THE VALUE ONE BEFORE THE CURRENT (SAME LOCATION AS LST%CUR%PRV)
    !
    ELSEIF(      ASSOCIATED(CUR_PRV) .AND. .NOT. ASSOCIATED(CUR_NXT)) THEN
                                                                          CUR_PRV%NXT => NULL()        !TO THE VALUE ONE BEYOND THE CURRENT
    !                                                                              
    ELSEIF(.NOT. ASSOCIATED(CUR_PRV) .AND.       ASSOCIATED(CUR_NXT)) THEN
                                                                          CUR_NXT%PRV => NULL()        !TO THE VALUE ONE BEYOND THE CURRENT
    ELSE
                                                                          CALL NULLIFY_LINK_ELEMENT(LST%BEG) !THIS IS THE LAST VALUE IN THE LIST SO ALL POINTERS ARE NULL
                                                                          CALL NULLIFY_LINK_ELEMENT(LST%END)
                                                                          LST%CUR%NXT => NULL()
                                                                          LST%CUR%PRV => NULL()
    END IF
    !
    DO WHILE(ASSOCIATED(CUR_NXT))
        CUR_NXT%P = CUR_NXT%P - 1
        IF(  ASSOCIATED(CUR_NXT%NXT)) THEN
                                          CUR_NXT => CUR_NXT%NXT
        ELSE
                                          EXIT
        END IF
    END DO
    !
    CUR_PRV =>NULL()
    CUR_NXT =>NULL()
    
    !CALL NULLIFY_LINK_ELEMENT(CUR_PRV)
    !CALL NULLIFY_LINK_ELEMENT(CUR_NXT)
    !
  END SUBROUTINE
  !
  !#######################################################################
  !
!!!  SUBROUTINE REMOVE_VAL_AT(LST,POS)
!!!    CLASS(ABSTRACT_LINKED_LIST)  :: LST
!!!    INTEGER, OPTIONAL, INTENT(IN):: POS
!!!    !
!!!    TYPE(LINK_ELEMENT),POINTER::CONNECTOR_PRV
!!!    TYPE(LINK_ELEMENT),POINTER::CONNECTOR_NXT
!!!    !
!!!    INTEGER::I, P
!!!    LOGICAL::USE_CURRENT
!!!    !
!!!    IF( LST%N < 2 ) THEN                         !LIST EITHER IS EMPTY OR CONTAINS ONE VALUE AND WILL BECOME EMPTY ONCE IT IS REMOVED
!!!        CALL DELETE_ALL_LINKED_LIST_ENTRIES(LST)
!!!        RETURN
!!!    END IF
!!!    !
!!!    P = -1
!!!    IF(PRESENT(POS)) P = POS
!!!    IF(P == LST%N  ) P = -1
!!!    !
!!!    SELECT CASE (P)
!!!    CASE (0)
!!!         P = LST%GETPOS() 
!!!    CASE (-1)
!!!         P = LST%N                                                !NO NEED TO UPDATE POSITIONS 
!!!    CASE (1)     
!!!         LST%CUR => LST%BEG
!!!         LST%BEG => LST%BEG%NXT                                   !REMOVING FIRST TERM SO UPDATE THE LOCATION OF LST%BEG
!!!    CASE (:-2, 2:)
!!!         CALL LST%POS(P)
!!!         P = LST%GETPOS()                                         !SWTICH TO POSITIVE POSITION
!!!         IF( ASSOCIATED(LST%CUR,LST%BEG) ) LST%BEG => LST%BEG%NXT !THIS CHECKS IF THE LOCATION FOUND IS ACTUALLY AT THE BEGINING OF THE LIST
!!!         IF( ASSOCIATED(LST%CUR,LST%END) ) LST%END => LST%END%PRV !THIS CHECKS IF THE LOCATION FOUND IS ACTUALLY AT THE END      OF THE LIST
!!!    END SELECT
!!!    !
!!!    IF(P == LST%N) THEN            !REMOVING LAST TERM SO UPDATE THE LOCATION OF LST%END
!!!         LST%CUR => LST%END
!!!         LST%END => LST%END%PRV                                   
!!!    END IF
!!!    !
!!!    LST%N = LST%N - 1
!!!    !
!!!    IF( ASSOCIATED(LST%CUR%PRV) ) THEN 
!!!        CONNECTOR_PRV => LST%CUR%PRV    !CONNECT TO ONE LIST ENTRY BEFORE THE CURRENT
!!!    ELSE
!!!       CONNECTOR_PRV => NULL()
!!!    END IF
!!!    IF( ASSOCIATED(LST%CUR%NXT) ) THEN 
!!!        CONNECTOR_NXT => LST%CUR%NXT    !CONNECT TO ONE LIST ENTRY AFTER  THE CURRENT
!!!    ELSE
!!!        CONNECTOR_NXT=>NULL()
!!!    END IF
!!!    !
!!!    IF    (      ASSOCIATED(CONNECTOR_PRV) .AND.       ASSOCIATED(CONNECTOR_NXT)) THEN
!!!                                                                                 CONNECTOR_PRV%NXT => CONNECTOR_NXT !TO THE VALUE ONE BEYOND THE CURRENT
!!!                                                                                 CONNECTOR_NXT%PRV => CONNECTOR_PRV !TO THE VALUE ONE BEFORE THE CURRENT (SAME LOCATION AS LST%CUR%PRV)
!!!    !
!!!    ELSEIF(      ASSOCIATED(CONNECTOR_PRV) .AND. .NOT. ASSOCIATED(CONNECTOR_NXT)) THEN
!!!                                                                                 CONNECTOR_PRV%NXT => NULL()        !TO THE VALUE ONE BEYOND THE CURRENT
!!!    !                                                                              
!!!    ELSEIF(.NOT. ASSOCIATED(CONNECTOR_PRV) .AND.       ASSOCIATED(CONNECTOR_NXT)) THEN
!!!                                                                                 CONNECTOR_NXT%PRV => NULL()        !TO THE VALUE ONE BEYOND THE CURRENT
!!!    ELSE
!!!                                                                                 LST%CUR%NXT => NULL()              !THIS IS THE LAST VALUE IN THE LIST SO ALL POINTERS ARE NULL
!!!                                                                                 LST%CUR%PRV => NULL()
!!!                                                                                 LST%BEG     => NULL()
!!!                                                                                 LST%END     => NULL()
!!!    END IF                           
!!!    !
!!!    DEALLOCATE(LST%CUR%I)                                         !CLEAN UP THE DELETED MEMORY - DELETE STORED ARRAY
!!!    DEALLOCATE(LST%CUR)                                           !CLEAN UP THE DELETED MEMORY - DELETE DATA TYPE
!!!    IF( ASSOCIATED(LST%BEG) ) THEN
!!!        IF( ASSOCIATED(CONNECTOR_NXT) ) THEN
!!!            LST%CUR=>CONNECTOR_NXT                                !NOW POINT CURRENT TO THE ENTRY THAT FOLLOWS THE DELETED POINT OR IF THERE IS NO ENTRY AFTER POINT TO THE END OF THE LIST
!!!        ELSE
!!!            LST%CUR=>LST%END
!!!        END IF
!!!    ELSE
!!!        LST%CUR=>NULL()
!!!    END IF
!!!    !
!!!    IF (P <= LST%N) THEN
!!!                        DO I=P, LST%N                  !UPDATE LIST POSITION
!!!                                     LST%CUR%P = I
!!!                                     CALL LST%NEXT()
!!!                        END DO
!!!                        !
!!!                        CALL LST%POS(P)
!!!    END IF
!!!    !
!!!  END SUBROUTINE
  !
  !#######################################################################
  !
  SUBROUTINE DEALLOCATE_LINK_ELEMENT_FINAL(LST)
    TYPE(LINK_ELEMENT):: LST
    !
    LST%P=0
    IF( ALLOCATED(LST%I) ) DEALLOCATE(LST%I)
    LST%NXT=>NULL()
    LST%PRV=>NULL()
  END SUBROUTINE
  !
  !#######################################################################
  !
  SUBROUTINE DELETE_ALL_LINKED_LIST_ENTRIES(LST)
    CLASS(ABSTRACT_LINKED_LIST):: LST
    !
    IF(LST%N==0) THEN      !NOTHING TO DEALLOCATE
        LST%CUR   => NULL()   
        LST%BEG   => NULL()
        LST%END   => NULL()
    ELSE
       !
       LST%CUR => LST%END                                            !POINT TO END OF LIST
       LST%BEG => NULL()                                             !BEGINING REFERENCE IS NOW LOST
       DO WHILE( LST%N > 0 )
           !
           LST%END    =>LST%CUR%PRV                                  !MOVE THE END POINTER UP ONE
           LST%CUR%PRV=>NULL()                                       !DELETE POSITIONAL POITNERS
           LST%CUR%NXT=>NULL()                                       !DELETE POSITIONAL POITNERS
           DEALLOCATE(LST%CUR)                                       !DELETE THE ENTIRE LOCATION
           LST%CUR=>LST%END                                          !POINT TO THE NEW ENDING
           LST%N = LST%N - 1
           !
       END DO
       !
       LST%END => NULL()
       LST%CUR => NULL()                                             !NULL THE FINAL POINTER OF THE LINKED LIST
    END IF
    !
  END SUBROUTINE
  !
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !
  FUNCTION INTEGER_RETURN_VALUE(LST, POS)  RESULT(IVAL)
    CLASS(INTEGER_LINKED_LIST), INTENT(INOUT):: LST
    INTEGER, OPTIONAL,          INTENT(IN   ):: POS
    INTEGER                                  :: IVAL
    !
    IF (PRESENT(POS)) CALL GOTO_POSITION(LST, POS) 
    !
    SELECT TYPE (INT => LST%CUR%I)
    TYPE IS (INTEGER);            IVAL = INT
    END SELECT
    !
  END FUNCTION
  !
  !#######################################################################
  !
  SUBROUTINE INTEGER_LIST_TO_ARRAY_ALLOCATABLE(LST,ARR,SORT)
    CLASS(INTEGER_LINKED_LIST),      INTENT(INOUT):: LST
    INTEGER,DIMENSION(:),ALLOCATABLE,INTENT(INOUT):: ARR
    LOGICAL, OPTIONAL:: SORT
    LOGICAL:: SRT
    INTEGER:: VAL,IDX, I, J
    !
    IF(LST%N > 0) THEN
                     IF(ALLOCATED(ARR)) THEN
                          IF(SIZE(ARR).NE.LST%N) THEN
                                             DEALLOCATE(ARR)
                                               ALLOCATE(ARR(LST%N))
                          END IF
                     ELSE
                         ALLOCATE(ARR(LST%N))
                     END IF
                     !
                     SRT=.FALSE.
                     IF(PRESENT(SORT)) SRT=SORT
                     !
                     CALL MOVE_TO_FIRST_POS(LST)
                     DO IDX=1, LST%N
                           SELECT TYPE(INT => LST%CUR%I)
                                           TYPE IS (INTEGER)
                                                            ARR(IDX) = INT
                           END SELECT
                           CALL LST%NEXT()
                     END DO
                     !
                     IF (SRT) THEN                                   ! Rearrange array by insertion sort ascending order
                         DO I=2, LST%N
                             VAL=ARR(I)
                             J=I
                             DO WHILE ( J > 1 )
                                                IF (ARR(J-1) < VAL) EXIT
                                                ARR(J) = ARR(J-1)
                                                J=J-1
                             END DO
                             ARR(J) = VAL
                         END DO
                     END IF
    ELSEIF(ALLOCATED(ARR)) THEN 
         DEALLOCATE(ARR)
    END IF
    !
  END SUBROUTINE
  !
  !#######################################################################
  !
  SUBROUTINE INTEGER_LIST_TO_ARRAY_POINTER(LST,ARR,SORT)
    CLASS(INTEGER_LINKED_LIST)      :: LST
    INTEGER,DIMENSION(:),POINTER:: ARR
    LOGICAL, OPTIONAL:: SORT
    LOGICAL:: SRT
    INTEGER:: VAL,IDX, I, J
    !
    DEALLOCATE(ARR, STAT=I) !AUTODEALLOCATE IN CASE MEMORY WAS ALREADY IN USE.
    ARR=>NULL()
    !
    IF(LST%N > 0) THEN
                     !
                     SRT=.FALSE.
                     IF(PRESENT(SORT)) SRT=SORT
                     !
                     !
                     ALLOCATE(ARR(LST%N))
                     !
                     CALL MOVE_TO_FIRST_POS(LST)
                     DO IDX=1, LST%N
                           SELECT TYPE(INT => LST%CUR%I)
                                           TYPE IS (INTEGER)
                                                            ARR(IDX) = INT
                           END SELECT
                           CALL LST%NEXT()
                     END DO
                     !
                     IF (SRT) THEN                                   ! Rearrange array by insertion sort ascending order
                         DO I=2, LST%N
                             VAL=ARR(I)
                             J=I
                             DO WHILE ( J > 1 )
                                                IF (ARR(J-1) < VAL) EXIT
                                                ARR(J) = ARR(J-1)
                                                J=J-1
                             END DO
                             ARR(J) = VAL
                         END DO
                     END IF
    END IF
    !
  END SUBROUTINE
  !
  !#######################################################################
  !
  SUBROUTINE INTEGER_LIST_DROP_DUPLICATES(LST)
    CLASS(INTEGER_LINKED_LIST)      :: LST
    INTEGER:: POS, COMPARE_POS, IVAL
    !
    IF(LST%N > 0) THEN
                     !
                     COMPARE_POS = 1
                     DO WHILE (COMPARE_POS < LST%N)
                        !
                        CALL LST%POS(COMPARE_POS)
                        !
                        SELECT TYPE(INT => LST%CUR%I)
                        TYPE IS (INTEGER);            IVAL = INT
                        END SELECT
                        !
                        CALL LST%NEXT()
                        POS = LST%GETPOS()
                        DO WHILE (COMPARE_POS < POS .AND. POS <= LST%N)
                              !
                              SELECT TYPE(INT => LST%CUR%I)
                              TYPE IS (INTEGER)
                                               IF(IVAL == INT) THEN
                                                   CALL LST%POP(0)
                                               ELSE
                                                   CALL LST%NEXT()
                                               END IF    
                              END SELECT
                              !
                              POS = LST%GETPOS()
                        END DO
                        !
                        COMPARE_POS = COMPARE_POS + 1
                     END DO
    END IF
    !
  END SUBROUTINE
  !
  !#######################################################################
  !
  SUBROUTINE INTEGER_SORT_LIST(LST, REVERSE)
    CLASS(INTEGER_LINKED_LIST):: LST
    LOGICAL, OPTIONAL, INTENT(IN):: REVERSE
    !
    TYPE(LINK_ELEMENT),POINTER::BEG, LNK, PRV
    !
    INTEGER:: I,P,IVAL
    !
    IF(LST%N > 2) THEN
                     NULLIFY(BEG)
                     NULLIFY(LNK)
                     NULLIFY(PRV)
                     !
                     CALL MOVE_TO_FIRST_POS(LST)
                     !
                     SELECT TYPE(INT => LST%CUR%I)
                     TYPE IS (INTEGER)
                                       IVAL = INT
                     END SELECT
                     !
                     P = 1
                     !
                     DO I=2, LST%N
                         !
                         CALL LST%NEXT()
                         !
                         SELECT TYPE(INT => LST%CUR%I)
                         TYPE IS (INTEGER)
                                           IF(INT < IVAL)  THEN
                                               IVAL = INT
                                               P = LST%GETPOS()
                                           END IF
                         END SELECT
                         !
                     END DO
                     !
                     CALL LST%POS(P)                         ! MOVE CUR TO P
                     CALL DISCONNECT_CUR_TO_MEMORY_LEAK(LST) ! CUR IS HELD IN MEMORY BY BEG DESPITE MEMORY LEAK
                     !
                     LST%CUR%PRV => NULL()
                     LST%CUR%NXT => NULL()
                     !
                     BEG => LST%CUR   !NEW BEGINING
                     LNK => LST%CUR   !CURRENT LINK IN LIST
                     !
                     LST%CUR  => NULL()
                     !
                     DO WHILE( ASSOCIATED(LST%BEG)) 
                           !
                           CALL MOVE_TO_FIRST_POS(LST)
                           !
                           SELECT TYPE(INT => LST%CUR%I)
                           TYPE IS (INTEGER)
                                             IVAL = INT
                           END SELECT
                           !
                           P = 1
                           CALL LST%NEXT()
                           !
                           DO WHILE(LST%IS_ASSOCIATED())
                               !
                               SELECT TYPE(INT => LST%CUR%I)
                               TYPE IS (INTEGER)
                                                 IF(INT < IVAL)  THEN
                                                     IVAL = INT
                                                     P = LST%GETPOS()
                                                 END IF
                               END SELECT
                               !
                               CALL LST%NEXT()
                           END DO
                           !
                           CALL LST%POS(P)
                           CALL DISCONNECT_CUR_TO_MEMORY_LEAK(LST) !CUR IS HELD IN MEMORY BY BEG DESPITE MEMORY LEAK
                           !
                           IF(ASSOCIATED(LST%CUR)) THEN
                               !
                               LNK%NXT => LST%CUR   !UPDATE NEXT CONNECTION
                               PRV     => LNK
                               LNK     => LNK%NXT  ! = LST%CUR -- MOVE FORWARD
                               LNK%NXT => NULL()
                               LNK%PRV => PRV
                               LST%CUR  => NULL()
                           END IF
                     END DO
                     !
                     ! REINITIALIZE THE LIST
                     LST%BEG => BEG
                     LST%END => LNK
                     LST%CUR => LST%BEG
                     !
                     BEG => NULL()
                     LNK => NULL()
                     PRV => NULL()
    END IF
    !
    IF(PRESENT(REVERSE)) THEN; IF(REVERSE) CALL INTEGER_REVERSE_LIST(LST)
    END IF
    !
  END SUBROUTINE
  !
  !#######################################################################
  !
  SUBROUTINE INTEGER_REVERSE_LIST(LST)
    CLASS(INTEGER_LINKED_LIST):: LST
    !
    TYPE(LINK_ELEMENT),POINTER::BEG, LNK, PRV
    !
    IF(LST%N > 1) THEN
                     NULLIFY(BEG)
                     NULLIFY(LNK)
                     NULLIFY(PRV)
                     !
                     CALL MOVE_TO_LAST_POS(LST)
                     !
                     CALL DISCONNECT_CUR_TO_MEMORY_LEAK(LST) ! CUR IS HELD IN MEMORY BY BEG DESPITE MEMORY LEAK
                     !
                     LST%CUR%PRV => NULL()
                     LST%CUR%NXT => NULL()
                     !
                     BEG => LST%CUR   !NEW BEGINING
                     LNK => LST%CUR   !CURRENT LINK IN LIST
                     !
                     LST%CUR  => NULL()
                     !
                     DO WHILE( ASSOCIATED(LST%BEG)) 
                           !
                           CALL MOVE_TO_LAST_POS(LST)
                           !
                           CALL DISCONNECT_CUR_TO_MEMORY_LEAK(LST) !CUR IS HELD IN MEMORY BY BEG DESPITE MEMORY LEAK
                           !
                           IF(ASSOCIATED(LST%CUR)) THEN
                               !
                               LNK%NXT => LST%CUR   !UPDATE NEXT CONNECTION
                               PRV     => LNK
                               LNK     => LNK%NXT  ! = LST%CUR -- MOVE FORWARD
                               LNK%NXT => NULL()
                               LNK%PRV => PRV
                               LST%CUR  => NULL()
                           END IF
                     END DO
                     !
                     ! REINITIALIZE THE LIST
                     LST%BEG => BEG
                     LST%END => LNK
                     LST%CUR => LST%BEG
                     !
                     BEG => NULL()
                     LNK => NULL()
                     PRV => NULL()
                     !
                     CALL RENUMBER_LIST_POSITIONS(LST)
    END IF
    !
  END SUBROUTINE
  !
  !#######################################################################
  !
  SUBROUTINE INTEGER_ADD_UNIQUE_VALUE_TO_ENDING(LST,IVAL,POS)
    CLASS(INTEGER_LINKED_LIST)    :: LST
    INTEGER, INTENT(IN )          :: IVAL
    INTEGER, INTENT(OUT), OPTIONAL:: POS
    INTEGER:: I
    LOGICAL:: NOT_FOUND
    !
    IF(LST%N > 0) THEN
                     IF(PRESENT(POS)) POS = 0
                     !
                     NOT_FOUND=.TRUE.
                     CALL MOVE_TO_FIRST_POS(LST)
                     DO I=1, LST%N
                                  SELECT TYPE(INT => LST%CUR%I)
                                  TYPE IS (INTEGER)
                                                   IF(IVAL == INT) THEN
                                                                        NOT_FOUND = .FALSE.
                                                                        IF(PRESENT(POS)) POS = I
                                                                        EXIT
                                                   END IF    
                                  END SELECT
                                  CALL MOVE_TO_NEXT_POS(LST)
                     END DO
                     IF(NOT_FOUND) CALL EXTEND_LIST_FROM_ENDING(LST,IVAL)
    ELSE
                                   CALL EXTEND_LIST_FROM_ENDING(LST,IVAL)
                                   IF(PRESENT(POS)) POS = 1
    END IF
    !
  END SUBROUTINE
  !
  !#######################################################################
  !
  SUBROUTINE INTEGER_ADD_UNIQUE_VALUE_TO_ENDING_VECTOR(LST,VEC)
    CLASS(INTEGER_LINKED_LIST)    :: LST
    INTEGER, DIMENSION(:), INTENT(IN)          :: VEC
    INTEGER:: I
    !
    DO I = 1, SIZE(VEC)
        CALL INTEGER_ADD_UNIQUE_VALUE_TO_ENDING(LST,VEC(I))
    END DO
    !
  END SUBROUTINE
  !
  !#######################################################################
  !
  FUNCTION INTEGER_VALUE_UNIQUE_IN_LIST(LST,IVAL) RESULT(IS_UNIQUE)
    CLASS(INTEGER_LINKED_LIST)      :: LST
    INTEGER, INTENT(IN)             :: IVAL
    INTEGER:: I
    LOGICAL:: IS_UNIQUE
    !
    IS_UNIQUE = .TRUE.
    IF(LST%N > 0) THEN
                     !
                     CALL MOVE_TO_FIRST_POS(LST)
                     DO I=1, LST%N
                                  SELECT TYPE(INT => LST%CUR%I)
                                  TYPE IS (INTEGER)
                                                   IF(IVAL == INT) THEN
                                                                        IS_UNIQUE = .FALSE.
                                                                        EXIT
                                                   END IF    
                                  END SELECT
                                  CALL MOVE_TO_NEXT_POS(LST)
                     END DO
    END IF
    !
  END FUNCTION
  !
  FUNCTION INTEGER_VALUE_NOT_UNIQUE_IN_LIST(LST,IVAL) RESULT(NOT_UNIQUE)
    CLASS(INTEGER_LINKED_LIST)      :: LST
    INTEGER, INTENT(IN)             :: IVAL
    LOGICAL:: NOT_UNIQUE
    !
    NOT_UNIQUE = .NOT. INTEGER_VALUE_UNIQUE_IN_LIST(LST,IVAL)
    !
  END FUNCTION
  !
  !#######################################################################
  !
  FUNCTION INTEGER_LIST_MAXVAL(LST) RESULT(IVAL)
    CLASS(INTEGER_LINKED_LIST)      :: LST
    INTEGER:: IVAL
    INTEGER:: I
    !
    IF(LST%N > 0) THEN
                     !
                     CALL MOVE_TO_FIRST_POS(LST)
                     !
                     SELECT TYPE(INT => LST%CUR%I)
                     TYPE IS (INTEGER);            IVAL = INT   
                     END SELECT
                     !
                     DO I=2, LST%N
                                  CALL MOVE_TO_NEXT_POS(LST)
                                  SELECT TYPE(INT => LST%CUR%I)
                                  TYPE IS (INTEGER)
                                                   IF(IVAL < INT) IVAL = INT
                                  END SELECT
                     END DO
    END IF
    !
  END FUNCTION
  !
  !#######################################################################
  !
  FUNCTION INTEGER_LIST_MINVAL(LST) RESULT(IVAL)
    CLASS(INTEGER_LINKED_LIST)      :: LST
    INTEGER:: IVAL
    INTEGER:: I
    !
    IF(LST%N > 0) THEN
                     !
                     CALL MOVE_TO_FIRST_POS(LST)
                     !
                     SELECT TYPE(INT => LST%CUR%I)
                     TYPE IS (INTEGER);            IVAL = INT   
                     END SELECT
                     !
                     DO I=2, LST%N
                                  CALL MOVE_TO_NEXT_POS(LST)
                                  SELECT TYPE(INT => LST%CUR%I)
                                  TYPE IS (INTEGER)
                                                   IF(IVAL > INT) IVAL = INT
                                  END SELECT
                     END DO
    END IF
    !
  END FUNCTION
  !
  !#######################################################################
  !
  FUNCTION INTEGER_LIST_RETURN_AS_STRING(LST, DELIM, PAD) RESULT(STR)
    CLASS(INTEGER_LINKED_LIST)      :: LST
    CHARACTER(*),OPTIONAL,INTENT(IN):: DELIM
    INTEGER,     OPTIONAL,INTENT(IN):: PAD
    CHARACTER(:),ALLOCATABLE:: STR
    CHARACTER(:),ALLOCATABLE:: TXT
    CHARACTER(30)::NUM
    INTEGER:: I, J
    !
    IF    (LST%N == 0) THEN
                     STR = ' '
    ELSEIF(LST%N == 1) THEN
                     CALL MOVE_TO_FIRST_POS(LST)
                     SELECT TYPE(INT => LST%CUR%I)
                     TYPE IS (INTEGER)
                                      WRITE(NUM,'(I30)') INT 
                     END SELECT
                     !
                     STR = TRIM(ADJUSTL(NUM))
    ELSE
                     J = 0
                     !
                     IF(PRESENT(DELIM)) THEN
                         TXT=DELIM
                     ELSE
                         TXT = ' '
                     END IF
                     !
                     IF(PRESENT(PAD)) THEN
                           CALL MOVE_TO_FIRST_POS(LST)
                           DO I=1, LST%N
                                        SELECT TYPE(INT => LST%CUR%I)
                                        TYPE IS (INTEGER)
                                                         WRITE(NUM,'(I30)') INT 
                                                         NUM=ADJUSTL(NUM)
                                        END SELECT
                                        IF(LEN_TRIM(NUM) > J) J = LEN_TRIM(NUM)
                                        CALL MOVE_TO_NEXT_POS(LST)
                           END DO
                           IF(J>30) J=30
                     END IF
                     !
                     CALL MOVE_TO_FIRST_POS(LST)
                     SELECT TYPE(INT => LST%CUR%I)
                     TYPE IS (INTEGER)
                                      WRITE(NUM,'(I30)') INT 
                                      NUM=ADJUSTL(NUM)
                     END SELECT
                     !
                     IF(J > 0) THEN
                         STR = REPEAT(' ',J-LEN_TRIM(NUM))//TRIM(NUM)//TXT
                     ELSE
                         STR = TRIM(NUM)//TXT
                     END IF
                     CALL MOVE_TO_NEXT_POS(LST)
                     !
                     DO I=2, LST%N-1
                           SELECT TYPE(INT => LST%CUR%I)
                           TYPE IS (INTEGER)
                                            WRITE(NUM,'(I30)') INT 
                                            NUM=ADJUSTL(NUM)
                           END SELECT
                           !
                           IF(J > 0) THEN
                               STR = STR//REPEAT(' ',J-LEN_TRIM(NUM))//TRIM(NUM)//TXT
                           ELSE
                               STR = STR//TRIM(NUM)//TXT
                           END IF
                           CALL MOVE_TO_NEXT_POS(LST)
                     END DO
                     !
                     SELECT TYPE(INT => LST%CUR%I)
                     TYPE IS (INTEGER)
                                      WRITE(NUM,'(I30)') INT 
                                      NUM=ADJUSTL(NUM)
                     END SELECT
                     !
                     IF(J > 0) THEN
                         STR = STR//REPEAT(' ',J-LEN_TRIM(NUM))//TRIM(NUM)
                     ELSE
                         STR = STR//TRIM(NUM)
                     END IF
    END IF
    !
  END FUNCTION
  !
  !
  !##################################################################################################
  !
  SUBROUTINE INTEGER_LINKED_LIST_DEALLOCATE(LST)
     TYPE(INTEGER_LINKED_LIST):: LST
     !
     CALL DELETE_ALL_LINKED_LIST_ENTRIES(LST)
     !
  END SUBROUTINE
  !
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !
  SUBROUTINE CHAR_START_LINE(LST)    !FUNCTION SEEMS TO FAIL IF LIST ENTRY WAS MADE BY A SUBSTRING (ie CHAR(4:8))
    CLASS(CHARACTER_LINKED_LIST):: LST
    CALL MOVE_TO_FIRST_POS(LST)
    CALL CHAR_SET_LN(LST)
    !
  END SUBROUTINE
  !
  SUBROUTINE CHAR_NEXT_LINE(LST)    !FUNCTION SEEMS TO FAIL IF LIST ENTRY WAS MADE BY A SUBSTRING (ie CHAR(4:8))
    CLASS(CHARACTER_LINKED_LIST):: LST
    CALL MOVE_TO_NEXT_POS(LST)
    CALL CHAR_SET_LN(LST)
    !
  END SUBROUTINE
  !
  SUBROUTINE CHAR_SET_LN(LST)    !FUNCTION SEEMS TO FAIL IF LIST ENTRY WAS MADE BY A SUBSTRING (ie CHAR(4:8))
    CLASS(CHARACTER_LINKED_LIST) :: LST
    CLASS(*), POINTER:: CHAR
    !
    IF(ASSOCIATED(LST%CUR)) THEN
       CHAR => LST%CUR%I
       SELECT TYPE (CHAR)
                             TYPE IS (CHARACTER(*))
                                              LST%LN => CHAR
                             CLASS DEFAULT
                                              LST%LN => LST%ERR
                             END SELECT
    ELSE
                                              LST%LN => LST%ERR
    END IF
    !
  END SUBROUTINE
  !
  FUNCTION CHAR_RETURN_VALUE(LST,POS)  RESULT(VAL)  !FUNCTION SEEMS TO FAIL IF LIST ENTRY WAS MADE BY A SUBSTRING (ie CHAR(4:8))
    CLASS(CHARACTER_LINKED_LIST), INTENT(INOUT):: LST
    INTEGER,      OPTIONAL,       INTENT(IN   ):: POS
    CHARACTER(:), ALLOCATABLE                  :: VAL
    !
    IF (PRESENT(POS)) CALL GOTO_POSITION(LST, POS) 
    !
    IF(ALLOCATED(VAL)) DEALLOCATE(VAL)
    !
    IF(ALLOCATED(LST%CUR%I)) THEN
       SELECT TYPE (CHAR => LST%CUR%I)
                             TYPE IS (CHARACTER(*))
                                              ALLOCATE(VAL, SOURCE = CHAR)
       END SELECT
    ELSE
       ALLOCATE(VAL, SOURCE = 'ERROR')
    END IF
    
    !
  END FUNCTION
  !
  SUBROUTINE CHAR_DEL_LINE(LST,POS)    !FUNCTION SEEMS TO FAIL IF LIST ENTRY WAS MADE BY A SUBSTRING (ie CHAR(4:8))
    CLASS(CHARACTER_LINKED_LIST):: LST
    INTEGER, OPTIONAL,INTENT(IN):: POS
    INTEGER:: P
    !
    P = 0                   !Delete Current line if not specified
    IF(PRESENT(POS)) P = POS
    !
    CALL REMOVE_VAL_AT(LST,P)
    CALL CHAR_SET_LN(LST)
    !
  END SUBROUTINE
  !
  !#######################################################################
  !
  !
  FUNCTION CHAR_FIND_VALUE(LST,STR)  RESULT(POS)  !FUNCTION SEEMS TO FAIL IF LIST ENTRY WAS MADE BY A SUBSTRING (ie CHAR(4:8))
    CLASS(CHARACTER_LINKED_LIST), INTENT(INOUT):: LST
    CHARACTER(*),                 INTENT(IN   ):: STR
    INTEGER:: POS
    INTEGER:: IDX
    !
    POS = 0
    IF(LST%N>0) THEN
                   CALL MOVE_TO_FIRST_POS(LST)
                   DO IDX=1, LST%N
                         SELECT TYPE(CHAR => LST%CUR%I)
                                         TYPE IS (CHARACTER(*))
                                                          IF(CHAR == STR) THEN
                                                              POS = IDX
                                                              EXIT
                                                          END IF
                         END SELECT
                         CALL LST%NEXT()
                   END DO
    END IF
    !
  END FUNCTION
  !
  !#######################################################################
  !
  SUBROUTINE CHAR_LIST_TO_ARRAY_ALLOCATABLE(LST,ARR,SORT,DIM)
    CLASS(CHARACTER_LINKED_LIST)      :: LST
    CHARACTER(:),DIMENSION(:),ALLOCATABLE:: ARR
    LOGICAL, OPTIONAL:: SORT
    INTEGER, OPTIONAL:: DIM
    LOGICAL:: SRT
    INTEGER:: IDX, I, J, SIZ
    CHARACTER(:), ALLOCATABLE:: VAL
    !
    IF(ALLOCATED(ARR)) DEALLOCATE(ARR)
    !
    IF(LST%N > 0) THEN
                     !
                     SRT=.FALSE.
                     IF(PRESENT(SORT)) SRT=SORT
                     !
                     IF (PRESENT(DIM)) THEN
                          SIZ=DIM
                     ELSE
                          SIZ=0
                          CALL MOVE_TO_FIRST_POS(LST)
                          DO IDX=1, LST%N
                                SELECT TYPE(CHAR => LST%CUR%I)
                                                TYPE IS (CHARACTER(*))
                                                                       IF ( LEN(CHAR) > SIZ) SIZ = LEN(CHAR)
                                END SELECT
                                CALL LST%NEXT()
                          END DO
                     END IF
                     !
                     ALLOCATE(CHARACTER(SIZ)::ARR(LST%N))
                     !
                     CALL MOVE_TO_FIRST_POS(LST)
                     DO IDX=1, LST%N
                           SELECT TYPE(CHAR => LST%CUR%I)
                                           TYPE IS (CHARACTER(*))
                                                            ARR(IDX) = CHAR
                           END SELECT
                           CALL LST%NEXT()
                     END DO
                     !
                     IF (SRT) THEN                                   ! Rearrange array by insertion sort ascending order
                         ALLOCATE(CHARACTER(SIZ)::VAL)
                         DO I=2, LST%N
                             VAL(:)=ARR(I)
                             J=I
                             DO WHILE ( J > 1 )
                                                IF (ARR(J-1) < VAL) EXIT
                                                ARR(J) = ARR(J-1)
                                                J=J-1
                             END DO
                             ARR(J) = VAL
                         END DO
                         DEALLOCATE(VAL)
                     END IF
    END IF
    !
  END SUBROUTINE
  !
  !#######################################################################
  !
  SUBROUTINE CHAR_LIST_TO_ARRAY_POINTER(LST,ARR,SORT,DIM)
    CLASS(CHARACTER_LINKED_LIST)      :: LST
    CHARACTER(:),DIMENSION(:),POINTER:: ARR
    LOGICAL, OPTIONAL:: SORT
    INTEGER, OPTIONAL:: DIM
    LOGICAL:: SRT
    INTEGER:: IDX, I, J, SIZ
    CHARACTER(:), ALLOCATABLE:: VAL
    !
    DEALLOCATE(ARR, STAT=I)
    ARR => NULL()
    !
    IF(LST%N > 0) THEN
                     !
                     SRT=.FALSE.
                     IF(PRESENT(SORT)) SRT=SORT
                     !
                     IF (PRESENT(DIM)) THEN
                          SIZ=DIM
                     ELSE
                          SIZ=0
                          CALL MOVE_TO_FIRST_POS(LST)
                          DO IDX=1, LST%N
                                SELECT TYPE(CHAR => LST%CUR%I)
                                                TYPE IS (CHARACTER(*))
                                                                       IF ( LEN(CHAR) > SIZ) SIZ = LEN(CHAR)
                                END SELECT
                                CALL LST%NEXT()
                          END DO
                     END IF
                     !
                     ALLOCATE(CHARACTER(SIZ)::ARR(LST%N))
                     !
                     CALL MOVE_TO_FIRST_POS(LST)
                     DO IDX=1, LST%N
                           SELECT TYPE(CHAR => LST%CUR%I)
                                           TYPE IS (CHARACTER(*))
                                                            ARR(IDX) = CHAR
                           END SELECT
                           CALL LST%NEXT()
                     END DO
                     !
                     IF (SRT) THEN                                   ! Rearrange array by insertion sort ascending order
                         ALLOCATE(CHARACTER(SIZ)::VAL)
                         DO I=2, LST%N
                             VAL(:)=ARR(I)
                             J=I
                             DO WHILE ( J > 1 )
                                                IF (ARR(J-1) < VAL) EXIT
                                                ARR(J) = ARR(J-1)
                                                J=J-1
                             END DO
                             ARR(J) = VAL
                         END DO
                         DEALLOCATE(VAL)
                     END IF
    END IF
    !
  END SUBROUTINE
  !
  !#######################################################################
  !
  SUBROUTINE CHAR_LIST_TO_ARRAY_ASSUME_ALLOCATABLE(LST,ARR,SORT)
    CLASS(CHARACTER_LINKED_LIST)      :: LST
    CHARACTER(*),DIMENSION(:),ALLOCATABLE:: ARR
    LOGICAL, OPTIONAL:: SORT
    LOGICAL:: SRT
    INTEGER:: IDX, I, J
    CHARACTER(:), ALLOCATABLE:: VAL
    !
    SRT=.FALSE.
    IF(PRESENT(SORT)) SRT=SORT
    !
    IF(ALLOCATED(ARR)) DEALLOCATE(ARR)
    ALLOCATE(ARR(LST%N))
    !
    CALL MOVE_TO_FIRST_POS(LST)
    DO IDX=1, LST%N
          SELECT TYPE(CHAR => LST%CUR%I)
                          TYPE IS (CHARACTER(*))
                                           ARR(IDX) = CHAR
          END SELECT
          CALL LST%NEXT()
    END DO
    !
    IF (SRT) THEN                                   ! Rearrange array by insertion sort ascending order
        ALLOCATE(CHARACTER(LEN(ARR))::VAL)
        DO I=2, LST%N
            VAL(:)=ARR(I)
            J=I
            DO WHILE ( J > 1 )
                               IF (ARR(J-1) < VAL) EXIT
                               ARR(J) = ARR(J-1)
                               J=J-1
            END DO
            ARR(J) = VAL
        END DO
        DEALLOCATE(VAL)
    END IF
    !
  END SUBROUTINE
  !
  !#######################################################################
  !
  SUBROUTINE CHAR_LIST_TO_ARRAY_ASSUME_POINTER(LST,ARR,SORT)
    CLASS(CHARACTER_LINKED_LIST)      :: LST
    CHARACTER(*),DIMENSION(:),POINTER:: ARR
    LOGICAL, OPTIONAL:: SORT
    LOGICAL:: SRT
    INTEGER:: IDX, I, J
    CHARACTER(:), ALLOCATABLE:: VAL
    !
    SRT=.FALSE.
    IF(PRESENT(SORT)) SRT=SORT
    !
    DEALLOCATE(ARR, STAT=I)
    ALLOCATE(ARR(LST%N))
    !
    CALL MOVE_TO_FIRST_POS(LST)
    DO IDX=1, LST%N
          SELECT TYPE(CHAR => LST%CUR%I)
                          TYPE IS (CHARACTER(*))
                                           ARR(IDX) = CHAR
          END SELECT
          CALL LST%NEXT()
    END DO
    !
    IF (SRT) THEN                                   ! Rearrange array by insertion sort ascending order
        ALLOCATE(CHARACTER(LEN(ARR))::VAL)
        DO I=2, LST%N
            VAL(:)=ARR(I)
            J=I
            DO WHILE ( J > 1 )
                               IF (ARR(J-1) < VAL) EXIT
                               ARR(J) = ARR(J-1)
                               J=J-1
            END DO
            ARR(J) = VAL
        END DO
        DEALLOCATE(VAL)
    END IF
    !
  END SUBROUTINE
  !
  !#######################################################################
  !
  SUBROUTINE CHAR_LIST_TO_LINE_ALLOCATABLE(LST,LINE,SEP)
    CLASS(CHARACTER_LINKED_LIST), INTENT(INOUT):: LST
    CHARACTER(:),ALLOCATABLE,     INTENT(  OUT):: LINE
    CHARACTER(*), OPTIONAL,       INTENT(IN   ):: SEP
    INTEGER:: IDX, I, J, SIZ, SEP_LEN
    !
    IF(ALLOCATED(LINE)) DEALLOCATE(LINE)
    !
    IF(LST%N > 0) THEN
                     !
                     IF(PRESENT(SEP)) THEN
                         SEP_LEN = LEN(SEP)
                     ELSE
                         SEP_LEN = 0
                     END IF
                     !
                     SIZ=0
                     CALL MOVE_TO_FIRST_POS(LST)
                     DO IDX=1, LST%N
                           SELECT TYPE(CHAR => LST%CUR%I)
                                           TYPE IS (CHARACTER(*))
                                                                  SIZ = SIZ + LEN(CHAR)
                                                                  
                           END SELECT
                           CALL LST%NEXT()
                     END DO
                     !
                     IF(SEP_LEN > 0) THEN
                           DO CONCURRENT (IDX=1:LST%N-1);  SIZ = SIZ + SEP_LEN
                           END DO
                     END IF
                     !
                     ALLOCATE(CHARACTER(SIZ)::LINE)
                     !
                     CALL MOVE_TO_FIRST_POS(LST)
                     I=1
                     DO IDX=1, LST%N
                           SELECT TYPE(CHAR => LST%CUR%I)
                                           TYPE IS (CHARACTER(*))
                                                            SIZ = LEN(CHAR)
                                                            J = I - 1 + SIZ
                                                            LINE(I:J) = CHAR
                                                            I = I + SIZ
                           END SELECT
                           CALL LST%NEXT()
                           !
                           IF(SEP_LEN > 0) THEN
                                     IF(IDX .NE. LST%N) THEN
                                                            J = I - 1 + SEP_LEN
                                                            LINE(I:J) = SEP
                                                            I = I + SEP_LEN
                                     END IF
                                     
                           END IF
                     END DO
                     !
    ELSE
                     ALLOCATE(LINE, SOURCE=' ')
    END IF
    !
  END SUBROUTINE
  !
  !#######################################################################
  !
  SUBROUTINE CHAR_LIST_TO_LINE_ASSUME(LST,LINE,SEP)
    CLASS(CHARACTER_LINKED_LIST), INTENT(INOUT):: LST
    CHARACTER(*),                 INTENT(  OUT):: LINE
    CHARACTER(*), OPTIONAL,       INTENT(IN   ):: SEP
    INTEGER:: IDX, I, J, SIZ, LEN_LINE, SEP_LEN
    !
    LEN_LINE = LEN(LINE)
    IF(LST%N > 0) THEN
                     !
                     IF(PRESENT(SEP)) THEN
                         SEP_LEN = LEN(SEP)
                     ELSE
                         SEP_LEN = 0
                     END IF
                     !
                     CALL MOVE_TO_FIRST_POS(LST)
                     I=1
                     DO IDX=1, LST%N
                           SELECT TYPE(CHAR => LST%CUR%I)
                                           TYPE IS (CHARACTER(*))
                                                            SIZ = LEN(CHAR)
                                                            J = I - 1 + SIZ
                                                            !
                                                            IF( I > LEN_LINE ) EXIT
                                                            IF( J > LEN_LINE ) J = LEN_LINE
                                                            !
                                                            LINE(I:J) = CHAR
                                                            I = I + SIZ
                           END SELECT
                           CALL LST%NEXT()
                           !
                           IF(SEP_LEN > 0) THEN
                                     IF(IDX .NE. LST%N) THEN
                                                            J = I - 1 + SEP_LEN
                                                            !
                                                            IF( I > LEN_LINE ) EXIT
                                                            IF( J > LEN_LINE ) J = LEN_LINE
                                                            !
                                                            LINE(I:J) = SEP
                                                            I = I + SEP_LEN
                                     END IF
                           END IF
                     END DO
                     !
    ELSE
                     LINE = ''
    END IF
    !
  END SUBROUTINE
  !
  !#######################################################################
  !
  FUNCTION RETURN_CHAR_LIST_TO_LINE(LST,SEP,EXT,SEP2)    RESULT(LINE)
    CLASS(CHARACTER_LINKED_LIST),          INTENT(INOUT):: LST
    CHARACTER(*),                OPTIONAL, INTENT(IN   ):: SEP, SEP2
    CLASS(CHARACTER_LINKED_LIST),OPTIONAL, INTENT(INOUT):: EXT  !IF SPECIFIED YOU MUST SPECIFY SEP2
    CHARACTER(:),                            ALLOCATABLE:: LINE
    INTEGER:: IDX, I, J, SIZ, SEP_LEN, SEP2_LEN
    !
    IF(LST%N > 0) THEN
                     !
                     IF(PRESENT(SEP)) THEN
                         SEP_LEN = LEN(SEP)
                     ELSE
                         SEP_LEN = 0
                     END IF
                     SEP2_LEN = 0
                     !
                     SIZ=0
                     CALL MOVE_TO_FIRST_POS(LST)
                     DO IDX=1, LST%N
                           SELECT TYPE(CHAR => LST%CUR%I)
                                           TYPE IS (CHARACTER(*))
                                                                  SIZ = SIZ + LEN(CHAR)
                           END SELECT
                           CALL LST%NEXT()
                     END DO
                     !
                     IF(SEP_LEN > 0) THEN
                           DO CONCURRENT (IDX=1:LST%N-1);  SIZ = SIZ + SEP_LEN
                           END DO
                     END IF
                     !
                     IF(PRESENT(EXT)) THEN
                           IF(PRESENT(SEP2)) THEN
                              SEP2_LEN = LEN(SEP2)
                              DO CONCURRENT (IDX=1:EXT%N-1);  SIZ = SIZ + SEP2_LEN
                              END DO
                           END IF
                           !
                           CALL EXT%START
                           DO IDX=1, EXT%N
                                 SELECT TYPE(CHAR => EXT%CUR%I)
                                                 TYPE IS (CHARACTER(*))
                                                                        SIZ = SIZ + LEN(CHAR) + 1
                                 END SELECT
                                 CALL EXT%NEXT()
                           END DO
                           CALL EXT%START
                     END IF
                     !
                     ALLOCATE(CHARACTER(SIZ)::LINE)
                     !
                     CALL MOVE_TO_FIRST_POS(LST)
                     I=1
                     DO IDX=1, LST%N
                           SELECT TYPE(CHAR => LST%CUR%I)
                                           TYPE IS (CHARACTER(*))
                                                            SIZ = LEN(CHAR)
                                                            J = I - 1 + SIZ
                                                            LINE(I:J) = CHAR
                                                            I = I + SIZ
                           END SELECT
                           CALL LST%NEXT()
                           !
                           IF(PRESENT(EXT)) THEN
                               IF(IDX <= EXT%N) THEN
                                         IF(SEP2_LEN > 0) THEN
                                                         J = I - 1 + SEP2_LEN
                                                         LINE(I:J) = SEP2
                                                         I = I + SEP2_LEN
                                         END IF
                                         SELECT TYPE(CHAR => EXT%CUR%I)
                                                         TYPE IS (CHARACTER(*))
                                                                          SIZ = LEN(CHAR)
                                                                          J = I - 1 + SIZ
                                                                          LINE(I:J) = CHAR
                                                                          I = I + SIZ
                                         END SELECT
                                         CALL EXT%NEXT()
                               END IF
                           END IF
                           !
                           IF(SEP_LEN > 0) THEN
                                     IF(IDX .NE. LST%N) THEN
                                                            J = I - 1 + SEP_LEN
                                                            LINE(I:J) = SEP
                                                            I = I + SEP_LEN
                                     END IF
                                     
                           END IF
                     END DO
                     !
    ELSE
                     ALLOCATE(LINE, SOURCE=' ')
    END IF
    !
  END 
  !
  !#######################################################################
  !
  FUNCTION RETURN_CHAR_LIST_NBACK_LINES(LST,NBACK,SEP)    RESULT(LINE)
    CLASS(CHARACTER_LINKED_LIST),          INTENT(INOUT):: LST
    INTEGER,                               INTENT(IN   ):: NBACK
    CHARACTER(*),                OPTIONAL, INTENT(IN   ):: SEP
    CHARACTER(:),                            ALLOCATABLE:: LINE
    INTEGER:: IDX, I, J, SIZ, SEP_LEN, NLINE
    !
    IF(LST%N > 0 .AND. ASSOCIATED(LST%CUR)) THEN
                     !
                     IF(PRESENT(SEP)) THEN
                         SEP_LEN = LEN(SEP)
                     ELSE
                         SEP_LEN = 0
                     END IF
                     !
                     NLINE = 1
                     SELECT TYPE(CHAR => LST%CUR%I)
                                     TYPE IS (CHARACTER(*))
                                                            SIZ = LEN(CHAR)
                     END SELECT
                     !
                     DO IDX=1, NBACK
                           IF(ASSOCIATED(LST%CUR%PRV)) THEN
                                                                 LST%CUR=>LST%CUR%PRV
                                                                 NLINE = NLINE + 1
                                                                 SELECT TYPE(CHAR => LST%CUR%I)
                                                                                 TYPE IS (CHARACTER(*))
                                                                                                        SIZ = SIZ + LEN(CHAR)
                                                                 END SELECT
                           ELSE
                                                                 EXIT
                           END IF
                     END DO
                     !
                     IF(SEP_LEN > 0) THEN
                           DO CONCURRENT (IDX=1:NLINE-1);  SIZ = SIZ + SEP_LEN
                           END DO
                     END IF
                     !
                     ALLOCATE(CHARACTER(SIZ)::LINE)
                     !
                     I=1
                     DO IDX=1, NLINE
                           SELECT TYPE(CHAR => LST%CUR%I)
                                           TYPE IS (CHARACTER(*))
                                                            SIZ = LEN(CHAR)
                                                            J = I - 1 + SIZ
                                                            LINE(I:J) = CHAR
                                                            I = I + SIZ
                           END SELECT
                           CALL LST%NEXT()
                           !
                           !
                           IF(SEP_LEN > 0) THEN
                                     IF(IDX .NE. NLINE) THEN
                                                            J = I - 1 + SEP_LEN
                                                            LINE(I:J) = SEP
                                                            I = I + SEP_LEN
                                     END IF
                                     
                           END IF
                     END DO
                     !
    ELSE
                     ALLOCATE(LINE, SOURCE=' ')
    END IF
    !
  END FUNCTION
  !
  !#######################################################################
  !
  SUBROUTINE CHAR_ADD_TRIMMED_VALUE_TO_ENDING(LST,STR,POS)
    CLASS(CHARACTER_LINKED_LIST), INTENT(INOUT):: LST
    CHARACTER(*),                 INTENT(IN   ):: STR
    INTEGER, OPTIONAL,            INTENT(IN   ):: POS
    !
    IF(PRESENT(POS)) THEN
        CALL ADD_VALUE_AT(LST,TRIM(STR),POS)
    ELSE
        CALL EXTEND_LIST_FROM_ENDING(LST,TRIM(STR))
    END IF
    !
  END SUBROUTINE
  !
  !#######################################################################
  !
  SUBROUTINE CHAR_ADD_CORE_VALUE_TO_ENDING(LST,STR,POS)
    CLASS(CHARACTER_LINKED_LIST), INTENT(INOUT):: LST
    CHARACTER(*),                 INTENT(IN   ):: STR
    INTEGER, OPTIONAL,            INTENT(IN   ):: POS
    !
    IF(PRESENT(POS)) THEN
        CALL ADD_VALUE_AT(LST,TRIM(ADJUSTL(STR)),POS)
    ELSE
        CALL EXTEND_LIST_FROM_ENDING(LST,TRIM(ADJUSTL(STR)))
    END IF
    !
  END SUBROUTINE
  !
  !#######################################################################
  !
  SUBROUTINE CHAR_ADD_UNIQUE_VALUE_TO_ENDING(LST,STR,POS)
    CLASS(CHARACTER_LINKED_LIST), INTENT(INOUT):: LST
    CHARACTER(*),                 INTENT(IN   ):: STR
    INTEGER, INTENT(OUT), OPTIONAL:: POS
    INTEGER:: I
    LOGICAL:: NOT_FOUND
    !
    IF(LST%N > 0) THEN
                     IF(PRESENT(POS)) POS = 0
                     !
                     NOT_FOUND=.TRUE.
                     CALL MOVE_TO_FIRST_POS(LST)
                     DO I=1, LST%N
                                  SELECT TYPE(CHAR => LST%CUR%I)
                                  TYPE IS (CHARACTER(*))
                                                   IF(STR == CHAR) THEN
                                                                        NOT_FOUND = .FALSE.
                                                                        IF(PRESENT(POS)) POS = I
                                                                        EXIT
                                                   END IF    
                                  END SELECT
                                  CALL MOVE_TO_NEXT_POS(LST)
                     END DO
                     IF(NOT_FOUND) CALL EXTEND_LIST_FROM_ENDING(LST,STR)
    ELSE
                                   CALL EXTEND_LIST_FROM_ENDING(LST,STR)
                                   IF(PRESENT(POS)) POS = 0
    END IF
    !
  END SUBROUTINE
  !
  !#######################################################################
  !
  FUNCTION CHAR_VALUE_UNIQUE_IN_LIST(LST,STR) RESULT(IS_UNIQUE)
    CLASS(CHARACTER_LINKED_LIST), INTENT(INOUT):: LST
    CHARACTER(*),                 INTENT(IN   ):: STR
    INTEGER:: I
    LOGICAL:: IS_UNIQUE
    !
    IS_UNIQUE = .TRUE.
    IF(LST%N > 0) THEN
                     !
                     CALL MOVE_TO_FIRST_POS(LST)
                     DO I=1, LST%N
                                  SELECT TYPE(CHAR => LST%CUR%I)
                                  TYPE IS (CHARACTER(*))
                                                   IF(STR == CHAR) THEN
                                                                        IS_UNIQUE = .FALSE.
                                                                        EXIT
                                                   END IF    
                                  END SELECT
                                  CALL MOVE_TO_NEXT_POS(LST)
                     END DO
    END IF
    !
  END FUNCTION
  !
  !##################################################################################################
  !
  SUBROUTINE CHAR_INITIALIZE_LINKED_LIST(LST)
     CLASS(CHARACTER_LINKED_LIST):: LST
     !
     CALL CHAR_LINKED_LIST_DEALLOCATE(LST)
     CALL INITIALIZE_LINKED_LIST(LST)
     !
     ALLOCATE(LST%ERR, SOURCE='ERROR')
     !
  END SUBROUTINE
  !
  !##################################################################################################
  !
  SUBROUTINE CHAR_LINKED_LIST_DEALLOCATE(LST)
     CLASS(CHARACTER_LINKED_LIST):: LST
     !
     LST%LN => NULL()
     IF(ASSOCIATED(LST%ERR)) DEALLOCATE(LST%ERR)
     CALL DELETE_ALL_LINKED_LIST_ENTRIES(LST)
     !
  END SUBROUTINE
  !
  !##################################################################################################
  !
  SUBROUTINE FINAL_CHARACTER_LINKED_LIST_DEALLOCATE(LST)
     TYPE(CHARACTER_LINKED_LIST):: LST
     !
     CALL CHAR_LINKED_LIST_DEALLOCATE(LST)
     !
  END SUBROUTINE
  !
END MODULE
!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!    
