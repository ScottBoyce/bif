!
! CODE DEVELOPED BY SCOTT E BOYCE
!                   CONTACT <seboyce@usgs.gov> or <Boyce@engineer.com>
!
! MODULE LISTING:
!   PARSE_WORD_INTERFACE  - Routine to identify next word--that is next char that is not a space, tab, or comma.
!                              Or sequence captured between single or double quotes, such as "Hellow World"
!
!                           Subroutines
!
!                                    PARSE_WORD   (LN, LOC, ISTART, ISTOP, [COM_STOP], [FIND_NEXT], [OLD_LOC], [EOL]) --> Does not modify LN   --> INTENT(IN)
!                                    PARSE_WORD_UP(LN, LOC, ISTART, ISTOP, [COM_STOP], [FIND_NEXT], [OLD_LOC], [EOL]) --> Make word upper case --> INTENT(INOUT)
!                                    FIND_NONBLANK(LN, LOC)
!
!                                    GET_WORD(WORD,LN,LOC,         [OLD_LOC],[COM_STOP],[NO_UPCASE])
!                                    GET_WORD(WORD,LN,LOC,IS_ALLOC,[OLD_LOC],[COM_STOP],[NO_UPCASE])
!                                    GET_WORD(LN,LOC,ISTART,ISTOP,WORD,         [OLD_LOC],[COM_STOP],[NO_UPCASE])
!                                    GET_WORD(LN,LOC,ISTART,ISTOP,WORD,IS_ALLOC,[OLD_LOC],[COM_STOP],[NO_UPCASE])
!
!                                    RET_WORD(LN,[LOC,COM_STOP,NO_UPCASE]) RESULT(WORD)
!                                    RET_WORD(N,LN,[COM_STOP,NO_UPCASE])   RESULT(WORD)
!
!
MODULE PARSE_WORD_INTERFACE!, ONLY: PARSE_WORD, PARSE_WORD_UP, FIND_NONBLANK
  IMPLICIT NONE
  PRIVATE
  PUBLIC:: PARSE_WORD, PARSE_WORD_UP, FIND_NONBLANK
  PUBLIC:: GET_WORD, RET_WORD
  ! -------
  ! Caution with using GET_WORD as it is also defined in UTIL_INTERFACE - Just dont double import the subroutine through USE statments
  ! -------
  INTERFACE GET_WORD
    MODULE PROCEDURE GET_WORD_ASSUM        !GET_WORD(WORD,LN,LOC,         [OLD_LOC],[COM_STOP],[NO_UPCASE])
    MODULE PROCEDURE GET_WORD_ALLOC        !GET_WORD(WORD,LN,LOC,IS_ALLOC,[OLD_LOC],[COM_STOP],[NO_UPCASE])
    MODULE PROCEDURE GET_WORD_ASSUM_ISTART !GET_WORD(LN,LOC,ISTART,ISTOP,WORD,         [OLD_LOC],[COM_STOP],[NO_UPCASE])
    MODULE PROCEDURE GET_WORD_ALLOC_ISTART !GET_WORD(LN,LOC,ISTART,ISTOP,WORD,IS_ALLOC,[OLD_LOC],[COM_STOP],[NO_UPCASE])
  END INTERFACE
  !
  INTERFACE RET_WORD
    MODULE PROCEDURE GET_WORD_FUNCTION     !GET_WORD(LN,[LOC,COM_STOP,NO_UPCASE]) RESULT(WORD)
    MODULE PROCEDURE GET_WORD_FUNCTION_N   !GET_WORD(N,LN,[COM_STOP,NO_UPCASE]) RESULT(WORD)
  END INTERFACE
  !
  ! Constants used internally to module ----------------------------------------------------
  !
  CHARACTER,    PARAMETER:: TAB = ACHAR(9)
  CHARACTER(*), PARAMETER:: lowerCHAR="abcdefghijklmnopqrstuvwxyz"
  CHARACTER(*), PARAMETER:: upperCHAR="ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  !
  ! ----------------------------------------------------------------------------------------
  !
  CONTAINS
  !
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !
  !  Parse Word Routines
  !
  PURE SUBROUTINE PARSE_WORD_UP(LN, LOC, ISTART, ISTOP, COM_STOP, FIND_NEXT, OLD_LOC, EOL)
    ! ASSUMES COMPILER CAN HANDEL LN(I:I-1) AND SETS IT TO BLANK STRING
    CHARACTER(*),      INTENT(INOUT):: LN
    INTEGER,           INTENT(INOUT):: LOC,ISTART,ISTOP
    LOGICAL, OPTIONAL, INTENT(IN   ):: COM_STOP, FIND_NEXT
    INTEGER, OPTIONAL, INTENT(INOUT):: OLD_LOC           !RETURN ORIGINAL LOC LOCATION
    LOGICAL, OPTIONAL, INTENT(  OUT):: EOL
    INTEGER:: I, N
    !
    CALL PARSE_WORD(LN, LOC, ISTART, ISTOP, COM_STOP, FIND_NEXT, OLD_LOC, EOL)
    !
    IF(ISTART <= ISTOP) THEN
                        DO I=ISTART, ISTOP
                                     N = INDEX( lowerCHAR, LN(I:I))
                                     IF(N > 0) LN(I:I) = upperCHAR(N:N)
                        END DO
    END IF
    !
  END SUBROUTINE
  !
  !#########################################################################################################################
  !
  PURE SUBROUTINE PARSE_WORD(LN, LOC, ISTART, ISTOP, COM_STOP, FIND_NEXT, OLD_LOC, EOL)!SIMPLE_WORD_PARSE
    ! ASSUMES COMPILER CAN HANDEL LN(I:I-1) AND SETS IT TO BLANK STRING
    CHARACTER(*),      INTENT(IN   ):: LN
    INTEGER,           INTENT(INOUT):: LOC,ISTART,ISTOP
    LOGICAL, OPTIONAL, INTENT(IN   ):: COM_STOP, FIND_NEXT
    INTEGER, OPTIONAL, INTENT(INOUT):: OLD_LOC           !RETURN ORIGINAL LOC LOCATION
    LOGICAL, OPTIONAL, INTENT(  OUT):: EOL
    INTEGER:: LINE_LEN, LINE_TRIM, LOC0
    !
    LOC0 = LEN_TRIM(LN)        !Temp use in case of COM_STOP
    !
    LINE_TRIM= LOC0 + 1
    !
    IF(PRESENT(COM_STOP)) THEN
            IF(COM_STOP .AND. LOC0 > 0) THEN
                     !
                     LINE_LEN=INDEX(LN(:LOC0),'#')     !TMP VAR TO HOLD # POSITION
                  IF(LINE_LEN > 0) LINE_TRIM=LINE_LEN  !FOUND  # UPDATE LINE_TRIM TO BE ITS LENGTH
            END IF
    END IF
    !
    LOC0 = LOC  !Make backup of Loc
    !
    DO WHILE( LOC < LINE_TRIM )
                              IF(LN(LOC:LOC).NE.TAB .AND. LN(LOC:LOC).NE.' ' .AND. LN(LOC:LOC).NE.',') EXIT
                              LOC = LOC+1
    END DO
    !
    IF( LOC >= LINE_TRIM ) THEN
               LINE_LEN = LEN(LN)
        LOC   =LINE_LEN+1
        ISTART=LINE_LEN
        ISTOP =LINE_LEN-1
        IF(PRESENT(EOL)) EOL = .TRUE.
    ELSE
        IF(LN(LOC:LOC)=='"') THEN
                                LOC = LOC+1
                                ISTART = LOC
                                DO WHILE( LOC < LINE_TRIM )
                                    IF( LN(LOC:LOC) == '"' ) EXIT
                                    LOC = LOC+1
                                END DO
                                ISTOP = LOC-1
                                LOC = LOC+1
        ELSEIF(LN(LOC:LOC)=="'") THEN
                                LOC = LOC+1
                                ISTART = LOC
                                DO WHILE( LOC < LINE_TRIM )
                                    IF( LN(LOC:LOC) == "'" ) EXIT
                                    LOC = LOC+1
                                END DO
                                ISTOP = LOC-1
                                LOC = LOC+1

        ELSE
                                ISTART = LOC
                                LOC = LOC+1
                                DO WHILE( LOC < LINE_TRIM )
                                    IF( LN(LOC:LOC)==TAB .OR. LN(LOC:LOC)==' ' .OR. LN(LOC:LOC)==',') EXIT
                                    LOC = LOC+1
                                END DO
                                ISTOP = LOC-1
                                IF(ISTOP<ISTART) ISTOP=ISTART
        END IF
        !
        IF(PRESENT(FIND_NEXT)) THEN  !LOC MOVE TO NEXT WORD
        IF        (FIND_NEXT)  THEN
                                   DO WHILE( LOC < LINE_TRIM )
                                                             IF(LN(LOC:LOC).NE.TAB .AND. LN(LOC:LOC).NE.' ' .AND. LN(LOC:LOC).NE.',') EXIT
                                                             LOC = LOC+1
                                   END DO
        END IF
        END IF
        !
        IF(PRESENT(EOL)) EOL = .FALSE.
    END IF
    !
    IF(PRESENT(OLD_LOC)) OLD_LOC = LOC0
    !
  END SUBROUTINE
  !
  !#############################################################################################################################################################
  !
  PURE SUBROUTINE FIND_NONBLANK(LN,LOC)  ! FINDS NEXT NON-BLANK SPOT AND SETS LOC TO IT. SET TO LEN(LN)+1
    CHARACTER(*),           INTENT(IN   ):: LN                ! LINE TO PARSE WORD FROM
    INTEGER,                INTENT(INOUT):: LOC               ! LOC => STARTING LOCATION TO FIND WORD
    INTEGER:: DIM, LINE_TRIM
    !
    DIM       = LEN(LN)
    LINE_TRIM = LEN_TRIM(LN) + 1
    IF(LINE_TRIM > DIM) LINE_TRIM = DIM
    !
    IF(LN(LOC:LOC)==TAB .OR. LN(LOC:LOC)==' ' .OR. LN(LOC:LOC)==',') THEN
       LOC = LOC + 1
       DO WHILE( LOC < LINE_TRIM )
                              IF(LN(LOC:LOC).NE.TAB .AND. LN(LOC:LOC).NE.' ' .AND. LN(LOC:LOC).NE.',') EXIT
                              LOC = LOC + 1
       END DO
       IF(LN(LOC:LOC)==TAB .OR. LN(LOC:LOC)==' ' .OR. LN(LOC:LOC)==',') LOC = DIM + 1  !Reached end of line, no non-blanks left
    END IF
    !
  END SUBROUTINE
  !
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !
  !  Get Word Routines
  !
  PURE FUNCTION GET_WORD_FUNCTION(LN,LOC,COM_STOP,NO_UPCASE) RESULT(WORD) ! SETS WORD TO PARSED WORD, SETS IT TO "" IF END OF LINE OR NO WORD PARSED
    CHARACTER(*),             INTENT(IN):: LN          ! LINE TO PARSE WORD FROM
    INTEGER,      OPTIONAL,   INTENT(IN):: LOC         ! LOC => STARTING LOCATION TO FIND WORD
    LOGICAL,      OPTIONAL,   INTENT(IN):: COM_STOP
    LOGICAL,      OPTIONAL,   INTENT(IN):: NO_UPCASE   !SET IF YOU WANT TO LEAVE WORD ALONE
    CHARACTER(:),            ALLOCATABLE:: WORD        ! RETURN WORD
    INTEGER:: LLOC,ISTART,ISTOP
    LOGICAL:: UPCASE
    !
    IF(PRESENT(LOC)) THEN
          LLOC=LOC
    ELSE
          LLOC=1
    END IF
    !
    IF(PRESENT(NO_UPCASE)) THEN
          UPCASE=.NOT. NO_UPCASE
    ELSE
          UPCASE=.TRUE.
    END IF
    !
    CALL PARSE_WORD(LN,LLOC,ISTART,ISTOP,COM_STOP)
    !
    IF(ISTART <= ISTOP) THEN
                        WORD=LN(ISTART:ISTOP)
                        !
                        IF(UPCASE) CALL UPPER(WORD)
    ELSE
        WORD=''   ! = NULL_CHAR
    END IF
    !
  END FUNCTION
  !
  !#############################################################################################################################################################
  !
  PURE FUNCTION GET_WORD_FUNCTION_N(N,LN,COM_STOP,NO_UPCASE) RESULT(WORD) ! SETS WORD TO PARSED WORD, SETS IT TO "" IF END OF LINE OR NO WORD PARSED
    INTEGER,                  INTENT(IN):: N          ! The Nth word to return
    CHARACTER(*),             INTENT(IN):: LN         ! LINE TO PARSE WORD FROM
    LOGICAL,      OPTIONAL,   INTENT(IN):: COM_STOP
    LOGICAL,      OPTIONAL,   INTENT(IN):: NO_UPCASE  !SET IF YOU WANT TO LEAVE WORD ALONE
    CHARACTER(:),            ALLOCATABLE:: WORD       ! RETURN WORD
    INTEGER:: I,LLOC,ISTART,ISTOP
    LOGICAL:: UPCASE
    !
    LLOC=1
    !
    IF(PRESENT(NO_UPCASE)) THEN
          UPCASE=.NOT. NO_UPCASE
    ELSE
          UPCASE=.TRUE.
    END IF
    !
    DO I=1, N
              CALL PARSE_WORD(LN,LLOC,ISTART,ISTOP,COM_STOP)
    END DO
    !
    IF(ISTART <= ISTOP) THEN
                        WORD=LN(ISTART:ISTOP)
                        !
                        IF(UPCASE) CALL UPPER(WORD)
    ELSE
        WORD=''   ! = NULL_CHAR
    END IF
    !
  END FUNCTION
  !
  !#############################################################################################################################################################
  !
  PURE SUBROUTINE GET_WORD_ASSUM(WORD,LN,LOC,OLD_LOC,COM_STOP,NO_UPCASE)  ! SETS WORD TO PARSED WORD, SETS IT TO "" IF END OF LINE OR NO WORD PARSED
    CHARACTER(*),           INTENT(IN   ):: LN                ! LINE TO PARSE WORD FROM
    INTEGER,                INTENT(INOUT):: LOC               ! LOC => STARTING LOCATION TO FIND WORD
    CHARACTER(*),           INTENT(  OUT):: WORD              ! RETURN WORD
    LOGICAL,      OPTIONAL, INTENT(IN   ):: NO_UPCASE         !SET IF YOU WANT TO LEAVE WORD ALONE
    INTEGER,      OPTIONAL, INTENT(INOUT):: OLD_LOC           !RETURN ORIGINAL LOC LOCATION
    LOGICAL,      OPTIONAL, INTENT(IN   ):: COM_STOP
    INTEGER:: LOC0,ISTART,ISTOP
    !
    LOC0=LOC
    CALL PARSE_WORD(LN,LOC,ISTART,ISTOP,COM_STOP)
    !
    IF(ISTART <= ISTOP) THEN
        !
        WORD = LN(ISTART:ISTOP)
        !
        IF(PRESENT(NO_UPCASE)) THEN
            IF(.NOT. NO_UPCASE) CALL UPPER(WORD)
        ELSE
                                CALL UPPER(WORD)
        END IF
    ELSE
        WORD = ' '
    END IF
    !
    IF(PRESENT(OLD_LOC)) OLD_LOC = LOC0  !HAS TO BE AT END BECAUSE LOC COULD BE ALSO PASSED IN AS OLD_LOC
    !
  END SUBROUTINE
  !
  !#############################################################################################################################################################
  !
  PURE SUBROUTINE GET_WORD_ALLOC(WORD,LN,LOC,IS_ALLOC,OLD_LOC,COM_STOP,NO_UPCASE)  ! SETS WORD TO PARSED WORD, SETS IT TO "" IF END OF LINE OR NO WORD PARSED
    CHARACTER(*),             INTENT(IN   ):: LN                ! LINE TO PARSE WORD FROM
    INTEGER,                  INTENT(INOUT):: LOC               ! LOC => STARTING LOCATION TO FIND WORD
    CHARACTER(:),ALLOCATABLE, INTENT(INOUT):: WORD              ! RETURN WORD
    LOGICAL,                  INTENT(IN   ):: IS_ALLOC
    LOGICAL,      OPTIONAL,   INTENT(IN   ):: NO_UPCASE         !SET IF YOU WANT TO LEAVE WORD ALONE
    INTEGER,      OPTIONAL,   INTENT(INOUT):: OLD_LOC           !RETURN ORIGINAL LOC LOCATION
    LOGICAL,      OPTIONAL,   INTENT(IN   ):: COM_STOP
    INTEGER:: LOC0,ISTART,ISTOP
    !
    LOC0=LOC
    CALL PARSE_WORD(LN,LOC,ISTART,ISTOP,COM_STOP)
    !
    IF(ISTART <= ISTOP) THEN
        !
        IF(IS_ALLOC) THEN
           IF(ALLOCATED(WORD)) DEALLOCATE(WORD)
           ALLOCATE(WORD, SOURCE=LN(ISTART:ISTOP))
        ELSE
            WORD=LN(ISTART:ISTOP)
        END IF
        !
        IF(PRESENT(NO_UPCASE)) THEN
            IF(.NOT. NO_UPCASE) CALL UPPER(WORD)
        ELSE
                                CALL UPPER(WORD)
        END IF
    ELSE
        IF(IS_ALLOC) THEN
           IF(ALLOCATED(WORD)) DEALLOCATE(WORD)
           ALLOCATE(WORD, SOURCE=' ')
        ELSE
            WORD=' '
        END IF
    END IF
    !
    IF(PRESENT(OLD_LOC)) OLD_LOC = LOC0  !HAS TO BE AT END BECAUSE LOC COULD BE ALSO PASSED IN AS OLD_LOC
    !
  END SUBROUTINE
  !
  !#############################################################################################################################################################
  !
  PURE SUBROUTINE GET_WORD_ASSUM_ISTART(LN,LOC,ISTART,ISTOP,WORD,OLD_LOC,COM_STOP,NO_UPCASE)  ! SETS WORD TO PARSED WORD, SETS IT TO "" IF END OF LINE OR NO WORD PARSED
    CHARACTER(*),           INTENT(IN   ):: LN                ! LINE TO PARSE WORD FROM
    INTEGER,                INTENT(INOUT):: LOC               ! LOC => STARTING LOCATION TO FIND WORD
    INTEGER,                INTENT(INOUT):: ISTART,ISTOP
    CHARACTER(*),           INTENT(  OUT):: WORD              ! RETURN WORD
    LOGICAL,      OPTIONAL, INTENT(IN   ):: NO_UPCASE         !SET IF YOU WANT TO LEAVE WORD ALONE
    INTEGER,      OPTIONAL, INTENT(INOUT):: OLD_LOC           !RETURN ORIGINAL LOC LOCATION
    LOGICAL,      OPTIONAL, INTENT(IN   ):: COM_STOP
    INTEGER:: LOC0
    !
    LOC0=LOC
    CALL PARSE_WORD(LN,LOC,ISTART,ISTOP,COM_STOP)
    !
    IF(ISTART <= ISTOP) THEN
        !
        WORD = LN(ISTART:ISTOP)
        !
        IF(PRESENT(NO_UPCASE)) THEN
            IF(.NOT. NO_UPCASE) CALL UPPER(WORD)
        ELSE
                                CALL UPPER(WORD)
        END IF
    ELSE
        WORD = ' '
    END IF
    !
    IF(PRESENT(OLD_LOC)) OLD_LOC = LOC0  !HAS TO BE AT END BECAUSE LOC COULD BE ALSO PASSED IN AS OLD_LOC
    !
  END SUBROUTINE
  !
  !#############################################################################################################################################################
  !
  PURE SUBROUTINE GET_WORD_ALLOC_ISTART(LN,LOC,ISTART,ISTOP,WORD,IS_ALLOC,OLD_LOC,COM_STOP,NO_UPCASE)  ! SETS WORD TO PARSED WORD, SETS IT TO "" IF END OF LINE OR NO WORD PARSED
    CHARACTER(*),             INTENT(IN   ):: LN                ! LINE TO PARSE WORD FROM
    INTEGER,                  INTENT(INOUT):: LOC               ! LOC => STARTING LOCATION TO FIND WORD
    INTEGER,                  INTENT(INOUT):: ISTART,ISTOP
    CHARACTER(:),ALLOCATABLE, INTENT(  OUT):: WORD              ! RETURN WORD
    LOGICAL,                  INTENT(IN   ):: IS_ALLOC
    LOGICAL,      OPTIONAL,   INTENT(IN   ):: NO_UPCASE         !SET IF YOU WANT TO LEAVE WORD ALONE
    INTEGER,      OPTIONAL,   INTENT(INOUT):: OLD_LOC           !RETURN ORIGINAL LOC LOCATION
    LOGICAL,      OPTIONAL,   INTENT(IN   ):: COM_STOP
    INTEGER:: LOC0
    !
    LOC0=LOC
    CALL PARSE_WORD(LN,LOC,ISTART,ISTOP,COM_STOP)
    !
    IF(ISTART <= ISTOP) THEN
        !
        IF(IS_ALLOC) THEN
           IF(ALLOCATED(WORD)) DEALLOCATE(WORD)
           ALLOCATE(WORD, SOURCE=LN(ISTART:ISTOP))
        ELSE
            WORD=LN(ISTART:ISTOP)
        END IF
        !
        IF(PRESENT(NO_UPCASE)) THEN
            IF(.NOT. NO_UPCASE) CALL UPPER(WORD)
        ELSE
                                CALL UPPER(WORD)
        END IF
    ELSE
        IF(IS_ALLOC) THEN
           IF(ALLOCATED(WORD)) DEALLOCATE(WORD)
           ALLOCATE(WORD, SOURCE=' ')
        ELSE
            WORD=' '
        END IF
    END IF
    !
    IF(PRESENT(OLD_LOC)) OLD_LOC = LOC0  !HAS TO BE AT END BECAUSE LOC COULD BE ALSO PASSED IN AS OLD_LOC
    !
  END SUBROUTINE
  !
  ! -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  !
  PURE SUBROUTINE UPPER(LN)           ! UPPER only for internal use to MODULE PARSE_WORD_INTERFACE --> coerce compiler to inline calls in module
    CHARACTER(*), INTENT(INOUT):: LN
    INTEGER:: I, N
    !
    DO I=1, LEN_TRIM(LN)
        N = INDEX( lowerCHAR, LN(I:I))
        !
        IF(N > 0) LN(I:I) = upperCHAR(N:N)
    END DO
  END SUBROUTINE
  !
  !PURE SUBROUTINE UPPER(LN)
  !  CHARACTER(*), INTENT(INOUT):: LN
  !  INTEGER::I
  !  DO CONCURRENT(I=1:LEN(LN), LN(I:I).GE.'a' .AND. LN(I:I) <= 'z');  LN(I:I)=CHAR(ICHAR(LN(I:I))+Lower_to_Upper)
  !  END DO
  !END SUBROUTINE
  !
  ! -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  !
END MODULE