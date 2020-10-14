!
! CODE DEVELOPED BY SCOTT E BOYCE 
!                   CONTACT <seboyce@usgs.gov> or <Boyce@engineer.com>
!
!
!       
!   CYCLING_TEXT_FILE_INTERFACE
!                           DATA TYPE
!                                    CYCLING_TEXT_FILE
!                           SUBROUTINES
!                                    FL%OPEN      
!                                    FL%SET_HEADER
!                                    FL%REWIND    
!                                    FL%WRITE     
!                                    FL%SET_NVAL
!                                    FL%SET_FMT   
!                                    FL%SET_REC   
!                                    FL%BACK      
!                                    FL%NEXT      
!                                    FL%MOVE      
!                                    FL%CLOSE     
!                                    FL%SET_FNAME 
!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
! 
MODULE CYCLING_TEXT_FILE_INTERFACE!, ONLY: CYCLING_TEXT_FILE
  !OPENS AN FILE FOR WRITING. PROVIDES GENERIC INTERFACING FOR WRITTING TO FILE. IF OPEN/CLOSE AUOTMATICALLY CLOSES FILES
  !
  USE, INTRINSIC:: ISO_FORTRAN_ENV, ONLY: REAL32, REAL64, REAL128
  !
  USE CONSTANTS,                      ONLY: BLNK,NL,BLN,TAB,COM,Z,ONE,TWO,TEN,DZ,UNO,DOS,DIEZ,TRUE,FALSE
  USE GENERIC_OPEN_INTERFACE,         ONLY: GENERIC_OPEN, FORCE_UNIT_CLOSE
  USE NUM2STR_INTERFACE,              ONLY: NUM2STR
  USE ARRAY_DATA_TYPES,               ONLY: CHARACTER_TYPE
  USE ERROR_INTERFACE,                ONLY: FILE_IO_ERROR, WARNING_MESSAGE
  USE FILE_IO_INTERFACE,              ONLY: DATAFILE_UNIT_NUMBER
  USE FILE_IO_INTERFACE,              ONLY: GET_FILE_NAME
  USE POST_KEY_SUB,                   ONLY: CHECK_FOR_POST_KEY
  USE PARSE_WORD_INTERFACE,           ONLY: PARSE_WORD
  USE STRINGS,                        ONLY: UPPER, GET_INTEGER 
  IMPLICIT NONE
  PRIVATE
  PUBLIC:: CYCLING_TEXT_FILE
  !
  TYPE CYCLING_TEXT_FILE
      INTEGER:: RECLEN = Z
      CHARACTER(:), ALLOCATABLE:: REC
      !
      INTEGER:: HEDLEN = Z
      CHARACTER(:),ALLOCATABLE:: HED
      !
      INTEGER:: IU = Z
      LOGICAL:: IS_OPEN   = FALSE
      LOGICAL:: OPENCLOSE = FALSE
      LOGICAL:: ERROR
      INTEGER:: IN, IOUT
      CHARACTER(:),ALLOCATABLE:: FNAME
      !
      INTEGER:: NVAL=Z, VPOS = ONE, RPOS = ONE
      INTEGER,              DIMENSION(:), ALLOCATABLE:: NSPACE
      TYPE(CHARACTER_TYPE), DIMENSION(:), ALLOCATABLE:: FMT
      !
      CONTAINS
      !
      PROCEDURE, PASS(FL):: OPEN        => OPEN_CYCLING_TEXT_FILE !(LLOC,LN,IOUT,IN,NOSTOP,REQKEY,IU,BUFFER, KEY, NVAL, HED)
      PROCEDURE, PASS(FL):: SET_HEADER  => CYCLING_TEXT_FILE_SET_HEADER!(HEADER, [NOWRITE]) --WILL REWIND FILE AND WRITE HEADER
      PROCEDURE, PASS(FL):: REWIND      => REWIND_CYCLING_TEXT_FILE
      PROCEDURE, PASS(FL):: WRITE       => WRITE_RECORD_CYCLING_TEXT_FILE
      PROCEDURE, PASS(FL):: SET_NVAL
      GENERIC::             SET_FMT     => SET_SINGLE_FMT, SET_MULTI_FMT !(FMT,NSPACE,[POS])
      GENERIC::             SET_REC     => SET_SINGLE_REC, SET_MULTI_REC !(VAL,[POS])
      PROCEDURE, PASS(FL):: BACK        => BACKSPACE_TEXT_FILE !(FL, NLINE)
      PROCEDURE, PASS(FL):: NEXT        => NEXTLINE_TEXT_FILE  !(FL, NLINE)
      PROCEDURE, PASS(FL):: MOVE        => MOVE_CYCLING_TEXT_FILE !(FL_NEW)
      PROCEDURE, PASS(FL):: CLOSE       => CLOSE_CYCLING_TEXT_FILE
      PROCEDURE, PASS(FL):: SET_FNAME   => SET_FILE_NAME_CYCLING_TEXT_FILE
      PROCEDURE, PASS(FL), PRIVATE:: SET_SINGLE_FMT
      PROCEDURE, PASS(FL), PRIVATE:: SET_MULTI_FMT
      PROCEDURE, PASS(FL), PRIVATE:: SET_SINGLE_REC
      PROCEDURE, PASS(FL), PRIVATE:: SET_MULTI_REC
      !
      FINAL::                            FINAL_CLOSE_CYCLING_TEXT_FILE
  END TYPE
  !
  CONTAINS
  !
  PURE SUBROUTINE ALLOCATE_REC(FL, RECLEN)
    CLASS(CYCLING_TEXT_FILE),  INTENT(INOUT):: FL
    INTEGER,                   INTENT(IN   ):: RECLEN
    !
    IF(FL%RECLEN .NE. RECLEN) THEN
        !
        FL%RECLEN = RECLEN
        IF(ALLOCATED(FL%REC)) DEALLOCATE(FL%REC)
        !
        ALLOCATE(CHARACTER(RECLEN):: FL%REC)
        !
    END IF
    !
    FL%REC(:) = BLNK
    !
  END SUBROUTINE
  !
  PURE SUBROUTINE MOVE_CYCLING_TEXT_FILE(FL, FL_NEW)
    CLASS(CYCLING_TEXT_FILE),  INTENT(INOUT):: FL
    CLASS(CYCLING_TEXT_FILE),  INTENT(INOUT):: FL_NEW
    !
    FL_NEW%IU         =  FL%IU
    FL_NEW%OPENCLOSE  =  FL%OPENCLOSE
    FL_NEW%ERROR      =  FL%ERROR
    FL_NEW%IN         =  FL%IN
    FL_NEW%IOUT       =  FL%IOUT
    FL_NEW%IS_OPEN    =  FL%IS_OPEN
    FL_NEW%HEDLEN     =  FL%HEDLEN
    FL_NEW%NVAL       =  FL%NVAL
    FL_NEW%VPOS       =  FL%VPOS
    FL_NEW%RPOS       =  FL%RPOS
    !
    CALL ALLOCATE_REC(FL_NEW, FL%RECLEN)  !Set up FL%REC
    !
    FL%OPENCLOSE  =FALSE
    FL%IS_OPEN    =FALSE
    !
    IF(ALLOCATED(FL%FNAME)) THEN
        CALL MOVE_ALLOC(FL%FNAME,FL_NEW%FNAME)
    END IF
    !
    IF(ALLOCATED(FL%HED)) THEN
        CALL MOVE_ALLOC(FL%HED, FL_NEW%HED)
    END IF
    !
    IF(ALLOCATED(FL%FMT   )) THEN
        ALLOCATE(FL_NEW%FMT(FL%NVAL) )
        CALL FL%FMT%MOVE(FL_NEW%FMT)
        DEALLOCATE(FL%FMT)
    END IF
    IF(ALLOCATED(FL%NSPACE)) CALL MOVE_ALLOC(FL%NSPACE,FL_NEW%NSPACE)
    !
  END SUBROUTINE
  !
  IMPURE ELEMENTAL SUBROUTINE REWIND_CYCLING_TEXT_FILE(FL)
    CLASS(CYCLING_TEXT_FILE), INTENT(INOUT):: FL
    !
    IF(FL%IU.NE.Z) THEN
        REWIND(FL%IU)
        IF(ALLOCATED(FL%HED))  WRITE(FL%IU) FL%HED
    END IF
    !
  END SUBROUTINE
  !
  SUBROUTINE WRITE_RECORD_CYCLING_TEXT_FILE(FL)
    CLASS(CYCLING_TEXT_FILE), INTENT(INOUT):: FL
    !
    IF(FL%IU.NE.Z) WRITE(FL%IU) FL%REC, NL
    !
    FL%RPOS = ONE
    FL%VPOS = ONE
    !
  END SUBROUTINE
  !
  SUBROUTINE SET_NVAL(FL,NVAL)
    CLASS(CYCLING_TEXT_FILE), INTENT(INOUT):: FL
    INTEGER,                     INTENT(IN   ):: NVAL
    !
    IF(FL%NVAL < NVAL) THEN
        !
        FL%NVAL = NVAL
        !
        IF(ALLOCATED(FL%FMT   )) DEALLOCATE(FL%FMT   )
        IF(ALLOCATED(FL%NSPACE)) DEALLOCATE(FL%NSPACE)
        !
        ALLOCATE(FL%FMT   (FL%NVAL))
        ALLOCATE(FL%NSPACE(FL%NVAL), SOURCE=Z)
        !
    ELSEIF(NVAL < ONE) THEN
        FL%NVAL = Z
        IF(ALLOCATED(FL%FMT   )) DEALLOCATE(FL%FMT   )
        IF(ALLOCATED(FL%NSPACE)) DEALLOCATE(FL%NSPACE)
    END IF
    !
  END SUBROUTINE
  !
  SUBROUTINE SET_SINGLE_FMT(FL,FMT,NSPACE,POS)
    CLASS(CYCLING_TEXT_FILE), INTENT(INOUT):: FL
    CHARACTER(*),             INTENT(IN   ):: FMT
    INTEGER,                  INTENT(IN   ):: NSPACE
    INTEGER,                  INTENT(IN   ):: POS
    !
    IF(FL%NVAL > Z .AND. POS > Z .AND. POS <= FL%NVAL) THEN
        FL%FMT   (POS) = FMT
        FL%NSPACE(POS) = NSPACE
    ELSE
        WRITE(*,'(A)') 'CODE ERROR -- CYCLING_TEXT_FILE%SET_SINGLE_FMT NOT SET UP CORRECTLY'
    END IF
    !
  END SUBROUTINE
  !
  SUBROUTINE SET_MULTI_FMT(FL,FMT,NSPACE,POS)
    CLASS(CYCLING_TEXT_FILE), INTENT(INOUT):: FL
    CHARACTER(*),DIMENSION(:),INTENT(IN   ):: FMT
    INTEGER,     DIMENSION(:),INTENT(IN   ):: NSPACE
    INTEGER,     OPTIONAL,    INTENT(IN   ):: POS
    INTEGER:: I, J, P, N
    !
    P = ONE
    IF(PRESENT(POS)) P = POS
    !
    N = P+SIZE(NSPACE)-ONE
    !
    IF(FL%NVAL > Z .AND.N <= FL%NVAL) THEN
        J = Z
        DO I = P, N
                     J = J + ONE
                     FL%FMT   (I) = FMT   (J)
                     FL%NSPACE(I) = NSPACE(J)
        END DO
    ELSE
        WRITE(*,'(A)') 'CODE ERROR -- CYCLING_TEXT_FILE%SET_SINGLE_FMT NOT SET UP CORRECTLY'
    END IF
    !
  END SUBROUTINE
  !
  SUBROUTINE SET_SINGLE_REC(FL,VAL,POS)
    CLASS(CYCLING_TEXT_FILE), INTENT(INOUT):: FL
    CLASS(*),                 INTENT(IN   ):: VAL
    INTEGER,     OPTIONAL,    INTENT(IN   ):: POS
    !
    ASSOCIATE(P => FL%VPOS, LOC => FL%RPOS)
       !
       IF(PRESENT(POS)) THEN
                        P = POS
                        IF    (P==ONE) THEN
                                                 LOC = ONE
                        ELSEIF(P <= FL%NVAL) THEN
                                                 LOC = SUM(FL%NSPACE(1:POS-1)) + ONE
                        END IF
       END IF
       !
       IF(LOC <= FL%RECLEN .AND. P <= FL%NVAL) THEN
           SELECT TYPE (VAL)
           TYPE IS (INTEGER       ); WRITE( FL%REC(LOC:), FL%FMT(P)%STR ) VAL      
           TYPE IS (CHARACTER(*)  ); WRITE( FL%REC(LOC:), FL%FMT(P)%STR ) VAL 
           TYPE IS (REAL(REAL64)  ); WRITE( FL%REC(LOC:), FL%FMT(P)%STR ) VAL
           TYPE IS (REAL(REAL32)  ); WRITE( FL%REC(LOC:), FL%FMT(P)%STR ) VAL
           TYPE IS (REAL(REAL128) ); WRITE( FL%REC(LOC:), FL%FMT(P)%STR ) VAL
           TYPE IS (CHARACTER_TYPE); WRITE( FL%REC(LOC:), FL%FMT(P)%STR ) VAL%STR
           END SELECT
           !
           LOC = LOC + FL%NSPACE(P)
           P = P + ONE
       ELSE
           WRITE(*,'(A)') 'CODE ERROR -- CYCLING_TEXT_FILE%SET_SINGLE_FMT NOT SET UP CORRECTLY'
       END IF
       !
    END ASSOCIATE
    !
  END SUBROUTINE
  !
  SUBROUTINE SET_MULTI_REC(FL,VAL,POS)
    CLASS(CYCLING_TEXT_FILE), INTENT(INOUT):: FL
    CLASS(*),    DIMENSION(:),INTENT(IN   ):: VAL
    INTEGER,     OPTIONAL,    INTENT(IN   ):: POS
    INTEGER:: I, J, N
    !
    ASSOCIATE(P => FL%VPOS, LOC => FL%RPOS)
       !
       IF(PRESENT(POS)) THEN
                        P = POS
                        IF    (P==ONE) THEN
                                                 LOC = ONE
                        ELSEIF(P <= FL%NVAL) THEN
                                                 LOC = SUM(FL%NSPACE(1:POS-1)) + ONE
                        END IF
       END IF
       !
       J = Z
       N = P + SIZE(VAL) - ONE
       !
       IF(N>FL%NVAL) N = FL%NVAL
       !
       DO I=P, N
           !
           IF(LOC <= FL%RECLEN .AND. P <= FL%NVAL) THEN
               J = J + ONE
               SELECT TYPE (VAL)
               TYPE IS (INTEGER       ); WRITE( FL%REC(LOC:), FL%FMT(P)%STR ) VAL(J)      
               TYPE IS (CHARACTER(*)  ); WRITE( FL%REC(LOC:), FL%FMT(P)%STR ) VAL(J) 
               TYPE IS (REAL(REAL64)  ); WRITE( FL%REC(LOC:), FL%FMT(P)%STR ) VAL(J)
               TYPE IS (REAL(REAL32)  ); WRITE( FL%REC(LOC:), FL%FMT(P)%STR ) VAL(J)
               TYPE IS (REAL(REAL128) ); WRITE( FL%REC(LOC:), FL%FMT(P)%STR ) VAL(J)
               TYPE IS (CHARACTER_TYPE); WRITE( FL%REC(LOC:), FL%FMT(P)%STR ) VAL(J)%STR
               END SELECT
               !
               LOC = LOC + FL%NSPACE(I)
           ELSE
               WRITE(*,'(A)') 'CODE ERROR -- CYCLING_TEXT_FILE%SET_SINGLE_FMT NOT SET UP CORRECTLY'
           END IF
       END DO
       !
       P = P + SIZE(VAL)
       !
    END ASSOCIATE
    !
  END SUBROUTINE
  !
  SUBROUTINE OPEN_CYCLING_TEXT_FILE(FL,RECLEN,LLOC,LN,IOUT,IN,NOSTOP,REQKEY,IU,BUFFER, KEY, DIM, NVAL, HED)
    ! ATTEMPTS TO READ KEYWORDS AND CREATE A FILE FOR WRITTING TO. 
    ! FILE ASSUMES CONSTANT CHRACTER(RECLEN) RECORD LENGTH PER LINE
    ! SETS ERROR=TRUE IF FILE FAILED TO IDENTIFY OR OPEN A FILE/UNIT.
    !
    ! THE ORDER THAT THE FILE IS ATTEMPTED TO BE IDENTIFIED IS:
    ! 1) READ SINGLE UNIT NUMBER
    ! 2) CHECK FOR OPTIONAL KEYWORD "BINARY" --ADDED TO OPEN FILE
    ! 4) CHECK FOR KEYWORD "INTERNAL", "LIST", OR THE REST OF HTE LINE IS BLANK TO INDICATE THAT OUTPUT IS WRITTEN TO LIST FILE
    ! 5) CHECK FOR KEYWORD EXTERNAL FOLLOWED BY UNIT NUMBER
    ! 6) CHECK FOR KEYWORD OPEN/CLOSE AND THEN OPENS FILE SPECIFIED
    ! 7) CHEKS IF THE LINE CONTAINS A FILE THAT CAN BE OPENED (SAME AS IF OPEN/CLOSE WAS NOT PRESENT)
    !
    ! LLOC   is the starting location of the line to look for KEYWORD/NAME
    ! LN     is the line to process the KEYWORDS/UNIT/NAME
    ! IOUT   is where to write error messages too
    ! IN     is the input file that LN originated from
    ! NOSTOP optional, when present and is true will prevent the program from stopping if the file fails to open. ERROR will be set to TRUE
    ! REQKEY optional, when present and is true indicates that a keyword is required to open file (viz. no reading a single number or just a file name)
    ! IU     optional, when present is the unit number used when a file is opened by OPEN/CLOSE or by NAME
    ! BUFFER optional, when present sets the buffer size in KB. --131072 = 128KB is the default and 1048576 = 1MB  --BUFFER USES TWO THREADS SO ACTUAL BUFFER IS TWICE THE VALUE (eg. 256KB)
    ! 
    CLASS(CYCLING_TEXT_FILE),  INTENT(INOUT):: FL
    INTEGER,                   INTENT(IN   ):: RECLEN
    INTEGER,                   INTENT(INOUT):: LLOC
    CHARACTER(*),              INTENT(IN   ):: LN
    INTEGER,                   INTENT(IN   ):: IOUT, IN
    LOGICAL,      OPTIONAL,    INTENT(IN   ):: NOSTOP
    LOGICAL,      OPTIONAL,    INTENT(IN   ):: REQKEY
    INTEGER,      OPTIONAL,    INTENT(IN   ):: IU
    INTEGER,      OPTIONAL,    INTENT(IN   ):: BUFFER
    CHARACTER(*), OPTIONAL,    INTENT(  OUT):: KEY  !SHOULD BE CHARACTER(10)
    INTEGER,      OPTIONAL,    INTENT(INOUT):: DIM
    INTEGER,      OPTIONAL,    INTENT(IN   ):: NVAL
    CHARACTER(*), OPTIONAL,    INTENT(IN   ):: HED
    LOGICAL:: ISOPEN, ALLOW_ERROR, NOREQKEY, DATAFILE, FOUND_KEY
    CHARACTER(10):: EXT
    !CHARACTER(13):: FORM_CHK, ACC_CHCK
    CHARACTER(:), ALLOCATABLE:: FNAME, ERR_MSG
    INTEGER:: I, ISTART, ISTOP, IERR, BUF, IU_READ, SPLIT
    !
    IERR = Z
    IU_READ = Z
    FL%ERROR     = FALSE
    FL%IS_OPEN   = FALSE
    DATAFILE     = FALSE
    FOUND_KEY    = FALSE
    !
    CALL ALLOCATE_REC(FL, RECLEN)  !Set up FL%REC
    !
    FL%RPOS = ONE
    FL%VPOS = ONE
    FL%NVAL = Z
    IF(PRESENT(NVAL)) FL%NVAL = NVAL
    !
    IF(ALLOCATED(FL%FNAME )) DEALLOCATE(FL%FNAME )
    IF(ALLOCATED(FL%FMT   )) DEALLOCATE(FL%FMT   )
    IF(ALLOCATED(FL%NSPACE)) DEALLOCATE(FL%NSPACE)
    !
    IF(FL%NVAL > Z) THEN
        ALLOCATE(FL%FMT   (FL%NVAL))
        ALLOCATE(FL%NSPACE(FL%NVAL), SOURCE=Z)
    END IF
    !
    IF(FL%OPENCLOSE) CLOSE(FL%IU)
    FL%OPENCLOSE = FALSE
    !
    FL%IOUT = IOUT
    FL%IU = Z
    !
    ISOPEN = FALSE
    IF(PRESENT(IU)) THEN
        IF(IU.NE.Z) THEN
                        INQUIRE(IU, OPENED=ISOPEN)
                        IF(ISOPEN) FL%IU = IU
        END IF
    END IF
    !
    IF(PRESENT(NOSTOP)) THEN
        ALLOW_ERROR = .NOT. NOSTOP
    ELSE
        ALLOW_ERROR = TRUE
    END IF
    !
    IF(PRESENT(REQKEY)) THEN
        NOREQKEY = .NOT. REQKEY
    ELSE
        NOREQKEY = TRUE
    END IF
    !
    BUF = 16384 != 16KB x2 = 32KB  --1048576 = 1MB   --NOTE THAT TWO THREADS ARE USED PER BUFFER SO ACTUAL SPACE IS TWICE AS BIG
    IF(PRESENT(BUFFER)) BUF = BUFFER
    !
    IF(ISOPEN) THEN
        !
        CALL STREAM_FILE_CHECK(FL, BUF, LN)
        !
        IF(FL%ERROR) THEN
            FL%IU = Z
        ELSE
            REWIND(FL%IU)
        END IF
        !
    ELSE!IF(.NOT. ISOPEN) THEN
            CALL PARSE_WORD(LN,LLOC,ISTART,ISTOP)
            IF (NOREQKEY .AND. LN(ISTART:ISTOP).NE.BLNK) THEN
                              READ(LN(ISTART:ISTOP),*,IOSTAT=IERR) IU_READ
            ELSE
                IERR=69
            END IF
            !
            IF (IERR .EQ. Z) THEN
                !
                FL%IU = IU_READ 
                CALL CHECK_FOR_POST_KEY(LLOC,LN,IN,IOUT,BUF,SPLIT,DIM=DIM)
                !
                IF (FL%IU == Z) THEN
                    CALL CLOSE_CYCLING_TEXT_FILE(FL)
                    CALL WARNING_MESSAGE(LN,IN,IOUT,MSG='GENERIC_OUTPUT_FILE_INSTRUCTION: SUCESSFULLY LOADED A UNIT NUMBER ON LINE,'//NL//'BUT THE UNIT NUMBER WAS ZERO, SO IT IS ASSUMED THAT NO OUTPUT IS MEANT TO BE WRITTEN.')
                    IF(PRESENT(KEY)) KEY = 'NOKEY'
                    RETURN
                END IF
                !
                CALL STREAM_FILE_CHECK(FL, BUF, LN)
            ELSE
                IERR= Z
                !
                EXT = LN(ISTART:ISTOP)
                CALL UPPER(EXT)
                !
                IF(EXT == 'BINARY') THEN
                                        CALL PARSE_WORD(LN,LLOC,ISTART,ISTOP)
                                        EXT = LN(ISTART:ISTOP)
                                        CALL UPPER(EXT)
                END IF
                !
                IF    (EXT == 'NOPRINT' .OR. EXT == 'SKIP' .OR. EXT == 'NUL' .OR. EXT == 'NULL') THEN
                                                 CALL CHECK_FOR_POST_KEY(LLOC,LN,IN,IOUT,BUF,SPLIT,DIM=DIM)
                                                 CALL CLOSE_CYCLING_TEXT_FILE(FL)
                                                 FL%IU = Z
                                                 IF(PRESENT(KEY)) KEY = ADJUSTL(EXT)
                                                 IF(PRESENT(DIM)) DIM = Z
                                                 RETURN
                ELSEIF(EXT == 'INTERNAL'.OR. EXT == 'LIST' .OR. EXT==BLNK) THEN  !WILL RAISE ERROR LATER
                                                     FL%IU = IOUT
                ELSEIF(EXT == 'EXTERNAL' .OR. EXT=='DATAUNIT') THEN
                                                 CALL GET_INTEGER(LN,LLOC,ISTART,ISTOP,IOUT,IN,FL%IU,MSG='GENERIC_OUTPUT_FILE_INSTRUCTION ERROR: FROUND KEYWORD "'//TRIM(EXT)//'" WHICH SHOULD BE FOLLOWED BY AN INTEGER REPRESENTING THE UNIT NUMBER TO USE.')
                                                 CALL CHECK_FOR_POST_KEY(LLOC,LN,IN,IOUT,BUF,SPLIT,DIM=DIM)
                                                 FOUND_KEY=TRUE
                                                 CALL STREAM_FILE_CHECK(FL, BUF, LN)
                ELSE
                      !
                      IF (EXT == 'OPEN/CLOSE' .OR. EXT=='DATAFILE') THEN  ! OPEN/CLOSE KEYWORD
                                                 DATAFILE = EXT=='DATAFILE'
                                                 CALL PARSE_WORD(LN,LLOC,ISTART,ISTOP)   !MOVE TO NEXT WORD WHICH IS THE FILE NAME
                                                 FL%OPENCLOSE = TRUE
                                                 FOUND_KEY    = TRUE
                      ELSEIF(NOREQKEY) THEN
                                                 FL%OPENCLOSE = TRUE
                      END IF
                      !
                      ALLOCATE( FNAME, SOURCE = LN(ISTART:ISTOP) )
                      !
                      CALL CHECK_FOR_POST_KEY(LLOC,LN,IN,IOUT,BUF,SPLIT,DIM=DIM)  !FL%BINARY IS ONLY SET TO TRUE IF BINARY FLAG FOUND, OTHERWISE IGNORED
                      !
                      INQUIRE(FILE=FNAME, NUMBER=I, OPENED=ISOPEN)
                      !
                      IF(ISOPEN .AND. PRESENT(IU)) THEN; IF (I.NE.IU) ISOPEN = FALSE
                      END IF
                      !
                      IF( DATAFILE .AND. ISOPEN) THEN
                                                 FL%IU = I
                                                 CALL STREAM_FILE_CHECK(FL, BUF, LN)
                      ELSEIF(FL%OPENCLOSE) THEN
                                                 INQUIRE(FILE=FNAME,NUMBER=FL%IU,OPENED=ISOPEN)
                                                 IF(ISOPEN) THEN
                                                     CALL WARNING_MESSAGE(LN,IN,IOUT,MSG='GENERIC_OUTPUT FILE OPEN: OPEN/CLOSE FILE WITH FILENAME:'//NL//TRIM(FNAME)//NL//'HAS ALREADY BEEN OPENED/ASSOCIATED WITH A FORTRAN UNIT NUMBER.'//NL//'OUTPUT TO THIS FILE MAY CONTAIN INFORMATION FROM MULTIPLE SOURCES.'//NL//'(THIS IS JUST A WARNING AS YOU MAY BE INTENTIONALY DOING THIS.)')
                                                     CALL STREAM_FILE_CHECK(FL, BUF, LN)
                                                     FL%OPENCLOSE = FALSE
                                                 ELSE
                                                     FL%IU = Z
                                                     IF(PRESENT(IU)) FL%IU = IU
                                                     !
                                                     CALL GENERIC_OPEN(FNAME, FL%IU, IOUT, ACTION='WRITE', FORM='UNFORMATTED', ACCESS='STREAM', STATUS='REPLACE', ASYNC='NO', BUFFER_BLOCKSIZE=BUF, BUFFER_COUNT=2, LINE=LN, INFILE=IN, ERROR=FL%ERROR)
                                                     !
                                                     IF     (FL%ERROR) THEN
                                                                           FL%OPENCLOSE = FALSE
                                                     ELSEIF (DATAFILE) THEN
                                                                           FL%OPENCLOSE = FALSE
                                                                           CALL DATAFILE_UNIT_NUMBER%ADD(FL%IU)
                                                     END IF
                                                 END IF
                      ELSE
                                                 FL%ERROR = TRUE  !DID NOT FIND OPEN/CLOSE KEYWORD AND REQKEY=TRUE
                      END IF
                END IF
            END IF
    END IF
    !
    IF((FL%IU == Z .OR. FL%IU == IOUT) .AND. .NOT. FL%ERROR) THEN
            !
            FL%ERROR = TRUE
            !
            IF(ALLOW_ERROR) THEN
                  !
                  IF( EXT=='INTERNAL') CALL FILE_IO_ERROR(Z,IN,LINE=LN,OUTPUT=IOUT,MSG='CYCLING_TEXT_FILE ERROR: FOUND KEYWORD "INTERNAL" BUT THIS OUTPUT DATA ITEM DOES NOT ALLOW FOR INTERNAL KEYWORD.'//BLN//'PLEASE WRITE OUTPUT TO SEPARATE FILE AND USE EITHER OPEN/CLOSE, DATAFILE, DATAUNIT, OR EXTERNAL.')
                  IF( EXT=='LIST')     CALL FILE_IO_ERROR(Z,IN,LINE=LN,OUTPUT=IOUT,MSG='CYCLING_TEXT_FILE ERROR: FOUND KEYWORD "LIST" BUT THIS OUTPUT DATA ITEM DOES NOT ALLOW FOR INTERNAL KEYWORD.'//BLN//'PLEASE WRITE OUTPUT TO SEPARATE FILE AND USE EITHER OPEN/CLOSE, DATAFILE, DATAUNIT, OR EXTERNAL.')
                  IF( FL%IU == Z .OR. EXT==BLNK) CALL FILE_IO_ERROR(Z,IN,LINE=LN,OUTPUT=IOUT,MSG='CYCLING_TEXT_FILE ERROR: THIS OUTPUT DATA ITEM DOES NOT ALLOW WRITING TO LIST FILE (NO KEYWORD FOUND, SO ASSUMING YOU MEANT THE LIST FILE).'//NL//'PLEASE WRITE OUTPUT TO SEPARATE FILE AND USE EITHER OPEN/CLOSE, DATAFILE, DATAUNIT, OR EXTERNAL.')
            END IF
    END IF
    !
    IF(.NOT. FL%ERROR) THEN
                           IF( FL%IU .NE. Z) THEN
                                                 INQUIRE(FL%IU, OPENED=ISOPEN) 
                           ELSE
                                                 ISOPEN = FALSE
                           END IF
                           !
                           IF(.NOT. ISOPEN) FL%ERROR =TRUE !FILE IS NOT INTERNAL, NOR UNIT WAS ASSOCIATED WITH A FILE, NOR WAS IT SUCCESFULLY OPENED.
    END IF
    !
    IF (FL%ERROR .AND. ALLOW_ERROR) THEN
        IF(EXT == 'INTERNAL'.OR. EXT == 'LIST' .OR. EXT==BLNK) THEN
            IF(EXT == 'INTERNAL') THEN
                ERR_MSG = NL//'FOUND KEYWORD "INTERNAL",'
            ELSEIF( EXT == 'LIST') THEN
                ERR_MSG = NL//'FOUND KEYWORD "LIST",'
            ELSE
                ERR_MSG = NL//'NO KEYWORD FOUND, ASSUMING THAT OUTPUT IS WRITTEN TO LIST.'
            END IF
            ERR_MSG = ERR_MSG//NL//'HOWEVER THE LIST FILE UNIT WAS NOT PASSED TO SUBROUTINE. THIS INDICATES THAT LIST WRITING IS NOT SUPPORTED FOR THIS OneWater OUTPUT OPTION.'//NL//'PLEASE CHOSE A DIFFERENT OUTPUT LOCATION (eg OPEN/CLOSE OR EXTERNAL).'
        ELSEIF(ALLOCATED(FNAME)) THEN
            IF(EXT == 'OPEN/CLOSE') THEN
                ERR_MSG = NL//'FOUND KEYWORD "OPEN/CLOSE",'
            ELSEIF( EXT == 'DATAFILE') THEN
                ERR_MSG = NL//'FOUND KEYWORD "DATAFILE",'
            ELSE
                ERR_MSG = NL//'NO KEYWORD FOUND, ASSUMING FILE NAME IS JUST SPECIFIED ON CURRENT LINE,'
            END IF
            ERR_MSG = ERR_MSG//NL//'BUT FAILED TO OPEN THE FOLLOWING FILE FOR WRITING:'//BLN//'"'//FNAME//'"'//BLN//'PLEASE CHECK TO SEE IF THE PATH AND FILE NAME CORRECT.'//BLN//' ***NOTE THAT THE "/" WORKS FOR BOTH WINDOWS AND LINUX,' //NL//'    BUT THE "\" ONLY WORKS ON WINDOWS.'
        ELSE
            IF(EXT == 'EXTERNAL') THEN
                ERR_MSG = NL//'FOUND KEYWORD "EXTERNAL",'
            ELSEIF( EXT == 'DATAUNIT') THEN
                ERR_MSG = NL//'FOUND KEYWORD "DATAUNIT",'
            ELSE
                ERR_MSG = NL//'NO KEYWORD FOUND AND A UNIT NUMBER WAS SUCCESSFULLY READ,'//NL//'SO IT WAS ASSUMED TO BE A UNIT SPECIFIED IN THE NAME FILE OR ALREADY OPEN,'
            END IF
            ERR_MSG = ERR_MSG//NL//'BUT UNIT NUMBER WAS NOT ASSOCIATED WITH ANY FILE (NOT OPEN) FOR WRITING.'//NL//'THIS PROBABLY IS BECAUSE IT WAS NOT SPECIFIED IN THE NAME FILE,'//NL//'FILE WAS CLOSED AT SOME POINT,'//NL//'OR EVEN NEVER SUCESSFULLY OPENED.'//BLN//'THE FOLLOWING IS THE UNIT NUMBER SEARCHED FOR: "'//NUM2STR(FL%IU)//'"'//BLN//'PLEASE CHECK TO SEE IF UNIT IS SPECIFIED IN THE NAME FILE WITH DATA OR DATA(BINARY) KEYWORDS.'
        END IF
        !
        ERR_MSG = 'FAILED TO OPEN FILE WITH GENERIC_OUTPUT_FILE_INSTRUCTION.'//BLN//ERR_MSG
        CALL FILE_IO_ERROR(IERR,IN,LINE=LN,OUTPUT=IOUT,MSG=ERR_MSG)
    END IF
    IF (FL%ERROR) FL%IU = Z
    !
    IF(PRESENT(KEY)) THEN
          IF(FOUND_KEY) THEN
                             KEY = ADJUSTL(EXT)
          ELSE
                             KEY = 'NOKEY'
          END IF
    END IF
    !
    FL%IS_OPEN = FL%IU.NE.Z
    !
    IF(FL%IS_OPEN) REWIND(FL%IU)
    !
    IF(PRESENT(HED)) CALL CYCLING_TEXT_FILE_SET_HEADER(FL, HED)
    !
    END SUBROUTINE
    !
  SUBROUTINE STREAM_FILE_CHECK(FL, BUF, LINE)
    CLASS(CYCLING_TEXT_FILE),  INTENT(INOUT):: FL
    INTEGER:: BUF
    CHARACTER(*), INTENT(IN):: LINE
    CHARACTER(13):: FORM_CHK, ACC_CHCK
    !
    INQUIRE(FL%IU, FORM=FORM_CHK, ACCESS=ACC_CHCK)
    !
    IF( (FORM_CHK == 'UNFORMATTED' .OR. FORM_CHK == 'BINARY') .AND. ACC_CHCK == 'STREAM' ) THEN
        !
        IF(.NOT. ALLOCATED(FL%FNAME)) CALL GET_FILE_NAME(FL%IU,FL%FNAME,IOUT=FL%IOUT,IN=FL%IN, MSG='CYCLING_TEXT_FILE_INTERFACE ERROR: UNKNOWN ERROR WHILE TRYING TO INDENTIFY THE FILE NAME.', LINE=LINE)
        CALL FORCE_UNIT_CLOSE(FL%IU)
        CALL GENERIC_OPEN(FL%FNAME, FL%IU, FL%IOUT, ACTION='WRITE', FORM='UNFORMATTED', ACCESS='STREAM', STATUS='REPLACE', ASYNC='NO', BUFFER_BLOCKSIZE=BUF, BUFFER_COUNT=2, LINE=LINE, INFILE=FL%IN, ERROR=FL%ERROR)
    END IF
    !
  END SUBROUTINE
  !
  SUBROUTINE CYCLING_TEXT_FILE_SET_HEADER(FL, HEADER, NOWRITE)
    CLASS(CYCLING_TEXT_FILE), INTENT(INOUT):: FL
    CHARACTER(*),             INTENT(IN   ):: HEADER
    LOGICAL,     OPTIONAL,    INTENT(IN   ):: NOWRITE
    LOGICAL:: WRITE_HED
    !
    IF(ALLOCATED(FL%HED)) DEALLOCATE(FL%HED)
                            ALLOCATE(FL%HED, SOURCE=HEADER//NL)
    !
    WRITE_HED = TRUE
    IF(PRESENT(NOWRITE)) WRITE_HED = .NOT. NOWRITE
    !
    IF(WRITE_HED .AND. FL%IU.NE.Z) THEN
        REWIND(FL%IU)
        WRITE(FL%IU) FL%HED
    END IF
    !
    FL%HEDLEN = LEN(FL%HED)
    !
    FL%VPOS = ONE
    FL%RPOS = ONE
    !
  END SUBROUTINE
  !
  SUBROUTINE BACKSPACE_TEXT_FILE(FL, NLINE)
    CLASS(CYCLING_TEXT_FILE), INTENT(INOUT):: FL
    INTEGER,     OPTIONAL,    INTENT(IN   ):: NLINE
    INTEGER:: N, P
    !
    P = Z
    !
    IF(FL%IU.NE.Z) THEN
       N = ONE
       IF(PRESENT(NLINE)) N = NLINE
       !
       N = N * (FL%RECLEN+ONE)
       !
       INQUIRE(FL%IU, POS=P)
       !
       IF(P - N > FL%HEDLEN ) THEN
           P = P - N
           WRITE(FL%IU,POS=P)
       ELSE
           CALL REWIND_CYCLING_TEXT_FILE(FL)
       END IF
    END IF
    !
  END SUBROUTINE
  !
  SUBROUTINE NEXTLINE_TEXT_FILE(FL, NLINE)
    CLASS(CYCLING_TEXT_FILE), INTENT(INOUT):: FL
    INTEGER,     OPTIONAL,    INTENT(IN   ):: NLINE
    INTEGER:: N, P
    !
    P = Z
    !
    IF(FL%IU.NE.Z) THEN
       N = ONE
       IF(PRESENT(NLINE)) N = NLINE
       !
       N = N * (FL%RECLEN+ONE)
       !
       INQUIRE(FL%IU, POS=P)
       !
       P = P + N - 1
       WRITE(FL%IU,POS=P)
    END IF
    !
  END SUBROUTINE
  !
  IMPURE ELEMENTAL SUBROUTINE SET_FILE_NAME_CYCLING_TEXT_FILE(FL)
    CLASS(CYCLING_TEXT_FILE), INTENT(INOUT):: FL
    LOGICAL:: EXIST
    !
    IF(FL%IU.NE.Z) THEN
        IF(.NOT. ALLOCATED(FL%FNAME)) THEN
              CALL GET_FILE_NAME(FL%IU,FL%FNAME,EXIST,FL%IOUT,FL%IN,MSG='CYCLING_TEXT_FILE_INTERFACE ERROR: FROUND KEYWORD "EXTERNAL" OR "DATAUNIT", BUT FAILED TO IDENTIFY THE FILE (IN PARTICULAR ITS NAME) THAT IS ASSOCAITED WITH IT.')
        END IF
    ELSEIF(ALLOCATED(FL%FNAME)) THEN
        DEALLOCATE(FL%FNAME)
    END IF
    !
  END SUBROUTINE
  !
  SUBROUTINE CLOSE_CYCLING_TEXT_FILE(FL)
    CLASS(CYCLING_TEXT_FILE), INTENT(INOUT):: FL
    !
    IF(FL%OPENCLOSE) CLOSE(FL%IU)
    IF(ALLOCATED(FL%FNAME)) DEALLOCATE(FL%FNAME)
    !
    FL%IU  = Z
    FL%RECLEN = Z
    FL%OPENCLOSE = FALSE
    FL%IS_OPEN   = FALSE
    !
    IF(ALLOCATED(FL%REC)) DEALLOCATE(FL%REC)
    !
    FL%NVAL = Z
    FL%VPOS = ONE
    FL%RPOS = ONE
    IF(ALLOCATED(FL%FMT)) DEALLOCATE(FL%FMT)
    !
    IF(ALLOCATED(FL%NSPACE)) DEALLOCATE(FL%NSPACE)
    !
  END SUBROUTINE
  !
  SUBROUTINE FINAL_CLOSE_CYCLING_TEXT_FILE(FL)
    TYPE(CYCLING_TEXT_FILE), INTENT(INOUT):: FL
    !
    CALL CLOSE_CYCLING_TEXT_FILE(FL)
    !
  END SUBROUTINE
  !
  !!!PURE SUBROUTINE CYCLING_TEXT_FILE_DEALLOCATE(FL)
  !!!  TYPE(CYCLING_TEXT_FILE),ALLOCATABLE,INTENT(INOUT):: FL
  !!!  INTEGER:: I
  !!!  !
  !!!  DEALLOCATE(FL, STAT=I)
  !!!  !
  !!!END SUBROUTINE
  !!!!
  !!!PURE SUBROUTINE CYCLING_TEXT_FILE_DEPOINT(FL)
  !!!  TYPE(CYCLING_TEXT_FILE),POINTER,INTENT(INOUT):: FL
  !!!  INTEGER:: I
  !!!  !
  !!!  DEALLOCATE(FL, STAT=I)
  !!!  !
  !!!END SUBROUTINE
  !
END MODULE
!
!  
