!
! CODE DEVELOPED BY SCOTT E BOYCE 
!                   CONTACT <seboyce@usgs.gov> or <Boyce@engineer.com>
!
! Holds a list of file unit numbers files and provides a convenient method of closing the files.
!    
! MODULE CONTAINS:
!
!    CHECK_FOR_POST_KEY(LLOC,LN,IN,IOUT,BUF,SPLIT,SCALE,FILSTAT,FILACT,ASYN, BINARY, NOPRINT, GO_TO_TOP, DIM, OLDLOC, FMT, ONLY_CHECK, MSG)
!                    !
!                    This routine is used to parse for various file post-keywords for Generic_Input and Generic_Output
!
!    FILE_AND_POST_KEY_PARSE(LLOC, LN, ISTART, ISTOP, CHECK_SCALE_WITHOUT_SF)
!                    !
!                    This routine sets ISTART and ISTOP such that it includes file directive, file name, and postkeywords in LN(ISTART:ISTOP)
!                    That is, it returns what the part of LN that CHECK_FOR_POST_KEY would use, and returns LLOC just beyond it search.
!
!
MODULE POST_KEY_SUB!, ONLY: CHECK_FOR_POST_KEY, FILE_AND_POST_KEY_PARSE
  USE CONSTANTS, ONLY: Z, ONE, TWO, FOUR, SEV, EIGHT, THOU, QUIN, DZ, UNO, NL, BLN, BLNK, COM, NO, TRUE, FALSE
  USE ERROR_INTERFACE,      ONLY: STOP_ERROR, WARNING_MESSAGE
  USE PARSE_WORD_INTERFACE, ONLY: PARSE_WORD
  IMPLICIT NONE
  PRIVATE
  PUBLIC:: CHECK_FOR_POST_KEY
  PUBLIC:: FILE_AND_POST_KEY_PARSE
  !
  CHARACTER(*), PARAMETER:: lowerCHAR="abcdefghijklmnopqrstuvwxyz"
  CHARACTER(*), PARAMETER:: upperCHAR="ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  !
  CONTAINS
  !
  SUBROUTINE CHECK_FOR_POST_KEY(LLOC,LN,IN,IOUT,BUF,SPLIT,SCALE,FILSTAT,FILACT,ASYN, BINARY, NOPRINT, GO_TO_TOP, DIM, OLDLOC, FMT, ONLY_CHECK, MSG)
    CHARACTER(*),              INTENT(IN   ):: LN                  !LN IS NOT MODIFIED, BUT NEEDS INOUT TO PASS TO PARSEWORD
    INTEGER,                   INTENT(IN   ):: IN, IOUT            !INFILE THAT LN CAME FROM, ERROR UNIT TO WRITE TOO
    INTEGER,                   INTENT(INOUT):: LLOC, BUF, SPLIT    !BUF=Returned buffer size, SPLIT=Returned Split size
    DOUBLE PRECISION,          INTENT(INOUT), OPTIONAL:: SCALE     !IS RETURNED IF A SCALE FACTOR IS FOUND
    CHARACTER(*),              INTENT(INOUT), OPTIONAL:: FILSTAT, FILACT, ASYN   !RETURNS ACTIONS THAT WERE TAKEN TO FILE
    LOGICAL,                   INTENT(INOUT), OPTIONAL:: BINARY, NOPRINT  !BINARY IS SET TO TRUE IF KEYWORD FOUND, OTHERWISE LEFT ALONE
    LOGICAL,                   INTENT(  OUT), OPTIONAL:: GO_TO_TOP
    INTEGER,                   INTENT(INOUT), OPTIONAL:: DIM
    INTEGER,                   INTENT(  OUT), OPTIONAL:: OLDLOC
    CHARACTER(:), ALLOCATABLE, INTENT(  OUT), OPTIONAL:: FMT
    LOGICAL,                   INTENT(IN   ), OPTIONAL:: ONLY_CHECK
    CHARACTER(*),              INTENT(IN   ), OPTIONAL:: MSG
    CHARACTER(32):: KEY  !Note 32 is hardwired into case check
    CHARACTER(:), ALLOCATABLE:: ERR
    INTEGER:: I, N, ISTART, ISTOP, LLOC_BAK, IERR, C, OLD_LOC !,Z
    DOUBLE PRECISION:: SF
    LOGICAL:: SKIP
    !
    SKIP = .FALSE.
    IF(PRESENT(ONLY_CHECK)) SKIP = ONLY_CHECK
    !
    !Z = 0
    IF(PRESENT(SCALE    )) SCALE    = UNO
    IF(PRESENT(FILSTAT  )) FILSTAT  = 'UNKNOWN'
    IF(PRESENT(FILACT   )) FILACT   = 'READWRITE'
    IF(PRESENT(ASYN     )) ASYN     = NO
    IF(PRESENT(NOPRINT  )) NOPRINT  = FALSE
    IF(PRESENT(GO_TO_TOP)) GO_TO_TOP= FALSE
    !
    OLD_LOC = LLOC
    ERR = NL
    !
    C = INDEX(LN,COM)   !COM = '#'
    IF( C==Z ) C = MIN( LEN_TRIM(LN)+ONE, LEN(LN) )   !NO # FOUND, SO GET THE SMALLER OF THE TWO LENGTHS
    !
    LLOC_BAK = LLOC
    DO WHILE (LLOC < C)
      !
      CALL PARSE_WORD(LN(:C),LLOC,ISTART,ISTOP)   !CHECK FOR BUFFER OR SPLIT FLAG
      !
      IF(LN(ISTOP:ISTOP) == COM ) ISTOP = ISTOP - ONE
      !
      IF(ISTART > ISTOP .OR. LN(ISTART:ISTOP) == BLNK) EXIT
      !
      KEY=LN(ISTART:ISTOP)
      !
      DO I=1, ISTOP - ISTART + 1
          N = INDEX( lowerCHAR, KEY(I:I))
          !
          IF(N > 0) KEY(I:I) = upperCHAR(N:N)
      END DO
      !
      SELECT CASE (KEY)
      CASE( 'BUFFER', 'BUFER', 'BUF', 'BUFF' )
                      LLOC_BAK = LLOC
                      CALL PARSE_WORD(LN,LLOC,ISTART,ISTOP)
                      !
                      READ(LN(ISTART:ISTOP),*,IOSTAT=IERR) BUF
                      !
                      IF(IERR.NE.Z .OR. LN(ISTART:ISTOP)==BLNK) THEN
                                                   BUF = 65536    ! 64KB x2 = 128KB  --1048576 = 1MB   --NOTE THAT TWO THREADS ARE USED PER BUFFER SO ACTUAL SPACE IS TWICE AS BIG
                                                   LLOC = LLOC_BAK
                      ELSEIF( BUF == THOU ) THEN
                                                   BUF = 524288   ! 512KB x2 = 1MB
                      ELSEIF( BUF == QUIN ) THEN
                                                   BUF = 262144   ! 256KB x2 = 0.5MB
                      ELSEIF( BUF > SEV   ) THEN
                                                   BUF = 4096*(BUF/EIGHT)  !Make sure it is a multiple of 8  -- 4096=512*8
                      ELSEIF( BUF > Z ) THEN
                                                   BUF = 512*BUF
                      ELSE
                                                   BUF = Z
                      END IF
                      LLOC_BAK = LLOC
      CASE( 'SPLIT' )
                      LLOC_BAK = LLOC
                      CALL PARSE_WORD(LN,LLOC,ISTART,ISTOP)
                      !
                      READ(LN(ISTART:ISTOP),*,IOSTAT=IERR) SPLIT
                      !
                      IF(IERR.NE.Z .OR. LN(ISTART:ISTOP)==BLNK) THEN
                                                   SPLIT = 2048     !NOTE MOST OS HAVE 4GB file size limit
                                                   LLOC = LLOC_BAK
                      ELSEIF( SPLIT == THOU ) THEN
                                                   SPLIT = 1024

                      ELSEIF( SPLIT == QUIN ) THEN
                                                   SPLIT = 512
                      ELSEIF( SPLIT > EIGHT ) THEN
                                                   SPLIT = 8*(SPLIT/8)  !Make sure it is a multiple of 8
                      ELSEIF( SPLIT > FOUR  ) THEN
                                                   SPLIT = 8
                      ELSEIF( SPLIT > TWO   ) THEN
                                                   SPLIT = 4
                      ELSEIF( SPLIT > Z ) THEN
                                                   SPLIT = 2
                      ELSE
                                                   SPLIT = Z
                      END IF
                      LLOC_BAK = LLOC
      CASE( 'SF', 'SCALE' )
                      CALL PARSE_WORD(LN,LLOC,ISTART,ISTOP)
                      !
                      IF(PRESENT(SCALE)) THEN
                          SF = UNO
                          READ(LN(ISTART:ISTOP),*, IOSTAT=I) SF
                          IF(I.NE.Z) THEN
                              ERR = 'FOUND KEYWORD "SF" OR "SCALE", BUT FAILED TO CONVERT SCALE FACTOR TO FLOATING POINT NUMBER.'
                              IF(PRESENT(MSG))  ERR=ERR//BLN//'THE FOLLOWING MESSAGE WAS PASSED TO CHECK_FOR_POST_KEY:'//BLN//MSG
                              CALL STOP_ERROR(LN,IN,IOUT,ERR)
                          END IF
                          IF(SF.NE.DZ) SCALE = SCALE * SF
                          !
                      ELSEIF(.NOT. SKIP) THEN
                          ERR = 'FOUND KEYWORD "SF" OR "SCALE", BUT THIS MODEL FEATURE DOES NOT ALLOW SCALE FACTORS. PLEASE REMOVE KEYWORD SF OR SCALE.'
                          IF(PRESENT(MSG))  ERR=ERR//BLN//'THE FOLLOWING MESSAGE WAS PASSED TO CHECK_FOR_POST_KEY:'//BLN//MSG
                          CALL STOP_ERROR(LN,IN,IOUT,ERR)
                      END IF
                      !
                      LLOC_BAK = LLOC
      CASE( 'OLD', 'READ' )
                      IF(PRESENT(FILSTAT  )) FILSTAT  = 'OLD'
                      IF(PRESENT(FILACT   )) FILACT   = 'READ'
                                             LLOC_BAK =  LLOC
      CASE( 'REPLACE', 'WRITE' )
                      IF(PRESENT(FILSTAT  )) FILSTAT  = 'REPLACE'
                      IF(PRESENT(FILACT   )) FILACT   = 'WRITE'
                                             LLOC_BAK =  LLOC
      CASE( 'BINARY' )
                      IF(PRESENT(BINARY)) THEN
                          BINARY = TRUE
                      ELSEIF(.NOT. SKIP) THEN
                          ERR = ERR//'FOUND KEYWORD "BINARY", BUT THIS INPUT/OUTPUT LINE DOES NOT SUPPORT THE "BINARY" KEYWORD. IT WILL BE IGNORED.'//NL
                          !WRITE(IOUT,'(/A,/A,/A/)') 'CHECK_FOR_POST_KEY WARNING: WHILE PROCESSING LINE: ',TRIM(LN),'FOUND KEYWORD "BINARY", BUT THIS INPUT/OUTPUT LINE DOES NOT SUPPORT THE "BINARY" KEYWORD. IT WILL BE IGNORED.'
                      END IF
                      LLOC_BAK =  LLOC
      CASE( 'ASYNC' )
                      IF(PRESENT(ASYN)) ASYN='YES'
                      LLOC_BAK =  LLOC
      CASE( 'NOPRINT' )
                      LLOC_BAK =  LLOC
                      IF(PRESENT(NOPRINT)) THEN
                          NOPRINT = TRUE
                          RETURN
                      ELSEIF(.NOT. SKIP) THEN
                          ERR = ERR//'FOUND KEYWORD "NOPRINT", BUT THIS INPUT/OUTPUT LINE DOES NOT SUPPORT THE NOPRINT KEYWORD. IT WILL BE IGNORED.'//NL
                          !WRITE(IOUT,'(/A,/A,/A)') 'CHECK_FOR_POST_KEY WARNING: WHILE PROCESSING LINE: ',TRIM(LN),'FOUND KEYWORD "NOPRINT", BUT THIS INPUT/OUTPUT LINE DOES NOT SUPPORT THE NOPRINT KEYWORD. IT WILL BE IGNORED.'
                      END IF
      CASE( 'REWIND', 'GO_TO_TOP' )
                      LLOC_BAK =  LLOC
                      IF(PRESENT(GO_TO_TOP)) THEN
                          GO_TO_TOP = TRUE
                      ELSEIF(.NOT. SKIP) THEN
                          ERR = ERR//'FOUND KEYWORD "'//TRIM(KEY)//'", BUT THIS INPUT/OUTPUT LINE DOES NOT SUPPORT REWINDING THE FILE TO THE FIRST LINE. IT WILL BE IGNORED.'//NL
                          !WRITE(IOUT,'(/A,/A,/A)') 'CHECK_FOR_POST_KEY WARNING: WHILE PROCESSING LINE: ',TRIM(LN),'FOUND KEYWORD "'//TRIM(KEY)//'", BUT THIS INPUT/OUTPUT LINE DOES NOT SUPPORT REWINDING THE FILE TO THE FIRST LINE. IT WILL BE IGNORED.'
                      END IF
                      !
      CASE ('DIM','DIMENSION')
                      CALL PARSE_WORD(LN,LLOC,ISTART,ISTOP)
                      !
                      IF(PRESENT(DIM)) THEN
                          READ(LN(ISTART:ISTOP),*, IOSTAT=I) DIM
                          IF(I.NE.Z) THEN
                             ERR = 'FOUND KEYWORD "'//TRIM(KEY)//'", WHICH MUST BE FOLLOWED BY AN INTEGER THAT REPRESENTS THE NUMBER OF ROWS TO LOAD, BUT FAILED TO CONVERT THE DIMENSION TO AN INTEGER (NUMBER FOLLOWING KEYWORD).'
                             IF(PRESENT(MSG))  ERR=ERR//BLN//'THE FOLLOWING MESSAGE WAS PASSED TO CHECK_FOR_POST_KEY:'//BLN//MSG
                             CALL STOP_ERROR(LN,IN,IOUT,ERR)
                          END IF
                      ELSEIF(.NOT. SKIP) THEN
                          ERR = ERR//'FOUND KEYWORD "'//TRIM(KEY)//'", BUT THIS MODEL FEATURE DOES NOT USE A USER SPECIFIED DIMENSION. IT WILL BE IGNORED.'//NL
                          !CALL WARNING_MESSAGE(LN,IN,IOUT,'FOUND KEYWORD "'//TRIM(KEY)//'", BUT THIS MODEL FEATURE DOES NOT USE A USER SPECIFIED DIMENSION. IT WILL BE IGNORED.')
                      END IF
                      !
                      LLOC_BAK = LLOC
      CASE ('AUTO','AUTOCOUNT','AUTO-COUNT')
                      LLOC_BAK = LLOC
                      IF(PRESENT(DIM)) THEN
                            DIM=Z
                      ELSEIF(.NOT. SKIP) THEN
                          ERR = ERR//'FOUND KEYWORD "'//TRIM(KEY)//'", BUT THIS MODEL FEATURE DOES NOT SUPPORT AUTOMATIC DIMENSION CALCULATION. IT WILL BE IGNORED.'
                          !CALL WARNING_MESSAGE(LN,IN,IOUT,'FOUND KEYWORD "'//TRIM(KEY)//'", BUT THIS MODEL FEATURE DOES NOT SUPPORT AUTOMATIC DIMENSION CALCULATION. IT WILL BE IGNORED.')
                      END IF
                      !
      CASE ('SIGFIG')
                      IF(PRESENT(FMT)) THEN
                          CALL PARSE_WORD(LN,LLOC,ISTART,ISTOP)
                          READ(LN(ISTART:ISTOP),*, IOSTAT=IERR) I
                          IF(IERR.NE.Z) THEN
                             ERR = 'FOUND KEYWORD "'//TRIM(KEY)//'", WHICH MUST BE FOLLOWED BY AN INTEGER THAT REPRESENTS THE NUMBER OF SIGNIFICANT DIGITS (PRECISION) OF THE OUTPUT, BUT FAILED TO READ THE INTEGER SPECIFIED AFTER THE KEYWORD.'
                             IF(PRESENT(MSG))  ERR=ERR//BLN//'THE FOLLOWING MESSAGE WAS PASSED TO CHECK_FOR_POST_KEY:'//BLN//MSG
                             CALL STOP_ERROR(LN,IN,IOUT,ERR)
                          END IF
                          FMT = LN(ISTART:ISTOP)
                      ELSEIF(.NOT. SKIP) THEN
                          ERR = ERR//'FOUND KEYWORD "'//TRIM(KEY)//'", BUT THIS MODEL FEATURE DOES NOT SUPPORT SPECIFYING THE NUMBER OF SIGNIFICANT DIGITS. IT WILL BE IGNORED.'
                          !CALL WARNING_MESSAGE(LN,IN,IOUT,'FOUND KEYWORD "'//TRIM(KEY)//'", BUT THIS MODEL FEATURE DOES NOT SUPPORT AUTOMATIC DIMENSION CALCULATION. IT WILL BE IGNORED.')
                      END IF
                      LLOC_BAK = LLOC
      CASE DEFAULT
                      IF(SKIP) EXIT
                      !
                      I = ONE
                      IF(PRESENT(SCALE)) THEN
                          SF = UNO
                          READ(LN(ISTART:ISTOP),*, IOSTAT=I) SF
                          IF(I==Z .AND. SF.NE.DZ) SCALE = SCALE * SF
                      END IF
                      !
                      IF(I.NE.Z) THEN
                          ERR = ERR//'FAILED TO IDENTIFY THE FOLLOWING POST-KEYWORD: '//BLN//'"'//TRIM(KEY)//'"'//BLN//'IT WILL BE IGNORED AND NO FURTHER KEYWORDS WILL BE CHECKED/INCLUDED TO THE RIGHT OF IT.'//NL//'TO SUPRESS THIS WARNING PLEASE PLACE A "#" SYMBOL TO THE LEFT OF THE WORD TO INDICATE EVERYTHING TO THE RIGHT OF THE # IS A COMMENT'
                          !CALL WARNING_MESSAGE(LN,IN,IOUT,MSG='CHECK_FOR_POST_KEY: FAILED TO IDENTIFY THE FOLLOWING POST-KEYWORD: '//BLN//'"'//TRIM(KEY)//'"'//BLN//'IT WILL BE IGNORED AND NO FURTHER KEYWORDS WILL BE CHECKED/INCLUDED TO THE RIGHT OF IT.'//NL//'TO SUPRESS THIS WARNING PLEASE PLACE A "#" SYMBOL TO THE LEFT OF THE WORD TO INDICATE EVERYTHING TO THE RIGHT OF THE # IS A COMMENT')
                          EXIT
                      ELSE
                          LLOC_BAK =  LLOC
                      END IF
      END SELECT
    END DO
    !
    LLOC = LLOC_BAK
    !
    IF(PRESENT(OLDLOC)) OLDLOC = OLD_LOC
    !
    IF(ERR.NE.NL) CALL WARNING_MESSAGE(LN,IN,IOUT,MSG='...CHECK_FOR_POST_KEY WARNINGS...'//NL//' THE FOLLOWING MESSAGES WERE PASSED WHILE CHECKING FOR ANY ADDITIONAL KEYWORDS ON LOADED LINE.'//BLN//ERR)
    !
  END SUBROUTINE
  !
  SUBROUTINE FILE_AND_POST_KEY_PARSE(LLOC, LN, ISTART, ISTOP, CHECK_SCALE_WITHOUT_SF)
    INTEGER,                   INTENT(INOUT):: LLOC                    ! Position to start search from, returns as ISTOP+1
    CHARACTER(*),              INTENT(IN   ):: LN                      ! LN IS NOT MODIFIED, BUT NEEDS INOUT TO PASS TO PARSEWORD
    INTEGER,                   INTENT(INOUT):: ISTART, ISTOP           ! Holds portion of line that contains DIRECTIVE, FILENAME, and POSTkeywords.
    LOGICAL,         OPTIONAL, INTENT(IN    ):: CHECK_SCALE_WITHOUT_SF ! Default is False, if TRUE, then will check for scale factor even if SF is not present.
    CHARACTER(32):: KEY  !Note 32 is hardwired into case check
    CHARACTER(:), ALLOCATABLE:: ERR
    INTEGER:: I, N, ISTR, ISTP, LLOC_BAK, IERR, C, ITMP !,Z
    DOUBLE PRECISION:: SF
    LOGICAL:: CHECK_SCALE
    !
    CHECK_SCALE = FALSE
    IF(PRESENT(CHECK_SCALE_WITHOUT_SF)) CHECK_SCALE = CHECK_SCALE_WITHOUT_SF
    !
    C = INDEX(LN,COM)   !COM = '#'
    IF( C==Z ) C = MIN( LEN_TRIM(LN)+ONE, LEN(LN) )   !NO # FOUND, SO GET THE SMALLER OF THE TWO LENGTHS
    !
    ISTART = LLOC
    ISTOP  = C
    !
    CALL PARSE_WORD(LN(:C),LLOC,ISTR,ISTP)  
    IF(LN(ISTP:ISTP) == COM ) ISTP = ISTP - ONE
    !
    IF( IS_EOL(ln, ISTR, ISTP) ) THEN  ! Line is empty, nothing to parse
        LLOC = ISTART
        RETURN
    END IF
    !
    ISTART = ISTR
    !
    KEY=LN(ISTR:ISTP)
    CALL UPCASE(KEY)
    !
    IF( KEY == 'BINARY' ) THEN
        ISTOP = ISTP
        CALL PARSE_WORD(LN(:C),LLOC,ISTR,ISTP)  
        CALL CHECK_COM(LN, ISTP, LLOC)
        KEY=LN(ISTR:ISTP)
        CALL UPCASE(KEY)
    END IF
    !
    IF ( KEY == 'CONSTANT'   .OR. &  ! Check for Directive that reads second argument
         KEY == 'EXTERNAL'   .OR. &
         KEY == 'DATAUNIT'   .OR. &
         KEY == 'OPEN/CLOSE' .OR. &
         KEY == 'DATAFILE'   ) THEN
                                   ISTOP = ISTP
                                   CALL PARSE_WORD(LN(:C),LLOC,ISTR,ISTP)  
                                   CALL CHECK_COM(LN, ISTP, LLOC)
    END IF
    !
    ! Start search for post keywords
    DO WHILE (ISTR <= ISTP)                   ! PARSE_WORD sets ISTP < ISTR when end of line is reached
        ISTOP = ISTP
        !
        CALL PARSE_WORD(LN(:C),LLOC,ISTR,ISTP)  
        CALL CHECK_COM(LN, ISTP, LLOC)
        !
        IF( IS_EOL(lN,ISTR,ISTP) ) EXIT
        !
        KEY=LN(ISTR:ISTP)
        CALL UPCASE(KEY)
        !
        IF ( KEY == 'BUFFER' .OR. &
             KEY == 'BUFER'  .OR. &
             KEY == 'BUF'    .OR. & 
             KEY == 'BUFF'   .OR. &
             KEY == 'SPLIT'  ) THEN
                                   ISTOP = ISTP
                                   CALL PARSE_WORD(LN(:C),LLOC,ISTR,ISTP)  
                                   CALL CHECK_COM(LN, ISTP, LLOC)
                                   !
                                   IF( IS_EOL(lN,ISTR,ISTP) ) EXIT
                                   !
                                   READ(LN(ISTR:ISTP),*,IOSTAT=IERR) ITMP
                                   IF(IERR /= Z) THEN
                                              LLOC = ISTOP + ONE
                                              ISTP = ISTOP
                                   END IF
        ELSE IF ( KEY == 'SF'        .OR. &
                  KEY == 'SCALE'     .OR. &
                  KEY == 'SIGFIG'    .OR. &
                  KEY == 'DIM'       .OR. &
                  KEY == 'DIMENSION' ) THEN
                                   CALL PARSE_WORD(LN(:C),LLOC,ISTR,ISTP)  
                                   CALL CHECK_COM(LN, ISTP, LLOC)
        ELSE IF ( KEY == 'BINARY'     .OR. &
                  KEY == 'ASYNC'      .OR. &
                  KEY == 'OLD'        .OR. &
                  KEY == 'READ'       .OR. &
                  KEY == 'REPLACE'    .OR. &
                  KEY == 'WRITE'      .OR. &
                  KEY == 'NOPRINT'    .OR. &
                  KEY == 'REWIND'     .OR. &
                  KEY == 'GO_TO_TOP'  .OR. &
                  KEY == 'AUTO'       .OR. &
                  KEY == 'AUTOCOUNT'  .OR. &
                  KEY == 'AUTO-COUNT' ) THEN
                                   CYCLE
        ELSE IF(CHECK_SCALE) THEN
                                   READ(LN(ISTR:ISTP),*,IOSTAT=IERR) SF
                                   IF(IERR /= Z) EXIT
        ELSE
            EXIT  ! Unknown keyword, so done searching
        END IF
    END DO
    !
    LLOC = ISTOP + ONE
    !
    CONTAINS
    !
    PURE SUBROUTINE UPCASE(KEY)
       CHARACTER(*),  INTENT(INOUT):: KEY
       INTEGER:: I, N
       !
       N = LEN_TRIM(KEY)
       !
       DO I=1, N
           N = INDEX( lowerCHAR, KEY(I:I))
           !
           IF(N > 0) KEY(I:I) = upperCHAR(N:N)
       END DO
       !
    END SUBROUTINE
    !
    PURE SUBROUTINE CHECK_COM(LN, ISTP, LLOC)
       CHARACTER(*),  INTENT(IN   ):: LN
       INTEGER,       INTENT(INOUT):: ISTP, LLOC
       IF(LN(ISTP:ISTP) == COM ) THEN
                  LLOC = ISTP
                  ISTP = ISTP - ONE
       END IF
    END SUBROUTINE
    !
    PURE FUNCTION IS_EOL(LN, ISTART, ISTOP) RESULT(ANS)
       CHARACTER(*), INTENT(IN):: LN
       INTEGER,      INTENT(IN):: ISTART, ISTOP
       LOGICAL:: ANS
       !
       IF     ( ISTART > ISTOP ) THEN
                             ANS = TRUE
       ELSE IF(LN(ISTART:ISTOP) == BLNK) THEN
                             ANS = TRUE
       ELSE
                             ANS = FALSE
       END IF
       !
    END FUNCTION
    !
  END SUBROUTINE
END MODULE