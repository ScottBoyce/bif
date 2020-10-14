!Only works if file storage and read is delimited to 1btye, 
!    that is ISO_FORTRAN_ENV has CHARACTER_STORAGE_SIZE = FILE_STORAGE_SIZE = 8 bit/stor
! BUFFERED_READER may have problems with files greater than 2GB due to int32 storage limits 
!                 or at least it has not been tested for files of that size
!
MODULE BUFFERED_READER_INSTRUCTION!, ONLY: BUFFERED_READER
  USE, INTRINSIC:: ISO_FORTRAN_ENV, ONLY: i32 => INT32,  i64 => INT64
  USE CONSTANTS, ONLY: NEG, Z, ONE, TWO, THREE, FOUR, TEN, TRUE, FALSE,            &
                       BLNK, TAB, CR, LF, NL, BLN,                      &
                       LONG_NEG, LONG_ZER, LONG_ONE, LONG_TWO, LONG_TEN
  USE ROLLING_POINTER_INSTRUCTION, ONLY: ROLLING_POINTER_INT64
  IMPLICIT NONE(TYPE, EXTERNAL)
  !
  PRIVATE
  PUBLIC:: BUFFERED_READER
  !
  INTEGER, PARAMETER :: initialBufSize = 4096
  !
  CHARACTER(2), PARAMETER :: SPLF = " "//ACHAR(10)  ! One Space with One Line Feed - Same as a Windows Line Feed after CR is stripped
  !
  TYPE BUFFERED_READER
      INTEGER(i32) :: DIM   = Z                     ! Current size of BUF -> CHARACTER(DIM):: BUF
      !
      CHARACTER(:), ALLOCATABLE :: FILE_NAME        ! Name of files associated with IU
      INTEGER(i32) :: IU    = Z                     ! Non-zero integer represents the FILE read into BUF
      !INTEGER(i32) :: IOUT  = Z                     ! File to write error messages and output
      !
      CHARACTER(:), POINTER:: BUF   => NULL()        ! Holds buffered part of file for reading
      CHARACTER(:), POINTER:: LINE  => NULL()        ! Pointer that holds current line, points to NULL_SPACE if not used
      CHARACTER(:), POINTER:: WORD  => NULL()        ! Pointer that holds current line, points to NULL_SPACE if not used
      !
      CHARACTER(:), POINTER:: NULL_SPACE  => NULL()  ! Only allocated as CHARACTER(4) and is pointed to by LINE
      !
      INTEGER(i32) :: r      = Z            ! Current Read Position in BUF, if less than 1, then indicates that buff is empty or at the start of the file (which has not been read)
      INTEGER(i32) :: w      = ONE          ! Position in BUF that can read more text -- file read buffer space
      INTEGER(i32) :: nextNL = NEG          ! Position in BUF where next NL is after r. If BUF is at end of file, then set to DIM+1. Set to -1 as a null value to indicate the next NL should be searched for.
      !
      !INTEGER(i64) :: POS  = LONG_ONE      ! Byte Position where file is located at
      INTEGER(i64) :: POSr = LONG_ONE       ! Byte Position at current reading point within BUF, viz. -> POSr is the byte position in IU for BUF(r:r)  => If r < 1, then POSr=1 to indicate the first byte is referenced --> CB%POS0 + CB%r - ONE 
      INTEGER(i64) :: POSw = LONG_ONE       ! Byte Position at w (typically the end of BUF),     viz. -> POSw is the byte position in IU for BUF(w:w)
      !
      INTEGER(i64) :: FILE_SIZE = LONG_ZER
      INTEGER(i32) :: FIRST_POS = ONE       ! Pointer to start of file. Set to 1, unless there is a byte order mark (BOM), then it is set to 4. --> Note it is a INT32 number
      !
      INTEGER(i64) :: NLINE     = LONG_ZER
      INTEGER(i64) :: LINE_NUM  = LONG_ZER
      !
      INTEGER(i32) :: MAX_LINE_SIZE = Z  
      !
      INTEGER(i32) :: p1 = NEG              !     Position in BUF that LINE points to
      INTEGER(i32) :: p2 = NEG              ! End Position in BUF that LINE points to, Set to -1 to indicate LINE has yet to be loaded, otherwise should equal nextNL-1
      !INTEGER, DIMENSION(:), ALLOCATABLE :: LINE_POS  
      !
      !
      LOGICAL :: CLOSE_IU = FALSE
      LOGICAL :: EOF      = FALSE
      !
      LOGICAL :: ERROR    = FALSE           ! If true, then BUF holds the error message
      CHARACTER(:), ALLOCATABLE :: MSG
      CHARACTER(:), ALLOCATABLE :: COM
      !
      LOGICAL:: TAB_TO_SPACE   = TRUE
      LOGICAL:: COMMA_TO_SPACE = TRUE
      !
      TYPE(ROLLING_POINTER_INT64):: BACK      ! BACK(1) = NL ahead, BACK(2) = NL behind, BACK(3) = BACKSPACE(IU)
      !
      CONTAINS
      !
      PROCEDURE, PASS(CB):: INIT         => INIT_BUFFERED_READER                ! CB%INIT([siz])
      !
      GENERIC::             READ_TO_DATA => READ_TO_DATA_EOF,      &    ! READ_TO_DATA([EOF])                 -> Sets CB%LINE to next line
                                            READ_TO_DATA_ECHO,     &    ! READ_TO_DATA(OUTPUT, [EOF], [HED]) 
                                            READ_TO_DATA_EOF_LINE, &    ! READ_TO_DATA(LINE, [EOF])
                                            READ_TO_DATA_ECHO_LINE      ! READ_TO_DATA(LINE, OUTPUT, [EOF], [HED])
      !
      PROCEDURE, PASS(CB):: REWIND       => FILL_AT_FILE_START_BUFFERED_READER  ! REWIND()
      PROCEDURE, PASS(CB):: BACKSPACE    => BACKSPACE_BUFFERED_READER           ! BACKSPACE([NBACK])
      PROCEDURE, PASS(CB):: GOTO_EOF     => END_OF_BUF_AND_FILE_BUFFERED_READER ! GOTO_EOF()
      !
      PROCEDURE, PRIVATE, PASS(CB):: ERROR_MSG   => ERROR_MSG_BUFFERED_READER ! Internal use
      PROCEDURE, PRIVATE, PASS(CB):: READ_TO_DATA_ECHO
      PROCEDURE, PRIVATE, PASS(CB):: READ_TO_DATA_EOF
      PROCEDURE, PRIVATE, PASS(CB):: READ_TO_DATA_ECHO_LINE
      PROCEDURE, PRIVATE, PASS(CB):: READ_TO_DATA_EOF_LINE
      !
      FINAL:: FINAL_BUFFERED_READER
      !
  END TYPE
  !
  INTERFACE INT2STR
    MODULE PROCEDURE INT2STR_i32
    MODULE PROCEDURE INT2STR_i64
  END INTERFACE
  !
  CONTAINS   ! ======================================================================================================================================= 
  !
  SUBROUTINE INIT_BUFFERED_READER(CB, FILE, COM, TAB_TO_SPACE, COMMA_TO_SPACE, SIZ, MAX_BACKSPACE)
    CLASS(BUFFERED_READER), INTENT(INOUT):: CB
    CHARACTER(*), OPTIONAL, INTENT(IN   ):: FILE
    LOGICAL,      OPTIONAL, INTENT(IN   ):: TAB_TO_SPACE   ! Strips all TAB's   from BUF to a space, Excludes values within quotes, Default is True
    LOGICAL,      OPTIONAL, INTENT(IN   ):: COMMA_TO_SPACE ! Strips all comma's from BUF to a space, Excludes values within quotes, Default is True
    CHARACTER(*), OPTIONAL, INTENT(IN   ):: COM   ! STRING OF ACCEPTED COMMENT SYMBOLS (1 byte/Char per symbol). 
                                                  !   -- DO NOT INCLUDE BLANK SPACES WITH OTHER SYMBOLS --
                                                  !   ---- FOR EXAMPLE THESE ARE WRONG:  "# !" or " #!" or "#! "  ----
    INTEGER,      OPTIONAL, INTENT(IN   ):: SIZ   ! Initial Size of BUF, default is initialBufSize
    INTEGER,      OPTIONAL, INTENT(IN   ):: MAX_BACKSPACE
    INTEGER:: DIM
    !
    CB%r = Z
    CB%w = ONE
    CB%nextNL = Z
    CB%EOF    = FALSE
    !
    CB%TAB_TO_SPACE   = TRUE
    CB%COMMA_TO_SPACE = TRUE
    !
    IF(PRESENT(MAX_BACKSPACE)) THEN
        CALL CB%BACK%INIT(MAX_BACKSPACE + TWO)   ! BACK(1) = NL ahead, BACK(2) = NL behind, BACK(3) = BACKSPACE(IU)
    ELSE
        CALL CB%BACK%INIT(8)
    END IF
    !
    IF(.NOT. ASSOCIATED(CB%NULL_SPACE)) THEN
               ALLOCATE(CHARACTER(FOUR):: CB%NULL_SPACE)
               CB%NULL_SPACE(:) = ""
    END IF
    !
    CALL RESET_ERROR_VARIABLES_BUFFERED_READER(CB)
    !
    ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    !
    IF( PRESENT(FILE) ) THEN
         CALL OPEN_FILE_BUFFERED_READER(CB, FILE)
    ELSE
         CALL RESET_BASE_VARIABLES_BUFFERED_READER(CB)
         CALL  RESET_POS_VARIABLES_BUFFERED_READER(CB)
    END IF
    !
    ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    !
    IF( .NOT. PRESENT(COM) ) THEN
                           CB%COM = " "
    ELSEIF(COM == "") THEN
                           CB%COM = " "
    ELSE
                           CB%COM = COM
    END IF
    !
    ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    !
    IF( PRESENT(TAB_TO_SPACE  ) ) CB%TAB_TO_SPACE   = TAB_TO_SPACE  
    IF( PRESENT(COMMA_TO_SPACE) ) CB%COMMA_TO_SPACE = COMMA_TO_SPACE
    !
    ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    !
    IF(PRESENT(SIZ)) THEN
        DIM = BUFFERED_READER_PROPER_DIM(SIZ)
    ELSE
        DIM = initialBufSize
    END IF
    !
    IF( LONG_ZER < CB%FILE_SIZE .AND. CB%FILE_SIZE < INT(DIM, i64) )   DIM = 128*(INT(CB%FILE_SIZE, i32)/128) + 128  ! File size is less then buffer
    !
    IF( DIM < CB%MAX_LINE_SIZE ) DIM = 512*(CB%MAX_LINE_SIZE/512) + 512                                              ! Max Line Length greater than buffer, increase to include it
    !
    CALL GROW_BUFFERED_READER(CB, DIM)
    !
    ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    !
    CALL SET_TO_BLANK(ONE, CB%DIM, CB%BUF)
    !
    CB%LINE => CB%NULL_SPACE
    !
    ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    !
    IF(CB%IU /= Z) CALL FILL_AT_FILE_START_BUFFERED_READER(CB)    ! CALL  FILL_BUF_BUFFERED_READER(CB)
    !
  END SUBROUTINE
  !
  !--------------------------------------------------------------------------------------------------------------------------
  !
  SUBROUTINE OPEN_FILE_BUFFERED_READER(CB, FILE)
    CLASS(BUFFERED_READER), INTENT(INOUT):: CB
    CHARACTER(*),           INTENT(IN   ):: FILE
    INTEGER:: ERR
    !
    IF( FILE == BLNK)  RETURN      ! If FILE = "", then do nothing
    !
    CALL RESET_BASE_VARIABLES_BUFFERED_READER(CB)
    !
    CALL RESET_POS_VARIABLES_BUFFERED_READER(CB)
    !
    CB%FILE_NAME = TRIM(FILE)
    !
    OPEN(NEWUNIT=CB%IU, FILE=FILE,          IOSTAT=ERR,       &
                        STATUS='OLD',       ACTION='READ',    &   
                        FORM='UNFORMATTED', ACCESS='STREAM',  &
                        POSITION='REWIND',  ASYNCHRONOUS='YES' )
    ! 
    ! Check if file contains the byte order mark (BOM)
    IF(ERR /= Z) THEN
        CB%IU = Z
        CALL CB%ERROR_MSG('OPEN_FILE_BUFFERED_READER(): Failed to open the read only file "'//TRIM(FILE)//'"'//NL// &
                          '                             The fortran error status code is: '//INT2STR(ERR) )
    ELSE
        CALL FILE_BOM_CHECK_BUFFERED_READER(CB)
    END IF
    !
    IF(ERR == Z .AND. CB%IU /= Z) CALL COUNT_LINES_BUFFERED_READER(CB)
    !
  END SUBROUTINE 
  !
  !--------------------------------------------------------------------------------------------------------------------------
  !
  SUBROUTINE FILL_AT_FILE_START_BUFFERED_READER(CB)
    CLASS(BUFFERED_READER), INTENT(INOUT):: CB
    !
    IF( CB%IU /= Z .AND. CB%DIM > Z ) THEN
        CALL    REWIND_BUFFERED_READER(CB, TRUE) 
        !
        CALL RESET_POS_VARIABLES_BUFFERED_READER(CB)
        CALL RESET_BUF_BUFFERED_READER(CB)
        !
        CALL  READ_BUF_BUFFERED_READER(CB, FALSE)
    END IF
    !
  END SUBROUTINE 
  !
  !--------------------------------------------------------------------------------------------------------------------------
  !
  SUBROUTINE BACKSPACE_BUFFERED_READER(CB, NBACK)
    CLASS(BUFFERED_READER), INTENT(INOUT):: CB
    INTEGER, OPTIONAL, INTENT(IN):: NBACK
    INTEGER:: NB
    INTEGER(i64):: POSnl
    INTEGER(i32):: DIFF                                 
    CHARACTER :: TXT
    !                                                   
    IF( CB%IU == Z .OR. CB%DIM < ONE ) RETURN 
    !
    ! NB < 0 -> Move forward NB lines
    ! NB = 0 -> Move to start of current line
    ! NB > 0 -> Move back NB lines
    ! 
    ! BACK(1) = NL ahead, 
    ! BACK(2) = NL behind 
    ! BACK(3) = BACKSPACE(IU)
    ! BACK(N) = BACKSPACE(N-2)
    !                                                   
    IF(PRESENT(NBACK)) THEN
                       IF ( NBACK < Z ) THEN        ! Negative backspace moves forward
                            DO NB = ONE, ABS(NBACK)
                                         CALL NEXT_LINE_BUFFERED_READER(CB)
                            END DO
                            RETURN
                       END IF                         
                       NB = NBACK + ONE             
    ELSE                                            
                       NB = TWO     ! Move back one line
    END IF
    !
    IF( NB > ONE .AND. CB%EOF ) CB%EOF = FALSE
    !
    POSnl = LONG_NEG
    NB = NB + ONE
    DO WHILE ( NB <= CB%BACK%DIM )
                                 POSnl = CB%BACK%GET(NB)
                                 IF( CB%POSr > POSnl    ) EXIT
                                 IF( POSnl  == LONG_NEG ) EXIT
                                 IF( POSnl  == LONG_ZER ) EXIT
                                 NB = NB + ONE
    END DO
    !
    IF    ( POSnl == LONG_NEG .OR. NB > CB%BACK%DIM ) THEN
                                                  NB = NB - TWO                ! Needs to specify the eact number of lines to search backward for.
                                                  CALL FIND_PREVIOUS_LF(CB,NB) ! Brute force search for previous line
    ELSEIF( POSnl == LONG_ZER ) THEN
                                CALL FILL_AT_FILE_START_BUFFERED_READER(CB)
                                CALL NEXT_LINE_BUFFERED_READER(CB)
    ELSE
        CB%LINE_NUM = CB%LINE_NUM - INT( NB - TWO, i64 )
        !
        DIFF = INT( CB%POSr - POSnl, i32 )
        IF( CB%r >= DIFF ) THEN
            CB%r    = CB%r - DIFF + ONE
            CB%POSr = POSnl + LONG_ONE
            !
            CB%nextNL = NEG
            CALL CHECK_nextNL_BUFFERED_READER(CB)
            CALL SET_LINE_POINTER_BUFFERED_READER(CB, CB%r, CB%nextNL - ONE)
            !
            NB = NB - TWO
            DO DIFF=ONE, NB                              ! Drop NL pointeres that are no longer relevant
                      CALL CB%BACK%APPEND(LONG_NEG)
            END DO
        ELSE 
            IF(POSnl < LONG_ONE) THEN
                POSnl = LONG_ZER
                CALL REWIND_BUFFERED_READER(CB, FALSE)
            ELSE
                READ(CB%IU, POS=POSnl) TXT
            END IF
            !
            CB%nextNL = NEG
            CB%w    = ONE
            CB%r    = ONE
            CB%POSr = POSnl + LONG_ONE
            CB%POSw = CB%POSr
            CALL READ_BUF_BUFFERED_READER(CB, FALSE)
            CALL SET_LINE_POINTER_BUFFERED_READER(CB, CB%r, CB%nextNL - ONE)
        END IF
    END IF
    !
  END SUBROUTINE 
  !
  !--------------------------------------------------------------------------------------------------------------------------
  !
  SUBROUTINE FIND_PREVIOUS_LF(CB, NBACK)        !Only call this function if CB%BACK does not hold backspace location
    CLASS(BUFFERED_READER), INTENT(INOUT):: CB
    INTEGER,                INTENT(IN   ):: NBACK
    INTEGER:: NB, I, STAT
    INTEGER(i64):: POS
    CHARACTER:: TXT
    !
    IF ( NBACK < Z ) THEN        ! Negative backspace moves forward
         DO NB = ONE, ABS(NBACK)
                      CALL NEXT_LINE_BUFFERED_READER(CB)
         END DO
         RETURN
    END IF   
    !
    !    RESET BACK TO ALL NUL VALUES
    CALL CB%BACK%SET(LONG_NEG)
    !
    IF( NBACK > Z .AND. CB%EOF ) CB%EOF = FALSE
    !
    NB = NBACK
    !
    ! Move back to start of line
    STAT = Z
    LINE_START: DO 
        DO I=CB%r, ONE, NEG
                   IF( CB%BUF(I:I) == LF ) EXIT LINE_START
                   CB%r    = CB%r + NEG         ! Subtract 1
                   CB%POSr = CB%POSr + LONG_NEG
        END DO
        !
        IF(CB%POSr < LONG_ONE)  EXIT LINE_START ! Reached start of file
        !
        ! Did not find LF, reverse load buf
        !
        CALL  SET_TO_BLANK(ONE, CB%DIM, CB%BUF)
        !
        POS = CB%POSr - INT(CB%DIM, i64) + LONG_ONE
        IF(POS < LONG_ONE) THEN
                         CALL REWIND_BUFFERED_READER(CB, TRUE)
                         CB%r = CB%POSr
                         READ(CB%IU, IOSTAT=STAT) CB%BUF
        ELSE
                         CB%r = CB%DIM
                         READ(CB%IU, POS=POS, IOSTAT=STAT) CB%BUF
        END IF
        !
        IF(STAT > Z)  EXIT LINE_START
        !
    END DO LINE_START
    !
    IF(STAT > Z) THEN
                 CALL CB%ERROR_MSG('READ_BUF_BUFFERED_READER(): Unknown error reading a line from the file "'//CB%FILE_NAME//'"'//NL// &
                                   '                            The fortran error status code is: '//INT2STR(STAT)//NL// &
                                   '                            This error occured while searching for a previous new line marker (BACKSPACE)' )
                 CALL END_OF_BUF_AND_FILE_BUFFERED_READER(CB)
                 RETURN
    END IF
    IF(CB%POSr < LONG_ONE) THEN
                 CALL FILL_AT_FILE_START_BUFFERED_READER(CB)
                 CALL NEXT_LINE_BUFFERED_READER(CB)
                 RETURN
    END IF
    !
    ! CB%r now points to the first-previous LF
    !
    STAT = Z
    LINE_BACKSPACE: DO WHILE ( NB > Z )
        CB%r    = CB%r    + NEG      
        CB%POSr = CB%POSr + LONG_NEG
        DO I=CB%r, ONE, NEG
                   IF( CB%BUF(I:I) == LF ) THEN
                                           NB = NB + NEG   ! Subtract 1
                                           CYCLE LINE_BACKSPACE
                   END IF
                   CB%r    = CB%r + NEG      
                   CB%POSr = CB%POSr + LONG_NEG
        END DO
        !
        IF(CB%POSr < LONG_ONE)  EXIT LINE_BACKSPACE ! Reached start of file
        !
        ! Did not find LF, reverse load buf
        !
        CB%POSr = CB%POSr + LONG_ONE         ! The plus one is to negate the "CB%POSr = CB%POSr + LONG_NEG" at the start of the loop
        !
        CALL SET_TO_BLANK(ONE, CB%DIM, CB%BUF)
        !
        POS = CB%POSr - INT(CB%DIM, i64) ! + LONG_ONE  -> already part of previous statement 
        IF(POS < LONG_ONE) THEN
                         CALL REWIND_BUFFERED_READER(CB, TRUE)
                         CB%r = CB%POSr
                         READ(CB%IU, IOSTAT=STAT) CB%BUF
        ELSE
                         CB%r = CB%DIM + ONE   ! The plus one is to negate the "CB%r = CB%r + NEG" at the start of the loop
                         READ(CB%IU, POS=POS, IOSTAT=STAT) CB%BUF
        END IF
        !
        IF(STAT > Z)  EXIT LINE_BACKSPACE
        !
    END DO LINE_BACKSPACE
    !
    IF    (STAT > Z) THEN
                 CALL CB%ERROR_MSG('READ_BUF_BUFFERED_READER(): Unknown error reading a line from the file "'//CB%FILE_NAME//'"'//NL// &
                                   '                            The fortran error status code is: '//INT2STR(STAT)//NL// &
                                   '                            This error occured while searching for a previous new line marker (BACKSPACE)' )
                 CALL END_OF_BUF_AND_FILE_BUFFERED_READER(CB)
                 !
    ELSEIF(CB%POSr < LONG_ONE) THEN
                 CALL FILL_AT_FILE_START_BUFFERED_READER(CB)
                 CALL NEXT_LINE_BUFFERED_READER(CB)
    ELSEIF(CB%r >= CB%DIM + NEG) THEN
           !
           READ(CB%IU, POS=CB%POSr) TXT
           !
           CB%nextNL = NEG
           CB%w    = ONE
           CB%r    = ONE
           CB%POSr = CB%POSr + LONG_ONE
           CB%POSw = CB%POSr
           CALL READ_BUF_BUFFERED_READER(CB, FALSE)
           CALL SET_LINE_POINTER_BUFFERED_READER(CB, CB%r, CB%nextNL - ONE)
           CB%LINE_NUM = CB%LINE_NUM - INT(NBACK,i64)
    ELSE
        IF( CB%BUF(CB%r:CB%r) /= LF ) ERROR STOP "FIND_PREVIOUS_LF(CB, NBACK) CODE ERRROR THIS SHOULD NOT HAPPEN"
        !
        CALL STRIP_CR(ONE, CB%DIM, CB%BUF)                         ! Clean up the buffer
        IF(CB%TAB_TO_SPACE  ) CALL STRIP_TAB  (ONE, CB%DIM, CB%BUF)
        IF(CB%COMMA_TO_SPACE) CALL STRIP_COMMA(ONE, CB%DIM, CB%BUF)
        !
        CB%nextNL = CB%r
        CALL UPDATE_BACK_BUFFERED_READER(CB)
        !
        CB%r    = CB%r    + ONE
        CB%POSr = CB%POSr + LONG_ONE
        !
        CALL UPDATE_POSw_W_BUFFERED_READER(CB)
        CALL CHECK_nextNL_BUFFERED_READER(CB)
        !
        IF( CB%nextNL < ONE ) CALL FILL_BUF_BUFFERED_READER(CB)
        CALL SET_LINE_POINTER_BUFFERED_READER(CB, CB%r, CB%nextNL - ONE)
        CALL UPDATE_BACK_BUFFERED_READER(CB)
        CB%LINE_NUM = CB%LINE_NUM - INT(NBACK,i64)
    END IF
    IF ( CB%LINE_NUM < LONG_ONE ) CB%LINE_NUM = LONG_ONE
    !
  END SUBROUTINE 
  !
  !--------------------------------------------------------------------------------------------------------------------------
  !
  PURE SUBROUTINE END_OF_BUF_AND_FILE_BUFFERED_READER(CB)
    CLASS(BUFFERED_READER), INTENT(INOUT):: CB
    !
    CB%EOF    = TRUE
    CB%nextNL = CB%DIM + ONE
    CB%r      = CB%nextNL
    CB%w      = CB%nextNL
    !CB%POS    = CB%FILE_SIZE + LONG_ONE
    CB%POSr   = CB%FILE_SIZE + LONG_ONE
    CB%POSw   = CB%POSr
    CB%LINE_NUM  = CB%NLINE  + LONG_ONE
    CALL SET_TO_BLANK(ONE, CB%DIM, CB%BUF)
    CALL SET_LINE_POINTER_BUFFERED_READER(CB, NEG, NEG)
    !
  END SUBROUTINE 
  !
  !--------------------------------------------------------------------------------------------------------------------------
  !
  SUBROUTINE FILL_BUF_BUFFERED_READER(CB)                    !Should only call this with CB%r >> 1 or CB%r -> CB%w, otherwise buf will just increase its size
    CLASS(BUFFERED_READER), INTENT(INOUT):: CB
    INTEGER:: LINE_LEN
    !
    IF( CB%IU == Z .OR. CB%DIM < ONE .OR. CB%POSr > CB%FILE_SIZE) RETURN
    !
    LINE_LEN = LEN_TRIM(CB%BUF)
    IF(LINE_LEN <  ONE   ) LINE_LEN = ONE
    IF(LINE_LEN == CB%DIM) LINE_LEN = CB%DIM - ONE 
    !
    IF( CB%POSw > CB%FILE_SIZE .AND. CB%r > LINE_LEN ) THEN ! At EOF
                                                       CALL END_OF_BUF_AND_FILE_BUFFERED_READER(CB)
    ELSE       ! File open 
        IF( CB%r > CB%DIM ) THEN                                  ! r greater than BUF, so reset it
                              CALL RESET_BUF_BUFFERED_READER(CB)
        ELSEIF( CB%r > ONE ) THEN                                 ! Shift buffer to drop previously read data
                              CALL SHIFT_r_TO_ONE_BUFFERED_READER(CB)
        END IF
        !
        CALL READ_BUF_BUFFERED_READER(CB, FALSE)
    END IF
    !
  END SUBROUTINE
  !
  !--------------------------------------------------------------------------------------------------------------------------
  !
  SUBROUTINE READ_BUF_BUFFERED_READER(CB, FILE_CHECK)
    CLASS(BUFFERED_READER), INTENT(INOUT):: CB
    LOGICAL,                INTENT(IN   ):: FILE_CHECK
    INTEGER:: STAT
    !
    IF(FILE_CHECK) THEN
       IF( CB%IU == Z .OR. CB%DIM < ONE .OR. CB%POSr > CB%FILE_SIZE) RETURN
       !
       STAT = LEN_TRIM(CB%BUF)                            ! Use STAT as temp holder of LINE_LEN
       IF(STAT <  ONE   ) STAT = ONE
       IF(STAT == CB%DIM) STAT = CB%DIM - ONE 
       !
       IF( CB%POSw > CB%FILE_SIZE .AND. CB%r > STAT) THEN ! At End of File
                                                     CALL END_OF_BUF_AND_FILE_BUFFERED_READER(CB)
                                                     RETURN
       END IF
    END IF
    !
    IF( CB%w > CB%DIM .AND. CB%r > ONE ) CALL SHIFT_r_TO_ONE_BUFFERED_READER(CB)  ! Out of written buffer, but can save some space by shifting --> Should never be true
    !
    IF( CB%w <= CB%DIM ) THEN
        !
        STAT = Z
        DO WHILE (STAT <= Z)
           CALL SET_TO_BLANK(CB%w, CB%DIM, CB%BUF)
           !
           READ(CB%IU, IOSTAT=STAT) CB%BUF(CB%w:CB%DIM)
           !
           IF( STAT <= Z ) THEN
              !
              CALL STRIP_CR(CB%w, CB%DIM, CB%BUF)
              !
              IF(CB%TAB_TO_SPACE  ) CALL STRIP_TAB  (CB%w, CB%DIM, CB%BUF)
              IF(CB%COMMA_TO_SPACE) CALL STRIP_COMMA(CB%w, CB%DIM, CB%BUF)
              !
              CALL UPDATE_POSw_W_BUFFERED_READER(CB)  ! Byte Position at end of BUF
              !
              CALL CHECK_nextNL_BUFFERED_READER(CB)
              !
              IF( Z < CB%nextNL ) THEN         ! Found LF, exit loop
                      STAT = ONE               
              ELSE
                  IF( STAT < Z ) THEN
                      CB%nextNL = CB%DIM + ONE
                      STAT      = ONE          ! Flag to exit loop
                  ELSE
                      CALL INCREASE_BUF_BUFFERED_READER(CB)       ! Failed to find LF, increase buffer
                  END IF
              END IF
           ELSE
               CALL CB%ERROR_MSG('READ_BUF_BUFFERED_READER(): Unknown error reading a line from the file "'//CB%FILE_NAME//'"'//NL// &
                                 '                            The fortran error status code is: '//INT2STR(STAT) )
               CALL END_OF_BUF_AND_FILE_BUFFERED_READER(CB)
           END IF
            !
            IF( Z < CB%nextNL ) CALL UPDATE_BACK_BUFFERED_READER(CB)
        END DO
    END IF
    !
  END SUBROUTINE
  !
  !--------------------------------------------------------------------------------------------------------------------------
  !
  RECURSIVE SUBROUTINE NEXT_LINE_BUFFERED_READER(CB, FIRST_LINE) ! Set CB%p2 = NEG for first line load or FIRST_LINE = TRUE
    CLASS(BUFFERED_READER), INTENT(INOUT):: CB
    LOGICAL, OPTIONAL,      INTENT(IN   ):: FIRST_LINE
    INTEGER:: DIF
    !
    IF(PRESENT(FIRST_LINE)) THEN
            IF(FIRST_LINE)  CALL FILL_AT_FILE_START_BUFFERED_READER(CB)
    END IF
    !
    IF(CB%DIM < ONE .OR. CB%EOF) RETURN
    !
    CALL CHECK_nextNL_BUFFERED_READER(CB)
    !
    IF( CB%nextNL < Z .AND. CB%POSw <= CB%FILE_SIZE ) CALL FILL_BUF_BUFFERED_READER(CB)   ! Need to feed more text into buf to get NL
    !
    IF    ( CB%nextNL >= CB%DIM .OR.  &
            CB%r      >  CB%DIM .OR.  &
            CB%POSr   >  CB%FILE_SIZE  ) THEN
                                         CALL END_OF_BUF_AND_FILE_BUFFERED_READER(CB)
                                         RETURN
    ELSEIF( CB%r == Z ) THEN                 ! Only true if file hs yet to be processed -> Read First Line => Asssumes CB%nextNL is set by FILL routine
                    CB%r  = ONE
                    CB%p1 = ONE
                    CB%p2 = CB%nextNL - ONE
                    CB%POSr     = LONG_ONE
                    CB%LINE_NUM = LONG_ONE
                    CALL UPDATE_BACK_BUFFERED_READER(CB)
    ELSE
        IF( CB%r > CB%nextNL ) THEN
                               CALL CHECK_nextNL_BUFFERED_READER(CB)
                               IF( CB%nextNL < Z ) CALL FILL_BUF_BUFFERED_READER(CB)
        END IF
        !
        DIF     = CB%nextNL - CB%r + ONE
        CB%r    = CB%nextNL + ONE    ! Read position moved to start of next line
        CB%POSr = CB%POSr + INT(DIF, i64)
        !
        CALL CHECK_nextNL_BUFFERED_READER(CB)
        IF( CB%nextNL < Z ) CALL FILL_BUF_BUFFERED_READER(CB)   ! Need to feed more text into buf to get next NL
        !
        CB%p1 = CB%r
        !
        IF(CB%nextNL > CB%DIM) THEN                            ! End of file - Point to LEN_TRIM + 1
                               CB%p2 = LEN_TRIM(CB%BUF) + ONE
                            IF(CB%p2 > CB%DIM) CB%p2 = CB%DIM
        ELSE
                               CB%p2 = CB%nextNL - ONE
        END IF
        !
        CB%LINE_NUM = CB%LINE_NUM + LONG_ONE
        CALL UPDATE_BACK_BUFFERED_READER(CB)
        !
    END IF
    !
    CALL SET_LINE_POINTER_BUFFERED_READER(CB)
    !
  END SUBROUTINE 
  !
  !--------------------------------------------------------------------------------------------------------------------------
  !
  SUBROUTINE FILE_BOM_CHECK_BUFFERED_READER(CB)
    CLASS(BUFFERED_READER), INTENT(INOUT):: CB
    CHARACTER(3):: BOM
    INTEGER:: ERR
    !
    CB%FIRST_POS = ONE
    !
    IF ( CB%IU /= Z ) THEN
        !
        REWIND( CB%IU, IOSTAT=ERR)
        READ  ( CB%IU, IOSTAT=ERR) BOM
        !
        IF(ERR == Z) THEN
            IF ( ICHAR(BOM(1:1)) == 239 .AND.  &
                 ICHAR(BOM(2:2)) == 187 .AND.  &
                 ICHAR(BOM(3:3)) == 191        ) INQUIRE(CB%IU, POS=CB%FIRST_POS) ! File contains byte order mark (BOM)
        END IF
        !
        CALL REWIND_BUFFERED_READER(CB, FALSE)
        !
    END IF
    !
  END SUBROUTINE 
  !
  !--------------------------------------------------------------------------------------------------------------------------
  !
  SUBROUTINE REWIND_BUFFERED_READER(CB, RESET)
    CLASS(BUFFERED_READER), INTENT(INOUT):: CB
    LOGICAL, OPTIONAL,      INTENT(IN   ):: RESET
    CHARACTER :: TXT
    INTEGER   :: P
    !
    IF(PRESENT(RESET)) THEN
            IF(RESET) THEN
                CB%EOF      = FALSE
                CB%LINE_NUM = LONG_ZER
                !
                CALL CB%BACK%SET(LONG_ZER)
            END IF
    END IF
    !
    IF( CB%IU /= Z ) THEN
        IF( CB%FIRST_POS > ONE ) THEN
                                 P = CB%FIRST_POS - ONE
                                 READ(CB%IU, POS=P) TXT
        ELSE
            REWIND(CB%IU, IOSTAT=P)
        END IF
    END IF
  END SUBROUTINE 
  !
  !#########################################################################################################################
  !#########################################################################################################################
  !#########################################################################################################################
  !  
  PURE FUNCTION BUFFERED_READER_PROPER_DIM(siz) RESULT(DIM)
    INTEGER, INTENT(IN):: siz
    INTEGER:: DIM
    !
    IF    ( siz <   ONE ) THEN
                              DIM = initialBufSize
    ELSEIF( siz <= 1024 ) THEN
                              DIM = 1024
    ELSEIF( siz <= 5120 ) THEN
                              DIM = 512*(siz/512) + 512
    ELSE
                              DIM = 1024*(siz/1024) + 1024
    END IF
    !
  END FUNCTION
  !
  !--------------------------------------------------------------------------------------------------------------------------
  !
  PURE SUBROUTINE INCREASE_BUF_BUFFERED_READER(CB)
    CLASS(BUFFERED_READER), INTENT(INOUT):: CB
    INTEGER:: siz
    !
    siz = CB%DIM
    IF    (siz <  4096 ) THEN
                              siz = 1024*(siz/1024) + 1024
    ELSEIF(siz < 65536 ) THEN
                              siz = 2*siz
    ELSE
                              siz = 16384*(siz/16384) + 16384
    END IF
    !
    CALL GROW_BUFFERED_READER(CB, siz)
    !
  END SUBROUTINE 
  !
  !--------------------------------------------------------------------------------------------------------------------------
  !
  PURE SUBROUTINE GROW_BUFFERED_READER(CB, SIZ)
    CLASS(BUFFERED_READER), INTENT(INOUT):: CB
    INTEGER,                INTENT(IN   ):: SIZ
    INTEGER:: I
    !
    IF( CB%DIM < SIZ ) THEN
          IF( CB%DIM < ONE ) THEN
                             NULLIFY( CB%BUF  )
                             NULLIFY( CB%LINE )
                             ALLOCATE(CHARACTER(SIZ):: CB%BUF)
          ELSE
              CB%LINE => CB%BUF
              !
              NULLIFY( CB%BUF )
              ALLOCATE(CHARACTER(SIZ):: CB%BUF)
              !
              DO I=ONE, CB%DIM
                      CB%BUF(I:I) = CB%LINE(I:I)
              END DO
              !
              DEALLOCATE( CB%LINE )
              NULLIFY   ( CB%LINE )
          END IF
          !
          I = CB%DIM + ONE
          CALL SET_TO_BLANK(I, SIZ, CB%BUF)
          !
          CB%DIM = SIZ
          !
          CALL SET_LINE_POINTER_BUFFERED_READER(CB)
    END IF
    !
  END SUBROUTINE 
  !
  !--------------------------------------------------------------------------------------------------------------------------
  !
  SUBROUTINE COUNT_LINES_BUFFERED_READER(CB)
    CLASS(BUFFERED_READER), INTENT(INOUT):: CB
    CHARACTER(4096):: TMP                                 ! Count lines in 4KB chunks
    INTEGER:: I, EOF, P
    INTEGER:: LF0, LF1, SIZ
    !
    CB%MAX_LINE_SIZE = Z
    !
    IF ( CB%IU == Z ) THEN
         CB%FILE_SIZE = LONG_ZER
         CB%NLINE     = LONG_ZER
    ELSE
       CB%NLINE = LONG_ZER
       !
       INQUIRE(CB%IU, SIZE=CB%FILE_SIZE)                  ! Update File Size
       !
       IF( CB%FILE_SIZE > LONG_ZER) THEN
           REWIND(CB%IU, IOSTAT=I)                        ! Move to start of file to begin counting.
           P   = ONE                                      ! Keep track of position for last read
           LF0 = ONE
           LF1 = Z
           EOF = Z
           DO WHILE ( EOF == Z )
              READ(CB%IU, IOSTAT=EOF) TMP
              !
              IF    ( EOF > Z ) THEN                      ! Unknown Error
                                     EXIT
              ELSEIF( EOF < Z ) THEN                      ! -> Imcomplete read of TMP, so there is garbage at the end.
                                     TMP = ""             !    Clear TMP and reread it
                                     READ(CB%IU, POS=P, IOSTAT=EOF) TMP
              ELSE
                                     P = P + 4096
              END IF
              !
              DO I=1, 4096
                 LF1 = LF1 + ONE 
                 IF(TMP(I:I) == LF) THEN
                                    CB%NLINE = CB%NLINE + LONG_ONE
                                    !
                                    SIZ = LF1 - LF0 + TWO  ! Size of line including LF plus 1
                                    LF0 = LF1 + ONE
                                    IF(SIZ > CB%MAX_LINE_SIZE) CB%MAX_LINE_SIZE = SIZ
                 END IF
              END DO
           END DO
           !
           IF(EOF < ONE) THEN
                       CB%NLINE = CB%NLINE + LONG_ONE
                       INQUIRE( CB%IU, POS=LF1 )
                       SIZ = LF1 - LF0 + TWO
                       IF(SIZ > CB%MAX_LINE_SIZE) CB%MAX_LINE_SIZE = SIZ
           ELSE
                       CB%NLINE = LONG_ZER
                       CALL CB%ERROR_MSG('COUNT_LINES_BUFFERED_READER(): Unknown error reading a line from the file "'//CB%FILE_NAME//'"'//NL// &
                                         '                               The fortran error status code is: '//INT2STR(EOF) )
           END IF
           !
           CALL REWIND_BUFFERED_READER(CB, FALSE)
       END IF
    END IF
    !
  END SUBROUTINE
  !
  !--------------------------------------------------------------------------------------------------------------------------
  !
  PURE SUBROUTINE SET_LINE_POINTER_BUFFERED_READER(CB, P1, P2)
    CLASS(BUFFERED_READER), INTENT(INOUT):: CB
    INTEGER,      OPTIONAL, INTENT(IN   ):: P1, P2
    !
    IF(PRESENT(P1)) CB%p1 = P1
    IF(PRESENT(P2)) CB%p2 = P2
    !
    IF( CB%DIM > Z ) THEN
        IF    ( CB%p1 < ONE .AND. CB%r > CB%p2 ) THEN
                                                 CB%LINE => CB%NULL_SPACE
        ELSEIF( CB%p2 < ONE ) THEN
                                                 CB%LINE => CB%NULL_SPACE
        ELSEIF( CB%p1 < ONE ) THEN
                                                 CB%LINE => CB%BUF(CB%r:CB%p2)
        ELSEIF(    Z <  CB%p1 .AND. CB%p1 <= CB%DIM .AND. &
               CB%p1 <= CB%p2 .AND. CB%p2 <= CB%DIM        ) THEN
                               CB%LINE => CB%BUF(CB%p1:CB%p2)
        ELSE
            ERROR STOP "SET_LINE_POINTER_BUFFERED_READER() PROGRAMMING ERROR"
        END IF
    ELSEIF( ASSOCIATED(CB%NULL_SPACE) ) THEN
            CB%LINE => CB%NULL_SPACE
    END IF
    !
  END SUBROUTINE 
  !
  !#########################################################################################################################
  !#########################################################################################################################
  !#########################################################################################################################
  !
  ! Support Subroutines
  !    
  SUBROUTINE UPDATE_POSw_W_BUFFERED_READER(CB)
    CLASS(BUFFERED_READER), INTENT(INOUT):: CB
    !
    INQUIRE( CB%IU, POS=CB%POSw )  ! Byte Position at end of BUF
    !
    CB%w = int(CB%POSw - CB%POSr, i32) + ONE
    !
    IF( CB%r > ONE ) CB%w = CB%w + CB%r + NEG
    !
  END SUBROUTINE 
  !
  !--------------------------------------------------------------------------------------------------------------------------
  !
  PURE SUBROUTINE UPDATE_BACK_BUFFERED_READER(CB)
    CLASS(BUFFERED_READER), INTENT(INOUT):: CB
    INTEGER(i64):: POSnl
    !
    IF ( CB%r < TWO ) THEN
                      POSnl = CB%POSr + INT( CB%nextNL - ONE, i64 )
    ELSE
                      POSnl = CB%POSr + INT( CB%nextNL - CB%r, i64 )
    END IF
    !
    IF(POSnl < LONG_ZER    ) POSnl = LONG_ZER
    IF(POSnl > CB%FILE_SIZE) POSnl = CB%FILE_SIZE + LONG_ONE
    !
    CALL CB%BACK%SORT_PREPEND_ROLL(POSnl, LONG_NEG, FALSE)
    !
  END SUBROUTINE 
  !
  !--------------------------------------------------------------------------------------------------------------------------
  !
  PURE SUBROUTINE SHIFT_r_TO_ONE_BUFFERED_READER(CB)
    CLASS(BUFFERED_READER), INTENT(INOUT):: CB
    INTEGER:: I, P
    !
    IF( CB%r > ONE ) THEN             ! Shift buffer to drop previously read data
        P = ONE
        DO I=CB%r, CB%w - ONE
                      CB%BUF(P:P) = CB%BUF(I:I)
                      P = P + ONE
        END DO
        !
        CB%p1 = CB%p1 - CB%r + ONE
        CB%p2 = CB%p2 - CB%r + ONE
        !
        CB%nextNL = CB%nextNL - CB%r + ONE
        CB%r      = ONE
        CB%w      = P
        !
        CALL SET_LINE_POINTER_BUFFERED_READER(CB)
    END IF
    !
  END SUBROUTINE
  !
  !--------------------------------------------------------------------------------------------------------------------------
  !
  PURE SUBROUTINE CHECK_nextNL_BUFFERED_READER(CB) ! Only call if CB%DIM > 0
    CLASS(BUFFERED_READER), INTENT(INOUT):: CB
    LOGICAL:: SEARCH
    !
    IF(CB%nextNL <= CB%DIM) THEN                   ! Already at the End of File
       SEARCH = FALSE
       IF    ( CB%POSr > CB%FILE_SIZE ) THEN       ! End of File Check
                                        CB%nextNL = CB%DIM + ONE
       ELSEIF( Z < CB%nextNL .AND. CB%r <= CB%nextNL ) THEN       ! Does it point to New Line Marker
           SEARCH = CB%BUF(CB%nextNL:CB%nextNL) /= LF
       ELSE
           SEARCH = TRUE
       END IF
       !
       IF(SEARCH) CALL FIND_LF(CB%r, CB%DIM, CB%BUF, CB%nextNL)
    END IF
    !
  END SUBROUTINE
  !
  !--------------------------------------------------------------------------------------------------------------------------
  !
  PURE SUBROUTINE FIND_LF(Pin, N, LN, nextNL)
    INTEGER,      INTENT(IN ):: Pin      ! Pin is stating pointer, and updated with position at LF
    INTEGER,      INTENT(IN ):: N        ! N is max dimension
    CHARACTER(N), INTENT(IN ):: LN
    INTEGER,      INTENT(OUT):: nextNL   ! Location of LF, set to -1 if not found
    INTEGER:: P, I
    !
    P = Pin
    IF(      P < ONE ) P = ONE
    IF(LN(P:P) == LF ) P = P + ONE
    !
    nextNL = NEG
    IF ( P <= N ) THEN
                  DO I=P, N
                         IF( LN(I:I) == LF ) THEN
                                             nextNL = I
                                             EXIT
                         END IF
                  END DO
    END IF
    !
  END SUBROUTINE
  !
  !--------------------------------------------------------------------------------------------------------------------------
  !
  PURE SUBROUTINE FIND_COM(P, N, LN, COM, LOC)
    INTEGER,      INTENT(IN   ):: P, N    ! P is stating pointer, N is max dimension
    CHARACTER(N), INTENT(INOUT):: LN
    CHARACTER(*), INTENT(IN   ):: COM
    INTEGER,      INTENT(INOUT):: LOC
    INTEGER:: I
    !
    LOC = NEG
    IF( COM /= " " .AND. P <= N ) THEN
        IF( P > Z) THEN
            I = P - ONE
        ELSE
            I = ONE
        END IF
        !
        IF( COM == "#" ) THEN
            DO WHILE (I < N)
                      I = I + ONE
                      IF( LN(I:I) == LF  ) EXIT
                      IF( LN(I:I) == "#" ) THEN
                                           LOC = I
                                           EXIT
                      END IF
                      IF( LN(I:I) == "'" ) CALL NEXT_QUOTE(I, N, LN, "'")  ! Assumes line has matching quote - Otherwise nothing happens
                      IF( LN(I:I) == '"' ) CALL NEXT_QUOTE(I, N, LN, '"')  ! Assumes line has matching quote - Otherwise nothing happens
            END DO
        ELSE
            DO WHILE (I < N)
                      I = I + ONE
                      IF( LN(I:I) == LF  ) EXIT
                      IF( INDEX(COM, LN(I:I)) > Z ) THEN
                                                    LOC = I
                                                    EXIT
                      END IF
                      IF( LN(I:I) == "'" ) CALL NEXT_QUOTE(I, N, LN, "'")  ! Assumes line has matching quote - Otherwise nothing happens
                      IF( LN(I:I) == '"' ) CALL NEXT_QUOTE(I, N, LN, '"')  ! Assumes line has matching quote - Otherwise nothing happens
            END DO
        END IF
    END IF
    !
  END SUBROUTINE 
  !
  !--------------------------------------------------------------------------------------------------------------------------
  !
  PURE SUBROUTINE STRIP_CR(P, N, LN)
    INTEGER,      INTENT(IN   ):: P, N    ! P is stating pointer, N is max dimension
    CHARACTER(*), INTENT(INOUT):: LN
    INTEGER:: I
    !
    IF (Z < P .AND. P <= N ) THEN
                 DO I=P, N
                        IF( LN(I:I) == CR ) LN(I:I) = " "
                 END DO
    END IF
    !
  END SUBROUTINE 
  !
  !--------------------------------------------------------------------------------------------------------------------------
  !
  PURE SUBROUTINE STRIP_TAB(P, N, LN)
    INTEGER,      INTENT(IN   ):: P, N    ! P is stating pointer, N is max dimension
    CHARACTER(N), INTENT(INOUT):: LN
    INTEGER:: I
    !
    IF (Z < P .AND. P <= N ) THEN
                 I = P - ONE
                 DO WHILE (I < N)
                           I = I + ONE
                           IF( LN(I:I) == TAB ) LN(I:I) = " "
                           IF( LN(I:I) == "'" ) CALL NEXT_QUOTE(I, N, LN, "'")  ! Assumes line has matching quote - Otherwise nothing happens
                           IF( LN(I:I) == '"' ) CALL NEXT_QUOTE(I, N, LN, '"')  ! Assumes line has matching quote - Otherwise nothing happens
                 END DO
    END IF
    !
  END SUBROUTINE 
  !
  !--------------------------------------------------------------------------------------------------------------------------
  !
  PURE SUBROUTINE STRIP_COMMA(P, N, LN)
    INTEGER,      INTENT(IN   ):: P, N    ! P is stating pointer, N is max dimension
    CHARACTER(N), INTENT(INOUT):: LN
    INTEGER:: I
    !
    IF (Z < P .AND. P <= N ) THEN
                 I = P - ONE
                 DO WHILE (I < N)
                           I = I + ONE
                           IF( LN(I:I) == "," ) LN(I:I) = " "
                           IF( LN(I:I) == "'" ) CALL NEXT_QUOTE(I, N, LN, "'")  ! Assumes line has matching quote - Otherwise nothing happens
                           IF( LN(I:I) == '"' ) CALL NEXT_QUOTE(I, N, LN, '"')  ! Assumes line has matching quote - Otherwise nothing happens
                 END DO
    END IF
    !
  END SUBROUTINE 
  !
  !--------------------------------------------------------------------------------------------------------------------------
  !
  PURE SUBROUTINE NEXT_QUOTE(P, N, LN, QT)
    INTEGER,      INTENT(INOUT):: P
    INTEGER,      INTENT(IN   ):: N    ! P is stating pointer, N is max dimension
    CHARACTER(N), INTENT(INOUT):: LN
    CHARACTER,    INTENT(IN   ):: QT  ! = ' or "
    !
    IF( P < ONE ) P = ONE
    IF( LN(P:P) == QT ) P = P + ONE
    !
    DO WHILE ( P < N )
              IF( LN(P:P) == QT ) EXIT
               P = P + ONE
    END DO
    !
    IF( LN(P:P) == QT ) THEN
                        P = P + ONE
    ELSE
                        P = N + TEN        ! Did not find matching QT
    END IF
    !
  END SUBROUTINE 
  !
  !--------------------------------------------------------------------------------------------------------------------------
  !
  PURE SUBROUTINE SET_TO_BLANK(P, N, LN)
    INTEGER,      INTENT(IN   ):: P, N    ! P is stating pointer, N is max dimension
    CHARACTER(N), INTENT(INOUT):: LN
    INTEGER:: I
    !
    IF (Z < P .AND. P <= N ) THEN
                 DO I=P, N
                        LN(I:I) = " "
                 END DO
    END IF
    !
  END SUBROUTINE
  !
  !--------------------------------------------------------------------------------------------------------------------------
  !
  PURE SUBROUTINE ERROR_MSG_BUFFERED_READER(CB, MSG)
    CLASS(BUFFERED_READER), INTENT(INOUT):: CB
    CHARACTER(*), OPTIONAL, INTENT(IN   ):: MSG
    !
    CB%ERROR = TRUE
    !
    IF(PRESENT(MSG)) THEN
       IF(ALLOCATED( CB%MSG )) THEN
                               CB%MSG = CB%MSG//BLN//MSG
       ELSE
                               CB%MSG = MSG
       END IF
    END IF
    !
  END SUBROUTINE 
  !
  !--------------------------------------------------------------------------------------------------------------------------
  !
!!!  PURE SUBROUTINE FIND_NON_BLANK(LOC, N, LN)  ! NL is considered a non-blank
!!!    INTEGER,      INTENT(INOUT):: LOC
!!!    INTEGER,      INTENT(IN   ):: N
!!!    CHARACTER(N), INTENT(IN   ):: LN
!!!    !
!!!    DO WHILE( LOC <= N ) 
!!!                    IF(LN(LOC:LOC) /= ' ') EXIT
!!!                    LOC = LOC + ONE
!!!    END DO
!!!    IF(LOC > N) LOC = NEG                         ! Entire Line is Blank
!!!    !
!!!  END SUBROUTINE
!!!  !
!!!  !--------------------------------------------------------------------------------------------------------------------------
!!!  !
!!!  PURE SUBROUTINE GO_TO_LF(LOC, N, LN)  ! NL is considered a non-blank
!!!    INTEGER,      INTENT(INOUT):: LOC         ! Set to Z to include start of line, otherwise do LOC-1 to use the location LOC as starting point
!!!    INTEGER,      INTENT(IN   ):: N
!!!    CHARACTER(N), INTENT(IN   ):: LN
!!!    !
!!!    DO WHILE( LOC <= N ) 
!!!                    IF( LN(LOC:LOC) == LF ) EXIT
!!!                    LOC = LOC + ONE
!!!    END DO
!!!    IF(LOC > N) LOC = NEG                         ! NO LF FOUND
!!!    !
!!!  END SUBROUTINE
  !
  !#########################################################################################################################
  !#########################################################################################################################
  !#########################################################################################################################
  !
  ! RESET AND FINAL Subroutines
  !
  SUBROUTINE FINAL_BUFFERED_READER(CB)           ! IMPURE ELEMENTAL 
    TYPE(BUFFERED_READER), INTENT(INOUT):: CB
    CALL DESTROY_BUFFERED_READER(CB)
  END SUBROUTINE 
  !
  !--------------------------------------------------------------------------------------------------------------------------
  !
  SUBROUTINE DESTROY_BUFFERED_READER(CB)
    CLASS(BUFFERED_READER), INTENT(INOUT):: CB
    INTEGER:: ERR
    !
    CALL  RESET_BASE_VARIABLES_BUFFERED_READER(CB)
    CALL   RESET_POS_VARIABLES_BUFFERED_READER(CB)
    CALL RESET_ERROR_VARIABLES_BUFFERED_READER(CB)
    !
    CALL DESTROY_BUF_BUFFERED_READER(CB)
    CB%IU  = Z
    !
    CB%CLOSE_IU = FALSE
    !
    DEALLOCATE( CB%NULL_SPACE, STAT=ERR )
    NULLIFY(CB%NULL_SPACE)
    !
  END SUBROUTINE 
  !
  !--------------------------------------------------------------------------------------------------------------------------
  !
  PURE SUBROUTINE DESTROY_BUF_BUFFERED_READER(CB)
    CLASS(BUFFERED_READER), INTENT(INOUT):: CB
    INTEGER:: ERR
    !
    NULLIFY( CB%LINE )
    !
    IF( CB%DIM > Z) THEN
                      DEALLOCATE( CB%BUF, STAT=ERR )
                      NULLIFY   ( CB%BUF  )
    END IF
    CB%DIM = Z
  END SUBROUTINE 
  !
  !--------------------------------------------------------------------------------------------------------------------------
  !
  SUBROUTINE RESET_BASE_VARIABLES_BUFFERED_READER(CB)
    CLASS(BUFFERED_READER), INTENT(INOUT):: CB
    INTEGER:: ERR
    !
    IF( CB%IU /= Z  .AND. CB%CLOSE_IU) CLOSE( CB%IU, IOSTAT=ERR )
    !
    CB%FILE_NAME = "FILE_NOT_DEFINED"
    !
    CB%FILE_SIZE = LONG_ZER
    !
    CB%LINE_NUM  = LONG_ZER
    CB%NLINE     = LONG_ZER
    !
    CB%MAX_LINE_SIZE = Z
    !
    CB%FIRST_POS  = ONE      ! Note that a BOM check should update this variable
    !
    CB%IU        = Z
    CB%CLOSE_IU  = FALSE
    !
    CALL SET_LINE_POINTER_BUFFERED_READER(CB, NEG, NEG)  ! P2 = Neg causes NEXT_LINE_BUFFERED_READER to start at begining of file.
    !
  END SUBROUTINE 
  !
  !--------------------------------------------------------------------------------------------------------------------------
  !
  SUBROUTINE RESET_BUF_BUFFERED_READER(CB)
    CLASS(BUFFERED_READER), INTENT(INOUT):: CB
    !
    IF( CB%DIM > Z) THEN
        !
        CB%r = Z
        CB%w = ONE
        CB%nextNL = Z
        !
        CALL SET_TO_BLANK(ONE, CB%DIM, CB%BUF)
        !
        CALL SET_LINE_POINTER_BUFFERED_READER(CB, NEG, NEG)  ! P2 = Neg causes NEXT_LINE_BUFFERED_READER to start at begining of file.
    END IF
    !
  END SUBROUTINE 
  !
  !--------------------------------------------------------------------------------------------------------------------------
  !
  PURE SUBROUTINE RESET_POS_VARIABLES_BUFFERED_READER(CB)
    CLASS(BUFFERED_READER), INTENT(INOUT):: CB
    !
    CB%POSr      = LONG_ONE   ! Byte Position at current reading point within BUF, viz. -> POSr is the byte position in IU for BUF(r:r)  => If r < 1, then POSr=1 to indicate the first byte is referenced --> CB%POS0 + CB%r - ONE 
    CB%POSw      = LONG_ONE   ! Byte Position at w (typically the end of BUF),     viz. -> POSw is the byte position in IU for BUF(w:w)
    !
  END SUBROUTINE 
  !
  !--------------------------------------------------------------------------------------------------------------------------
  !
  PURE SUBROUTINE RESET_ERROR_VARIABLES_BUFFERED_READER(CB)
    CLASS(BUFFERED_READER), INTENT(INOUT):: CB
    !
    CB%ERROR = FALSE
    !
    IF(ALLOCATED( CB%MSG )) DEALLOCATE( CB%MSG )
    !
  END SUBROUTINE
  !
  !#########################################################################################################################
  !#########################################################################################################################
  !#########################################################################################################################
  !
  !  Read Utilities
  !
  SUBROUTINE READ_TO_DATA_ECHO_LINE(CB, LINE, OUTPUT, EOF, HED)
    CLASS(BUFFERED_READER), INTENT(INOUT):: CB
    CHARACTER(*),           INTENT(INOUT):: LINE
    INTEGER,                INTENT(IN   ):: OUTPUT  !UNIT TO WRITE TRANSCRIPT OF WHAT IS LOADED TOO
    LOGICAL,      OPTIONAL, INTENT(OUT  ):: EOF     !SET TO TRUE IF END OF FILE IS REACHED
    CHARACTER(*), OPTIONAL, INTENT(IN   ):: HED     !IF OUTPUT IS SPECIFIED, THEN WRITE HEADER BEFORE ECHOING COMMENTS OUT.
    !
    CALL READ_TO_DATA_ECHO(CB, OUTPUT, EOF, HED)
    LINE = CB%LINE
    !
  END SUBROUTINE
  !
  !--------------------------------------------------------------------------------------------------------------------------
  !
  SUBROUTINE READ_TO_DATA_EOF_LINE(CB, LINE, EOF)
    CLASS(BUFFERED_READER), INTENT(INOUT):: CB
    CHARACTER(*),           INTENT(INOUT):: LINE
    LOGICAL,      OPTIONAL, INTENT(OUT  ):: EOF     !SET TO TRUE IF END OF FILE IS REACHED
    !
    CALL READ_TO_DATA_EOF(CB, EOF)
    LINE = CB%LINE
    !
  END SUBROUTINE
  !
  !--------------------------------------------------------------------------------------------------------------------------
  !
  SUBROUTINE READ_TO_DATA_ECHO(CB, OUTPUT, EOF, HED)
    CLASS(BUFFERED_READER), INTENT(INOUT):: CB
    INTEGER,                INTENT(IN   ):: OUTPUT  !UNIT TO WRITE TRANSCRIPT OF WHAT IS LOADED TOO
    LOGICAL,      OPTIONAL, INTENT(OUT  ):: EOF     !SET TO TRUE IF END OF FILE IS REACHED
    CHARACTER(*), OPTIONAL, INTENT(IN   ):: HED     !IF OUTPUT IS SPECIFIED, THEN WRITE HEADER BEFORE ECHOING COMMENTS OUT.
    INTEGER:: C, IOUT
    LOGICAL:: FIRST, TRANSCRIBE, POUND_COM, NO_COM
    !
    IF(CB%POSr > CB%FILE_SIZE .OR. CB%ERROR) THEN
                                   IF(PRESENT(EOF)) EOF=TRUE
                                   CALL END_OF_BUF_AND_FILE_BUFFERED_READER(CB)
                                   RETURN
    END IF
    !
    FIRST = TRUE
    !
    IOUT=OUTPUT
    !
    IF(PRESENT(EOF)) EOF=FALSE
    !
    NO_COM    = CB%COM == " "
    POUND_COM = CB%COM == "#"
    !
    TRANSCRIBE = IOUT /= Z
    !
    READ_LOOP: DO
          CALL NEXT_LINE_BUFFERED_READER(CB)
          IF    (CB%POSr > CB%FILE_SIZE .OR. CB%ERROR) THEN                                    !  At EOF and BUF is empty or Error Occurred
                                                       IF(PRESENT(EOF)) EOF = TRUE
                                                       CALL END_OF_BUF_AND_FILE_BUFFERED_READER(CB)
                                                       RETURN
          END IF
          ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          !
          CALL FIND_COM(CB%r, CB%DIM, CB%BUF, CB%COM, C)
          C = C - ONE
          !
          IF (C  < Z) C=LEN_TRIM(CB%LINE)      ! NO # FOUND, SO USE ENTIRE STRING
          IF (C == Z) C=ONE                    ! # IS ON FIRST COLUMN OR LINE IS BLANK, SET TO 1
          !
          ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          !
          IF    (   NO_COM) THEN
                            IF(CB%LINE(1:C) /= " ") EXIT READ_LOOP
          ELSEIF(POUND_COM) THEN
                            IF(CB%LINE(C:C) /= '#' .AND. CB%LINE(1:C) /= " ") EXIT READ_LOOP   !Start of line is not COM and all char before COM are blank
          ELSE
                            IF(INDEX(CB%COM, CB%LINE(C:C)) == Z .AND. CB%LINE(1:C) /= " ") EXIT READ_LOOP  
          END IF
          !
          IF(TRANSCRIBE) THEN
                IF(FIRST) THEN
                              IF( PRESENT(HED) ) WRITE(IOUT,'(/ A)') HED
                              WRITE(IOUT,'(/ A)') TRIM(CB%LINE)
                              FIRST = FALSE
                ELSE
                              WRITE(IOUT,'(  A)') TRIM(CB%LINE)
                END IF
          END IF
          !
    END DO READ_LOOP
    !
    IF(.NOT. FIRST) WRITE(IOUT,'(A)')
    !
    IF(PRESENT(EOF)) EOF = CB%EOF
    !
  END SUBROUTINE
  !
  !--------------------------------------------------------------------------------------------------------------------------
  !
  SUBROUTINE READ_TO_DATA_EOF(CB, EOF)
    CLASS(BUFFERED_READER), INTENT(INOUT):: CB
    LOGICAL,      OPTIONAL, INTENT(OUT  ):: EOF     !SET TO TRUE IF END OF FILE IS REACHED
    INTEGER:: C
    LOGICAL:: POUND_COM, NO_COM
    !
    IF(CB%POSr > CB%FILE_SIZE .OR. CB%ERROR) THEN
                                   IF(PRESENT(EOF)) EOF=TRUE
                                   CALL END_OF_BUF_AND_FILE_BUFFERED_READER(CB)
                                   RETURN
    END IF
    !
    NO_COM    = CB%COM == " "
    POUND_COM = CB%COM == "#"
    !
    READ_LOOP: DO
          CALL NEXT_LINE_BUFFERED_READER(CB)
          IF    (CB%POSr > CB%FILE_SIZE .OR. CB%ERROR) THEN                                    !  At EOF and BUF is empty or Error Occurred
                                                       IF(PRESENT(EOF)) EOF = TRUE
                                                       CALL END_OF_BUF_AND_FILE_BUFFERED_READER(CB)
                                                       RETURN
          END IF
          ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          !
          CALL FIND_COM(CB%r, CB%DIM, CB%BUF, CB%COM, C)
          C = C - ONE
          !
          IF (C  < Z) C=LEN_TRIM(CB%LINE)      ! NO # FOUND, SO USE ENTIRE STRING
          IF (C == Z) C=ONE                    ! # IS ON FIRST COLUMN OR LINE IS BLANK, SET TO 1
          !
          ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          !
          IF    (   NO_COM) THEN
                            IF(CB%LINE(1:C) /= " ") EXIT READ_LOOP
          ELSEIF(POUND_COM) THEN
                            IF(CB%LINE(C:C) /= '#' .AND. CB%LINE(1:C) /= " ") EXIT READ_LOOP   !Start of line is not COM and all char before COM are blank
          ELSE
                            IF(INDEX(CB%COM, CB%LINE(C:C)) == Z .AND. CB%LINE(1:C) /= " ") EXIT READ_LOOP  
          END IF
          !
    END DO READ_LOOP
    !
    IF(PRESENT(EOF)) EOF = CB%EOF
    !
  END SUBROUTINE
  !
  !--------------------------------------------------------------------------------------------------------------------------
  !
  !@PURE SUBROUTINE PARSE_WORD(CB, UPCASE, IGNORE_LF)
  !@  CLASS(BUFFERED_READER), INTENT(INOUT):: CB
  !@  LOGICAL,      OPTIONAL, INTENT(IN   ):: UPCASE, IGNORE_LF
  !@  INTEGER:: N, LOC, ISTART, ISTOP, ICOM
  !@  LOGICAL:: CHECK, CHECK_COM, POUND_COM, LF_IS_BLANK
  !@  !
  !@  IF( CB%ERROR .OR. CB%POS0 > CB%FILE_SIZE ) THEN 
  !@      CB%LINE => CB%NULL_SPACE
  !@      RETURN
  !@  END IF
  !@  !
  !@  CHECK_COM = CB%COM /= " "
  !@  POUND_COM = CB%COM == "#"
  !@  !
  !@  LF_IS_BLANK = FALSE
  !@  IF(PRESENT(IGNORE_LF)) LF_IS_BLANK = IGNORE_LF
  !@  !
  !@  CALL CHECK_nextNL_BUFFERED_READER(CB)
  !@  IF( CB%nextNL < ONE ) CALL FILL_BUF_BUFFERED_READER(CB)               ! Increase BUF until a NL is found 
  !@  !
  !@  IF(CB%POS0 > CB%FILE_SIZE) RETURN  ! Nothing left in File or BUF
  !@  !
  !@  LOC = CB%r
  !@  !
  !@  CALL FIND_NON_BLANK(LOC, CB%DIM, CB%BUF)
  !@  !
  !@  IF(LOC < ONE) THEN  ! Rest of line is blank - Should not happen unless at EOF
  !@           CALL SET_LINE_POINTER_BUFFERED_READER(CB, NEG, NEG)
  !@           RETURN
  !@  END IF
  !@  !
  !@  DO
  !@      
  !@      
  !@  END DO
  !@  
  !@  
  !@  
  !@  IF(LF_IS_BLANK .AND. CB%BUF(LOC:LOC) == LF ) THEN
  !@      DO WHILE (CB%BUF(LOC:LOC) == LF)
  !@         CB%r = LOC+ONE
  !@         CALL FILL_BUF_BUFFERED_READER(CB)
  !@         LOC = CB%r
  !@         CALL FIND_NON_BLANK(LOC, CB%DIM, CB%BUF)
  !@      END DO
  !@  END IF
  !@  !
  !@  IF(CHECK_COM) THEN
  !@      CHECK = FALSE
  !@      IF( POUND_COM ) THEN
  !@                      CHECK = CB%BUF(LOC:LOC) == "#"
  !@      ELSE
  !@                      CHECK = INDEX( CB%COM, CB%BUF(LOC:LOC) ) > Z
  !@      END IF
  !@      IF(CHECK) THEN
  !@                LOC = CB%nextNL
  !@                IF(LOC > CB%DIM) THEN
  !@                         CALL END_OF_BUF_AND_FILE_BUFFERED_READER(CB)
  !@                         RETURN
  !@                END IF
  !@      END IF
  !@  END IF
  !@  !
  !@  
  !@  !!! N = CB%nextNL - ONE
  !@  !!! IF( N > CB%DIM) THEN
  !@  !!!                 N = CB%DIM + ONE
  !@  !!! ELSE
  !@  !!!                 N = LEN_TRIM(CB%BUF(ONE:N)) + ONE
  !@  !!! END IF
  !@  !!! !
  !@  !!! LOC = CB%r
  !@  !!! !
  !@  !!! DO WHILE( LOC < N ) 
  !@  !!!                 IF(CB%BUF(LOC:LOC) /= TAB .AND. CB%BUF(LOC:LOC) /= ' ' .AND. CB%BUF(LOC:LOC) /= ',' .AND. CB%BUF(LOC:LOC) /= CHK) EXIT
  !@  !!!                 LOC = LOC + ONE
  !@  !!! END DO
  !@  !
  !@  IF( LOC == N ) THEN
  !@  !
  !@  IF( LOC >= N ) THEN
  !@             LINE_LEN = LEN(LN)
  !@      LOC   =LINE_LEN+1
  !@      ISTART=LINE_LEN
  !@      ISTOP =LINE_LEN-1
  !@      IF(PRESENT(EOL)) EOL = .TRUE.
  !@  ELSE
  !@      IF(CB%BUF(LOC:LOC)=='"') THEN
  !@                              LOC = LOC+1
  !@                              ISTART = LOC
  !@                              DO WHILE( LOC < N )
  !@                                  IF( CB%BUF(LOC:LOC) == '"' ) EXIT
  !@                                  LOC = LOC+1
  !@                              END DO
  !@                              ISTOP = LOC-1
  !@                              LOC = LOC+1
  !@      ELSEIF(CB%BUF(LOC:LOC)=="'") THEN
  !@                              LOC = LOC+1
  !@                              ISTART = LOC
  !@                              DO WHILE( LOC < N )
  !@                                  IF( CB%BUF(LOC:LOC) == "'" ) EXIT
  !@                                  LOC = LOC+1
  !@                              END DO
  !@                              ISTOP = LOC-1
  !@                              LOC = LOC+1
  !@          
  !@      ELSE
  !@                              ISTART = LOC
  !@                              LOC = LOC+1
  !@                              DO WHILE( LOC < N )
  !@                                  IF( CB%BUF(LOC:LOC)==TAB .OR. CB%BUF(LOC:LOC)==' ' .OR. CB%BUF(LOC:LOC)==',') EXIT
  !@                                  LOC = LOC+1
  !@                              END DO
  !@                              ISTOP = LOC-1
  !@                              IF(ISTOP<ISTART) ISTOP=ISTART
  !@      END IF
  !@      !
  !@  END IF
  !@  !
  !@  IF(PRESENT(OLD_LOC)) OLD_LOC = LOC0
  !@  !
  !@END SUBROUTINE
  !!
  !!--------------------------------------------------------------------------------------------------------------------------
  !!
  !SUBROUTINE GET_INTEGER_VAL(CB,LOC,ISTART,ISTOP,IOUT,IN,VAL,NO_PARSE_WORD,MSG, ERROR_VAL, IOSTAT)  !IF NO_PARSE_WORD=TRUE, THEN ASSUMES ISTART AND ISTOP ARE ALREADY SET TO CORRECT LOCATION
  !  CLASS(BUFFERED_READER), INTENT(INOUT):: CB
  !  CHARACTER(*),           INTENT(IN   ):: LN                ! LINE TO PARSE DOUBLE FROM
  !  INTEGER,                INTENT(INOUT):: LOC,ISTART,ISTOP  ! LOC => STARTING LOCATION TO FIND NUMBER, NUMBER AT EXIT IS LOCATED AT LN(ISTART:ISTOP) AND LOC = ISTOP+1
  !  INTEGER,                INTENT(IN   ):: IOUT, IN          ! ERROR UNIT TO WRITE TO, FILE UNIT THAT LN ORIGINATED FROM
  !  INTEGER,                INTENT(OUT  ):: VAL               ! DOUBLE VALUE TO RETURN
  !  LOGICAL,      OPTIONAL, INTENT(IN   ):: NO_PARSE_WORD     ! INDCIATES THAT ISTART AND ISTOP ALREADY POINT TO NUMBER
  !  CHARACTER(*), OPTIONAL, INTENT(IN   ):: MSG               ! ERROR MESSAGE PRINTED WHEN FAIL TO LOAD NUMBER AND STOP PROGRAM, IF "NOSTOP" THEN VAL IS SET TO inf_I = HUGE(VAL)
  !  INTEGER,      OPTIONAL, INTENT(IN   ):: ERROR_VAL         ! IF FAIL TO LOAD NUMBER, DO NOT STOP, BUT INSTEAD SET VAL =  ERROR_VAL
  !  LOGICAL,      OPTIONAL, INTENT(  OUT):: HAS_ERROR         ! SET TO TRUE IF THERE IS AN ERROR
  !  INTEGER,      OPTIONAL, INTENT(  OUT):: IOSTAT            ! SET to zero if there is no error, otherwise non-zero error code
  !  LOGICAL:: CHECK
  !  INTEGER:: IERR
  !  !
  !  CALL PARSE_WORD(LN,LOC,ISTART,ISTOP)
  !  !
  !  READ(LN(ISTART:ISTOP),*, IOSTAT=IERR) VAL
  !  !
  !  IF(IERR /= Z .OR. LN(ISTART:ISTOP)==BLNK) THEN
  !        CHECK = TRUE
  !        IF(PRESENT(ERROR_VAL)) THEN
  !                 !
  !                 CHECK = FALSE
  !                 VAL = ERROR_VAL
  !                 !
  !        ELSEIF(PRESENT(MSG)) THEN
  !             !
  !             IF (MSG=='NOSTOP') THEN
  !                 CHECK = FALSE
  !                 VAL = inf_I  !SET VAL TO HUGE(I) WHEN THERE IS NOSTOP OPTION. NOTE NAN DOES NOT EXIST FOR INT
  !             END IF
  !             IF(PRESENT(HAS_ERROR)) HAS_ERROR = TRUE
  !             !
  !        ELSEIF(PRESENT(IOSTAT)) THEN
  !             !
  !             IF(IERR == Z .AND. LN(ISTART:ISTOP)==BLNK) THEN
  !                                                        IOSTAT = -1
  !             ELSE
  !                                                        IOSTAT = IERR
  !             END IF
  !             CHECK  = FALSE
  !             VAL = inf_I  !SET VAL TO HUGE(I) WHEN THERE IS NOSTOP OPTION. NOTE NAN DOES NOT EXIST FOR INT
  !             IF(PRESENT(HAS_ERROR)) HAS_ERROR = TRUE
  !        ELSE IF(PRESENT(HAS_ERROR)) THEN
  !             !
  !             HAS_ERROR = TRUE
  !             CHECK  = FALSE
  !             VAL = inf_I  !SET VAL TO HUGE(I) WHEN THERE IS NOSTOP OPTION. NOTE NAN DOES NOT EXIST FOR INT
  !        END IF
  !        !
  !        IF(CHECK) THEN
  !            ERRMSG = 'GET_INTEGER READ UTILITY ERROR: FAILED TO CONVERT TEXT TO INTEGER NUMBER.'//BLN//'THE FOLLOWING IS THE TEXT ATTEMPTED TO BE CONVERTED "'//LN(ISTART:ISTOP)//'".'
  !            IF(LN(ISTART:ISTOP)==BLNK) ERRMSG = ERRMSG//BLN//'THE POSSIBLE REASON FOR THIS ERROR IS DUE TO READING IN A BLANK/EMPTY LINE OR YOU DID NOT PROVIDE ENOUGH NUMBERS ON THE LINE.'
  !            IF(PRESENT(MSG)) ERRMSG = ERRMSG//BLN//'THE FOLLOWING IS AN ADDITIONAL COMMENT PASSED TO GET_INTEGER:'//BLN//TRIM(MSG)
  !            !
  !            CALL STOP_ERROR(LN, IN, IOUT, ERRMSG )
  !        END IF
  !  END IF
  !  !
  !END SUBROUTINE
  !
  !--------------------------------------------------------------------------------------------------------------------------
  !
  !!!SUBROUTINE GET_INTEGER_VEC(LN,LOC,ISTART,ISTOP,IOUT,IN,VAL,MSG,ERROR_VAL, HAS_ERROR, IOSTAT)
  !!!  CHARACTER(*),                  INTENT(IN   ):: LN                ! LINE TO PARSE DOUBLE FROM
  !!!  INTEGER,                       INTENT(INOUT):: LOC,ISTART,ISTOP  ! LOC => STARTING LOCATION TO FIND NUMBER, NUMBER AT EXIT IS LOCATED AT LN(ISTART:ISTOP) AND LOC = ISTOP+1
  !!!  INTEGER,                       INTENT(IN   ):: IOUT, IN          ! ERROR UNIT TO WRITE TO, FILE UNIT THAT LN ORIGINATED FROM
  !!!  INTEGER,         DIMENSION(:), INTENT(OUT  ):: VAL               ! DOUBLE VALUE TO RETURN
  !!!  CHARACTER(*),        OPTIONAL, INTENT(IN   ):: MSG               ! ERROR MESSAGE PRINTED WHEN FAIL TO LOAD NUMBER AND STOP PROGRAM, IF "NOSTOP" THEN VAL IS SET TO NaN
  !!!  INTEGER,             OPTIONAL, INTENT(IN   ):: ERROR_VAL         ! IF FAIL TO LOAD NUMBER, DO NOT STOP, BUT INSTEAD SET VAL =  ERROR_VAL
  !!!  LOGICAL,             OPTIONAL, INTENT(  OUT):: HAS_ERROR         ! SET TO TRUE IF THERE IS AN ERROR
  !!!  INTEGER,             OPTIONAL, INTENT(  OUT):: IOSTAT            ! SET to zero if there is no error, otherwise non-zero error code
  !!!  CONTIGUOUS:: VAL
  !!!  LOGICAL:: MUST_STOP
  !!!  INTEGER:: IERR, I
  !!!  CHARACTER(:), ALLOCATABLE:: ERRMSG
  !!!  !
  !!!  IF(PRESENT(HAS_ERROR)) HAS_ERROR = FALSE
  !!!  !
  !!!  MUST_STOP = FALSE
  !!!  VAL = Z
  !!!  !
  !!!  DO I=ONE,SIZE(VAL) 
  !!!            CALL PARSE_WORD(LN,LOC,ISTART,ISTOP)
  !!!            !
  !!!            READ(LN(ISTART:ISTOP),*, IOSTAT=IERR) VAL(I)
  !!!            !
  !!!            IF(IERR /= Z .OR. LN(ISTART:ISTOP)==BLNK) THEN
  !!!                  MUST_STOP = TRUE
  !!!                  IF(PRESENT(ERROR_VAL)) THEN
  !!!                           !
  !!!                           MUST_STOP = FALSE
  !!!                           VAL(I) = ERROR_VAL
  !!!                           !
  !!!                  ELSEIF(PRESENT(MSG)) THEN
  !!!                       !
  !!!                       IF (MSG=='NOSTOP') THEN
  !!!                           MUST_STOP = FALSE
  !!!                           VAL(I) = inf_I  !SET VAL TO HUGE(I) WHEN THERE IS NOSTOP OPTION. NOTE NAN DOES NOT EXIST FOR INT
  !!!                       END IF
  !!!                       IF(PRESENT(HAS_ERROR)) HAS_ERROR = TRUE
  !!!                       !
  !!!                  ELSEIF(PRESENT(IOSTAT)) THEN
  !!!                       !
  !!!                       IF(IERR == Z .AND. LN(ISTART:ISTOP)==BLNK) THEN
  !!!                                                                  IOSTAT = -1
  !!!                       ELSE
  !!!                                                                  IOSTAT = IERR
  !!!                       END IF
  !!!                       MUST_STOP = FALSE
  !!!                       VAL(I) = inf_I  !SET VAL TO HUGE(I) WHEN THERE IS NOSTOP OPTION. NOTE NAN DOES NOT EXIST FOR INT
  !!!                       IF(PRESENT(HAS_ERROR)) HAS_ERROR = TRUE
  !!!                  ELSE IF(PRESENT(HAS_ERROR)) THEN
  !!!                       !
  !!!                       HAS_ERROR = TRUE
  !!!                       MUST_STOP = FALSE
  !!!                       VAL(I) = inf_I  !SET VAL TO HUGE(I) WHEN THERE IS NOSTOP OPTION. NOTE NAN DOES NOT EXIST FOR INT
  !!!                  END IF
  !!!                  !
  !!!                  IF(MUST_STOP) THEN
  !!!                      IERR = SIZE(VAL) 
  !!!                      ERRMSG = 'GET_INTEGER READ UTILITY ERROR: EXPECTED TO READ '//NUM2STR(IERR)//' INTEGERS, BUT FAILED TO CONVERT THE POSITION '//NUM2STR(I)//' TEXT TO INTEGER.'//BLN//'THE FOLLOWING IS THE TEXT ATTEMPTED TO BE CONVERTED "'//LN(ISTART:ISTOP)//'".'//NL//'(IF THIS IS BLANK/EMPTY THEN YOU MAY NOT HAVE ENOUGH NUMBERS SPECIFIED ON THE LINE)'
  !!!                      IF(LN(ISTART:ISTOP)==BLNK) ERRMSG = ERRMSG//BLN//'THE POSSIBLE REASON FOR THIS ERROR IS DUE TO READING IN A BLANK/EMPTY LINE OR YOU DID NOT PROVIDE ENOUGH NUMBERS ON THE LINE.'
  !!!                      IF(PRESENT(MSG)) ERRMSG = ERRMSG//BLN//'THE FOLLOWING IS AN ADDITIONAL COMMENT PASSED TO GET_INTEGER:'//BLN//TRIM(MSG)
  !!!                      !
  !!!                      CALL STOP_ERROR(LN, IN, IOUT, ERRMSG )
  !!!                  END IF
  !!!            END IF
  !!!  END DO
  !!!  !
  !!!END SUBROUTINE
  !
  !
  !#########################################################################################################################
  !#########################################################################################################################
  !#########################################################################################################################
  !
  ! INT2STR Subroutines
  !
  PURE FUNCTION INT2STR_i32(IVAL,PAD,ZPAD) RESULT(INT2STR)
    INTEGER(i32),    INTENT(IN):: IVAL
    INTEGER,OPTIONAL,INTENT(IN):: PAD
    LOGICAL,OPTIONAL,INTENT(IN):: ZPAD
    CHARACTER(:),   ALLOCATABLE:: INT2STR
    CHARACTER(32)::NUM
    !
    WRITE(NUM,'(I32)') IVAL
    !
    NUM=ADJUSTL(NUM)
    IF(PRESENT(PAD)) THEN
          !
          IF(LEN_TRIM(NUM) < ABS(PAD)) THEN
                IF (PAD>0) THEN
                           IF(PRESENT(ZPAD)) THEN 
                               IF(ZPAD) THEN
                                   INT2STR = TRIM( REPEAT('0',PAD-LEN_TRIM(NUM))//NUM )
                               ELSE
                                   INT2STR = TRIM( REPEAT(' ',PAD-LEN_TRIM(NUM))//NUM )
                               END IF
                           ELSE
                                   INT2STR = TRIM( REPEAT(' ',PAD-LEN_TRIM(NUM))//NUM )
                           END IF
                ELSE
                                   INT2STR = TRIM(NUM)//REPEAT(' ',ABS(PAD)-LEN_TRIM(NUM))
                END IF
          ELSE
                                   INT2STR = TRIM(NUM)
          END IF
    ELSE
                                   INT2STR = TRIM(NUM)
    END IF
    !
  END FUNCTION
  !
  !--------------------------------------------------------------------------------------------------------------------------
  !
  PURE FUNCTION INT2STR_i64(IVAL,PAD,ZPAD) RESULT(INT2STR)
    INTEGER(i64),    INTENT(IN):: IVAL
    INTEGER,OPTIONAL,INTENT(IN):: PAD
    LOGICAL,OPTIONAL,INTENT(IN):: ZPAD
    CHARACTER(:),   ALLOCATABLE:: INT2STR
    CHARACTER(32)::NUM
    !
    WRITE(NUM,'(I32)') IVAL
    !
    NUM=ADJUSTL(NUM)
    IF(PRESENT(PAD)) THEN
          !
          IF(LEN_TRIM(NUM) < ABS(PAD)) THEN
                IF (PAD>0) THEN
                           IF(PRESENT(ZPAD)) THEN 
                               IF(ZPAD) THEN
                                   INT2STR = TRIM( REPEAT('0',PAD-LEN_TRIM(NUM))//NUM )
                               ELSE
                                   INT2STR = TRIM( REPEAT(' ',PAD-LEN_TRIM(NUM))//NUM )
                               END IF
                           ELSE
                                   INT2STR = TRIM( REPEAT(' ',PAD-LEN_TRIM(NUM))//NUM )
                           END IF
                ELSE
                                   INT2STR = TRIM(NUM)//REPEAT(' ',ABS(PAD)-LEN_TRIM(NUM))
                END IF
          ELSE
                                   INT2STR = TRIM(NUM)
          END IF
    ELSE
                                   INT2STR = TRIM(NUM)
    END IF
    !
  END FUNCTION
  !
  !--------------------------------------------------------------------------------------------------------------------------
  !
  !#########################################################################################################################
  !#########################################################################################################################
  !#########################################################################################################################
  !
  ! 
  !
END MODULE
