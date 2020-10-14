! CODE DEVELOPED BY SCOTT E BOYCE 
!                   CONTACT <seboyce@usgs.gov> or <Boyce@engineer.com>
!
! MODULE GENERIC_OPEN_INTERFACE
!
! MODULE PROVIDES ACCESS TO OPEN FUNCTION THAT ALLOWS FILE BUFFERING
! IT ALSO PROVIDES A GENERIC WAY OF OPENING FILES WITH EITHER A UNIT NUMBER OR AUTOASSIGNING A UNIT NUMBER
! TO USE A UNIT NUMER SET IU EQUAL TO THE UNIT NUMBER. 
! TO ASSIGN A NEWUNIT THEN SET IU=0 AND THE AUTOMATIC UNIT NUMBER WILL BE SET TO IU ON RETURN.
! THIS FILE USES CONDITIONAL COMPILATION WITH STANTDARD FORTRAN PREPROCESSING --hense the extension of .fpp and not .f
!
!IF COMPILER STILL HAS ISSUES WITH FREE FORMAT CONDITIONAL COMPILATION PLEASE REMOVE THE #IFDEF AND BUFFER='YES' CODE TO SUITE YOUR COMPILER NEEDS
!
!
!  VERSION 1.2 [9/15/2019] ADDED UTF8_BOM_OFFSET_REWIND, OPEN_NEW_READ_UNIT, UNIT_IS_BOM routines (stranger is that each time I update this, it turns out to be on the 15th of the month!)
!
!  VERSION 1.1 [5/15/2017] ADDED SCRATCH AND NULL FILE ROUTINES
!
!  VERSION 1.0 [5/15/2016] ORIGINAL VERSION THAT OPENS A REQUESTED FILE
!
!
!            SUBROUTINES
!                     GENERIC_OPEN           <= Open file of any format, optional buffering with INTEL Compiler
!                     GENERIC_SCRATCH_FILE   <= Open scratch file
!                     GENERIC_NULL_FILE_OPEN <= Open file that writes to the bit bucket (/dev/null or nul)
!                     SET_TO_NULL_UNIT       <= Sets passed integer to the unit number associated with the null file. 
!                                               CALL SET_TO_NULL_UNIT(IU) is eqiv to IU = NULL_FILE%GET( )
!                     FORCE_UNIT_CLOSE       <= Calls CLOSE multiple times to ensure file unit is closed
!                     OPEN_NEW_READ_UNIT     <= Open same file with a new unit number and read only aspeck
!                     UTF8_BOM_OFFSET_REWIND <= REWIND file, if it contains the UTF8 BOM header, the starting position is adjusted beyond it
!
!            FUNCTION
!                     NULL_FILE%GET( )       <= Returns global null file unit number to write too. see "RETURN_NULL_UNIT" for more details
!                     UNIT_IS_BOM (IU)       <= Checks if unit IU contains the UTF8 BOM header - does this by calling open_new_read_unit
!                         
!    
    
      MODULE GENERIC_OPEN_INTERFACE!, ONLY: GENERIC_OPEN, GENERIC_SCRATCH_FILE, NULL_FILE, GENERIC_NULL_FILE_OPEN, FORCE_UNIT_CLOSE, SET_GENERIC_OPEN_WARN_IU, UTF8_BOM_OFFSET_REWIND, OPEN_NEW_READ_UNIT, UNIT_IS_BOM, SET_TO_NULL_UNIT
      USE PATH_INTERFACE, ONLY: MAKE_DIRECTORY
      IMPLICIT NONE
      PRIVATE
      PUBLIC:: GENERIC_OPEN, GENERIC_SCRATCH_FILE, 
     +         NULL_FILE,    SET_TO_NULL_UNIT,   GENERIC_NULL_FILE_OPEN,
     +         FORCE_UNIT_CLOSE,  SET_GENERIC_OPEN_WARN_IU,
     +         UTF8_BOM_OFFSET_REWIND, OPEN_NEW_READ_UNIT, UNIT_IS_BOM
      !
      TYPE NULL_FILE_TYPE
          INTEGER:: IU = 0
          CONTAINS
          PROCEDURE, PASS(NUL):: GET  => RETURN_NULL_UNIT
          FINAL::                        FINAL_CLOSE_NULL_UNIT
      END TYPE
      !
      TYPE (NULL_FILE_TYPE), SAVE:: NULL_FILE
      !
      INTEGER, SAVE:: WARN_IU = 0   !UNIT IS SET BY WARNING ROUTINE AND PROVIDES SEPARATE FILE FOR STOP, FILEIOU, AND WARN ROUTINES TO WRITE TOO
      !
      ! Constants used internally to module ----------------------------------------------------
      ! 
      LOGICAL, PARAMETER:: TRUE  = .TRUE.
      LOGICAL, PARAMETER:: FALSE = .FALSE.
      !
      ! ----------------------------------------------------------------------------------------
      ! 
      !
      CONTAINS
      !
      SUBROUTINE SET_GENERIC_OPEN_WARN_IU(IU)
        INTEGER, INTENT(IN):: IU
        !
        WARN_IU = IU
        !
      END SUBROUTINE
      !
      RECURSIVE SUBROUTINE GENERIC_OPEN(FNAME, IU, IOUT, ACTION, FORM,
     +                        ACCESS, STATUS, RECORDTYPE, ASYNC, 
     +                        BUFFER_BLOCKSIZE, BUFFER_COUNT, 
     +                        LINE, INFILE, ERROR, IS_BOM, WARN, 
     +                        NO_FIX_DIR)
        CHARACTER(*),           INTENT(IN   ):: FNAME
        INTEGER,                INTENT(INOUT):: IU               ! Unit Number To Open With or IU=0 To return a unit number
        INTEGER,      OPTIONAL, INTENT(IN   ):: IOUT             ! File to write errors too, set to 0 to not use
        CHARACTER(*), OPTIONAL, INTENT(IN   ):: ACTION           ! Set to 'WRITE', 'READ', 'READWRITE'
        CHARACTER(*), OPTIONAL, INTENT(IN   ):: FORM             ! Set to 'FORMATTED' or 'UNFORMATTED'
        CHARACTER(*), OPTIONAL, INTENT(IN   ):: ACCESS           ! Set to 'SEQUENTIAL' or 'STREAM'
        CHARACTER(*), OPTIONAL, INTENT(IN   ):: STATUS           ! Set to 'OLD', 'NEW', 'REPLACE', 'SCRATCH'
        CHARACTER(*), OPTIONAL, INTENT(IN   ):: RECORDTYPE       ! Set to 'STREAM', 'STREAM_LF', 'STREAM_CRLF'
        INTEGER,      OPTIONAL, INTENT(IN   ):: BUFFER_BLOCKSIZE ! Set to 1048576 for file buffering  (size in bytes of buffer)
        INTEGER,      OPTIONAL, INTENT(IN   ):: BUFFER_COUNT     ! Set to 2 for file buffering (threads of buffer)
        CHARACTER(*), OPTIONAL, INTENT(IN   ):: ASYNC            ! Set to 'YES' for asyncronous writing, 'NO' to disable (recommended is 'NO')
        CHARACTER(*), OPTIONAL, INTENT(IN   ):: LINE             ! Line that file or unit number originated from for error reporting.
        INTEGER,      OPTIONAL, INTENT(IN   ):: INFILE           ! File unit that LINE originated from for error reporting.
        LOGICAL,      OPTIONAL, INTENT(  OUT):: ERROR            ! Returns true if file failed to open. When present then subroutine will not stop program on failed open.
        LOGICAL,      OPTIONAL, INTENT(  OUT):: IS_BOM           ! Set to true if file opened was detected as UTF8-BOM Format
        INTEGER,      OPTIONAL, INTENT(IN   ):: WARN             ! Warning File to write errors too, set to 0 to not use. If not present and global variable WARN_IU is set than writes errors/warnings to that
        LOGICAL,      OPTIONAL, INTENT(IN   ):: NO_FIX_DIR       ! If present, disables a recursive call to GENERIC_OPEN in an attempt to fix missing directories in the path
        !
        CHARACTER(:),ALLOCATABLE:: FMTARG, ACCARG, FILSTAT, FILACT, ASYN
        CHARACTER(:),ALLOCATABLE:: ERRMSG, IN_FILE_NAM, RECTYP
        CHARACTER:: NL
        CHARACTER(3 ):: BUF 
        CHARACTER(15):: CERR 
        INTEGER:: BUFFER, BC, IERR, IO
        LOGICAL:: ISOPEN, CHECK
        !
        IF(PRESENT(IOUT)) THEN
            IO=IOUT
        ELSE
            IO=0
        END IF
        !
        IF(PRESENT(ACTION)) THEN
            FILACT=ACTION
        ELSE
            FILACT='READWRITE'
        END IF
        !
        IF(PRESENT(FORM)) THEN
            FMTARG=FORM
        ELSE
            FMTARG='FORMATTED'
        END IF
        !
        IF(PRESENT(ACCESS)) THEN
            ACCARG=ACCESS
        ELSE
            ACCARG='SEQUENTIAL'
        END IF
        !
        IF(PRESENT(STATUS)) THEN
            FILSTAT=STATUS
        ELSE
            FILSTAT='UNKNOWN'
        END IF
        !
        IF(PRESENT(IS_BOM)) IS_BOM = FALSE
        !
        IF (PRESENT(BUFFER_BLOCKSIZE) .AND. PRESENT(BUFFER_COUNT)) THEN
                                                BUFFER=BUFFER_BLOCKSIZE
                                                BC    =BUFFER_COUNT
        ELSEIF(PRESENT(BUFFER_BLOCKSIZE)) THEN
                                                BUFFER=BUFFER_BLOCKSIZE
                                                BC    =2
        ELSEIF(PRESENT(BUFFER_COUNT)) THEN
                                                BUFFER=65536  !64KB x2 = 128KB
                                                BC    =BUFFER_COUNT
        ELSE
                                                BUFFER=0
                                                BC=0
        END IF
        !
        IF (BUFFER==0 .OR. BC==0) THEN
                                      BUFFER=0
                                      BC=0
                                      BUF = 'NO '
        ELSE
                                      BUF = 'YES'
        END IF
        !
        IF(PRESENT(ASYNC)) THEN
            ASYN=ASYNC
        ELSE
            ASYN='NO'
        END IF
        !
        IF    (PRESENT(RECORDTYPE)) THEN
                                 RECTYP = RECORDTYPE
        ELSEIF(FMTARG=='FORMATTED') THEN
                                 RECTYP = 'STREAM_LF' ! Use LF for record terminator
        ELSE
                                 RECTYP = 'STREAM'    ! Indicates that no record ending is created
        END IF
        !
        !IF (ASYN='YES' .AND. BUFFER>0) BUFFER=0
        !
        IF(IU==0) THEN
          !----------------------------------------------------------------------------
#ifdef  __INTEL_COMPILER
          IF (FILACT == 'WRITE' .OR. PRESENT(RECORDTYPE)) THEN
              !
              OPEN(NEWUNIT=IU, FILE=FNAME, ACTION=FILACT, FORM=FMTARG, 
     +             ACCESS=ACCARG, STATUS=FILSTAT, POSITION='REWIND', 
     +             IOSTAT=IERR, ASYNCHRONOUS=ASYN,
     +             BUFFERED=BUF, BLOCKSIZE=BUFFER, BUFFERCOUNT=BC,
     +             RECORDTYPE=RECTYP)
          ELSE
              OPEN(NEWUNIT=IU, FILE=FNAME, ACTION=FILACT, FORM=FMTARG, 
     +             ACCESS=ACCARG, STATUS=FILSTAT, POSITION='REWIND', 
     +             IOSTAT=IERR, ASYNCHRONOUS=ASYN,
     +             BUFFERED=BUF, BLOCKSIZE=BUFFER, BUFFERCOUNT=BC)
          END IF
          !----------------------------------------------------------------------------
#else       
             OPEN(NEWUNIT=IU, FILE=FNAME, ACTION=FILACT, FORM=FMTARG, 
     +            ACCESS=ACCARG, STATUS=FILSTAT, POSITION='REWIND', 
     +            IOSTAT=IERR, ASYNCHRONOUS=ASYN)
#endif 
             !---------------------------------------------------------------------------------------------------------
        ELSE !######################################################################################################### (IU /= 0)
             !---------------------------------------------------------------------------------------------------------
#ifdef  __INTEL_COMPILER
          IF (FILACT == 'WRITE' .OR. PRESENT(RECORDTYPE)) THEN
              !
              OPEN(   UNIT=IU, FILE=FNAME, ACTION=FILACT, FORM=FMTARG, 
     +             ACCESS=ACCARG, STATUS=FILSTAT, POSITION='REWIND', 
     +             IOSTAT=IERR, ASYNCHRONOUS=ASYN,
     +             BUFFERED=BUF, BLOCKSIZE=BUFFER, BUFFERCOUNT=BC,
     +             RECORDTYPE=RECTYP)
          ELSE
              OPEN(   UNIT=IU, FILE=FNAME, ACTION=FILACT, FORM=FMTARG, 
     +             ACCESS=ACCARG, STATUS=FILSTAT, POSITION='REWIND', 
     +             IOSTAT=IERR, ASYNCHRONOUS=ASYN,
     +             BUFFERED=BUF, BLOCKSIZE=BUFFER, BUFFERCOUNT=BC)
          END IF
          !----------------------------------------------------------------------------
#else       
             OPEN(   UNIT=IU, FILE=FNAME, ACTION=FILACT, FORM=FMTARG, 
     +            ACCESS=ACCARG, STATUS=FILSTAT, POSITION='REWIND', 
     +            IOSTAT=IERR, ASYNCHRONOUS=ASYN)
#endif 
          !----------------------------------------------------------------------------
        END IF
        !
        IF(IERR.NE.0) THEN            ! Check to see if the error is cause directories need to be created.
         !
         IF(PRESENT(NO_FIX_DIR)) THEN ! Only fix directory if NO_FIX_DIR is not present or NO_FIX_DIR=FALSE
                                 CHECK = .NOT. NO_FIX_DIR 
         ELSE
                                 CHECK = TRUE
         END IF
         !
         IF(CHECK) THEN           
            !
            CHECK = STATUS=='UNKNOWN' .OR.  ! Place holder to simplify the following IF
     +               STATUS=='REPLACE' .OR. 
     +               STATUS=='NEW'
            !
            IF( FILACT == 'WRITE' .OR. 
     +         (FILACT == 'READWRITE' .AND. CHECK) ) THEN
               !
            CALL MAKE_DIRECTORY(FNAME, HAS_FILE=TRUE, FIXED=IN_FILE_NAM)
               !
               CALL GENERIC_OPEN(IN_FILE_NAM, IU, IOUT, ACTION, FORM,
     +                          ACCESS, STATUS, RECORDTYPE, ASYNC, 
     +                          BUFFER_BLOCKSIZE, BUFFER_COUNT, 
     +                          ERROR=CHECK, NO_FIX_DIR=TRUE)
               !
               IF(.NOT. CHECK) IERR = 0 ! File sucessfully opened with directory fix
               !    
               DEALLOCATE(IN_FILE_NAM)
            END IF
         END IF
        END IF
        !
        IF(PRESENT(ERROR)) THEN
            !
            ERROR = IERR.NE.0
            !
        ELSEIF(IERR.NE.0) THEN
           !
           INQUIRE(FILE=FNAME, OPENED=ISOPEN)
           !
           NL=NEW_LINE(NL)
           !
           IF(ISOPEN) THEN
              WRITE(CERR,'(I15)') IERR
              ERRMSG= NL//NL//'FAILURE TO OPEN FILE: '//NL//
     +        '"'//TRIM(FNAME)//'"'//NL//NL//
     +     'DUE TO THE FACT THAT THE FILE HAS ALREADY BEEN OPEN '//
     +     'ELSEWHERE IN THE SIMULATION'//NL//'OR THE FILE IS LOCKED '//
     +     'AND CANNOT BE OPENED FOR READING/WRITING.'//NL//NL//
     +        'THE RETURNED FORTRAN ERROR CODE IS: '//TRIM(CERR)//NL
           ELSE
              WRITE(CERR,'(I15)') IERR
              ERRMSG= NL//NL//'FAILURE TO OPEN FILE: '//NL//
     +        '"'//TRIM(FNAME)//'"'//NL//NL//
     +        'THE LOCATION COULD BE BAD OR THE FILE LOCKED.'//NL//NL//
     +        'THE RETURNED FORTRAN ERROR CODE IS: '//TRIM(CERR)//NL
           END IF
           !
           IF(PRESENT(INFILE)) THEN
              ALLOCATE(CHARACTER(768):: IN_FILE_NAM)
              INQUIRE(INFILE, NAME=IN_FILE_NAM)
              ERRMSG = ERRMSG//NL//NL//
     +          'THE INPUT FILE THAT REQUESTED TO OPEN THIS FILE IS:'
     +          //NL//TRIM(IN_FILE_NAM)//NL
           END IF
           !
           IF(PRESENT(LINE)) ERRMSG = ERRMSG//NL//NL//
     +        'THE LINE THAT CONTAINED THE REQUEST TO OPEN THE FILE IS:'
     +                                              //NL//TRIM(LINE)//NL
           !
           IF(PRESENT(WARN)) THEN
                 IF(WARN.NE.0) WRITE(WARN,'(// 27x,A,//A)') 
     +                                         'ONE-WATER ERROR',ERRMSG
           ELSEIF(WARN_IU.NE.0) THEN
                 IF(WARN_IU.NE.0) WRITE(WARN_IU,'(// 27x,A,//A)') 
     +                                         'ONE-WATER ERROR',ERRMSG
           END IF
           ERRMSG=NL//NL//'  ONE-WATER ERROR'//ERRMSG
           IF(IO.NE.0) WRITE(IO,'(A)')ERRMSG
           WRITE(*,'(A)')ERRMSG
           ERROR STOP
        END IF
        !
        IF(IERR == 0       .AND. FMTARG == 'FORMATTED') THEN
        IF(FILACT == 'READ' .OR. FILACT == 'READWRITE') THEN
             !
             CALL UTF8_BOM_OFFSET_REWIND(IU, IS_BOM)
             !
        END IF
        END IF
        !
        DEALLOCATE(FMTARG, ACCARG, FILSTAT, FILACT, ASYN, RECTYP)
      END SUBROUTINE
      !
      !##################################################################################################
      !##################################################################################################
      !
      SUBROUTINE UTF8_BOM_OFFSET_REWIND(IU, IS_BOM)
         INTEGER,           INTENT(IN   ):: IU
         LOGICAL, OPTIONAL, INTENT(  OUT):: IS_BOM
         LOGICAL:: NOT_BOM
         CHARACTER(3):: BOM
         INTEGER:: IERR
         !
         REWIND(IU)
         READ(IU, '(A)', ADVANCE='NO', IOSTAT=IERR) BOM
         !
         IF(IERR .NE. 0) THEN
                         NOT_BOM = TRUE
         ELSE
                         NOT_BOM = ICHAR(BOM(1:1)) .NE. 239 .OR.  
     +                             ICHAR(BOM(2:2)) .NE. 187 .OR.  
     +                             ICHAR(BOM(3:3)) .NE. 191     
         END IF
         !
         IF( NOT_BOM ) REWIND(IU)
         !
         IF(PRESENT(IS_BOM)) IS_BOM = .NOT. NOT_BOM
         !
      END SUBROUTINE
      !
      !##################################################################################################
      !##################################################################################################
      !
      SUBROUTINE OPEN_NEW_READ_UNIT(IU, IU_OUT, IS_BOM)
         INTEGER,           INTENT(IN   ):: IU      !File unit to clone to new file unit
         INTEGER,           INTENT(INOUT):: IU_OUT  !Must be set to zero to auto-make with NEWUNIT otherwise uses unit number stored in variable
         LOGICAL, OPTIONAL, INTENT(  OUT):: IS_BOM
         CHARACTER(768):: FNAME
         CHARACTER( 16):: ACCESS, FORM
         LOGICAL:: ISOPEN,ERROR
         !
         IF(IU .NE. 0) THEN
             !
             INQUIRE(IU, ACCESS=ACCESS, FORM=FORM, 
     +               OPENED=ISOPEN, NAME=FNAME )
             IF(ISOPEN) THEN 
                 !
                 CALL GENERIC_OPEN(
     +           FNAME, IU_OUT, ACTION='READ', FORM=FORM, 
     +           ACCESS=ACCESS, STATUS='OLD', BUFFER_BLOCKSIZE=16384, ! 16KB x2 = 32KB  --1048576 = 1MB   --NOTE THAT TWO THREADS ARE USED PER BUFFER SO ACTUAL SPACE IS TWICE AS BIG
     +           ERROR=ERROR, IS_BOM=IS_BOM)
                 !
                 IF(ERROR) IU_OUT = 0
             ELSE
                 IU_OUT = 0
             END IF
         ELSE
             IU_OUT = 0
         END IF
         !
      END SUBROUTINE
      !
      !##################################################################################################
      !##################################################################################################
      !
      FUNCTION UNIT_IS_BOM(IU) RESULT(IS_BOM)
         INTEGER, INTENT(IN   ):: IU
         LOGICAL:: IS_BOM
         !
         INTEGER:: IU_OUT, I
         CHARACTER(768):: FNAME
         CHARACTER( 16):: ACCESS, FORM, ACTION
         LOGICAL:: ISOPEN,ERROR
         !
         IS_BOM = FALSE
         !
         IF(IU .NE. 0) THEN
             !
             INQUIRE(IU, ACCESS=ACCESS, FORM=FORM, 
     +               OPENED=ISOPEN, NAME=FNAME, ACTION=ACTION )
             !
             IF(ISOPEN            .AND. FORM == 'FORMATTED' .AND. 
     +          (ACTION == 'READ' .OR.  ACTION == 'READWRITE')   ) THEN 
                   !
                   IU_OUT = 0
                   CALL GENERIC_OPEN(
     +             FNAME, IU_OUT, ACTION='READ', FORM=FORM, 
     +             ACCESS=ACCESS, STATUS='OLD', BUFFER_BLOCKSIZE=16384, ! 16KB x2 = 32KB  --1048576 = 1MB   --NOTE THAT TWO THREADS ARE USED PER BUFFER SO ACTUAL SPACE IS TWICE AS BIG
     +             ERROR=ERROR, IS_BOM=IS_BOM)
                   !
                   IF(.NOT. ERROR) CLOSE(IU_OUT, IOSTAT=I)
             END IF
         END IF
         !
      END FUNCTION
      !
      !##################################################################################################
      !##################################################################################################
      !
      SUBROUTINE GENERIC_SCRATCH_FILE(IU, IOUT, FORM, ACCESS, 
     +                     BUFFER_BLOCKSIZE, LINE, INFILE, ERROR)
        INTEGER,                INTENT(INOUT):: IU               ! Unit Number To Open With or IU=0 To return a unit number
        INTEGER,      OPTIONAL, INTENT(IN   ):: IOUT             ! File to write errors too, set to 0 to not use
        CHARACTER(*), OPTIONAL, INTENT(IN   ):: FORM             ! Set to 'FORMATTED' or 'UNFORMATTED'
        CHARACTER(*), OPTIONAL, INTENT(IN   ):: ACCESS           ! Set to 'SEQUENTIAL' or 'STREAM'
        INTEGER,      OPTIONAL, INTENT(IN   ):: BUFFER_BLOCKSIZE ! Set to 1048576 for file buffering  (size in bytes of buffer)
        CHARACTER(*), OPTIONAL, INTENT(IN   ):: LINE             ! Line that file or unit number originated from for error reporting.
        INTEGER,      OPTIONAL, INTENT(IN   ):: INFILE           ! File unit that LINE originated from for error reporting.
        LOGICAL,      OPTIONAL, INTENT(OUT  ):: ERROR            ! Returns true if file failed to open. When present then subroutnie will not stop program on failed open.
        !
        CHARACTER(:),ALLOCATABLE:: FMTARG, ACCARG
        CHARACTER(:),ALLOCATABLE:: ERRMSG, IN_FILE_NAM
        CHARACTER(7), PARAMETER:: FILSTAT='SCRATCH'
        CHARACTER(9), PARAMETER:: FILACT='READWRITE'
        CHARACTER:: NL
        CHARACTER(15):: CERR 
        INTEGER:: BUFFER, BC, IERR, IO
        !
        IF(PRESENT(IOUT)) THEN
            IO=IOUT
        ELSE
            IO=0
        END IF
        !
        IF(PRESENT(FORM)) THEN
            FMTARG=FORM
        ELSE
            FMTARG='FORMATTED'
        END IF
        !
        IF(PRESENT(ACCESS)) THEN
            ACCARG=ACCESS
        ELSE
            ACCARG='SEQUENTIAL'
        END IF
        !
        IF(PRESENT(BUFFER_BLOCKSIZE)) THEN
                                                BUFFER=BUFFER_BLOCKSIZE
                                                BC    =1
        ELSE
                                                BUFFER=0
                                                BC=0
        END IF
        !
        IF (BUFFER==0 .OR. BC==0) THEN
                                      BUFFER=0
                                      BC=0
        END IF
        !
        IF(IU==0) THEN
          IF (BUFFER > 0) THEN
#ifdef  __INTEL_COMPILER
             OPEN(NEWUNIT=IU, ACTION=FILACT, FORM=FMTARG, 
     +            ACCESS=ACCARG, STATUS=FILSTAT, POSITION='REWIND', 
     +            IOSTAT=IERR,
     +            BUFFERED='yes', BLOCKSIZE=BUFFER,BUFFERCOUNT=BC)
#else       
             OPEN(NEWUNIT=IU, ACTION=FILACT, FORM=FMTARG, 
     +            ACCESS=ACCARG, STATUS=FILSTAT, POSITION='REWIND', 
     +            IOSTAT=IERR)
#endif 
          ELSE
             OPEN(NEWUNIT=IU, ACTION=FILACT, FORM=FMTARG, 
     +            ACCESS=ACCARG, STATUS=FILSTAT, POSITION='REWIND', 
     +            IOSTAT=IERR)
          END IF
          !
        ELSE !############################################################3
          !
          IF (BUFFER > 0) THEN
#ifdef  __INTEL_COMPILER
             OPEN(UNIT=IU, ACTION=FILACT, FORM=FMTARG, 
     +            ACCESS=ACCARG, STATUS=FILSTAT, POSITION='REWIND', 
     +            IOSTAT=IERR,
     +            BUFFERED='yes', BLOCKSIZE=BUFFER,BUFFERCOUNT=BC)
#else       
             OPEN(UNIT=IU, ACTION=FILACT, FORM=FMTARG, 
     +            ACCESS=ACCARG, STATUS=FILSTAT, POSITION='REWIND', 
     +            IOSTAT=IERR)
#endif 
          ELSE
             OPEN(UNIT=IU, ACTION=FILACT, FORM=FMTARG, 
     +            ACCESS=ACCARG, STATUS=FILSTAT, POSITION='REWIND', 
     +            IOSTAT=IERR)
          END IF
          
        END IF
        !
        IF(PRESENT(ERROR)) THEN
            !
            ERROR = IERR.NE.0
            !
        ELSEIF(IERR.NE.0) THEN
           !
           NL=NEW_LINE(NL)
           WRITE(CERR,'(I15)') IERR
           ERRMSG= NL//NL//'FAILURE TO SCRATCH FILE: '//NL//
     +     'THE LOCATION COULD BE BAD OR THE FILE LOCKED.'//NL//
     +     'THE RETURNED FORTRAN ERROR CODE IS: '//TRIM(CERR)//NL
           !
           IF(PRESENT(INFILE)) THEN
              ALLOCATE(CHARACTER(768):: IN_FILE_NAM)
              INQUIRE(INFILE, NAME=IN_FILE_NAM)
              ERRMSG = ERRMSG//NL//
     +     'THE INPUT FILE THAT REQUESTED TO OPEN THIS SCRATCH FILE IS:'
     +          //NL//TRIM(IN_FILE_NAM)//NL
           END IF
           !
           IF(PRESENT(LINE)) ERRMSG = ERRMSG//NL//
     +'THE LINE THAT CONTAINED THE REQUEST TO OPEN THE SCRATCH FILE IS:'
     +                                              //NL//TRIM(LINE)//NL
           !
           IF(IO.NE.0) WRITE(IO,'(A)')ERRMSG
           WRITE(*,'(A)')ERRMSG
           ERROR STOP
        END IF
        !
        DEALLOCATE(FMTARG, ACCARG)
      END SUBROUTINE
      !
      !##################################################################################################
      !##################################################################################################
      !
      SUBROUTINE GENERIC_NULL_FILE_OPEN(IU, LINE, INFILE, IOUT, 
     +                                      FORM, ACCESS, ERROR)
        INTEGER,                INTENT(INOUT):: IU               ! Unit Number To Open With or IU=0 To return a unit number
        INTEGER,      OPTIONAL, INTENT(IN   ):: IOUT             ! File to write errors too, set to 0 to not use
        CHARACTER(*), OPTIONAL, INTENT(IN   ):: FORM             ! Set to 'FORMATTED' or 'UNFORMATTED'
        CHARACTER(*), OPTIONAL, INTENT(IN   ):: ACCESS           ! Set to 'SEQUENTIAL' or 'STREAM'
        CHARACTER(*), OPTIONAL, INTENT(IN   ):: LINE             ! Line that file or unit number originated from for error reporting.
        INTEGER,      OPTIONAL, INTENT(IN   ):: INFILE           ! File unit that LINE originated from for error reporting.
        LOGICAL,      OPTIONAL, INTENT(OUT  ):: ERROR            ! Returns true if file failed to open. When present then subroutnie will not stop program on failed open.
        !
        CHARACTER:: NL
        !CHARACTER(5):: FILACT
        !CHARACTER(7):: FILSTAT
        CHARACTER(9):: FNAM
        CHARACTER(:),ALLOCATABLE:: ERRMSG
        INTEGER:: BUFFER, BC
        LOGICAL:: ERR
        !
        BUFFER = 1048576  !1MB Buffer
        BC     = 1
        !
#ifdef  __unix__
        FNAM = '/dev/null'
#else     
        FNAM = 'nul'
#endif 
        !
        CALL  GENERIC_OPEN(FNAM, IU, IOUT, ACTION='WRITE',
     +           FORM=FORM,ACCESS=ACCESS, STATUS='UNKNOWN', 
     +           BUFFER_BLOCKSIZE=BUFFER, BUFFER_COUNT=BC,
     +           LINE=LINE, INFILE=INFILE, ERROR=ERR)
        !
        IF(ERR) THEN
            IF(FNAM=='nul') THEN
                FNAM = '/dev/null'
                CALL  GENERIC_OPEN(FNAM, IU, IOUT, ACTION='WRITE', 
     +                   FORM=FORM,ACCESS=ACCESS, STATUS='UNKNOWN', 
     +                   BUFFER_BLOCKSIZE=BUFFER, BUFFER_COUNT=BC,
     +                   LINE=LINE, INFILE=INFILE, ERROR=ERR)
            END IF
        END IF
        !
        IF(PRESENT(ERROR)) THEN
            !
            ERROR = ERR
            !
        ELSEIF(ERR) THEN
           !
           NL=NEW_LINE(NL)
         ERRMSG= NL//NL//'FAILURE TO OPEN BIT BUCKET (NULL) FILE'//NL//
     +'COMPILER OR OPERATING SYSTEM MOST LIKELY DOES NOT SUPPORT '//
     +'WRITING TO A NULL FILE.'//NL//'PLEASE REMOVE "NOPRINT" OPTION '//
     +'AND WRITE TO AN ACTUAL FILE OR'//NL//
     +'ENSURE THAT THE LIST FILE IS SPECIFIED IN THE NAME.'//NL
           !
           IF(PRESENT(LINE)) ERRMSG = ERRMSG//NL//
     +'THE INPUT LINE THAT CAUSED THIS ERROR AND NEEDS TO BE FIXED IS:'
     +                                        //NL//NL//TRIM(LINE)//NL
           !
           IF(PRESENT(IOUT)) THEN; IF(IOUT.NE.0) WRITE(IOUT,'(A)')ERRMSG
           END IF
           !
           WRITE(*,'(A)')ERRMSG
           ERROR STOP
        END IF
        !
      END SUBROUTINE
!      !
!      !##################################################################################################
!      !##################################################################################################
!      !
!      SUBROUTINE GENERIC_NULL_FILE_OPEN(IU, LINE, INFILE, IOUT, 
!     +                                      FORM, ACCESS, ERROR)
!        INTEGER,                INTENT(INOUT):: IU               ! Unit Number To Open With or IU=0 To return a unit number
!        INTEGER,      OPTIONAL, INTENT(IN   ):: IOUT             ! File to write errors too, set to 0 to not use
!        CHARACTER(*), OPTIONAL, INTENT(IN   ):: FORM             ! Set to 'FORMATTED' or 'UNFORMATTED'
!        CHARACTER(*), OPTIONAL, INTENT(IN   ):: ACCESS           ! Set to 'SEQUENTIAL' or 'STREAM'
!        CHARACTER(*), OPTIONAL, INTENT(IN   ):: LINE             ! Line that file or unit number originated from for error reporting.
!        INTEGER,      OPTIONAL, INTENT(IN   ):: INFILE           ! File unit that LINE originated from for error reporting.
!        LOGICAL,      OPTIONAL, INTENT(OUT  ):: ERROR            ! Returns true if file failed to open. When present then subroutnie will not stop program on failed open.
!        !
!        CHARACTER(:),ALLOCATABLE:: FMTARG, ACCARG, FILSTAT, FILACT
!        CHARACTER(:),ALLOCATABLE:: ERRMSG, IN_FILE_NAM
!        CHARACTER:: NL
!        CHARACTER(15):: CERR 
!        INTEGER:: BUFFER, BC, IERR, IO
!        !
!        IF(PRESENT(IOUT)) THEN
!            IO=IOUT
!        ELSE
!            IO=0
!        END IF
!        !
!        FILACT='WRITE'
!        !
!        IF(PRESENT(FORM)) THEN
!            FMTARG=FORM
!        ELSE
!            FMTARG='FORMATTED'
!        END IF
!        !
!        IF(PRESENT(ACCESS)) THEN
!            ACCARG=ACCESS
!        ELSE
!            ACCARG='SEQUENTIAL'
!        END IF
!        !
!        FILSTAT='UNKNOWN'
!        !
!        IF(IU==0) THEN
!#ifdef  __unix__
!          OPEN(NEWUNIT=IU, FILE='/dev/null', ACTION=FILACT, FORM=FMTARG,
!     +         ACCESS=ACCARG, STATUS=FILSTAT, IOSTAT=IERR)
!#else     
!          OPEN(NEWUNIT=IU, FILE='nul', ACTION=FILACT, FORM=FMTARG,
!     +         ACCESS=ACCARG, STATUS=FILSTAT, IOSTAT=IERR)
!          !
!          IF(IERR.NE.0) OPEN(UNIT=IU, FILE='/dev/null', 
!     +                      ACTION=FILACT, FORM=FMTARG,ACCESS=ACCARG, 
!     +                      STATUS=FILSTAT, IOSTAT=IERR)
!#endif 
!          !
!        ELSE !############################################################3
!          !
!#ifdef  __unix__
!          OPEN(UNIT=IU, FILE='/dev/null', ACTION=FILACT, FORM=FMTARG,
!     +         ACCESS=ACCARG, STATUS=FILSTAT, IOSTAT=IERR)
!#else     
!          OPEN(UNIT=IU, FILE='nul', ACTION=FILACT, FORM=FMTARG,
!     +         ACCESS=ACCARG, STATUS=FILSTAT, IOSTAT=IERR)
!          !
!          IF(IERR.NE.0) OPEN(UNIT=IU, FILE='/dev/null', 
!     +                      ACTION=FILACT, FORM=FMTARG,ACCESS=ACCARG, 
!     +                      STATUS=FILSTAT, IOSTAT=IERR)
!#endif 
!          
!        END IF
!        !
!        IF(PRESENT(ERROR)) THEN
!            !
!            ERROR = IERR.NE.0
!            !
!        ELSEIF(IERR.NE.0) THEN
!           !
!           NL=NEW_LINE(NL)
!           WRITE(CERR,'(I15)') IERR
!         ERRMSG= NL//NL//'FAILURE TO OPEN BIT BUCKET (NULL) FILE'//NL//
!     +'COMPILER OR OPERATING SYSTEM MOST LIKELY DOES NOT SUPPORT '//
!     +                                              'THIS OPTION.'//NL//
!     +'PLEASE REMOVE "NOPRINT" OPTION AND WRITE TO AN ACTUAL FILE.'//NL
!           !
!           IF(PRESENT(LINE)) ERRMSG = ERRMSG//NL//
!     +'THE INPUT LINE THAT NEEDS TO BE FIXED IS:'
!     +                                              //NL//TRIM(LINE)//NL
!           !
!           IF(IO.NE.0) WRITE(IO,'(A)')ERRMSG
!           WRITE(*,'(A)')ERRMSG
!           ERROR STOP
!        END IF
!        !
!        DEALLOCATE(FMTARG, ACCARG, FILSTAT, FILACT)
!      END SUBROUTINE
      !
      FUNCTION RETURN_NULL_UNIT( NUL, LINE, INFILE, IOUT, FORM, ACCESS )  !"NUL" IS AUTOMATICALLY PASSED, REST OF THE ARG ARE OPTIONAL  --> IU = NULL_FILE%GET(LINE, INFILE, IOUT, FORM, ACCESS)
     +                                                        RESULT(IU)
       CLASS(NULL_FILE_TYPE),  INTENT(INOUT):: NUL
       INTEGER,      OPTIONAL, INTENT(IN   ):: IOUT             ! File to write errors too, set to 0 to not use
       CHARACTER(*), OPTIONAL, INTENT(IN   ):: FORM             ! Set to 'FORMATTED' or 'UNFORMATTED'
       CHARACTER(*), OPTIONAL, INTENT(IN   ):: ACCESS           ! Set to 'SEQUENTIAL' or 'STREAM'
       CHARACTER(*), OPTIONAL, INTENT(IN   ):: LINE             ! Line that file or unit number originated from for error reporting.
       INTEGER,      OPTIONAL, INTENT(IN   ):: INFILE           ! File unit that LINE originated from for error reporting.
       INTEGER:: IU 
       !
       IF( NUL%IU == 0 ) 
     +  CALL GENERIC_NULL_FILE_OPEN(NUL%IU,LINE,INFILE,IOUT,FORM,ACCESS)
       !
       IU = NUL%IU
       !
      END FUNCTION
      !
      SUBROUTINE SET_TO_NULL_UNIT(IU)  !SET IU TO NUL UNIT NUMBER  
       INTEGER,  INTENT(OUT):: IU 
       !
       IU = RETURN_NULL_UNIT( NULL_FILE )
       !
      END SUBROUTINE
      !
      SUBROUTINE FINAL_CLOSE_NULL_UNIT( NUL )
        TYPE(NULL_FILE_TYPE),  INTENT(INOUT):: NUL
        INTEGER:: I
        !
        IF(NUL%IU .NE. 0) CLOSE(NUL%IU,IOSTAT=I)
        !
      END SUBROUTINE
      !
      SUBROUTINE FORCE_UNIT_CLOSE(IU)
      INTEGER, INTENT(IN):: IU
      INTEGER:: I, IERR
      LOGICAL:: ISOPEN
      !
      IF(IU.NE.0) THEN
         INQUIRE(IU,OPENED=ISOPEN)
         IF(ISOPEN) THEN
             FLUSH(IU,IOSTAT=IERR)
             IF(IERR.NE.0) THEN
                 DO I=1, 5
                          FLUSH(IU,IOSTAT=IERR)
                          IF(IERR==0) EXIT
                 END DO
             END IF
             !
             CLOSE(IU,IOSTAT=IERR)
             IF(IERR.NE.0) THEN
                 DO I=1, 5
                          CLOSE(IU,IOSTAT=IERR)
                          IF(IERR==0) EXIT
                 END DO
             END IF
             !
             INQUIRE(IU,OPENED=ISOPEN)
             IF(ISOPEN) CLOSE(IU,IOSTAT=IERR)
             !
             FLUSH(IU,IOSTAT=IERR)
         END IF
      END IF
      !
      END SUBROUTINE
      !
      END MODULE