!
! CODE DEVELOPED BY SCOTT E BOYCE 
!                   CONTACT <seboyce@usgs.gov> or <Boyce@engineer.com>
!
!                FILE_INCREMENTER_INTERFACE
!                           DATA TYPE
!                                    FILE_INCREMENTER
!                           SUBROUTINES
!                                    FI%INIT
!                                    FI%SIZE_CHECK --NOTE THIS MAY CHANGE FI%IU --DUE TO COMPILER BUG
!                                    FI%SET_HEADER
!                                    FI%MOVE
!         
!    
!
MODULE FILE_INCREMENTER_INTERFACE!, ONLY: FILE_INCREMENTER
  !USE UTIL_INTERFACE, ONLY: GET_WARN
  USE CONSTANTS,      ONLY: BLNK,NL,TAB,COM,Z,ONE,TWO,TEN,DZ,UNO,DOS,DIEZ,TRUE,FALSE
  USE GENERIC_OPEN_INTERFACE
  IMPLICIT NONE
  PRIVATE
  PUBLIC:: FILE_INCREMENTER
  !  
  TYPE FILE_INCREMENTER
      CHARACTER(:),ALLOCATABLE:: FILE           !FILE BASENAME
      CHARACTER(:),ALLOCATABLE:: EXT            !FILE EXTENSION
      CHARACTER(:),ALLOCATABLE:: HED            !FILE HEADER PLACED ON FIRST LINE
      INTEGER:: NUM                             !COUNTER FOR THE FILE NUMBER TO ADD TO END OF FILENAME
      INTEGER:: MAXSIZE                         !MAXIMUM FILE SIZE IN KB, FOR RUTINE INIT IT ARRIVES IN MB AND CONVERTED TO KB
      !INTEGER:: LINECOUNT, MAXCOUNT
      INTEGER:: IU                              !FILE UNIT NUMBER
      INTEGER:: BUF                             !IF >0 THEN BUFFER FILE OUTPUT. THIS MAY MAKE SPLIT A BIT MORE INACCURATE, BUT FASTER WRITE.
      INTEGER:: COUNTER  = Z                    ! KEEPS TRACK OF WHEN SIZE CHECK IS MADE
      INTEGER:: MAXCOUNT = TEN                  ! WHEN COUNTER REACHES THIS VALUE IT DOES AN ACTUAL SIZE CHECK
      CHARACTER(15):: FORM                      !BINARY, UNFORMATTED, FORMATTED
      CHARACTER(10):: ACCESS
      LOGICAL:: BINARY
      LOGICAL:: OPENCLOSE
      !
    CONTAINS
      !NOTE THAT PASSED IN MAXSIZE IS IN MB, BUT INTERNALLY USED AS KB
      GENERIC:: INIT                      => INITIALIZE_FILE_INCREMENTER_UNIT_FILE, INITIALIZE_FILE_INCREMENTER_FILE, INITIALIZE_FILE_INCREMENTER_UNIT !(MAXSIZE, [FILE], [UNIT], [BUFFER])  --BUFFER REQUIRED FOR BOTH
      PROCEDURE, PASS(FI):: SIZE_CHECK    => FILE_INCREMENTER_SIZE_CHECK !([LINE])  --NOTE THIS CHANGES FI%IU DUE TO COMPILER BUG
      PROCEDURE, PASS(FI):: SET_HEADER    => FILE_INCREMENTER_SET_HEADER !(HEADER, [NOWRITE])
      PROCEDURE, PASS(FI):: MOVE          => MOVE_FILE_INCREMENTER_FILE  !(FI_NEW)
      PROCEDURE, PASS(FI):: DESTROY       => DEALLOCATE_FILE_INCREMENTER !()
      GENERIC            :: ASSIGNMENT(=) => COPY_FILE_INCREMENTER_FILE
      !
      !PROCEDURE, PASS(FI),PRIVATE:: INITIALIZE_FILE_INCREMENTER
      PROCEDURE, PASS(FI),PRIVATE:: INITIALIZE_FILE_INCREMENTER_FILE
      PROCEDURE, PASS(FI),PRIVATE:: INITIALIZE_FILE_INCREMENTER_UNIT
      PROCEDURE, PASS(FI),PRIVATE:: INITIALIZE_FILE_INCREMENTER_UNIT_FILE
      PROCEDURE,          PRIVATE:: COPY_FILE_INCREMENTER_FILE
      !PROCEDURE, PASS(FI),PRIVATE:: INITIALIZE_FILE_INCREMENTER_BOTH
      !
      FINAL::                       FINAL_DEALLOCATE_FILE_INCREMENTER
    END TYPE
      
    CONTAINS
    !
  SUBROUTINE COPY_FILE_INCREMENTER_FILE(FI_OUT, FI_IN)
    CLASS(FILE_INCREMENTER), INTENT(OUT):: FI_OUT
    CLASS(FILE_INCREMENTER), INTENT(IN ):: FI_IN
    !
    CALL DEALLOCATE_FILE_INCREMENTER(FI_OUT)
    !
    IF(ALLOCATED(FI_IN%FILE)) ALLOCATE(FI_OUT%FILE, SOURCE = FI_IN%FILE)
    IF(ALLOCATED(FI_IN%EXT )) ALLOCATE(FI_OUT%EXT , SOURCE = FI_IN%EXT )
    IF(ALLOCATED(FI_IN%HED )) ALLOCATE(FI_OUT%HED , SOURCE = FI_IN%HED )
    !
    FI_OUT%NUM       = FI_IN%NUM                
    FI_OUT%MAXSIZE   = FI_IN%MAXSIZE       
    FI_OUT%IU        = FI_IN%IU                  
    FI_OUT%BUF       = FI_IN%BUF                 
    FI_OUT%FORM      = FI_IN%FORM          
    FI_OUT%ACCESS    = FI_IN%ACCESS
    FI_OUT%OPENCLOSE = FI_IN%OPENCLOSE 
    FI_OUT%COUNTER   = FI_IN%COUNTER
    FI_OUT%MAXCOUNT  = FI_IN%MAXCOUNT
    !
  END SUBROUTINE
    !
  SUBROUTINE MOVE_FILE_INCREMENTER_FILE(FI, FI_NEW)
    CLASS(FILE_INCREMENTER), INTENT(INOUT):: FI, FI_NEW
    !
    CALL DEALLOCATE_FILE_INCREMENTER(FI_NEW)
    !
    IF(ALLOCATED(FI%FILE)) CALL MOVE_ALLOC(FI%FILE, FI_NEW%FILE)
    IF(ALLOCATED(FI%EXT )) CALL MOVE_ALLOC(FI%EXT , FI_NEW%EXT )
    IF(ALLOCATED(FI%HED )) CALL MOVE_ALLOC(FI%HED , FI_NEW%HED )
    !
    FI_NEW%NUM       = FI%NUM                
    FI_NEW%MAXSIZE   = FI%MAXSIZE       
    FI_NEW%IU        = FI%IU                  
    FI_NEW%BUF       = FI%BUF                 
    FI_NEW%FORM      = FI%FORM          
    FI_NEW%ACCESS    = FI%ACCESS
    FI_NEW%OPENCLOSE = FI%OPENCLOSE
    FI_NEW%COUNTER    = FI%COUNTER 
    FI_NEW%MAXCOUNT   = FI%MAXCOUNT
    !
    FI%OPENCLOSE = FALSE
    !
  END SUBROUTINE
  !
  SUBROUTINE INITIALIZE_FILE_INCREMENTER_FILE(FI, MAXSIZE, FILE, BUFFER, MAXCOUNT)
    CLASS(FILE_INCREMENTER), INTENT(INOUT):: FI
    INTEGER,     INTENT(IN):: MAXSIZE
    CHARACTER(*),INTENT(IN):: FILE
    INTEGER,     INTENT(IN), OPTIONAL:: BUFFER
    INTEGER,     INTENT(IN), OPTIONAL:: MAXCOUNT
    INTEGER:: BUF, MXCNT
    IF(PRESENT(BUFFER)) THEN
                            BUF = BUFFER
    ELSE
                            BUF = Z
    END IF
    IF(PRESENT(MAXCOUNT)) THEN
                            MXCNT = MAXCOUNT
    ELSE
                            MXCNT = 10
    END IF
    !
    CALL INITIALIZE_FILE_INCREMENTER(FI, MAXSIZE, FILE=FILE, BUFFER=BUF, MAXCOUNT=MXCNT)
    !
  END SUBROUTINE
  !
  SUBROUTINE INITIALIZE_FILE_INCREMENTER_UNIT(FI, MAXSIZE, UNIT, BUFFER, MAXCOUNT)
    CLASS(FILE_INCREMENTER), INTENT(INOUT):: FI
    INTEGER,     INTENT(IN):: MAXSIZE
    INTEGER,     INTENT(IN):: UNIT
    INTEGER,     INTENT(IN), OPTIONAL:: BUFFER
    INTEGER,     INTENT(IN), OPTIONAL:: MAXCOUNT
    INTEGER:: BUF, MXCNT
    IF(PRESENT(BUFFER)) THEN
                            BUF = BUFFER
    ELSE
                            BUF = Z
    END IF
    IF(PRESENT(MAXCOUNT)) THEN
                            MXCNT = MAXCOUNT
    ELSE
                            MXCNT = 10
    END IF
    !
    CALL INITIALIZE_FILE_INCREMENTER(FI, MAXSIZE, UNIT=UNIT, BUFFER=BUF, MAXCOUNT=MXCNT)
    !
  END SUBROUTINE
  !
  SUBROUTINE INITIALIZE_FILE_INCREMENTER_UNIT_FILE(FI, MAXSIZE, UNIT, FILE,  BUFFER, MAXCOUNT)
    CLASS(FILE_INCREMENTER), INTENT(INOUT):: FI
    INTEGER,     INTENT(IN):: MAXSIZE
    INTEGER,     INTENT(IN):: UNIT
    CHARACTER(*),INTENT(IN):: FILE
    INTEGER,     INTENT(IN), OPTIONAL:: BUFFER
    INTEGER,     INTENT(IN), OPTIONAL:: MAXCOUNT
    INTEGER:: BUF, MXCNT
    IF(PRESENT(BUFFER)) THEN
                            BUF = BUFFER
    ELSE
                            BUF = Z
    END IF
    IF(PRESENT(MAXCOUNT)) THEN
                            MXCNT = MAXCOUNT
    ELSE
                            MXCNT = 10
    END IF
    !
    IF (UNIT.NE.Z)  THEN
        CALL INITIALIZE_FILE_INCREMENTER(FI, MAXSIZE, FILE, UNIT, BUFFER=BUF, MAXCOUNT=MXCNT)
    ELSE
        CALL INITIALIZE_FILE_INCREMENTER(FI, MAXSIZE, FILE=FILE, BUFFER=BUF, MAXCOUNT=MXCNT)
    END IF
    !
  END SUBROUTINE
  !
  !SUBROUTINE INITIALIZE_FILE_INCREMENTER_BOTH(FI, MAXSIZE, FILE, UNIT, BUFFER)
  !  CLASS(FILE_INCREMENTER), INTENT(INOUT):: FI
  !  INTEGER,     INTENT(IN):: MAXSIZE
  !  CHARACTER(*),INTENT(IN):: FILE
  !  INTEGER,     INTENT(IN):: UNIT
  !  INTEGER,     INTENT(IN):: BUFFER
  !  !
  !  CALL INITIALIZE_FILE_INCREMENTER(FI, MAXSIZE, FILE=FILE, UNIT=UNIT, BUFFER=BUFFER)
  !END SUBROUTINE
  !
  SUBROUTINE INITIALIZE_FILE_INCREMENTER(FI, MAXSIZE, FILE, UNIT, BUFFER, MAXCOUNT)
    CLASS(FILE_INCREMENTER), INTENT(INOUT):: FI
    INTEGER,     INTENT(IN):: MAXSIZE               !MAX FILE SIZE IN MB
    CHARACTER(*),INTENT(IN), OPTIONAL:: FILE
    INTEGER,     INTENT(IN), OPTIONAL:: UNIT
    INTEGER,     INTENT(IN):: BUFFER
    INTEGER,     INTENT(IN):: MAXCOUNT
    LOGICAL:: CHECK
    !
    INTEGER:: I, IU
    CHARACTER(:), ALLOCATABLE:: FNAME, FNAME_LONG
    CHARACTER(4):: NUM 
    !
    IF(PRESENT(FILE)) THEN
        CHECK = FILE.NE.BLNK
    ELSE
        CHECK=FALSE
    END IF
    !
    FI%BUF = BUFFER
    FI%COUNTER  = Z 
    FI%MAXCOUNT = MAXCOUNT
    !
    FI%OPENCLOSE=FALSE
    !
    IF(CHECK) THEN  !FILE PRESENT AND NON-EMPTY
        I=INDEX(FILE,'.',TRUE)
        IF(I>Z) THEN          
          ALLOCATE(FI%FILE, SOURCE=FILE(:I-1))
          ALLOCATE(FI%EXT, SOURCE=FILE(  I:))
        ELSE
          ALLOCATE(FI%FILE, SOURCE=FILE)
          ALLOCATE(FI%EXT, SOURCE='')
        END IF
        !
        !IF(PRESENT(UNIT)) THEN
        !    FI%IU=UNIT
        !    !
        !    INQUIRE(UNIT, OPENED=CHECK)
        !    IF(.NOT. CHECK) THEN
        !        OPEN(UNIT=FI%IU, FILE=FILE, ACTION='WRITE', STATUS='REPLACE', POSITION='REWIND', IOSTAT=I)
        !        IF(I.NE.0) CALL FILE_IO_ERROR(I, FNAME=FILE, UNIT=FI%IU)
        !    END IF
        !ELSE      
        !    OPEN(NEWUNIT=FI%IU,  FILE=FILE, ACTION='WRITE', STATUS='REPLACE', POSITION='REWIND', IOSTAT=I)
        !    IF(I.NE.0) CALL FILE_IO_ERROR(I, FNAME=FILE, UNIT=FI%IU)
        !END IF
        IF(PRESENT(UNIT)) THEN
            INQUIRE(UNIT, OPENED=CHECK)
            FI%IU=UNIT
        ELSE
            CHECK=FALSE
            FI%IU=Z
        END IF
        IF (.NOT. CHECK) THEN
            FI%OPENCLOSE=TRUE
            CALL GENERIC_OPEN(FILE, FI%IU, Z, ACTION='WRITE', FORM='FORMATTED', ACCESS='SEQUENTIAL', STATUS='REPLACE', BUFFER_BLOCKSIZE=FI%BUF, ASYNC='NO')   !, WARN=GET_WARN()
        END IF
        !
    ELSEIF(PRESENT(UNIT)) THEN
        FI%IU=UNIT
        ALLOCATE(CHARACTER(1000)::FNAME_LONG)
        !
        INQUIRE(UNIT, OPENED=CHECK, NAME=FNAME_LONG)
        IF(.NOT. CHECK)  WRITE(*,'(A)')'PROGRAMMING ERROR: A UNIT NUMBER WAS PASSED INTO SUBROUTINE INITIALIZE_FILE_INCREMENTER() BUT IT HAS NOT BEEN OPENED WITH A FILE NOR WAS A FILE NAME PASSED INTO THE SUBROUTINE TO ALLOW OPENING OF A FILE FOR THAT GIVEN UNIT NUMBER.'
        !CALL FILE_IO_ERROR(-1, UNIT=UNIT)
        !
        ALLOCATE(  FNAME, SOURCE=TRIM(FNAME_LONG))
        DEALLOCATE(FNAME_LONG)
        !
        I=INDEX(FNAME,'.',TRUE)
        IF(I>Z) THEN          
          ALLOCATE(FI%FILE, SOURCE=FNAME(:I-1))
          ALLOCATE(FI%EXT,  SOURCE=FNAME( I:))
        ELSE
          ALLOCATE(FI%FILE, SOURCE=FNAME)
          ALLOCATE(FI%EXT,  SOURCE='')
        END IF
        DEALLOCATE(FNAME)
    END IF
    !
    FI%NUM = Z
    IF(MAXSIZE>Z) THEN
        FI%MAXSIZE = MAXSIZE*1024
    ELSE
        FI%MAXSIZE = -1
    END IF
    !
    DO I=ONE, 5000
        IF     (I<100)  THEN
                             WRITE(NUM,'(I0.2)') I
        ELSEIF (I<1000) THEN
                             WRITE(NUM,'(I0.3)') I
        ELSE
                             WRITE(NUM,'(I4)'  ) I
        END IF
        !
        ALLOCATE(FNAME, SOURCE=FI%FILE//TRIM(NUM)//FI%EXT)
        !
        INQUIRE(FILE=FNAME, EXIST=CHECK)
        !
        IF (CHECK) THEN
            IU=Z
            INQUIRE(FILE=FNAME, OPENED=CHECK, NUMBER=IU)
            !
            IF (CHECK)  THEN
                CLOSE(IU, STATUS='DELETE')
            ELSE
                OPEN(NEWUNIT=IU, FILE=FNAME)
                CLOSE(IU, STATUS='DELETE')
            END IF
            DEALLOCATE(FNAME)
        ELSE
            EXIT
        END IF
        !
    END DO
    !
    INQUIRE(FI%IU, ACCESS=FI%ACCESS, FORM=FI%FORM)!, ASYNCHRONOUS=FI%ASYN)
    !
    !IF(FI%ASYN=='UNK'    ) FI%ASYN='NO'
    IF(FI%FORM=='UNKNOWN') FI%FORM='FORMATTED'
    !
    FI%BINARY = FI%FORM == 'UNFORMATTED' .OR.  FI%FORM == 'BINARY'
    !
  END SUBROUTINE
  !
  SUBROUTINE FILE_INCREMENTER_SET_HEADER(FI, HEADER, NOWRITE)
    CLASS(FILE_INCREMENTER), INTENT(INOUT):: FI
    CHARACTER(*),         INTENT(IN):: HEADER
    LOGICAL,     OPTIONAL,INTENT(IN):: NOWRITE
    LOGICAL:: WRITE_HED
    !
    IF(ALLOCATED(FI%HED)) DEALLOCATE(FI%HED)
                            ALLOCATE(FI%HED, SOURCE=TRIM(HEADER))
    !
    WRITE_HED = TRUE
    IF(PRESENT(NOWRITE)) WRITE_HED = .NOT. NOWRITE
    !
    IF(WRITE_HED .AND. FI%IU.NE.Z) THEN
                     IF(FI%BINARY) THEN
                          WRITE(FI%IU      ) FI%HED
                     ELSE
                          WRITE(FI%IU,'(A)') FI%HED
                     END IF
    END IF
    !
  END SUBROUTINE
  !
  SUBROUTINE FILE_INCREMENTER_SIZE_CHECK(FI, LINE, NOWRITE)  !NOTE FI%IU CHANGES DUE TO COMPILER BUG -- PROBLEM IS WITH SINGLE FILE IPO
    CLASS(FILE_INCREMENTER), INTENT(INOUT):: FI
    CHARACTER(*),  OPTIONAL, INTENT(IN   ):: LINE
    LOGICAL,       OPTIONAL, INTENT(IN   ):: NOWRITE
    !
    LOGICAL:: WRITE_HED
    INTEGER:: FSIZE
    CHARACTER(:), ALLOCATABLE:: FNAME
    CHARACTER(4):: NUM
    !
    FI%COUNTER = FI%COUNTER + ONE
    !
    IF( FI%MAXSIZE>Z .AND. FI%COUNTER.GE.FI%MAXCOUNT ) THEN
       !
       FI%COUNTER = Z
       !
       INQUIRE(FI%IU,SIZE=FSIZE)    !Assumes size returns 1 byte icrements file unit storage ( USE ISO_FORTRAN_ENV, ONLY : FILE_STORAGE_SIZE  => 8 bit/file storage unit)
       FSIZE=FSIZE/1024             !SIZE in KB  1 KB = 1024 B
       !
       IF (FSIZE>FI%MAXSIZE) THEN
                                 CALL FORCE_UNIT_CLOSE(FI%IU)  !SEPARTE IF TO GIVE TIME FOR BUFFER TO WRITE TO CLOSED FILE
       END IF
       !
       IF (FSIZE>FI%MAXSIZE) THEN
           !
           !FI%IU = Z  !--UNCOMENT IF THERE IS A PROBLEM WITH UNIT CLAIMING FILE ALREADY OPEN
           FI%NUM = FI%NUM + ONE
           !
           IF     (FI%NUM<100)  THEN
                                    WRITE(NUM,'(I0.2)') FI%NUM
           ELSEIF (FI%NUM<1000) THEN
                                    WRITE(NUM,'(I0.3)') FI%NUM
           ELSE
                                    WRITE(NUM,'(I4)'  ) FI%NUM
           END IF
           !
           ALLOCATE(FNAME, SOURCE=FI%FILE//TRIM(NUM)//FI%EXT)
           !
           CALL GENERIC_OPEN(FNAME, FI%IU, Z, ACTION='WRITE', FORM=FI%FORM, ACCESS=FI%ACCESS, STATUS='REPLACE', BUFFER_BLOCKSIZE=FI%BUF, ASYNC='NO')   !, WARN=GET_WARN()
           !OPEN(UNIT=FI%IU,  FILE=FNAME, ACTION='WRITE', STATUS='REPLACE', POSITION='REWIND', IOSTAT=I)
           !IF(I.NE.Z) CALL FILE_IO_ERROR(I, FNAME=FNAME, UNIT=FI%IU)
           !
           IF(PRESENT(LINE)) THEN
                     IF(FI%BINARY) THEN
                                            WRITE(FI%IU      ) LINE
                     ELSEIF(LINE.NE.BLNK) THEN
                                            WRITE(FI%IU,'(A)') TRIM(LINE)
                     END IF
           END IF
           !
           WRITE_HED = TRUE
           IF(PRESENT(NOWRITE)) WRITE_HED = .NOT. NOWRITE
           !
           IF( WRITE_HED .AND. ALLOCATED(FI%HED) ) THEN
                     IF(FI%BINARY) THEN
                          WRITE(FI%IU      ) FI%HED
                     ELSE
                          WRITE(FI%IU,'(A)') FI%HED
                     END IF
           END IF
           !
           DEALLOCATE(FNAME)
       END IF
       !
    END IF
    !
  END SUBROUTINE   
  !
  SUBROUTINE DEALLOCATE_FILE_INCREMENTER(FI)
    CLASS(FILE_INCREMENTER), INTENT(INOUT):: FI
    !
    IF (ALLOCATED(FI%HED))  DEALLOCATE(FI%HED)
    IF (ALLOCATED(FI%FILE)) DEALLOCATE(FI%FILE)
    IF (ALLOCATED(FI%EXT))  DEALLOCATE(FI%EXT)
    IF(FI%OPENCLOSE) CLOSE(FI%IU)
    !
  END SUBROUTINE 
  !
  SUBROUTINE FINAL_DEALLOCATE_FILE_INCREMENTER(FI)
    TYPE(FILE_INCREMENTER), INTENT(INOUT):: FI
    !
    IF (ALLOCATED(FI%HED))  DEALLOCATE(FI%HED)
    IF (ALLOCATED(FI%FILE)) DEALLOCATE(FI%FILE)
    IF (ALLOCATED(FI%EXT))  DEALLOCATE(FI%EXT)
    IF(FI%OPENCLOSE) CLOSE(FI%IU)
    !
  END SUBROUTINE       
  !
END MODULE
!
!  
