!
! CODE DEVELOPED BY SCOTT E BOYCE 
!                   CONTACT <seboyce@usgs.gov> or <Boyce@engineer.com>
!  
!  Error, Warning, and Pause routines
!
MODULE ERROR_INTERFACE
  !
  !USE ISO_FORTRAN_ENV,   ONLY: stderr => ERROR_UNIT
  !  
  USE CONSTANTS,         ONLY: Z, ONE, QUIN, TRUE, FALSE, BLNK, NL, BLN!DNEG, UNO, SUB_ONE, NEAR_ONE, HALF, TRES, NEARZERO_29
  !
  USE NUM2STR_INTERFACE, ONLY: NUM2STR
  !
  IMPLICIT NONE
  !
  INTEGER, SAVE:: WARN_IU = Z   ! UNIT IS SET BY WARNING ROUTINE AND PROVIDES SEPARATE FILE FOR STOP, FILEIO, AND WARN ROUTINES TO WRITE TOO
  !
  PRIVATE
  !
  PUBLIC:: STOP_ERROR, FILE_IO_ERROR, WARNING_MESSAGE, PAUSE, SET_WARN_UNIT
  !
  PUBLIC:: GET_WARN, CLOSE_WARNING_UNIT, RESET_WARN_UNIT
  !
  PUBLIC:: GAME_OVER, EPIC_FAIL
  !
  PUBLIC:: GET_FILE_NAME             !GET_FILE_NAME(IU, FNAME, SET_FNAME)
  !
  CONTAINS
  !
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !  
  !  Error and Warning routines
  !
  !
  !#########################################################################################################################
  !
  SUBROUTINE CLOSE_WARNING_UNIT()             ! Close WARN_IU and Zero it out
    INTEGER:: I
    IF(WARN_IU /= Z) CLOSE(WARN_IU, IOSTAT=I)
    WARN_IU = Z
  END SUBROUTINE
  !
  !-------------------------------------------------------------------------------------------------------------------------
  !
  SUBROUTINE RESET_WARN_UNIT(CLOSE_WARN)      ! Zero out WARN_IU and optionally close it
    LOGICAL, OPTIONAL, INTENT(IN):: CLOSE_WARN
    INTEGER:: I
    !
    IF(WARN_IU /= Z .AND. PRESENT(CLOSE_WARN)) THEN
                              IF( CLOSE_WARN ) CLOSE(WARN_IU, IOSTAT=I)
    END IF
    WARN_IU = Z
    !
  END SUBROUTINE
  !
  !-------------------------------------------------------------------------------------------------------------------------
  !
  PURE FUNCTION GET_WARN() RESULT(IU)
    INTEGER:: IU
    IU = WARN_IU
  END FUNCTION
  !
  !-------------------------------------------------------------------------------------------------------------------------
  !
  SUBROUTINE SET_WARN_UNIT(IU)  !CAN ALSO SET WITH CALL WARNING_MESSAGE(OUTPUT=IU, SET_UNIT=TRUE)  -- Not should only call once per simulation
    INTEGER,      INTENT(IN):: IU
    WARN_IU = IU
    IF(WARN_IU /= Z) THEN
                     WRITE(WARN_IU,'(A)') BLN//'                           WARNING FILE'//BLN//'         THE FOLLOWING COMMENTS WERE PASSED TO THE WARNING ROUTINE'//BLN//REPEAT('#',80)//BLN
    END IF
    !
  END SUBROUTINE
  !
  !#########################################################################################################################
  !
  SUBROUTINE WARNING_MESSAGE(LINE, INFILE, OUTPUT, MSG, INLINE, CMD_PRINT, SET_UNIT, KPER)
    INTEGER,      INTENT(IN), OPTIONAL:: INFILE
    CHARACTER(*), INTENT(IN), OPTIONAL:: LINE
    CHARACTER(*), INTENT(IN), OPTIONAL:: MSG
    INTEGER,      INTENT(IN), OPTIONAL:: OUTPUT       !UNIT TO WRITE WARNING TOO, MAY ALSO SET WARN_IU
    LOGICAL,      INTENT(IN), OPTIONAL:: INLINE       !IF TRUE, WARNING IS  WRITTEN TO ONE LINE
    LOGICAL,      INTENT(IN), OPTIONAL:: CMD_PRINT    !IF TRUE, WARNING IS  WRITTEN TO CMD PROMPT
    LOGICAL,      INTENT(IN), OPTIONAL:: SET_UNIT     !IF TRUE, THEN ONLY SETS OUTPUT TO WARN_IU AND RETURNS
    INTEGER,      INTENT(IN), OPTIONAL:: KPER         !RESETS WARNING HEADER TO NEW STRESS PERIOD, NEXT CALLED WARNING WILL WRITE HEADER
    !
    INTEGER, SAVE:: SP_NUM = Z
    LOGICAL, SAVE:: WRITE_HEADER = FALSE
    !
    CHARACTER(:),ALLOCATABLE :: WARN, FNAME
    INTEGER:: IOUT, ICLIP
    LOGICAL:: CMD_PRN, ONE_LINE, CHECK
    !
    ! Not that changes affect WRITE(WARN_IU,'(A)') WARN(ICLIP:LEN(WARN)-1), which is used for the WARN file to prevent print a header twice
    !
    !
    IOUT = Z
    IF(PRESENT(OUTPUT)) IOUT = OUTPUT
    !
    IF(PRESENT(SET_UNIT)) THEN
          IF(SET_UNIT) THEN
                           WARN_IU = IOUT
                           IF(WARN_IU /= Z) WRITE(WARN_IU,'(A)') BLN//'                           WARNING FILE'//BLN//'         THE FOLLOWING COMMENTS WERE PASSED TO THE WARNING ROUTINE'//BLN//REPEAT('#',80)//BLN
                           RETURN
          END IF
    END IF
    !
    IF(PRESENT(KPER)) THEN
         IF(WARN_IU /= Z .AND. WARN_IU /= IOUT) THEN
             SP_NUM = KPER
             WRITE_HEADER = TRUE
         END IF
         RETURN
    END IF
    !
    IF(WRITE_HEADER) THEN
       WRITE_HEADER = FALSE
       WRITE(WARN_IU,'(A)') BLN//REPEAT('>',104)//NL//REPEAT('<',104)//BLN//REPEAT(BLNK,33)//'WARNINGS FOR STRESS PERIOD '//NUM2STR(SP_NUM)//BLN//REPEAT('<',104)//NL//REPEAT('>',104)//BLN//BLN//REPEAT('#',104)//NL
    END IF
    !
    CMD_PRN = FALSE
    IF(PRESENT(CMD_PRINT)) CMD_PRN = CMD_PRINT
    !
    ONE_LINE = FALSE
    IF(PRESENT(INLINE)) ONE_LINE = INLINE
    !
    IF(ONE_LINE) THEN
        WARN=NL//'WARNING: '
        IF(PRESENT(MSG )) THEN; IF(MSG  /= BLNK) WARN = WARN//TRIM(MSG)//BLNK
        END IF
        IF(PRESENT(LINE)) THEN; IF(LINE /= BLNK) WARN = WARN//'FROM PROCESSING LINE "'//TRIM(ADJUSTL(LINE))//'" '
        END IF
        IF(PRESENT(INFILE)) THEN
           IF(INFILE /= Z) THEN 
              CALL GET_FILE_NAME(INFILE,FNAME,CHECK) 
              IF(CHECK) WARN = WARN//'FROM FILE "'//TRIM(FNAME)//'" '
              DEALLOCATE(FNAME)
           END IF
        END IF
    ELSE
        WARN=BLN//'                           WARNING_MESSAGE'//BLN//'         THE FOLLOWING COMMENTS WERE PASSED TO THE WARNING ROUTINE'//NL
        !
        ICLIP = LEN(WARN) + 106  !The 106 is for BLN//REPEAT('#',104)//
        !
        IF(PRESENT(INFILE)) THEN
           IF(INFILE /= Z) THEN
              CALL GET_FILE_NAME(INFILE,FNAME,CHECK) 
              IF(CHECK) WARN = WARN//NL//'THE WARNING IS BELIEVED TO HAVE ORIGINATED FROM THE FOLLOWING FILE:'//NL//'"'//TRIM(FNAME)//'"'//NL
              DEALLOCATE(FNAME)
           END IF
        END IF
        IF(PRESENT(LINE)) THEN; IF(LINE /= BLNK) WARN = WARN//NL//'THE GUESSED LINE THAT THE WARNING OCCURED ON IS:'//NL//'"'//TRIM(LINE)//'"'//NL
        END IF
        IF(PRESENT(MSG )) THEN; IF(MSG  /= BLNK) WARN = WARN//NL//'THE DESCRIPTION OF THE WARNING IS:'//BLN//TRIM(MSG)//NL
        END IF
        !
        WARN = BLN//REPEAT('#',104)//WARN//NL//REPEAT('#',104)//BLN
    END IF
    !
    IF(WARN_IU /= Z .AND. WARN_IU /= IOUT) THEN
        IF(ONE_LINE) THEN
            WRITE(WARN_IU,'(A///,A//)') WARN,REPEAT('#',104)
        ELSE
            WRITE(WARN_IU,'(A)') WARN(ICLIP:LEN(WARN)-1)
        END IF
    END IF
    !
    IF(IOUT /= Z) WRITE(IOUT,'(A/)') WARN
    IF(CMD_PRN  ) WRITE(*,   '(A/)') WARN
  END SUBROUTINE
  !
  !#########################################################################################################################
  !
  SUBROUTINE GET_FILE_NAME(IU,FNAME,SET_FNAME) 
    INTEGER,                 INTENT(IN ):: IU                ! UNIT NUMBER TO LOOK FILE NAME UP FROM
    CHARACTER(:),ALLOCATABLE,INTENT(OUT):: FNAME             ! FILE NAME ASSOCIATED WITH UNIT NUMBER
    LOGICAL,                 INTENT(OUT):: SET_FNAME         ! SET TO TRUE IF THERE IS AN ERROR
    CHARACTER(:), ALLOCATABLE:: FNAM
    INTEGER:: I, SIZ
    LOGICAL:: CHECK
    !
    IF(IU == Z) THEN
        FNAME = '"INTERNAL FILE" with unit number set to zero. Did you use "INTERNAL" when you should specify a file name?'
        SET_FNAME = FALSE
    ELSE
       ALLOCATE(CHARACTER(256):: FNAM)
       INQUIRE(IU, NAME=FNAM, EXIST=CHECK)
       !
       IF(CHECK) THEN
             INQUIRE(FILE=FNAM, EXIST=CHECK)  !CHECK IF FILE NAME SIZE IS BIG ENOUGH
             IF(.NOT. CHECK) THEN
                   DO I=ONE, 15
                     IF(CHECK) THEN
                                   EXIT
                     ELSE
                         SIZ = 600 * I
                         DEALLOCATE(FNAM)
                         ALLOCATE(CHARACTER(SIZ):: FNAM)
                         INQUIRE(IU, NAME=FNAM)
                         INQUIRE(FILE=FNAM, EXIST=CHECK)  !CHECK IF FILE NAME SIZE IS BIG ENOUGH
                     END IF
                   END DO
             END IF
             !
             I = Z
             IF(CHECK) I = LEN_TRIM(FNAM)
             !
             IF(I > Z) THEN
                   ALLOCATE(FNAME, SOURCE=FNAM(ONE:I))
                   SET_FNAME = TRUE
             ELSE
                   FNAME = 'GET_FILE_NAME ERROR: Failed to identy file name from unit number '//NUM2STR(IU)
                   SET_FNAME = FALSE
             END IF
             !
       ELSE
           FNAME = '"UNKNOWN FILE" Failed to identy file name from unit number '//NUM2STR(IU)
           SET_FNAME = FALSE
       END IF
    END IF
    !
  END SUBROUTINE
  !
  !#########################################################################################################################
  !
  SUBROUTINE STOP_ERROR(LINE, INFILE, OUTPUT, MSG, MSG2, STAT, GUESS)
    CHARACTER(*), INTENT(IN), OPTIONAL:: LINE       ! Line that error occured on
    INTEGER,      INTENT(IN), OPTIONAL:: INFILE     ! File Unit that error originated from
    INTEGER,      INTENT(IN), OPTIONAL:: OUTPUT     ! File unit to write error too
    CHARACTER(*), INTENT(IN), OPTIONAL:: MSG, MSG2  ! Supplemental messages to write in error
    INTEGER,      INTENT(IN), OPTIONAL:: STAT       ! IOSTAT or STAT number associated with error (STAT=0 disables ERROR STOP)
    LOGICAL,      INTENT(IN), OPTIONAL:: GUESS      ! If set to TRUE, then the line is guessed as being the previous line in INFILE
    !
    CHARACTER(:), ALLOCATABLE:: FNAME
    CHARACTER(:), ALLOCATABLE:: ERR
    !
    LOGICAL:: HAS_LINE, HAS_INFILE, HAS_OUTPUT, GET_LINE, HAS_MSG, HAS_MSG2
    !
    INTEGER:: IOUT, IE
    !
    ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    !
    IF(PRESENT(STAT)) THEN; IF(STAT == Z) RETURN
    END IF
    !
    ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    !
    HAS_LINE = PRESENT(LINE)
    !
    IF(HAS_LINE) THEN; IF(LINE == BLNK) HAS_LINE = FALSE
    END IF
    !
    HAS_INFILE = PRESENT(INFILE)
    !
    IF(HAS_INFILE) THEN; IF(INFILE == Z) HAS_INFILE = FALSE
    END IF
    !
    HAS_OUTPUT = PRESENT(OUTPUT)
    !
    IF(HAS_OUTPUT) THEN; IF(OUTPUT == Z) HAS_OUTPUT = FALSE
    END IF
    !
    GET_LINE = FALSE
    IF( PRESENT(GUESS) )  GET_LINE = GUESS .AND. HAS_INFILE
    !
    HAS_MSG = FALSE
    IF( PRESENT(MSG) )  HAS_MSG = MSG  /= BLNK
    !
    HAS_MSG2 = FALSE
    IF( PRESENT(MSG2) )  HAS_MSG2 = MSG2  /= BLNK
    !
    IF(HAS_INFILE .OR. GET_LINE) ALLOCATE(CHARACTER(768):: FNAME)
    !
    ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    !
    ERR=NL//NL//'                           ERROR'//BLN//'         THE FOLLOWING COMMENTS WERE PASSED TO THE ERROR ROUTINE'//NL
    !
    IF(HAS_INFILE) THEN
        INQUIRE(INFILE,NAME=FNAME)
        ERR = ERR//NL//'THIS ERROR IS BELIEVED TO HAVE ORIGINATED FROM THE FOLLOWING FILE:'//NL//'"'//TRIM(FNAME)//'"'//NL
    END IF
    !
    IF(HAS_LINE) ERR = ERR//NL//'THE GUESSED LINE THAT THE ERROR OCCURED ON IS:'//BLN//'"'//TRIM(LINE)//'"'//NL
    !
    IF(GET_LINE) THEN
                 FNAME(:) = BLNK
                 BACKSPACE(INFILE, IOSTAT=IE)
                 IF(IE /= Z) READ(INFILE, "(A)", IOSTAT=IE) FNAME
                 !
                 IF(IE /= Z .AND. FNAME /= BLNK) ERR = ERR//NL//"THE GUESSED PREVIOUS INPUT FILE'S LINE THAT WAS READ AND MAY CONTAIN THE ERROR IS:"//BLN//'"'//TRIM(FNAME)//'"'//NL 
    END IF
    !
    IF(PRESENT(STAT)) THEN
                      ERR = ERR//NL//'THE FORTRAN ERROR STATUS CODE IS '//NUM2STR(STAT)//NL
                      !
                      IF(STAT<Z) ERR = ERR//NL//'    **NOTE THAT A STATUS CODE <0 INDICATES THAT THE END OF FILE WAS REACHED'//NL//        &
                                                '      OR A END OF RECORD CONDITION OCCURED'//NL//                                         &
                                                '      OR YOU DID NOT HAVE ENOUGH INPUT VALUES SPECIFIED ON LINE.'//NL//                   &
                                                '      NOTE THAT IF YOU HAVE THE CORRECT NUMBER OF VALUES ON THE LINE'//NL//               &
                                                '        AND USED THE KEWYORD INTERNAL TO LOAD THEM,'//NL//                                &
                                                '        TRY MOVING INPUT TO SEPARATE FILE AND USE OPEN/CLOSE TO LOAD DATA.'//NL//         &
                                                '        THE BUFFER FOR AN INTERNAL LOAD MAY NOT BE SUFFICIENT TO LOAD THE ENTIRE LINE.'//NL
    END IF
    !
    IF(    HAS_MSG .AND. HAS_MSG2 )THEN
                                          ERR = ERR//NL//'THE DESCRIPTION OF THE ERROR IS:'//BLN// TRIM(MSG)//BLN//  TRIM(MSG2)//NL
    ELSEIF(HAS_MSG                )THEN
                                          ERR = ERR//NL//'THE DESCRIPTION OF THE ERROR IS:'//BLN//TRIM(MSG )//NL
    ELSEIF(HAS_MSG2               )THEN
                                          ERR = ERR//NL//'THE DESCRIPTION OF THE ERROR IS:'//BLN//TRIM(MSG2)//NL
    END IF
    !
    IF( .NOT. (HAS_LINE .OR. HAS_INFILE .OR. GET_LINE .OR. HAS_MSG .OR. HAS_MSG2) ) ERR = ERR//BLN//' --- SORRY UNKNOWN ERROR ---'//NL//NL
    !
    ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    !
    WRITE(*,   '(A/)') ERR  !WRITE TO CMD PROMPT FIRST
    !
    IOUT = Z
    IF(PRESENT(OUTPUT)) IOUT = OUTPUT
    !    
    IF(WARN_IU /= Z .AND. WARN_IU /= IOUT) THEN
                                           WRITE(WARN_IU,'(/A/)') ERR
                                           CALL EPIC_FAIL(WARN_IU)
    END IF
    !
    IF(IOUT /= Z) WRITE(IOUT,'(A/)') ERR
    !
    IF(IOUT /= Z) CALL GAME_OVER(IOUT)
    !
    ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    !
    ERROR STOP ! :(
    !
  END SUBROUTINE
  !
  !#########################################################################################################################
  !
  SUBROUTINE FILE_IO_ERROR(IOSTAT,UNIT,FNAME,LINE,INFILE,OUTPUT,MSG,MSG2)
    ! IOSTAT  IS THE ASSOCIATED IOSTAT ERROR
    ! UNIT   IS THE FILE THAT IS BEING OPENED, READ FROM, OR WRITTEN TOO
    ! LINE   IS THE LINE THAT IS BEING PROCESSED EITHER FOR PARSING THE INPUT FILE, READING DATA, OR WRITING DATA
    ! INFILE IS THE FILE FROM WHICH LINE ORIGINATED FROM. IT CAN BE THE SAME FILE AS FNAME
    ! MSG    IS AN ADDITIONAL ERROR MESSAGE THAT IS APPEND TO THE END OF THE ERROR REPORT
    ! MSG2   IS AN ADDITIONAL ERROR MESSAGE THAT IS APPEND TO THE END OF MSG. USED IN CASE OF SUBROUTINE THAT CALLS FILE_IO_ERROR HAS ARGUMENT THAT IS MSG
    !
    INTEGER,     OPTIONAL:: IOSTAT, UNIT,INFILE,OUTPUT
    CHARACTER(*),OPTIONAL:: FNAME,LINE,MSG,MSG2  !MSG2 allows for 
    !
    INTEGER:: IU,IFILE,IOUT
    CHARACTER(QUIN):: LN
    CHARACTER(:),ALLOCATABLE:: ERRMSG,FN,ERRLINE,INLINE,MSGLINE, ERR_CODE
    LOGICAL::ISOPEN
    !
    IOUT=Z
    IF(PRESENT(OUTPUT)) IOUT=OUTPUT
    !
    IFILE=Z
    INLINE='¿¿¿UNKOWN FILE???'
    IF(PRESENT(INFILE)) THEN
        IF (INFILE /= Z) THEN
           IFILE=INFILE
           INQUIRE(INFILE,NAME=LN,OPENED=ISOPEN)
           IF(ISOPEN) INLINE=TRIM(ADJUSTL(LN))
        END IF
    END IF
    !
    IF(PRESENT(LINE))THEN
        IF(LINE /= BLNK) THEN
            ERRLINE=TRIM(ADJUSTL(LINE))
        ELSE
            ERRLINE='*** EMPTY/BLANK LINE LOADED ***'
        END IF
    ELSE
        ERRLINE='¿¿¿UNKOWN LINE???'
    END IF
    !
    IF(          PRESENT(FNAME).AND. .NOT. PRESENT(UNIT)) THEN
        FN=TRIM(ADJUSTL(FNAME))
        INQUIRE(FILE=FN,NUMBER=IU,OPENED=ISOPEN)
        IF(IU.EQ.-1) IU=Z
    ELSEIF(.NOT. PRESENT(FNAME).AND.       PRESENT(UNIT))THEN
        IU=UNIT
        INQUIRE(IU,NAME=LN,OPENED=ISOPEN)
        FN=TRIM(ADJUSTL(LN))
    ELSEIF(.NOT. PRESENT(FNAME).AND. .NOT. PRESENT(UNIT))THEN
        IU=Z
        FN=BLNK!' '
        ISOPEN=FALSE
    ELSE
        IU=UNIT
        FN=TRIM(ADJUSTL(FNAME))
        INQUIRE(IU, OPENED=ISOPEN)
        IF(.NOT. ISOPEN) INQUIRE(FILE=FN,OPENED=ISOPEN)
    END IF
    !
    IF(    PRESENT(MSG) .AND. PRESENT(MSG2) )THEN
                                             MSGLINE=TRIM(ADJUSTL(MSG)) //BLN// TRIM(ADJUSTL(MSG2))
    ELSEIF(PRESENT(MSG))THEN
                                             MSGLINE=TRIM(ADJUSTL(MSG))
    ELSEIF(PRESENT(MSG2))THEN
                                             MSGLINE=TRIM(ADJUSTL(MSG2))
    ELSE
        MSGLINE=BLNK
    END IF
    !
    IF(PRESENT(IOSTAT)) THEN
        IF(IOSTAT  /=  Z) THEN
            ERR_CODE='AND HAS THE FOLLOWING IOSTAT ERROR CODE: '//NUM2STR(IOSTAT)
            IF(ISOPEN) THEN
              IF(IOSTAT<Z) THEN
                  ERR_CODE=ERR_CODE//BLN//'    ERROR<0 INDICATES THAT THE END OF FILE WAS REACHED'//NL//'    OR A END OF RECORD CONDITION OCCURED'//NL//'    OR YOU DID NOT HAVE ENOUGH INPUT VALUES SPECIFIED ON LINE.'//NL//'    NOTE THAT IF YOU HAVE THE CORRECT NUMBER OF VALUES ON THE LINE'//NL//'      AND USED THE KEWYORD INTERNAL TO LOAD THEM,'//NL//'      TRY MOVING INPUT TO SEPARATE FILE AND USE OPEN/CLOSE TO LOAD DATA.'//NL//'      THE BUFFER FOR AN INTERNAL LOAD MAY NOT BE SUFFICIENT TO LOAD THE ENTIRE LINE.'
                      
              ELSE
                  ERR_CODE=ERR_CODE//BLN//'    ERROR>0 INDICATES THAT YOU HAVE TO LOOK UP THE SPECIFIC ERROR CONDITION SPECIFIED BY THE COMPILER.' 
              END IF
            END IF
            !
        ELSE
            ERR_CODE=BLNK
        END IF
    ELSE
            ERR_CODE=BLNK
    END IF
    !
    IF(IFILE.EQ.Z) THEN
       !
       !
       IF(ISOPEN) THEN
           ERRMSG=NL//'FILE I/O ERROR:'                                         //BLN//       &
           'FOR FILE UNIT '//NUM2STR(IU)                                         //BLN//       &
           'WHICH IS ASSOCIATED WITH FILE: '//FN                                 //BLN//       &
           'WHILE READING OR WRITING LINE '//NL//'"'//ERRLINE//'"'               //BLN//       &
            ERR_CODE
       ELSEIF( .NOT. ISOPEN .AND. (IU /= Z .OR. FN /= BLNK) )THEN
           IF(IU.EQ.Z) THEN
         ERRMSG=NL//'FILE I/O ERROR:'                                               //BLN//  &
                    'FOR AN UNKNOWN FILE UNIT [POSSIBLE FAILURE TO OPEN/FIND FILE]'  //BLN//  &
                    'FOR THE REQUESTED FILE NAME: '//FN                              //BLN//  &
                    ERR_CODE
           ELSEIF (FN /= BLNK) THEN
         ERRMSG=NL//'FILE I/O ERROR:'                                                      //BLN//  &
                    'FOR FILE UNIT '//NUM2STR(IU)//' [POSSIBLE FAILURE TO OPEN/FIND FILE]' //BLN//  &
                    'FOR THE REQUESTED FILE NAME: '//NL//FN                               //BLN//  &
                    ERR_CODE
           ELSE
         ERRMSG=NL//'FILE I/O ERROR:'                                                     //BLN//   &
                    'FOR FILE UNIT '//NUM2STR(IU)//' [POSSIBLE FAILURE TO OPEN/FIND FILE]' //BLN//  &
                    'WHICH HAS NO FILE NAME ASSOCIATED WITH IT'                            //BLN//  &
                    ERR_CODE
           END IF          
       ELSE
         ERRMSG=NL//'FILE I/O ERROR:'                                                        //BLN//  &
                    'FOR AN UNKNOWN FILE UNIT AND FILE [POSSIBLE FAILURE TO OPEN/FIND FILE]'  //BLN//  &
                     ERR_CODE          //NL//  &
                    'FOR THE FOLLOWING LINE '//NL//'"'//ERRLINE//'"'
       END IF
       !
       !
    ELSE
       !
       !
       IF(ISOPEN) THEN
           ERRMSG=NL//'FILE I/O ERROR:'                                                      //BLN//        &
                      'FOR FILE UNIT '//NUM2STR(IU)                                          //BLN//        &
                      'WHICH IS ASSOCIATED WITH FILE: '//NL//FN                              //BLN//        &
                      'WHILE UTILIZING THE FOLLOWING LINE: '//NL//'"'//ERRLINE//'"'          //BLN//       &
                      'THAT IS ASSOCIATED WITH INPUT FILE: '//NL//'"'//INLINE//'"'           //BLN//       &
                      ERR_CODE                                                                
       ELSEIF( .NOT. ISOPEN .AND. (IU /= Z .OR. FN /= BLNK) )THEN
           IF(IU.EQ.Z) THEN
         ERRMSG=NL//'FILE I/O ERROR:'                                                //BLN//  &
                    'FOR AN UNKNOWN FILE UNIT [POSSIBLE FAILURE TO OPEN/FIND FILE]'  //BLN//  &
                    'FOR THE REQUESTED FILE NAME: '//NL//FN                          //BLN//  &
                    ERR_CODE                                                         //BLN//  &
                    'WHILE UTILIZING THE FOLLOWING LINE: '//NL//'"'//ERRLINE//'"'    //BLN//  &
                    'THAT IS ASSOCIATED WITH INPUT FILE: '//NL//'"'//INLINE//'"'
           ELSEIF (FN /= BLNK) THEN
         ERRMSG=NL//'FILE I/O ERROR:'                                                      //BLN//  &
                    'FOR FILE UNIT '//NUM2STR(IU)//' [POSSIBLE FAILURE TO OPEN/FIND FILE]' //BLN//  &
                    'FOR THE REQUESTED FILE NAME: '//NL//FN                                //BLN//  &
                    ERR_CODE
           ELSE
         ERRMSG=NL//'FILE I/O ERROR:'                                                     //BLN//   &
                    'FOR FILE UNIT '//NUM2STR(IU)//' [POSSIBLE FAILURE TO OPEN/FIND FILE]' //BLN//  &
                    'WHICH HAS NO FILE NAME ASSOCIATED WITH IT'                            //BLN//  &
                    ERR_CODE
           END IF          
       ELSE
         ERRMSG=NL//'FILE I/O ERROR:'                                                       //BLN//  &
                    'FOR AN UNKNOWN FILE UNIT AND FILE [POSSIBLE FAILURE TO OPEN/FIND FILE]' //BLN//  &
                    ERR_CODE                                                                 //BLN//  &
                    'WHILE UTILIZING THE FOLLOWING LINE: '//NL//'"'//ERRLINE//'"'            //BLN// &
                    'THAT IS ASSOCIATED WITH INPUT FILE: '//NL//'"'//INLINE//'"'
       END IF
       !
       !
    END IF
    !
    IF(MSGLINE /= BLNK) ERRMSG=ERRMSG//BLN//'THE FOLLOWING IS AN ADDITIONAL COMMENT INCLUDED WITH ERROR:'//BLN//MSGLINE
    !
    IF(WARN_IU /= Z .AND. WARN_IU /= IOUT) THEN
                                           WRITE(WARN_IU,'(/A/)') ERRMSG
                                           CALL EPIC_FAIL(WARN_IU)
    END IF
    !
    IF(IOUT /= Z) WRITE(IOUT,'(A)') ERRMSG
                  WRITE(*,   '(A)') ERRMSG
    !
    IF(IOUT /= Z) CALL GAME_OVER(IOUT)
    !
    ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    !
    ERROR STOP ! ;(
    !
  END SUBROUTINE
  !
  !#########################################################################################################################
  !
  SUBROUTINE PAUSE(LINE)
    USE, INTRINSIC:: ISO_FORTRAN_ENV, ONLY: STDIN=>INPUT_UNIT, STDOUT=>OUTPUT_UNIT
    CHARACTER(*), INTENT(IN), OPTIONAL:: LINE
    !
    IF(PRESENT(LINE)) THEN
        WRITE(STDOUT,'(/A/)') LINE
    ELSE
        WRITE(STDOUT,'(//A//)') 'PAUSED - Press Enter to Continue'
    END IF
    !
    READ(STDIN,*) 
    !
  END SUBROUTINE
  !
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !  
  !  GAME_OVER  routine
  !
  SUBROUTINE GAME_OVER(IOUT)
    INTEGER, INTENT(IN):: IOUT
    !
    IF(IOUT /= Z) WRITE(IOUT,'(///A)')                                                                                                                   &
    '             GGGGGGGGGGGGG                    AAA                    MMMMMMMM               MMMMMMMM     EEEEEEEEEEEEEEEEEEEEEE'//NL//  &
    '          GGG::::::::::::G                   A:::A                   M:::::::M             M:::::::M     E::::::::::::::::::::E'//NL//  &
    '        GG:::::::::::::::G                  A:::::A                  M::::::::M           M::::::::M     E::::::::::::::::::::E'//NL//  &
    '       G:::::GGGGGGGG::::G                 A:::::::A                 M:::::::::M         M:::::::::M     EE::::::EEEEEEEEE::::E'//NL//  &
    '      G:::::G       GGGGGG                A:::::::::A                M::::::::::M       M::::::::::M       E:::::E       EEEEEE'//NL//  &
    '     G:::::G                             A:::::A:::::A               M:::::::::::M     M:::::::::::M       E:::::E             '//NL//  &
    '     G:::::G                            A:::::A A:::::A              M:::::::M::::M   M::::M:::::::M       E::::::EEEEEEEEEE   '//NL//  &
    '     G:::::G    GGGGGGGGGG             A:::::A   A:::::A             M::::::M M::::M M::::M M::::::M       E:::::::::::::::E   '//NL//  &
    '     G:::::G    G::::::::G            A:::::A     A:::::A            M::::::M  M::::M::::M  M::::::M       E:::::::::::::::E   '//NL//  &
    '     G:::::G    GGGGG::::G           A:::::AAAAAAAAA:::::A           M::::::M   M:::::::M   M::::::M       E::::::EEEEEEEEEE   '//NL//  &
    '     G:::::G        G::::G          A:::::::::::::::::::::A          M::::::M    M:::::M    M::::::M       E:::::E             '//NL//  &
    '      G:::::G       G::::G         A:::::AAAAAAAAAAAAA:::::A         M::::::M     MMMMM     M::::::M       E:::::E       EEEEEE'//NL//  &
    '       G:::::GGGGGGGG::::G        A:::::A             A:::::A        M::::::M               M::::::M     EE::::::EEEEEEEE:::::E'//NL//  &
    '        GG:::::::::::::::G       A:::::A               A:::::A       M::::::M               M::::::M     E::::::::::::::::::::E'//NL//  &
    '          GGG::::::GGG:::G      A:::::A                 A:::::A      M::::::M               M::::::M     E::::::::::::::::::::E'//NL//  &
    '             GGGGGG   GGGG     AAAAAAA                   AAAAAAA     MMMMMMMM               MMMMMMMM     EEEEEEEEEEEEEEEEEEEEEE'//NL//  &
    ' '//NL//  &
    ' '//NL//  &
    ' '//NL//  &
    ' '//NL//  &
    ' '//NL//  &
    ' '//NL//  &
    ' '//NL//  &
    ' '//NL//  &
    ' '//NL//  &
    '          OOOOOOOOO               VVVVVVVV           VVVVVVVV          EEEEEEEEEEEEEEEEEEEEEE          RRRRRRRRRRRRRRRRR       '//NL//  &
    '        OO:::::::::OO             V::::::V           V::::::V          E::::::::::::::::::::E          R::::::::::::::::R      '//NL//  &
    '      OO:::::::::::::OO           V::::::V           V::::::V          E::::::::::::::::::::E          R::::::RRRRRR:::::R     '//NL//  &
    '     O:::::::OOO:::::::O          V::::::V           V::::::V          EE::::::EEEEEEEEE::::E          RR:::::R     R:::::R    '//NL//  &
    '     O::::::O   O::::::O           V:::::V           V:::::V             E:::::E       EEEEEE            R::::R     R:::::R    '//NL//  &
    '     O:::::O     O:::::O            V:::::V         V:::::V              E:::::E                         R::::R     R:::::R    '//NL//  &
    '     O:::::O     O:::::O             V:::::V       V:::::V               E::::::EEEEEEEEEE               R::::RRRRRR:::::R     '//NL//  &
    '     O:::::O     O:::::O              V:::::V     V:::::V                E:::::::::::::::E               R:::::::::::::RR      '//NL//  &
    '     O:::::O     O:::::O               V:::::V   V:::::V                 E:::::::::::::::E               R::::RRRRRR:::::R     '//NL//  &
    '     O:::::O     O:::::O                V:::::V V:::::V                  E::::::EEEEEEEEEE               R::::R     R:::::R    '//NL//  &
    '     O:::::O     O:::::O                 V:::::V:::::V                   E:::::E                         R::::R     R:::::R    '//NL//  &
    '     O::::::O   O::::::O                  V:::::::::V                    E:::::E       EEEEEE            R::::R     R:::::R    '//NL//  &
    '     O:::::::OOO:::::::O                   V:::::::V                   EE::::::EEEEEEEE:::::E          RR:::::R     R:::::R    '//NL//  &
    '      OO:::::::::::::OO                     V:::::V                    E::::::::::::::::::::E          R::::::R     R:::::R    '//NL//  &
    '        OO:::::::::OO                        V:::V                     E::::::::::::::::::::E          R::::::R     R:::::R    '//NL//  &
    '          OOOOOOOOO                           VVV                      EEEEEEEEEEEEEEEEEEEEEE          RRRRRRRR     RRRRRRR    '

  END SUBROUTINE
  !
  !  EPIC_FAIL  routine
  !
  SUBROUTINE EPIC_FAIL(IOUT)
    INTEGER, INTENT(IN):: IOUT
    !
    IF(IOUT /= Z) WRITE(IOUT,'(///A)')                                                                     &
     "    .----------------.     .----------------.     .----------------.     .----------------. "//NL//  &
     "   | .--------------. |   | .--------------. |   | .--------------. |   | .--------------. |"//NL//  &
     "   | |  _________   | |   | |   ______     | |   | |     _____    | |   | |     ______   | |"//NL//  &
     "   | | |_   ___  |  | |   | |  |_   __ \   | |   | |    |_   _|   | |   | |   .' ___  |  | |"//NL//  &
     "   | |   | |_  \_|  | |   | |    | |__) |  | |   | |      | |     | |   | |  / .'   \_|  | |"//NL//  &
     "   | |   |  _|  _   | |   | |    |  ___/   | |   | |      | |     | |   | |  | |         | |"//NL//  &
     "   | |  _| |___/ |  | |   | |   _| |_      | |   | |     _| |_    | |   | |  \ `.___.'\  | |"//NL//  &
     "   | | |_________|  | |   | |  |_____|     | |   | |    |_____|   | |   | |   `._____.'  | |"//NL//  &
     "   | |              | |   | |              | |   | |              | |   | |              | |"//NL//  &
     "   | '--------------' |   | '--------------' |   | '--------------' |   | '--------------' |"//NL//  &
     "    '----------------'     '----------------'     '----------------'     '----------------' "//NL//  &
     " "//NL//  &
     "                                                  .----------------.     .----------------.     .----------------.     .----------------. "//NL//  &
     "                                                 | .--------------. |   | .--------------. |   | .--------------. |   | .--------------. |"//NL//  &
     "                                                 | |  _________   | |   | |      __      | |   | |     _____    | |   | |   _____      | |"//NL//  &
     "                                                 | | |_   ___  |  | |   | |     /  \     | |   | |    |_   _|   | |   | |  |_   _|     | |"//NL//  &
     "                                                 | |   | |_  \_|  | |   | |    / /\ \    | |   | |      | |     | |   | |    | |       | |"//NL//  &
     "                                                 | |   |  _|      | |   | |   / ____ \   | |   | |      | |     | |   | |    | |   _   | |"//NL//  &
     "                                                 | |  _| |_       | |   | | _/ /    \ \_ | |   | |     _| |_    | |   | |   _| |__/ |  | |"//NL//  &
     "                                                 | | |_____|      | |   | ||____|  |____|| |   | |    |_____|   | |   | |  |________|  | |"//NL//  &
     "                                                 | |              | |   | |              | |   | |              | |   | |              | |"//NL//  &
     "                                                 | '--------------' |   | '--------------' |   | '--------------' |   | '--------------' |"//NL//  &
     "                                                  '----------------'     '----------------'     '----------------'     '----------------' "//NL



  END SUBROUTINE
  !
END MODULE