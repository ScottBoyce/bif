!
!--------------------------------------------------------------------------------------------------------
!
! CODE DEVELOPED BY SCOTT E BOYCE
!                   CONTACT <seboyce@usgs.gov> or <Boyce@engineer.com>
!
!--------------------------------------------------------------------------------------------------------
!
!  Simple data type that keeps track of unit tests
!    and provides easy access to utilities that open files and read input.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! ---------------------------- Unit Test Methods -------------------------------------------------------------
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
! TYPE(UNIT_TESTS):: UT
!
!  CALL UT % INIT([IOUT], [NTEST])  --> initialized a set of unit tests
!
!  CALL UT % SET_IOUT(IOUT)
!
!  CALL UT % NEXT_SECTION(HEADER)                    --> Start and name a group of individual tests
!       CALL UT % NEXT_TEST(NAME, [assume_pass])     --> Start and Name a single test.
!                                                        Test contains a single FAILED logical that is .FALSE. until an assertion test fails.
!       CALL UT % ASSERT( arg )                      --> Performs a comparison (subtest). See below for full assert options.
!                                                            Routine can be called multiple times; if one subtest fails causes the entire test to fail.
!       CALL UT %FAIL([MSG], [KILL])                 --> Changes current test status to FAILED, equivalent to ut%assert(1==0)
!       CALL UT % END_TEST()                         --> Print PASS or FAIL from the previous UT%NEXT_TEST(NAME)
!                                                            If test failed, then any optional test messages are printed.
!  CALL UT%WRITE_RESULTS(IOUT)                     --> Write to IOUT all the results for the Sections and Tests.
!  CALL UT%COMPLETE(PRINT_FAILED, [NOSTOP], [MSG]) --> Ends the set of unit tests and optionally prints all all the section results.
!                                                            All past test results are deleted to start a new set of sections/tests
!                                                            If there is a failed test, then the program is STOPed
!
! IOUT          int,  is the output file unit, default is stdout; IOUT = -1 supresses output
! NTEST         int,  is the number of tests to initially allocate for, default is 32
! HEADER        char, is the name of the group of tests called a section
!
! NAME          char, is the name of an indigual test, which can contain lots of comparisons called assertions.
! assume_pass  logical, if .TRUE. then change initial test status from 'UNKN' to 'PASS'
!
! PRINT_FAILED logical, if .TRUE. then write a listing of all failed assertion tests.
! NOSTOP       logical, if .TRUE. then if UT%COMPLETE has a failed test will NOT stop the program.
! MSG          char, is the message to attach to the failed test
! KILL         if present, and set to .TRUE., and failed test stops program with ERROR STOP
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
! CALL UT%ASSERT( arg )
!                 arg  are the arguement for ASSERT as described below:
!
!    CALL UT%%ASSERT(A, op, B,   [MSG], [KILL])       -> set op to "EQ", "LT", "LE", "GT", "GE"; A and B are INTEGER Types
!    CALL UT%%ASSERT(TF, [want], [MSG], [KILL])       -> TF  and WANT are a logical vairable;
!    CALL UT%%ASSERT(INT,[want], [MSG], [KILL])       -> INT and WANT are the same INTEGER type, if zero then test fails, otherwise passes
!
!    CALL UT%%ASSERT(A, op, B, [MSG], [KILL], [CAP])  -> A and B are CHAR(*), default is to make A and B upcase before comparison
!    CALL UT%%ASSERT(A, op, B, [MSG], [KILL], [TOL])  -> A and B are REAL, TOL is only used if OP='eq' and overrides the IS_CLOSE routing to solve abs(A-B) < TOL
!
! A, B    are values to test, may be scalar, vector, or matrix of data types INTEGER, INTEGER(int32), INTEGER(int64), REAL, DOUBLE PRECISION, REAL(rel32), REAL(rel64)
! op      is set to "EQ", "LT", "LE", "GT", "GE" to indicate how A and B are compared.
!            That is op="LT", tests "A < B"
! TF      is a logical, if true, the test passes, if false than test fails.      Can be a scalar or vector.
! INT     is any INTEGER type that fails if 0, otherwise passes.                 Can be a scalar or vector.
! WANT    is what is desired, that is TF.eqv.WANT to pass and INT==WANT to pass  Type and dimension must match TF and INT, respectively
! MSG     is the message to assign for the comparison (subtest).
!            If the subtest fails, then its message is printed when END_TEST() is called.
! CAP     if present, and set to .FALSE., and the A and B are not made upper case before the comparison test
! KILL    if present, and set to .TRUE., and the comparison test fails ERROR STOP is run
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!   QUICK_TEST is equivalent to calling NEXT_TEST, ASSERT, and END_TEST in a single call
!
! CALL UT%QUICK_TEST(NAME, PASSED)
!                                equivalent =>  CALL UT%NEXT_TEST(NAME)
!                                               CALL UT%ASSERT   ( PASSED )   -> PASSED is a logical
!                                               CALL UT%END_TEST ( )
! CALL UT%QUICK_TEST(NAME, HAVE, WANT,[TOL],[CAP])
!                                equivalent =>  CALL UT%NEXT_TEST(NAME)
!                                               CALL UT%ASSERT   ( HAVE, 'eq', WANT )  --> CAP only has meaning if HAVE and WANT are CAHRACTER(*); TOL only has meaning if HAVE and WANT are REAL;
!                                               CALL UT%END_TEST ( )
!
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! ---------------------------- Helpful Extra Functions and Subroutines ---------------------------------------
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!  These are clones of other BiF routines to provide access within the UT data type.
!
!    CALL UT % IS_CLOSE(VAR1,VAR2, [TOL])  RESULT(abs(VAR1-VAR2)<TOL)
!
!    CALL UT % OPEN_READ( FILE, IU, [FORM], [ACCESS])       ! Open a file for reading
!    CALL UT % OPEN_WRITE(FILE, IU, [FORM], [ACCESS])       ! Open a file for writing
!
!    CALL UT % READ_LINE(LINE,IU,COM,[EOL],[EOF])           ! Load next line into LINE
!
!    CALL UT % PARSE_WORD   (LN, LOC, ISTART, ISTOP, [COM]) ! Parse LN for next delimited word, after LOC, is LN(istart:istop)
!    CALL UT % PARSE_WORD_UP(LN, LOC, ISTART, ISTOP, [COM]) ! Parse LN for next delimited word, after LOC, is LN(istart:istop), which is also made upper case
!    CALL UT % UPPER(line)                                  ! Make line upper case
!
! IU      is a Unit number file that is opened or read from, it is set to -1 if there is an error
! FORM    is set to 'FORMATTED' or 'UNFORMATTED' or "TEXT" or "BINARY"; default is 'FORMATTED' (which is the same as 'TEXT')
! ACCESS  is set to 'SEQUENTIAL' or 'STREAM';                           default is 'STREAM'
!
! COM     is the comment symbol where anything to the right of it is ignored, if set to " ", then no comment is searched for. For READ_LINE, if COM="1", then only read one line
! LINE    character(*) that is set with the line or processed.
! EOL     is intger location where the end of the line is, or one minus the location of COM
! EOF     is logical that is set to true if the end of file is reached
!
! LN      character(*) that is a line that is processed.
! LOC     is starting location for a search, set to 1 on first call, typically updated to ISTOP+1 after use
! ISTART  is the starting location of a parsed word
! ISTOP   is the   ending location of a parsed word
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
! Get routines parse a line to get a word or specific value of a base type (INTEGER/REAL)
!
!    CALL UT % GET(LN, LOC, ISTART, ISTOP, VAL, [ERROR_VAL], [IOSTAT], [COM])  ! Parse LN for next value VAL
!
!    CALL UT % GET(LN, LOC,                WORD(:), [NO_UPCASE], [COM])        ! Parse LN for next WORD
!    CALL UT % GET(LN, LOC, ISTART, ISTOP, WORD(*), [NO_UPCASE], [COM])
!
!    CALL UT % GET(IU, FNAME, SET_FNAME)                                       ! Give file unit IU, find its assocaited absolute path and file name.
!
! WORD(:)    character(:), allocatable type that is allocated to LN(ISTART:ISTOP)
! WORD(*)    character(*) that is set to LN(ISTART:ISTOP),                        -> note if ISTOP-ISTART+1 > len(WORD) then the output is clipped
!
! NO_UPCASE  if present and true, than WORD is not set to upper case.
! COM        next word will stop before the COM Comment
!
! VAL        Either of type INTEGER, REAL, or DOUBLE PRECISION, may be a scalar or vector
! ERROR_VAL  if there is an error setting VAL, instead of stopping program set VAL=ERROR_VAL
! IOSTAT     if there is an error,             instead of stopping program return IOSTAT
!
! IU         is a file unit that is open
! FNAME      character(:), allocatable type that is set to the absolute path and file name associated with IU
! SET_FNAME  Logical flag to indicate if file name was found or false if an error occured
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
! NUM2STR are routines that take a value and return it as a character(:) represntation of it
!
!    CALL UT % NUM2STR(IVAL, [PAD], [ZPAD])
!    CALL UT % NUM2STR(DVAL, [PAD], [GENERAL])
!    CALL UT % NUM2STR(LVAL, [PAD], [fmt])
!    CALL UT % NUM2STR(IVEC, [PAD], [SEP], [ZPAD])
!
! IVAL    integer           value that is converted to character(:)
! DVAL    double precision  value that is converted to character(:)
! LVAL    logical           value that is converted to character(:)
! IVEC    integer(:)        vector that is converted to character(:); eg [integer:: 1,3,2,5] => '1 3 2 5'
!
! PAD     integer, Minimum size of returned character, PAD>0 is right justified, PAD<0 is left justified, default is PAD=0
! ZPAD    logical, if true and PAD>0, than the integer is zero padded rather than space padded, eg 0005
! SEP     character(*), is the separater applied between values in VEC. Default is " ", a blank space
! fmt     character(*), default is 'T' to indicate output is a T or F, gives shape of output format.
!            fmt indicates how output should be formatted
!            For example, 
!                   fmt='1'  to get '0'    '1'     output, or 
!                   fmt='TR' to get 'TRUE' 'FALSE' output, or 
!                   fmt='Tr' to get 'True' 'False' output, or 
!                   fmt='t'  to get 't'    'f'     output
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
! CONSOLE are routines that assist in invoking the system command prompt of a specific command
!
!    CALL UT % CONSOLE(CMD, IOSTAT, [OUTPUT])
!
! CMD      character(*), is the command to run on the console
! IOSTAT   integer, is status from running the command
! OUTPUT   character(:), allocatable type that is set to any output that resulted from running the console command
!
!
!#########################################################################################################################
!#########################################################################################################################
!#########################################################################################################################
!
!
MODULE UNIT_TESTING_INSTRUCTION
  USE IEEE_ARITHMETIC, ONLY: IEEE_VALUE, IEEE_QUIET_NAN
  USE ISO_FORTRAN_ENV, ONLY: stdout => OUTPUT_UNIT,        &
                             stderr => ERROR_UNIT,         &
                             rel32 => REAL32, rel64 => REAL64, INT8, INT16, INT32, INT64
  IMPLICIT NONE
  !
  INTEGER,       PARAMETER:: PRINT_COL = 50   !Column to print PASS/FAIL
  !
  CHARACTER(4),  PARAMETER:: P = "PASS",  F = "FAIL"
  !
  CHARACTER,     PARAMETER:: BLNK = ' ', TAB = ACHAR(9), NL = NEW_LINE(' ')
  !
  INTEGER,       PARAMETER:: NEG = -1, Z = 0, ONE = 1
  !
  REAL(rel64),   PARAMETER:: DNEG = -1._rel64, DZ = 0._rel64, UNO = 1._rel64
  !
  LOGICAL,       PARAMETER:: TRUE  = .TRUE.,  FALSE = .FALSE.
  !
  PRIVATE
  PUBLIC:: UNIT_TESTS
  !
  TYPE UNIT_TEST
    !
    LOGICAL:: SECTION_HEADER = FALSE
    !
    CHARACTER( 64):: NAME = BLNK !Test Name
    CHARACTER(  4):: STAT = BLNK !PASS or FAIL
    !
    LOGICAL:: HAS_NOTE = FALSE
    CHARACTER(512), allocatable:: NOTE
    !
    CONTAINS
    !
    PROCEDURE, pass(UT):: ADD_HEADER    => ADD_HEADER_UNIT_TEST
    PROCEDURE, pass(UT):: ADD_NOTE      => ADD_NOTE_UNIT_TEST
    GENERIC            :: ASSIGNMENT(=) => COPY_UNIT_TEST_TYPE
    PROCEDURE,  PRIVATE:: COPY_UNIT_TEST_TYPE
  END TYPE
  !
  TYPE UNIT_TESTS
    LOGICAL:: FAILED = FALSE
    INTEGER:: IOUT   = stdout
    INTEGER:: DIM    = Z
    INTEGER:: N      = Z
    LOGICAL:: DOTS   = FALSE
    LOGICAL:: TESTING= FALSE
    LOGICAL:: LAST_TEST_FAIL = FALSE
    TYPE(UNIT_TEST), dimension(:), allocatable:: TEST
    !
    CONTAINS
    !
    ! ---------------------------- Unit Test Methods --------------------------------------------------------------------------------------
    !
    PROCEDURE, pass(UT):: INIT          => INITIALIZE_UNIT_TESTS    ! %INIT([IOUT], [NTEST]), Set IOUT = -1 to supress output while running tests
    PROCEDURE, pass(UT):: SET_IOUT      => SET_IOUT_UNIT_TESTS      ! %SET_IOUT(IOUT)
    !
    PROCEDURE, pass(UT):: NEXT_SECTION  => SECTION_HEADER_UNIT_TEST ! %NEXT_SECTION(HEADER)
    !
    PROCEDURE, pass(UT):: NEXT_TEST     => NEXT_UNIT_TEST           ! %NEXT_TEST(NAME, [assume_pass])
    PROCEDURE, pass(UT):: END_TEST      => END_TEST_UNIT_TEST       ! %END_TEST()
    PROCEDURE, pass(UT):: FAIL          => FAIL_UNIT_TEST           ! %FAIL([MSG], [KILL])
    PROCEDURE, pass(UT):: LAST_TEST
    !                                                               !
    !                                                               ! %ASSERT(A, op, B, [MSG], [KILL])   -> set op to "EQ", "LT", "LE", "GT", "GE";
    !                                                               ! %ASSERT(TF, [want], [MSG], [KILL]) -> TF  and WANT are a logical vairable;
    !                                                               ! %ASSERT(INT,[want], [MSG], [KILL]) -> INT and WANT are the same INTEGER type, if zero then test fails, otherwise passes
    GENERIC            :: ASSERT        => ASSERT_UNIT_TEST_0D_CHAR, ASSERT_UNIT_TEST_0D_INT8, ASSERT_UNIT_TEST_0D_INT16, ASSERT_UNIT_TEST_0D_INT32, ASSERT_UNIT_TEST_0D_INT64, ASSERT_UNIT_TEST_0D_REL32, ASSERT_UNIT_TEST_0D_REL64, ASSERT_UNIT_TEST_0D_TF, ASSERT_UNIT_TEST_0D_TFOP, &
                                           ASSERT_UNIT_TEST_1D_CHAR, ASSERT_UNIT_TEST_1D_INT8, ASSERT_UNIT_TEST_1D_INT16, ASSERT_UNIT_TEST_1D_INT32, ASSERT_UNIT_TEST_1D_INT64, ASSERT_UNIT_TEST_1D_REL32, ASSERT_UNIT_TEST_1D_REL64, ASSERT_UNIT_TEST_1D_TF, ASSERT_UNIT_TEST_1D_TFOP, &
                                           ASSERT_UNIT_TEST_2D_CHAR, ASSERT_UNIT_TEST_2D_INT8, ASSERT_UNIT_TEST_2D_INT16, ASSERT_UNIT_TEST_2D_INT32, ASSERT_UNIT_TEST_2D_INT64, ASSERT_UNIT_TEST_2D_REL32, ASSERT_UNIT_TEST_2D_REL64, ASSERT_UNIT_TEST_2D_TF, ASSERT_UNIT_TEST_2D_TFOP, &
                                           ASSERT_UNIT_TEST_0D_NONZERO_INT8, ASSERT_UNIT_TEST_0D_NONZERO_INT16, ASSERT_UNIT_TEST_0D_NONZERO_INT32, ASSERT_UNIT_TEST_0D_NONZERO_INT64,                                                                                                   &
                                           ASSERT_UNIT_TEST_1D_NONZERO_INT8, ASSERT_UNIT_TEST_1D_NONZERO_INT16, ASSERT_UNIT_TEST_1D_NONZERO_INT32, ASSERT_UNIT_TEST_1D_NONZERO_INT64,                                                                                                   &
                                           ASSERT_UNIT_TEST_2D_NONZERO_INT8, ASSERT_UNIT_TEST_2D_NONZERO_INT16, ASSERT_UNIT_TEST_2D_NONZERO_INT32, ASSERT_UNIT_TEST_2D_NONZERO_INT64

    !
    GENERIC            :: QUICK_TEST    => QUICK_UNIT_TEST,      &   ! %QUICK_TEST(NAME, PASSED)  -- Combines NEXT_TEST, TEST_STATUS, and END_TEST
                                           QUICK_UNIT_TEST_WILD, &   ! %QUICK_TEST(NAME, HAVE, WANT, [TOL], [CAP])
                                           QUICK_UNIT_TEST_WILD_1D
    !
    PROCEDURE, pass(UT):: WRITE_RESULTS => WRITE_RESULTS_UNIT_TEST  ! %WRITE_RESULTS(IOUT)
    !
    PROCEDURE, pass(UT):: COMPLETE      => COMPLETE_UNIT_TEST       ! %COMPLETE(PRINT_FAILED, [NOSTOP], [MSG])
    PROCEDURE, pass(UT):: DESTROY       => DESTROY_UNIT_TESTS       ! %DESTROY()
    !
    ! ---------------------------- Helpful Extra Functions and Subroutines ----------------------------------------------------------------
    !
    GENERIC::           IS_CLOSE        => IS_CLOSE_DBL, IS_CLOSE_SNG ! UT%IS_CLOSE(VAR1,VAR2, TOL)  RESULT(VAR1==VAR2)
    !
    PROCEDURE, nopass:: OPEN_READ       ! CALL UT%OPEN_READ( FILE, IU, [FORM], [ACCESS])
    PROCEDURE, nopass:: OPEN_WRITE      ! CALL UT%OPEN_WRITE(FILE, IU, [FORM], [ACCESS])
    !
    PROCEDURE, nopass:: READ_LINE       ! CALL UT%READ_LINE(LINE,INFILE,COM,[EOL],[EOF])
    !
    PROCEDURE, nopass:: PARSE_WORD      ! CALL UT%PARSE_WORD(LN, LOC, ISTART, ISTOP, COM)
    PROCEDURE, nopass:: PARSE_WORD_UP   ! CALL UT%PARSE_WORD_UP(LN, LOC, ISTART, ISTOP, COM)
    PROCEDURE, nopass:: UPPER           ! CALL UT%UPPER(LINE)
    !
    GENERIC:: CONSOLE =>           &
              CONSOLE_CMD,         &    !UT%CONSOLE(CMD, IOSTAT)
              CONSOLE_CMD_OUTPUT        !UT%CONSOLE(CMD, IOSTAT, OUTPUT)
    !
    GENERIC:: NUM2STR => INT2STR,  &    ! UT%NUM2STR(IVAL,[PAD],[ZPAD])      PAD is min lenght of number, <0 is left justified at ABS(PAD) min lenght
                         DBLE2STR, &    ! UT%NUM2STR(DVAL,[PAD],[GENERAL])
                         TF2STR,   &    ! UT%NUM2STR(LVAL,[PAD],[fmt])      !!fmt indicates output format such as fmt='1' for '0' or '1' output, or fmt='TR' to get 'TRUE' 'FALSE' output, or fmt='Tr' to get 'True' 'False' output
                         INTVEC2STR     ! UT%NUM2STR(IVAL,[PAD],  [SEP], [ZPAD])  --ZPAD is logical to indicate padding with 000
    GENERIC:: GET  =>                &
              GET_WORD_ALLOC,        &  ! CALL UT%GET(LN, LOC,                WORD, [NO_UPCASE], [COM])
              GET_WORD_ASSUM_ISTART, &  ! CALL UT%GET(LN, LOC, ISTART, ISTOP, WORD, [NO_UPCASE], [COM])
              GET_DOUBLE_VEC,        &  ! CALL UT%GET(LN, LOC, ISTART, ISTOP, VAL, [ERROR_VAL], [IOSTAT], [COM])
              GET_DOUBLE_VAL,        &  !
              GET_INTEGER_VEC,       &  !
              GET_INTEGER_VAL,       &  !
              GET_REAL_VEC,          &  !
              GET_REAL_VAL,          &  !
              GET_FILE_NAME             ! CALL UT%GET(IU,FNAME,SET_FNAME)
    !
    ! ---------------------------- Private Routines ---------------------------------------------------------------------------------------
    !
    PROCEDURE, PASS(UT), PRIVATE::  QUICK_UNIT_TEST                 ! %QUICK_TEST(NAME, PASSED)
    PROCEDURE, PASS(UT), PRIVATE::  QUICK_UNIT_TEST_WILD            ! %QUICK_TEST(NAME, HAVE, WANT, [TOL], [CAP])
    PROCEDURE, PASS(UT), PRIVATE::  QUICK_UNIT_TEST_WILD_1D         ! %QUICK_TEST(NAME, HAVE, WANT)
    !
    PROCEDURE, nopass,   PRIVATE:: GET_WORD_ALLOC                   ! UT%GET_WORD_ALLOC_ISTART(LN, LOC,                WORD, [NO_UPCASE], [COM])
    PROCEDURE, nopass,   PRIVATE:: GET_WORD_ASSUM_ISTART            ! UT%GET_WORD_ASSUM_ISTART(LN, LOC, ISTART, ISTOP, WORD, [NO_UPCASE], [COM])
    PROCEDURE, nopass,   PRIVATE:: GET_DOUBLE_VEC                   ! UT%GET_DOUBLE_VEC(LN, LOC, ISTART, ISTOP, VAL, [ERROR_VAL], [IOSTAT], [COM])
    PROCEDURE, nopass,   PRIVATE:: GET_DOUBLE_VAL                   ! UT%GET_DOUBLE_VAL
    PROCEDURE, nopass,   PRIVATE:: GET_INTEGER_VEC                  ! UT%GET_INTEGER_VEC
    PROCEDURE, nopass,   PRIVATE:: GET_INTEGER_VAL                  ! UT%GET_INTEGER_VAL
    PROCEDURE, nopass,   PRIVATE:: GET_REAL_VEC                     ! UT%GET_REAL_VEC
    PROCEDURE, nopass,   PRIVATE:: GET_REAL_VAL                     ! UT%GET_REAL_VAL
    PROCEDURE, nopass,   PRIVATE:: GET_FILE_NAME                    ! UT%GET_FILE_NAME(IU,FNAME,SET_FNAME)
    !
    PROCEDURE, nopass,   PRIVATE:: INT2STR                          ! UT%INT2STR(IVAL,PAD,ZPAD)
    PROCEDURE, nopass,   PRIVATE:: DBLE2STR                         ! UT%DBLE2STR(DVAL,PAD,GENERAL)
    PROCEDURE, nopass,   PRIVATE:: TF2STR                           ! UT%TF2STR(LVAL,PAD,OPT) !NO OPT or OPT = 0 returns number, OPT = 1 returns T/F OPT = 2 returns TRUE/FALSE
    PROCEDURE, nopass,   PRIVATE:: INTVEC2STR
    !
    PROCEDURE, nopass,   PRIVATE:: CONSOLE_CMD                      !UT%CONSOLE_CMD(CMD, IOSTAT)
    PROCEDURE, nopass,   PRIVATE:: CONSOLE_CMD_OUTPUT               !UT%CONSOLE_CMD_OUTPUT(CMD, IOSTAT, OUTPUT)
    !
    PROCEDURE, nopass,   PRIVATE:: IS_CLOSE_DBL
    PROCEDURE, nopass,   PRIVATE:: IS_CLOSE_SNG
    PROCEDURE, pass(UT), PRIVATE:: ASSERT_UNIT_TEST_0D_INT32, ASSERT_UNIT_TEST_0D_INT64, ASSERT_UNIT_TEST_0D_REL32, ASSERT_UNIT_TEST_0D_REL64, ASSERT_UNIT_TEST_0D_TF, ASSERT_UNIT_TEST_0D_TFOP, ASSERT_UNIT_TEST_0D_CHAR, ASSERT_UNIT_TEST_0D_INT8, ASSERT_UNIT_TEST_0D_INT16
    PROCEDURE, pass(UT), PRIVATE:: ASSERT_UNIT_TEST_1D_INT32, ASSERT_UNIT_TEST_1D_INT64, ASSERT_UNIT_TEST_1D_REL32, ASSERT_UNIT_TEST_1D_REL64, ASSERT_UNIT_TEST_1D_TF, ASSERT_UNIT_TEST_1D_TFOP, ASSERT_UNIT_TEST_1D_CHAR, ASSERT_UNIT_TEST_1D_INT8, ASSERT_UNIT_TEST_1D_INT16
    PROCEDURE, pass(UT), PRIVATE:: ASSERT_UNIT_TEST_2D_INT32, ASSERT_UNIT_TEST_2D_INT64, ASSERT_UNIT_TEST_2D_REL32, ASSERT_UNIT_TEST_2D_REL64, ASSERT_UNIT_TEST_2D_TF, ASSERT_UNIT_TEST_2D_TFOP, ASSERT_UNIT_TEST_2D_CHAR, ASSERT_UNIT_TEST_2D_INT8, ASSERT_UNIT_TEST_2D_INT16
    PROCEDURE, pass(UT), PRIVATE:: ASSERT_UNIT_TEST_0D_NONZERO_INT8, ASSERT_UNIT_TEST_0D_NONZERO_INT16, ASSERT_UNIT_TEST_0D_NONZERO_INT32, ASSERT_UNIT_TEST_0D_NONZERO_INT64
    PROCEDURE, pass(UT), PRIVATE:: ASSERT_UNIT_TEST_1D_NONZERO_INT8, ASSERT_UNIT_TEST_1D_NONZERO_INT16, ASSERT_UNIT_TEST_1D_NONZERO_INT32, ASSERT_UNIT_TEST_1D_NONZERO_INT64
    PROCEDURE, pass(UT), PRIVATE:: ASSERT_UNIT_TEST_2D_NONZERO_INT8, ASSERT_UNIT_TEST_2D_NONZERO_INT16, ASSERT_UNIT_TEST_2D_NONZERO_INT32, ASSERT_UNIT_TEST_2D_NONZERO_INT64
    !
    ! ---------------------------- Final Routine ------------------------------------------------------------------------------------------
    FINAL:: FINAL_UNIT_TESTS
    !
  END TYPE
  !
  INTERFACE IS_CLOSE
     MODULE PROCEDURE IS_CLOSE_DBL
     MODULE PROCEDURE IS_CLOSE_SNG
  END INTERFACE
  !
  CONTAINS
  !
  ! ---------------------------------------------------------------------------------------------
  !
  ELEMENTAL PURE SUBROUTINE COPY_UNIT_TEST_TYPE(UT_OUT,UT_IN)
    CLASS(UNIT_TEST), intent(inout):: UT_OUT
    CLASS(UNIT_TEST), intent(in   ):: UT_IN
    !
    UT_OUT%NAME     = UT_IN%NAME
    UT_OUT%STAT     = UT_IN%STAT
    UT_OUT%HAS_NOTE = UT_IN%HAS_NOTE
    !
    UT_OUT%SECTION_HEADER = UT_IN%SECTION_HEADER
    !
    IF(UT_IN%HAS_NOTE) THEN
        IF(.NOT. ALLOCATED(UT_OUT%NOTE)) ALLOCATE(UT_OUT%NOTE)
        UT_OUT%NOTE(:) = UT_IN%NOTE(:)
    END IF
    !
  END SUBROUTINE
  !
  ! ---------------------------------------------------------------------------------------------
  !
  PURE SUBROUTINE ADD_HEADER_UNIT_TEST(UT, MSG)
    CLASS(UNIT_TEST),       intent(inout):: UT
    CHARACTER(*), optional, intent(in   ):: MSG
    !
    UT%SECTION_HEADER = TRUE
    UT%HAS_NOTE       = TRUE
    !
    IF(.NOT. ALLOCATED(UT%NOTE)) ALLOCATE(UT%NOTE)
    !
    IF(.NOT. PRESENT(MSG)) THEN
        UT%NOTE = "SECTION BREAK"
    ELSEIF(MSG == BLNK) THEN
        UT%NOTE = "SECTION BREAK"
    ELSE
        UT%NOTE = MSG
    END IF
    !
  END SUBROUTINE
  !
  ! ---------------------------------------------------------------------------------------------
  !
  PURE SUBROUTINE ADD_NOTE_UNIT_TEST(UT, MSG)
    CLASS(UNIT_TEST),  intent(inout):: UT
    CHARACTER(*),      intent(in   ):: MSG
    !
    IF(MSG == BLNK) RETURN
    !
    UT%HAS_NOTE = TRUE
    !
    IF(.NOT. ALLOCATED(UT%NOTE)) THEN
        ALLOCATE(UT%NOTE)
        UT%NOTE = BLNK
    END IF
    !
    IF(UT%NOTE /= BLNK) THEN
        UT%NOTE = TRIM(UT%NOTE)//" ; "//MSG
    ELSE
        UT%NOTE = MSG
    END IF
    !
  END SUBROUTINE
  !
  ! ---------------------------------------------------------------------------------------------
  !
  SUBROUTINE INITIALIZE_UNIT_TESTS(UT, IOUT, NTEST)
    CLASS(UNIT_TESTS), intent(inout):: UT
    INTEGER, optional, intent(in   ):: IOUT  !Unit to write results to. If not specified then set to stdout. Set to -1 to disable output
    INTEGER, optional, intent(in   ):: NTEST !Intial guess for number of tests to do
    INTEGER:: NT
    !
    IF(PRESENT(NTEST)) THEN
        NT = NTEST
    ELSE
        NT = 32
    END IF
    !
    IF(UT%DIM <= NT) THEN
                        CALL DESTROY_UNIT_TESTS(UT)
                        CALL GROW_UNIT_TESTS(UT, NTEST)
    ELSE
                        CALL RESET_UNIT_TESTS(UT)
                        UT%IOUT = stdout
    END IF
    !
    IF(PRESENT(IOUT)) UT%IOUT = IOUT
    !
  END SUBROUTINE
  !
  ! ---------------------------------------------------------------------------------------------
  !
  SUBROUTINE RESET_UNIT_TESTS(UT, IOUT)
    CLASS(UNIT_TESTS), intent(inout):: UT
    INTEGER, optional, intent(in   ):: IOUT  !Unit to write results to. If not specified then set to stdout. Set to -1 to disable output
    INTEGER:: I
    !
    IF(UT%TESTING) CALL END_TEST_UNIT_TEST(UT)
    !
    DO I=1, UT%DIM
        UT%TEST(I)%SECTION_HEADER = FALSE
        !
        UT%TEST(I)%NAME = BLNK
        UT%TEST(I)%STAT = BLNK
        !
        IF(UT%TEST(I)%HAS_NOTE) UT%TEST(I)%NOTE = BLNK
        !
        UT%TEST(I)%HAS_NOTE = FALSE
        !
    END DO
    !
    IF(PRESENT(IOUT)) UT%IOUT = IOUT
    UT%LAST_TEST_FAIL = FALSE
    UT%FAILED = FALSE
    UT%DOTS   = FALSE
    UT%N      = Z
    !
  END SUBROUTINE
  !
  ! ---------------------------------------------------------------------------------------------
  !
  SUBROUTINE DESTROY_UNIT_TESTS(UT)
    CLASS(UNIT_TESTS), intent(inout):: UT
    !
    IF(UT%TESTING) CALL END_TEST_UNIT_TEST(UT)
    !
    UT%LAST_TEST_FAIL = FALSE
    UT%FAILED = FALSE
    UT%DOTS   = FALSE
    UT%IOUT   = stdout
    UT%DIM    = Z
    UT%N      = Z
    IF(ALLOCATED(UT%TEST)) DEALLOCATE(UT%TEST)
    !
  END SUBROUTINE
  !
  ! ---------------------------------------------------------------------------------------------
  !
  SUBROUTINE FINAL_UNIT_TESTS(UT)
    TYPE(UNIT_TESTS), intent(inout):: UT
    !
    CALL DESTROY_UNIT_TESTS(UT)
    !
  END SUBROUTINE
  !
  ! ---------------------------------------------------------------------------------------------
  !
  SUBROUTINE GROW_UNIT_TESTS(UT, By)
    CLASS(UNIT_TESTS), intent(inout):: UT
    INTEGER, optional, intent(in   ):: By
    TYPE(UNIT_TEST), dimension(:), allocatable:: TMP
    INTEGER:: GROW
    !
                    GROW = 32
    IF(PRESENT(By)) GROW = 32*(By/32) + 32  !Make sure it is a multiple of 32

    !
    IF(UT%DIM == Z) THEN
                    UT%DIM = GROW
                    ALLOCATE(UT%TEST(UT%DIM))
    ELSE
        CALL MOVE_ALLOC(UT%TEST, TMP)
        !
        UT%DIM = UT%DIM + GROW
        ALLOCATE(UT%TEST(UT%DIM))
        !
        DEALLOCATE(TMP)
    END IF
    !
  END SUBROUTINE
  !
  ! ---------------------------------------------------------------------------------------------
  !
  SUBROUTINE SET_IOUT_UNIT_TESTS(UT, IOUT)
    CLASS(UNIT_TESTS), intent(inout):: UT
    INTEGER,           intent(in   ):: IOUT  !Unit to write results to. If not specified then set to stdout. Set to -1 to disable output
    !
    UT%IOUT = IOUT
    !
  END SUBROUTINE
  !
  ! ---------------------------------------------------------------------------------------------
  !
  SUBROUTINE SECTION_HEADER_UNIT_TEST(UT, HEADER)
    CLASS(UNIT_TESTS),      intent(inout):: UT
    CHARACTER(*), optional, intent(in   ):: HEADER
    !
    IF(UT%TESTING) CALL END_TEST_UNIT_TEST(UT)
    !
    IF(UT%DIM == UT%N) CALL GROW_UNIT_TESTS(UT)
    !
    UT%N = UT%N + 1
    !
    CALL UT%TEST(UT%N)%ADD_HEADER(HEADER)
    !
    CALL PRINT_SECTION_HEADER(UT, UT%N, UT%IOUT)
    !
  END SUBROUTINE
  !
  ! ---------------------------------------------------------------------------------------------
  !
  PURE FUNCTION LAST_TEST(UT) RESULT(ANS)
    CLASS(UNIT_TESTS), intent(in):: UT
    LOGICAL:: ANS
    !
    ANS = .not. UT%LAST_TEST_FAIL
    !
  END FUNCTION
  !
  ! ---------------------------------------------------------------------------------------------
  !
  SUBROUTINE QUICK_UNIT_TEST(UT, NAME, PASSED)
    CLASS(UNIT_TESTS),      intent(inout):: UT
    CHARACTER(*),           intent(in   ):: NAME
    LOGICAL,                intent(in   ):: PASSED
    !
    CALL NEXT_UNIT_TEST(UT, NAME)
    CALL TEST_STATUS_UNIT_TEST(UT, PASSED)
    CALL END_TEST_UNIT_TEST(UT)
    !
  END SUBROUTINE
  !
  SUBROUTINE QUICK_UNIT_TEST_WILD(UT, NAME, HAVE, WANT, TOL, CAP)
    CLASS(UNIT_TESTS),     intent(inout):: UT
    CHARACTER(*),          intent(in   ):: NAME
    CLASS(*),              intent(in   ):: HAVE, WANT
    REAL(rel64), optional, intent(in):: TOL
    LOGICAL,     optional, intent(in   ):: CAP
    LOGICAL:: PASSED
    !
    PASSED = ASSERT_EQUAL(HAVE, WANT, TOL, CAP)
    !
    CALL NEXT_UNIT_TEST(UT, NAME)
    CALL TEST_STATUS_UNIT_TEST(UT, PASSED)
    CALL END_TEST_UNIT_TEST(UT)
    !
  END SUBROUTINE
  !
  SUBROUTINE QUICK_UNIT_TEST_WILD_1D(UT, NAME, HAVE, WANT, CAP)
    CLASS(UNIT_TESTS),      intent(inout):: UT
    CHARACTER(*),           intent(in   ):: NAME
    CLASS(*), dimension(:), intent(in   ):: HAVE, WANT
    LOGICAL,      optional, intent(in   ):: CAP
    LOGICAL:: PASSED
    INTEGER:: I, dim1, dim2
    !
    dim1 = size(HAVE)
    dim2 = size(WANT)
    IF( dim1 /= dim2 ) THEN
        PASSED = FALSE
    ELSE
        I=0;     PASSED = TRUE
        DO WHILE (PASSED .and. I < dim1)
            I = I + 1
            PASSED = ASSERT_EQUAL(HAVE(I), WANT(I), CAP=CAP)
        END DO
    END IF
    !
    CALL NEXT_UNIT_TEST(UT, NAME)
    CALL TEST_STATUS_UNIT_TEST(UT, PASSED)
    CALL END_TEST_UNIT_TEST(UT)
    !
  END SUBROUTINE
  !
  ! ---------------------------------------------------------------------------------------------
  !
  SUBROUTINE NEXT_UNIT_TEST(UT, NAME, assume_pass)
    CLASS(UNIT_TESTS),      intent(inout) :: UT
    CHARACTER(*),           intent(in   ) :: NAME
    LOGICAL,      optional, intent(in   ) :: assume_pass
    !                                     
    IF(UT%TESTING) CALL END_TEST_UNIT_TEST(UT)
    !
    IF(UT%DIM == UT%N) CALL GROW_UNIT_TESTS(UT)
    !
    UT%N = UT%N + 1
    UT%TEST(UT%N)%NAME = NAME
    UT%TESTING = TRUE
    UT%LAST_TEST_FAIL = FALSE
    !
    UT%TEST(UT%N)%STAT = BLNK
    !
    CALL PRINT_TEST_NAME(UT, UT%N, UT%IOUT)
    !
    if(present(assume_pass)) then
            if(assume_pass) UT%TEST(UT%N)%STAT = P
    end if
    !
  END SUBROUTINE
  !
  ! ---------------------------------------------------------------------------------------------
  !
  SUBROUTINE FAIL_UNIT_TEST(UT, MSG, KILL)
    CLASS(UNIT_TESTS),      intent(inout):: UT
    CHARACTER(*), optional, intent(in   ):: MSG
    LOGICAL,      optional, intent(in   ):: KILL
    !
    IF( UT%N == 0 ) CALL NEXT_UNIT_TEST(UT, 'No_Test_Name')
    !
    UT%FAILED = TRUE
    UT%LAST_TEST_FAIL = UT%FAILED
    UT%TEST(UT%N)%STAT = F
    IF(PRESENT(MSG)) CALL UT%TEST(UT%N)%ADD_NOTE(MSG)
    !
    IF(PRESENT(KILL)) THEN
           IF( KILL ) THEN
                      CALL END_TEST_UNIT_TEST(UT)
                      ERROR STOP "Unit Test Fatal Error"
           END IF
    END IF
    !
  END SUBROUTINE
  !
  ! ---------------------------------------------------------------------------------------------
  !
  SUBROUTINE TEST_STATUS_UNIT_TEST(UT, PASSED, END_TEST, MSG, KILL)
    CLASS(UNIT_TESTS),      intent(inout):: UT
    LOGICAL,                intent(in   ):: PASSED
    LOGICAL,      optional, intent(in   ):: END_TEST
    CHARACTER(*), optional, intent(in   ):: MSG
    LOGICAL,      optional, intent(in   ):: KILL
    INTEGER:: I
    !
    IF( UT%N == 0 ) CALL NEXT_UNIT_TEST(UT, 'No_Test_Name')
    !
    I = UT%N
    UT%LAST_TEST_FAIL = .not. PASSED
    !
    IF(PASSED) THEN
        IF(UT%TEST(I)%STAT == BLNK) UT%TEST(I)%STAT = P
    ELSE
        UT%FAILED = TRUE
        UT%TEST(I)%STAT = F
        !
        IF(PRESENT(MSG)) CALL UT%TEST(I)%ADD_NOTE(MSG)
        !
    END IF
    !
    IF(PRESENT(END_TEST)) THEN
            IF(END_TEST) CALL END_TEST_UNIT_TEST(UT)
    END IF
    !
    IF(PRESENT(KILL)) THEN
          IF( .not. PASSED .and. KILL ) THEN
                     IF(UT%TESTING) CALL END_TEST_UNIT_TEST(UT)
                     ERROR STOP "Unit Test Fatal Error"
          END IF
    END IF
    !
  END SUBROUTINE
  !
  ! ---------------------------------------------------------------------------------------------
  !
  SUBROUTINE END_TEST_UNIT_TEST(UT)
    CLASS(UNIT_TESTS),      intent(inout):: UT
    !
    CALL PRINT_TEST_STATUS(UT, UT%N, UT%IOUT)
    !
    UT%TESTING = FALSE
    !
  END SUBROUTINE
  !
  ! ---------------------------------------------------------------------------------------------
  !
  SUBROUTINE PRINT_SECTION_HEADER(UT, I, IOUT)
    CLASS(UNIT_TESTS), intent(inout):: UT
    INTEGER,           intent(in   ):: I, IOUT
    !
    UT%DOTS = FALSE
    IF(IOUT /= NEG) WRITE(UT%IOUT,'(/,A,/,3x,A,/,A,/)') REPEAT('=', 96), TRIM(UT%TEST(I)%NOTE), REPEAT('=', 96)
    !
  END SUBROUTINE
  !
  SUBROUTINE PRINT_TEST_NAME(UT, I, IOUT)
    CLASS(UNIT_TESTS), intent(in):: UT
    INTEGER,           intent(in):: I, IOUT
    !
    IF(IOUT /= NEG) WRITE(IOUT,'(2x,A)', ADVANCE='NO') TRIM(UT%TEST(I)%NAME)
    !
  END SUBROUTINE
  !
  SUBROUTINE PRINT_TEST_STATUS(UT, I, IOUT)
    CLASS(UNIT_TESTS), intent(inout):: UT
    INTEGER,           intent(in   ):: I, IOUT
    INTEGER:: N
    CHARACTER:: PAD
    CHARACTER(4):: STATUS
    !
    IF(IOUT == NEG) RETURN
    !
    IF(UT%DOTS) THEN  !When to print dots
       UT%DOTS = FALSE
       PAD     = '.'
    ELSE
       UT%DOTS = TRUE
       PAD     = BLNK
    END IF
    !
    STATUS = UT%TEST(I)%STAT
    IF(STATUS /= P .AND. STATUS /= F) STATUS = 'UNKN'
    !
    N = LEN_TRIM(UT%TEST(I)%NAME) + 1 - 2 !1 is for the 2x and -2 is for the two BLNKs
    IF(N < PRINT_COL) THEN
       N = PRINT_COL - N
    ELSE
       N = 1
    END IF
    WRITE(UT%IOUT,'(4A)') BLNK, REPEAT(PAD, N), BLNK, STATUS
    !
    IF(UT%TEST(I)%HAS_NOTE) THEN
            N = LEN_TRIM(UT%TEST(I)%NOTE) + 3 - 2  !3 is for the 4x and -2 is for the two BLNKs
            IF(N < PRINT_COL) THEN
               N = PRINT_COL - N
            ELSE
               N = 1
            END IF
            WRITE(UT%IOUT,'(4x,2A,/)') 'msg> ',TRIM(UT%TEST(I)%NOTE)
    END IF
    !
    !IF(UT%TEST(I)%HAS_NOTE) THEN
    !        N = LEN_TRIM(UT%TEST(I)%NOTE) + 3 - 2  !3 is for the 4x and -2 is for the two BLNKs
    !        IF(N < PRINT_COL) THEN
    !           N = PRINT_COL - N
    !        ELSE
    !           N = 1
    !        END IF
    !        WRITE(UT%IOUT,'(/,4x,5A)') TRIM(UT%TEST(I)%NOTE), BLNK, REPEAT(PAD, N), BLNK, STATUS
    !ELSE
    !        N = LEN_TRIM(UT%TEST(I)%NAME) + 1 - 2 !1 is for the 2x and -2 is for the two BLNKs
    !        IF(N < PRINT_COL) THEN
    !           N = PRINT_COL - N
    !        ELSE
    !           N = 1
    !        END IF
    !        WRITE(UT%IOUT,'(4A)') BLNK, REPEAT(PAD, N), BLNK, STATUS
    !END IF
    !
  END SUBROUTINE
  !
  ! ---------------------------------------------------------------------------------------------
  !
  SUBROUTINE WRITE_RESULTS_UNIT_TEST(UT, IOUT)
    CLASS(UNIT_TESTS),      intent(inout):: UT
    INTEGER,                intent(in   ):: IOUT
    INTEGER:: I
    !
    IF(UT%TESTING) CALL END_TEST_UNIT_TEST(UT)
    !
    IF(IOUT == NEG) RETURN
    !
    DO I=1, UT%N
        IF( UT%TEST(I)%SECTION_HEADER ) THEN
                                        CALL PRINT_SECTION_HEADER(UT, I, IOUT)
        ELSE
                                        CALL PRINT_TEST_NAME(UT, I, IOUT)
                                        CALL PRINT_TEST_STATUS(UT, I, IOUT)
        END IF
    END DO
    !
  END SUBROUTINE
  !
  ! ---------------------------------------------------------------------------------------------
  !
  SUBROUTINE COMPLETE_UNIT_TEST(UT, PRINT_FAILED, NOSTOP, MSG)
    CLASS(UNIT_TESTS),      intent(inout):: UT
    LOGICAL,                intent(in   ):: PRINT_FAILED
    LOGICAL,      optional, intent(in   ):: NOSTOP
    CHARACTER(*), optional, intent(in   ):: MSG
    INTEGER:: I, J
    CHARACTER(32):: NT
    LOGICAL:: STOP_PROG
    !
    IF(UT%TESTING) CALL END_TEST_UNIT_TEST(UT)
    !
    IF(UT%FAILED) THEN
        !
        STOP_PROG = TRUE
        IF(PRESENT(NOSTOP)) STOP_PROG = .NOT. NOSTOP
        !
        IF(PRINT_FAILED) THEN
           J = Z
           WRITE(stderr, "(/, A, /)") "v-------------- FAILED TESTS ------------------v"
           DO I=1, UT%N
               IF(UT%TEST(I)%SECTION_HEADER) THEN
                                             J = I
               ELSEIF(UT%TEST(I)%STAT == F ) THEN
                                             IF(J > 0) THEN
                                                       WRITE(stderr, "(A)") TRIM(UT%TEST(J)%NOTE)
                                                       J = Z
                                             END IF
                                             WRITE(stderr, "(4x, A)") TRIM(UT%TEST(I)%NAME)
               END IF
           END DO
           WRITE(stderr, "(/, A, /)") "^-------------- FAILED TESTS ------------------^"
        END IF
        !
        IF(PRESENT(MSG)) WRITE(stderr, "(A)") MSG
        !
        J = Z
        DO I=1, UT%N
            IF(UT%TEST(I)%STAT == F) J = J + 1
        END DO
        !
        WRITE(NT,'(I32)') J
        WRITE(stderr, "(2A)") TRIM(ADJUSTL(NT))," TESTS FAILED"
        !
        IF(STOP_PROG) ERROR STOP 55
    END IF
    !
    CALL RESET_UNIT_TESTS(UT)
    !
  END SUBROUTINE
  !
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !
  !  FUNCTIONS THAT DO NOT USE UNIT_TESTING_INSTRUCTION
  !
  PURE FUNCTION IS_CLOSE_DBL(VAR1,VAR2,TOL) RESULT(IS_CLOSE)
    REAL(rel64),           intent(in):: VAR1, VAR2
    REAL(rel64), optional, intent(in):: TOL
    LOGICAL    :: IS_CLOSE
    REAL(rel64):: ATOL, RTOL
    REAL(rel64):: CHK, DIFF, DZ, DNEG
    !ABS(VAR1-VAR2) <= (ATOL + RTOL * ABS(VAR2))
    !
    IF(present(TOL)) THEN
       IS_CLOSE = ABS(VAR2-VAR1) <= TOL
       RETURN
    END IF
    !
    ATOL = 1.D-10
    RTOL = 1.D-5  !COMPARE AGAINST 6th DIGIT (~SINGLE PRECISION)
    !
    DZ   =  0.D0
    DNEG = -1.D0
    !
    IF(VAR1.NE.VAR1 .OR. VAR2.NE.VAR2) THEN
        IS_CLOSE = FALSE
    ELSE
       IF(VAR1 > VAR2) THEN
           !
           IF    (VAR2 >= DZ) THEN           ! Solve CHK = MIN(ABS(VAR1), ABS(VAR2))
                              CHK = VAR2
           ELSEIF(VAR1 <= DZ) THEN
                              CHK = DNEG*VAR1
           ELSEIF(DNEG*VAR2 > VAR1) THEN         !Only here if VAR1 > 0 and VAR2 < 0 and VAR1 > VAR2
                              CHK = VAR1
           ELSE
                              CHK = DNEG*VAR2
           END IF
           !
           DIFF  = VAR1 - VAR2
       ELSE
           IF    (VAR1 >= DZ) THEN           ! Solve CHK = MIN(ABS(VAR1), ABS(VAR2))
                              CHK = VAR1
           ELSEIF(VAR2 <= DZ) THEN
                              CHK = DNEG*VAR2
           ELSEIF(DNEG*VAR1 > VAR2) THEN         !Only here if VAR2 > 0 and VAR1 < 0 and VAR2 > VAR1
                              CHK = VAR2
           ELSE
                              CHK = DNEG*VAR1
           END IF
           !
           DIFF   = VAR2 - VAR1
       END IF
       !
       IS_CLOSE = DIFF <= ATOL + CHK*RTOL
       !
    END IF
    !
  END FUNCTION
  !
  PURE FUNCTION IS_CLOSE_SNG(VAR1,VAR2,TOL) RESULT(IS_CLOSE)
    REAL(rel32),           intent(in):: VAR1, VAR2
    REAL(rel32), optional, intent(in):: TOL
    LOGICAL    :: IS_CLOSE
    REAL(rel64):: v1, v2
    v1 = real(VAR1,rel64)
    v2 = real(VAR2,rel64)
    if(present(TOL)) then
               IS_CLOSE = IS_CLOSE_DBL( v1, v2, real(TOL,rel64) )
    else
               IS_CLOSE = IS_CLOSE_DBL( v1, v2 )
    end if

  END FUNCTION
  !
  ! ---------------------------------------------------------------------------------------------
  !
  SUBROUTINE READ_LINE(LINE,IU,COM,EOL,EOF)  ! --> In BiF file_io_interface this is called READ_TO_DATA()
    CHARACTER(*),      intent(inout):: LINE      !LINE TO LOAD DATA TOO
    INTEGER,           intent(in   ):: IU        !UNIT OF FILE TO LOAD LINE FROM
    CHARACTER,         intent(in   ):: COM       !CHARACTER FOR ACCEPTED COMMENT. COM= " " TO NOT USE, COM="1" TO READ ONLY ONE LINE.
    INTEGER,  optional,intent(out  ):: EOL       !LOCATIONS OF WHERE THE END OF LINE IS OR ONE SPACE BEFORE #
    LOGICAL,  optional,intent(out  ):: EOF       !SET TO TRUE IF END OF FILE IS REACHED
    !
    INTEGER:: C, ERR
    !
    !
    IF(PRESENT(EOF)) EOF=FALSE
    !
    IF(COM == "1") THEN
        READ(IU,'(A)',IOSTAT=ERR) LINE
        IF    (ERR > Z) THEN                                   !LINE FAILED TO READ, THIS IS MOST LIKELY DUE TO END OF FILE LINE,IU,OUTPUT,MSG
                             WRITE(stderr, "(A, I0)") "ERROR READING LINE FROM FILE ON UNIT ", IU
                             ERROR STOP  55
        ELSEIF(ERR < Z) THEN !EOF
                             LINE=BLNK
                             C=ONE
                             IF(PRESENT(EOF)) EOF=TRUE
                             BACKSPACE(IU) !NOTE THAT EOF COUNTS OF 1 READ, BUT MULTIPLE READS TO EOF WILL NOT MOVE ANY FURTHER, SO REPOSITION TO THE END OF THE FILE, BUT NOT ONE BEYOND TO KEEP N (THE READ COUNT) CORRET
        ELSEIF(PRESENT(EOL)) THEN
                             IF(COM == BLNK) THEN
                                 C = NEG
                             ELSE
                                 C=INDEX(LINE,COM)-ONE      ! Check for comment symbol
                             END IF
                             !
                             IF (C  < Z) THEN
                                 C=LEN_TRIM(LINE)      ! NO # FOUND, SO USE ENTIRE STRING
                                 IF (C == Z) C=ONE     ! LINE IS BLANK, SET TO 1
                             END IF
                             IF (C == Z) C=ONE         !# IS ON FIRST COLUMN OR LINE IS BLANK, SET TO 1
        END IF
    ELSE
        READ_LOOP: DO
              READ(IU,'(A)',IOSTAT=ERR) LINE
              IF    (ERR > Z) THEN                                   !LINE FAILED TO READ, THIS IS MOST LIKELY DUE TO END OF FILE LINE,IU,OUTPUT,MSG
                                   WRITE(stderr, "(A, I0)") "ERROR READING LINE FROM FILE ON UNIT ", IU
                                   ERROR STOP  55
              ELSEIF(ERR < Z) THEN !EOF
                                   LINE=BLNK
                                   C=ONE
                                   IF(PRESENT(EOF)) EOF=TRUE
                                   BACKSPACE(IU) !NOTE THAT EOF COUNTS OF 1 READ, BUT MULTIPLE READS TO EOF WILL NOT MOVE ANY FURTHER, SO REPOSITION TO THE END OF THE FILE, BUT NOT ONE BEYOND TO KEEP N (THE READ COUNT) CORRET
                                   EXIT READ_LOOP
              END IF
              !----------------------------------------------------------------------------------------------------------
              IF(COM == BLNK) THEN
                  C = NEG
              ELSE
                  C=INDEX(LINE,COM)-ONE      ! Check for comment symbol
              END IF
              !
              IF (C  < Z) THEN
                  C=LEN_TRIM(LINE)      ! NO # FOUND, SO USE ENTIRE STRING
                  IF (C == Z) C=ONE     ! LINE IS BLANK, SET TO 1
              END IF
              IF (C == Z) C=ONE         !# IS ON FIRST COLUMN OR LINE IS BLANK, SET TO 1
              !----------------------------------------------------------------------------------------------------------
              !
              IF(LINE(C:C) /= COM .AND. LINE(1:C) /= " " .AND. COM /= BLNK) EXIT READ_LOOP
              !
        END DO READ_LOOP
    END IF
    !
    IF(PRESENT(EOL)) EOL = C
    !
  END SUBROUTINE
  !
  ! -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  !
  SUBROUTINE OPEN_READ(FILE, IU, FORM, ACCESS)
      CHARACTER(*),           intent(in ):: FILE
      INTEGER,                intent(out):: IU      ! Unit number file was opened on, set to -1 if there is an error
      CHARACTER(*), optional, intent(in ):: FORM    ! Set to 'FORMATTED' or 'UNFORMATTED' or "TEXT" or "BINARY"
      CHARACTER(*), optional, intent(in ):: ACCESS  ! Set to 'SEQUENTIAL' or 'STREAM'
      !
      CHARACTER(16):: FMTARG, ACCARG
      INTEGER:: IERR
      !
      IF(PRESENT(FORM)) THEN
                        FMTARG=FORM
                        CALL UPPER(FMTARG)
                        IF(FORM == 'TEXT'  ) FMTARG='FORMATTED'
                        IF(FORM == 'BINARY') FMTARG='UNFORMATTED'
      ELSE
                        FMTARG='FORMATTED'
      END IF
      !
      IF(PRESENT(ACCESS)) THEN
                                   ACCARG=ACCESS
                                   CALL UPPER(ACCARG)
      ELSE
          IF(FMTARG =='FORMATTED') THEN
                                   ACCARG='SEQUENTIAL'
          ELSE
                                   ACCARG='STREAM'
          END IF
      END IF
      !
      OPEN(NEWUNIT=IU, FILE=FILE, ACTION="READ", FORM=FMTARG, &
           ACCESS=ACCARG, STATUS="OLD", POSITION='REWIND', IOSTAT=IERR)
      !
      IF(IERR /= Z) IU = NEG
      !
  END SUBROUTINE
  !
  ! -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  !
  SUBROUTINE OPEN_WRITE(FILE, IU, FORM, ACCESS)
      CHARACTER(*),           intent(in ):: FILE
      INTEGER,                intent(out):: IU      ! Unit number file was opened on, set to -1 if there is an error
      CHARACTER(*), optional, intent(in ):: FORM    ! Set to 'FORMATTED' or 'UNFORMATTED' or "TEXT" or "BINARY"
      CHARACTER(*), optional, intent(in ):: ACCESS  ! Set to 'SEQUENTIAL' or 'STREAM'
      !
      CHARACTER(16):: FMTARG, ACCARG
      INTEGER:: IERR
      !
      IF(PRESENT(FORM)) THEN
                        FMTARG=FORM
                        CALL UPPER(FMTARG)
                        IF(FORM == 'TEXT'  ) FMTARG='FORMATTED'
                        IF(FORM == 'BINARY') FMTARG='UNFORMATTED'
      ELSE
                        FMTARG='FORMATTED'
      END IF
      !
      IF(PRESENT(ACCESS)) THEN
                                   ACCARG=ACCESS
                                   CALL UPPER(ACCARG)
      ELSE
          IF(FMTARG =='FORMATTED') THEN
                                   ACCARG='SEQUENTIAL'
          ELSE
                                   ACCARG='STREAM'
          END IF
      END IF
      !
      OPEN(NEWUNIT=IU, FILE=FILE, ACTION="WRITE", FORM=FMTARG, &
           ACCESS=ACCARG, STATUS="REPLACE", POSITION='REWIND', IOSTAT=IERR)
      !
      IF(IERR /= Z) IU = NEG
      !
  END SUBROUTINE
  !
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !
  !  Parse Word Routines
  !
  PURE SUBROUTINE PARSE_WORD_UP(LN, LOC, ISTART, ISTOP, COM)
    ! ASSUMES COMPILER CAN HANDEL LN(I:I-1) AND SETS IT TO BLANK STRING
    CHARACTER(*),        intent(inout):: LN
    INTEGER,             intent(inout):: LOC,ISTART,ISTOP
    CHARACTER, optional, intent(in   ):: COM
    !
    CALL PARSE_WORD(LN, LOC, ISTART, ISTOP, COM)
    !
    IF(ISTART <= ISTOP) CALL UPPER(LN(ISTART:ISTOP))
    !
  END SUBROUTINE
  !
  ! ---------------------------------------------------------------------------------------------
  !
  PURE SUBROUTINE PARSE_WORD(LN, LOC, ISTART, ISTOP, COM)
    ! ASSUMES COMPILER CAN HANDEL LN(I:I-1) AND SETS IT TO BLANK STRING
    CHARACTER(*),        intent(in   ):: LN
    INTEGER,             intent(inout):: LOC,ISTART,ISTOP
    CHARACTER, optional, intent(in   ):: COM
    INTEGER:: LINE_LEN, LINE_TRIM
    !
    LINE_TRIM = LEN_TRIM(LN) + 1   !LEN_TRIM(LN) = 0 if empty line
    !
    IF(PRESENT(COM)) THEN
            IF(COM /= "BLNK" .AND. LINE_TRIM > 1) THEN  !At least 1 non-blank
                  !
                         LINE_LEN = INDEX(LN,COM)                           !TMP VAR TO HOLD COM POSITION
                  IF(Z < LINE_LEN .AND. &
                         LINE_LEN < LINE_TRIM) LINE_TRIM=LINE_LEN  !FOUND COM UPDATE LINE_TRIM TO BE ITS LENGTH
            END IF
    END IF
    !
    DO WHILE( LOC < LINE_TRIM )
                              IF(LN(LOC:LOC) /= TAB .AND. LN(LOC:LOC) /= BLNK .AND. LN(LOC:LOC).NE.',') EXIT
                              LOC = LOC+1
    END DO
    !
    IF( LOC >= LINE_TRIM ) THEN
               LINE_LEN = LEN(LN)
        LOC   =LINE_LEN + ONE
        ISTART=LINE_LEN
        ISTOP =LINE_LEN - ONE
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
                                    IF( LN(LOC:LOC)==TAB .OR. LN(LOC:LOC)==BLNK .OR. LN(LOC:LOC)==',') EXIT
                                    LOC = LOC+1
                                END DO
                                ISTOP = LOC-1
                                IF(ISTOP<ISTART) ISTOP=ISTART
        END IF
        !
    END IF
    !
  END SUBROUTINE
  !
  ! ---------------------------------------------------------------------------------------------
  !
  !!!PURE SUBROUTINE PARSE_LINE(LN, LOC, ISTART, ISTOP, COM)
  !!!  ! ASSUMES COMPILER CAN HANDEL LN(I:I-1) AND SETS IT TO BLANK STRING
  !!!  CHARACTER(*),        intent(in   ):: LN
  !!!  INTEGER,             intent(inout):: LOC,ISTART,ISTOP
  !!!  CHARACTER, optional, intent(in   ):: COM
  !!!  INTEGER:: LINE_LEN, LINE_TRIM
  !!!  !
  !!!  LINE_TRIM = LEN_TRIM(LN) + 1   !LEN_TRIM(LN) = 0 if empty line
  !!!  !
  !!!  IF(PRESENT(COM)) THEN
  !!!          IF(COM /= "BLNK" .AND. LINE_TRIM > 1) THEN  !At least 1 non-blank
  !!!                !
  !!!                       LINE_LEN = INDEX(LN,COM)                           !TMP VAR TO HOLD COM POSITION
  !!!                IF(Z < LINE_LEN .AND. &
  !!!                       LINE_LEN < LINE_TRIM) LINE_TRIM=LINE_LEN  !FOUND COM UPDATE LINE_TRIM TO BE ITS LENGTH
  !!!          END IF
  !!!  END IF
  !!!  !
  !!!  DO WHILE( LOC < LINE_TRIM )
  !!!                            IF(LN(LOC:LOC) /= TAB .AND. LN(LOC:LOC) /= BLNK .AND. LN(LOC:LOC).NE.',') EXIT
  !!!                            LOC = LOC+1
  !!!  END DO
  !!!  !
  !!!  IF( LOC >= LINE_TRIM ) THEN
  !!!             LINE_LEN = LEN(LN)
  !!!      LOC   =LINE_LEN + ONE
  !!!      ISTART=LINE_LEN
  !!!      ISTOP =LINE_LEN - ONE
  !!!  ELSE
  !!!      IF(LN(LOC:LOC)=='"') THEN
  !!!                              LOC = LOC+1
  !!!                              ISTART = LOC
  !!!                              DO WHILE( LOC < LINE_TRIM )
  !!!                                  IF( LN(LOC:LOC) == '"' ) EXIT
  !!!                                  LOC = LOC+1
  !!!                              END DO
  !!!                              ISTOP = LOC-1
  !!!                              LOC = LOC+1
  !!!      ELSEIF(LN(LOC:LOC)=="'") THEN
  !!!                              LOC = LOC+1
  !!!                              ISTART = LOC
  !!!                              DO WHILE( LOC < LINE_TRIM )
  !!!                                  IF( LN(LOC:LOC) == "'" ) EXIT
  !!!                                  LOC = LOC+1
  !!!                              END DO
  !!!                              ISTOP = LOC-1
  !!!                              LOC = LOC+1
  !!!
  !!!      ELSE
  !!!                              ISTART = LOC
  !!!                              LOC = LOC+1
  !!!                              DO WHILE( LOC < LINE_TRIM )
  !!!                                  IF( LN(LOC:LOC)==TAB .OR. LN(LOC:LOC)==BLNK .OR. LN(LOC:LOC)==',') EXIT
  !!!                                  LOC = LOC+1
  !!!                              END DO
  !!!                              ISTOP = LOC-1
  !!!                              IF(ISTOP<ISTART) ISTOP=ISTART
  !!!      END IF
  !!!      !
  !!!  END IF
  !!!  !
  !!!END SUBROUTINE
  !
  ! -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  !
  PURE SUBROUTINE UPPER(LN)
    CHARACTER(*), intent(inout):: LN
    INTEGER:: I, N
    !
    DO I=1, LEN_TRIM(LN)
        N = INDEX( "abcdefghijklmnopqrstuvwxyz", LN(I:I))
        !
        IF(N > 0) LN(I:I) = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"(N:N)
    END DO
  END SUBROUTINE
  !
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !
  !  GET routines
  !
  SUBROUTINE GET_DOUBLE_VEC(LN, LOC, ISTART, ISTOP, VAL, ERROR_VAL, IOSTAT, COM)
    CHARACTER(*),                  intent(in   ):: LN                ! LINE TO PARSE DOUBLE FROM
    INTEGER,                       intent(inout):: LOC,ISTART,ISTOP  ! LOC => STARTING LOCATION TO FIND NUMBER, NUMBER AT EXIT IS LOCATED AT LN(ISTART:ISTOP) AND LOC = ISTOP+1
    REAL(rel64),     dimension(:), intent(out  ):: VAL               ! DOUBLE VALUE TO RETURN
    REAL(rel64),         optional, intent(in   ):: ERROR_VAL         ! IF FAIL TO LOAD NUMBER, DO NOT STOP, BUT INSTEAD SET VAL =  ERROR_VAL
    INTEGER,             optional, intent(  out):: IOSTAT            ! SET to zero if there is no error, otherwise non-zero error code
    CHARACTER,           optional, intent(in   ):: COM               ! Comment marker to stop by
    CONTIGUOUS:: VAL
    LOGICAL:: MUST_STOP
    INTEGER:: IERR, I
    CHARACTER(:), allocatable:: ERRMSG
    !
    MUST_STOP = FALSE
    VAL  = DZ
    IERR = Z
    !
    DO I=ONE,SIZE(VAL)
              CALL PARSE_WORD(LN,LOC,ISTART,ISTOP,COM)
              !
              SELECT CASE(LN(ISTART:ISTOP))
              CASE('NAN',  'NaN', 'nan'); VAL(I) = IEEE_VALUE(VAL(I), IEEE_QUIET_NAN)
              CASE('INF',  'inf', 'Inf'); VAL(I) =  HUGE(VAL)
              CASE('-INF','-inf','-Inf'); VAL(I) = -HUGE(VAL)
              CASE(  '-1', '-1.','-1.0'); VAL(I) = DNEG
              CASE(   '0',  '0.', '0.0'); VAL(I) = DZ
              CASE(   '1',  '1.', '1.0'); VAL(I) = UNO
              CASE DEFAULT
                          READ(LN(ISTART:ISTOP),*, IOSTAT=IERR) VAL(I)
              END SELECT
              !
              IF(IERR.NE.Z .OR. LN(ISTART:ISTOP)==BLNK) THEN
                    MUST_STOP = TRUE
                    IF(PRESENT(ERROR_VAL)) THEN
                             !
                             MUST_STOP = FALSE
                             VAL(I) = ERROR_VAL
                             !
                    ELSEIF(PRESENT(IOSTAT)) THEN
                         !
                         IF(IERR == Z .AND. LN(ISTART:ISTOP)==BLNK) THEN
                                                                    IOSTAT = -1
                         ELSE
                                                                    IOSTAT = IERR
                         END IF
                         MUST_STOP = FALSE
                         VAL(I) = IEEE_VALUE(VAL(I), IEEE_QUIET_NAN)   !SET VAL(I) TO NaN WHEN THERE IS NOSTOP OPTION
                    END IF
                    !
                    IF(MUST_STOP) THEN
                        ERRMSG = 'GET_VALUE READ UTILITY ERROR: FAILED TO CONVERT TEXT TO DOUBLE PRECISION NUMBER.'//NL//NL//'THE FOLLOWING IS THE TEXT ATTEMPTED TO BE CONVERTED "'//LN(ISTART:ISTOP)//'".'//NL//'(IF THIS IS BLANK/EMPTY THEN YOU MAY NOT HAVE ENOUGH NUMBERS SPECIFIED ON THE LINE)'
                        IF(LN(ISTART:ISTOP)==BLNK) ERRMSG = ERRMSG//NL//NL//'THE POSSIBLE REASON FOR THIS ERROR IS DUE TO READING in A BLANK/EMPTY LINE OR YOU DID NOT PROVIDE ENOUGH NUMBERS ON THE LINE.'
                        !
                        WRITE(stderr, "(A)") ERRMSG
                        ERROR STOP 500
                    END IF
              END IF
    END DO
    !
  END SUBROUTINE
  !
  ! ---------------------------------------------------------------------------------------------
  !
  SUBROUTINE GET_DOUBLE_VAL(LN, LOC, ISTART, ISTOP, VAL, ERROR_VAL, IOSTAT, COM)
    CHARACTER(*),           intent(in   ):: LN                ! LINE TO PARSE DOUBLE FROM
    INTEGER,                intent(inout):: LOC,ISTART,ISTOP  ! LOC => STARTING LOCATION TO FIND NUMBER, NUMBER AT EXIT IS LOCATED AT LN(ISTART:ISTOP) AND LOC = ISTOP+1
    REAL(rel64),            intent(out  ):: VAL               ! DOUBLE VALUE TO RETURN
    REAL(rel64),  optional, intent(in   ):: ERROR_VAL         ! IF FAIL TO LOAD NUMBER, DO NOT STOP, BUT INSTEAD SET VAL =  ERROR_VAL
    INTEGER,      optional, intent(  out):: IOSTAT            ! SET to zero if there is no error, otherwise non-zero error code
    CHARACTER,    optional, intent(in   ):: COM               ! Comment marker to stop by
    LOGICAL:: CHECK
    INTEGER:: IERR
    CHARACTER(:), allocatable:: ERRMSG
    !
    CALL PARSE_WORD(LN,LOC,ISTART,ISTOP,COM)
    !
    IERR = Z
    SELECT CASE(LN(ISTART:ISTOP))
    CASE('NAN',  'NaN', 'nan'); VAL = IEEE_VALUE(VAL, IEEE_QUIET_NAN)
    CASE('INF',  'inf', 'Inf'); VAL =  HUGE(VAL)
    CASE('-INF','-inf','-Inf'); VAL = -HUGE(VAL)
    CASE(  '-1', '-1.','-1.0'); VAL = DNEG
    CASE(   '0',  '0.', '0.0'); VAL = DZ
    CASE(   '1',  '1.', '1.0'); VAL = UNO
    CASE DEFAULT
                READ(LN(ISTART:ISTOP),*, IOSTAT=IERR) VAL
    END SELECT
    !

    !
    IF(IERR.NE.Z .OR. LN(ISTART:ISTOP)==BLNK) THEN
          CHECK = TRUE
          IF(PRESENT(ERROR_VAL)) THEN
                   !
                   CHECK = FALSE
                   VAL = ERROR_VAL
                   !
          ELSEIF(PRESENT(IOSTAT)) THEN
               !
               IF(IERR == Z .AND. LN(ISTART:ISTOP)==BLNK) THEN
                                                          IOSTAT = -1
               ELSE
                                                          IOSTAT = IERR
               END IF
               CHECK  = FALSE
               VAL = IEEE_VALUE(VAL, IEEE_QUIET_NAN)   !SET VAL(I) TO NaN WHEN THERE IS NOSTOP OPTION
          END IF
          !
          IF(CHECK) THEN
              ERRMSG = 'GET_VALUE READ UTILITY ERROR: FAILED TO CONVERT TEXT TO DOUBLE PRECISION NUMBER.'//NL//NL//'THE FOLLOWING IS THE TEXT ATTEMPTED TO BE CONVERTED "'//LN(ISTART:ISTOP)//'".'
              IF(LN(ISTART:ISTOP)==BLNK) ERRMSG = ERRMSG//NL//NL//'THE POSSIBLE REASON FOR THIS ERROR IS DUE TO READING in A BLANK/EMPTY LINE OR YOU DID NOT PROVIDE ENOUGH NUMBERS ON THE LINE.'
              !
              WRITE(stderr, "(A)") ERRMSG
              ERROR STOP 501
          END IF
    END IF
    !
  END SUBROUTINE
  !
  ! ---------------------------------------------------------------------------------------------
  !
  SUBROUTINE GET_INTEGER_VEC(LN, LOC, ISTART, ISTOP, VAL, ERROR_VAL, IOSTAT, COM)
    CHARACTER(*),                  intent(in   ):: LN                ! LINE TO PARSE DOUBLE FROM
    INTEGER,                       intent(inout):: LOC,ISTART,ISTOP  ! LOC => STARTING LOCATION TO FIND NUMBER, NUMBER AT EXIT IS LOCATED AT LN(ISTART:ISTOP) AND LOC = ISTOP+1
    INTEGER,         dimension(:), intent(out  ):: VAL               ! DOUBLE VALUE TO RETURN
    INTEGER,             optional, intent(in   ):: ERROR_VAL         ! IF FAIL TO LOAD NUMBER, DO NOT STOP, BUT INSTEAD SET VAL =  ERROR_VAL
    INTEGER,             optional, intent(  out):: IOSTAT            ! SET to zero if there is no error, otherwise non-zero error code
    CHARACTER,           optional, intent(in   ):: COM               ! Comment marker to stop by
    CONTIGUOUS:: VAL
    LOGICAL:: MUST_STOP
    INTEGER:: IERR, I
    CHARACTER(:), allocatable:: ERRMSG
    !
    MUST_STOP = FALSE
    VAL = Z
    !
    DO I=ONE,SIZE(VAL)
              CALL PARSE_WORD(LN,LOC,ISTART,ISTOP,COM)
              !
              READ(LN(ISTART:ISTOP),*, IOSTAT=IERR) VAL(I)
              !
              IF(IERR.NE.Z .OR. LN(ISTART:ISTOP)==BLNK) THEN
                    MUST_STOP = TRUE
                    IF(PRESENT(ERROR_VAL)) THEN
                             !
                             MUST_STOP = FALSE
                             VAL(I) = ERROR_VAL
                             !
                    ELSEIF(PRESENT(IOSTAT)) THEN
                         !
                         IF(IERR == Z .AND. LN(ISTART:ISTOP)==BLNK) THEN
                                                                    IOSTAT = -1
                         ELSE
                                                                    IOSTAT = IERR
                         END IF
                         MUST_STOP = FALSE
                         VAL(I) = HUGE(I)  !SET VAL TO HUGE(I) WHEN THERE IS NOSTOP OPTION. NOTE NAN DOES NOT EXIST FOR INT
                    END IF
                    !
                    IF(MUST_STOP) THEN
                        IERR = SIZE(VAL)
                        ERRMSG = 'GET_VALUE READ UTILITY ERROR: EXPECTED TO READ '//INT2STR(IERR)//' INTEGERS, BUT FAILED TO CONVERT THE POSITION '//INT2STR(I)//' TEXT TO INTEGER.'//NL//NL//'THE FOLLOWING IS THE TEXT ATTEMPTED TO BE CONVERTED "'//LN(ISTART:ISTOP)//'".'//NL//'(IF THIS IS BLANK/EMPTY THEN YOU MAY NOT HAVE ENOUGH NUMBERS SPECIFIED ON THE LINE)'
                        IF(LN(ISTART:ISTOP)==BLNK) ERRMSG = ERRMSG//NL//NL//'THE POSSIBLE REASON FOR THIS ERROR IS DUE TO READING in A BLANK/EMPTY LINE OR YOU DID NOT PROVIDE ENOUGH NUMBERS ON THE LINE.'
                        !
                        WRITE(stderr, "(A)") ERRMSG
                        ERROR STOP 502
                    END IF
              END IF
    END DO
    !
  END SUBROUTINE
  !
  ! ---------------------------------------------------------------------------------------------
  !
  SUBROUTINE GET_INTEGER_VAL(LN, LOC, ISTART, ISTOP, VAL, ERROR_VAL, IOSTAT, COM)
    CHARACTER(*),           intent(in   ):: LN                ! LINE TO PARSE DOUBLE FROM
    INTEGER,                intent(inout):: LOC,ISTART,ISTOP  ! LOC => STARTING LOCATION TO FIND NUMBER, NUMBER AT EXIT IS LOCATED AT LN(ISTART:ISTOP) AND LOC = ISTOP+1
    INTEGER,                intent(out  ):: VAL               ! DOUBLE VALUE TO RETURN
    INTEGER,      optional, intent(in   ):: ERROR_VAL         ! IF FAIL TO LOAD NUMBER, DO NOT STOP, BUT INSTEAD SET VAL =  ERROR_VAL
    INTEGER,      optional, intent(  out):: IOSTAT            ! SET to zero if there is no error, otherwise non-zero error code
    CHARACTER,    optional, intent(in   ):: COM               ! Comment marker to stop by
    LOGICAL:: CHECK
    INTEGER:: IERR
    CHARACTER(:), allocatable:: ERRMSG
    !
    CALL PARSE_WORD(LN,LOC,ISTART,ISTOP,COM)
    !
    READ(LN(ISTART:ISTOP),*, IOSTAT=IERR) VAL
    !
    IF(IERR.NE.Z .OR. LN(ISTART:ISTOP)==BLNK) THEN
          CHECK = TRUE
          IF(PRESENT(ERROR_VAL)) THEN
                   !
                   CHECK = FALSE
                   VAL = ERROR_VAL
                   !
          ELSEIF(PRESENT(IOSTAT)) THEN
               !
               IF(IERR == Z .AND. LN(ISTART:ISTOP)==BLNK) THEN
                                                          IOSTAT = -1
               ELSE
                                                          IOSTAT = IERR
               END IF
               CHECK  = FALSE
               VAL = HUGE(VAL)  !SET VAL TO HUGE(I) WHEN THERE IS NOSTOP OPTION. NOTE NAN DOES NOT EXIST FOR INT
          END IF
          !
          IF(CHECK) THEN
              ERRMSG = 'GET_VALUE READ UTILITY ERROR: FAILED TO CONVERT TEXT TO INTEGER NUMBER.'//NL//NL//'THE FOLLOWING IS THE TEXT ATTEMPTED TO BE CONVERTED "'//LN(ISTART:ISTOP)//'".'
              IF(LN(ISTART:ISTOP)==BLNK) ERRMSG = ERRMSG//NL//NL//'THE POSSIBLE REASON FOR THIS ERROR IS DUE TO READING in A BLANK/EMPTY LINE OR YOU DID NOT PROVIDE ENOUGH NUMBERS ON THE LINE.'
              !
              WRITE(stderr, "(A)") ERRMSG
              ERROR STOP 503
          END IF
    END IF
    !
  END SUBROUTINE
  !
  ! ---------------------------------------------------------------------------------------------
  !
  SUBROUTINE GET_REAL_VEC(LN, LOC, ISTART, ISTOP, VAL, ERROR_VAL, IOSTAT, COM)
    CHARACTER(*),                  intent(in   ):: LN                ! LINE TO PARSE DOUBLE FROM
    INTEGER,                       intent(inout):: LOC,ISTART,ISTOP  ! LOC => STARTING LOCATION TO FIND NUMBER, NUMBER AT EXIT IS LOCATED AT LN(ISTART:ISTOP) AND LOC = ISTOP+1
    REAL(rel32),     dimension(:), intent(out  ):: VAL               ! DOUBLE VALUE TO RETURN
    REAL(rel32),         optional, intent(in   ):: ERROR_VAL         ! IF FAIL TO LOAD NUMBER, DO NOT STOP, BUT INSTEAD SET VAL =  ERROR_VAL
    INTEGER,             optional, intent(  out):: IOSTAT            ! SET to zero if there is no error, otherwise non-zero error code
    CHARACTER,           optional, intent(in   ):: COM               ! Comment marker to stop by
    CONTIGUOUS:: VAL
    LOGICAL:: MUST_STOP
    INTEGER:: IERR, I
    CHARACTER(:), allocatable:: ERRMSG
    !
    MUST_STOP = FALSE
    VAL  = DZ
    IERR = Z
    !
    DO I=ONE,SIZE(VAL)
              CALL PARSE_WORD(LN,LOC,ISTART,ISTOP,COM)
              !
              SELECT CASE(LN(ISTART:ISTOP))
              CASE('NAN',  'NaN', 'nan'); VAL(I) = IEEE_VALUE(VAL(I), IEEE_QUIET_NAN)
              CASE('INF',  'inf', 'Inf'); VAL(I) =  HUGE(VAL)
              CASE('-INF','-inf','-Inf'); VAL(I) = -HUGE(VAL)
              CASE(  '-1', '-1.','-1.0'); VAL(I) = -1.0
              CASE(   '0',  '0.', '0.0'); VAL(I) =  0.0
              CASE(   '1',  '1.', '1.0'); VAL(I) =  1.0
              CASE DEFAULT
                          READ(LN(ISTART:ISTOP),*, IOSTAT=IERR) VAL(I)
              END SELECT
              !
              IF(IERR.NE.Z .OR. LN(ISTART:ISTOP)==BLNK) THEN
                    MUST_STOP = TRUE
                    IF(PRESENT(ERROR_VAL)) THEN
                             !
                             MUST_STOP = FALSE
                             VAL(I) = ERROR_VAL
                             !
                    ELSEIF(PRESENT(IOSTAT)) THEN
                         !
                         IF(IERR == Z .AND. LN(ISTART:ISTOP)==BLNK) THEN
                                                                    IOSTAT = -1
                         ELSE
                                                                    IOSTAT = IERR
                         END IF
                         MUST_STOP = FALSE
                         VAL(I) = IEEE_VALUE(VAL(I), IEEE_QUIET_NAN)   !SET VAL(I) TO NaN WHEN THERE IS NOSTOP OPTION
                    END IF
                    !
                    IF(MUST_STOP) THEN
                        ERRMSG = 'GET_VALUE READ UTILITY ERROR: FAILED TO CONVERT TEXT TO SINGLE PRECISION NUMBER.'//NL//NL//'THE FOLLOWING IS THE TEXT ATTEMPTED TO BE CONVERTED "'//LN(ISTART:ISTOP)//'".'//NL//'(IF THIS IS BLANK/EMPTY THEN YOU MAY NOT HAVE ENOUGH NUMBERS SPECIFIED ON THE LINE)'
                        IF(LN(ISTART:ISTOP)==BLNK) ERRMSG = ERRMSG//NL//NL//'THE POSSIBLE REASON FOR THIS ERROR IS DUE TO READING in A BLANK/EMPTY LINE OR YOU DID NOT PROVIDE ENOUGH NUMBERS ON THE LINE.'
                        !
                        WRITE(stderr, "(A)") ERRMSG
                        ERROR STOP 504
                    END IF
              END IF
    END DO
    !
  END SUBROUTINE
  !
  ! ---------------------------------------------------------------------------------------------
  !
  SUBROUTINE GET_REAL_VAL(LN, LOC, ISTART, ISTOP, VAL, ERROR_VAL, IOSTAT, COM)
    CHARACTER(*),           intent(in   ):: LN                ! LINE TO PARSE DOUBLE FROM
    INTEGER,                intent(inout):: LOC,ISTART,ISTOP  ! LOC => STARTING LOCATION TO FIND NUMBER, NUMBER AT EXIT IS LOCATED AT LN(ISTART:ISTOP) AND LOC = ISTOP+1
    REAL(rel32),            intent(out  ):: VAL               ! DOUBLE VALUE TO RETURN
    REAL(rel32),  optional, intent(in   ):: ERROR_VAL         ! IF FAIL TO LOAD NUMBER, DO NOT STOP, BUT INSTEAD SET VAL =  ERROR_VAL
    INTEGER,      optional, intent(  out):: IOSTAT            ! SET to zero if there is no error, otherwise non-zero error code
    CHARACTER,    optional, intent(in   ):: COM               ! Comment marker to stop by
    LOGICAL:: CHECK
    INTEGER:: IERR
    CHARACTER(:), allocatable:: ERRMSG
    !
    CALL PARSE_WORD(LN,LOC,ISTART,ISTOP,COM)
    !
    IERR = Z
    SELECT CASE(LN(ISTART:ISTOP))
    CASE('NAN',  'NaN', 'nan'); VAL = IEEE_VALUE(VAL, IEEE_QUIET_NAN)
    CASE('INF',  'inf', 'Inf'); VAL =  HUGE(VAL)
    CASE('-INF','-inf','-Inf'); VAL = -HUGE(VAL)
    CASE(  '-1', '-1.','-1.0'); VAL = -1.0
    CASE(   '0',  '0.', '0.0'); VAL =  0.0
    CASE(   '1',  '1.', '1.0'); VAL =  1.0
    CASE DEFAULT
                READ(LN(ISTART:ISTOP),*, IOSTAT=IERR) VAL
    END SELECT
    !
    IF(IERR.NE.Z .OR. LN(ISTART:ISTOP)==BLNK) THEN
          CHECK = TRUE
          IF(PRESENT(ERROR_VAL)) THEN
                   !
                   CHECK = FALSE
                   VAL = ERROR_VAL
                   !
          ELSEIF(PRESENT(IOSTAT)) THEN
               !
               IF(IERR == Z .AND. LN(ISTART:ISTOP)==BLNK) THEN
                                                          IOSTAT = -1
               ELSE
                                                          IOSTAT = IERR
               END IF
               CHECK  = FALSE
               VAL = IEEE_VALUE(VAL, IEEE_QUIET_NAN)   !SET VAL(I) TO NaN WHEN THERE IS NOSTOP OPTION
          END IF
          !
          IF(CHECK) THEN
              ERRMSG = 'GET_VALUE READ UTILITY ERROR: FAILED TO CONVERT TEXT TO SINGLE PRECISION NUMBER.'//NL//NL//'THE FOLLOWING IS THE TEXT ATTEMPTED TO BE CONVERTED "'//LN(ISTART:ISTOP)//'".'
              IF(LN(ISTART:ISTOP)==BLNK) ERRMSG = ERRMSG//NL//NL//'THE POSSIBLE REASON FOR THIS ERROR IS DUE TO READING in A BLANK/EMPTY LINE OR YOU DID NOT PROVIDE ENOUGH NUMBERS ON THE LINE.'
              !
              WRITE(stderr, "(A)") ERRMSG
              ERROR STOP 505
          END IF
    END IF
    !
  END SUBROUTINE
  !
  ! ---------------------------------------------------------------------------------------------
  !
  SUBROUTINE GET_WORD_ALLOC(LN, LOC, WORD, NO_UPCASE, COM)  ! SETS WORD TO PARSED WORD, SETS IT TO "" IF END OF LINE OR NO WORD PARSED
    CHARACTER(*),             intent(in   ):: LN                ! LINE TO PARSE WORD FROM
    INTEGER,                  intent(inout):: LOC               ! LOC => STARTING LOCATION TO FIND WORD
    CHARACTER(:),allocatable, intent(  out):: WORD              ! RETURN WORD
    LOGICAL,      optional,   intent(in   ):: NO_UPCASE         ! SET IF YOU WANT TO LEAVE WORD ALONE
    CHARACTER,    optional,   intent(in   ):: COM
    INTEGER:: ISTART, ISTOP
    !
    CALL PARSE_WORD(LN,LOC,ISTART,ISTOP,COM)
    !
    IF(ISTART.LE.ISTOP) THEN
        !
        IF(ALLOCATED(WORD)) DEALLOCATE(WORD)
        ALLOCATE(WORD, SOURCE=LN(ISTART:ISTOP))
        !
        IF(PRESENT(NO_UPCASE)) THEN
            IF(.NOT. NO_UPCASE) CALL UPPER(WORD)
        ELSE
                                CALL UPPER(WORD)
        END IF
    ELSE
        IF(ALLOCATED(WORD)) DEALLOCATE(WORD)
        ALLOCATE(WORD, SOURCE=BLNK)
    END IF
    !
    !
  END SUBROUTINE
  !
  ! ---------------------------------------------------------------------------------------------
  !
  SUBROUTINE GET_WORD_ASSUM_ISTART(LN, LOC, ISTART, ISTOP, WORD, NO_UPCASE, COM)  ! SETS WORD TO PARSED WORD, SETS IT TO "" IF END OF LINE OR NO WORD PARSED
    CHARACTER(*),           intent(in   ):: LN                ! LINE TO PARSE WORD FROM
    INTEGER,                intent(inout):: LOC               ! LOC => STARTING LOCATION TO FIND WORD
    INTEGER,                intent(inout):: ISTART,ISTOP
    CHARACTER(*),           intent(  out):: WORD              ! RETURN WORD
    LOGICAL,      optional, intent(in   ):: NO_UPCASE         !SET IF YOU WANT TO LEAVE WORD ALONE
    CHARACTER,    optional, intent(in   ):: COM
    !
    CALL PARSE_WORD(LN,LOC,ISTART,ISTOP,COM)
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
        WORD = BLNK
    END IF
    !
  END SUBROUTINE
  !
  ! ---------------------------------------------------------------------------------------------
  !
  SUBROUTINE GET_FILE_NAME(IU,FNAME,SET_FNAME)
    INTEGER,                 intent(in ):: IU                ! UNIT NUMBER TO LOOK FILE NAME UP FROM
    CHARACTER(:),allocatable,intent(out):: FNAME             ! FILE NAME ASSOCIATED WITH UNIT NUMBER
    LOGICAL,                 intent(out):: SET_FNAME         ! SET TO TRUE IF THERE IS AN ERROR
    CHARACTER(:), allocatable:: FNAM
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
                         SIZ = 512 * I
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
                   FNAME = 'GET_FILE_NAME ERROR: Failed to identy file name from unit number '//INT2STR(IU)
                   SET_FNAME = FALSE
             END IF
             !
       ELSE
           FNAME = '"UNKNOWN FILE" Failed to identy file name from unit number '//INT2STR(IU)
           SET_FNAME = FALSE
       END IF
    END IF
    !
  END SUBROUTINE
  !
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !
  !  NUM2STR Routines
  !
  PURE FUNCTION INT2STR(IVAL,PAD,ZPAD) !,RIGHT
    INTEGER,         intent(in):: IVAL
    INTEGER,optional,intent(in):: PAD
    LOGICAL,optional,intent(in):: ZPAD
    CHARACTER(:),   allocatable:: INT2STR
    CHARACTER(30)::NUM
    !
    WRITE(NUM,'(I30)') IVAL
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
  ! ---------------------------------------------------------------------------------------------
  !
  PURE FUNCTION INTVEC2STR(IVAL,PAD,SEP,ZPAD) RESULT(STR)
    INTEGER,DIMENSION(:),CONTIGUOUS,INTENT(IN):: IVAL
    INTEGER,     OPTIONAL,          INTENT(IN):: PAD
    CHARACTER(*),OPTIONAL,          INTENT(IN):: SEP
    LOGICAL,     OPTIONAL,          INTENT(IN):: ZPAD
    !LOGICAL,OPTIONAL,INTENT(IN):: RIGHT
    CHARACTER(:),   ALLOCATABLE:: STR
    CHARACTER(:),   ALLOCATABLE:: SEPOR
    INTEGER:: I, N
    !
    IF(PRESENT(SEP)) THEN
        ALLOCATE(SEPOR, SOURCE=SEP)
    ELSE
        ALLOCATE(SEPOR, SOURCE=" ")
    END IF
    !
    N = SIZE(IVAL)
    !
    IF(N < 1) THEN
        STR = ''
    ELSE
        STR = INT2STR(IVAL(1),PAD,ZPAD)
    END IF
    !
    IF(N>1) THEN
          DO I=2, N
              STR=STR//SEPOR//INT2STR(IVAL(I),PAD,ZPAD)
          END DO
    END IF
    !
  END FUNCTION
  !
  ! ---------------------------------------------------------------------------------------------
  !
  PURE FUNCTION DBLE2STR(DVAL,PAD,GENERAL)
    REAL(rel64),       intent(in):: DVAL
    INTEGER, optional, intent(in):: PAD
    LOGICAL, optional, intent(in):: GENERAL
    CHARACTER(:),  allocatable :: DBLE2STR
    REAL(rel64):: DVAL1C, DVAL10, DVAL1K
    CHARACTER(15)::NUM !LARGEST POSSIBLE NUMBER IS 14 CHARACTERS
    LOGICAL::GEN
    !
    GEN=.FALSE.; IF(PRESENT(GENERAL)) GEN=GENERAL
    !
    NUM=''
    DVAL10 = 10._rel64*DVAL;   DVAL1C = 100._rel64*DVAL;   DVAL1K = 1000._rel64*DVAL
    !
    IF(DVAL.NE.DVAL) THEN
        NUM='NaN'
    ELSEIF(DVAL >=  HUGE(DVAL)*0.999999999_rel64) THEN
        NUM = 'inf'
    ELSEIF(DVAL <= -HUGE(DVAL)*0.999999999_rel64) THEN
        NUM = '-inf'
    ELSEIF(.NOT. GEN) THEN
    !
    IF(DVAL==DZ)                 THEN
       WRITE(NUM,'(F3.1)') DVAL
    ELSEIF(DVAL>=1e100_rel64 .OR. DVAL<=-1e100_rel64)       THEN
       WRITE(NUM,'(ES15.7E3)') DVAL
    ELSEIF(DVAL>=1e10_rel64 .OR. DVAL<=-1e10_rel64)         THEN
       WRITE(NUM,'(ES15.7E2)') DVAL
    ELSEIF( DVAL10 == AINT(DVAL10) .AND. (DVAL10>=UNO.OR.DVAL10<=DNEG) ) THEN
       WRITE(NUM,'(F15.1)') DVAL
    ELSEIF( DVAL1C == AINT(DVAL1C) .AND. (DVAL1C>=UNO.OR.DVAL1C<=DNEG) ) THEN
       WRITE(NUM,'(F15.2)') DVAL
    ELSEIF( DVAL1K == AINT(DVAL1K) .AND. (DVAL1K>=UNO.OR.DVAL1K<=DNEG) ) THEN
       WRITE(NUM,'(F15.3)') DVAL
    ELSEIF(DVAL>=1e6_rel64 .OR. DVAL<=-1e6_rel64)           THEN
       WRITE(NUM,'(ES15.7E1)') DVAL
    ELSEIF(DVAL>=1e2_rel64 .OR. DVAL<=-1e2_rel64 )          THEN
       WRITE(NUM,'(F15.5)') DVAL
    ELSEIF(DVAL>=0.00099e0_rel64 .OR. DVAL<=-0.00099e0_rel64 )  THEN
       WRITE(NUM,'(F15.7)') DVAL
    ELSEIF(DVAL>=1e-9_rel64 .OR. DVAL<=-1e-9_rel64)         THEN
       WRITE(NUM,'(ES15.7E1)') DVAL
    ELSEIF(DVAL>=1e-99_rel64 .OR. DVAL<=-1e-99_rel64)       THEN
       WRITE(NUM,'(ES15.7E2)') DVAL
    ELSEIF(DVAL>DZ .OR. DVAL<DZ)              THEN
       WRITE(NUM,'(ES15.7E3)') DVAL
    END IF
    !
    ELSE
        WRITE(NUM,'(ES15.6)') DVAL
    END IF
    !
    IF(PRESENT(PAD)) THEN
        !
        NUM = ADJUSTL(NUM)
        !
        IF( LEN_TRIM(NUM) < ABS(PAD)) THEN
                IF (PAD>0) THEN
                           DBLE2STR = TRIM( REPEAT(' ',PAD-LEN_TRIM(NUM))//NUM )
                ELSE
                           DBLE2STR = TRIM(NUM)//REPEAT(' ',ABS(PAD)-LEN_TRIM(NUM))
                END IF
        ELSE
                           DBLE2STR = TRIM(NUM)
        END IF
    ELSE
        DBLE2STR = TRIM(ADJUSTL(NUM))
    END IF
    !
  END FUNCTION
  !
  ! ---------------------------------------------------------------------------------------------
  !
  PURE FUNCTION TF2STR(LVAL, PAD, fmt)  
    LOGICAL,                INTENT(IN):: LVAL
    INTEGER,      OPTIONAL, INTENT(IN):: PAD
    CHARACTER(*), OPTIONAL, INTENT(IN):: fmt ! if not present then output is T for true and F for false. fmt indicates output format such as fmt='1' for '0' or '1' output, or fmt='TR' to get 'TRUE' 'FALSE' output, or fmt='Tr' to get 'True' 'False' output
    CHARACTER(:), ALLOCATABLE:: TF2STR
    CHARACTER(5)::TF
    INTEGER:: N
    TF = ''
    IF(PRESENT(fmt)) THEN
       N = LEN_TRIM(fmt)
       IF( N > 1 ) THEN
          SELECT CASE(fmt(1:2))
          CASE('Tr','Fa')
                          IF(LVAL) THEN
                              TF = 'True'
                          ELSE
                              TF = 'False'
                          END IF
          CASE('tr','fa')
                          IF(LVAL) THEN
                              TF = 'true'
                          ELSE
                              TF = 'false'
                          END IF
          CASE('TR','FA')
                          IF(LVAL) THEN
                              TF = 'TRUE'
                          ELSE
                              TF = 'FALSE'
                          END IF
          END SELECT
       END IF
       !
       IF( N == 1 .or. TF == '' ) THEN
          SELECT CASE(fmt(1:1))
          CASE('1','0')
                    IF(LVAL) THEN
                        TF = '1'
                    ELSE
                        TF = '0'
                    END IF
          CASE('t','f')
                    IF(LVAL) THEN
                        TF = 't'
                    ELSE
                        TF = 'f'
                    END IF
          CASE('T','F')
                    IF(LVAL) THEN
                        TF = 'T'
                    ELSE
                        TF = 'F'
                    END IF
          END SELECT
       END IF
    END IF
    !
    if( TF == '') then
        IF(LVAL) THEN
            TF = 'T'
        ELSE
            TF = 'F'
        END IF
    end if
    !   
    IF(PRESENT(PAD)) THEN
        !
        TF = ADJUSTL(TF)
        !
        IF( LEN_TRIM(TF) < ABS(PAD)) THEN
                IF (PAD>0) THEN
                           TF2STR = TRIM( REPEAT(' ',PAD-LEN_TRIM(TF))//TF )
                ELSE
                           TF2STR = TRIM(TF)//REPEAT(' ',ABS(PAD)-LEN_TRIM(TF))
                END IF
        ELSE
                           TF2STR = TRIM(TF)
        END IF
    ELSE
        TF2STR = TRIM(ADJUSTL(TF))
    END IF
       
    !
  END FUNCTION
  !
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !
  !  RUN COMMAND Routines
  !
  SUBROUTINE CONSOLE_CMD(CMD, IOSTAT)
    CHARACTER(*),           intent(in   ):: CMD
    INTEGER,                intent(inout):: IOSTAT
    INTEGER:: CMDSTAT
    !
    CALL EXECUTE_COMMAND_LINE(CMD, TRUE, IOSTAT, CMDSTAT)
    !
    IF(CMDSTAT == NEG) IOSTAT = CMDSTAT
    !
  END SUBROUTINE
  !
  ! ---------------------------------------------------------------------------------------------
  !
  SUBROUTINE CONSOLE_CMD_OUTPUT(CMD, IOSTAT, OUTPUT)  ! OUTPUT cannot exceed 1024 characters in a line cause FNAM is used to read it back in
    CHARACTER(*),              intent(in   ):: CMD
    INTEGER,                   intent(inout):: IOSTAT
    CHARACTER(:), allocatable, intent(inout):: OUTPUT
    CHARACTER(:), allocatable:: FNAM
    CHARACTER(3):: BREAK
    INTEGER:: IU, I, J, N, CMDSTAT
    LOGICAL:: CHECK
    !
    OPEN(NEWUNIT=IU, STATUS='SCRATCH',  ACTION='READWRITE',   &
                     FORM='FORMATTED',  ACCESS='SEQUENTIAL',  &
                     POSITION='REWIND', IOSTAT=IOSTAT)
    !
    IF(IOSTAT /= Z .OR. IU == NEG) THEN
                    OUTPUT = "ERROR FAILED TO GET CONSOLE OUTPUT"
                    CALL EXECUTE_COMMAND_LINE(CMD, TRUE, IOSTAT, CMDSTAT)
                    IF(CMDSTAT == NEG) IOSTAT = CMDSTAT
                    RETURN
    END IF
    !
    ALLOCATE(CHARACTER(1024):: FNAM, STAT=J)
    INQUIRE(IU, NAME=FNAM)
    INQUIRE(FILE=FNAM, EXIST=CHECK)     !CHECK IF FILE NAME SIZE IS BIG ENOUGH
    !
    IF(.NOT. CHECK) THEN                !Search for size that fits file name
        DO I=3, 15
                DEALLOCATE(FNAM)
                N = I * 512
                ALLOCATE(CHARACTER(N):: FNAM, STAT=J)
                INQUIRE(IU, NAME=FNAM)
                INQUIRE(FILE=FNAM, EXIST=CHECK)
                IF(CHECK .OR. J /= Z) EXIT
        END DO
    END IF
    !
    CLOSE(IU, IOSTAT=N) !Have file name, now close/delete it for use by EXECUTE_COMMAND_LINE
    !
    I = LEN_TRIM(FNAM)
    IF(CHECK .AND. J == Z .AND. I > Z) THEN
        OUTPUT = CMD//" > "//FNAM(ONE:I)
        !
        CALL EXECUTE_COMMAND_LINE(OUTPUT, TRUE, IOSTAT, CMDSTAT)
        !
        IF(CMDSTAT == NEG) IOSTAT = CMDSTAT
    ELSE
        OUTPUT = "ERROR FAILED TO GET CONSOLE OUTPUT"
        CALL EXECUTE_COMMAND_LINE(CMD, TRUE, IOSTAT, CMDSTAT)
        IF(CMDSTAT == NEG) IOSTAT = CMDSTAT
        RETURN
    END IF
    !
    IF(IOSTAT /= Z) THEN
                    OUTPUT = "ERROR"  !Something caused CMD to fail
    ELSE
        !OPEN(NEWUNIT=IU, FILE=FNAM(ONE:I), ACTION="READ", FORM=FMTARG, &
        !     ACCESS=ACCARG, STATUS="UNKOWN", POSITION='REWIND', IOSTAT=IERR)
        OPEN(NEWUNIT=IU, FILE=FNAM(ONE:I),                        &
                         STATUS='UNKNOWN',  ACTION='READ',        &
                         FORM='FORMATTED',  ACCESS='SEQUENTIAL',  &
                         POSITION='REWIND', IOSTAT=J)
        IF(J /= Z) THEN
                   OUTPUT = "ERROR FAILED TO GET CONSOLE OUTPUT"
                   RETURN
        END IF
        !
        READ(IU,'(A)',IOSTAT=J) FNAM
        !
        I = LEN_TRIM(FNAM)
        IF(I > Z .AND. J == Z) THEN
                               !
                               BREAK = BLNK//NL//BLNK
                               !
                               OUTPUT = BLNK//FNAM(1:I)
                               !
                               READ(IU,'(A)',IOSTAT=J) FNAM
                               DO WHILE(J == Z)
                                        I = LEN_TRIM(FNAM)
                                        IF(I > Z)THEN
                                            OUTPUT = OUTPUT//BREAK//FNAM(1:I)
                                        ELSE
                                            OUTPUT = OUTPUT//BREAK
                                        END IF
                                        READ(IU,'(A)',IOSTAT=J) FNAM
                               END DO

        ELSEIF(         J < Z) THEN             !EMPTY SCRATCH DOCUMENT
                               OUTPUT = "    "
        ELSE
                  IOSTAT = NEG
                  OUTPUT = "ERROR"
        END IF
        !
        CLOSE(IU, STATUS="DELETE", IOSTAT=J)
        !
    END IF
    !
  END SUBROUTINE
  !
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !
  SUBROUTINE ASSSERT_KILL(UT, PASSED, KILL)
    CLASS(UNIT_TESTS),      intent(inout):: UT
    LOGICAL,                intent(in   ):: PASSED
    LOGICAL,      optional, intent(in   ):: KILL
    IF(.not. PASSED) THEN
       if( present(KILL)) then
               if( KILL ) then
                          CALL END_TEST_UNIT_TEST(UT)
                          ERROR STOP "ASSERT TEST FAILED - CAN NOT CONTINUE"
               end if
       end if
    end if
  END SUBROUTINE
  !
  SUBROUTINE ASSERT_UNIT_TEST_0D_TF(UT, TEST, WANT, MSG, KILL)
    CLASS(UNIT_TESTS),      intent(inout):: UT
    LOGICAL,                intent(in   ):: TEST
    LOGICAL,      optional, intent(in   ):: WANT
    CHARACTER(*), optional, intent(in   ):: MSG
    LOGICAL,      optional, intent(in   ):: KILL
    LOGICAL:: PASSED
    !
    PASSED = TEST
    if(present(WANT)) PASSED = TEST .eqv. WANT
    !
    CALL TEST_STATUS_UNIT_TEST(UT, PASSED, MSG=MSG)
    !
    CALL ASSSERT_KILL(UT, PASSED, KILL)
  END SUBROUTINE
  !
  ! ---------------------------------------------------------------------------------------------
  !
  SUBROUTINE ASSERT_UNIT_TEST_0D_TFOP(UT, A, OP, B, MSG, KILL)
    CLASS(UNIT_TESTS),      intent(inout):: UT
    CHARACTER(2),           intent(in   ):: OP
    LOGICAL,                intent(in   ):: A, B
    CHARACTER(*), optional, intent(in   ):: MSG
    LOGICAL,      optional, intent(in   ):: KILL
    LOGICAL:: PASSED
    !
    SELECT CASE(OP)
    CASE('EQ', 'eq', 'Eq', 'eQ');  PASSED = A .eqv.  B
    CASE('LT', 'lt', 'Lt', 'lT');  PASSED = A .neqv. B
    CASE('LE', 'le', 'Le', 'lE');  PASSED = A .eqv.  B
    CASE('GT', 'gt', 'Gt', 'gT');  PASSED = A .neqv. B
    CASE('GE', 'ge', 'Ge', 'gE');  PASSED = A .eqv.  B
    CASE('NE', 'ne', 'Ne', 'nE');  PASSED = A .neqv. B
    CASE DEFAULT
        PASSED = A .eqv. B
    END SELECT
    !
    CALL TEST_STATUS_UNIT_TEST(UT, PASSED, MSG=MSG)
    !
    CALL ASSSERT_KILL(UT, PASSED, KILL)
    !
  END SUBROUTINE
  !
  ! ---------------------------------------------------------------------------------------------
  !
  SUBROUTINE ASSERT_UNIT_TEST_0D_INT8(UT, A, OP, B, MSG, KILL)
    CLASS(UNIT_TESTS),      intent(inout):: UT
    CHARACTER(2),           intent(in   ):: OP
    INTEGER(INT8),          intent(in   ):: A, B
    CHARACTER(*), optional, intent(in   ):: MSG
    LOGICAL,      optional, intent(in   ):: KILL
    LOGICAL:: PASSED
    !
    SELECT CASE(OP)
    CASE('EQ', 'eq', 'Eq', 'eQ');  PASSED = A .EQ. B
    CASE('LT', 'lt', 'Lt', 'lT');  PASSED = A .LT. B
    CASE('LE', 'le', 'Le', 'lE');  PASSED = A .LE. B
    CASE('GT', 'gt', 'Gt', 'gT');  PASSED = A .GT. B
    CASE('GE', 'ge', 'Ge', 'gE');  PASSED = A .GE. B
    CASE('NE', 'ne', 'Ne', 'nE');  PASSED = A .NE. B
    CASE DEFAULT
        ERROR STOP 'ASSERT_UNIT_TEST_0D_INT8(UT, A, OP, B, MSG, KILL) - Unknown "OP" received'
    END SELECT
    CALL TEST_STATUS_UNIT_TEST(UT, PASSED, MSG=MSG)
    !
    CALL ASSSERT_KILL(UT, PASSED, KILL)
    !
  END SUBROUTINE
  !
  SUBROUTINE ASSERT_UNIT_TEST_0D_INT16(UT, A, OP, B, MSG, KILL)
    CLASS(UNIT_TESTS),      intent(inout):: UT
    CHARACTER(2),           intent(in   ):: OP
    INTEGER(INT16),         intent(in   ):: A, B
    CHARACTER(*), optional, intent(in   ):: MSG
    LOGICAL,      optional, intent(in   ):: KILL
    LOGICAL:: PASSED
    !
    SELECT CASE(OP)
    CASE('EQ', 'eq', 'Eq', 'eQ');  PASSED = A .EQ. B
    CASE('LT', 'lt', 'Lt', 'lT');  PASSED = A .LT. B
    CASE('LE', 'le', 'Le', 'lE');  PASSED = A .LE. B
    CASE('GT', 'gt', 'Gt', 'gT');  PASSED = A .GT. B
    CASE('GE', 'ge', 'Ge', 'gE');  PASSED = A .GE. B
    CASE('NE', 'ne', 'Ne', 'nE');  PASSED = A .NE. B
    CASE DEFAULT
        ERROR STOP 'ASSERT_UNIT_TEST_0D_INT16(UT, A, OP, B, MSG, KILL) - Unknown "OP" received'
    END SELECT
    CALL TEST_STATUS_UNIT_TEST(UT, PASSED, MSG=MSG)
    !
    CALL ASSSERT_KILL(UT, PASSED, KILL)
    !
  END SUBROUTINE
  !
  SUBROUTINE ASSERT_UNIT_TEST_0D_INT32(UT, A, OP, B, MSG, KILL)
    CLASS(UNIT_TESTS),      intent(inout):: UT
    CHARACTER(2),           intent(in   ):: OP
    INTEGER(INT32),         intent(in   ):: A, B
    CHARACTER(*), optional, intent(in   ):: MSG
    LOGICAL,      optional, intent(in   ):: KILL
    LOGICAL:: PASSED
    !
    SELECT CASE(OP)
    CASE('EQ', 'eq', 'Eq', 'eQ');  PASSED = A .EQ. B
    CASE('LT', 'lt', 'Lt', 'lT');  PASSED = A .LT. B
    CASE('LE', 'le', 'Le', 'lE');  PASSED = A .LE. B
    CASE('GT', 'gt', 'Gt', 'gT');  PASSED = A .GT. B
    CASE('GE', 'ge', 'Ge', 'gE');  PASSED = A .GE. B
    CASE('NE', 'ne', 'Ne', 'nE');  PASSED = A .NE. B
    CASE DEFAULT
        ERROR STOP 'ASSERT_UNIT_TEST_0D_INT32(UT, A, OP, B, MSG, KILL) - Unknown "OP" received'
    END SELECT
    CALL TEST_STATUS_UNIT_TEST(UT, PASSED, MSG=MSG)
    !
    CALL ASSSERT_KILL(UT, PASSED, KILL)
    !
  END SUBROUTINE
  !
  ! ---------------------------------------------------------------------------------------------
  !
  SUBROUTINE ASSERT_UNIT_TEST_0D_INT64(UT, A, OP, B, MSG, KILL)
    CLASS(UNIT_TESTS),      intent(inout):: UT
    CHARACTER(2),           intent(in   ):: OP
    INTEGER(INT64),         intent(in   ):: A, B
    CHARACTER(*), optional, intent(in   ):: MSG
    LOGICAL,      optional, intent(in   ):: KILL
    LOGICAL:: PASSED
    !
    SELECT CASE(OP)
    CASE('EQ', 'eq', 'Eq', 'eQ');  PASSED = A .EQ. B
    CASE('LT', 'lt', 'Lt', 'lT');  PASSED = A .LT. B
    CASE('LE', 'le', 'Le', 'lE');  PASSED = A .LE. B
    CASE('GT', 'gt', 'Gt', 'gT');  PASSED = A .GT. B
    CASE('GE', 'ge', 'Ge', 'gE');  PASSED = A .GE. B
    CASE('NE', 'ne', 'Ne', 'nE');  PASSED = A .NE. B
    CASE DEFAULT
        ERROR STOP 'ASSERT_UNIT_TEST_0D_INT64(UT, A, OP, B, MSG, KILL) - Unknown "OP" received'
    END SELECT
    CALL TEST_STATUS_UNIT_TEST(UT, PASSED, MSG=MSG)
    !
    CALL ASSSERT_KILL(UT, PASSED, KILL)
    !
  END SUBROUTINE
  !
  ! ---------------------------------------------------------------------------------------------
  !
  SUBROUTINE ASSERT_UNIT_TEST_0D_REL32(UT, A, OP, B, MSG, KILL, TOL)
    CLASS(UNIT_TESTS),      intent(inout):: UT
    CHARACTER(2),           intent(in   ):: OP
    REAL(rel32),            intent(in   ):: A, B
    CHARACTER(*), optional, intent(in   ):: MSG
    LOGICAL,      optional, intent(in   ):: KILL
    REAL(rel32),  optional, intent(in   ):: TOL
    LOGICAL:: PASSED
    !
    SELECT CASE(OP)
    CASE('EQ', 'eq', 'Eq', 'eQ');  PASSED = IS_CLOSE(A, B, TOL)
    CASE('LT', 'lt', 'Lt', 'lT');  PASSED = A .LT. B
    CASE('LE', 'le', 'Le', 'lE');  PASSED = A .LE. B
    CASE('GT', 'gt', 'Gt', 'gT');  PASSED = A .GT. B
    CASE('GE', 'ge', 'Ge', 'gE');  PASSED = A .GE. B
    CASE('NE', 'ne', 'Ne', 'nE');  PASSED = A .NE. B
    CASE DEFAULT
        ERROR STOP 'ASSERT_UNIT_TEST_0D_REL32(UT, A, OP, B, MSG, KILL) - Unknown "OP" received'
    END SELECT
    CALL TEST_STATUS_UNIT_TEST(UT, PASSED, MSG=MSG)
    !
    CALL ASSSERT_KILL(UT, PASSED, KILL)
    !
  END SUBROUTINE
  !
  ! ---------------------------------------------------------------------------------------------
  !
  SUBROUTINE ASSERT_UNIT_TEST_0D_REL64(UT, A, OP, B, MSG, KILL, TOL)
    CLASS(UNIT_TESTS),      intent(inout):: UT
    CHARACTER(2),           intent(in   ):: OP
    REAL(rel64),            intent(in   ):: A, B
    CHARACTER(*), optional, intent(in   ):: MSG
    LOGICAL,      optional, intent(in   ):: KILL
    REAL(rel64),  optional, intent(in   ):: TOL
    LOGICAL:: PASSED
    !
    SELECT CASE(OP)
    CASE('EQ', 'eq', 'Eq', 'eQ');  PASSED = IS_CLOSE(A, B, TOL)
    CASE('LT', 'lt', 'Lt', 'lT');  PASSED = A .LT. B
    CASE('LE', 'le', 'Le', 'lE');  PASSED = A .LE. B
    CASE('GT', 'gt', 'Gt', 'gT');  PASSED = A .GT. B
    CASE('GE', 'ge', 'Ge', 'gE');  PASSED = A .GE. B
    CASE('NE', 'ne', 'Ne', 'nE');  PASSED = A .NE. B
    CASE DEFAULT
        ERROR STOP 'ASSERT_UNIT_TEST_0D_REL64(UT, A, OP, B, MSG, KILL) - Unknown "OP" received'
    END SELECT
    CALL TEST_STATUS_UNIT_TEST(UT, PASSED, MSG=MSG)
    !
    CALL ASSSERT_KILL(UT, PASSED, KILL)
    !
  END SUBROUTINE
  !
  ! ---------------------------------------------------------------------------------------------
  !
  SUBROUTINE ASSERT_UNIT_TEST_0D_CHAR(UT, A, OP, B, MSG, KILL, CAP)
    CLASS(UNIT_TESTS),      intent(inout):: UT
    CHARACTER(2),           intent(in   ):: OP
    CHARACTER(*),           intent(in   ):: A, B
    CHARACTER(*), optional, intent(in   ):: MSG
    LOGICAL,      optional, intent(in   ):: KILL
    LOGICAL,      optional, intent(in   ):: CAP
    CHARACTER(LEN(A)):: At
    CHARACTER(LEN(B)):: Bt
    LOGICAL:: NO_CASE
    !
    NO_CASE = .TRUE.
    if(present(CAP)) NO_CASE = CAP
    !
    At = A
    Bt = B
    if(NO_CASE) then
                call upper(At)
                call upper(Bt)
    end if
    !
    CALL ASSERT_UNIT_TEST_CHAR(UT, A, OP, B, MSG, KILL)
    !
  END SUBROUTINE
  !
  ! ---------------------------------------------------------------------------------------------
  !
  SUBROUTINE ASSERT_UNIT_TEST_CHAR(UT, A, OP, B, MSG, KILL)
    CLASS(UNIT_TESTS),      intent(inout):: UT
    CHARACTER(2),           intent(in   ):: OP
    CHARACTER(*),           intent(in   ):: A, B
    CHARACTER(*), optional, intent(in   ):: MSG
    LOGICAL,      optional, intent(in   ):: KILL
    LOGICAL:: PASSED
    !
    SELECT CASE(OP)
    CASE('EQ', 'eq', 'Eq', 'eQ');  PASSED = A .EQ. B
    CASE('LT', 'lt', 'Lt', 'lT');  PASSED = A .LT. B
    CASE('LE', 'le', 'Le', 'lE');  PASSED = A .LE. B
    CASE('GT', 'gt', 'Gt', 'gT');  PASSED = A .GT. B
    CASE('GE', 'ge', 'Ge', 'gE');  PASSED = A .GE. B
    CASE('NE', 'ne', 'Ne', 'nE');  PASSED = A .NE. B
    CASE DEFAULT
        ERROR STOP 'ASSERT_UNIT_TEST_0D_INT32(UT, A, OP, B, MSG, KILL) - Unknown "OP" received'
    END SELECT
    CALL TEST_STATUS_UNIT_TEST(UT, PASSED, MSG=MSG)
    !
    CALL ASSSERT_KILL(UT, PASSED, KILL)
    !
  END SUBROUTINE
  !
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !
  SUBROUTINE ASSERT_UNIT_TEST_1D_TF(UT, TEST, WANT, MSG, KILL)
    CLASS(UNIT_TESTS),              intent(inout):: UT
    LOGICAL,          dimension(:), intent(in   ):: TEST
    LOGICAL,optional, dimension(:), intent(in   ):: WANT
    CHARACTER(*),         optional, intent(in   ):: MSG
    LOGICAL,              optional, intent(in   ):: KILL
    LOGICAL:: PASSED
    INTEGER:: I, dim1, dim2
    !
    IF(present(WANT)) THEN
        dim1 = size(TEST)
        dim2 = size(WANT)
        IF( dim1 /= dim2 ) THEN
            PASSED = FALSE
        ELSE
            I=0;     PASSED = TRUE
            DO WHILE (PASSED .and. I < dim1)
                I = I + 1
                PASSED = TEST(I) .eqv. WANT(I)
            END DO
        END IF
    ELSE
        PASSED = ALL(TEST)
    END IF
    CALL TEST_STATUS_UNIT_TEST(UT, PASSED, MSG=MSG)
    !
    CALL ASSSERT_KILL(UT, PASSED, KILL)
    !
  END SUBROUTINE
  !
  ! ---------------------------------------------------------------------------------------------
  !
  SUBROUTINE ASSERT_UNIT_TEST_1D_TFOP(UT, A, OP, B, MSG, KILL)
    CLASS(UNIT_TESTS),            intent(inout):: UT
    CHARACTER(2),                 intent(in   ):: OP
    LOGICAL,        dimension(:), intent(in   ):: A, B
    CHARACTER(*),       optional, intent(in   ):: MSG
    LOGICAL,            optional, intent(in   ):: KILL
    LOGICAL:: PASSED
    INTEGER:: I, dim1, dim2
    !
    dim1 = size(A)
    dim2 = size(B)
    !
    IF( dim1 /= dim2 ) THEN
        PASSED = FALSE
    ELSE
       SELECT CASE(OP)
       CASE('EQ', 'eq', 'Eq', 'eQ')                                     ! These loops are intentially obtuse
                                    I=0;     PASSED = TRUE              !   to disable optimizations
                                    DO WHILE (PASSED .and. I < dim1)
                                        I = I + 1
                                        PASSED = A(I) .eqv. B(I)
                                    END DO
       CASE('LT', 'lt', 'Lt', 'lT')
                                    I=0;     PASSED = TRUE
                                    DO WHILE (PASSED .and. I < dim1)
                                        I = I + 1
                                        PASSED = A(I) .neqv. B(I)
                                    END DO
       CASE('LE', 'le', 'Le', 'lE')
                                    I=0;     PASSED = TRUE
                                    DO WHILE (PASSED .and. I < dim1)
                                        I = I + 1
                                        PASSED = A(I) .eqv. B(I)
                                    END DO
       CASE('GT', 'gt', 'Gt', 'gT')
                                    I=0;     PASSED = TRUE
                                    DO WHILE (PASSED .and. I < dim1)
                                        I = I + 1
                                        PASSED = A(I) .neqv. B(I)
                                    END DO
       CASE('GE', 'ge', 'Ge', 'gE')
                                    I=0;     PASSED = TRUE
                                    DO WHILE (PASSED .and. I < dim1)
                                        I = I + 1
                                        PASSED = A(I) .eqv. B(I)
                                    END DO
       CASE('NE', 'ne', 'Ne', 'nE')
                                    I=0;     PASSED = TRUE
                                    DO WHILE (PASSED .and. I < dim1)
                                        I = I + 1
                                        PASSED = A(I) .neqv. B(I)
                                    END DO
       CASE DEFAULT
           ERROR STOP 'ASSERT_UNIT_TEST_1D_TFOP(UT, A, OP, B, MSG, KILL) - Unknown "OP" received'
       END SELECT
    END IF
    CALL TEST_STATUS_UNIT_TEST(UT, PASSED, MSG=MSG)
    !
    CALL ASSSERT_KILL(UT, PASSED, KILL)
    !
  END SUBROUTINE
  !
  ! ---------------------------------------------------------------------------------------------
  !
  SUBROUTINE ASSERT_UNIT_TEST_1D_INT8(UT, A, OP, B, MSG, KILL)
    CLASS(UNIT_TESTS),           intent(inout):: UT
    CHARACTER(2),                intent(in   ):: OP
    INTEGER(INT8), dimension(:), intent(in   ):: A, B
    CHARACTER(*),      optional, intent(in   ):: MSG
    LOGICAL,           optional, intent(in   ):: KILL
    LOGICAL:: PASSED
    INTEGER:: I, dim1, dim2
    !
    dim1 = size(A)
    dim2 = size(B)
    !
    IF( dim1 /= dim2 ) THEN
        PASSED = FALSE
    ELSE
       SELECT CASE(OP)
       CASE('EQ', 'eq', 'Eq', 'eQ')                                     ! These loops are intentially obtuse
                                    I=0;     PASSED = TRUE              !   to disable optimizations
                                    DO WHILE (PASSED .and. I < dim1)
                                        I = I + 1
                                        PASSED = A(I) .EQ. B(I)
                                    END DO
       CASE('LT', 'lt', 'Lt', 'lT')
                                    I=0;     PASSED = TRUE
                                    DO WHILE (PASSED .and. I < dim1)
                                        I = I + 1
                                        PASSED = A(I) .LT. B(I)
                                    END DO
       CASE('LE', 'le', 'Le', 'lE')
                                    I=0;     PASSED = TRUE
                                    DO WHILE (PASSED .and. I < dim1)
                                        I = I + 1
                                        PASSED = A(I) .LE. B(I)
                                    END DO
       CASE('GT', 'gt', 'Gt', 'gT')
                                    I=0;     PASSED = TRUE
                                    DO WHILE (PASSED .and. I < dim1)
                                        I = I + 1
                                        PASSED = A(I) .GT. B(I)
                                    END DO
       CASE('GE', 'ge', 'Ge', 'gE')
                                    I=0;     PASSED = TRUE
                                    DO WHILE (PASSED .and. I < dim1)
                                        I = I + 1
                                        PASSED = A(I) .GE. B(I)
                                    END DO
       CASE('NE', 'ne', 'Ne', 'nE')
                                    I=0;     PASSED = TRUE
                                    DO WHILE (PASSED .and. I < dim1)
                                        I = I + 1
                                        PASSED = A(I) .NE. B(I)
                                    END DO
       CASE DEFAULT
           ERROR STOP 'ASSERT_UNIT_TEST_1D_INT8(UT, A, OP, B, MSG, KILL) - Unknown "OP" received'
       END SELECT
    END IF
    CALL TEST_STATUS_UNIT_TEST(UT, PASSED, MSG=MSG)
    !
    CALL ASSSERT_KILL(UT, PASSED, KILL)
    !
  END SUBROUTINE
  !
  ! ---------------------------------------------------------------------------------------------
  !
  SUBROUTINE ASSERT_UNIT_TEST_1D_INT16(UT, A, OP, B, MSG, KILL)
    CLASS(UNIT_TESTS),            intent(inout):: UT
    CHARACTER(2),                 intent(in   ):: OP
    INTEGER(INT16), dimension(:), intent(in   ):: A, B
    CHARACTER(*),       optional, intent(in   ):: MSG
    LOGICAL,            optional, intent(in   ):: KILL
    LOGICAL:: PASSED
    INTEGER:: I, dim1, dim2
    !
    dim1 = size(A)
    dim2 = size(B)
    !
    IF( dim1 /= dim2 ) THEN
        PASSED = FALSE
    ELSE
       SELECT CASE(OP)
       CASE('EQ', 'eq', 'Eq', 'eQ')                                     ! These loops are intentially obtuse
                                    I=0;     PASSED = TRUE              !   to disable optimizations
                                    DO WHILE (PASSED .and. I < dim1)
                                        I = I + 1
                                        PASSED = A(I) .EQ. B(I)
                                    END DO
       CASE('LT', 'lt', 'Lt', 'lT')
                                    I=0;     PASSED = TRUE
                                    DO WHILE (PASSED .and. I < dim1)
                                        I = I + 1
                                        PASSED = A(I) .LT. B(I)
                                    END DO
       CASE('LE', 'le', 'Le', 'lE')
                                    I=0;     PASSED = TRUE
                                    DO WHILE (PASSED .and. I < dim1)
                                        I = I + 1
                                        PASSED = A(I) .LE. B(I)
                                    END DO
       CASE('GT', 'gt', 'Gt', 'gT')
                                    I=0;     PASSED = TRUE
                                    DO WHILE (PASSED .and. I < dim1)
                                        I = I + 1
                                        PASSED = A(I) .GT. B(I)
                                    END DO
       CASE('GE', 'ge', 'Ge', 'gE')
                                    I=0;     PASSED = TRUE
                                    DO WHILE (PASSED .and. I < dim1)
                                        I = I + 1
                                        PASSED = A(I) .GE. B(I)
                                    END DO
       CASE('NE', 'ne', 'Ne', 'nE')
                                    I=0;     PASSED = TRUE
                                    DO WHILE (PASSED .and. I < dim1)
                                        I = I + 1
                                        PASSED = A(I) .NE. B(I)
                                    END DO
       CASE DEFAULT
           ERROR STOP 'ASSERT_UNIT_TEST_1D_INT16(UT, A, OP, B, MSG, KILL) - Unknown "OP" received'
       END SELECT
    END IF
    CALL TEST_STATUS_UNIT_TEST(UT, PASSED, MSG=MSG)
    !
    CALL ASSSERT_KILL(UT, PASSED, KILL)
    !
  END SUBROUTINE
  !
  ! ---------------------------------------------------------------------------------------------
  !
  SUBROUTINE ASSERT_UNIT_TEST_1D_INT32(UT, A, OP, B, MSG, KILL)
    CLASS(UNIT_TESTS),            intent(inout):: UT
    CHARACTER(2),                 intent(in   ):: OP
    INTEGER(INT32), dimension(:), intent(in   ):: A, B
    CHARACTER(*),       optional, intent(in   ):: MSG
    LOGICAL,            optional, intent(in   ):: KILL
    LOGICAL:: PASSED
    INTEGER:: I, dim1, dim2
    !
    dim1 = size(A)
    dim2 = size(B)
    !
    IF( dim1 /= dim2 ) THEN
        PASSED = FALSE
    ELSE
       SELECT CASE(OP)
       CASE('EQ', 'eq', 'Eq', 'eQ')                                     ! These loops are intentially obtuse
                                    I=0;     PASSED = TRUE              !   to disable optimizations
                                    DO WHILE (PASSED .and. I < dim1)
                                        I = I + 1
                                        PASSED = A(I) .EQ. B(I)
                                    END DO
       CASE('LT', 'lt', 'Lt', 'lT')
                                    I=0;     PASSED = TRUE
                                    DO WHILE (PASSED .and. I < dim1)
                                        I = I + 1
                                        PASSED = A(I) .LT. B(I)
                                    END DO
       CASE('LE', 'le', 'Le', 'lE')
                                    I=0;     PASSED = TRUE
                                    DO WHILE (PASSED .and. I < dim1)
                                        I = I + 1
                                        PASSED = A(I) .LE. B(I)
                                    END DO
       CASE('GT', 'gt', 'Gt', 'gT')
                                    I=0;     PASSED = TRUE
                                    DO WHILE (PASSED .and. I < dim1)
                                        I = I + 1
                                        PASSED = A(I) .GT. B(I)
                                    END DO
       CASE('GE', 'ge', 'Ge', 'gE')
                                    I=0;     PASSED = TRUE
                                    DO WHILE (PASSED .and. I < dim1)
                                        I = I + 1
                                        PASSED = A(I) .GE. B(I)
                                    END DO
       CASE('NE', 'ne', 'Ne', 'nE')
                                    I=0;     PASSED = TRUE
                                    DO WHILE (PASSED .and. I < dim1)
                                        I = I + 1
                                        PASSED = A(I) .NE. B(I)
                                    END DO
       CASE DEFAULT
           ERROR STOP 'ASSERT_UNIT_TEST_1D_INT32(UT, A, OP, B, MSG, KILL) - Unknown "OP" received'
       END SELECT
    END IF
    CALL TEST_STATUS_UNIT_TEST(UT, PASSED, MSG=MSG)
    !
    CALL ASSSERT_KILL(UT, PASSED, KILL)
    !
  END SUBROUTINE
  !
  ! ---------------------------------------------------------------------------------------------
  !
  SUBROUTINE ASSERT_UNIT_TEST_1D_INT64(UT, A, OP, B, MSG, KILL)
    CLASS(UNIT_TESTS),            intent(inout):: UT
    CHARACTER(2),                 intent(in   ):: OP
    INTEGER(INT64), dimension(:), intent(in   ):: A, B
    CHARACTER(*),       optional, intent(in   ):: MSG
    LOGICAL,            optional, intent(in   ):: KILL
    LOGICAL:: PASSED
    INTEGER:: I, dim1, dim2
    !
    dim1 = size(A)
    dim2 = size(B)
    !
    IF( dim1 /= dim2 ) THEN
        PASSED = FALSE
    ELSE
       SELECT CASE(OP)
       CASE('EQ', 'eq', 'Eq', 'eQ')                                     ! These loops are intentially obtuse
                                    I=0;     PASSED = TRUE              !   to disable optimizations
                                    DO WHILE (PASSED .and. I < dim1)
                                        I = I + 1
                                        PASSED = A(I) .EQ. B(I)
                                    END DO
       CASE('LT', 'lt', 'Lt', 'lT')
                                    I=0;     PASSED = TRUE
                                    DO WHILE (PASSED .and. I < dim1)
                                        I = I + 1
                                        PASSED = A(I) .LT. B(I)
                                    END DO
       CASE('LE', 'le', 'Le', 'lE')
                                    I=0;     PASSED = TRUE
                                    DO WHILE (PASSED .and. I < dim1)
                                        I = I + 1
                                        PASSED = A(I) .LE. B(I)
                                    END DO
       CASE('GT', 'gt', 'Gt', 'gT')
                                    I=0;     PASSED = TRUE
                                    DO WHILE (PASSED .and. I < dim1)
                                        I = I + 1
                                        PASSED = A(I) .GT. B(I)
                                    END DO
       CASE('GE', 'ge', 'Ge', 'gE')
                                    I=0;     PASSED = TRUE
                                    DO WHILE (PASSED .and. I < dim1)
                                        I = I + 1
                                        PASSED = A(I) .GE. B(I)
                                    END DO
       CASE('NE', 'ne', 'Ne', 'nE')
                                    I=0;     PASSED = TRUE
                                    DO WHILE (PASSED .and. I < dim1)
                                        I = I + 1
                                        PASSED = A(I) .NE. B(I)
                                    END DO
       CASE DEFAULT
           ERROR STOP 'ASSERT_UNIT_TEST_1D_INT64(UT, A, OP, B, MSG, KILL) - Unknown "OP" received'
       END SELECT
    END IF
    CALL TEST_STATUS_UNIT_TEST(UT, PASSED, MSG=MSG)
    !
    CALL ASSSERT_KILL(UT, PASSED, KILL)
    !
  END SUBROUTINE
  !
  ! ---------------------------------------------------------------------------------------------
  !
  SUBROUTINE ASSERT_UNIT_TEST_1D_REL32(UT, A, OP, B, MSG, KILL)
    CLASS(UNIT_TESTS),         intent(inout):: UT
    CHARACTER(2),              intent(in   ):: OP
    REAL(rel32), dimension(:), intent(in   ):: A, B
    CHARACTER(*),    optional, intent(in   ):: MSG
    LOGICAL,         optional, intent(in   ):: KILL
    LOGICAL:: PASSED
    INTEGER:: I, dim1, dim2
    !
    dim1 = size(A)
    dim2 = size(B)
    !
    IF( dim1 /= dim2 ) THEN
        PASSED = FALSE
    ELSE
       SELECT CASE(OP)
       CASE('EQ', 'eq', 'Eq', 'eQ')                                     ! These loops are intentially obtuse
                                    I=0;     PASSED = TRUE              !   to disable optimizations
                                    DO WHILE (PASSED .and. I < dim1)
                                        I = I + 1
                                        PASSED = A(I) .EQ. B(I)
                                    END DO
       CASE('LT', 'lt', 'Lt', 'lT')
                                    I=0;     PASSED = TRUE
                                    DO WHILE (PASSED .and. I < dim1)
                                        I = I + 1
                                        PASSED = A(I) .LT. B(I)
                                    END DO
       CASE('LE', 'le', 'Le', 'lE')
                                    I=0;     PASSED = TRUE
                                    DO WHILE (PASSED .and. I < dim1)
                                        I = I + 1
                                        PASSED = A(I) .LE. B(I)
                                    END DO
       CASE('GT', 'gt', 'Gt', 'gT')
                                    I=0;     PASSED = TRUE
                                    DO WHILE (PASSED .and. I < dim1)
                                        I = I + 1
                                        PASSED = A(I) .GT. B(I)
                                    END DO
       CASE('GE', 'ge', 'Ge', 'gE')
                                    I=0;     PASSED = TRUE
                                    DO WHILE (PASSED .and. I < dim1)
                                        I = I + 1
                                        PASSED = A(I) .GE. B(I)
                                    END DO
       CASE('NE', 'ne', 'Ne', 'nE')
                                    I=0;     PASSED = TRUE
                                    DO WHILE (PASSED .and. I < dim1)
                                        I = I + 1
                                        PASSED = A(I) .NE. B(I)
                                    END DO
       CASE DEFAULT
           ERROR STOP 'ASSERT_UNIT_TEST_1D_REL32(UT, A, OP, B, MSG, KILL) - Unknown "OP" received'
       END SELECT
    END IF
    CALL TEST_STATUS_UNIT_TEST(UT, PASSED, MSG=MSG)
    !
    CALL ASSSERT_KILL(UT, PASSED, KILL)
    !
  END SUBROUTINE
  !
  ! ---------------------------------------------------------------------------------------------
  !
  SUBROUTINE ASSERT_UNIT_TEST_1D_REL64(UT, A, OP, B, MSG, KILL, TOL)
    CLASS(UNIT_TESTS),         intent(inout):: UT
    CHARACTER(2),              intent(in   ):: OP
    REAL(rel64), dimension(:), intent(in   ):: A, B
    CHARACTER(*),    optional, intent(in   ):: MSG
    LOGICAL,         optional, intent(in   ):: KILL
    REAL(rel64),     optional, intent(in   ):: TOL
    LOGICAL:: PASSED
    INTEGER:: I, dim1, dim2
    !
    dim1 = size(A)
    dim2 = size(B)
    !
    IF( dim1 /= dim2 ) THEN
        PASSED = FALSE
    ELSE
       SELECT CASE(OP)
       CASE('EQ', 'eq', 'Eq', 'eQ')                                     ! These loops are intentially obtuse
                                    I=0;     PASSED = TRUE              !   to disable optimizations
                                    DO WHILE (PASSED .and. I < dim1)
                                        I = I + 1
                                        PASSED = IS_CLOSE(A(I), B(I), TOL)
                                    END DO
       CASE('LT', 'lt', 'Lt', 'lT')
                                    I=0;     PASSED = TRUE
                                    DO WHILE (PASSED .and. I < dim1)
                                        I = I + 1
                                        PASSED = A(I) .LT. B(I)
                                    END DO
       CASE('LE', 'le', 'Le', 'lE')
                                    I=0;     PASSED = TRUE
                                    DO WHILE (PASSED .and. I < dim1)
                                        I = I + 1
                                        PASSED = A(I) .LE. B(I)
                                    END DO
       CASE('GT', 'gt', 'Gt', 'gT')
                                    I=0;     PASSED = TRUE
                                    DO WHILE (PASSED .and. I < dim1)
                                        I = I + 1
                                        PASSED = A(I) .GT. B(I)
                                    END DO
       CASE('GE', 'ge', 'Ge', 'gE')
                                    I=0;     PASSED = TRUE
                                    DO WHILE (PASSED .and. I < dim1)
                                        I = I + 1
                                        PASSED = A(I) .GE. B(I)
                                    END DO
       CASE('NE', 'ne', 'Ne', 'nE')
                                    I=0;     PASSED = TRUE
                                    DO WHILE (PASSED .and. I < dim1)
                                        I = I + 1
                                        PASSED = A(I) .NE. B(I)
                                    END DO
       CASE DEFAULT
           ERROR STOP 'ASSERT_UNIT_TEST_1D_REL64(UT, A, OP, B, MSG, KILL) - Unknown "OP" received'
       END SELECT
    END IF
    CALL TEST_STATUS_UNIT_TEST(UT, PASSED, MSG=MSG)
    !
    CALL ASSSERT_KILL(UT, PASSED, KILL)
    !
  END SUBROUTINE
  !
  ! ---------------------------------------------------------------------------------------------
  !
  SUBROUTINE ASSERT_UNIT_TEST_1D_CHAR(UT, A, OP, B, MSG, KILL, CAP)
    CLASS(UNIT_TESTS),          intent(inout):: UT
    CHARACTER(2),               intent(in   ):: OP
    CHARACTER(*), dimension(:), intent(in   ):: A, B
    CHARACTER(*),     optional, intent(in   ):: MSG
    LOGICAL,          optional, intent(in   ):: KILL
    LOGICAL,          optional, intent(in   ):: CAP
    CHARACTER(len(A)):: At
    CHARACTER(len(B)):: Bt
    LOGICAL:: PASSED, NO_CASE
    INTEGER:: I, dim1, dim2
    !
    NO_CASE = .TRUE.
    if(present(CAP)) NO_CASE = CAP
    !
    dim1 = size(A)
    dim2 = size(B)
    !
    IF( dim1 /= dim2 ) THEN
        PASSED = FALSE
    ELSE
       SELECT CASE(OP)
       CASE('EQ', 'eq', 'Eq', 'eQ')                                     ! These loops are intentially obtuse
                                    I=0;     PASSED = TRUE              !   to disable optimizations
                                    DO WHILE (PASSED .and. I < dim1)
                                        I = I + 1
                                        At = A(i)
                                        Bt = B(i)
                                        if(NO_CASE) then
                                                call upper(At)
                                                call upper(Bt)
                                        end if
                                        PASSED = At .EQ. Bt
                                    END DO
       CASE('LT', 'lt', 'Lt', 'lT')
                                    I=0;     PASSED = TRUE
                                    DO WHILE (PASSED .and. I < dim1)
                                        I = I + 1
                                        At = A(i)
                                        Bt = B(i)
                                        if(NO_CASE) then
                                                call upper(At)
                                                call upper(Bt)
                                        end if
                                        PASSED = At .LT. Bt
                                    END DO
       CASE('LE', 'le', 'Le', 'lE')
                                    I=0;     PASSED = TRUE
                                    DO WHILE (PASSED .and. I < dim1)
                                        I = I + 1
                                        At = A(i)
                                        Bt = B(i)
                                        if(NO_CASE) then
                                                call upper(At)
                                                call upper(Bt)
                                        end if
                                        PASSED = At .LE. Bt
                                    END DO
       CASE('GT', 'gt', 'Gt', 'gT')
                                    I=0;     PASSED = TRUE
                                    DO WHILE (PASSED .and. I < dim1)
                                        I = I + 1
                                        At = A(i)
                                        Bt = B(i)
                                        if(NO_CASE) then
                                                call upper(At)
                                                call upper(Bt)
                                        end if
                                        PASSED = At .GT. Bt
                                    END DO
       CASE('GE', 'ge', 'Ge', 'gE')
                                    I=0;     PASSED = TRUE
                                    DO WHILE (PASSED .and. I < dim1)
                                        I = I + 1
                                        At = A(i)
                                        Bt = B(i)
                                        if(NO_CASE) then
                                                call upper(At)
                                                call upper(Bt)
                                        end if
                                        PASSED = At .GE. Bt
                                    END DO
       CASE('NE', 'ne', 'Ne', 'nE')
                                    I=0;     PASSED = TRUE
                                    DO WHILE (PASSED .and. I < dim1)
                                        I = I + 1
                                        At = A(i)
                                        Bt = B(i)
                                        if(NO_CASE) then
                                                call upper(At)
                                                call upper(Bt)
                                        end if
                                        PASSED = At .NE. Bt
                                    END DO
       CASE DEFAULT
           ERROR STOP 'ASSERT_UNIT_TEST_1D_REL64(UT, A, OP, B, MSG, KILL) - Unknown "OP" received'
       END SELECT
    END IF
    CALL TEST_STATUS_UNIT_TEST(UT, PASSED, MSG=MSG)
    !
    CALL ASSSERT_KILL(UT, PASSED, KILL)
    !
  END SUBROUTINE
  !
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !
  SUBROUTINE ASSERT_UNIT_TEST_2D_TF(UT, TEST, WANT, MSG, KILL)
    CLASS(UNIT_TESTS),                intent(inout):: UT
    LOGICAL,          dimension(:,:), intent(in   ):: TEST
    LOGICAL,optional, dimension(:,:), intent(in   ):: WANT
    CHARACTER(*),           optional, intent(in   ):: MSG
    LOGICAL,                optional, intent(in   ):: KILL
    LOGICAL:: PASSED
    INTEGER:: I, J, dim1a, dim2a, dim1b, dim2b
    !
    dim1a = size(TEST, dim=1)
    dim2a = size(TEST, dim=2)
    !
    IF(present(WANT)) THEN
       !
       dim1b = size(WANT, dim=1)
       dim2b = size(WANT, dim=2)
       !
       IF( dim1a /= dim1b .or. dim2a /= dim2b ) THEN
           PASSED = FALSE
       ELSE
            J=0;    PASSED = TRUE
            DO WHILE (PASSED .and. J < dim2a)
               J = J + 1
               I = 0
               DO WHILE (PASSED .and. I < dim1a)
                   I = I + 1
                   PASSED = TEST(I,J) .eqv. WANT(I,J)
               END DO
            END DO
       END IF
    ELSE
            J=0;    PASSED = TRUE
            DO WHILE (PASSED .and. J < dim2a)
               J = J + 1
               I = 0
               DO WHILE (PASSED .and. I < dim1a)
                   I = I + 1
                   PASSED = TEST(I,J)
               END DO
            END DO
    END IF
    CALL TEST_STATUS_UNIT_TEST(UT, PASSED, MSG=MSG)
    !
    CALL ASSSERT_KILL(UT, PASSED, KILL)
    !
  END SUBROUTINE
  !
  ! ---------------------------------------------------------------------------------------------
  !
  SUBROUTINE ASSERT_UNIT_TEST_2D_TFOP(UT, A, OP, B, MSG, KILL)
    CLASS(UNIT_TESTS),             intent(inout):: UT
    CHARACTER(2),                  intent(in   ):: OP
    LOGICAL,       dimension(:,:), intent(in   ):: A, B
    CHARACTER(*),        optional, intent(in   ):: MSG
    LOGICAL,             optional, intent(in   ):: KILL
    LOGICAL:: PASSED
    INTEGER:: I, J, dim1a, dim2a, dim1b, dim2b
    !
    dim1a = size(A, dim=1)
    dim2a = size(A, dim=2)
    !
    dim1b = size(B, dim=1)
    dim2b = size(B, dim=2)
    !
    IF( dim1a /= dim1b .or. dim2a /= dim2b ) THEN
        PASSED = FALSE
    ELSE
       SELECT CASE(OP)
       CASE('EQ', 'eq', 'Eq', 'eQ')                                     ! These loops are intentially obtuse
                                    J=0;    PASSED = TRUE               !   to disable optimizations
                                    DO WHILE (PASSED .and. J < dim2a)
                                       J = J + 1
                                       I = 0
                                       DO WHILE (PASSED .and. I < dim1a)
                                           I = I + 1
                                           PASSED = A(I,J) .eqv. B(I,J)
                                       END DO
                                    END DO
       CASE('LT', 'lt', 'Lt', 'lT')
                                    J=0;    PASSED = TRUE
                                    DO WHILE (PASSED .and. J < dim2a)
                                       J = J + 1
                                       I = 0
                                       DO WHILE (PASSED .and. I < dim1a)
                                           I = I + 1
                                           PASSED = A(I,J) .neqv. B(I,J)
                                       END DO
                                    END DO
       CASE('LE', 'le', 'Le', 'lE')
                                    J=0;    PASSED = TRUE
                                    DO WHILE (PASSED .and. J < dim2a)
                                       J = J + 1
                                       I = 0
                                       DO WHILE (PASSED .and. I < dim1a)
                                           I = I + 1
                                           PASSED = A(I,J) .eqv. B(I,J)
                                       END DO
                                    END DO
       CASE('GT', 'gt', 'Gt', 'gT')
                                    J=0;    PASSED = TRUE
                                    DO WHILE (PASSED .and. J < dim2a)
                                       J = J + 1
                                       I = 0
                                       DO WHILE (PASSED .and. I < dim1a)
                                           I = I + 1
                                           PASSED = A(I,J) .neqv. B(I,J)
                                       END DO
                                    END DO
       CASE('GE', 'ge', 'Ge', 'gE')
                                    J=0;    PASSED = TRUE
                                    DO WHILE (PASSED .and. J < dim2a)
                                       J = J + 1
                                       I = 0
                                       DO WHILE (PASSED .and. I < dim1a)
                                           I = I + 1
                                           PASSED = A(I,J) .eqv. B(I,J)
                                       END DO
                                    END DO
       CASE('NE', 'ne', 'Ne', 'nE')
                                    J=0;    PASSED = TRUE
                                    DO WHILE (PASSED .and. J < dim2a)
                                       J = J + 1
                                       I = 0
                                       DO WHILE (PASSED .and. I < dim1a)
                                           I = I + 1
                                           PASSED = A(I,J) .neqv. B(I,J)
                                       END DO
                                    END DO
       CASE DEFAULT
           ERROR STOP 'ASSERT_UNIT_TEST_2D_TFOP(UT, A, OP, B, MSG, KILL) - Unknown "OP" received'
       END SELECT
    END IF
    CALL TEST_STATUS_UNIT_TEST(UT, PASSED, MSG=MSG)
    !
    CALL ASSSERT_KILL(UT, PASSED, KILL)
    !
  END SUBROUTINE
  !
  ! ---------------------------------------------------------------------------------------------
  !
  SUBROUTINE ASSERT_UNIT_TEST_2D_INT8(UT, A, OP, B, MSG, KILL)
    CLASS(UNIT_TESTS),             intent(inout):: UT
    CHARACTER(2),                  intent(in   ):: OP
    INTEGER(INT8), dimension(:,:), intent(in   ):: A, B
    CHARACTER(*),        optional, intent(in   ):: MSG
    LOGICAL,             optional, intent(in   ):: KILL
    LOGICAL:: PASSED
    INTEGER:: I, J, dim1a, dim2a, dim1b, dim2b
    !
    dim1a = size(A, dim=1)
    dim2a = size(A, dim=2)
    !
    dim1b = size(B, dim=1)
    dim2b = size(B, dim=2)
    !
    IF( dim1a /= dim1b .or. dim2a /= dim2b ) THEN
        PASSED = FALSE
    ELSE
       SELECT CASE(OP)
       CASE('EQ', 'eq', 'Eq', 'eQ')                                     ! These loops are intentially obtuse
                                    J=0;    PASSED = TRUE               !   to disable optimizations
                                    DO WHILE (PASSED .and. J < dim2a)
                                       J = J + 1
                                       I = 0
                                       DO WHILE (PASSED .and. I < dim1a)
                                           I = I + 1
                                           PASSED = A(I,J) .EQ. B(I,J)
                                       END DO
                                    END DO
       CASE('LT', 'lt', 'Lt', 'lT')
                                    J=0;    PASSED = TRUE
                                    DO WHILE (PASSED .and. J < dim2a)
                                       J = J + 1
                                       I = 0
                                       DO WHILE (PASSED .and. I < dim1a)
                                           I = I + 1
                                           PASSED = A(I,J) .LT. B(I,J)
                                       END DO
                                    END DO
       CASE('LE', 'le', 'Le', 'lE')
                                    J=0;    PASSED = TRUE
                                    DO WHILE (PASSED .and. J < dim2a)
                                       J = J + 1
                                       I = 0
                                       DO WHILE (PASSED .and. I < dim1a)
                                           I = I + 1
                                           PASSED = A(I,J) .LE. B(I,J)
                                       END DO
                                    END DO
       CASE('GT', 'gt', 'Gt', 'gT')
                                    J=0;    PASSED = TRUE
                                    DO WHILE (PASSED .and. J < dim2a)
                                       J = J + 1
                                       I = 0
                                       DO WHILE (PASSED .and. I < dim1a)
                                           I = I + 1
                                           PASSED = A(I,J) .GT. B(I,J)
                                       END DO
                                    END DO
       CASE('GE', 'ge', 'Ge', 'gE')
                                    J=0;    PASSED = TRUE
                                    DO WHILE (PASSED .and. J < dim2a)
                                       J = J + 1
                                       I = 0
                                       DO WHILE (PASSED .and. I < dim1a)
                                           I = I + 1
                                           PASSED = A(I,J) .GE. B(I,J)
                                       END DO
                                    END DO
       CASE('NE', 'ne', 'Ne', 'nE')
                                    J=0;    PASSED = TRUE
                                    DO WHILE (PASSED .and. J < dim2a)
                                       J = J + 1
                                       I = 0
                                       DO WHILE (PASSED .and. I < dim1a)
                                           I = I + 1
                                           PASSED = A(I,J) .NE. B(I,J)
                                       END DO
                                    END DO
       CASE DEFAULT
           ERROR STOP 'ASSERT_UNIT_TEST_2D_INT8(UT, A, OP, B, MSG, KILL) - Unknown "OP" received'
       END SELECT
    END IF
    CALL TEST_STATUS_UNIT_TEST(UT, PASSED, MSG=MSG)
    !
    CALL ASSSERT_KILL(UT, PASSED, KILL)
    !
  END SUBROUTINE
  !
  ! ---------------------------------------------------------------------------------------------
  !
  SUBROUTINE ASSERT_UNIT_TEST_2D_INT16(UT, A, OP, B, MSG, KILL)
    CLASS(UNIT_TESTS),              intent(inout):: UT
    CHARACTER(2),                   intent(in   ):: OP
    INTEGER(INT16), dimension(:,:), intent(in   ):: A, B
    CHARACTER(*),         optional, intent(in   ):: MSG
    LOGICAL,              optional, intent(in   ):: KILL
    LOGICAL:: PASSED
    INTEGER:: I, J, dim1a, dim2a, dim1b, dim2b
    !
    dim1a = size(A, dim=1)
    dim2a = size(A, dim=2)
    !
    dim1b = size(B, dim=1)
    dim2b = size(B, dim=2)
    !
    IF( dim1a /= dim1b .or. dim2a /= dim2b ) THEN
        PASSED = FALSE
    ELSE
       SELECT CASE(OP)
       CASE('EQ', 'eq', 'Eq', 'eQ')                                     ! These loops are intentially obtuse
                                    J=0;    PASSED = TRUE               !   to disable optimizations
                                    DO WHILE (PASSED .and. J < dim2a)
                                       J = J + 1
                                       I = 0
                                       DO WHILE (PASSED .and. I < dim1a)
                                           I = I + 1
                                           PASSED = A(I,J) .EQ. B(I,J)
                                       END DO
                                    END DO
       CASE('LT', 'lt', 'Lt', 'lT')
                                    J=0;    PASSED = TRUE
                                    DO WHILE (PASSED .and. J < dim2a)
                                       J = J + 1
                                       I = 0
                                       DO WHILE (PASSED .and. I < dim1a)
                                           I = I + 1
                                           PASSED = A(I,J) .LT. B(I,J)
                                       END DO
                                    END DO
       CASE('LE', 'le', 'Le', 'lE')
                                    J=0;    PASSED = TRUE
                                    DO WHILE (PASSED .and. J < dim2a)
                                       J = J + 1
                                       I = 0
                                       DO WHILE (PASSED .and. I < dim1a)
                                           I = I + 1
                                           PASSED = A(I,J) .LE. B(I,J)
                                       END DO
                                    END DO
       CASE('GT', 'gt', 'Gt', 'gT')
                                    J=0;    PASSED = TRUE
                                    DO WHILE (PASSED .and. J < dim2a)
                                       J = J + 1
                                       I = 0
                                       DO WHILE (PASSED .and. I < dim1a)
                                           I = I + 1
                                           PASSED = A(I,J) .GT. B(I,J)
                                       END DO
                                    END DO
       CASE('GE', 'ge', 'Ge', 'gE')
                                    J=0;    PASSED = TRUE
                                    DO WHILE (PASSED .and. J < dim2a)
                                       J = J + 1
                                       I = 0
                                       DO WHILE (PASSED .and. I < dim1a)
                                           I = I + 1
                                           PASSED = A(I,J) .GE. B(I,J)
                                       END DO
                                    END DO
       CASE('NE', 'ne', 'Ne', 'nE')
                                    J=0;    PASSED = TRUE
                                    DO WHILE (PASSED .and. J < dim2a)
                                       J = J + 1
                                       I = 0
                                       DO WHILE (PASSED .and. I < dim1a)
                                           I = I + 1
                                           PASSED = A(I,J) .NE. B(I,J)
                                       END DO
                                    END DO
       CASE DEFAULT
           ERROR STOP 'ASSERT_UNIT_TEST_2D_INT16(UT, A, OP, B, MSG, KILL) - Unknown "OP" received'
       END SELECT
    END IF
    CALL TEST_STATUS_UNIT_TEST(UT, PASSED, MSG=MSG)
    !
    CALL ASSSERT_KILL(UT, PASSED, KILL)
    !
  END SUBROUTINE
  !
  ! ---------------------------------------------------------------------------------------------
  !
  SUBROUTINE ASSERT_UNIT_TEST_2D_INT32(UT, A, OP, B, MSG, KILL)
    CLASS(UNIT_TESTS),              intent(inout):: UT
    CHARACTER(2),                   intent(in   ):: OP
    INTEGER(INT32), dimension(:,:), intent(in   ):: A, B
    CHARACTER(*),         optional, intent(in   ):: MSG
    LOGICAL,              optional, intent(in   ):: KILL
    LOGICAL:: PASSED
    INTEGER:: I, J, dim1a, dim2a, dim1b, dim2b
    !
    dim1a = size(A, dim=1)
    dim2a = size(A, dim=2)
    !
    dim1b = size(B, dim=1)
    dim2b = size(B, dim=2)
    !
    IF( dim1a /= dim1b .or. dim2a /= dim2b ) THEN
        PASSED = FALSE
    ELSE
       SELECT CASE(OP)
       CASE('EQ', 'eq', 'Eq', 'eQ')                                     ! These loops are intentially obtuse
                                    J=0;    PASSED = TRUE               !   to disable optimizations
                                    DO WHILE (PASSED .and. J < dim2a)
                                       J = J + 1
                                       I = 0
                                       DO WHILE (PASSED .and. I < dim1a)
                                           I = I + 1
                                           PASSED = A(I,J) .EQ. B(I,J)
                                       END DO
                                    END DO
       CASE('LT', 'lt', 'Lt', 'lT')
                                    J=0;    PASSED = TRUE
                                    DO WHILE (PASSED .and. J < dim2a)
                                       J = J + 1
                                       I = 0
                                       DO WHILE (PASSED .and. I < dim1a)
                                           I = I + 1
                                           PASSED = A(I,J) .LT. B(I,J)
                                       END DO
                                    END DO
       CASE('LE', 'le', 'Le', 'lE')
                                    J=0;    PASSED = TRUE
                                    DO WHILE (PASSED .and. J < dim2a)
                                       J = J + 1
                                       I = 0
                                       DO WHILE (PASSED .and. I < dim1a)
                                           I = I + 1
                                           PASSED = A(I,J) .LE. B(I,J)
                                       END DO
                                    END DO
       CASE('GT', 'gt', 'Gt', 'gT')
                                    J=0;    PASSED = TRUE
                                    DO WHILE (PASSED .and. J < dim2a)
                                       J = J + 1
                                       I = 0
                                       DO WHILE (PASSED .and. I < dim1a)
                                           I = I + 1
                                           PASSED = A(I,J) .GT. B(I,J)
                                       END DO
                                    END DO
       CASE('GE', 'ge', 'Ge', 'gE')
                                    J=0;    PASSED = TRUE
                                    DO WHILE (PASSED .and. J < dim2a)
                                       J = J + 1
                                       I = 0
                                       DO WHILE (PASSED .and. I < dim1a)
                                           I = I + 1
                                           PASSED = A(I,J) .GE. B(I,J)
                                       END DO
                                    END DO
       CASE('NE', 'ne', 'Ne', 'nE')
                                    J=0;    PASSED = TRUE
                                    DO WHILE (PASSED .and. J < dim2a)
                                       J = J + 1
                                       I = 0
                                       DO WHILE (PASSED .and. I < dim1a)
                                           I = I + 1
                                           PASSED = A(I,J) .NE. B(I,J)
                                       END DO
                                    END DO
       CASE DEFAULT
           ERROR STOP 'ASSERT_UNIT_TEST_2D_INT32(UT, A, OP, B, MSG, KILL) - Unknown "OP" received'
       END SELECT
    END IF
    CALL TEST_STATUS_UNIT_TEST(UT, PASSED, MSG=MSG)
    !
    CALL ASSSERT_KILL(UT, PASSED, KILL)
    !
  END SUBROUTINE
  !
  ! ---------------------------------------------------------------------------------------------
  !
  SUBROUTINE ASSERT_UNIT_TEST_2D_INT64(UT, A, OP, B, MSG, KILL)
    CLASS(UNIT_TESTS),              intent(inout):: UT
    CHARACTER(2),                   intent(in   ):: OP
    INTEGER(INT64), dimension(:,:), intent(in   ):: A, B
    CHARACTER(*),         optional, intent(in   ):: MSG
    LOGICAL,              optional, intent(in   ):: KILL
    LOGICAL:: PASSED
    INTEGER:: I, J, dim1a, dim2a, dim1b, dim2b
    !
    dim1a = size(A, dim=1)
    dim2a = size(A, dim=2)
    !
    dim1b = size(B, dim=1)
    dim2b = size(B, dim=2)
    !
    IF( dim1a /= dim1b .or. dim2a /= dim2b ) THEN
        PASSED = FALSE
    ELSE
       SELECT CASE(OP)
       CASE('EQ', 'eq', 'Eq', 'eQ')                                     ! These loops are intentially obtuse
                                    J=0;    PASSED = TRUE               !   to disable optimizations
                                    DO WHILE (PASSED .and. J < dim2a)
                                       J = J + 1
                                       I = 0
                                       DO WHILE (PASSED .and. I < dim1a)
                                           I = I + 1
                                           PASSED = A(I,J) .EQ. B(I,J)
                                       END DO
                                    END DO
       CASE('LT', 'lt', 'Lt', 'lT')
                                    J=0;    PASSED = TRUE
                                    DO WHILE (PASSED .and. J < dim2a)
                                       J = J + 1
                                       I = 0
                                       DO WHILE (PASSED .and. I < dim1a)
                                           I = I + 1
                                           PASSED = A(I,J) .LT. B(I,J)
                                       END DO
                                    END DO
       CASE('LE', 'le', 'Le', 'lE')
                                    J=0;    PASSED = TRUE
                                    DO WHILE (PASSED .and. J < dim2a)
                                       J = J + 1
                                       I = 0
                                       DO WHILE (PASSED .and. I < dim1a)
                                           I = I + 1
                                           PASSED = A(I,J) .LE. B(I,J)
                                       END DO
                                    END DO
       CASE('GT', 'gt', 'Gt', 'gT')
                                    J=0;    PASSED = TRUE
                                    DO WHILE (PASSED .and. J < dim2a)
                                       J = J + 1
                                       I = 0
                                       DO WHILE (PASSED .and. I < dim1a)
                                           I = I + 1
                                           PASSED = A(I,J) .GT. B(I,J)
                                       END DO
                                    END DO
       CASE('GE', 'ge', 'Ge', 'gE')
                                    J=0;    PASSED = TRUE
                                    DO WHILE (PASSED .and. J < dim2a)
                                       J = J + 1
                                       I = 0
                                       DO WHILE (PASSED .and. I < dim1a)
                                           I = I + 1
                                           PASSED = A(I,J) .GE. B(I,J)
                                       END DO
                                    END DO
       CASE('NE', 'ne', 'Ne', 'nE')
                                    J=0;    PASSED = TRUE
                                    DO WHILE (PASSED .and. J < dim2a)
                                       J = J + 1
                                       I = 0
                                       DO WHILE (PASSED .and. I < dim1a)
                                           I = I + 1
                                           PASSED = A(I,J) .NE. B(I,J)
                                       END DO
                                    END DO
       CASE DEFAULT
           ERROR STOP 'ASSERT_UNIT_TEST_2D_INT64(UT, A, OP, B, MSG, KILL) - Unknown "OP" received'
       END SELECT
    END IF
    CALL TEST_STATUS_UNIT_TEST(UT, PASSED, MSG=MSG)
    !
    CALL ASSSERT_KILL(UT, PASSED, KILL)
    !
  END SUBROUTINE
  !
  ! ---------------------------------------------------------------------------------------------
  !
  SUBROUTINE ASSERT_UNIT_TEST_2D_REL32(UT, A, OP, B, MSG, KILL)
    CLASS(UNIT_TESTS),           intent(inout):: UT
    CHARACTER(2),                intent(in   ):: OP
    REAL(rel32), dimension(:,:), intent(in   ):: A, B
    CHARACTER(*),      optional, intent(in   ):: MSG
    LOGICAL,           optional, intent(in   ):: KILL
    LOGICAL:: PASSED
    INTEGER:: I, J, dim1a, dim2a, dim1b, dim2b
    !
    dim1a = size(A, dim=1)
    dim2a = size(A, dim=2)
    !
    dim1b = size(B, dim=1)
    dim2b = size(B, dim=2)
    !
    IF( dim1a /= dim1b .or. dim2a /= dim2b ) THEN
        PASSED = FALSE
    ELSE
       SELECT CASE(OP)
       CASE('EQ', 'eq', 'Eq', 'eQ')                                     ! These loops are intentially obtuse
                                    J=0;    PASSED = TRUE               !   to disable optimizations
                                    DO WHILE (PASSED .and. J < dim2a)
                                       J = J + 1
                                       I = 0
                                       DO WHILE (PASSED .and. I < dim1a)
                                           I = I + 1
                                           PASSED = A(I,J) .EQ. B(I,J)
                                       END DO
                                    END DO
       CASE('LT', 'lt', 'Lt', 'lT')
                                    J=0;    PASSED = TRUE
                                    DO WHILE (PASSED .and. J < dim2a)
                                       J = J + 1
                                       I = 0
                                       DO WHILE (PASSED .and. I < dim1a)
                                           I = I + 1
                                           PASSED = A(I,J) .LT. B(I,J)
                                       END DO
                                    END DO
       CASE('LE', 'le', 'Le', 'lE')
                                    J=0;    PASSED = TRUE
                                    DO WHILE (PASSED .and. J < dim2a)
                                       J = J + 1
                                       I = 0
                                       DO WHILE (PASSED .and. I < dim1a)
                                           I = I + 1
                                           PASSED = A(I,J) .LE. B(I,J)
                                       END DO
                                    END DO
       CASE('GT', 'gt', 'Gt', 'gT')
                                    J=0;    PASSED = TRUE
                                    DO WHILE (PASSED .and. J < dim2a)
                                       J = J + 1
                                       I = 0
                                       DO WHILE (PASSED .and. I < dim1a)
                                           I = I + 1
                                           PASSED = A(I,J) .GT. B(I,J)
                                       END DO
                                    END DO
       CASE('GE', 'ge', 'Ge', 'gE')
                                    J=0;    PASSED = TRUE
                                    DO WHILE (PASSED .and. J < dim2a)
                                       J = J + 1
                                       I = 0
                                       DO WHILE (PASSED .and. I < dim1a)
                                           I = I + 1
                                           PASSED = A(I,J) .GE. B(I,J)
                                       END DO
                                    END DO
       CASE('NE', 'ne', 'Ne', 'nE')
                                    J=0;    PASSED = TRUE
                                    DO WHILE (PASSED .and. J < dim2a)
                                       J = J + 1
                                       I = 0
                                       DO WHILE (PASSED .and. I < dim1a)
                                           I = I + 1
                                           PASSED = A(I,J) .NE. B(I,J)
                                       END DO
                                    END DO
       CASE DEFAULT
           ERROR STOP 'ASSERT_UNIT_TEST_2D_REL32(UT, A, OP, B, MSG, KILL) - Unknown "OP" received'
       END SELECT
    END IF
    CALL TEST_STATUS_UNIT_TEST(UT, PASSED, MSG=MSG)
    !
    CALL ASSSERT_KILL(UT, PASSED, KILL)
    !
  END SUBROUTINE
  !
  ! ---------------------------------------------------------------------------------------------
  !
  SUBROUTINE ASSERT_UNIT_TEST_2D_REL64(UT, A, OP, B, MSG, KILL, TOL)
    CLASS(UNIT_TESTS),           intent(inout):: UT
    CHARACTER(2),                intent(in   ):: OP
    REAL(rel64), dimension(:,:), intent(in   ):: A, B
    CHARACTER(*),      optional, intent(in   ):: MSG
    LOGICAL,           optional, intent(in   ):: KILL
    REAL(rel64),       optional, intent(in   ):: TOL
    LOGICAL:: PASSED
    INTEGER:: I, J, dim1a, dim2a, dim1b, dim2b
    !
    dim1a = size(A, dim=1)
    dim2a = size(A, dim=2)
    !
    dim1b = size(B, dim=1)
    dim2b = size(B, dim=2)
    !
    IF( dim1a /= dim1b .or. dim2a /= dim2b ) THEN
        PASSED = FALSE
    ELSE
       SELECT CASE(OP)
       CASE('EQ', 'eq', 'Eq', 'eQ')                                     ! These loops are intentially obtuse
                                    J=0;    PASSED = TRUE               !   to disable optimizations
                                    DO WHILE (PASSED .and. J < dim2a)
                                       J = J + 1
                                       I = 0
                                       DO WHILE (PASSED .and. I < dim1a)
                                           I = I + 1
                                           PASSED = IS_CLOSE(A(I,J), B(I,J), TOL)
                                       END DO
                                    END DO
       CASE('LT', 'lt', 'Lt', 'lT')
                                    J=0;    PASSED = TRUE
                                    DO WHILE (PASSED .and. J < dim2a)
                                       J = J + 1
                                       I = 0
                                       DO WHILE (PASSED .and. I < dim1a)
                                           I = I + 1
                                           PASSED = A(I,J) .LT. B(I,J)
                                       END DO
                                    END DO
       CASE('LE', 'le', 'Le', 'lE')
                                    J=0;    PASSED = TRUE
                                    DO WHILE (PASSED .and. J < dim2a)
                                       J = J + 1
                                       I = 0
                                       DO WHILE (PASSED .and. I < dim1a)
                                           I = I + 1
                                           PASSED = A(I,J) .LE. B(I,J)
                                       END DO
                                    END DO
       CASE('GT', 'gt', 'Gt', 'gT')
                                    J=0;    PASSED = TRUE
                                    DO WHILE (PASSED .and. J < dim2a)
                                       J = J + 1
                                       I = 0
                                       DO WHILE (PASSED .and. I < dim1a)
                                           I = I + 1
                                           PASSED = A(I,J) .GT. B(I,J)
                                       END DO
                                    END DO
       CASE('GE', 'ge', 'Ge', 'gE')
                                    J=0;    PASSED = TRUE
                                    DO WHILE (PASSED .and. J < dim2a)
                                       J = J + 1
                                       I = 0
                                       DO WHILE (PASSED .and. I < dim1a)
                                           I = I + 1
                                           PASSED = A(I,J) .GE. B(I,J)
                                       END DO
                                    END DO
       CASE('NE', 'ne', 'Ne', 'nE')
                                    J=0;    PASSED = TRUE
                                    DO WHILE (PASSED .and. J < dim2a)
                                       J = J + 1
                                       I = 0
                                       DO WHILE (PASSED .and. I < dim1a)
                                           I = I + 1
                                           PASSED = A(I,J) .NE. B(I,J)
                                       END DO
                                    END DO
       CASE DEFAULT
           ERROR STOP 'ASSERT_UNIT_TEST_2D_REL64(UT, A, OP, B, MSG, KILL) - Unknown "OP" received'
       END SELECT
    END IF
    CALL TEST_STATUS_UNIT_TEST(UT, PASSED, MSG=MSG)
    !
    CALL ASSSERT_KILL(UT, PASSED, KILL)
    !
  END SUBROUTINE
  !
  ! ---------------------------------------------------------------------------------------------
  !
  SUBROUTINE ASSERT_UNIT_TEST_2D_CHAR(UT, A, OP, B, MSG, KILL, CAP)
    CLASS(UNIT_TESTS),            intent(inout):: UT
    CHARACTER(2),                 intent(in   ):: OP
    CHARACTER(*), dimension(:,:), intent(in   ):: A, B
    CHARACTER(*),       optional, intent(in   ):: MSG
    LOGICAL,            optional, intent(in   ):: KILL
    LOGICAL,            optional, intent(in   ):: CAP
    CHARACTER(len(A)):: At
    CHARACTER(len(B)):: Bt
    LOGICAL:: PASSED, NO_CASE
    INTEGER:: I, J, dim1a, dim2a, dim1b, dim2b
    !
    NO_CASE = .TRUE.
    if(present(CAP)) NO_CASE = CAP
    !
    dim1a = size(A, dim=1)
    dim2a = size(A, dim=2)
    !
    dim1b = size(B, dim=1)
    dim2b = size(B, dim=2)
    !
    IF( dim1a /= dim1b .or. dim2a /= dim2b ) THEN
        PASSED = FALSE
    ELSE
       SELECT CASE(OP)
       CASE('EQ', 'eq', 'Eq', 'eQ')                                     ! These loops are intentially obtuse
                                    J=0;    PASSED = TRUE               !   to disable optimizations
                                    DO WHILE (PASSED .and. J < dim2a)
                                       J = J + 1
                                       I = 0
                                       DO WHILE (PASSED .and. I < dim1a)
                                           I = I + 1
                                           At = A(i,j)
                                           Bt = B(i,j)
                                           if(NO_CASE) then
                                                   call upper(At)
                                                   call upper(Bt)
                                           end if
                                           PASSED = At == Bt
                                       END DO
                                    END DO
       CASE('LT', 'lt', 'Lt', 'lT')
                                    J=0;    PASSED = TRUE
                                    DO WHILE (PASSED .and. J < dim2a)
                                       J = J + 1
                                       I = 0
                                       DO WHILE (PASSED .and. I < dim1a)
                                           I = I + 1
                                           At = A(i,j)
                                           Bt = B(i,j)
                                           if(NO_CASE) then
                                                   call upper(At)
                                                   call upper(Bt)
                                           end if
                                           PASSED = At .LT. Bt
                                       END DO
                                    END DO
       CASE('LE', 'le', 'Le', 'lE')
                                    J=0;    PASSED = TRUE
                                    DO WHILE (PASSED .and. J < dim2a)
                                       J = J + 1
                                       I = 0
                                       DO WHILE (PASSED .and. I < dim1a)
                                           I = I + 1
                                           At = A(i,j)
                                           Bt = B(i,j)
                                           if(NO_CASE) then
                                                   call upper(At)
                                                   call upper(Bt)
                                           end if
                                           PASSED = At .LE. Bt
                                       END DO
                                    END DO
       CASE('GT', 'gt', 'Gt', 'gT')
                                    J=0;    PASSED = TRUE
                                    DO WHILE (PASSED .and. J < dim2a)
                                       J = J + 1
                                       I = 0
                                       DO WHILE (PASSED .and. I < dim1a)
                                           I = I + 1
                                           At = A(i,j)
                                           Bt = B(i,j)
                                           if(NO_CASE) then
                                                   call upper(At)
                                                   call upper(Bt)
                                           end if
                                           PASSED = At .GT. Bt
                                       END DO
                                    END DO
       CASE('GE', 'ge', 'Ge', 'gE')
                                    J=0;    PASSED = TRUE
                                    DO WHILE (PASSED .and. J < dim2a)
                                       J = J + 1
                                       I = 0
                                       DO WHILE (PASSED .and. I < dim1a)
                                           I = I + 1
                                           At = A(i,j)
                                           Bt = B(i,j)
                                           if(NO_CASE) then
                                                   call upper(At)
                                                   call upper(Bt)
                                           end if
                                           PASSED = At .GE. Bt
                                       END DO
                                    END DO
       CASE('NE', 'ne', 'Ne', 'nE')
                                    J=0;    PASSED = TRUE
                                    DO WHILE (PASSED .and. J < dim2a)
                                       J = J + 1
                                       I = 0
                                       DO WHILE (PASSED .and. I < dim1a)
                                           I = I + 1
                                           At = A(i,j)
                                           Bt = B(i,j)
                                           if(NO_CASE) then
                                                   call upper(At)
                                                   call upper(Bt)
                                           end if
                                           PASSED = At .NE. Bt
                                       END DO
                                    END DO
       CASE DEFAULT
           ERROR STOP 'ASSERT_UNIT_TEST_2D_REL64(UT, A, OP, B, MSG, KILL) - Unknown "OP" received'
       END SELECT
    END IF
    CALL TEST_STATUS_UNIT_TEST(UT, PASSED, MSG=MSG)
    !
    CALL ASSSERT_KILL(UT, PASSED, KILL)
    !
  END SUBROUTINE
  !
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !
  SUBROUTINE ASSERT_UNIT_TEST_0D_NONZERO_INT8(UT, INT, WANT, MSG, KILL)
    CLASS(UNIT_TESTS),      intent(inout):: UT
    INTEGER(INT8),          intent(in   ):: INT
    INTEGER(INT8),optional, intent(in   ):: WANT
    CHARACTER(*), optional, intent(in   ):: MSG
    LOGICAL,      optional, intent(in   ):: KILL
    LOGICAL:: PASSED
    !
    if(present(WANT)) then
        PASSED = INT == WANT
    else
        PASSED = INT /= 0_int8
    end if
    CALL TEST_STATUS_UNIT_TEST(UT, PASSED, MSG=MSG)
    !
    CALL ASSSERT_KILL(UT, PASSED, KILL)
  END SUBROUTINE
  !
  ! ---------------------------------------------------------------------------------------------
  !
  SUBROUTINE ASSERT_UNIT_TEST_0D_NONZERO_INT16(UT, INT, WANT, MSG, KILL)
    CLASS(UNIT_TESTS),       intent(inout):: UT
    INTEGER(INT16),          intent(in   ):: INT
    INTEGER(INT16),optional, intent(in   ):: WANT
    CHARACTER(*),  optional, intent(in   ):: MSG
    LOGICAL,       optional, intent(in   ):: KILL
    LOGICAL:: PASSED
    !
    if(present(WANT)) then
        PASSED = INT == WANT
    else
        PASSED = INT /= 0_int16
    end if
    CALL TEST_STATUS_UNIT_TEST(UT, PASSED, MSG=MSG)
    !
    CALL ASSSERT_KILL(UT, PASSED, KILL)
  END SUBROUTINE
  !
  ! ---------------------------------------------------------------------------------------------
  !
  SUBROUTINE ASSERT_UNIT_TEST_0D_NONZERO_INT32(UT, INT, WANT, MSG, KILL)
    CLASS(UNIT_TESTS),       intent(inout):: UT
    INTEGER(INT32),          intent(in   ):: INT
    INTEGER(INT32),optional, intent(in   ):: WANT
    CHARACTER(*),  optional, intent(in   ):: MSG
    LOGICAL,       optional, intent(in   ):: KILL
    LOGICAL:: PASSED
    !
    if(present(WANT)) then
        PASSED = INT == WANT
    else
        PASSED = INT /= 0_int32
    end if
    CALL TEST_STATUS_UNIT_TEST(UT, PASSED, MSG=MSG)
    !
    CALL ASSSERT_KILL(UT, PASSED, KILL)
  END SUBROUTINE
  !
  ! ---------------------------------------------------------------------------------------------
  !
  SUBROUTINE ASSERT_UNIT_TEST_0D_NONZERO_INT64(UT, INT, WANT, MSG, KILL)
    CLASS(UNIT_TESTS),       intent(inout):: UT
    INTEGER(INT64),          intent(in   ):: INT
    INTEGER(INT64),optional, intent(in   ):: WANT
    CHARACTER(*),  optional, intent(in   ):: MSG
    LOGICAL,       optional, intent(in   ):: KILL
    LOGICAL:: PASSED
    !
    if(present(WANT)) then
        PASSED = INT == WANT
    else
        PASSED = INT /= 0_int64
    end if
    CALL TEST_STATUS_UNIT_TEST(UT, PASSED, MSG=MSG)
    !
    CALL ASSSERT_KILL(UT, PASSED, KILL)
  END SUBROUTINE
  !
  ! ---------------------------------------------------------------------------------------------
  !
  SUBROUTINE ASSERT_UNIT_TEST_1D_NONZERO_INT8(UT, INT, WANT, MSG, KILL)
    CLASS(UNIT_TESTS),                    intent(inout):: UT
    INTEGER(INT8),          dimension(:), intent(in   ):: INT
    INTEGER(INT8),optional, dimension(:), intent(in   ):: WANT
    CHARACTER(*), optional,               intent(in   ):: MSG
    LOGICAL,      optional,               intent(in   ):: KILL
    LOGICAL:: PASSED
    !
    if(present(WANT)) then
        if(size(INT) /= size(WANT)) then
             PASSED = .false.
        else
             PASSED = all(INT == WANT)
        end if
    else
        PASSED = all(INT /= 0_int8)
    end if
    CALL TEST_STATUS_UNIT_TEST(UT, PASSED, MSG=MSG)
    !
    CALL ASSSERT_KILL(UT, PASSED, KILL)
  END SUBROUTINE
  !
  ! ---------------------------------------------------------------------------------------------
  !
  SUBROUTINE ASSERT_UNIT_TEST_1D_NONZERO_INT16(UT, INT, WANT, MSG, KILL)
    CLASS(UNIT_TESTS),                     intent(inout):: UT
    INTEGER(INT16),          dimension(:), intent(in   ):: INT
    INTEGER(INT16),optional, dimension(:), intent(in   ):: WANT
    CHARACTER(*),  optional,               intent(in   ):: MSG
    LOGICAL,       optional,               intent(in   ):: KILL
    LOGICAL:: PASSED
    !
    if(present(WANT)) then
        if(size(INT) /= size(WANT)) then
             PASSED = .false.
        else
             PASSED = all(INT == WANT)
        end if
    else
        PASSED = all(INT /= 0_int16)
    end if
    CALL TEST_STATUS_UNIT_TEST(UT, PASSED, MSG=MSG)
    !
    CALL ASSSERT_KILL(UT, PASSED, KILL)
  END SUBROUTINE
  !
  ! ---------------------------------------------------------------------------------------------
  !
  SUBROUTINE ASSERT_UNIT_TEST_1D_NONZERO_INT32(UT, INT, WANT, MSG, KILL)
    CLASS(UNIT_TESTS),                     intent(inout):: UT
    INTEGER(INT32),          dimension(:), intent(in   ):: INT
    INTEGER(INT32),optional, dimension(:), intent(in   ):: WANT
    CHARACTER(*),  optional,               intent(in   ):: MSG
    LOGICAL,       optional,               intent(in   ):: KILL
    LOGICAL:: PASSED
    !
    if(present(WANT)) then
        if(size(INT) /= size(WANT)) then
             PASSED = .false.
        else
             PASSED = all(INT == WANT)
        end if
    else
        PASSED = all(INT /= 0_int32)
    end if
    CALL TEST_STATUS_UNIT_TEST(UT, PASSED, MSG=MSG)
    !
    CALL ASSSERT_KILL(UT, PASSED, KILL)
  END SUBROUTINE
  !
  ! ---------------------------------------------------------------------------------------------
  !
  SUBROUTINE ASSERT_UNIT_TEST_1D_NONZERO_INT64(UT, INT, WANT, MSG, KILL)
    CLASS(UNIT_TESTS),                     intent(inout):: UT
    INTEGER(INT64),          dimension(:), intent(in   ):: INT
    INTEGER(INT64),optional, dimension(:), intent(in   ):: WANT
    CHARACTER(*),  optional,               intent(in   ):: MSG
    LOGICAL,       optional,               intent(in   ):: KILL
    LOGICAL:: PASSED
    !
    if(present(WANT)) then
        if(size(INT) /= size(WANT)) then
             PASSED = .false.
        else
             PASSED = all(INT == WANT)
        end if
    else
        PASSED = all(INT /= 0_int64)
    end if
    CALL TEST_STATUS_UNIT_TEST(UT, PASSED, MSG=MSG)
    !
    CALL ASSSERT_KILL(UT, PASSED, KILL)
  END SUBROUTINE
  !
  ! ---------------------------------------------------------------------------------------------
  !
  SUBROUTINE ASSERT_UNIT_TEST_2D_NONZERO_INT8(UT, INT, WANT, MSG, KILL)
    CLASS(UNIT_TESTS),                      intent(inout):: UT
    INTEGER(INT8),          dimension(:,:), intent(in   ):: INT
    INTEGER(INT8),optional, dimension(:,:), intent(in   ):: WANT
    CHARACTER(*), optional,                 intent(in   ):: MSG
    LOGICAL,      optional,                 intent(in   ):: KILL
    LOGICAL:: PASSED
    !
    if(present(WANT)) then
        if(size(INT, 1) /= size(WANT, 1) .or. size(INT, 2) /= size(WANT, 2)) then
             PASSED = .false.
        else
             PASSED = all(INT == WANT)
        end if
    else
        PASSED = all(INT /= 0_int8)
    end if
    CALL TEST_STATUS_UNIT_TEST(UT, PASSED, MSG=MSG)
    !
    CALL ASSSERT_KILL(UT, PASSED, KILL)
  END SUBROUTINE
  !
  ! ---------------------------------------------------------------------------------------------
  !
  SUBROUTINE ASSERT_UNIT_TEST_2D_NONZERO_INT16(UT, INT, WANT, MSG, KILL)
    CLASS(UNIT_TESTS),                     intent(inout):: UT
    INTEGER(INT16),          dimension(:,:), intent(in   ):: INT
    INTEGER(INT16),optional, dimension(:,:), intent(in   ):: WANT
    CHARACTER(*),  optional,               intent(in   ):: MSG
    LOGICAL,       optional,               intent(in   ):: KILL
    LOGICAL:: PASSED
    !
    if(present(WANT)) then
        if(size(INT, 1) /= size(WANT, 1) .or. size(INT, 2) /= size(WANT, 2)) then
             PASSED = .false.
        else
             PASSED = all(INT == WANT)
        end if
    else
        PASSED = all(INT /= 0_int16)
    end if
    CALL TEST_STATUS_UNIT_TEST(UT, PASSED, MSG=MSG)
    !
    CALL ASSSERT_KILL(UT, PASSED, KILL)
  END SUBROUTINE
  !
  ! ---------------------------------------------------------------------------------------------
  !
  SUBROUTINE ASSERT_UNIT_TEST_2D_NONZERO_INT32(UT, INT, WANT, MSG, KILL)
    CLASS(UNIT_TESTS),                     intent(inout):: UT
    INTEGER(INT32),          dimension(:,:), intent(in   ):: INT
    INTEGER(INT32),optional, dimension(:,:), intent(in   ):: WANT
    CHARACTER(*),  optional,               intent(in   ):: MSG
    LOGICAL,       optional,               intent(in   ):: KILL
    LOGICAL:: PASSED
    !
    if(present(WANT)) then
        if(size(INT, 1) /= size(WANT, 1) .or. size(INT, 2) /= size(WANT, 2)) then
             PASSED = .false.
        else
             PASSED = all(INT == WANT)
        end if
    else
        PASSED = all(INT /= 0_int32)
    end if
    CALL TEST_STATUS_UNIT_TEST(UT, PASSED, MSG=MSG)
    !
    CALL ASSSERT_KILL(UT, PASSED, KILL)
  END SUBROUTINE
  !
  ! ---------------------------------------------------------------------------------------------
  !
  SUBROUTINE ASSERT_UNIT_TEST_2D_NONZERO_INT64(UT, INT, WANT, MSG, KILL)
    CLASS(UNIT_TESTS),                     intent(inout):: UT
    INTEGER(INT64),          dimension(:,:), intent(in   ):: INT
    INTEGER(INT64),optional, dimension(:,:), intent(in   ):: WANT
    CHARACTER(*),  optional,               intent(in   ):: MSG
    LOGICAL,       optional,               intent(in   ):: KILL
    LOGICAL:: PASSED
    !
    if(present(WANT)) then
        if(size(INT, 1) /= size(WANT, 1) .or. size(INT, 2) /= size(WANT, 2)) then
             PASSED = .false.
        else
             PASSED = all(INT == WANT)
        end if
    else
        PASSED = all(INT /= 0_int64)
    end if
    CALL TEST_STATUS_UNIT_TEST(UT, PASSED, MSG=MSG)
    !
    CALL ASSSERT_KILL(UT, PASSED, KILL)
  END SUBROUTINE
  !
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !
  PURE FUNCTION ASSERT_EQUAL(A, B, TOL, CAP) RESULT(ANS)
    CLASS(*),              intent(in):: A, B
    LOGICAL,     optional, intent(in):: CAP
    REAL(rel64), optional, intent(in):: TOL
    LOGICAL:: ANS
    !
    ANS = .FALSE.
    !
    SELECT TYPE(A)
    TYPE IS( REAL(rel64) )
         SELECT TYPE(B)
         TYPE IS( REAL(rel64) )
                       ANS = IS_CLOSE_DBL(A,B,TOL)
         END SELECT
    TYPE IS( INTEGER(int32) )
         SELECT TYPE(B)
         TYPE IS( INTEGER(int32) )
                       ANS = A == B
         END SELECT
    TYPE IS( INTEGER(int64) )
         SELECT TYPE(B)
         TYPE IS( INTEGER(int64) )
                       ANS = A == B
         END SELECT
    TYPE IS( REAL(rel32) )
         SELECT TYPE(B)
         TYPE IS( REAL(rel32) )
                       ANS = IS_CLOSE_SNG(A,B,real(TOL, rel32))
         END SELECT
    TYPE IS( INTEGER(int8) )
         SELECT TYPE(B)
         TYPE IS( INTEGER(int8) )
                       ANS = A == B
         END SELECT
    TYPE IS( INTEGER(int16) )
         SELECT TYPE(B)
         TYPE IS( INTEGER(int16) )
                       ANS = A == B
         END SELECT
    TYPE IS( CHARACTER(*) )
         SELECT TYPE(B)
         TYPE IS( CHARACTER(*) )
                       ANS = ASSERT_EQUAL_CHAR(A, B, CAP)
         END SELECT
    END SELECT
    !
  END FUNCTION
  !
  ! ---------------------------------------------------------------------------------------------
  !
  PURE FUNCTION ASSERT_EQUAL_CHAR(A, B, CAP) RESULT(ANS)
    CHARACTER(*),           intent(in   ):: A, B
    LOGICAL,      optional, intent(in   ):: CAP
    LOGICAL:: ANS
    CHARACTER(LEN(A)):: At
    CHARACTER(LEN(B)):: Bt
    LOGICAL:: NO_CASE
    !
    NO_CASE = .TRUE.
    if(present(CAP)) NO_CASE = CAP
    !
    At = A
    Bt = B
    if(NO_CASE) then
                call upper(At)
                call upper(Bt)
    end if
    !
    ANS = A == B
  END FUNCTION
  !
  ! ---------------------------------------------------------------------------------------------
  !
END MODULE
  !
  ! ---------------------------------------------------------------------------------------------
  !
  !!!SUBROUTINE STATUS_UNIT_TEST(UT, PASSED, MSG)
  !!!  CLASS(UNIT_TESTS),      intent(inout):: UT
  !!!  LOGICAL,                intent(in   ):: PASSED
  !!!  CHARACTER(*), optional, intent(in   ):: MSG
  !!!  INTEGER:: I
  !!!  !
  !!!  I = UT%N
  !!!  !
  !!!  IF(PASSED) THEN
  !!!      UT%TEST(I)%STAT = P
  !!!  ELSE
  !!!      UT%FAILED = TRUE
  !!!      UT%TEST(I)%STAT = F
  !!!      !
  !!!      IF(PRESENT(MSG)) CALL UT%TEST(I)%ADD_NOTE(MSG)
  !!!      !
  !!!  END IF
  !!!  !
  !!!  IF(UT%IOUT /= NEG) THEN
  !!!      WRITE(UT%IOUT,'(A)') UT%TEST(I)%STAT
  !!!      !
  !!!      IF(UT%TEST(I)%HAS_NOTE) WRITE(UT%IOUT,'(32x, 2A)') "- ",UT%TEST(I)%NOTE
  !!!      !
  !!!      WRITE(UT%IOUT,'(A)')
  !!!  END IF
  !!!  !
  !!!END SUBROUTINE