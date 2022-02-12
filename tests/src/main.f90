﻿!
! -----------------------------------------------------------------------------------------------
!
PROGRAM MAIN
  USE, INTRINSIC:: ISO_FORTRAN_ENV, ONLY: REL32=>REAL32, REL64=>REAL64, INT8, INT16, INT32, INT64, qp=>REAL128
  USE ERROR_INTERFACE, ONLY: PAUSE
  USE UNIT_TESTING_INSTRUCTION,    ONLY: UNIT_TESTS
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  IMPLICIT NONE 
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  TYPE(UNIT_TESTS):: UT
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !
  !
  CALL UT%INIT(NTEST=128)
  CALL test_WRITE_ARRAY_INTERFACE(UT)
  
  !
  ! ----------------------------------------------------------------------------------------------
  !
  CALL test_ADJACENCY_LIST_INSTRUCTION(UT)
  !
  ! ----------------------------------------------------------------------------------------------
  !
  CALL test_ALLOC_INTERFACE(UT)
  !
  ! ----------------------------------------------------------------------------------------------
  !
  CALL test_ARRAY_DATA_TYPES(UT)
  !
  ! ----------------------------------------------------------------------------------------------
  !
  CALL test_BINARY_HEAP_INSTRUCTION(UT)
  !
  ! ----------------------------------------------------------------------------------------------
  !
  CALL test_DYNAMIC_ARRAY(UT)
  !
  ! ----------------------------------------------------------------------------------------------
  !
  CALL test_CALENDAR_FUNCTIONS(UT)
  !
  ! ----------------------------------------------------------------------------------------------
  !
  CALL test_CAST_TO_STRING(UT)
  !
  ! ----------------------------------------------------------------------------------------------
  !
  CALL test_CONSOLE_COMMANDER(UT)
  !
  ! ----------------------------------------------------------------------------------------------
  !
  CALL test_CONSTANTS(UT)
  !
  ! ----------------------------------------------------------------------------------------------
  !
  CALL test_CYCLING_TEXT_FILE_INTERFACE(UT)
  !
  ! ----------------------------------------------------------------------------------------------
  !
  CALL test_DATE_OPERATOR_INSTRUCTION(UT)
  !
  ! ----------------------------------------------------------------------------------------------
  !
  CALL test_EquationParser(UT)
  !
  ! ----------------------------------------------------------------------------------------------
  !
  CALL test_ERROR_INTERFACE(UT)
  !
  ! ----------------------------------------------------------------------------------------------
  !
  CALL test_FILE_INCREMENTER_INTERFACE(UT)
  !
  ! ----------------------------------------------------------------------------------------------
  !
  CALL test_FILE_IO_INTERFACE(UT)
  !
  ! ----------------------------------------------------------------------------------------------
  !
  CALL test_GENERIC_BLOCK_READER_INSTRUCTION(UT)
  !
  ! ----------------------------------------------------------------------------------------------
  !
  CALL test_GENERIC_INPUT_FILE_INSTRUCTION(UT)
  !
  ! ----------------------------------------------------------------------------------------------
  !
  CALL test_GENERIC_OPEN_INTERFACE(UT)
  !
  ! ----------------------------------------------------------------------------------------------
  !
  CALL test_GENERIC_OUTPUT_FILE_INSTRUCTION(UT)
  !
  ! ----------------------------------------------------------------------------------------------
  !
  CALL test_HASH_TABLE_INSTRUCTION(UT)
  !
  ! ----------------------------------------------------------------------------------------------
  !
  CALL test_INTEGER_ARRAY_BUILDER_INTERFACE(UT)
  !
  ! ----------------------------------------------------------------------------------------------
  !
  CALL test_INTEGER_QUEUE_INSTRUCTION(UT)
  !
  ! ----------------------------------------------------------------------------------------------
  !
  CALL test_IS_ASCII_INTERFACE(UT)
  !
  ! ----------------------------------------------------------------------------------------------
  !
  CALL test_IS_ROUTINES(UT)
  !
  ! ----------------------------------------------------------------------------------------------
  !
  CALL test_IXJ_INSTRUCTION(UT)
  !
  ! ----------------------------------------------------------------------------------------------
  !
  CALL test_LINE_WRITER_INTERFACE(UT)
  !
  ! ----------------------------------------------------------------------------------------------
  !
  CALL test_LINKED_LIST_INSTRUCTION(UT)
  !
  ! ----------------------------------------------------------------------------------------------
  !
  CALL test_LIST_ARRAY_INPUT_INTERFACE(UT)
  !
  ! ----------------------------------------------------------------------------------------------
  !
  CALL test_LOG2_INTERFACE(UT)
  !
  ! ----------------------------------------------------------------------------------------------
  !
  CALL test_LOOKUP_TABLE_INSTRUCTION(UT)
  !
  ! ----------------------------------------------------------------------------------------------
  !
  CALL test_NAME_ID_INTERFACE(UT)
  !
  ! ----------------------------------------------------------------------------------------------
  !
  CALL test_NUM2STR_INTERFACE(UT)
  !
  ! ----------------------------------------------------------------------------------------------
  !
  CALL test_OBS_GROUP_INTERPOLATOR(UT)
  !
  ! ----------------------------------------------------------------------------------------------
  !
  CALL test_PARSE_WORD_INTERFACE(UT)
  !
  ! ----------------------------------------------------------------------------------------------
  !
  CALL test_PATH_INTERFACE(UT)
  !
  ! ----------------------------------------------------------------------------------------------
  !
  CALL test_POSITION_INTERFACE(UT)
  !
  ! ----------------------------------------------------------------------------------------------
  !
  CALL test_POST_KEY_SUB(UT)
  !
  ! ----------------------------------------------------------------------------------------------
  !
  CALL test_PRIME_FINDER(UT)
  !
  ! ----------------------------------------------------------------------------------------------
  !
  CALL test_RANDOM_ROUTINES(UT)
  !
  ! ----------------------------------------------------------------------------------------------
  !
  CALL test_RELAX_INTERFACE(UT)
  !
  ! ----------------------------------------------------------------------------------------------
  !
  CALL test_SET_ARRAY_INTERFACE(UT)
  !
  ! ----------------------------------------------------------------------------------------------
  !
  CALL test_SLEEP_INTERFACE(UT)
  !
  ! ----------------------------------------------------------------------------------------------
  !
  CALL test_SORT_INTERFACE(UT)
  !
  ! ----------------------------------------------------------------------------------------------
  !
  CALL test_STRINGS(UT)
  !
  ! ----------------------------------------------------------------------------------------------
  !
  CALL test_SUB_BLOCK_INPUT_INTERFACE(UT)
  !
  ! ----------------------------------------------------------------------------------------------
  !
  CALL test_TIME_SERIES_INSTRUCTION(UT)
  !
  ! ----------------------------------------------------------------------------------------------
  !
  CALL test_TIMER_INSTRUCTION(UT)
  !
  ! ----------------------------------------------------------------------------------------------
  !
  CALL test_TRANSIENT_FILE_READER_INSTRUCTION(UT)
  !
  ! ----------------------------------------------------------------------------------------------
  !
  CALL test_ULOAD_AND_SFAC_INTERFACE(UT)
  !
  ! ----------------------------------------------------------------------------------------------
  !
  CALL test_UTIL_INTERFACE(UT)
  !
  ! ----------------------------------------------------------------------------------------------
  !
  CALL test_VARIABLE_POINTER_LIST_INTERFACE(UT)
  !
  ! ----------------------------------------------------------------------------------------------
  !
  CALL test_WARNING_TYPE_INSTRUCTION(UT)
  !
  ! ----------------------------------------------------------------------------------------------
  !
  CALL test_WRITE_ARRAY_INTERFACE(UT)
  !
  ! ----------------------------------------------------------------------------------------------
  !
  CALL test_XY_GRID_COORDINATE_INTERFACE(UT)
  !
  ! ----------------------------------------------------------------------------------------------
  !
  CALL UT%COMPLETE(PRINT_FAILED=.TRUE., NOSTOP=.TRUE.) 
  !
  CALL PAUSE("PRESS ENTER TO END TESTS")
  !
END PROGRAM
!
!
! #################################################################################################
! #################################################################################################
! #################################################################################################
!


  !TYPE(RANDOM_GENERATOR):: rg
  !TYPE(ONLINE_STATS):: stat
  !INTEGER:: I, J, K, mxiter
  !DOUBLE PRECISION, dimension(6):: M
  !DOUBLE PRECISION:: div
  !DOUBLE PRECISION:: ans
  !mxiter = 100000000
  !M = 0.0_qp
  !div = 0.0_qp
  !do K=1, 100
  !do J=1, mxiter
  ! call rg%NORMAL(ans)
  !    call stat%add(ans)
  !    do i=1, 6
  !            M(i) = M(i) + ans**i
  !    end do
  !end do
  !div = div + REAL(mxiter, REL64)
  !WRITE(*,*) 'Iter ', K
  !WRITE(*,*) 
  !WRITE(*,*) 'av ', stat%mean(), M(1)/div
  !WRITE(*,*) 'va ', stat%var() , M(2)/div
  !WRITE(*,*) 'sk ', stat%skew(), M(3)/div
  !WRITE(*,*) 'ku ', stat%kurt(), M(4)/div
  !WRITE(*,*) 'M5 ', M(5)/div
  !WRITE(*,*) 'M6 ', M(6)/div
  !end do
  !WRITE(123,*) 
  !WRITE(123,*) 'av ', stat%mean(), M(1)/div
  !WRITE(123,*) 'va ', stat%var() , M(2)/div
  !WRITE(123,*) 'sk ', stat%skew(), M(3)/div
  !WRITE(123,*) 'ku ', stat%kurt(), M(4)/div
  !WRITE(123,*) 'M5 ', M(5)/div
  !WRITE(123,*) 'M6 ', M(6)/div
  !WRITE(*,*) 'NORM IS DONE'
  !PAUSE
  !M = 0.0_qp
  !div = 0.0_qp
  !call stat%init()
  !mxiter = 100000000
  !M = 0.0_qp
  !div = 0.0_qp
  !do K=1, 100
  !do J=1, mxiter
  !    call rg%exp(ans)
  !    call stat%add(ans)
  !    do i=1, 6
  !            M(i) = M(i) + ans**i
  !    end do
  !end do
  !div = div + REAL(mxiter, REL64)
  !WRITE(*,*) 'Iter ', K
  !WRITE(*,*) 
  !WRITE(*,*) 'av ', stat%mean(), M(1)/div
  !WRITE(*,*) 'va ', stat%var() , M(2)/div
  !WRITE(*,*) 'sk ', stat%skew(), M(3)/div
  !WRITE(*,*) 'ku ', stat%kurt(), M(4)/div
  !WRITE(*,*) 'M5 ', M(5)/div
  !WRITE(*,*) 'M6 ', M(6)/div
  !WRITE(*,*) 
  !end do
  !WRITE(123,*) 
  !WRITE(123,*) 'av ', stat%mean(), M(1)/div
  !WRITE(123,*) 'va ', stat%var() , M(2)/div
  !WRITE(123,*) 'sk ', stat%skew(), M(3)/div
  !WRITE(123,*) 'ku ', stat%kurt(), M(4)/div
  !WRITE(123,*) 'M5 ', M(5)/div
  !WRITE(123,*) 'M6 ', M(6)/div
  !! 
  !pause