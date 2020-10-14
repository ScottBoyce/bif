!
SUBROUTINE test_ARRAY_DATA_TYPES(UT)
  USE UNIT_TESTING_INSTRUCTION, ONLY: UNIT_TESTS
  USE ARRAY_DATA_TYPES, ONLY: JOIN_CHAR,                                     &
                              LEN, LEN_TRIM, SIZE,                           &
                              CHARACTER_TYPE, CHARACTER_TYPE_ARRAY,          &
                              CHARACTER_BUF_TYPE,                            &
                              CHARACTER_ARRAY,                               &
                              INTEGER_VECTOR, DOUBLE_VECTOR, LOGICAL_VECTOR, &
                              INTEGER_MATRIX, DOUBLE_MATRIX,                 &
                              COMPRESSED_VALUE_STORAGE,                      &
                              COMPRESSED_LOCATION_STORAGE,                   &
                              RAT_VOL_TYPE, RAT_VOL_BASE,                    &
                              ID_VAL_TYPE,                                   &
                              ID_RAT_VOL_TYPE,                               &
                              ID1_ID2_RAT_VOL_TYPE
  IMPLICIT NONE
  TYPE(UNIT_TESTS), INTENT(INOUT):: UT
  TYPE(CHARACTER_BUF_TYPE         ):: CBT
  TYPE(CHARACTER_ARRAY            ):: CARR
  TYPE(INTEGER_VECTOR             ):: IVEC
  TYPE(DOUBLE_VECTOR              ):: DVEC
  TYPE(LOGICAL_VECTOR             ):: LVEC
  TYPE(INTEGER_MATRIX             ):: IMAT
  TYPE(DOUBLE_MATRIX              ):: DMAT
  TYPE(COMPRESSED_VALUE_STORAGE   ):: CVS
  TYPE(COMPRESSED_LOCATION_STORAGE):: CLS
  TYPE(RAT_VOL_TYPE               ):: RVT
  TYPE(RAT_VOL_BASE               ):: RVB
  TYPE(ID_VAL_TYPE                ):: ID_VAL
  TYPE(ID_RAT_VOL_TYPE            ):: ID_RAT_VOL
  TYPE(ID1_ID2_RAT_VOL_TYPE       ):: ID12_RAT_VOL
  !
  LOGICAL:: TEST
  INTEGER:: I, J, K
  DOUBLE PRECISION:: DTMP
  TEST = .TRUE.
  !
  CALL UT%NEXT_SECTION("ARRAY_DATA_TYPES")
  CALL UT%NEXT_TEST("CHARACTER_TYPE")
  !
  BLOCK 
    TYPE(CHARACTER_TYPE):: CT
    CT = "Hello World"
    CALL UT%ASSERT(CT%STR=="Hello World", MSG='CT = "Hello World"')
    !
    CT = "Good Bye World"
    CALL UT%ASSERT(CT%STR=="Good Bye World", MSG='CT = "Good Bye World"')
    !
    CALL CT%DESTROY()
    CALL UT%ASSERT(.NOT. ALLOCATED(CT%STR), MSG='CT%DESTROY()')
    !
    CT = "Hello Again World"
    CALL UT%ASSERT(CT%STR=="Hello Again World", MSG='CT = "Hello Again World"')
    !
  END BLOCK
  !
  BLOCK 
    TYPE(CHARACTER_TYPE):: CT
    !
    CALL CT%SET("Hello World") 
    CALL UT%ASSERT(CT%STR=="Hello World", MSG='CT%SET("Hello World")')
    !
    CALL CT%SET("Good Bye World") 
    CALL UT%ASSERT(CT%STR=="Good Bye World", MSG='CT%SET("Good Bye World")')
    !
    CALL CT%DESTROY()
    CALL UT%ASSERT(.NOT. ALLOCATED(CT%STR), MSG='CT%DESTROY()')
    !
    CALL CT%SET("Hello Again World") 
    CALL UT%ASSERT(CT%STR=="Hello Again World", MSG='CT%SET("Hello Again World")')
    !
  END BLOCK
  !
  BLOCK 
    TYPE(CHARACTER_TYPE):: CT
    !
    CALL UT%ASSERT(CT%GET()=="", MSG='CT%SET("Hello World")')
    !
    CT = "A"; CALL CT%ADD("B"); CALL CT%ADD("C") 
    CALL UT%ASSERT(CT%GET()=="ABC", MSG='CT = "A"; CALL CT%ADD("B"); CALL CT%ADD("C")')
    !
    CALL CT%ADD_BEGIN("First 3 Letters = ") 
    CALL UT%ASSERT(CT%GET()=="First 3 Letters = ABC", MSG='CT%ADD_BEGIN("First 3 Letters = ")')
    !
    CALL CT%POP() 
    CALL UT%ASSERT(CT%GET()=="First 3 Letters = AB", MSG='CT%POP()')
    !
    CALL CT%POP(-2) 
    CALL UT%ASSERT(CT%GET()=="First 3 Letters = B", MSG='CT%POP(-2)')
    !
    CALL CT%POP(3) 
    CALL UT%ASSERT(CT%GET()=="Fist 3 Letters = B", MSG='CT%POP(3)')
    !
    CALL CT%POP(2,5) 
    CALL UT%ASSERT(CT%GET()=="F3 Letters = B", MSG='CT%POP(2,5)')
    !
    CALL CT%SET("Hello Again World") 
    CALL UT%ASSERT(CT%STR=="Hello Again World", MSG='CT%SET("Hello Again World") ')
    !
  END BLOCK
  !
  BLOCK 
    TYPE(CHARACTER_TYPE):: CT1, CT2
    !
    CT1 = "Word"
    CT2 = "Word"
    CALL UT%ASSERT(CT1==CT2, MSG='CT1==CT2')
    !
    CT1 = "Word 1"
    CT2 = "Word 2"
    CALL UT%ASSERT(CT1=="Word 1", MSG='CT1=="Word 1"')
    CALL UT%ASSERT(CT1/=CT2, MSG='CT1/=CT2')
    !
  END BLOCK
  !
  BLOCK 
    TYPE(CHARACTER_TYPE):: CT1, CT2, CT3
    !
    CT1 = "Word 1"
    CT2 = "Word 2"
    CT3 = CT1 + CT2
    CALL UT%ASSERT(CT3=="Word 1Word 2", MSG='CT3 = CT1 + CT2 - Attempt 1')
    !
    CT3 = CT1 + CT2
    CALL UT%ASSERT(CT3=="Word 1Word 2", MSG='CT3 = CT1 + CT2 - Attempt 2')
    !
    CT3 = CT1 + " and a String"
    CALL UT%ASSERT(CT3=="Word 1 and a String", MSG='CT1=="Word 1"')
    !
    CT3 = "Before There was a String there was " + CT2
    CALL UT%ASSERT(CT3=="Before There was a String there was Word 2", MSG='CT3 = CT1 + CT2 - Attempt 2')
    !
  END BLOCK
  !
  BLOCK 
    TYPE(CHARACTER_TYPE):: CT1, CT2
    CHARACTER(12):: TXT
    !
    CT1 = "Word 1"
    TXT = "Word 2"
    CT2 = CT1 + TXT
    CALL UT%ASSERT(CT2=="Word 1Word 2", MSG='CT3 = CT1 + CT2 - Attempt 1')
    !
    CT2 = TXT + CT1  !Note spaces in TXT are incldued
    CALL UT%ASSERT(CT2=="Word 2      Word 1", MSG='CT3 = CT1 + CT2 - Attempt 2')
    !
    CT1 = "Word 1"
    CT2 = "Word 2"
    TXT = CT1 // CT2
    CALL UT%ASSERT(TXT=="Word 1Word 2", MSG='CT3 = CT1 + CT2 - Attempt 1')
    !
    TXT = CT2 // CT1
    CALL UT%ASSERT(TXT=="Word 2Word 1", MSG='CT3 = CT1 + CT2 - Attempt 2')
    !
  END BLOCK
  !
  BLOCK 
    TYPE(CHARACTER_TYPE):: CT1, CT2, CT3
    !
    CT1 = "Word 1"
    CT2 = "Word 2"
    CT3 = CT1 // CT2
    CALL UT%ASSERT(CT3=="Word 1Word 2", MSG='CT3 = CT1 + CT2 - Attempt 1')
    !
    CT3 = CT2 // CT1
    CALL UT%ASSERT(CT3=="Word 2Word 1", MSG='CT3 = CT1 + CT2 - Attempt 2')
    !
  END BLOCK
  !    
  !##########################################################################################################################
  !##########################################################################################################################
  !##########################################################################################################################
  !
  CALL UT%NEXT_TEST("CHARACTER_TYPE_ARRAY")
  !
  BLOCK
    TYPE(CHARACTER_TYPE_ARRAY):: CTA
    CHARACTER(:), ALLOCATABLE:: LINE
    !
    CALL CTA%INIT()
    !
    CALL CTA%ADD("LINE 1")
    CALL CTA%ADD("LINE 2")
    CALL CTA%ADD("LINE 3")
    CALL CTA%ADD("LINE 4")
    CALL CTA%ADD("LINE 5")
    !
    CALL UT%ASSERT(CTA%VEC(3)%STR == "LINE 3", MSG='CTA%VEC(3)%STR == "LINE 3"')
    !
    CALL CTA%POP(3)
    CALL UT%ASSERT(CTA%VEC(3)%STR == "LINE 4", MSG='CTA%VEC(3)%STR == "LINE 4"')
    !
    CALL CTA%POP(2, LINE)
    CALL UT%ASSERT(CTA%VEC(3)%STR == "LINE 5" .AND. LINE=="LINE 2", MSG='CTA%VEC(3)%STR == "LINE 5" .AND. LINE=="LINE 2"')
    !
    CALL CTA%ADD("123456789012")
    CALL UT%ASSERT(CTA%MAX_LEN() == 12, MSG='CTA%MAX_LEN() == 12')
    !
    CALL CTA%ADD("ABC")
    CALL CTA%ADD("EFG")
    CALL CTA%ADD("HIJ")
    CALL CTA%ADD("efg")
    CALL CTA%ADD("LMOP")
    CALL CTA%ADD("EFG")
    CALL UT%ASSERT(CTA%FIND("ABC") == 5, MSG='CTA%FIND("ABC") == 5')
    CALL UT%ASSERT(CTA%COUNT("ABC") == 1, MSG='CTA%COUNT("ABC") == 1')
    CALL UT%ASSERT(CTA%COUNT("Poo Bear") == 0, MSG='CTA%COUNT("Poo Bear") == 0')
    CALL UT%ASSERT(CTA%COUNT("EFG") == 2, MSG='CTA%COUNT("EFG") == 2')
    CALL UT%ASSERT(CTA%COUNT("efg",.TRUE.) == 3, MSG='CTA%COUNT("efg",.TRUE.) == 3')
    !
    CALL CTA%RESET()
    CALL UT%ASSERT(CTA%FIND("ABC") == 0, MSG='CTA%FIND("ABC") == 0')
    !
    CALL CTA%ADD_UNIQUE("ABC")
    CALL UT%ASSERT(CTA%FIND("ABC") == 1, MSG='CTA%FIND("ABC") == 1 - Attempt 1')
    !
    CALL CTA%ADD_UNIQUE("ABC")
    CALL UT%ASSERT(CTA%FIND("ABC") == 1, MSG='CTA%FIND("ABC") == 1 - Attempt 2')
    !
    DO I=1, 5
            CALL CTA%POP()
    END DO
    CALL UT%ASSERT(SIZE(CTA) == 0, MSG='5 x CTA%POP()')
    !
    DO I=1, 16
            CALL CTA%ADD(CHAR(64+I))
    END DO
    !
    DO I=1, 16
            CALL CTA%ADD(CHAR(96+I))
    END DO
    CALL UT%ASSERT(SIZE(CTA) == 32, MSG='SIZE(CTA) == 32')
    CALL UT%ASSERT(CTA%VEC(20)%STR == 'd', MSG="CTA%VEC(20)%STR == 'd'")
    !
  END BLOCK
  !
  BLOCK
    TYPE(CHARACTER_TYPE_ARRAY):: CTA
    !
    CALL CTA%INIT()
    !
    DO I=1, 52+32
            CALL CTA%ADD(CHAR(32+I))
    END DO
    DO I=1, 52
            CALL CTA%ADD(CHAR(64+I))
    END DO
    !
    CALL UT%ASSERT(SIZE(CTA) == 136, MSG='SIZE(CTA) == 136')
    CALL UT%ASSERT(CTA%VEC(114)%STR == '^', MSG="CTA%VEC(114)%STR == '^'")
    ! 
    CALL CTA%INSERT(1,"POS1")
    CALL UT%ASSERT(SIZE(CTA) == 137, MSG='SIZE(CTA) == 137')
    CALL UT%ASSERT(CTA%VEC(1)%STR == 'POS1', MSG="CTA%VEC(1)%STR == 'POS1'")
    CALL UT%ASSERT(CTA%VEC(115)%STR == '^',  MSG="CTA%VEC(115)%STR == '^'")
    ! 
    CALL CTA%INSERT(12,"POS12")
    CALL UT%ASSERT(SIZE(CTA) == 138, MSG='SIZE(CTA) == 138')
    CALL UT%ASSERT(CTA%VEC(12)%STR == 'POS12', MSG="CTA%VEC(12)%STR == 'POS12'")
    CALL UT%ASSERT(CTA%VEC(116)%STR == '^',  MSG="CTA%VEC(116)%STR == '^'")
    ! 
    CALL CTA%INSERT(10000,"EOF")
    CALL UT%ASSERT(SIZE(CTA) == 139, MSG='SIZE(CTA) == 139')
    CALL UT%ASSERT(CTA%VEC(116)%STR == '^',  MSG="CTA%VEC(116)%STR == '^'")
    CALL UT%ASSERT(CTA%VEC(139)%STR == 'EOF', MSG="CTA%VEC(139)%STR == 'EOF'")
    ! 
  END BLOCK
  CONTINUE

      !PROCEDURE, PASS(CH) :: INIT    => INITIALIZE_CHARACTER_TYPE_ARRAY  ! %INIT([DIM])
      !PROCEDURE, PASS(CH) :: GET     => GET_CHARACTER_TYPE_ARRAY         ! %GET(POS)
      !GENERIC ::             POP     => REMOVE_LAST_POSITION,     &      ! %POP([LINE])      -> LINE holds popped string
      !                                  REMOVE_SPECIFIED_POSITION        ! %POP(POS, [LINE])
      !PROCEDURE, PASS(CH) :: RESET   => RESET_CHARACTER_TYPE_ARRAY
      !GENERIC ::             ADD     => ADD_CHARACTER_TYPE_ARRAY,     &      ! %ADD(LINE)
      !                                  ADD_DBL_CHARACTER_TYPE_ARRAY, &      ! %ADD(DVAL, PAD)  
      !                                  ADD_INT_CHARACTER_TYPE_ARRAY         ! %ADD(IVAL, PAD, ZPAD)
      !GENERIC ::          ADD_UNIQUE => ADD_UNIQUE_CHARACTER_TYPE_ARRAY,    &! %ADD_UNIQUE(STR, IGNORE_CASE)
      !                                  ADD_UNIQUE_INT_CHARACTER_TYPE_ARRAY,&! %ADD_UNIQUE(VAL, PAD, ZPAD)
      !                                  ADD_UNIQUE_DBL_CHARACTER_TYPE_ARRAY  ! %ADD_UNIQUE(VAL, PAD)
      !GENERIC ::              INSERT => INSERT_CHARACTER_TYPE_ARRAY,    &    ! %INSERT(POS, STR) 
      !                                  INSERT_INT_CHARACTER_TYPE_ARRAY,&    ! %INSERT(POS, VAL, PAD, ZPAD) 
      !                                  INSERT_DBL_CHARACTER_TYPE_ARRAY      ! %INSERT(POS, VAL, PAD)
      !PROCEDURE, PASS(CH) :: SIZE    => SIZE_CHARACTER_TYPE_ARRAY            ! %SIZE()
      !PROCEDURE, PASS(CH) :: MAX_LEN => MAX_LEN_CHARACTER_TYPE_ARRAY         ! %MAX_LEN()
      !PROCEDURE, PASS(CH) :: FIND    => FIND_CHARACTER_TYPE_ARRAY            ! %FIND(STR)
      !GENERIC ::             COUNT   => COUNT_CHARACTER_TYPE_ARRAY, &        ! %COUNT(STR)
      !                                  COUNT_NOCASE_CHARACTER_TYPE_ARRAY    ! %COUNT(STR,IGNORE_CASE)
      !PROCEDURE, PASS(CH) :: DESTROY => DEALLOCATE_CHARACTER_TYPE_ARRAY
    
    !
    !CALL UT%ASSERT(TEST, MSG='')
  !    
  !##########################################################################################################################
  !##########################################################################################################################
  !##########################################################################################################################
  !
  CALL UT%NEXT_TEST("CHARACTER_BUF_TYPE")
  !
  !BLOCK
  !  !
  !  !CALL UT%ASSERT(TEST, MSG='')
  !  !
  !END BLOCK
  !    
  !##########################################################################################################################
  !##########################################################################################################################
  !##########################################################################################################################
  !
  CALL UT%NEXT_TEST("CHARACTER_ARRAY")
  !
  !BLOCK
  !  
  !  !
  !  
  !  !
  !  !CALL UT%ASSERT(TEST, MSG='')
  !  !
  !END BLOCK
  !!    
  !!##########################################################################################################################
  !!##########################################################################################################################
  !!##########################################################################################################################
  !!
  !CALL UT%NEXT_TEST("INTEGER_VECTOR")
  !!
  !BLOCK
  !  
  !  !
  !  
  !  !
  !  !CALL UT%ASSERT(TEST, MSG='')
  !  !
  !END BLOCK
  !!    
  !!##########################################################################################################################
  !!##########################################################################################################################
  !!##########################################################################################################################
  !!
  !CALL UT%NEXT_TEST("DOUBLE_VECTOR")
  !!
  !BLOCK
  !  
  !  !
  !  
  !  !
  !  !CALL UT%ASSERT(TEST, MSG='')
  !  !
  !END BLOCK
  !!    
  !!##########################################################################################################################
  !!##########################################################################################################################
  !!##########################################################################################################################
  !!
  !!CALL UT%NEXT_TEST("LOGICAL_VECTOR")
  !!
  !BLOCK
  !  
  !  !
  !  
  !  !
  !  !CALL UT%ASSERT(TEST, MSG='')
  !  !
  !END BLOCK
  !!    
  !!##########################################################################################################################
  !!##########################################################################################################################
  !!##########################################################################################################################
  !!
  !!CALL UT%NEXT_TEST("INTEGER_MATRIX")
  !!
  !BLOCK
  !  
  !  !
  !  
  !  !
  !  CALL UT%ASSERT(TEST, MSG='')
  !  !
  !END BLOCK
  !!    
  !!##########################################################################################################################
  !!##########################################################################################################################
  !!##########################################################################################################################
  !!
  !CALL UT%NEXT_TEST("DOUBLE_MATRIX")
  !!
  !BLOCK
  !  
  !  !
  !  
  !  !
  !  CALL UT%ASSERT(TEST, MSG='')
  !  !
  !END BLOCK
  !!    
  !!##########################################################################################################################
  !!##########################################################################################################################
  !!##########################################################################################################################
  !!
  !CALL UT%NEXT_TEST("COMPRESSED_VALUE_STORAGE")
  !!
  !BLOCK
  !  
  !  !
  !  
  !  !
  !  CALL UT%ASSERT(TEST, MSG='')
  !  !
  !END BLOCK
  !!    
  !!##########################################################################################################################
  !!##########################################################################################################################
  !!##########################################################################################################################
  !!
  !CALL UT%NEXT_TEST("COMPRESSED_LOCATION_STORAGE")
  !!
  !BLOCK
  !  
  !  !
  !  
  !  !
  !  CALL UT%ASSERT(TEST, MSG='')
  !  !
  !END BLOCK
  !!    
  !!##########################################################################################################################
  !!##########################################################################################################################
  !!##########################################################################################################################
  !!
  !CALL UT%NEXT_TEST("RAT_VOL_TYPE")
  !!
  !BLOCK
  !  
  !  !
  !  
  !  !
  !  CALL UT%ASSERT(TEST, MSG='')
  !  !
  !END BLOCK
  !!    
  !!##########################################################################################################################
  !!##########################################################################################################################
  !!##########################################################################################################################
  !!
  !CALL UT%NEXT_TEST("RAT_VOL_BASE")
  !!
  !BLOCK
  !  
  !  !
  !  
  !  !
  !  CALL UT%ASSERT(TEST, MSG='')
  !  !
  !END BLOCK
  !!    
  !!##########################################################################################################################
  !!##########################################################################################################################
  !!##########################################################################################################################
  !!
  !CALL UT%NEXT_TEST("ID_VAL_TYPE")
  !!
  !BLOCK
  !  
  !  !
  !  
  !  !
  !  CALL UT%ASSERT(TEST, MSG='')
  !  !
  !END BLOCK
  !!    
  !!##########################################################################################################################
  !!##########################################################################################################################
  !!##########################################################################################################################
  !!
  !CALL UT%NEXT_TEST("ID_RAT_VOL_TYPE")
  !!
  !BLOCK
  !  
  !  !
  !  
  !  !
  !  CALL UT%ASSERT(TEST, MSG='')
  !  !
  !END BLOCK
  !!    
  !!##########################################################################################################################
  !!##########################################################################################################################
  !!##########################################################################################################################
  !!
  !CALL UT%NEXT_TEST("ID1_ID2_RAT_VOL_TYPE")
  !!
  !BLOCK
  !  
  !  !
  !  
  !  !
  !  CALL UT%ASSERT(TEST, MSG='')
  !  !
  !END BLOCK
  !!
  !BLOCK
  !  
  !  !
  !  
  !  !
  !  CALL UT%ASSERT(TEST, MSG='')
  !  !
  !END BLOCK
  !    
  !##########################################################################################################################
  !##########################################################################################################################
  !##########################################################################################################################
  !
  !
  !CALL JOIN_CHAR()
  !
END SUBROUTINE