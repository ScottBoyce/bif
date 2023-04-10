# Changelog

## Batteries Included Fortran Library (BiF-Lib)



## Code Citation

Boyce, S.E., 2023, Batteries Included Fortran Library (BiF-Lib), version 1.2.0: U.S. Geological Survey Software Release, https://doi.org/10.5066/P9K2IQ6Y



## Additional Citations

Part of these utilities were developed for the following project:

Boyce, S.E., 2023, MODFLOW One-Water Hydrologic Flow Model (MF-OWHM) Conjunctive Use and Integrated Hydrologic Flow Modeling Software, version 2.x: U.S. Geological Survey Software Release, https://doi.org/10.5066/P9P8I8GS

Boyce, S.E., Hanson, R.T., Ferguson, I., Schmid, W., Henson, W., Reimann, T., Mehl, S.M., and Earll, M.M., 2020, One-Water Hydrologic Flow Model: A MODFLOW based conjunctive-use simulation software: U.S. Geological Survey Techniques and Methods 6–A60, 435 p., [https://doi.org/10.3133/tm6A60](https://doi.org/10.3133/tm6A60)



### Useful Links

**[USGS Git Respository](https://code.usgs.gov/fortran/bif)**  &nbsp; &nbsp; &nbsp; &nbsp; **[Published Documentation](README.md#Publications-Involving-BiF-Lib)**&nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; **[This Repository Branch's README.md](README.md)**

## Contents

------

[[_TOC_]]     

------

&nbsp; 

## 1.2.0

TBA

Header TBA  
Header TBA  

#### Features

- Circular Queue Data Type added in `src/types_and_containers/circular_queue_instruction.f90`
  - This type is a linked list whose end points to the start to form a circle list.

  - Currently only supports `integer(int64) numbers`.

- `EXECUTE_COMMAND_GET_OUTPUT`  added to `system/system_call.f90 ` (new file).  
  This procedure executes a command using the operating system shell and returns the output from the command.
  - `SUBROUTINE EXECUTE_COMMAND_GET_OUTPUT(command, output, stat, keep_empty_lines)`   
    `character(*),                            intent(in ) :: command`  
    `character(*), dimension(:), allocatable, intent(out) :: output`  
    `integer,                       optional, intent(out) :: stat`  
    `logical,                       optional, intent(in ) :: keep_empty_lines` 
  - where  
    `command` - Command passed to terminal to be executed  
    `output` - Output generated from command, dimension is set to the number of non-empty/blank lines returned  
    `stat` - Status generated from command. If not present and error occurs, invokes `ERROR STOP`. If present, `OUTPUT` contains error message  
    `keep_empty_lines` - If present, and TRUE, then empty/blank lines are included in `OUTPUT`

- `src/system/path_interface.f90` added additional procedures: and `SUBROUTINE SET_TO_CWD(CWD, LENGTH)`
  - `FUNCTION GET_CWD()` returns the current working directory.
  -  `SUBROUTINE SET_TO_CWD(CWD, LENGTH)` sets the variable `CHARACTER(*):: CWD` to the current working directory and optional integer variable, `LENGTH`, as the size of the character array. 
    - Pretty much this routine just does `CWD = GET_CWD()` and `LENGTH = LEN(GET_CWD())`
    - Note it is possible to have, `LENGTH > LEN(CWD)`, that case `CWD` does not hold the entire path.
  - `SUBROUTINE GET_FILE_EXTENSION(FILE_NAME, EXT)` given a file name returns the extension (anything after last `.`)
    - For example: `"doc/myfile.txt"` would return `"txt"`
- `src/io/generic_open_interface.fpp` subroutine `GENERIC_OPEN() ` added the optional argument `POSITION`
  - All Fortran `OPEN(POSITION=)` strings are accepted.  
    However it is recommended to only use:
    - `POSITION="APPEND"`
    - `POSITION="ASIS"`
    - `POSITION="REWIND"`
  - If not specified, then it is set to `POSITION="REWIND"`.
- `src/io/file_io_interface.f90` data type `UNIT_ARRAY_BUILDER` added the `FUNCTION PRINT_STR()` type bound routine.
  - This routine is identical to `SUBROUTINE UNIT_ARRAY_BUILDER%PRINT(IOUT)`, except that it returns a `character(*)` rather than writing to a file.
  - The function call is: `str = UNIT_ARRAY_BUILDER%PRINT_STR()`
- `src/io/post_key_sub.f90` added the optional argument `NO_WARN` to `SUBROUTINE CHECK_FOR_POST_KEY`. 
  - If present, and set to `.TRUE.` will disable any warning messages when searching for a post-keyword option. 

  - This optional argument  is also added:
    - `src/io/generic_input_file_instruction.f90` in the routine `GENERIC_INPUT%OPEN`  

    - `src/io/generic_output_file_instruction.f90` in the routine `GENERIC_OUTPUT%OPEN`
- Added test for `MODULE GENERIC_INPUT_FILE_INSTRUCTION`
- `src/strings/num2str_interface.f90` added check for 0.0;  
  that is: `NUM2STR(0.0)` returns `'0.0'` instead of `'0.00000000E+00'`
- All code was shifted to be within the first 132 columns to be compatible with gfortran.
  - Fortran 95 free format standard limits the file width to 132 columns.

  - Note, code still fails to compile with gfortran because it does not support the destructor statement `FINAL::`



&nbsp; 

#### Bug Fixes

- `src/datetime/calendar_functions.f90` 
  uses the function `julianday_to_date(jdn_in, year_in, day, month, year, jdn, leap)` for converting Julian Day of the Year (`jdn_in`) to a calendar month, day, and year. However if `jdn_in > 364`, then the routine would only check if year_in is a leap year rather than each interval of Julian days overlapped a leap year. This has been fixed to account the appropriate 365 and 366 day years.

- `src/input_reader/uload_and_sfac_interface.f90` when loading a  `GENERIC_INPUT_FILE`  variable with `SUBROUTINE ULOAD_SCALAR` that has a specified unit number (`IU /= 0`) reads an extra line from `IN`.
  
  - This effected `MODULE LIST_ARRAY_INPUT_INTERFACE` when using `TYPE GENERIC_LINE_INPUT` that reads a transient file reader (TFR) with the `DATAFILE` or `DATAUNIT` keyword (bypass TFR and read input directly). The resulting call to the `MODULE TRANSIENT_FILE_READER_INSTRUCTION` reads a line to check for `SFAC`, but since `IU` is specified the extra read of the line is not needed because not file is parsed.
  
- `src/system/directory_iso_c_binding.f90`  `FUNCTION MAKE_C_CHAR` takes a Fortran character type and converts it to a C-string. The function was modified to return a character array (correct method) instead of CHARACTER(*). 

  - That is, the function previously returned:  
    &nbsp; &nbsp; &nbsp; `CHARACTER(len=LEN(F_STR)+1, kind=C_CHAR)`  
    now returns:  
    &nbsp; &nbsp; &nbsp; `CHARACTER(len=1, kind=C_CHAR), DIMENSION(LEN(F_STR)+1)`

- `C_LOC` requires argument that has either the `TARGET` or `POINTER` attribute but does NOT support `ALLOCATABLE` arrays. Routines that make use of `C_LOC` had the TARGET attribute added to incoming arrays. Also `FUNCTION GET_C_LOC` was added that adds to an array the `TARGET` attribute and returns its `C_LOC`. This effected the following files:

  - `src/util_misc/same_memory_address_interface.f90`  
    `src/dynamic_arrays/dynamic_array_int32.f90`

- `src/math_numbers/number_conversion_interface.f90` 

  - `MODULE NUMBER_CONVERSION_INTERFACE` converts all the major integer types from/to binary to decimal, octal, or hexadecimal. A set of parameters were added to ensure that zero and one comparisons are made with the same type integer as that of the routine. That is:
    - converting INT8 to Octal uses only INT8 operations

    - converting INT16 to Octal uses only INT16 operations

    - converting INT32 to Octal uses only INT32 operations

    - converting INT64 to Octal uses only INT64 operations

- `makefile` added gfortran complier flag fno-range-check to prevent compiler errors.

- Sort routines removed redundant use of `USE ISO_FORTRAN_ENV`

  - `MODULE SORT_INTERFACE` contains a set of `SUBMODULE`s that sort each of the major Fortran data types. The problem is that a `SUBMODULE` imports all the global variables from the parent `MODULE`. The parent module has:  
    `USE, INTRINSIC:: ISO_FORTRAN_ENV, ONLY: INT8, INT32, INT64, REL32=>REAL32, REL64=>REAL64`  
    so that declaration is not necessary in the submodules.

- Added `int(  )` around BOZ constants (binary, octal, hexadecimal constants) to conform to Fortran standard.

  - For example, fortran standard requires the binary notation `b''` to be converted to the desired integer type:  
    `int(b'1100000010000000', int32)`

- Added decimal to literal real numbers in a variaty of files. For example, changes `1D0` to `1.D0`

  - `src/datetime/calendar_functions.f90`  
    `src/datetime/date_operator_instruction.f90`  
    `src/math_numbers/EquationParser.f90`  
    `src/math_numbers/log2_interface.f90`  
    
    `src/math_numbers/secant_interface.f90`  
    `src/spatial/xy_grid_coordinate_interface.f90`  
    `src/types_and_containers/array_data_types_instruction.f90`  
    `src/types_and_containers/linked_list_instruction.f90`  
    `src/unit_test/unit_testing_instruction.f90`  
    `tests/src/main_tests.f90`

- `src/math_numbers/descriptive_statistics.f90`   
  changed the intent for the `online_stats%merge` routine from `intent(in)` to `intent(inout)`

  - This routine was generic for `MERGE_OUT_SG_qp(st, st1, st2)` and changed:   
    ``` fortran
    SUBROUTINE MERGE_OUT_SG_qp(st, st1, st2)
      CLASS(ONLINE_STATS), intent(in):: st
    ```
    to   
    ``` fortran
    SUBROUTINE MERGE_OUT_SG_qp(st, st1, st2)
      CLASS(ONLINE_STATS), intent(inout):: st
    ```

&nbsp; 

#### Refactoring

- Rewrite of `GENERIC_INPUT_FILE_INSTRUCTION` and `GENERIC_OUTPUT_FILE_INSTRUCTION` modules to have consistent interfaces and code structure.
  - **BREAKING CHANGE**: This changed the order of optional arguments in `%OPEN()` for the `GENERIC_INPUT_FILE` and `GENERIC_OUTPUT_FILE` data types. However, they know use the same arguments, and argument order, in their common type bound procedures. This also changed some of the components in the data types to be the same name.  In particular:
    - The attribute `%NULL_FILE` replaced `%SKIP`

    - `%IU == 0` only indicates implied internal, and added `%IS_INTERNAL` to indicate if file is internal and `%IU` is set the internal file unit.

    - `%IS_OPEN` is added and set to True if file is open and `IU` is connected to a file, and False otherwise.

- `src/io/generic_input_file_instruction.f90` and `src/io/generic_output_file_instruction.f90` improved error messages.

- `src/strings/parse_word_interface.f90` replaced `" "` with the `CONSTANT` module parameter `BLNK`

- `src/strings/string_routines.f90` embedded the `PARSE_WORD` subroutine to increase the likelihood that the compiler will inline it for faster execution. This also removes the dependency on the `PARSE_WORD_INTERFACE` module.

- `src/input_reader/sub_block_input_interface.f90` removed unused module imports.

- `src/input_reader/uload_and_sfac_interface.f90` changed `.ne.` to `/=` for clarity.

- `src/input_reader/uload_and_sfac_interface.f90` switched to `get_word` routine to replace using the following routines: `comment_index()`, `parse_word()` and `upper()`.

- Convert all capital case error messages to sentence case.

- Change all odd number check code from using IAND to BTEST.
  - The code previously used IAND to check if a number is odd with the following code:  
    &nbsp; &nbsp; &nbsp; `IAND( num, 1 ) == 1`  
    This bit folding only checks to see if the least significant bit is set, which indicates the number is odd.

  - Rather than doing two operations, the code was changed to:  
    &nbsp; &nbsp; &nbsp; `BTEST( num, 0 )`  
    which returns TRUE if the lest significant bit is set.

- Remove the integer *type-spec* from do *concurrent-header* for all source files that use it.

  - The gfortran compiler does not support the DO CONCURRENT *type-spec* in the *concurrent-header*. This has been removed to improve compatibility with gfortran. The following is an example of changed code.

    - That is,  
      &nbsp; &nbsp; &nbsp; `do concurrent ( integer:: i = 1 : 10 )`  
      is changed to:   
      &nbsp; &nbsp; &nbsp; `integer:: i`  
      &nbsp; &nbsp; &nbsp; `do concurrent (i = 1 : 10 )`


&nbsp; 

------

## 1.1.0

2022-2-09

Major release  
Includes a new module `WRITE_ARRAY_INTERFACE` that writes to a file 1D and 2D arrays.  
This also includes a substantial improvement to the `NUM2STR_INTERFACE` module.

#### Features

- `src/strings/num2str_interface.f90` 
  - now supports the Fortran data types:  `INT8`, `INT16`, `INT32`, `INT64`, and `REAL32`, `REAL64`  

- `src/io/generic_input_file_instruction.f90` and  
  `src/io/generic_output_file_instruction.f90` now offer nearly the same `%open` interface.
  - All features that can be applied to both are now present in both.

- `src/io/write_array_interface.f90` added.
  - `MODULE WRITE_ARRAY_INTERFACE` provides the generic subroutine `WRITE_ARRAY`.
  - Supports writing of the `INT8`, `INT16`, `INT32`, `INT64`, and `REAL32`, `REAL64`  
    to a unit number of file name.
    - The array is written with the first dimension being along the rows.  
      For example, a `DIMENSION(3,5)` array would write an array with 3 columns that occupy 5 lines of text.  
      &nbsp; 

  - `DIMENSION(:)`, 1D, array interface  
    `WRITE_ARRAY(IU,   ARR,         [WIDTH, FMT, SEP, TRANSPOSE, HED, SEP_ON_LAST, ADVANCE])`  
    `WRITE_ARRAY(FNAME, ARR, ERROR, [WIDTH, FMT, SEP, TRANSPOSE, HED, SEP_ON_LAST, ADVANCE, IU, NO_CLOSE])`  
    &nbsp; 

  - `DIMENSION(:,:)`, 2D, array interface  
    `WRITE_ARRAY(IU,   ARR,         [WIDTH, FMT, SEP, TRANSPOSE, HED, SEP_ON_LAST])`  
    `WRITE_ARRAY(FNAME, ARR, ERROR, [WIDTH, FMT, SEP, TRANSPOSE, HED, SEP_ON_LAST,          IU, NO_CLOSE])`  
    &nbsp; 

  - where:

    - `ARR` is the 1D or 2D array that is written to a file.
    - `character(*):: FNAME`  
      is a file name write the array to.   
      If the file exists, it is over written.  
      If the file is already opened, then it is appended to.
    - `integer:: IU`  
      is the unit number connected to an open file to write to.  
      If `FNAME` and `IU` are provided, then `IU` is the unit number to open the file on.  
      If `FNAME` is provided and `IU=0`, then `IU` is returned with the `NEWUNIT` used to open the file.
    - `logical:: ERROR`  
      is set to `.TRUE.` if there is a problem opening the file `FNAME`, otherwise it is set to `.FALSE.`.
    - `integer, optional:: WIDTH `  
      Specify the minimum width when writing each number.  
      For example, `WIDTH=12` will contain one number for every 12 characters.  
      If the number exceeds the width, then it will occupy the minimal space  
      to represent the number, plus 1 blank space afterwards.
    - `character(*), optional:: FMT`  
      Specify a Fortran format edit descriptor kernel.  
      If specified, then `WIDTH` is ignored.  
      Each number is written using the format plus a 1 space buffer (`1x`) between numbers.  
      If you want to disable the 1 space buffer you must specify `SEP=""`  
      &nbsp;  
      The format kernel is a Fortran format code for a single number (does not contain the repeat count).  
      For example, the format codes are used to transfer integer values `I` is `nIw.m`  
      This would not include the repeat count (`n`) and  
      can optionally include the width (`w`) and zero padding (`m`).  
      This format, must match the array type (that is, `I` for integer and `F`, `E`, `EN`, `ES`, or `G` for real).  
      For example, to have real number output of width 8 and 3 decimal places, then `FMT = 'F8.3'` 
    - `character(*), optional:: SEP`  
      If present that the character appended to each number, except for the last one.  
      For example, `SEP="A b"` would print the numbers 2, 4, 6 as "`2A b4A b6`" and  
       `SEP=", "` would print "`2, 4, 6`".
      If `SEP` and `FMT` are specified, then `SEP` is the separator between numbers and not the 1 space buffer.
    - `logical, optional:: TRANPOSE`  
      If present and true, then the written array is tranposed.  
      &nbsp;  
      Example 2D, an array with `dimension(3,5)` with:  
      `.false.` - writes an array with 3 columns that occupy 5 lines of text (default).  
      `.true .` - writes an array with 5 columns that occupy 3 lines of text.  
      &nbsp;  
      Example 1D, an array with `dimension(3)` with:  
      `.false.` - writes an array with 3 values along a single line of text (default).  
      `.true .` - writes an array with 1 value per line, occupying a total of 3 lines of text.
    - `character(*), optional:: HED`  
      If present, then it is written to the file before the actual array (a header).
    - `logical, optional:: SEP_ON_LAST`  
      If present, set to `.true. ` and `SEP` is defined, then the separator is added to the last number on each line.  
      For example, `SEP=", "` and `SEP_ON_LAST=.true.` would print the numbers 2, 4, 6 as "`2, 4, 6, `"
    - `logical, optional:: ADVANCE`  
      If present, and true, the a 1D array will not include a carriage return (new line) after the numbers are written.
    - `logical, optional:: NO_CLOSE`  
      If present, and true, then the file that is opened is not closed when the routine exits. The unit number 


* `src/system/path_interface.f90` function `IS_WINDOWS()` is optimized to store the if the OS is Windows or not with the SAVE attribute. This makes it so that the `OS` environmental variable is checked once. Previously, each time the function was called would query the operating system environmental variable. A new option, `always_check` is added to force the query the operating system environmental variable even if it has already been set.

- `src/types_and_containers/array_data_types_instruction.f90` subroutine `SET_ID_VAL_TYPE_POS(TYP, POS, ID, VAL, INUL, NUL)` added the optional parameters `INUL` and `NUL`.
  - If `POS` is greater than the size of `ID_VAL_TYPE`, which is `N`, then it is expanded to include `POS` and the `ID` is set to `INUL` and `VAL` set to `NUL` for the values located between `N+1` and `POS-1` (inclusive).

* `src/io/post_key_sub.f90` added the routine `file_and_post_key_parse`.
  
  * This routine given a `line` and starting position (`lloc`) returns the `line(istart:istop)` that contains the file directive that is used by generic input/output and any post-keywords. The return value also sets `lloc = istop + 1`. 
  * This is useful if an input line contains multiple keywords in addition to a *ULOAD* or *Generic Input* or *Generic Output*.
  * Note, this assumes that **SF** is used for any inline scale factors,  
    such as `./input.txt  SF 0.3048`  
    and the multiline scale factor, **SFAC**, is not used.
  * For example, if an input expected 2 integers, 1 output file, and 2 integers,  
    then the input line might look like this:  
    `2  4  ./output/results.txt  BUF 64  SPLIT 1024 6 8`  
    and given a `lloc=6` (before the `./`) would set `line(istart:istop)` to:  
    `./output/results.txt  BUF 64  SPLIT 1024`  
    and set `lloc=47`, which is just before the `6 8`

&nbsp; 

#### Bug Fixes

- `src/strings/num2str_interface.f90`  added a decimal place for real literals.
  - For example `1e5`, is treated by the compiler as the integer `100000` rather than real.
  - To fix this, the number must be written as `1.e5` 
- `src/math_numbers/random_routines_interface.f90` fixed spelling errors.

&nbsp; 

------

## 1.0.2

2022-1-15

Minor release

#### Features

- `src/types_and_containers/linked_list_instruction.f90` added a `REAL64_LINKED_LIST` type.
  - This is a clone of `INTEGER_LINKED_LIST`, but instead constructs a list of `REAL64` floating point numbers.

* `src/system/path_interface.f90` function `IS_WINDOWS()` is optimized to store the if the OS is Windows or not with the SAVE attribute. This makes it so that the `OS` environmental variable is checked once. Previously, each time the function was called would query the operating system environmental variable. A new option, `always_check` is added to force the query the operating system environmental variable even if it has already been set.

- `src/types_and_containers/array_data_types_instruction.f90` subroutine `SET_ID_VAL_TYPE_POS(TYP, POS, ID, VAL, INUL, NUL)` added the optional parameters `INUL` and `NUL`.
  - If `POS` is greater than the size of `ID_VAL_TYPE`, which is `N`, then it is expanded to include `POS` and the `ID` is set to `INUL` and `VAL` set to `NUL` for the values located between `N+1` and `POS-1` (inclusive).

* `src/io/post_key_sub.f90` added the routine `file_and_post_key_parse`.
  * This routine given a `line` and starting position (`lloc`) returns the `line(istart:istop)` that contains the file directive that is used by generic input/output and any post-keywords. The return value also sets `lloc = istop + 1`. 
  * This is useful if an input line contains multiple keywords in addition to a *ULOAD* or *Generic Input* or *Generic Output*.
  * Note, this assumes that **SF** is used for any inline scale factors,  
    such as `./input.txt  SF 0.3048`  
    and the multiline scale factor, **SFAC**, is not used.
  * For example, if an input expected 2 integers, 1 output file, and 2 integers,  
    then the input line might look like this:  
    `2  4  ./output/results.txt  BUF 64  SPLIT 1024 6 8`  
    and given a `lloc=6` (before the `./`) would set `line(istart:istop)` to:  
    `./output/results.txt  BUF 64  SPLIT 1024`  
    and set `lloc=47`, which is just before the `6 8`

&nbsp; 

#### Bug Fixes

- `src/error/error_interface.f90` minor spelling corrections.

&nbsp; 

------

## 1.0.1

2021-4-14

Minor release

#### Features

- `src/io/post_key_sub.f90`  routine `check_for_post_key` added the optional logical input `only_check`  
  If present and true, then only moves LLOC past the post-keywords and does not process them.

&nbsp; 

#### Bug Fixes

- `sort` minor optimization by not sorting arrays of size 1.

* `sort` routines now always initialize the optional permutation vector (`P`) if it is included in the subroutine call. 
  Previously, if the array to be sorted was already sorted, then  `P` was not initialized. 
  
- `sort` 2D array routines now correctly handle sorting when one of the dimensions is 1.
  That is, an array of `dimension(X,1)` or `dimension(1,X)` where `X ≥ 1`.
  Previously, sort would pass the array to a 1D sort routine without checking if the array should be sorted or the dimension to sort on.
  
* `unit_testing_instruction.f90` file fixed the`ASSERT_UNIT_TEST_0D_NONZERO_XYZ` routines that specified `WANT` as being logical rather than the corresponding `XYZ` type being checked against.
  
- `src/datetime/date_operator_instruction.f90` fixed format error for `date_operator%str_dyear()`

&nbsp; 

#### Refactoring

Minor refactoring that converted `.NE.` to `/=` and indentation for:

- `src/io/generic_input_file_instruction.f90` 
* `src/io/generic_open_interface.fpp` 
- `src/math_numbers/EquationParser.f90` 
* `src/strings/parse_word_interface.f90`

&nbsp; 

------

## 1.0.0

2020-10-28

Initial Release.