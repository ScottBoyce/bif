# Changelog

## Batteries Included Fortran Library (BiF-Lib)



## Code Citation

Boyce, S.E., 2022, Batteries Included Fortran Library (BiF-Lib), version 1.1.0: U.S. Geological Survey Software Release, https://doi.org/10.5066/P9K2IQ6Y



## Additional Citations

Part of these utilities were developed for the following project:

Boyce, S.E., 2022, MODFLOW One-Water Hydrologic Flow Model (MF-OWHM) Conjunctive Use and Integrated Hydrologic Flow Modeling Software, version 2.x: U.S. Geological Survey Software Release, https://doi.org/10.5066/P9P8I8GS

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

- `src/io/file_io_interface.f90` data type `UNIT_ARRAY_BUILDER` added the `PRINT_STR()` type bound routine.
  - This routine is identical to `UNIT_ARRAY_BUILDER%PRINT(IOUT)`, except that it returns a `character(*)` rather than writing to a file.


&nbsp; 

#### Bug Fixes

- `src/math_numbers/descriptive_statistics.f90`

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

- `src/strings/parse_word_interface.f90` replaced `" "` with the `CONSTANT` module parameter `BLNK`

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