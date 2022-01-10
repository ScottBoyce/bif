# Changelog and Release Notes

## Batteries Included Fortran Library (BiF-Lib)



## Code Citation

Boyce, S.E., 2022, Batteries Included Fortran Library (BiF-Lib), version 1.0.2: U.S. Geological Survey Software Release, https://doi.org/10.5066/xyz



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

  

#### Bug Fixes

- `src/error/error_interface.f90` minor spelling corrections.


## 1.0.1

2021-4-14

Minor release

#### Features

- `src/io/post_key_sub.f90`  routine `check_for_post_key` added the optional logical input `only_check`  
  If present and true, then only moves LLOC past the post-keywords and does not process them.

  

#### Bug Fixes

- `sort` minor optimization by not sorting arrays of size 1.

* `sort` routines now always initialize the optional permutation vector (`P`) if it is included in the subroutine call. 
  Previously, if the array to be sorted was already sorted, then  `P` was not initialized. 
  
- `sort` 2D array routines now correctly handle sorting when one of the dimensions is 1.
  That is, an array of `dimension(X,1)` or `dimension(1,X)` where `X ≥ 1`.
  Previously, sort would pass the array to a 1D sort routine without checking if the array should be sorted or the dimension to sort on.
  
* `unit_testing_instruction.f90` file fixed the`ASSERT_UNIT_TEST_0D_NONZERO_XYZ` routines that specified `WANT` as being logical rather than the corresponding `XYZ` type being checked against.
  
- `src/datetime/date_operator_instruction.f90` fixed format error for `date_operator%str_dyear()`

  

#### Refactoring

Minor refactoring that converted `.NE.` to `/=` and indentation for:

- `src/io/generic_input_file_instruction.f90` 
* `src/io/generic_open_interface.fpp` 
- `src/math_numbers/EquationParser.f90` 
* `src/strings/parse_word_interface.f90`


## 1.0.0

2020-10-28

Initial Release.