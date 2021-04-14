# Changelog and Release Notes

## Batteries Included Fortran Library (BiF-Lib)



## Code Citation

Boyce, S.E., 2021, Batteries Included Fortran Library (BiF-Lib), version 1.0.1: U.S. Geological Survey Software Release, https://doi.org/10.5066/xyz



## Additional Citations

Part of these utilities were developed for the following project:

Boyce, S.E., 2021, MODFLOW One-Water Hydrologic Flow Model (MF-OWHM) Conjunctive Use and Integrated Hydrologic Flow Modeling Software, version 2.0.x: U.S. Geological Survey Software Release, https://doi.org/10.5066/P9P8I8GS

Boyce, S.E., Hanson, R.T., Ferguson, I., Schmid, W., Henson, W., Reimann, T., Mehl, S.M., and Earll, M.M., 2020, One-Water Hydrologic Flow Model: A MODFLOW based conjunctive-use simulation software: U.S. Geological Survey Techniques and Methods 6–A60, 435 p., [https://doi.org/10.3133/tm6A60](https://doi.org/10.3133/tm6A60)



### Useful Links

**[USGS Git Respository](https://code.usgs.gov/fortran/bif)**  &nbsp; &nbsp; &nbsp; &nbsp; **[Published Documentation](README.md#Publications-Involving-BiF-Lib)**&nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; **[This Repository Branch's README.md](README.md)**

## Contents

------

[[_TOC_]]     

------



## 1.0.1

2021-4-14

Minor release

#### Features

- `src/io/post_key_sub.f90`  routine `check_for_post_key` added the optional logical input `only_check`  
  If present and true, then only moves LLOC past the post-keywords and does not process them.

  

#### Bug Fixes

- `sort` minor optimization but not sorting arrays of size 1.

- `sort` routines now always initialize the optional permutation vector (`P`) if it is included in the subroutine call. 
  Previously, if the array to be sorted was already sorted, then  `P` was not initialized. 
  
- `sort` 2D array routines now correctly handle sorting when one of the dimensions is 1.
  That is, an array of `dimension(X,1)` or `dimension(1,X)` where `X ≥ 1`.
  Previously, sort would pass the array to a 1D sort routine without checking if the array should be sorted or the dimension to sort on.
  
- `unit_testing_instruction.f90` file fixed the`ASSERT_UNIT_TEST_0D_NONZERO_XYZ` routines that specified `WANT` as being logical rather than the corresponding `XYZ` type being checked against.
  
- `src/datetime/date_operator_instruction.f90` fixed format error for `date_operator%str_dyear()`

  

#### Refactoring

Minor refactoring that converted `.NE.` to `/=` and indentation for:

- `src/io/generic_input_file_instruction.f90` 
- `src/io/generic_open_interface.fpp` 
- `src/math_numbers/EquationParser.f90` 
- `src/strings/parse_word_interface.f90`


## 1.0.0

2020-10-28

Initial Release.