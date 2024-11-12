![Batteries_Included](./img/Batteries_Included_Icon_300.jpeg)    

# BiF-lib Source Code Dependency Tree

&nbsp; 

- Landing Page: &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; https://www.usgs.gov/software/batteries-included-fortran-library-bif-lib
- Source Repository: &nbsp; &nbsp; https://code.usgs.gov/fortran/bif

&nbsp; 

------

[[_TOC_]]     

------

&nbsp; 

### adjacency_list_instruction_and_shortest_path.f90



| Dep  | adjacency_list_instruction_and_shortest_path.f90 |
| :--: | ------------------------------------------------ |
|  1   | constants.f90                                    |
|  2   | alloc_interface.f90                              |
|  3   | binary_heap_instruction.f90                      |
|  4   | num2str_interface.f90                            |
|  5   | sort_interface_driver.f90 and sort submodules    |
|  6   | error_interface.f90                              |
|  7   | warning_type_instruction.f90                     |




------



### alloc_interface.f90

*No Dependency*	




------



### array_data_types_instruction.f90

*No Dependency*	




------



### binary_heap_instruction.f90

*No Dependency*




------



### binomial_interface.f90

*No Dependency*




------


### buffered_reader_instruction.f90

| Dep  | buffered_reader_instruction.f90 |
| :--: | ------------------------------- |
|  1   | constants.f90                   |
|  2   | rolling_pointer_instruction.f90 |




------



### calendar_functions.f90

*No Dependency*




------



### cast_to_string_interface.f90

*No Dependency*




------



### circular_queue_instruction.f90

*No Dependency*




------



### console_commander.f90

*No Dependency*




------



### constants.f90

*No Dependency*	




------



### cycling_text_file_interface.f90

| Dep  | cycling_text_file_interface.f90  |
| :--: | -------------------------------- |
|  1   | constants.f90                    |
|  2   | array_data_types_instruction.f90 |
|  3   | num2str_interface.f90            |
|  4   | parse_word_interface.f90         |
|  5   | error_interface.f90              |
|  6   | generic_open_interface.fpp       |
|  7   | file_io_interface.f90            |
|  8   | string_routines.f90              |




------



### date_operator_instruction.f90

| Dep  | date_operator_instruction.f90 |
| :--: | ----------------------------- |
|  1   | calendar_functions.f90        |




------



### descriptive_statistics.f90

*No Dependency*	




------



### directory_iso_c_binding.f90

*No Dependency*	




------



### dynamic_array.f90

| Dep  | dynamic_array.f90       |
| :--: | ----------------------- |
|  1   | dynamic_array_int32.f90 |




------



### dynamic_array_int32.f90

*No Dependency*	




------



### EquationParser.f90

| Dep  | EquationParser.f90            |
| :--: | ----------------------------- |
|  1   | constants.f90                 |
|  2   | calendar_functions.f90        |
|  3   | date_operator_instruction.f90 |




------



### error_interface.f90

| Dep  | error_interface.f90   |
| :--: | --------------------- |
|  1   | constants.f90         |
|  2   | num2str_interface.f90 |




------



### factorial_interface.f90

*No Dependency*	




------



### file_incrementer_interface.f90

| Dep  | file_incrementer_interface.f90   |
| :--: | -------------------------------- |
|  1   | constants.f90                    |
|  2   | array_data_types_instruction.f90 |
|  3   | path_interface.f90               |
|  4   | generic_open_interface.fpp       |




------



### file_info_interface.f90

*No Dependency*




------



### file_io_interface.f90

| Dep  | file_io_interface.f90            |
| :--: | -------------------------------- |
|  1   | constants.f90                    |
|  2   | array_data_types_instruction.f90 |
|  3   | path_interface.f90               |
|  4   | num2str_interface.f90            |
|  5   | parse_word_interface.f90         |
|  6   | error_interface.f90              |
|  7   | generic_open_interface.fpp       |




------



### generic_block_reader_instruction.f90

| Dep  | generic_block_reader_instruction.f90 |
| :--: | ------------------------------------ |
|  1   | constants.f90                        |
|  2   | array_data_types_instruction.f90     |
|  3   | path_interface.f90                   |
|  3   | linked_list_instruction.f90          |
|  4   | num2str_interface.f90                |
|  5   | parse_word_interface.f90             |
|  6   | error_interface.f90                  |
|  7   | post_key_sub.f90                     |
|  8   | generic_open_interface.fpp           |
|  9   | file_io_interface.f90                |
|  10  | string_routines.f90                  |
|  11  | generic_input_file_instruction.f90   |




------



### generic_input_file_instruction.f90

| Dep  | generic_input_file_instruction.f90 |
| :--: | ---------------------------------- |
|  1   | constants.f90                      |
|  2   | array_data_types_instruction.f90   |
|  3   | num2str_interface.f90              |
|  4   | parse_word_interface.f90           |
|  5   | error_interface.f90                |
|  6   | post_key_sub.f90                   |
|  7   | generic_open_interface.fpp         |
|  8   | file_io_interface.f90              |
|  9   | string_routines.f90                |




------



### generic_open_interface.fpp

| Dep  | generic_open_interface.fpp       |
| :--: | -------------------------------- |
|  1   | constants.f90                    |
|  2   | array_data_types_instruction.f90 |
|  3   | path_interface.f90               |




------



### generic_output_file_instruction.f90

| Dep  | generic_output_file_instruction.f90 |
| :--: | ----------------------------------- |
|  1   | constants.f90                       |
|  2   | array_data_types_instruction.f90    |
|  3   | num2str_interface.f90               |
|  4   | parse_word_interface.f90            |
|  5   | error_interface.f90                 |
|  6   | post_key_sub.f90                    |
|  7   | generic_open_interface.fpp          |
|  8   | file_incrementer_interface.f90      |
|  9   | file_io_interface.f90               |
|  10  | string_routines.f90                 |



------



### hash_table_instruction.f90

| Dep  | hash_table_instruction.f90 |
| :--: | -------------------------- |
|  1   | constants.f90              |
|  2   | prime_finder.f90           |



------



###  hexadecimal_instruction.f90

*No Dependency*



------



### integer_array_builder.f90

*No Dependency*




------



###  integer_queue_instruction.f90

*No Dependency*


------



### is_ascii_interface.f90

| Dep  | is_ascii_interface.f90 |
| :--: | ---------------------- |
|  1   | constants.f90          |
|  2   | num2str_interface.f90  |
|  3   | error_interface.f90    |




------



###  is_integer_interface.f90

*No Dependency*




------



### is_routine_interface.f90

| Dep  | is_routine_interface.f90 |
| :--: | ------------------------ |
|  1   | constants.f90            |




------



###  isqrt_interface.f90

*No Dependency*




------



### IXJ_instruction.f90

| Dep  | IXJ_instruction.f90                  |
| :--: | ------------------------------------ |
|  1   | constants.f90                        |
|  2   | alloc_interface.f90                  |
|  3   | array_data_types_instruction.f90     |
|  4   | path_interface.f90                   |
|  5   | linked_list_instruction.f90          |
|  6   | num2str_interface.f90                |
|  7   | parse_word_interface.f90             |
|  8   | error_interface.f90                  |
|  9   | post_key_sub.f90                     |
|  10  | generic_open_interface.fpp           |
|  11  | file_io_interface.f90                |
|  12  | string_routines.f90                  |
|  13  | generic_input_file_instruction.f90   |
|  14  | generic_block_reader_instruction.f90 |




------



### line_writer_interface.f90

| Dep  | line_writer_interface.f90        |
| :--: | -------------------------------- |
|  1   | constants.f90                    |
|  2   | array_data_types_instruction.f90 |
|  3   | num2str_interface.f90            |




------



### linked_list_instruction.f90

*No Dependency*




------



### list_array_input_interface.f90

| Dep  | list_array_input_interface.f90                |
| :--: | --------------------------------------------- |
|  1   | constants.f90                                 |
|  2   | alloc_interface.f90                           |
|  3   | array_data_types_instruction.f90              |
|  4   | path_interface.f90                            |
|  5   | calendar_functions.f90                        |
|  6   | is_routine_interface.f90                      |
|  7   | linked_list_instruction.f90                   |
|  8   | num2str_interface.f90                         |
|  9   | parse_word_interface.f90                      |
|  10  | set_array_interface.f90                       |
|  11  | sort_interface_driver.f90 and sort submodules |
|  12  | date_operator_instruction.f90                 |
|  13  | error_interface.f90                           |
|  14  | post_key_sub.f90                              |
|  15  | generic_open_interface.fpp                    |
|  16  | file_io_interface.f90                         |
|  17  | string_routines.f90                           |
|  18  | generic_input_file_instruction.f90            |
|  19  | generic_block_reader_instruction.f90          |
|  20  | IXJ_instruction.f90                           |
|  21  | lookup_table_instruction.f90                  |
|  22  | time_series_file_instruction.f90              |
|  23  | uload_and_sfac_interface.f90                  |
|  24  | transient_file_reader_instruction.f90         |




------



### log2_interface.f90

*No Dependency*




------



### lookup_table_instruction.f90

| Dep  | lookup_table_instruction.f90                  |
| :--: | --------------------------------------------- |
|  1   | constants.f90                                 |
|  2   | array_data_types_instruction.f90              |
|  3   | linked_list_instruction.f90                   |
|  4   | num2str_interface.f90                         |
|  5   | parse_word_interface.f90                      |
|  6   | sort_interface_driver.f90 and sort submodules |
|  7   | error_interface.f90                           |
|  8   | post_key_sub.f90                              |
|  9   | generic_open_interface.fpp                    |
|  10  | file_io_interface.f90                         |
|  11  | string_routines.f90                           |
|  12  | generic_input_file_instruction.f90            |
|  13  | generic_block_reader_instruction.f90          |




------



### name_id_interface.f90

*No Dependency*




------



### num2str_interface.f90

*No Dependency*




------



### number_conversion_interface.f90

*No Dependency*




------


### obs_group_interpolator.f90

| Dep  | obs_group_interpolator.f90       |
| :--: | -------------------------------- |
|  1   | constants.f90                    |
|  2   | calendar_functions.f90           |
|  3   | xy_grid_coordinate_interface.f90 |
|  4   | date_operator_instruction.f90    |




------



### parse_word_interface.f90

*No Dependency*




------



### path_interface.f90

| Dep  | path_interface.f90               |
| :--: | -------------------------------- |
|  1   | constants.f90                    |
|  2   | array_data_types_instruction.f90 |




------



### position_interface.f90

| Dep  | position_interface.f90           |
| :--: | -------------------------------- |
|  1   | constants.f90                    |
|  2   | array_data_types_instruction.f90 |


------



### post_key_sub.f90

| Dep  | post_key_sub.f90         |
| :--: | ------------------------ |
|  1   | constants.f90            |
|  2   | num2str_interface.f90    |
|  3   | parse_word_interface.f90 |
|  4   | error_interface.f90      |




------



### power_interface.f90

*No Dependency*



------



### prime_finder.f90

*No Dependency*	



------



### random_routines_interface.f90

*No Dependency*



------



### relax_interface.f90

*No Dependency*



------



### rolling_pointer_instruction.f90

*No Dependency*



------



### same_memory_address_interface.f90

*No Dependency*



------



### secant_interface.f90

*No Dependency*



------


### set_array_interface.f90

*No Dependency*



------



### file_info_interface.f90

*No Dependency*




------



### simple_file_io.f90

*No Dependency*




------



### sort_interface_ascii.f90

| Dep  | sort_interface_ascii.f90  |
| ---- | ------------------------- |
| 1    | sort_interface_driver.f90 |



------

### sort_interface_driver.f90

Not really dependencies, but rather it contains interfaces that describe module procedures in the following files.

| Dep  | sort_interface_driver.f90 |
| ---- | ------------------------- |
| 1    | sort_interface_ascii.f90  |
| 2    | sort_interface_int32.f90  |
| 3    | sort_interface_int64.f90  |
| 4    | sort_interface_multi.f90  |
| 5    | sort_interface_rel32.f90  |
| 6    | sort_interface_rel64.f90  |
| 7    | sort_interface_wild.f90   |



------



### sort_interface_int32.f90

| Dep  | sort_interface_int32.f90  |
| ---- | ------------------------- |
| 1    | sort_interface_driver.f90 |



------



### sort_interface_int64.f90

| Dep  | sort_interface_int64.f90  |
| ---- | ------------------------- |
| 1    | sort_interface_driver.f90 |



------



### sort_interface_multi.f90

| Dep  | sort_interface_multi.f90  |
| ---- | ------------------------- |
| 1    | sort_interface_driver.f90 |



------



### sort_interface_rel32.f90

| Dep  | sort_interface_rel32.f90  |
| ---- | ------------------------- |
| 1    | sort_interface_driver.f90 |



------



### sort_interface_rel64.f90

| Dep  | sort_interface_rel64.f90  |
| ---- | ------------------------- |
| 1    | sort_interface_driver.f90 |



------




###  sort_interface_wild.f90

| Dep  | sort_interface_wild.f90   |
| ---- | ------------------------- |
| 1    | sort_interface_driver.f90 |



------



### string_routines.f90

| Dep  | string_routines.f90              |
| :--: | -------------------------------- |
|  1   | constants.f90                    |
|  2   | array_data_types_instruction.f90 |
|  3   | path_interface.f90               |
|  3   | num2str_interface.f90            |
|  4   | parse_word_interface.f90         |
|  5   | error_interface.f90              |
|  6   | generic_open_interface.fpp       |
|  7   | file_io_interface.f90            |



------



### sub_block_input_interface.f90

| Dep  | sub_block_input_interface.f90                 |
| :--: | --------------------------------------------- |
|  1   | constants.f90                                 |
|  2   | alloc_interface.f90                           |
|  3   | array_data_types_instruction.f90              |
|  4   | path_interface.f90                            |
|  5   | calendar_functions.f90                        |
|  6   | is_routine_interface.f90                      |
|  7   | linked_list_instruction.f90                   |
|  8   | num2str_interface.f90                         |
|  9   | parse_word_interface.f90                      |
|  10  | set_array_interface.f90                       |
|  11  | sort_interface_driver.f90 and sort submodules |
|  12  | date_operator_instruction.f90                 |
|  13  | error_interface.f90                           |
|  14  | post_key_sub.f90                              |
|  15  | generic_open_interface.fpp                    |
|  16  | file_io_interface.f90                         |
|  17  | string_routines.f90                           |
|  18  | generic_input_file_instruction.f90            |
|  19  | generic_block_reader_instruction.f90          |
|  20  | IXJ_instruction.f90                           |
|  21  | lookup_table_instruction.f90                  |
|  22  | time_series_file_instruction.f90              |
|  23  | uload_and_sfac_interface.f90                  |
|  24  | transient_file_reader_instruction.f90         |



------



### system_call.f90

*No Dependency*



------



### time_series_file_instruction.f90

| Dep  | time_series_file_instruction.f90     |
| :--: | ------------------------------------ |
|  1   | constants.f90                        |
|  2   | array_data_types_instruction.f90     |
|  3   | path_interface.f90                   |
|  3   | calendar_functions.f90               |
|  4   | linked_list_instruction.f90          |
|  5   | num2str_interface.f90                |
|  6   | parse_word_interface.f90             |
|  7   | date_operator_instruction.f90        |
|  8   | error_interface.f90                  |
|  9   | post_key_sub.f90                     |
|  10  | generic_open_interface.fpp           |
|  11  | file_io_interface.f90                |
|  12  | string_routines.f90                  |
|  13  | generic_input_file_instruction.f90   |
|  14  | generic_block_reader_instruction.f90 |




------



### timer_instruction.f90

*No Dependency*



------



### transient_file_reader_instruction.f90

| Dep  | transient_file_reader_instruction.f90         |
| :--: | --------------------------------------------- |
|  1   | constants.f90                                 |
|  2   | alloc_interface.f90                           |
|  3   | array_data_types_instruction.f90              |
|  4   | path_interface.f90                            |
|  5   | calendar_functions.f90                        |
|  6   | is_routine_interface.f90                      |
|  7   | linked_list_instruction.f90                   |
|  8   | num2str_interface.f90                         |
|  9   | parse_word_interface.f90                      |
|  10  | set_array_interface.f90                       |
|  11  | sort_interface_driver.f90 and sort submodules |
|  12  | date_operator_instruction.f90                 |
|  13  | error_interface.f90                           |
|  14  | post_key_sub.f90                              |
|  15  | generic_open_interface.fpp                    |
|  16  | file_io_interface.f90                         |
|  17  | string_routines.f90                           |
|  18  | generic_input_file_instruction.f90            |
|  19  | generic_block_reader_instruction.f90          |
|  20  | IXJ_instruction.f90                           |
|  21  | lookup_table_instruction.f90                  |
|  22  | time_series_file_instruction.f90              |
|  23  | uload_and_sfac_interface.f90                  |



------



### uload_and_sfac_interface.f90

| Dep  | uload_and_sfac_interface.f90                  |
| :--: | --------------------------------------------- |
|  1   | constants.f90                                 |
|  2   | alloc_interface.f90                           |
|  3   | array_data_types_instruction.f90              |
|  4   | path_interface.f90                            |
|  5   | calendar_functions.f90                        |
|  6   | is_routine_interface.f90                      |
|  7   | linked_list_instruction.f90                   |
|  8   | num2str_interface.f90                         |
|  9   | parse_word_interface.f90                      |
|  10  | set_array_interface.f90                       |
|  11  | sort_interface_driver.f90 and sort submodules |
|  12  | date_operator_instruction.f90                 |
|  13  | error_interface.f90                           |
|  14  | post_key_sub.f90                              |
|  15  | generic_open_interface.fpp                    |
|  16  | file_io_interface.f90                         |
|  17  | string_routines.f90                           |
|  18  | generic_input_file_instruction.f90            |
|  19  | generic_block_reader_instruction.f90          |
|  20  | IXJ_instruction.f90                           |
|  21  | lookup_table_instruction.f90                  |
|  22  | time_series_file_instruction.f90              |



------



### unicode_interface.f90

*No Dependency*



------



### unit_testing_instruction.f90

*No Dependency*



------



### util_interface.f90

| Dep  | util_interface.f90               |
| :--: | -------------------------------- |
|  1   | constants.f90                    |
|  2   | array_data_types_instruction.f90 |



------



### variable_pointer_list_interface.f90

| Dep  | variable_pointer_list_interface.f90 |
| ---- | ----------------------------------- |
| 1    | constants.f90                       |



------



### warning_type_instruction.f90

| Dep  | warning_type_instruction.f90 |
| :--: | ---------------------------- |
|  1   | constants.f90                |
|  2   | num2str_interface.f90        |
|  3   | error_interface.f90          |



------



### write_array_interface.f90

| Dep  | write_array_interface.f90 |
| :--: | ------------------------- |
|  1   | num2str_interface.f90     |



------


### xy_grid_coordinate_interface.f90

*No Dependency*

