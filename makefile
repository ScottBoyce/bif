#
# MAKEFILE for compiling and testing the Batteries Included Fortran Library (BiF-Lib)
#
# Developed by Scott E Boyce <seboyce@usgs.gov>
#
# If you use BiF-lib, this MAKEFILE, a derivative of this makefile 
#   please include in any publications the following citations:
#
#    Boyce, S.E., Hanson, R.T., Ferguson, I., Schmid, W., Henson, W., Reimann, T., Mehl, S.M., and Earll, M.M., 2020, One-Water Hydrologic Flow Model: A MODFLOW based conjunctive-use simulation software: U.S. Geological Survey Techniques and Methods 6–A60, 435 p., https://doi.org/10.3133/tm6A60
#
#    Boyce, S.E., 2022, Batteries Included Fortran Library (BiF-Lib), version 1.1.0: U.S. Geological Survey Software Release, https://doi.org/10.5066/xyz
#
#    Boyce, S.E., 2022, MODFLOW One-Water Hydrologic Flow Model (MF-OWHM) Conjunctive Use and Integrated Hydrologic Flow Modeling Software, version 2.2.x: U.S. Geological Survey Software Release, https://doi.org/10.5066/P9P8I8GS
#
# PROVIDED AS IS WITHOUT WARRANTY OR HELP.
#
# See next section to set the compiler type variables
#   The initial setup has the compiler type variables set to the 
#   Intel Fortran Compiler Classic (ifort)
#
# This makefile contains the options for compiling BiF using Intel, GFortran, and LLVM. 
#   It is recommended to use the Intel Fortran Compiler Classic for compiling BiF.
#   gfortran is unlikely to successfully compile do to the variability of versions 
#   included with linux or windows and several known compiler bugs present in multiple versions.
#   At some point, BiF may be refactored to work around the known compiler bugs.
#   The LLVM compilers, FLANG and CLANG, are still experimental and not yet fully supported.
#
# To Invoke/Compile type from command prompt:
# make                  Compile the src files and the main.f90 that is used for testing. Binary located in bin
# make run              Same as above, but runs the resulting test binary. By default compiles with main.f90, but you can change to different tests by: makefile run MAIN=DriverFile.f90
# make clean            Delete all object (.o) and module (.mod) files --> Note this uses the Unix find command and not Windows
# make reset            Same as clean, except deletes all binaries.    --> Note this uses the Unix find command and not Windows
#
# To define a different driver main file you either edit this makefile or specify the following argument
# make MAIN=main.f90     #where main.f90 is replaced with your driver file.
#
# Specify debug or release to run without modifying the makefile:
# make CONFIG=debug       or
# make CONFIG=release     or
# make run CONFIG=debug   or
# 
# make COMPILER=gcc F90=gfortran       or
# make COMPILER=gcc F90=gfortran CONFIG=debug
#
# Accepted keywords are (Note that all have defaults within this script):
#     CONFIG    => debug or release
#     COMPILER  => GCC or INTEL or LLVM  --> Indicates the compiler collection used for auto setting compiler flags
#     F90       => gfortran or ifort
#     CC        => gcc or icc
#
#     bin_out   => Location and Name (plus extension) of the final program. If not specified, then bin_out = $(bin_dir)/$(PROGRAM)$(ext)
#     src_dir   => Location of the source files                      -- Do not include a trailing / (that is, ./out1/out2)
#     int_dir   => Location of the intermediate files (.o and .mod)  -- Do not include a trailing / (that is, ./out1/out2)
#     test_dir  => Location to run program when using "make run"     -- Do not include a trailing / (that is, ./out1/out2)
#
#     ARG       => Arguments passed to program when using "make run"
#     PROGRAM   => Name file program without extension (note variable is ignored if bin_out is set).
#     bin_dir   => Location to place final program (note variable is ignored if bin_out is set).      -- Do not include a trailing / (that is, ./out1/out2)
#
#   --note all these variables should have detailed explanations where they are defined in this makefile.
#   --bin_out overrides the default binary name with the user specified one, that is: $(F90) -o $(bin_out)
#
# If you want to use Intel Fortran on Windows 10 
#    then run the makefile in the Intel Command Prompt (for example, run from the start menu: Compiler 19.1 Update 1 for Intel 64 Visual Studio 2019 environment)
#    or you will get path or license errors from Intel.
#    -- Linux Intel Fortran works fine with this makefile.
#
#         
#############################################################################
#############################################################################
###   Set the following Variables                                         ###
#############################################################################
#############################################################################
#
# Compilation Configuration-Optimization Scheme
#   ===> Accepted Answers: RELEASE, DEBUG
CONFIG := debug
#
# Compilation Software                        --> Indicates the compiler collection used for setting compiler flags
#   ===> Accepted Answers: INTEL, GCC, LLVM       --> LLVM not fully-supported yet
COMPILER := INTEL
#
# Define the PROGRAM MAIN file location
#
MAIN := ./tests/src/array_data_types_test.f90 \
        ./tests/src/sort_test.f90             \
        ./tests/src/main_tests.f90            \
        ./tests/src/main.f90                 
#
#
# Define the Fortran Compiler
#                    ===> For example: gfortran, gfortran-9, gfortran-10, ifort
#                         ****Note that the version of your Fortran compiler may not support all of the Fortran Standards (viz 2003, 2008, 2015)
F90 := ifort
#
# Define the C Compiler
#   ===> Accepted Answers: gcc, icc
CC  := icc
#
# Program Name - Do not include extension (eg .exe). Also _debug will automatically be added if CONFIG = debug. Use bin_out= to specify exact location and name for binary.
#
PROGRAM := bif
#
# Compile STATICALLY? That is, no library dependence and compile with STATIC options.
#   ===> Accepted Answers: YES, NO
STATIC := YES
#
# Should compilation force DOUBLE PRECISION for all REAL variables? That is: REAL => DOUBLE PRECIONS.
# ===> Accepted Answers: YES, NO
#
DBLE := NO
#
# Pass Command Arguments when running executable.
#  This is only used for "make run", for example "make run ARG=Name.nam"
ARG = 
#
#
########################################################################
########################################################################
#             DO NOT MODIFY BELOW THIS LINE
#            UNLESS YOU KNOW WHAT YOUR DOING
########################################################################
########################################################################
#
########################################################################
#
#
#Define final BIN, SOURCE, and Testing directories => Can be blank, but do not include the trailing forward slash /
#
bin_dir ?=./bin
#
src_dir ?=./src
#
# Location where test files are run
test_dir ?=./examples
#
# define the intermediate directory object and mod files are placed here
#
obj_dir :=$(strip $(shell echo $(CONFIG) | tr A-Z a-z))_$(strip $(shell echo $(F90) | tr A-Z a-z))
int_dir ?=./obj/$(obj_dir)
#int_dir:= ./obj
#
TESTDIR := ./tests/src
#
#############################################################################
#
# Construct a function for converting space delimited variables to new lines

null :=
sp := ${null} ${null}
sp2 := ${null}  ${null}
sp4 := ${null}    ${null}
#${sp} := ${sp}    # ${sp}  function returns a space
#${sp2} := ${sp2}  # ${sp2} function returns 2 spaces

#funct -> @echo -e " $(subst ${sp},\n,${text})"
#
#
#############################################################################
#
# Define the Fortran source files.
#
main_src:= \
           $(src_dir)/util_misc/constants.f90                                  \
           $(src_dir)/datetime/calendar_functions.f90                          \
           $(src_dir)/datetime/sleep_interface.f90                             \
           $(src_dir)/datetime/timer_instruction.f90                           \
           $(src_dir)/dynamic_arrays/dynamic_array_int32.f90                   \
           $(src_dir)/math_numbers/descriptive_statistics.f90                  \
           $(src_dir)/math_numbers/hexadecimal_instruction.f90                 \
           $(src_dir)/math_numbers/isqrt_interface.f90                         \
           $(src_dir)/math_numbers/log2_interface.f90                          \
           $(src_dir)/math_numbers/number_conversion_interface.f90             \
           $(src_dir)/math_numbers/power_interface.f90                         \
           $(src_dir)/math_numbers/prime_finder.f90                            \
           $(src_dir)/math_numbers/random_routines_interface.f90               \
           $(src_dir)/math_numbers/relax_interface.f90                         \
           $(src_dir)/math_numbers/secant_interface.f90                        \
           $(src_dir)/spatial/xy_grid_coordinate_interface.f90                 \
           $(src_dir)/strings/cast_to_string_interface.f90                     \
           $(src_dir)/strings/num2str_interface.f90                            \
           $(src_dir)/strings/parse_word_interface.f90                         \
           $(src_dir)/system/console_commander.f90                             \
           $(src_dir)/system/directory_iso_c_binding.f90                       \
           $(src_dir)/types_and_containers/array_data_types_instruction.f90    \
           $(src_dir)/types_and_containers/binary_heap_instruction.f90         \
           $(src_dir)/types_and_containers/integer_array_builder.f90           \
           $(src_dir)/types_and_containers/circular_queue_instruction.f90      \
           $(src_dir)/types_and_containers/integer_queue_instruction.f90       \
           $(src_dir)/types_and_containers/linked_list_instruction.f90         \
           $(src_dir)/types_and_containers/name_id_interface.f90               \
           $(src_dir)/types_and_containers/rolling_pointer_instruction.f90     \
           $(src_dir)/types_and_containers/variable_pointer_list_interface.f90 \
           $(src_dir)/unicode/unicode_interface.f90                            \
           $(src_dir)/unit_test/unit_testing_instruction.f90                   \
           $(src_dir)/util_misc/alloc_interface.f90                            \
           $(src_dir)/util_misc/is_routine_interface.f90                       \
           $(src_dir)/util_misc/same_memory_address_interface.f90              \
           $(src_dir)/util_misc/set_array_interface.f90                        \
           $(src_dir)/datetime/date_operator_instruction.f90                   \
           $(src_dir)/dynamic_arrays/dynamic_array.f90                         \
           $(src_dir)/error/error_interface.f90                                \
           $(src_dir)/input_reader/buffered_reader_instruction.f90             \
           $(src_dir)/io/post_key_sub.f90                                      \
           $(src_dir)/sort/sort_interface_driver.f90                           \
           $(src_dir)/sort/sort_interface_ascii.f90                            \
           $(src_dir)/sort/sort_interface_int32.f90                            \
           $(src_dir)/sort/sort_interface_int64.f90                            \
           $(src_dir)/sort/sort_interface_multi.f90                            \
           $(src_dir)/sort/sort_interface_rel32.f90                            \
           $(src_dir)/sort/sort_interface_rel64.f90                            \
           $(src_dir)/sort/sort_interface_wild.f90                             \
           $(src_dir)/strings/line_writer_interface.f90                        \
           $(src_dir)/system/path_interface.f90                                \
           $(src_dir)/types_and_containers/hash_table_instruction.f90          \
           $(src_dir)/util_misc/position_interface.f90                         \
           $(src_dir)/util_misc/util_interface.f90                             \
           $(src_dir)/error/warning_type_instruction.f90                       \
           $(src_dir)/io/generic_open_interface.fpp                            \
           $(src_dir)/math_numbers/EquationParser.f90                          \
           $(src_dir)/spatial/obs_group_interpolator.f90                       \
           $(src_dir)/strings/is_ascii_interface.f90                           \
           $(src_dir)/io/file_incrementer_interface.f90                        \
           $(src_dir)/io/file_io_interface.f90                                 \
           $(src_dir)/spatial/adjacency_list_instruction_and_shortest_path.f90 \
           $(src_dir)/strings/string_routines.f90                              \
           $(src_dir)/io/cycling_text_file_interface.f90                       \
           $(src_dir)/io/generic_input_file_instruction.f90                    \
           $(src_dir)/io/generic_output_file_instruction.f90                   \
           $(src_dir)/io/write_array_interface.f90                             \
           $(src_dir)/input_reader/generic_block_reader_instruction.f90        \
           $(src_dir)/datetime/time_series_file_instruction.f90                \
           $(src_dir)/types_and_containers/IXJ_instruction.f90                 \
           $(src_dir)/types_and_containers/lookup_table_instruction.f90        \
           $(src_dir)/input_reader/uload_and_sfac_interface.f90                \
           $(src_dir)/input_reader/transient_file_reader_instruction.f90       \
           $(src_dir)/input_reader/list_array_input_interface.f90              \
           $(src_dir)/input_reader/sub_block_input_interface.f90 
#
#############################################################################
#
# Listing of all source files (if multiple variables for specifying source locations)
#
all_src := $(main_src) $(MAIN)
#
#############################################################################
#
#Define the source code directory search path (really only need source directories)
#      $(sort $(dir $(all_src))) gets all the directories that source files reside in 
#                                -? "$(sort" XYZ) removes duplicates
#
VPATH := $(sort $(dir $(all_src)))  $(int_dir)  $(bin_dir)  $(TESTDIR) 
#
#############################################################################
#
# Change all source names with extension changed to ".o"
obj:=$(addsuffix .o, $(basename $(notdir $(all_src))))

# Old method, manually replace each extension by patter matching
#obj:=  $(patsubst               %.f90, %.o,   \
#         $(patsubst             %.f,   %.o,   \
#           $(patsubst           %.fpp, %.o,   \
#             $(patsubst         %.c,   %.o,   \
#               $(patsubst       %.for, %.o,   \
#                 $(patsubst     %.F,   %.o,   \
#                   $(patsubst   %.F90, %.o,   \
#                     $(patsubst %.FOR, %.o,   \
#                            $(notdir $(all_src))  \
#        ) ) ) ) ) ) ) )
#
#obj:= $(patsubst %.f90,%.o,$(patsubst %.f,%.o,$(src)))
#
########################################################################
#
# Check for if Windows or Unix
#
#
ifeq ($(OS),Windows_NT)
    ext:=.exe
else
    ext:=.nix
endif
#
########################################################################
#
# Note bash on windows sometimes calls C:\Windows\System32\find.exe, 
#   but want Bash "find" with no extension
# Check if windows, if so, then find bash find equivalent.
#
ifeq ($(OS),Windows_NT)
    FIND:=$(strip $(shell                                       \
                   (                                            \
                   notSET="T";                                  \
                   for FP in `which --all find`; do             \
                      if [[ $$FP == *Win* ]] ||                 \
                         [[ $$FP == *WIN* ]] ||                 \
                         [[ $$FP == *win* ]];                   \
                         then continue ;                        \
                         else                                   \
                             echo "$$FP";                       \
                             notSET="F";                        \
                             break;  fi;                        \
                   done;                                        \
                   if [ $$notSET = "T" ]; then echo "find"; fi; \
                   FP=;                                         \
                   notSET=;                                     \
                   )                                            \
           ) )
else
    FIND:=find
endif
#
########################################################################
#
# Check for if Windows and set resource file for icon  -> windres ./icon/*.rc  ./icon/*.res
#  
#  --> windres and gfortran can have issues so skipping makefile icon generation
#  
###icon_rc  := ./icon/XYZ.rc
###icon_res := ./icon/XYZ.res
####
###ifeq ($(OS),Windows_NT)
###    res:=$(shell windres $(icon_rc) $(icon_res) || echo )
###	ifeq ($(res), )
###        res:=$(icon_res)
###    else
###        res:=
###    endif
###    
###else
###        res:=
###endif
#
# ------------------------------------
define echo2
            @echo; echo
endef
# ------------------------------------
define echo3
            @echo; echo; echo
endef
# ------------------------------------
define echo4
            @echo; echo; echo; echo
endef
# ------------------------------------
#
#
########################################################################
#
# Remove blanks to ensure checks match to names
#
CFG     :=$(strip $(shell echo $(CONFIG) | tr [:lower:] [:upper:]))
CMPLR   :=$(strip $(shell echo $(COMPILER) | tr a-z A-Z))
STATIC  :=$(strip $(shell echo $(STATIC) | tr a-z A-Z))
DBLE    :=$(strip $(shell echo $(DBLE)   | tr a-z A-Z))
F90     :=$(strip $(F90))
CC      :=$(strip $(CC))
PROGRAM :=$(strip $(PROGRAM))
#
########################################################################
#
# Set up names and optimizations depending on compiler and configuration
#
ifeq ($(CFG), DEBUG)
   #
   PROGRAM:=$(PROGRAM)_debug
   #
   F90FlagsIntel :=-O0 -g -debug -traceback -assume nocc_omp -fpe0 -fp-model source -nologo -warn nousage -check bounds,pointers,stack,format,output_conversion,uninit
   F90FlagsGCC   :=-O0 -g -w -fbacktrace -fdefault-double-8  -ffree-line-length-2048 -fmax-errors=10 -ffpe-trap=zero,overflow,underflow -finit-real=nan #-fstack-usage  #<= THIS PROVIDES LOTS OF INFO   -std=f2008
   F90FlagsLLVM  :=
   #
   CFlagsIntel   :=-O0 -debug -g  -fbuiltin 
   CFlagsGCC     :=-O0        -g  -Wno-int-to-pointer-cast -Wno-pointer-to-int-cast
   CFlagsLLVM    :=
else
   # NOTE "-ip" can sometimes cause catastrophic error: **Internal compiler error:
   #
   F90FlagsIntel :=-O2 -assume nocc_omp -fpe0 -fp-model source -threads -warn nousage -nologo
   F90FlagsGCC   :=-O2 -w -fno-backtrace -fno-range-check -fdefault-double-8 -ffree-line-length-2048
   F90FlagsLLVM  :=
   #
   CFlagsIntel   :=-O2  -fbuiltin
   CFlagsGCC     :=-O2  -Wno-int-to-pointer-cast -Wno-pointer-to-int-cast
   CFlagsLLVM    :=
endif
#
########################################################################
#
# Check if DBLE == YES to add default REAL-8 options
#
ifeq ($(DBLE), YES)
   #
   F90FlagsIntel+= -real-size 64                        
   F90FlagsGCC  += -fdefault-real-8                  
   F90FlagsLLVM += -fdefault-real-8
endif
#
########################################################################
#
# Establish Proper Optimization Flags and Static Flags
#
# Ensure that variables are set if STATIC not in use
ifneq ($(strip $(STATIC)), YES)
  STATIC:=
  STATICLNK:=
endif
#
# STATICLNK+= -static-libgfortran  => This can cause problems with gcc runtime so best not to use
ifeq ($(CMPLR), GCC)
  mod:=-J$(int_dir)
  F90FLAGS:=$(F90FlagsGCC)
  #
  ifdef GDB
       F90FLAGS += -ggdb3
  endif
  #
  ifeq ($(strip $(STATIC)), YES)
    STATIC   :=-static -static-libgfortran -static-libgcc  -static-libstdc++
    STATICLNK:=-static -static-libgfortran -static-libgcc  -static-libstdc++
  endif
  #
  CFLAGS:=$(CFlagsGCC)
endif
#
ifeq ($(CMPLR), INTEL)
  mod:=-module $(int_dir)
  F90FLAGS:=$(F90FlagsIntel)
  ifeq ($(strip $(STATIC)), YES)
    STATIC   :=-static -static-intel -qopenmp-link=static -static-libstdc++ -static-libgcc
    STATICLNK:=-static -static-intel -qopenmp-link=static -static-libstdc++ -static-libgcc
  endif
  #
  CFLAGS:=$(CFlagsINTEL)
endif
#
ifeq ($(CMPLR), LLVM)
  mod:=-module $(int_dir)
  F90FLAGS:=$(F90FlagsLLVM)
  #
  ifdef GDB
       F90FLAGS += -ggdb3
  endif
  #
  ifeq ($(strip $(STATIC)), YES)
    STATIC   :=-static-flang-libs 
    STATICLNK:=-static-flang-libs 
  endif
  #
  CFLAGS:=$(CFlagsLLVM)
endif
#
#
########################################################################
#
# Remove Variable Blank Space For Cleaner Output
#
F90FLAGS :=$(strip $(F90FLAGS))
CFLAGS   :=$(strip $(CFLAGS))
STATIC   :=$(strip $(STATIC))
STATICLNK:=$(strip $(STATICLNK))
#
########################################################################
#
#SET UP PROGRAM NAME
#
bin_out ?= $(bin_dir)/$(PROGRAM)$(ext)
#
###########################################################################
###########################################################################
#     DEFINE ALL TASK FUNCTIONS                                         ###
#     THE FOLLOW TARGETS EVALUATE THE COMPILATION                       ###
###########################################################################
###########################################################################
#
#
all: 
	@$(MAKE) --no-print-directory runMake || $(MAKE) --no-print-directory errorClean
#
run: 
	@$(MAKE) --no-print-directory runTest || $(MAKE) --no-print-directory errorClean
#
rebuild: quietClean all
#
compile: prinLink:=NO
compile: preClean $(int_dir) $(bin_out) completed
#
runMake: prinLink:=YES
runMake: startMSG preClean CompFlags $(int_dir) $(bin_out) dashes completed
#
runTest: runMake
	${echo2}
	@echo "Now running program to evaluate tests."
	${echo2}
	cd $(test_dir) ; $(abspath $(bin_out)) $(ARG)
	${echo3}
	@echo "   MAKEFILE TESTS COMPLETE"
	${echo2}
#
startMSG:
	${echo2}
	@echo "                 $(CFG) COMPILATION"
	@echo
	@echo "                        OF"
	@echo
	@echo "              Batteries Included Fortran"
	${echo2}
#
#@echo "                   $(strip $(shell echo $(PROGRAM) | tr a-z A-Z))"
#
CompFlags:
	@echo "--------------------------------------------------------------------------------"
	@echo 
	@echo "Starting compilation with the following flags: "
	${echo2}
	@echo " $(F90):"
	@printf "    $(subst ${sp},\n${sp4},$(F90FLAGS) $(mod) $(STATIC))"
	${echo3}
	@echo " $(CC):"
	@printf "    $(subst ${sp},\n${sp4},$(CFLAGS) $(STATIC))"
	${echo3}
	@echo "Compiled object, module, and submodule files will be placed in:"
	@echo; echo "    $(realpath $(int_dir))"
	${echo2}
	@echo "================================================================================"
	${echo2}
#
preClean:
	@rm -rf $(bin_out) 
#
$(int_dir):
	@mkdir -p $@
#
$(bin_out): $(addprefix $(int_dir)/,$(obj))
	@echo
	@if [ "$(prinLink)" = "YES" ]; then                                                           \
	   echo                                                                                    ;  \
	   echo "================================================================================" ;  \
	   echo; echo                                                                              ;  \
	   echo "OBJECTS HAVE BEEN CREATED NOW LINKING FINAL BINARY:"                              ;  \
	   echo                                                                                    ;  \
	   echo "                              $(bin_out)"                                         ;  \
	   echo                                                                                    ;  \
	else                                                                                          \
	   echo "NOW LINKING FINAL BINARY"                                                        ;  \
	   echo                                                                                    ;  \
	fi
	@$(F90) $(F90FLAGS) $(mod)   $^   $(STATICLNK)  -o  $@
#
dashes:
	@echo
	@echo "--------------------------------------------------------------------------------"
	${echo2}
        
completed:
	@echo "   MAKEFILE COMPILATION COMPLETE"
	${echo2}
#
#      --> Note this uses the Unix find command and NOT Windows
#          If you get an error on windows, its using the wrong one --> See MinGW or MySYS64)
clean: 
	@echo
	$(FIND) ./obj -not -name 'obj' -name '*.o'    -delete
	$(FIND) ./obj -not -name 'obj' -name '*.mod'  -delete
	$(FIND) ./obj -not -name 'obj' -name '*.smod' -delete
	${echo2}
#
cleanOBJ: 
	@echo
	$(FIND) ./obj -not -name 'obj' -not -name '.keep' -name '*'  -delete
	${echo2}
#
quietClean: 
	@echo
	$(FIND) $(int_dir) -name '*.o'    -delete  2>/dev/null  ||  true
	$(FIND) $(int_dir) -name '*.mod'  -delete  2>/dev/null  ||  true
	$(FIND) $(int_dir) -name '*.smod' -delete  2>/dev/null  ||  true
	${echo2}
#
errorClean: 
	${echo4}
	@echo "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@"
	@echo "@@########################################################@@"
	@echo "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@"
	@echo
	@echo "                   MAKEFILE FAILED"
	@echo
	@echo "             TO MAKE $(CFG) COMPILATION OF"
	@echo
	@echo "               $(bin_out)"
	${echo2}
	rm -rf $(bin_out) 
	${echo2}
#
reset: 
	@echo
	$(FIND) . -name '*.o'    -delete
	$(FIND) . -name '*.mod'  -delete
	$(FIND) . -name '*.smod' -delete
	@echo
	$(FIND) ./obj -not -name 'obj' -not -name '.keep' -name '*'  -delete
	@echo
	$(FIND) $(notdir $(bin_dir)) -not -name '$(notdir $(bin_dir))' -not -name '.keep' -name '*'  -delete
	${echo2}
#
# The following target allows for printing a specific variable.  old method: @echo $*=$($*)
#  make print-CC  --> will print out hte value of $(CC)
#
print-%:
	@echo '$*=$($*)'
	@echo ' origin = $(origin $*)'
	@echo ' flavor = $(flavor $*)'
	@echo ' value = $(value $*)'
#
#################################################################################
###  Object Code Recipes  ######################################################
#################################################################################
#
$(int_dir)/%.o : %.c
	@echo "$(CC)   $(notdir $<)"
	@echo
	@$(CC) $(CFLAGS) $(STATIC)  -c  $<  -o $@
#
$(int_dir)/%.o : %.f
	@echo "$(F90)   $(notdir $<)"
	@echo
	@$(F90) $(F90FLAGS) $(mod) $(STATIC)  -c  $<  -o $@
#
$(int_dir)/%.o : %.F
	@echo "$(F90)   $(notdir $<)"
	@echo
	@$(F90) $(F90FLAGS) $(mod) $(STATIC)  -c  $<  -o $@
#
$(int_dir)/%.o : %.f90
	@echo "$(F90)   $(notdir $<)"
	@echo
	@$(F90) $(F90FLAGS) $(mod) $(STATIC)  -c  $<  -o $@
#
$(int_dir)/%.o : %.F90
	@echo "$(F90)   $(notdir $<)"
	@echo
	@$(F90) $(F90FLAGS) $(mod) $(STATIC)  -c  $<  -o $@
#
$(int_dir)/%.o : %.fpp
	@echo "$(F90)   $(notdir $<)"
	@echo
	@$(F90) $(F90FLAGS) $(mod) $(STATIC)  -c  $<  -o $@
#
$(int_dir)/%.o : %.for
	@echo "$(F90)   $(notdir $<)"
	@echo
	@$(F90) $(F90FLAGS) $(mod) $(STATIC)  -c  $<  -o $@
#

#
###############################################################################
#
# Setup suffixes that will be processes # .h .mod
.SUFFIXES: .o .f .for .F .FOR .f90 .F90 .fpp .c 
#
# #############################################################################
#
# Phony Targets
.PHONY: all run clean cleanOBJ reset runMake runTest preclean errorClean startMSG print-% completed dashes compile
#
# #############################################################################
#
# Suppress echoing of commands
.SILENT: startMSG CompFlags dashes completed print-%
#
#
# THE END #####################################################################


