!
! CODE DEVELOPED BY SCOTT E BOYCE 
!                   CONTACT <seboyce@usgs.gov> or <Boyce@engineer.com>
!
! Contains a set of convenience routines for parsing input files 
!    and opening read only or write only files
! 
! The module is self-contained with simplified versions of multiple files 
!    from the full Batteries Included Fortran (BiF) library.
!    
! This file is meant for programs that want a minimal dependency tree,
!    However, it is recomended to use their original counter-parts to have the full BiF options (viz, error reporting and full option set)
!    
! It NOT recomended to use this with any BiF componet that provides the same functionality (or be careful about import conflicts)
!    Such as: file_io_interface.f90
!             num2str_interface.f90
!             parse_word_interface.f90
!             string_routines.f90
!
! If you use this code, please cite it for any publications that use it (change X to appropiate digits):
!    Boyce, S.E., 20XX, Batteries Included Fortran Library (BiF-lib), version X.X: U.S. Geological Survey Software Release, https://doi.org/10.5066/P9K2IQ6Y
!
! MODULE LISTING:
!   simple_file_io
!
!                           SUBROUTINES
!                                    open_read             <= Open a read-only  text file with the NEWUNIT option
!                                    open_write            <= Open a write-only text file with the NEWUNIT option
!                                    open_read_binary      <= Open a read-only  stream-binary file with the NEWUNIT option
!                                    open_write_binary     <= Open a write-only stream-binary file with the NEWUNIT option
!
!                                    read_to_data          <= Store in line the next, uncommented line in file unit. From MODULE FILE_IO_INTERFACE    > SUBROUTINE READ_TO_DATA
!                                                                                                                    
!                                    get_word              <= Parse and return next word in line.                    From MODULE PARSE_WORD_INTERFACE > SUBROUTINE GET_WORD_ALLOC
!                                    get_number            <= Parse next intger, single, or double in line.          From MODULE STRINGS              > SUBROUTINE GET_INTEGER_VAL, SUBROUTINE GET_REAL_VAL, SUBROUTINE GET_DOUBLE_VAL
!
!                                    parse_word            <= Parse next word in line.                               From MODULE PARSE_WORD_INTERFACE > SUBROUTINE PARSE_WORD
!                                    upper                 <= Convert line to upper case.                            From MODULE STRINGS              > SUBROUTINE UPPER
!                                    lower                 <= Convert line to lower case.
!
!                                    get_file_name         <= Return path and file name associated with unit number. From MODULE FILE_IO_INTERFACE    > SUBROUTINE GET_FILE_NAME
!                                    error_stop            <= Stop program with error message (thread friendly)
!                                    
!                           FUNCTIONS
!                                    num2str               <= Conveter integer to character.                         From MODULE NUM2STR_INTERFACE    > FUNCTION INT08_2STR, FUNCTION INT16_2STR, FUNCTION INT32_2STR, FUNCTION INT64_2STR
!                                    max_line_length       <= Return a file's longest line length.                   From MODULE FILE_IO_INTERFACE    > FUNCTION MAX_LINE_LENGTH_UNIT, FUNCTION MAX_LINE_LENGTH_FILE
!
module simple_file_io
  use, intrinsic:: iso_fortran_env, only: int8, int16, int32, int64, real32, real64
  use, intrinsic:: ieee_arithmetic, only: IEEE_VALUE, IEEE_QUIET_NAN, IEEE_POSITIVE_INF, IEEE_NEGATIVE_INF
  implicit none
  !
  private
  !
  public :: num2str               ! str   = num2str(val, [pad])
  !
  public :: open_read             ! call open_read(file, unit, iostat)   -> Same arguments for other opens
  public :: open_write            ! call open_write       
  public :: open_read_binary      ! call open_read_binary 
  public :: open_write_binary     ! call open_write_binary
  !
  public :: read_to_data          ! call read_to_data(line, infile, [error], [output], [com])
  !
  public :: get_word              ! call get_word(line, loc, val, [upcase], [iostat], [istart], [istop]) -> val is char(:), allocatable; otherwise use parse_word
  public :: get_number            ! call get_number(line, loc, val, [iostat], [istart], [istop], [i], [o], [msg])
  !
  public :: parse_word            ! call parse_word(line, loc, istart, istop, com_stop, com)
  public :: upper                 ! call upper(line)
  public :: lower                 ! call lower(line)
  !
  public :: get_file_name         ! call get_file_name(unit, fname, exist) 
  public :: error_stop            ! call error_stop([msg], [unit], [line], [output], [msg2])
  !
  public :: max_line_length       ! mxlen = max_line_length(unit)  ||  mxlen = max_line_length(file_name)
  !
  integer,   parameter :: Z     = 0
  logical,   parameter :: TRUE  = .TRUE.
  logical,   parameter :: FALSE = .FALSE.
  character, parameter :: NL    = new_line(' ')
  character, parameter :: BLNK  = " "
  character, parameter :: TAB   = ACHAR(9)
  character, parameter :: CR    = ACHAR(13)  !Carriage return (Unix ending)
  character, parameter :: LF    = ACHAR(10)  !Line feed (CRLF is windowns ending)
  !
  interface num2str
    module procedure int08_2str   ! str = num2str(val, [pad])
    module procedure int16_2str
    module procedure int32_2str
    module procedure int64_2str
    module procedure real32_2str
    module procedure real64_2str
    module procedure int08_2str_pad
    module procedure int16_2str_pad
    module procedure int32_2str_pad
    module procedure int64_2str_pad
  end interface 
  !
  interface get_number
    module procedure get_integer   ! call get_number(line, loc, val, [iostat], [istart], [istop], [i], [o], [msg])
    module procedure get_real
    module procedure get_double
  end interface
  !
  interface max_line_length
    module procedure max_line_length_file   !(fname) result(mxlen)
    module procedure max_line_length_unit   !(unit ) result(mxlen)
  end interface
  !
  contains
  !
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !
  subroutine read_to_data(line, infile, error, output, com)
    character(*),         intent(inout):: line    !line to load data too
    integer,              intent(in   ):: infile  !unit of file to load line from
    integer,     optional,intent(in   ):: error   !unit to write error message too
    integer,     optional,intent(in   ):: output  !unit to write transcript of what is loaded too
    character(*),optional,intent(in   ):: com     !string of accepted comment symbols (1 byte/char per symbol). 
                                                  !  set to "" to only include empty lines as comments
                                                  !  -- do not include blank spaces with other symbols --
                                                  !  ---- for example these are wrong:  "# !" or " #!" or "#! "  ----
                                                  !  default when not present is com = #
    integer:: i, c, err, n, iout, ierr
    logical:: first, transcribe, pound_com, no_com
    !
    first = true
    !
    if(present(error))then
        ierr=error
    else
        ierr=Z
    end if
    !
    if(present(output))then
        iout=output
    else
        iout=Z
    end if
    !
       pound_com = .not. present(com)
    if(pound_com) then
                  no_com = false
    else
                  no_com = com == ""
    end if
    !
    transcribe = iout /= Z
    n=Z
    read_loop: do
          read(infile,'(a)',iostat=err) line
          if    (err > Z) then                                   !line failed to read, this is most likely due to end of file line,infile,output,msg
                               call error_stop( msg =  &
                                               '"subroutine read_to_data" fatal error'//NL//                                   &
                                               'Recieved a fortran error greater than zero (iostat>0),'//nl//                  &
                                               'while reading in the next line in the file.'//NL//NL//                         &
                                               'This is not an end of file error,'//NL//                                       &
                                               'but a rather a failure to read the next line from the file itself.'//NL//NL//  &
                                               'the guessed previous line that was sucessfully read is:'//NL//                 &
                                               '"'//trim(line)//'"',                                                           &
                                               unit = infile                                                                   &
                                              )
          elseif(err < Z) then !eof
                               line=" "
                               c=1
                               backspace(infile) !note that eof counts of 1 read, but multiple reads to eof will not move any further, so reposition to the end of the file, but not 1 beyond to keep n (the read count) corret 
                               exit read_loop
          end if
          !
          n = n + 1  ! sucessfully read of line, keep track of total line count
          !----------------------------------------------------------------------------------------------------------
          do concurrent (i=1:len_trim(line), line(i:i)==tab .or. line(i:i)==CR .or. line(i:i)==LF)  !tab = char(9) -> fortran treates tab as if it was character--make spaces to improve search --also remove dangling cr or lf  --> note that this is identicaly to "call special_blank_strip(line)"   
              line(i:i)=BLNK
          end do
          !----------------------------------------------------------------------------------------------------------
          if(line(1:1) == BLNK) line = adjustl(line)
          !----------------------------------------------------------------------------------------------------------
          !
          if (pound_com) then
                            c=index(line,'#')-1      ! check for comment symbol  --> same as c = comment_index(line)
          elseif(no_com) then
                            c = -1
          else
                            c=scan(line, com)-1
          end if
          !
          if (c < Z) then
              c=len_trim(line)      ! no # found, so use entire string
              if (c == Z) c=1     ! line is blank, set to 1
          end if
          if (c == Z) c=1         !# is on first column or line is blank, set to 1
          !----------------------------------------------------------------------------------------------------------
          !
          if (pound_com) then
                            if(line(c:c) /= '#' .and. line(1:c) /= " ") exit read_loop   !start of line is not com and all char before com are blank
          elseif(no_com) then
                            if(line(1:c) /= " ") exit read_loop
          else
                            if(index(com, line(c:c)) == Z .and. line(1:c) /= " ") exit read_loop  
          end if
          !
          if(transcribe) then
                if(first) then
                              write(iout,'(/ a)') trim(line)
                              first = false
                else
                              write(iout,'(  a)') trim(line)
                end if
          end if
          !
    end do read_loop
    !
    if(transcribe .and. .not. first) write(iout,'(a)')
    !
  end subroutine
  !
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !
  pure subroutine parse_word(line, loc, istart, istop, com_stop, com)
    ! assumes compiler can handel line(i:i-1) and sets it to blank string
    character(*),          intent(in   ):: line
    integer,               intent(inout):: loc,istart,istop
    logical,    optional,  intent(in   ):: com_stop
    character(*),optional, intent(in   ):: com     !string of accepted comment symbols (1 byte/char per symbol). 
                                                   !  set to "" to only include empty lines as comments
                                                   !  -- do not include blank spaces with other symbols --
                                                   !  ---- for example these are wrong:  "# !" or " #!" or "#! "  ----
                                                   !  default when not present is com = #    integer:: line_len, line_trim, loc0
    integer:: line_len, line_trim, p
    !                      
    line_trim = len_trim(line) + 1
    !
    if(present(com_stop)) then
            if(com_stop .and. line_trim > 1) then
                p = line_trim - 1                         ! Only scan non-blank characters for comment symbol
                !
                if ( .not. present(com) ) then
                    line_len = index(line(:p),'#')     !tmp var to hold # position
                else
                    line_len = scan(line(:p), com)     !tmp var to hold # position
                end if
                if(line_len > 0) line_trim = line_len  !found  # update line_trim to be its length
            end if
    end if
    !
    do while( loc < line_trim )
                              if(line(loc:loc) /= TAB .AND. line(loc:loc) /= BLNK .AND. line(loc:loc) /= ',') exit
                              loc = loc+1
    end do
    !
    if( loc >= line_trim ) then
               line_len = len(line)
        loc   =line_len+1
        istart=line_len
        istop =line_len-1
    else
        if(line(loc:loc)=='"') then
                                loc = loc+1
                                istart = loc
                                do while( loc < line_trim )
                                    if( line(loc:loc) == '"' ) exit
                                    loc = loc+1
                                end do
                                istop = loc-1
                                loc = loc+1
        elseif(line(loc:loc)=="'") then
                                loc = loc+1
                                istart = loc
                                do while( loc < line_trim )
                                    if( line(loc:loc) == "'" ) exit
                                    loc = loc+1
                                end do
                                istop = loc-1
                                loc = loc+1
        else
                                istart = loc
                                loc = loc+1
                                do while( loc < line_trim )
                                    if( line(loc:loc)==TAB .OR. line(loc:loc)==BLNK .OR. line(loc:loc)==',') exit
                                    loc = loc+1
                                end do
                                istop = loc-1
                                if(istop<istart) istop=istart
        end if
    end if
    !
  end subroutine
  !
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !
  subroutine get_integer(line, loc, val, iostat, istart, istop, i, o, msg)
    character(*),           intent(in   ):: line              ! line to parse integer from
    integer,                intent(inout):: loc               ! loc => starting location to find number, number at exit is located at line(istart:istop) and loc = istop+1
    integer,                intent(  out):: val               ! Integer value to return
    integer,      optional, intent(  out):: iostat            ! set to zero if there is no error, otherwise non-zero error code
    integer,      optional, intent(  out):: istart            ! Pointer to start of integer in line  -> line(istart:istop)
    integer,      optional, intent(  out):: istop             ! Pointer to end   of integer in line
    integer,      optional, intent(in   ):: i                 ! Unit number of file that read in line
    integer,      optional, intent(in   ):: o                 ! Unit number to write error messages to
    character(*), optional, intent(in   ):: msg               ! additional message for error message
    integer:: st, sp, ierr
    !
    call parse_word(line, loc, st, sp)
    !
    if(line(st:sp) /= BLNK .and. st <= sp) then
        read(line(st:sp),*, iostat=ierr) val
    else
        ierr = -1
    end if
    !
    if(present(iostat))  then
        iostat = ierr
    elseif(ierr /= Z) then
        call error_stop("get_number error: Failed to convert text to integer number."//NL//NL//    &
                        'The following is the text attempted to be converted "'//line(st:sp)//'".', &
                        i, line, o, msg)
    end if
    !
    if(ierr /= Z) val = HUGE(val)
    !
    if(present(istart)) istart = st
    if(present(istop )) istop  = sp
    !
  end subroutine
  !
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !
  subroutine get_real(line, loc, val, iostat, istart, istop, i, o, msg)
    character(*),           intent(in   ):: line              ! line to parse real from
    integer,                intent(inout):: loc               ! loc => starting location to find number, number at exit is located at line(istart:istop) and loc = istop+1
    real(real32),           intent(  out):: val               ! Real value to return
    integer,      optional, intent(  out):: iostat            ! set to zero if there is no error, otherwise non-zero error code
    integer,      optional, intent(  out):: istart            ! Pointer to start of real in line  -> line(istart:istop)
    integer,      optional, intent(  out):: istop             ! Pointer to end   of real in line
    integer,      optional, intent(in   ):: i                 ! Unit number of file that read in line
    integer,      optional, intent(in   ):: o                 ! Unit number to write error messages to
    character(*), optional, intent(in   ):: msg               ! additional message for error message
    integer:: st, sp, ierr
    !
    call parse_word(line, loc, st, sp)
    !
    if(line(st:sp) /= BLNK .and. st <= sp) then
        ierr = Z
        select case(line(st:sp))
        case('NAN',  'NaN', 'nan'); val = IEEE_VALUE(val, IEEE_QUIET_NAN)
        case(  '-1', '-1.','-1.0'); val = -1.0_real32
        case(   '0',  '0.', '0.0'); val =  0.0_real32
        case(   '1',  '1.', '1.0'); val =  1.0_real32
        case('INF',  'inf', 'Inf',  "Infinity",  "infinity"); val = IEEE_VALUE(val, IEEE_POSITIVE_INF)  !  HUGE(val)
        case('-INF','-inf','-Inf', "-Infinity", "-infinity"); val = IEEE_VALUE(val, IEEE_NEGATIVE_INF)  ! -HUGE(val)
        case DEFAULT
                    read(line(st:sp),*, iostat=ierr) val
        end select
    else
        ierr = -1
    end if
    !
    if(present(iostat))  then
        iostat = ierr
    elseif(ierr /= Z) then
        call error_stop("get_number error: Failed to convert text to real32 float number."//NL//NL//  &
                        'The following is the text attempted to be converted "'//line(st:sp)//'".',   &
                        i, line, o, msg)
    end if
    !
    if(ierr /= Z) val = IEEE_VALUE(val, IEEE_QUIET_NAN)
    !
    if(present(istart)) istart = st
    if(present(istop )) istop  = sp
    !
  end subroutine
  !
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !
  subroutine get_double(line, loc, val, iostat, istart, istop, i, o, msg)
    character(*),           intent(in   ):: line              ! line to parse real from
    integer,                intent(inout):: loc               ! loc => starting location to find number, number at exit is located at line(istart:istop) and loc = istop+1
    real(real64),           intent(  out):: val               ! Real value to return
    integer,      optional, intent(  out):: iostat            ! set to zero if there is no error, otherwise non-zero error code
    integer,      optional, intent(  out):: istart            ! Pointer to start of real in line  -> line(istart:istop)
    integer,      optional, intent(  out):: istop             ! Pointer to end   of real in line
    integer,      optional, intent(in   ):: i                 ! Unit number of file that read in line
    integer,      optional, intent(in   ):: o                 ! Unit number to write error messages to
    character(*), optional, intent(in   ):: msg               ! additional message for error message
    integer:: st, sp, ierr
    !
    call parse_word(line, loc, st, sp)
    !
    if(line(st:sp) /= BLNK .and. st <= sp) then
        ierr = Z
        select case(line(st:sp))
        case('NAN',  'NaN', 'nan'); val = IEEE_VALUE(val, IEEE_QUIET_NAN)
        case(  '-1', '-1.','-1.0'); val = -1.0_real64
        case(   '0',  '0.', '0.0'); val =  0.0_real64
        case(   '1',  '1.', '1.0'); val =  1.0_real64
        case('INF',  'inf', 'Inf',  "Infinity",  "infinity"); val = IEEE_VALUE(val, IEEE_POSITIVE_INF)  !  HUGE(val)
        case('-INF','-inf','-Inf', "-Infinity", "-infinity"); val = IEEE_VALUE(val, IEEE_NEGATIVE_INF)  ! -HUGE(val)
        case DEFAULT
                    read(line(st:sp),*, iostat=ierr) val
        end select
    else
        ierr = -1
    end if
    !
    if(present(iostat))  then
        iostat = ierr
    elseif(ierr /= Z) then
        call error_stop("get_number error: Failed to convert text to real64 float number."//NL//NL//  &
                        'The following is the text attempted to be converted "'//line(st:sp)//'".',   &
                        i, line, o, msg)
    end if
    !
    if(ierr /= Z) val = IEEE_VALUE(val, IEEE_QUIET_NAN)
    !
    if(present(istart)) istart = st
    if(present(istop )) istop  = sp
    !
  end subroutine
  !
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !
  subroutine get_word(line, loc, val, upcase, iostat, istart, istop)
    character(*),              intent(in   ):: line              ! line to parse real from
    integer,                   intent(inout):: loc               ! loc => starting location to find number, number at exit is located at line(istart:istop) and loc = istop+1
    character(:), allocatable, intent(  out):: val               ! Real value to return
    logical,      optional,    intent(in   ):: upcase            ! Set to True if you want val to be converted to upper case.
    integer,      optional,    intent(  out):: iostat            ! set to zero if there is no error, otherwise non-zero error code
    integer,      optional,    intent(  out):: istart            ! Pointer to start of real in line  -> line(istart:istop)
    integer,      optional,    intent(  out):: istop             ! Pointer to end   of real in line
    integer:: st, sp
    !
    if(present(iostat))  iostat = Z
    !
    call parse_word(line, loc, st, sp)
    !
    if (st > sp ) then
        val = " "
        if(present(iostat))  iostat = 1
    else
        val = line(st:sp)
        !
        if(present(upcase)) then
           if (upcase) call upper(val)
        end if
    end if
    !
    if(present(istart)) istart = st
    if(present(istop )) istop  = sp
    !
    !
  end subroutine
  !
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !
  function max_line_length_unit(unit) result(mxlen)
    integer,               intent(in   ):: unit
    integer:: mxlen
    character(:), allocatable:: fname
    logical :: exist
    !
    mxlen = Z
    !
    if(unit /= Z) then
        call get_file_name(unit, fname, exist) 
        if ( exist ) mxlen = max_line_length_file(fname)
    end if
    !
  end function
  !
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !
  function max_line_length_file(fname) result(mxlen)
    character(*),      intent(in   ):: fname
    integer:: mxlen
    integer:: unit, wid, ierr, iblnk
    character:: txt
    !
    mxlen = Z
    !
    open(newunit=unit, file=fname, action='READ', form='UNFORMATTED', access='STREAM', status='OLD', position="REWIND")
    !
    iblnk = Z  ! Keep track of trailing blank spaces (dont include them)
    wid = Z
    do
        read(unit,iostat=ierr) txt
        if(ierr /= Z) exit
        !
        if(txt==LF) then   ! End of Line Marker
            !
            if(mxlen < wid) mxlen = wid
            wid   = Z
            iblnk = Z
            !
        elseif(txt==BLNK .OR. txt==TAB) then
            !
            iblnk = iblnk + 1
            !
        elseif(txt /= CR) then  !Is not a DOS carriage return, not a blank, and not a linefeed (LF)
            wid = wid + iblnk + 1
            iblnk = Z
        end if
    end do
    !
    if(mxlen < wid) mxlen = wid
    !
    close(unit)
    !
  end function
  !
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !
  pure function int08_2str(ival) result(str)
    integer(int8), intent(in) :: ival
    character(:), allocatable :: str
    character(32)::num
    !
    write(num,'(i32)') ival
    str = trim(adjustl(num))
    !
  end function
  !
  pure function int16_2str(ival) result(str)
    integer(int16), intent(in) :: ival
    character(:),  allocatable :: str
    character(32)::num
    !
    write(num,'(i32)') ival
    str = trim(adjustl(num))
    !
  end function
  !
  pure function int32_2str(ival) result(str)
    integer(int32), intent(in) :: ival
    character(:),  allocatable :: str
    character(32)::num
    !
    write(num,'(i32)') ival
    str = trim(adjustl(num))
    !
  end function
  !
  pure function int64_2str(ival) result(str)
    integer(int64), intent(in) :: ival
    character(:),  allocatable :: str
    character(32)::num
    !
    write(num,'(i32)') ival
    str = trim(adjustl(num))
    !
  end function
  !
  pure function real32_2str(val, pad) result(str)
    real(real32),      intent(in) :: val
    integer, optional, intent(in) :: pad
    character(:),     allocatable :: str
    real(real32):: tmp
    logical :: val10_chk, val1c_chk, val1k_chk
    character(16):: num                                         ! Largest possible number is 14 characters
    !
    num=''
    !
    if(val /= val) then  ! NaN never equals itself if IEEE float
        num='nan'
    elseif(val >= HUGE(tmp)) then
        num = 'inf'
    elseif(val <= -HUGE(tmp)) then
        num = '-inf'
    elseif(val == 0.0_real32) then
        num = '0.0'
    elseif(val>=1.e10_real32  .or. val<=-1.e10_real32) then
       write(num,'(es16.7e2)') val
    elseif(val>=1.e6_real32   .or. val<=-1.e6_real32) then
       write(num,'(es16.7e1)') val
    else !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~v
    !
    tmp =   10._real32*val; val10_chk = abs(tmp - anint(tmp)) < abs(tmp) * 1.e-7_real32 .and. (tmp>=1.0_real32 .or. tmp<=-1.0_real32)
    tmp =  100._real32*val; val1c_chk = abs(tmp - anint(tmp)) < abs(tmp) * 1.e-7_real32 .and. (tmp>=1.0_real32 .or. tmp<=-1.0_real32)
    tmp = 1000._real32*val; val1k_chk = abs(tmp - anint(tmp)) < abs(tmp) * 1.e-7_real32 .and. (tmp>=1.0_real32 .or. tmp<=-1.0_real32)
    !
    if( val10_chk ) then
       write(num,'(f16.1)') val
    elseif( val1c_chk ) then
       write(num,'(f16.2)') val
    elseif( val1k_chk ) then
       write(num,'(f16.3)') val
    elseif(val>=100._real32 .or. val<=-100._real32 ) then
       write(num,'(f16.5)') val
    elseif(val>=0.00099_real32 .or. val<=-0.00099_real32 ) then
       write(num,'(f16.7)') val
    elseif(val>=1.e-9_real32 .or. val<=-1.e-9_real32) then
       write(num,'(es16.7e1)') val
    else
       write(num,'(es16.7e2)') val
    end if
    !
    end if !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~^
    !
    if(present(pad)) then
        call pad2str(pad, num, str)
    else
        str = trim(adjustl(num))
    end if
    !
  end function
  !
  pure function real64_2str(val, pad) result(str)
    real(real64),      intent(in) :: val
    integer, optional, intent(in) :: pad
    character(:),     allocatable :: str
    real(real64):: tmp
    logical :: val10_chk, val1c_chk, val1k_chk, val100k_chk
    character(16):: num                                         ! Largest possible number is 14 characters
    !
    num=''
    !
    if(val /= val) then  ! NaN never equals itself if IEEE float
        num='nan'
    elseif(val >= HUGE(tmp)) then
        num = 'inf'
    elseif(val <= -HUGE(tmp)) then
        num = '-inf'
    elseif(val == 0.0_real64) then
        num = '0.0'
    elseif(val>=1.e100_real64 .or. val<=-1.e100_real64) then
       write(num,'(es16.7e3)') val
    elseif(val>=1.e10_real64  .or. val<=-1.e10_real64) then
       write(num,'(es16.7e2)') val
    elseif(val>=1.e6_real64   .or. val<=-1.e6_real64) then
       write(num,'(es16.7e1)') val
    else !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~v
    !
    tmp =     10._real64*val;   val10_chk = abs(tmp - anint(tmp)) < abs(tmp) * 1.e-13_real64 .and. (tmp>=1.0_real64 .or. tmp<=-1.0_real64)
    tmp =    100._real64*val;   val1c_chk = abs(tmp - anint(tmp)) < abs(tmp) * 1.e-13_real64 .and. (tmp>=1.0_real64 .or. tmp<=-1.0_real64)
    tmp =   1000._real64*val;   val1k_chk = abs(tmp - anint(tmp)) < abs(tmp) * 1.e-13_real64 .and. (tmp>=1.0_real64 .or. tmp<=-1.0_real64)
    tmp = 100000._real64*val; val100k_chk = abs(tmp - anint(tmp)) < abs(tmp) * 1.e-13_real64 .and. (tmp>=1.0_real64 .or. tmp<=-1.0_real64)
    !
    if( val10_chk ) then
       write(num,'(f16.1)') val
    elseif( val1c_chk ) then
       write(num,'(f16.2)') val
    elseif( val1k_chk ) then
       write(num,'(f16.3)') val
    elseif(val100k_chk .or. val>=100._real64 .or. val<=-100._real64 ) then
       write(num,'(f16.5)') val
    elseif(val>=0.00099_real64 .or. val<=-0.00099_real64 ) then
       write(num,'(f16.7)') val
    elseif(val>=1.e-9_real64 .or. val<=-1.e-9_real64) then
       write(num,'(es16.7e1)') val
    elseif(val>=1.e-99_real64 .or. val<=-1.e-99_real64) then
       write(num,'(es16.7e2)') val
    else
       write(num,'(es16.7e3)') val
    end if
    !
    end if !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~^
    !
    if(present(pad)) then
        call pad2str(pad, num, str)
    else
        str = trim(adjustl(num))
    end if
    !
  end function
  !
  pure function int08_2str_pad(ival, pad) result(str)
    integer(int8), intent(in) :: ival
    integer,       intent(in) :: pad
    character(:), allocatable :: str
    character(32)::num
    !
    write(num,'(i32)') ival
    call pad2str(pad, num, str)
    !
  end function
  !
  pure function int16_2str_pad(ival, pad) result(str)
    integer(int16), intent(in) :: ival
    integer,        intent(in) :: pad
    character(:),  allocatable :: str
    character(32)::num
    !
    write(num,'(i32)') ival
    call pad2str(pad, num, str)
    !
  end function
  !
  pure function int32_2str_pad(ival, pad) result(str)
    integer(int32), intent(in) :: ival
    integer,        intent(in) :: pad
    character(:),  allocatable :: str
    character(32)::num
    !
    write(num,'(i32)') ival
    call pad2str(pad, num, str)
    !
  end function
  !
  pure function int64_2str_pad(ival, pad) result(str)
    integer(int64), intent(in) :: ival
    integer,        intent(in) :: pad
    character(:),  allocatable :: str
    character(32)::num
    !
    write(num,'(i32)') ival
    call pad2str(pad, num, str)
    !
  end function
  !
  pure subroutine pad2str(pad, numstr, str)
    integer,                   intent(in ) :: pad
    character(16),             intent(in ) :: numstr ! from XXX2STR that will be padded
    character(:), allocatable, intent(out) :: str
    character(16) :: num
    !
    num = adjustl(numstr)
    !
    if( len_trim(num) < abs(pad)) then
       if (pad>0) then
             str = trim( repeat(' ',pad-len_trim(num))//num )
       else
             str = trim(num)//repeat(' ',abs(pad)-len_trim(num))
       end if
    else
             str = trim(num)
    end if
    !
  end subroutine
  !
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !
  subroutine get_file_name(unit, fname, exist) 
    integer,                 intent(in   ):: unit              ! unit number to look file name up from
    character(:),allocatable,intent(out  ):: fname             ! file name associated with unit number
    logical,                 intent(out  ):: exist             ! return true if unit number is attached to a file that exists, false if not found or error occured
    character(:), allocatable:: fnam
    integer:: i, siz
	character(16):: ciu
    !
    if( unit == Z ) then
        exist = FALSE
        fnam = 'get_file_name error: unit = 0'
        return
    end if
    !
    allocate(character(256):: fnam)
    inquire(unit, name=fnam, exist=exist)
    !
    if(exist) then
          inquire(file=fnam, exist=exist)  !check if file name size is big enough
          if(.not. exist) then
                do i=1, 15
                  if(exist) then
                                exit
                  else
                      siz = 600 * i
                      deallocate(fnam)
                      allocate(character(siz):: fnam)
                      inquire(unit, name=fnam)
                      inquire(file=fnam, exist=exist)  !check if file name size is big enough
                  end if
                end do
          end if
          !
          if(.not. exist) then
	            write(ciu, "(i16)") unit
	            fnam = 'get_file_name error: failed to identy file name from unit number '//trim(adjustl(ciu))
          end if
          !
          i = len_trim(fnam)
          allocate(fname, source=fnam(1:i))
    else
	    write(ciu, "(i16)") unit
        fname = 'get_file_name error: "unknown file" failed to identy file name from unit number '//trim(adjustl(ciu))
    end if
    !
  end subroutine
  !
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !
  pure subroutine upper(line)
    character(*), intent(inout):: line
    integer:: i, n
    !
    do i=1, len_trim(line)
        n = index( "abcdefghijklmnopqrstuvwxyz", line(i:i) )
        !
        if(n > 0) line(i:i) = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"(n:n)
    end do
  end subroutine
  !
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !
  pure subroutine lower(line)
    character(*), intent(inout):: line
    integer:: i, n
    !
    do i=1, len_trim(line)
        n = index( "ABCDEFGHIJKLMNOPQRSTUVWXYZ", line(i:i) )
        !
        if(n > 0) line(i:i) = "abcdefghijklmnopqrstuvwxyz"(n:n)
    end do
  end subroutine
  !
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !
  subroutine open_read(file, unit, iostat)
    character(*), intent(in   ) :: file
    integer,      intent(inout) :: unit
    integer,      intent(inout) :: iostat
    !
    open(newunit=unit, file=file, action='READ', form='FORMATTED', access='SEQUENTIAL', status='OLD', position="REWIND", iostat=iostat)
    !
  end subroutine
  !
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !
  subroutine open_write(file, unit, iostat)
    character(*), intent(in   ) :: file
    integer,      intent(inout) :: unit
    integer,      intent(inout) :: iostat
    !
    open(newunit=unit, file=file, action='WRITE', form='FORMATTED', access='SEQUENTIAL', status='REPLACE', position="REWIND", iostat=iostat)
    !
  end subroutine
  !
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !
  subroutine open_read_binary(file, unit, iostat)
    character(*), intent(in   ) :: file
    integer,      intent(inout) :: unit
    integer,      intent(inout) :: iostat
    !
    open(newunit=unit, file=file, action='READ', form='UNFORMATTED', access='STREAM', status='OLD', position="REWIND", iostat=iostat)
    !
  end subroutine
  !
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !
  subroutine open_write_binary(file, unit, iostat)
    character(*), intent(in   ) :: file
    integer,      intent(inout) :: unit
    integer,      intent(inout) :: iostat
    !
    open(newunit=unit, file=file, action='WRITE', form='UNFORMATTED', access='STREAM', status='REPLACE', position="REWIND", iostat=iostat)
    !
  end subroutine
  !
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !
  subroutine error_stop(msg, unit, line, output, msg2)
    character(*), optional, intent(in) :: msg     ! Error message to print
    integer,      optional, intent(in) :: unit    ! Unit number of file that error occured in
    character(*), optional, intent(in) :: line    ! Line being processed that triggered error
    integer,      optional, intent(in) :: output  ! Unit number of file to write error message to (still writes to stdout as well)
    character(*), optional, intent(in) :: msg2    ! Additional message to append to error (useful for cascading messages
    character(:), allocatable :: fname, err
    logical :: exist
    !
    err = NL // "PROGRAM ERROR STOP" // NL // NL
    if(present(msg)) then
        if( err /= BLNK )  err = NL // msg // NL // NL
    end if
    !
    if(present(msg2)) then
        if( msg2 /= BLNK ) err = err // msg2 // NL // NL
    end if
    !
    if(present(line)) then
        if(line /= BLNK) err = err // 'The guessed line that the error occured on is: ' //NL// '"' // trim(line) // '"' // NL // NL
    end if
    !
    if(present(unit)) then
        if( unit /= Z ) then
           call get_file_name(unit, fname, exist)
           if(exist) err = err // 'This error is believed to have originated from the following file: ' //NL// '"' // trim(fname) // '"'  // NL // NL
        end if
    end if
    !
    if( present(output) ) write(output, "(A)") err
    !
    ERROR STOP err
    !
  end subroutine
  !
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !
end module