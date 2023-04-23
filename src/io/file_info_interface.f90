!
! CODE DEVELOPED BY SCOTT E BOYCE 
!                   CONTACT <seboyce@usgs.gov> or <Boyce@engineer.com>
!
! Queries a file by name or unit number.
! Stores in a data type all Fortran properties about the file.
!    
! MODULE LISTING:
!   file_info_interface
!
!                           type(file_info):: fi
!                                    call fi%init(unit)    -> unit is the unit number to query
!                                    call fi%init(file)    -> file is the file to query, it must be opened
!                                    call fi%destroy()     -> deletes all items in type
!
!                           Constructors
!                                    fi = file_info(unit)  -> Equivalent to call fi%init(unit)
!                                    fi = file_info(file)  -> Equivalent to call fi%init(file)
!
!                           Components
!                                    char: fi % file   -> full path and file name or ERROR message. 
!                                                            note: if file is opened, then file = path // base
!                                    char: fi % base   -> file name only (base name)
!                                    char: fi % path   -> path to file with trailing slash
!                                    !                 
!                                    logi: fi % exist  -> True if file is found.
!                                    logi: fi % opened -> True if file is opened and connected to file unit.
!                                    !                 
!                                    fi % read         -> "YES" or "NO"; can file be read from
!                                    fi % write        -> "YES" or "NO"; can file be written to
!                                    fi % formatted    -> "YES" or "NO"; if file is formatted
!                                    !
!                                    fi % access       -> set to: 'SEQUENTIAL', 'STREAM', 'DIRECT'
!                                    fi % action       -> set to: 'READ',       'WRITE',  'READWRITE'
!                                    fi % decimal      -> set to: 'COMMA',      'POINT'
!                                    fi % encoding     -> set to: 'UTF-8',      'UNKNOWN'
!                                    fi % form         -> set to: 'FORMATTED',  'UNFORMATTED', 'BINARY'
!                                    fi % position     -> set to: 'REWIND',     'APPEND',      'ASIS'
!                                    !
!                                    fi % readwrite   -> "YES" or "NO"; can file be read from and written to
!                                    fi % sequential  -> "YES" or "NO"; if file is sequential
!                                    fi % unformatted -> "YES" or "NO"; if file is unformatted
!                                    fi % direct      -> "YES" or "NO"; if file is direct access
!                                    fi % async       -> "YES" or "NO"; if file is asynchronous
!                                    !
!                                    fi % size        -> Size of file in bytes, set to -1 if not opened
!
module file_info_interface 
  !
  use, intrinsic:: iso_fortran_env, only: real32, real64, real128, int8, int16, int32, int64
  !
  implicit none
  private
  !
  public:: file_info
  !
  interface file_info
    module procedure file_info_file_constructor ! obj = file_info(file)
    module procedure file_info_unit_constructor ! obj = file_info(unit)
  end interface
  !s
  integer,      parameter :: Z = 0
  logical,      parameter :: TRUE  = .TRUE.
  logical,      parameter :: FALSE = .FALSE.
  character(*), parameter :: UNKNOWN = 'UNKNOWN'
  character(*), parameter :: UNDEFINED = 'UNDEFINED'
  !
  type file_info
      integer :: unit = Z
      character(:), allocatable :: file
      logical ::       exist     = FALSE
      logical ::       opened    = FALSE
      !                            
      character( 8) :: read        = UNKNOWN        ! set to: 'YES', 'NO'
      character( 8) :: write       = UNKNOWN        ! set to: 'YES', 'NO'
      character( 8) :: formatted   = UNKNOWN        ! set to: 'YES', 'NO'
      !
      character(12) :: access      = UNDEFINED      ! set to: 'SEQUENTIAL', 'STREAM', 'DIRECT'
      character(12) :: action      = UNDEFINED      ! set to: 'READ', 'WRITE', 'READWRITE'
      character(12) :: decimal     = UNDEFINED      ! set to: 'COMMA', 'POINT'
      character(12) :: encoding    = UNDEFINED      ! set to: 'UTF-8', 'UNKNOWN'
      character(12) :: form        = UNDEFINED      ! set to: 'FORMATTED', 'UNFORMATTED', 'BINARY'
      character(12) :: position    = UNDEFINED      ! set to: 'REWIND', 'APPEND', 'ASIS'
      !                                               
      character( 8) :: readwrite   = UNKNOWN        ! set to: 'YES', 'NO'
      character( 8) :: sequential  = UNKNOWN        ! set to: 'YES', 'NO'
      character( 8) :: unformatted = UNKNOWN        ! set to: 'YES', 'NO'
      character( 8) :: direct      = UNKNOWN        ! set to: 'YES', 'NO'
      character( 8) :: async       = UNKNOWN        ! set to: 'YES', 'NO'
      !
      integer ::       size        = -1               ! Size of file in bytes, set to -1 of unknown
      character(:), allocatable :: base
      character(:), allocatable :: path
      !
  contains
      !
      generic ::               init    => file_info_unit, file_info_file   ! (unit) or (file)
      procedure, pass(self) :: destroy => destroy_file_info                !([keep_file_name])
      !
      procedure, pass(self), private :: file_info_unit
      procedure, pass(self), private :: file_info_file
      final :: final_file_info
  end type
  !
  contains
  !
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !  
  subroutine file_info_file(self, file)
    class(file_info), intent(inout) :: self
    character(*),     intent(in   ) :: file
    integer :: unit
    logical :: exist, opened
    !
    inquire(file=file, number=unit, opened=opened, exist=exist)
    !
    if ( .NOT. opened ) then 
        self%file  = file
    else if ( .NOT. exist ) then
        self%file  = "file_info error: file does not exist"
    end if
    !
    if ( .NOT. opened .or. .NOT. exist ) then
        call self%destroy(keep_file_name=TRUE)
        self%opened = opened
        self%exist = exist
        self%base = file
        self%path = ""
    else
        call file_info_unit(self, unit)
    end if
    !
  end subroutine
  !  
  subroutine file_info_unit(self, unit)
    class(file_info), intent(inout) :: self
    integer,          intent(in   ) :: unit
    integer :: p
    !
    self%unit = unit
    !
    if ( unit /= 0 ) then
        call get_file_name(unit, self%file, self%exist) 
    else
        self%exist = FALSE
        self%file  = "file_info error: unit = 0"
    end if
    !
    if (.not. self%exist ) then
        call self%destroy(keep_file_name=TRUE)
        RETURN
    end if
    !
    inquire(unit=unit                          &
            , access       = self%access       &
            , action       = self%action       &
            , asynchronous = self%async        &
            , decimal      = self%decimal      &
            , direct       = self%direct       &
            , encoding     = self%encoding     &
            , form         = self%form         &
            , formatted    = self%formatted    &
            , opened       = self%opened       &
            , position     = self%position     &
            , read         = self%read         &
            , readwrite    = self%readwrite    &
            , sequential   = self%sequential   &
            , unformatted  = self%unformatted  &
            , write        = self%write        &
            , size         = self%size         &
            )
        
        call upper_componets(self)
        !
        call get_base_pos(self%file, p)
        
        if ( p < 1 ) then
            self%base = self%file(p+1:)
            self%path = ""
        else
            self%base = self%file(p+1:)
            self%path = self%file(:p)
        end if
        !
  end subroutine
  !
  subroutine destroy_file_info(self, keep_file_name)
    class(file_info),  intent(inout) :: self
    logical, optional, intent(in   ) :: keep_file_name
    logical :: del_file
    !
    del_file = TRUE
    if (present(keep_file_name)) del_file = .NOT. keep_file_name
    !
    if ( del_file .and. allocated(self%file) ) deallocate(self%file)
    !
    if ( allocated(self%base) ) deallocate(self%base)
    if ( allocated(self%path) ) deallocate(self%path)
    !
    self%unit = Z
    !
    self%exist     = FALSE
    self%opened    = FALSE
    !           
    self%read        = UNKNOWN  
    self%write       = UNKNOWN  
    self%formatted   = UNKNOWN  
    !
    self%access      = UNDEFINED
    self%action      = UNDEFINED
    self%decimal     = UNDEFINED
    self%encoding    = UNDEFINED
    self%form        = UNDEFINED
    self%position    = UNDEFINED
    !                 
    self%readwrite   = UNKNOWN  
    self%sequential  = UNKNOWN  
    self%unformatted = UNKNOWN  
    self%direct      = UNKNOWN  
    self%async       = UNKNOWN  
    !
    self%size = -1
    !
  end subroutine
  !
  subroutine final_file_info(self)
    type(file_info),  intent(inout) :: self
    call destroy_file_info(self)
  end subroutine
  !
  function file_info_file_constructor(file) Result(self)
    type(file_info) :: self
    character(*), intent(in) :: file
    call self%init(file)
  end function 
  !
  function file_info_unit_constructor(unit) Result(self)
    type(file_info) :: self
    integer, intent(in) :: unit
    call self%init(unit)
  end function 
  !
  pure subroutine get_base_pos(file, pos)
    character(*), intent(inout):: file
    integer,      intent(inout) :: pos
    integer:: i, n
    !
    pos = Z
    !
    n = len_trim(file)
    if ( n < 1 ) RETURN
    !
    if ( file(n:n) == "/" .or. file(n:n) == "\") then
        if ( n == 1 ) RETURN
        file(n:n) = " "
        n = n - 1
    end if
    !
    do i=n, 1, -1 
        if ( file(i:i) == "/" .or. file(i:i) == "\") then
            pos = i
            exit
        end if
    end do
    !
  end subroutine
  !
  pure subroutine upper_componets(self)
    class(file_info), intent(inout) :: self
    call upper( self%access      )
    call upper( self%action      )
    call upper( self%async       )
    call upper( self%decimal     )
    call upper( self%direct      )
    call upper( self%encoding    )
    call upper( self%form        )
    call upper( self%formatted   )
    call upper( self%position    )
    call upper( self%read        )
    call upper( self%readwrite   )
    call upper( self%sequential  )
    call upper( self%unformatted )
    call upper( self%write       )
  end subroutine
  !
  pure subroutine upper(line)
    character(*), intent(inout):: line
    integer:: i, n
    !
    do i=1, len_trim(line)
        n = index( "abcdefghijklmnopqrstuvwxyz", line(i:i))
        !
        if(n > 0) line(i:i) = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"(n:n)
    end do
  end subroutine
  !
  subroutine get_file_name(iu,fname,exist) 
    integer,                 intent(in   ):: iu                ! unit number to look file name up from
    character(:),allocatable,intent(out  ):: fname             ! file name associated with unit number
    logical,                 intent(out  ):: exist             ! return true if unit number is attached to a file that exists, false if not found or error occured
    character(:), allocatable:: fnam
    integer:: i, siz
    character(16):: ciu
    !
    allocate(character(256):: fnam)
    inquire(iu, name=fnam, exist=exist)
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
                      inquire(iu, name=fnam)
                      inquire(file=fnam, exist=exist)  !check if file name size is big enough
                  end if
                end do
          end if
          !
          if(.not. exist) then
               write(ciu, "(i16)") iu
               fnam = 'get_file_name error: failed to identify file name from unit number '//trim(adjustl(ciu))
          end if
          !
          i = len_trim(fnam)
          allocate(fname, source=fnam(1:i))
    else
        write(ciu, "(i16)") iu
        fname = 'get_file_name error: "unknown file" failed to identify file name from unit number '//trim(adjustl(ciu))
    end if
    !
  end subroutine
end module
