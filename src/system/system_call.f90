module system_call
  implicit none
  !
  private
  !
  public:: execute_command_get_output  ! call execute_command_get_output(command, output, [stat], [keep_empty_lines])
  !
  contains
  !
  subroutine execute_command_get_output(command, output, stat, keep_empty_lines)
    character(*),                            intent(in ) :: command             ! Command that will be executed in system terminal shell
    character(*), dimension(:), allocatable, intent(out) :: output              ! Output generated from command, dimension is set to the number of non-empty/blank lines returned. 
    integer,                       optional, intent(out) :: stat                ! Status generated from command. If not present and error occurs, invokes ERROR STOP. If present, OUTPUT contains error message.
    logical,                       optional, intent(in ) :: keep_empty_lines    ! If present, and TRUE, then empty/blank lines are included in OUTPUT
    character(:), allocatable:: file_name
    character(512):: line
    integer:: iu, ierr, dim
    logical:: keep_empty
    
    keep_empty = .FALSE.
    if(present(keep_empty_lines)) keep_empty = keep_empty_lines
    
    if(present(stat)) stat = 0
    
    if(allocated(output)) deallocate(output)
    
    if(index(command, '>') > 0) then
       if(present(stat)) then
          allocate(output(1))
          output = '> not allowed'
          stat = huge(stat)
          return
       end if
       write(*,'(A)') 'execute_command_output error: command not allowed to contain ">", but command = "'//command//'"'
       ERROR STOP 99
    end if
    
    open(newunit=iu, status='SCRATCH', iostat=ierr)
    
    if(ierr == 0) then
       call get_file_name(iu, file_name, ierr)
       close(iu, iostat=ierr)
    end if
    
    if(ierr /= 0) then
       if(present(stat)) then
          allocate(output(1))
          output = 'scratch file error'
          stat = huge(stat) - 1
          return
       end if
       write(*,'(A)') 'execute_command_output error: failed to open scratch file to capture command output'
       ERROR STOP 98
    end if
    !
    call execute_command_line(command//' > '//file_name, exitstat=ierr)
    
    if(ierr /= 0) then
       if(present(stat)) then
          allocate(output(1))
          output = 'command error'
          stat = ierr
          return
       end if
       write(*,'(A)') 'execute_command_output error: failed to open scratch file to capture command output'
       ERROR STOP 98
    end if
    !
    open(newunit=iu, file=file_name, status='OLD', action='READ',  &
         form='FORMATTED', access='SEQUENTIAL', position='REWIND', &
         iostat=ierr)
    !
    dim = 0
    do 
       read(iu, '(A)', iostat=ierr) line
       if(ierr /= 0) exit
       if(len_trim(line) > 0 .or. keep_empty) dim = dim + 1
    end do
    !
    if( dim == 0 ) then
       allocate(output(1))
       output = ''
       return
    end if
    allocate(output(dim))
    !
    rewind(iu)
    !
    dim = 0
    do 
       read(iu, '(A)', iostat=ierr) line
       if(ierr /= 0) exit
       if(len_trim(line) > 0 .or. keep_empty) then
          dim = dim + 1
          output(dim) = line
       end if
    end do
    !
    close(iu, status='DELETE', iostat=ierr)
    !
  end subroutine
  !
  subroutine get_file_name(iu, fname, ierr) 
    integer,                   intent(in ):: iu        ! unit number to look file name up from
    character(:), allocatable, intent(out):: fname     ! file name associated with unit number
    integer,                   intent(out):: ierr
    character(:), allocatable:: fnam
    integer:: i, siz
    logical:: file_found
    !
    ierr = 1
    !
    if(iu == 0) return
    !
    if(allocated(fnam)) deallocate(fnam)
    allocate(character(256):: fnam)
    inquire(iu, name=fnam, exist=file_found)
    !
    if(file_found) then
       inquire(file=fnam, exist=file_found)  !check if file name size is big enough
       if(.not. file_found) then
          do i=1, 15
             if(file_found) exit
             !
             siz = 512 * i
             deallocate(fnam)
             allocate(character(siz):: fnam)
             inquire(iu, name=fnam)
             inquire(file=fnam, exist=file_found)  !check if file name size is big enough
          end do
       end if
    end if
    !
    if(file_found) then
       ierr = 0
       i = len_trim(fnam)
       allocate(fname, source=fnam(1:i))
    end if
    !
  end subroutine
end module
