module circular_queue_instruction
  !
  ! Circular Queue Type
  !   Builds a linked list that has the bottom of the list point to the top.    
  !   Values are added (put) to the top of the list, if the queue size exceeds max_size, the oldest value is lost
  !   Values are removed (pop) from the top of the list
  !
  !
  use, intrinsic:: iso_fortran_env, only: real32, real64, real128, int8, int16, int32, int64, stderr => error_unit
  !
  implicit none
  private
  !
  public:: circ_queue_int64
  !
  public:: CIRC_QUEUE_ERROR_INT64   ! type int64
  public:: MAX_SIZE_ALLOWED         ! type int32
  !
  ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ! Parameters
  !
  integer(int64), parameter:: CIRC_QUEUE_ERROR_INT64 = -HUGE(CIRC_QUEUE_ERROR_INT64)
  integer(int32), parameter:: MAX_SIZE_ALLOWED = HUGE(MAX_SIZE_ALLOWED)
  integer(int64), parameter:: MAX_SIZE_ALLOWED_I64 = int(MAX_SIZE_ALLOWED, int64)
  !
  type int64_link
     integer(int64):: i = CIRC_QUEUE_ERROR_INT64
     type(int64_link), pointer:: prev=>null()
     type(int64_link), pointer:: next=>null()
     !
     contains
     !
     final:: final_int64_link
  end type
  !
  type circ_queue_int64
     private
     integer(int32):: dim = 0
     integer(int32):: siz = 0
     type(int64_link), pointer:: top
     type(int64_link), pointer:: bot
     !
     contains
     !
     generic:: init => init_circ_queue_int64, &                      ! st%init(max_size)
                       init_circ_queue_i32_int64 
     !
     procedure, pass(st):: put => put_circ_queue_int64               ! st%put(val)                   adds val to top of cirlcular queue, if max_size exceeded last value is dropped
     procedure, pass(st):: get => get_circ_queue_int64               ! st%get(val, [pos], [error])   get value at start/top of queue, or at pos position relative to start/top of queue (1 is the start)
     procedure, pass(st):: pop => pop_circ_queue_int64               ! st%pop([val], [pos], [error]) shift queue up and drop top value, set val to it if present. pos drops the top pos positions.
     procedure, pass(st):: reset => reset_circ_queue_int64           ! st%reset()                    removes entire queue
     procedure, pass(st):: get_start => get_start_circ_queue_int64   ! x = st%get_start()            equivalent to st%get(x)
     procedure, pass(st):: get_end   => get_end_circ_queue_int64     ! x = st%get_end()              equivalent to st%get(x, pos=st%siz)
     procedure, pass(st):: pop_start => pop_start_circ_queue_int64   ! x = st%pop_start()            equivalent to st%get(x)
     procedure, pass(st):: pop_end   => pop_end_circ_queue_int64     ! x = st%pop_end()              st%get(x, pos=st%siz) and ONLY drops the last/bottom value
     !
     procedure, pass(st):: change_max_size => change_max_size_int64
     !
     procedure, pass(st):: get_size
     procedure, pass(st):: get_max_size
     procedure, pass(st):: to_array
     !
     procedure, pass(st), private:: init_circ_queue_i32_int64
     procedure, pass(st), private:: init_circ_queue_int64
     !
     procedure, pass(st):: destroy => destroy_circ_queue_int64
     final:: final_circ_queue_int64
  end type
  !
  ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ! Routines
  !
  !
  contains  
  !
  !##########################################################################################################################
  ! Init circ_queue_int64 Routine
  !
  pure subroutine init_circ_queue_i32_int64(st, max_size)
    class(circ_queue_int64), intent(inout):: st
    integer(int64),          intent(in   ):: max_size
    integer:: mx_size
    if(max_size > MAX_SIZE_ALLOWED_I64) then
        mx_size = MAX_SIZE_ALLOWED
    else
        mx_size = int(max_size, int32)
    end if
    call init_circ_queue_int64(st, mx_size)
  end subroutine
  !
  pure subroutine init_circ_queue_int64(st, max_size)
    class(circ_queue_int64), intent(inout):: st
    integer(int32),          intent(in   ):: max_size
    type(int64_link), pointer:: pnt
    integer:: i
    !
    call destroy_circ_queue_int64(st)
    if (max_size < 1) return
    !
    st%dim = max_size
    !
    allocate(st%top)
    pnt => st%top
    do i=2, st%dim
       allocate(pnt%next)
       pnt%next%prev => pnt
       pnt => pnt%next
    end do
    pnt%next => st%top
    st%bot => pnt
    st%top%prev => st%bot
    pnt => NULL()
  end subroutine
  !
  !##########################################################################################################################
  ! Get Property Routine
  !
  pure function get_size(st) result(size)
    class(circ_queue_int64), intent(in):: st
    integer(int32):: size
    size = st%siz
  end function
  !
  pure function get_max_size(st) result(size)
    class(circ_queue_int64), intent(in):: st
    integer(int32):: size
    size = st%dim
  end function
  !
  subroutine to_array(st, array)
    class(circ_queue_int64), intent(in):: st
    integer(int64), dimension(:), allocatable, intent(out):: array
    type(int64_link), pointer:: pnt
    integer:: i
    if(allocated(array)) deallocate(array)
    IF(st%siz < 1) return
    !
    allocate(array(st%siz))
    !
    pnt => st%top
    array(1) = pnt%i
    !
    do i=2, st%siz
       pnt => pnt%next
       array(i) = pnt%i
    end do
    !
    pnt => NULL()
  end subroutine
  !
  !##########################################################################################################################
  ! Shift Routine
  !
  pure subroutine shift_top_up(st)
    class(circ_queue_int64), intent(inout):: st
    st%top => st%top%prev
    st%bot => st%bot%prev
  end subroutine
  !
  pure subroutine shift_top_down(st)
    class(circ_queue_int64), intent(inout):: st
    st%top => st%top%next
    st%bot => st%bot%next
  end subroutine
  !
  !##########################################################################################################################
  ! Modify Routine
  !
  pure subroutine change_max_size_int64(st, max_size)
    class(circ_queue_int64), intent(inout):: st
    integer(int64),          intent(in   ):: max_size
    type(int64_link), pointer:: pnt
    !
    if(st%dim == max_size) return
    !
    if(st%dim < 1 .or. max_size < 1) then
        call st%init(max_size)
        return
    end if
    !
    if( max_size < st%dim ) then
        if(st%siz < max_size) st%siz = max_size
        !
        do while (max_size < st%dim )
           st%dim = st%dim - 1
           pnt => st%bot
           st%bot => st%bot%prev
           pnt%prev => NULL()
           pnt%next => NULL()
           deallocate(pnt)
        end do
        st%bot%next => st%top
        st%top%prev => st%bot
    else
        do while (st%dim < max_size)
           st%dim = st%dim + 1
           allocate(pnt)
           pnt%prev => st%bot
           st%bot%next => pnt
           pnt => NULL()
           st%bot => st%bot%next
        end do
        st%bot%next => st%top
        st%top%prev => st%bot
    end if
    !
  end subroutine 
  !
  pure subroutine put_circ_queue_int64(st, val)
    class(circ_queue_int64), intent(inout):: st
    integer(int64),          intent(in   ):: val
    !
    call shift_top_up(st)
    st%top%i = val
    !
    if( st%siz < st%dim ) st%siz = st%siz + 1
  end subroutine 
  !
  subroutine get_circ_queue_int64(st, val, pos, error)
    class(circ_queue_int64),  intent(in   ):: st
    integer(int64),           intent(  out):: val
    integer(int32), optional, intent(in   ):: pos
    logical,        optional, intent(  out):: error
    type(int64_link), pointer:: pnt
    integer:: i, p
    p = 1
    if(present(pos)) p = pos
    !
    if ( p < 1 .or. p > st%siz ) then
       val = CIRC_QUEUE_ERROR_INT64
       if(present(error)) error = .TRUE.
       return
    end if
    !
    pnt => st%top
    do i=2, p
       pnt => pnt%next
    end do
    val = pnt%i
    !
    if(present(error)) error = .FALSE.
  end subroutine 
  !
  pure subroutine pop_circ_queue_int64(st, val, pos, error)
    class(circ_queue_int64),  intent(inout):: st
    integer(int64), optional, intent(  out):: val
    integer(int32), optional, intent(in   ):: pos
    logical,        optional, intent(  out):: error
    integer:: i, p
    p = 1
    if(present(pos)) p = pos
    !
    if ( p < 1 ) then
       if(present(val)) val = CIRC_QUEUE_ERROR_INT64
       if(present(error)) error = .TRUE.
       return
    end if
    !
    if ( p > st%siz ) then
       st%siz = 0
       if(present(val)) val = CIRC_QUEUE_ERROR_INT64
       if(present(error)) error = .TRUE.
       return
    end if
    !
    do i=2, p
       call shift_top_down(st)
    end do
    if(present(val)) val = st%top%i
    call shift_top_down(st)
    st%siz = st%siz - p
    !
    if(present(error)) error = .FALSE.
  end subroutine 
  !
  pure subroutine reset_circ_queue_int64(st)
    class(circ_queue_int64), intent(inout):: st
    st%siz = 0
  end subroutine
  !
  function get_start_circ_queue_int64(st) result(val)
    class(circ_queue_int64), intent(in):: st
    integer(int64):: val
    call get_circ_queue_int64(st, val)
  end function 
  !
  function get_end_circ_queue_int64(st) result(val)
    class(circ_queue_int64), intent(in):: st
    integer(int64):: val
    call get_circ_queue_int64(st, val, st%siz)
  end function 
  !
  function pop_start_circ_queue_int64(st) result(val)
    class(circ_queue_int64), intent(inout):: st
    integer(int64):: val
    call st%pop(val)
  end function 
  !
  function pop_end_circ_queue_int64(st) result(val)
    class(circ_queue_int64), intent(inout):: st
    integer(int64):: val
    call st%get(val, st%siz)
    st%siz = st%siz - 1
  end function 
  !
  !##########################################################################################################################
  ! Destroy circ_queue_int64 Routine
  !
  pure subroutine destroy_circ_queue_int64(st)
    class(circ_queue_int64), intent(inout):: st
    type(int64_link), pointer:: pnt
    !
    st%siz = 0
    if (st%dim < 1) return
    !
    st%bot%next => NULL()
    st%bot => NULL()
    !
    do while (associated(st%top%next))
       pnt => st%top
       st%top => st%top%next
       pnt%prev => NULL()
       pnt%next => NULL()
       deallocate(pnt)
    end do
    !
    pnt => st%top
    st%top => NULL()
    pnt%prev => NULL()
    pnt%next => NULL()
    deallocate(pnt)
    pnt => NULL()
    !
    st%dim = 0
    !
  end subroutine
  !
  pure subroutine final_circ_queue_int64(st)
    type(circ_queue_int64), intent(inout):: st
    call destroy_circ_queue_int64(st)
  end subroutine
  !
  pure subroutine final_int64_link(lnk)
    type(int64_link), intent(inout):: lnk
    lnk%i = 0_int64
    lnk%prev => NULL()
    lnk%next => NULL()
  end subroutine
  !
end module
