module day2_utils 
  implicit none
  contains
  subroutine has_fail_value(list, res) 
    implicit none
    integer, allocatable, intent(in) :: list(:)
    integer :: i
    integer, intent(out):: res
    logical is_decrement

    res = -1 
    do i = 1, size(list) - 1
    if (list(i) == list(i+1)) then 
       res= i 
       exit
      end if
      if(i == 1 .and. list(i) > list(i+1)) then
        is_decrement = .true.
      else if (i==1 .and. list(i) < list(i+1)) then
        is_decrement = .false.
      end if
      
      if(is_decrement)  then
        if (list(i) < list(i+1) .or. abs(list(i) - list(i+1)) > 3) then 
         res = i 
          exit
        end if
      else
        if (list(i) >  list(i+1) .or. abs(list(i) - list(i+1)) > 3) then 
          res = i
          exit
        end if
      end if
    end do
  end subroutine has_fail_value
end module day2_utils

program day_02_part_2
  use day2_utils
  implicit none
  logical :: is_decrement 
  integer :: io, i,x,y, fail, spaces, res
  integer, dimension(:), allocatable :: list, list2
  character(len=100) :: line

  open(newunit=io, file='./day_02_input.txt', status='old', action='read')

  res = 0
  do i = 1, 1000 
    read(io, '(a)') line ! reading the line 
    spaces = 1 ! reset space count
    do x = 1, len(trim(line))
      ! counting spaces to see how big of an array we need to allocate
      if(iachar(line(x:x))== 32 ) then
        spaces = spaces+1 
      end if
    end do

    allocate(list(spaces))
    read(line, *) list !reading the ints into an array
    fail = -1
    call has_fail_value(list, fail) 
    if (fail > 0) then 
      y = 1
      allocate(list2(spaces -1))
      do x = 1, size(list)
        if(x /= fail) then
          list2(y) = list(x)
          y = y+1
        end if
      end do
      fail = -1
      call has_fail_value(list2, fail)
      if(fail > 0) then
        print*, list2(:)
       end if
      deallocate(list2)

     end if
     if (fail == -1) then
     res = res + 1 
    endif
    deallocate(list)
  end do
  print*, res 
end program day_02_part_2
