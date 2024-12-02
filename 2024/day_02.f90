program day_02
   implicit none
   logical :: is_decrement, fail
   integer :: io, i,x, spaces, res
   integer, dimension(:), allocatable :: list
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
      fail = .false.

      do x = 1, size(list) -1
         if (list(x) == list(x+1)) then
            fail = .true.
            exit
         end if
         if(x == 1 .and. list(x) > list(x+1)) then
            is_decrement = .true.
         else if (x==1 .and. list(x) < list(x+1)) then
            is_decrement = .false.
         end if

         if(is_decrement)  then
            if (list(x) < list(x+1) .or. abs(list(x) - list(x+1)) > 3) then
               fail = .true.
               exit
            end if
         else
            if (list(x) >  list(x+1) .or. abs(list(x) - list(x+1)) > 3) then
               fail = .true.
               exit
            end if
         end if
      end do
      if (fail .eqv. .false.) then
         res = res + 1
      endif
      deallocate(list)
   end do
   print*, res
end program day_02
