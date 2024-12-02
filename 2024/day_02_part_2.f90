module day2_utils
   implicit none
contains
   subroutine is_safe(list, res)
      implicit none
      integer, allocatable, intent(in) :: list(:)
      logical, intent(out):: res
      integer :: i, diff
      logical is_decrement

      res = .true.
      is_decrement = .false.
      do i = 1, size(list) - 1
         if(i == 1 .and. list(i) > list(i+1)) then
            is_decrement = .true.
         end if
         diff = abs(list(i) - list(i+1))
         if(is_decrement)  then
            if (list(i) < list(i+1) .or. diff > 3 .or. diff < 1) then
               res = .false.
               exit
            end if
         else
            if (list(i) >  list(i+1) .or. diff > 3 .or. diff < 1) then
               res = .false.
               exit
            end if
         end if
      end do
   end subroutine is_safe
   subroutine safe_with_dampener(list, res)
      implicit none
      integer, allocatable, intent(in) :: list(:)
      integer, allocatable :: work(:)
      logical, intent(out):: res
      integer :: i, x, idx
      res = .true.
      call is_safe(list, res)
      if(.not. res) then
         allocate(work(size(list)-1))
         do i = 1, size(list)
            idx = 1
            do x = 1, size(list)
               if (x /= i) then
                  work(idx) = list(x)
                  idx = idx + 1
               end if
            end do
            res = .true.
            call is_safe(work, res)
            if(res) then
               exit
            end if
         end do
      end if
   end subroutine safe_with_dampener
end module day2_utils

program day_02_part_2
   use day2_utils
   implicit none
   logical :: is_decrement, safe
   integer :: io, i, x, spaces, res
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
      safe = .true. !set fail to false by default
      call safe_with_dampener(list, safe) ! check if the array has a fail idx
      if (safe) then
         res = res + 1
      endif
      deallocate(list)
   end do
   print*, res
end program day_02_part_2
