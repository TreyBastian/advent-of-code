module day_05_utils
contains
   subroutine append_to_integer_array(arr, val)
      integer, allocatable, intent(inout) :: arr(:)
      integer, intent(in) :: val
      integer, allocatable :: temp(:)
      if(.not. allocated(arr)) then
         ERROR STOP 'Array not allocated'
      end if
      allocate(temp(size(arr) + 1))
      temp(1:size(arr)) = arr
      temp(size(arr) + 1) = val
      call move_alloc(temp, arr)
   end subroutine append_to_integer_array
end module day_05_utils
program day_05
   use day_05_utils
   implicit none
   integer :: io, ios, idx, a, b, ct, i, j, res
   integer, allocatable :: left(:), right(:), print_list(:), work(:)
   character(len=100) ::line
   logical :: found

   open(newunit=io, file='./day_05_input.txt', status='old', action='read')
   res = 0
   do
      read(io, '(a)', iostat=ios) line
      if(ios /= 0) exit !eof
      idx = index(line, '|')
      if(idx > 0) then ! rules
         read(line(1:idx-1), *) a
         read(line(idx+1:), *) b
         if(.not. allocated(left)) then
            allocate(left(1))
            left(1) = a
         else
            call append_to_integer_array(left, a)
         end if
         if(.not. allocated(right)) then
            allocate(right(1))
            right(1) = b
         else
            call append_to_integer_array(right, b)
         end if
      end if
      idx = index(line, ',')
      if (idx > 0 ) then ! print_list
         ct = 1
         do i = 1 , len_trim(line)
            if(line(i:i) == ',') then
               ct = ct + 1
            end if
         end do
         allocate(print_list(ct))
         read(line, *) print_list
         found = .false.
         do i = 1, size(print_list)
            if(i+1 < size(print_list)) then ! if we can check forward
               do j = 1, size(right) ! find value in right and save left to work
                  if (print_list(i) == right(j)) then
                     if(.not. allocated(work)) then
                        allocate(work(1))
                        work(1) = left(j)
                     else
                        call append_to_integer_array(work, left(j))
                     end if
                  end if
               end do
               do j = 1, size(work)
                  if(any(print_list(i+1:size(print_list)) == work(j))) then
                     found = .true.
                     exit
                  end if
               end do
               deallocate(work)
            end if
            if(.not. found) then ! if we are still valid lets keep going
               if(i-1 > 0) then ! if we can check backwards
                  do j = 1, size(left) ! find value in left and save left to work
                     if (print_list(i) == left(j)) then
                        if(.not. allocated(work)) then
                           allocate(work(1))
                           work(1) = right(j)
                        else
                           call append_to_integer_array(work, right(j))
                        end if
                     end if
                  end do
                  do j = 1, size(work)
                     if(any(print_list(1:i-1) == work(j))) then
                        found = .true.
                        exit
                     end if
                  end do
                  deallocate(work)
               end if
            else
               exit
            end if
         end do
         if (.not. found) then
            res = res + print_list((size(print_list) + 1)/2)
         end if

         deallocate(print_list)
      end if
   end do

   deallocate(left)
   deallocate(right)
   print*, "Result ", res
end program day_05
