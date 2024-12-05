module day_05_utils
   implicit none
contains
   subroutine append_to_integer_array(arr, val)
      implicit none
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
   function fix_and_get_middle_value(arr, left, right) result(res)
      ! we are going to fix the array swapping until its right
      ! there's probably a better way maybe? but I just want to get it done
      implicit none
      integer, allocatable, intent(in) :: arr(:), left(:), right(:)
      integer, allocatable :: work(:), copy(:)
      integer :: res, i, j, tmp
      logical :: found
      allocate(copy(size(arr)))
      copy = arr
      do
         found = .false.
         outer: do i = 1, size(copy)
            if(i+1 < size(copy)) then ! if we can check forward
               if(allocated(work)) then
                  deallocate(work)
               end if
               do j = 1, size(right) ! find value in right and save left to work
                  if (copy(i) == right(j)) then
                     if(.not. allocated(work)) then
                        allocate(work(1))
                        work(1) = left(j)
                     else
                        call append_to_integer_array(work, left(j))
                     end if
                  end if
               end do
               do j = i+1, size(copy)
                  if(any(work == copy(j))) then
                     tmp = copy(i)
                     copy(i) = copy(j)
                     copy(j) = tmp
                     found = .true.
                     exit outer
                  end if
               end do
            end if

            if(.not. found) then ! if we are still valid lets keep going
               if(allocated(work)) then
                  deallocate(work)
               end if
               if(i-1 > 0) then ! if we can check backwards
                  do j = 1, size(left) ! find value in left and save left to work

                     if (copy(i) == left(j)) then
                        if(.not. allocated(work)) then
                           allocate(work(1))
                           work(1) = right(j)
                        else
                           call append_to_integer_array(work, right(j))
                        end if
                     end if
                  end do
                  do j = 1, size(copy)
                     if(any(work == copy(j) .and. j < i)) then
                        tmp = copy(i)
                        copy(i) = copy(j)
                        copy(j) = tmp
                        found = .true.
                        exit outer
                     end if
                  end do
               end if
            else
               exit outer
            end if
         end do outer
         if(.not. found) exit
      end do
      res = copy((size(copy) + 1)/2)
      deallocate(copy)
      if(allocated(work)) then
         deallocate(work)
      end if
   end function fix_and_get_middle_value
end module day_05_utils
program day_05
   use day_05_utils
   implicit none
   integer :: io, ios, idx, a, b, ct, i, j, res, res_part2
   integer, allocatable :: left(:), right(:), print_list(:), work(:)
   character(len=100) ::line
   logical :: found

   open(newunit=io, file='./day_05_input.txt', status='old', action='read')
   res = 0
   res_part2 = 0
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
         else
            res_part2 = res_part2 + fix_and_get_middle_value(print_list, left, right)
         end if
         deallocate(print_list)
      end if
   end do

   deallocate(left)
   deallocate(right)
   print*, "Result ", res
   print*, "Part 2 Result ", res_part2
end program day_05
