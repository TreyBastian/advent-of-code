module day_09_utils
   implicit none
contains
   subroutine swap_to_first_positive_integer_from_end(arr, idx)
      implicit none
      integer, allocatable, intent(inout) :: arr(:)
      integer, intent(in) :: idx
      integer, allocatable :: temp(:)
      integer :: i, res
      do i = 0, size(arr) - 1
         if(arr(size(arr) - i) > -1) then
            arr(idx) = arr(size(arr) - i)
            allocate(temp(size(arr) - (i +1)))
            temp = arr(1:size(arr) - (i+1))
            call move_alloc(temp, arr)
            return
         end if
      end do
   end subroutine swap_to_first_positive_integer_from_end
   subroutine append_to_integer_array_times(arr, val, times)
      implicit none
      integer, allocatable, intent(inout) :: arr(:)
      integer, intent(in) :: val, times
      integer, allocatable :: temp(:)
      if(.not. allocated(arr)) then
         ERROR STOP 'Array not allocated'
      end if
      allocate(temp(size(arr) + times))
      temp(1:size(arr)) = arr
      temp(size(arr) + 1:size(temp)) = val
      call move_alloc(temp, arr)
   end subroutine append_to_integer_array_times
end module day_09_utils
program day_09
   use iso_fortran_env, only: int64
   use day_09_utils
   implicit none
   integer :: io, ios, i, j, block_n, ct, block_start, block_end, space_start, space_end
   integer(kind=int64) :: res
   character(len=1) :: c
   integer, allocatable :: system(:), work(:)
   logical :: is_space, space_block_start

   open(newunit=io, file='./day_09_input.txt', status='old', action='read', access='stream')
   is_space = .false.
   block_n = 0
   do
      read(io, iostat=ios) c
      if (ios /= 0) exit
      read(c, *, iostat=ios) i
      if(ios /= 0) exit
      if (i == 0) then
         is_space = .false.
         cycle
      end if
      if (.not. allocated(system)) then
         allocate(system(i))
         system(1:i) = block_n
         is_space = .true.
         block_n = block_n + 1
      else
         if (is_space) then
            call append_to_integer_array_times(system, -1, i)
            is_space = .false.
         else
            call append_to_integer_array_times(system, block_n, i)
            block_n = block_n + 1
            is_space = .true.
         end if
      end if
   end do
   allocate(work(size(system)))
   work = system
   ct = count(work > -1)
   outer: do
      do i = 1, size(work)
         if (i == ct) exit outer ! we are done
         if(work(i) < 0) then
            call swap_to_first_positive_integer_from_end(work, i)
            exit
         end if
      end do
   end do outer
   res = 0
   do i = 1, size(work)
      if(work(i) > -1) then
         res = res + ((i-1) * work(i))
      else
      end if
   end do
   print *, res
   ! start_part_2
   res = 0
   deallocate(work)
   allocate(work(size(system)))
   work = system
   block_n = -1
   block_start = -1
   block_end = -1
   do i = 0, size(work) -1
      if(block_n == -1 .and. work(size(work) - i) > -1) then
         ! we are starting a block
         block_end = size(work) - i
         block_n = work(size(work) - i)
      else if (block_n /= -1 .and. block_n /= work(size(work)-i)) then
         ! we are ending the block
         block_start = size(work) - i
         ! lets try to move the block
         space_block_start = .true.
         do j = 1, size(work)
            if (j >= block_start) exit
            if(space_block_start .and. work(j) == -1) then
               space_start = j
               space_block_start = .false.
            else if (.not. space_block_start .and. work(j) /= -1) then
               space_end = j
               space_block_start = .true.
               if(space_end - space_start >= block_end - block_start) then
                  work(space_start:(space_start + (block_end - (block_start +1) ))) = work((block_start +1):block_end)
                  work(block_start+1:block_end) = -1
                  block_n = -1
                  block_start = -1
                  block_end = -1

                  exit
               end if
            end if
         end do
         block_n = work(size(work) - i)
         block_end = size(work) - i
         block_start = -1
      end if
   end do
   do i = 1, size(work)
      if(work(i) > -1) then
         res = res + ((i-1) * work(i))
      end if
   end do
   print *, res
end program day_09


