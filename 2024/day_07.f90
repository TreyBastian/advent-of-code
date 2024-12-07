module day_07_utils
   use iso_fortran_env, only: int64
   implicit none
contains
   recursive function test_values(arr, target, next_idx, current_value) result(pass)
      implicit none
      integer(kind=int64), intent(in) :: target, current_value, arr(:)
      integer, intent(in) :: next_idx
      logical :: pass
      if(next_idx > size(arr)) then
         pass = current_value == target
         return
      end if
      pass = test_values(arr, target, next_idx + 1, current_value + arr(next_idx)) .or. &
         test_values(arr, target, next_idx + 1, current_value * arr(next_idx))
   end function test_values
   logical function is_calibrated(arr, target)
      implicit none
      integer(kind=int64), intent(in) :: target, arr(:)
      is_calibrated = test_values(arr, target, 2, arr(1))
   end function is_calibrated
end module day_07_utils
program day_07
   use iso_fortran_env, only: int64
   use day_07_utils
   implicit none
   integer(kind=int64) :: test_number, res
   integer(kind=int64), allocatable:: work(:)
   integer :: io, ios, idx, i, ct
   character(len=200) :: line, work_line
   logical :: pass
   res = 0
   open(newunit=io, file='./day_07_input.txt', status='old', action='read')
   do
      read(io, '(a)', iostat=ios) line
      if (ios /= 0) exit
      idx = index(line, ':')
      if (idx > 0) then
         read(line(1:idx-1), *) test_number
         ct = 0
         work_line = trim(line(idx+1:len(trim(line))))
         do i = 1 , len(trim(work_line))
            if(work_line(i:i) == ' ') then
               ct = ct + 1
            end if
         end do
         allocate(work(ct))
         read(work_line, *) work(:)
      end if
      pass = is_calibrated(work, test_number)
      if(pass) then
         res = res + test_number
      end if
      deallocate(work)
   end do
   print*, "Result: ", res
end program day_07
