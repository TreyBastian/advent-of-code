module day_04_utils
   implicit none
contains
   recursive function find_xmas(arr, row, col,row_dir, col_dir, search_letter) result(found)
      implicit none
      character(len=140), intent(in), allocatable :: arr(:)
      character(len=1), intent(in) :: search_letter
      integer, intent(in) :: row, col, row_dir, col_dir
      integer :: new_row, new_col
      logical :: found

      if(arr(row)(col:col) == search_letter) then
         if(search_letter == 'S') then
            found = .true.
         else
            new_row = row + row_dir
            new_col = col + col_dir
            if(new_row > len(arr(row)) .or. new_row < 1 .or. new_col > size(arr) .or. new_col < 1) then
               found = .false.
            else
               select case(search_letter)
                case('M')
                  found = find_xmas(arr, new_row, new_col, row_dir, col_dir, 'A')
                case('A')
                  found = find_xmas(arr, new_row, new_col, row_dir, col_dir, 'S')
               end select
            end if
         end if
      else
         found = .false.
      end if
   end function find_xmas
end module day_04_utils
program day_04
   use day_04_utils
   implicit none
   character(len=140) , allocatable :: lines(:)
   integer :: io, row, col, score, z
   logical :: found

   score = 0
   open(newunit=io, file='./day_04_input.txt', status='old', action='read')
   allocate(lines(140))
   read(io, '(a)') lines
   do row = 1, size(lines)
      do col = 1, len(lines(row))
         if (lines(row)(col:col) == 'X') then
            do z = 1, 8
               found = .false.
               select case (z)
                case(1)
                  found = find_xmas(lines, row, col-1, 0, -1, 'M')
                case(2)
                  found = find_xmas(lines, row-1, col-1, -1, -1, 'M')
                case(3)
                  found = find_xmas(lines, row-1, col, -1, 0, 'M')
                case(4)
                  found = find_xmas(lines, row-1, col+1,  -1, 1, 'M')
                case(5)
                  found = find_xmas(lines, row, col+1, 0, 1, 'M')
                case(6)
                  found = find_xmas(lines, row+1, col+1, 1, 1, 'M')
                case(7)
                  found = find_xmas(lines, row+1, col, 1, 0, 'M')
                case(8)
                  found = find_xmas(lines, row+1, col-1, 1, -1, 'M')
               end select
               if(found) then
                  score = score + 1
               end if
            end do
         end if
      end do
   end do
   deallocate(lines)
   print*, score

end program day_04
