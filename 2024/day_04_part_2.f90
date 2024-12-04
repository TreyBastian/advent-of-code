program day_04
   implicit none
   character(len=140) ::  lines(140)
   character(len=1) :: c
   integer :: io, row, col, score, z
   logical :: left_m, right_m

   left_m = .false.
   right_m = .false.
   score = 0
   open(newunit=io, file='./day_04_input.txt', status='old', action='read')
   read(io, '(a)') lines
   do row = 2, size(lines) - 1
      do col = 2, len(lines(row)) - 1
         if (lines(row)(col:col) == 'A') then
            c = lines(row-1)(col-1:col-1)
            if (c == 'S' .or. c == 'M') then
               left_m = c == 'M'
               c = lines(row-1)(col+1:col+1)
               if (c == 'S' .or. c == 'M') then
                  right_m = c == 'M'
                  c = lines(row+1)(col-1:col-1)
                  if (right_m .and. c == 'S' .or. (.not. right_m .and. c == 'M')) then
                     c = lines(row+1)(col+1:col+1)
                     if (left_m .and. c == 'S' .or. (.not. left_m .and. c =='M')) then
                        score = score + 1
                     end if
                  end if
               end if
            end if
         end if
         left_m = .false.
         right_m = .false.
         c = ''
      end do
   end do
   print*, score

end program day_04
