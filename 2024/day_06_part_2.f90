program day_06_part_2
   implicit none
   character(len=130) :: line, lines(130), temp_lines(130)
   integer:: io, x,y, xd,yd, i, j, ct, o_ct, init_x, init_y, init_xd, init_yd

   open(newunit=io, file='./day_06_input.txt', status='old', action='read')
   read(io, '(a)') lines

   ct = 0
   ! find starting position
   do i = 1, size(lines)
      if (index(lines(i), '^') > 0) then
         y = i
         x = index(lines(i), '^')
         lines(i)(x:x) = '.' ! replace with a dot to not confuse this part
         xd = 0
         yd = -1
         init_x = x
         init_y = y
         init_xd = xd
         init_yd = yd
         exit
      end if
   end do

   do i = 1, size(lines)
      do j = 1, len(lines(i))
         o_ct = 0
         ! reset positions
         x = init_x
         y = init_y
         xd = init_xd
         yd = init_yd
         temp_lines = lines ! fresh board every run
         if(temp_lines(i)(j:j) == '.') then
            temp_lines(i)(j:j) = 'O'  ! place obstacle to test
            ! move until we leave the bounds when we hit a valid spot mark it with an X and increase count
            do
               ! are we going out of bounds?
               if (x+xd < 1 .or. x+xd > len(temp_lines(y)) .or. y+yd < 1 .or. y+yd > size(temp_lines)) then
                  exit
               end if

               ! try to move
               select case(temp_lines(y+yd)(x+xd:x+xd))
                case('.')
                  ! valid move
                  x = x + xd
                  y = y + yd
                case('O')
                  ! we are blocked lets rotate 90 degrees and count obstacle
                  o_ct = o_ct + 1
                  if(xd == 0 .and. yd == -1) then
                     xd = 1
                     yd = 0
                  else if (xd == 1 .and. yd == 0) then
                     xd = 0
                     yd = 1
                  else if (xd == 0 .and. yd == 1) then
                     xd = -1
                     yd = 0
                  else if (xd == -1 .and. yd == 0) then
                     xd = 0
                     yd = -1
                  end if
                case('#')
                  ! we are blocked lets rotate 90 degrees
                  ! change this to an O to try and detect when stuck
                  temp_lines(y+yd)(x+xd:x+xd) = 'O'
                  if(xd == 0 .and. yd == -1) then
                     xd = 1
                     yd = 0
                  else if (xd == 1 .and. yd == 0) then
                     xd = 0
                     yd = 1
                  else if (xd == 0 .and. yd == 1) then
                     xd = -1
                     yd = 0
                  else if (xd == -1 .and. yd == 0) then
                     xd = 0
                     yd = -1
                  end if
                case default
                  exit
               end select
               if(o_ct > 100) then
                  exit ! probably stuck
               end if
            end do
         end if
         if(o_ct > 100) then ! we got stuck probably 100 is so arbitrary
            ct = ct + 1
         end if
      end do
   end do
   print*, "block points: ", ct
end program day_06_part_2
