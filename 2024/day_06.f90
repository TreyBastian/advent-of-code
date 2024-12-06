program day_06
   implicit none
   character(len=130) :: line, lines(130)
   integer:: io, x,y, xd,yd, i, ct

   open(newunit=io, file='./day_06_input.txt', status='old', action='read')
   read(io, '(a)') lines

   ct = 0
   ! find starting position
   do i = 1, size(lines)
      if (index(lines(i), '^') > 0) then
         y = i
         x = index(lines(i), '^')
         xd = 0
         yd = -1
         exit
      end if
   end do

   ! move until we leave the bounds when we hit a valid spot mark it with an X and increase count
   do
      ! are we going out of bounds?
      if (x+xd < 1 .or. x+xd > len(lines(y)) .or. y+yd < 1 .or. y+yd > size(lines)) then
         ct = ct + 1
         lines(y)(x:x) = 'X'
         exit
      end if

      ! try to move
      select case(lines(y+yd)(x+xd:x+xd))
       case('.')
         ! valid move
         ct = ct + 1
         x = x + xd
         y = y + yd
         lines(y)(x:x) = 'X'
       case('X')
         ! valid move but we've been here before
         x = x + xd
         y = y + yd
       case('#')
         ! we are blocked lets rotate 90 degrees
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
   end do
   print*, lines
   print*, "distinct: ", ct
end program day_06

