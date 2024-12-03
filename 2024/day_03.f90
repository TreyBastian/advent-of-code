program day_03
   implicit none
   integer ::io, ios, a, b, state, res, cmd_state, digit_state
   character(len=1) :: c
   character(len=10) :: buffer ! buffer to store the number
   res = 0
   open(newunit=io, file='./day_03_input.txt', status='old', action='read', access='stream')
   state = 0 ! 0 = find command, 1 = find number, 3 = calculate
   digit_state = 0 ! 0 = find a, 1 = find b
   cmd_state = 0 ! 0 = find m, 1 = find u, 2 = find l 3 = find (
   buffer = ''
   do
      read(io, iostat=ios) c
      if (ios /= 0) exit
      select case(state)
       case (0) ! find command
         select case(cmd_state)
          case (0) ! find m
            if (c == 'm') then
               cmd_state = 1
            else
               cmd_state = 0
            end if
          case (1) ! find u
            if (c == 'u') then
               cmd_state = 2
            else
               cmd_state = 0
            end if
          case(2) ! find l
            if (c == 'l') then
               cmd_state = 3
            else
               cmd_state = 0
            end if
          case(3)   ! find (
            if (c == '(') then
               state = 1
               cmd_state = 0
            else
               cmd_state = 0
            end if
         end select
       case (1) ! find digits
         select case (iachar(c))
          case (48:57) ! 0-9
            buffer = trim(buffer) // c
          case (44) ! ,
            if (digit_state == 0 .and. len(trim(buffer)) > 0) then
               read(buffer, *) a
               print*, 'a: ', a
               buffer = ''
               digit_state = 1
            else
               state = 0 ! invalid
               digit_state = 0
               buffer = ''
            end if
          case (41) ! )
            if(digit_state == 1 .and. len(trim(buffer)) > 0) then
               read(buffer, *) b
               print*, 'b: ', b
               state = 3 ! reset state
               digit_state = 0
               buffer = ''
            else
               state = 0
               digit_state = 0
               buffer = ''
            end if
          case default
            state = 0
            digit_state = 0
            buffer = ''
         end select
      end select
      if(state == 3) then
         res = res + (a * b)
         state = 0
      end if
   end do
   print*, res
end program
