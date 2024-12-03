program day_03_part_2
   implicit none
   logical :: find_a, is_dont
   integer ::io, ios, a, b, state, res
   character(len=1) :: c
   character(len=10) :: buffer
   character(len=4) :: mul_cmd = 'mul('
   character(len=7) :: dont_cmd = "don't()"
   character(len=4) :: do_cmd = 'do()'

   res = 0
   open(newunit=io, file='./day_03_input.txt', status='old', action='read', access='stream')
   state = 0 ! 0 = find command, 1 = find number, 2 = calculate
   find_a = .true.
   is_dont = .false.
   buffer = ''
   do
      read(io, iostat=ios) c
      if (ios /= 0) exit
      select case(state)
       case (0) ! find command
         buffer = trim(buffer) // c
         if(is_dont) then
            if (trim(buffer) == do_cmd) then
               state = 0
               buffer = ''
               is_dont = .false.
            else if (trim(buffer) /= do_cmd(1:len(trim(buffer)))) then
               state = 0
               buffer = ''
            end if
         else
            if (trim(buffer) == dont_cmd) then
               state = 0
               buffer = ''
               is_dont = .true.
            else if(trim(buffer) == mul_cmd) then
               state = 1
               buffer = ''
            else if(trim(buffer) /= mul_cmd(1:len(trim(buffer))) .and. trim(buffer) /= dont_cmd(1:len(trim(buffer)))) then
               state = 0
               buffer = ''
            end if
         end if
       case (1) ! find digits
         select case (iachar(c))
          case (48:57) ! 0-9
            buffer = trim(buffer) // c
          case (44) ! ,
            if (find_a .and. len(trim(buffer)) > 0) then
               read(buffer, *) a
               buffer = ''
               find_a = .false.
            else
               state = 0 ! invalid
               find_a = .true.
               buffer = ''
            end if
          case (41) ! )
            if(.not. find_a .and. len(trim(buffer)) > 0) then
               read(buffer, *) b
               state = 2
               find_a = .true.
               buffer = ''
            else
               state = 0
               find_a = .true.
               buffer = ''
            end if
          case default
            state = 0
            find_a = .true.
            buffer = ''
         end select
      end select
      if(state == 2) then
         res = res + (a * b)
         state = 0
         buffer = ''
      end if
   end do
   print*, res
end program day_03_part_2
