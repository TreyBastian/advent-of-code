module day_08_utils
   implicit none
   type node
      character :: c
      integer, allocatable :: x(:), y(:)
   end type node
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

   subroutine add_node(nodes, c, x, y)
      type(node), allocatable, intent(inout) :: nodes(:)
      type(node), allocatable :: temp(:)
      character, intent(in) :: c
      integer, intent(in) :: x, y
      integer :: i
      do i = 1, size(nodes)
         if (nodes(i)%c == c) then
            if(.not. allocated(nodes(i)%x)) then
               allocate(nodes(i)%x(1))
               nodes(i)%x(1) = x
            else
               call append_to_integer_array(nodes(i)%x, x)
            end if
            if(.not. allocated(nodes(i)%y)) then
               allocate(nodes(i)%y(1))
               nodes(i)%y(1) = y
            else
               call append_to_integer_array(nodes(i)%y, y)
            end if
            return
         end if
      end do
      allocate(temp(size(nodes) + 1))
      temp(1:size(nodes)) = nodes
      allocate(temp(size(nodes) + 1)%x(1))
      allocate(temp(size(nodes) + 1)%y(1))
      temp(size(nodes) + 1)%c = c
      temp(size(nodes) + 1)%x(1) = x
      temp(size(nodes) + 1)%y(1) = y
      call move_alloc(temp, nodes)
   end subroutine add_node
end module day_08_utils

program day_08
   use day_08_utils
   implicit none
   character(len=50) :: lines(50)
   integer :: io, i, j, k, l, score, xdist, ydist, next_x, next_y
   integer, allocatable :: anti_x(:), anti_y(:)
   type(node), allocatable :: nodes(:)
   logical :: found

   open(newunit=io, file='day_08_input.txt', status='old', action='read')
   read(io, '(A)') lines
   score = 0
   do i = 1, 50
      do j = 1, 50
         if(lines(i)(j:j) /= '.') then
            if(.not. allocated(nodes)) then
               allocate(nodes(1))
               nodes(1)%c = lines(i)(j:j)
               allocate(nodes(1)%x(1))
               allocate(nodes(1)%y(1))
               nodes(1)%x(1) = j
               nodes(1)%y(1) = i
            else
               call add_node(nodes, lines(i)(j:j), j, i)
            end if
         end if
      end do
   end do
   do i = 1, size(nodes)
      do j = 1, size(nodes(i)%x)
         if(.not. allocated(anti_x)) then
            allocate(anti_x(1))
            anti_x(1) = nodes(i)%x(j)
            allocate(anti_y(1))
            anti_y(1) = nodes(i)%y(j)
            score = score + 1
         else
            found = .false.
            do k = 1, size(anti_x)
               if(anti_x(k) == nodes(i)%x(j) .and. anti_y(k) == nodes(i)%y(j)) then
                  found = .true.
                  exit
               end if
            end do
            if(.not. found) then
               call append_to_integer_array(anti_x, nodes(i)%x(j))
               call append_to_integer_array(anti_y, nodes(i)%y(j))

               score = score + 1
            end if
         end if
         do l = 1, size(nodes(i)%x)
            if(nodes(i)%x(j) == nodes(i)%x(l) .and. nodes(i)%y(j) == nodes(i)%y(l)) cycle ! don't compare same values
            xdist = nodes(i)%x(j) - nodes(i)%x(l)
            ydist = nodes(i)%y(j) - nodes(i)%y(l)
            next_x = nodes(i)%x(j) + xdist
            next_y = nodes(i)%y(j) + ydist
            do
               if(next_x> 0 .and. next_x <= 50 .and. &
                  next_y > 0 .and. next_y  <= 50) then
                  found = .false.
                  do k = 1, size(anti_x)
                     if(anti_x(k) == next_x .and. anti_y(k) == next_y) then
                        found = .true.
                        exit
                     end if
                  end do
                  if(.not. found) then
                     call append_to_integer_array(anti_x, next_x)
                     call append_to_integer_array(anti_y, next_y)

                     score = score + 1
                  end if
                  next_x = next_x + xdist
                  next_y = next_y + ydist
               else
                  exit
               end if
            end do
            next_x = nodes(i)%x(l) - xdist
            next_y = nodes(i)%y(l) - ydist
            do
               if(next_x > 0 .and. next_x  <= 50 .and. &
                  next_y > 0 .and. next_y <= 50) then
                  found = .false.
                  do k = 1, size(anti_x)
                     if(anti_x(k) == next_x .and. anti_y(k) == next_y) then
                        found = .true.
                        exit
                     end if
                  end do
                  if(.not. found) then
                     call append_to_integer_array(anti_x, next_x)
                     call append_to_integer_array(anti_y, next_y)

                     score = score + 1
                  end if
                  next_x = next_x - xdist
                  next_y = next_y - ydist

               else
                  exit
               end if
            end do
         end do
      end do
   end do
   print*, "Total : ", score
end program day_08
