module sorting 
  ! apparently you need to put the subroutine in a module
  implicit none
  contains 
  subroutine sort(arr)
      !! a little insertion sort KISS
      !! also yea we have no in built sorting
    implicit none
    integer, intent(inout) :: arr(1000)
    integer :: i, j, key
    do i = 2, 1000 
      key = arr(i)
      j = i - 1
      do while (j > 0 .and. arr(j) > key)
        arr(j+1) = arr(j)
        j = j - 1
      end do
      arr(j+1) = key
    end do
  end subroutine sort
end module sorting

program day_01
  use sorting
  implicit none
  integer :: io, i, dist, sim, left(1000), right(1000) ! yea we are coding to the input file and not generic
  open(newunit=io, file='./day_01_input.txt', status='old', action='read')

  do i = 1, 1000
    read(io, *) left(i), right(i) ! read both values in line
  end do

  close(10) ! close the file

  call sort(left)
  call sort(right)

  ! if we dont init these bad things happen on multiple runs
  dist = 0 
  sim = 0

  do i = 1, 1000
   dist = dist + abs(left(i) - right(i)) 
   sim = sim + left(i) * count(right == left(i)) 
  end do

    print*, "distance: ", dist
    print*, "similarity: ", sim
end program day_01

