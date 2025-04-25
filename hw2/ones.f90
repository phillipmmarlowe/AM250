program game_of_life
  implicit none
  integer :: n, i, j
  integer, allocatable :: input(:,:), output(:,:)
  real :: r

  ! Get the size of the array from the user
  print *, 'Enter the size of the square array:'
  read *, n

  ! Allocate the input and output arrays
  allocate(input(n, n), output(n, n))

  ! Fill the input array with random 0s and 1s
  call random_seed()
  do i = 1, n
    do j = 1, n
      call random_number(r)
      input(i,j) = merge(1, 0, r > 0.6)
    end do
  end do

  ! Output the original input array
  print *, 'Input array:'
  do i = 1, n
    print *, input(i,:)
  end do

  ! Call the subroutine to process the array
  call count_neighbors(input, n, output)

  ! Output the resulting array
  print *, 'Output:'
  do i = 1, n
    print *, output(i,:)
  end do

  deallocate(input, output)

contains

  subroutine count_neighbors(input, n, neighbor_counts)
    implicit none
    integer, intent(in) :: n
    integer, intent(in) :: input(n, n)
    integer, intent(out) :: neighbor_counts(n, n)
    integer :: i, j, count, row_offset, col_offset, neighbor_row, neighbor_col

    do i = 1, n
      do j = 1, n
        count = 0
        do row_offset = -1, 1
          do col_offset = -1, 1
            if (row_offset == 0 .and. col_offset == 0) cycle ! Skip the cell itself

            ! Calculate neighbor coordinates with wrap-around
            neighbor_row = i + row_offset
            neighbor_col = j + col_offset

            if (neighbor_row < 1) then
              neighbor_row = n
            elseif (neighbor_row > n) then
              neighbor_row = 1
            end if

            if (neighbor_col < 1) then
              neighbor_col = n
            elseif (neighbor_col > n) then
              neighbor_col = 1
            end if
						
            count = count + input(neighbor_row, neighbor_col)
          end do
        end do
				! Dont remove 1s already present
        if (input(i,j) == 0) then 
				  neighbor_counts(i, j) = count
				else
					neighbor_counts(i, j) = 3
				end if
      end do
    end do
		
		do i = 1, n
      do j = 1, n
      
				if (neighbor_counts(i, j) > 2) then
					neighbor_counts(i, j) = 1
				else 
				  neighbor_counts(i, j) = 0
				end if
			end do
    end do
		
  end subroutine count_neighbors

end program game_of_life
