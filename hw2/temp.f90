program game_of_life
  implicit none
  integer :: n, i, j
  integer, allocatable :: input(:,:), output(:,:), next_gen(:,:)
  real :: r

  ! Get the size of the array from the user
  print *, 'Enter the size of the square array:'
  read *, n

  ! Allocate the input and output arrays
  allocate(input(n, n), output(n, n), next_gen(n,n))

  ! Fill the input array with random 0s and 1s
  call random_seed()
  do i = 1, n
    do j = 1, n
      call random_number(r)
      input(i,j) = merge(1, 0, r > 0.5)
    end do
  end do

  ! Output the original input array
  print *, 'Original input array:'
  do i = 1, n
    print *, input(i,:)
  end do
	
	

  deallocate(input, output, next_gen)

end program game_of_life
