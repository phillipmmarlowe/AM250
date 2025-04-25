program test_integration
  implicit none

  ! Declarations
  character(len=20) :: datafile, gpfile
  real :: result, a, b
  integer :: num_steps, n, ns
  integer :: func_choice

  ! File names
  datafile = 'coords.dat'
  gpfile = 'plot.gp'

  ! Ask for function choice
  print *, 'Choose a function to integrate:'
  print *, '1. f(x) = x^2'
  print *, '2. f(x) = sin(x)'
  read *, func_choice

  ! Input bounds
  print *, 'Enter lower bound (a): '
  read *, a
  print *, 'Enter upper bound (b): '
  read *, b

  ! Write data to file
  open(unit=10, file=datafile, status='replace')
  do n = 1, 8
    ns = 2 ** n
    result = trap_integral(f, a, b, ns, func_choice)
    write(10,*) ns, result
  end do
  close(10)

  ! Gnuplot script
  open(unit=20, file=gpfile, status='replace')
  write(20,*) "set title 'Integration Result'"
  write(20,*) "set xlabel 'Number of steps'"
  write(20,*) "set ylabel 'Integral value'"
  write(20,*) "plot '" // trim(datafile) // "' using 1:2 with linespoints title 'f(x)'"
  close(20)

  ! Call gnuplot
  call execute_command_line("gnuplot -persist " // trim(gpfile))

contains

  ! Function f(x)
  function f(x, choice)
    real :: f
    real, intent(in) :: x
    integer, intent(in) :: choice
    select case (choice)
      case (1)
        f = x ** 2
      case (2)
        f = sin(x)
      case default
        f = 0.0
    end select
  end function f

  ! Trapezoidal rule function
  function trap_integral(f, a, b, n, choice) result(integral)
    implicit none
    interface
      function f(x, choice)
        real :: f
        real, intent(in) :: x
        integer, intent(in) :: choice
      end function f
    end interface
    real, intent(in) :: a, b
    integer, intent(in) :: n, choice
    real :: integral, h, x
    integer :: i

    h = (b - a) / real(n)
    integral = 0.5 * (f(a, choice) + f(b, choice))
    do i = 1, n - 1
      x = a + i * h
      integral = integral + f(x, choice)
    end do
    integral = integral * h
  end function trap_integral

end program test_integration
