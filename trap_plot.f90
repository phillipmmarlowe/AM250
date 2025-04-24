

! begin program description
program test_integration
	! standard op
  implicit none
	! gnuplot stuff
  character(len=20) :: datafile, gpfile
	datafile = 'coords.dat'
  gpfile = 'plot.gp'
	! classic statement
  real :: result, a, b
	integer :: num_steps, n, ns
	print *, 'Enter lwr bnd (a): '
	read *, a
	print *, 'Enter upr bnd (b): '
	read *, b
	open(unit=10, file=datafile, status='replace')
	! get result to print and print
	do n = 1, 8	
	  ns = 2 ** n
		result = trap_integral(f, a, b, ns)
		write(10,*) ns, result
		!print *, 'Integral of f(x) = x^2 from', a, 'to', b, 'is', result, 'with step', ns
	end do
	
	! Write Gnuplot script	
  open(unit=20, file=gpfile, status='replace')
  write(20,*) "set title 'Plot of x^2'"
  write(20,*) "set xlabel 'num steps'"
  write(20,*) "set ylabel 'x^2'"
  write(20,*) "plot '" // trim(datafile) // "' using 1:2 with linespoints title 'x^2'"
  close(20)

  ! Call Gnuplot
  call execute_command_line("gnuplot -persist " // trim(gpfile))
	
	! define what we just used above ie f and trap_integral
contains

  function f(x)
    real :: f
    real, intent(in) :: x
    f = x ** 2
		!f = sin(x)
  end function f
  
  ! Trap_rule_integral
	function trap_integral(f, a, b, n) result(integral)
		! standard
		implicit none
		! explain what f is
		interface
			 function f(x)
				 real :: f
				 real, intent(in) :: x
			 end function f
		end interface
		! explain what the other inputs are
		real, intent(in) :: a, b
		integer, intent(in) :: n
		! define variables
		real :: integral, h, x
		integer :: i

		! math time
		integral = 0.0
		h = (b - a) / real(n)

		integral = 0.5 * (f(a) + f(b))
		do i = 1, n - 1
			 x = a + i * h
			 integral = integral + f(x)
		end do

		integral = integral * h
	end function trap_integral

end program test_integration