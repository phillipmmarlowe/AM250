

! begin program description
program test_integration
	! standard op
  implicit none
	! classic statement
  real :: result, a, b
	integer :: num_steps
	print *, 'Enter lwr bnd (a): '
	read *, a
	print *, 'Enter upr bnd (b): '
	read *, b
	print *, 'Enter number of steps (whole number): '
	read *, num_steps
	! get result to print and print
  result = trap_integral(f, a, b, num_steps)
  print *, 'Integral of f(x) = x^2 from', a, 'to', b, 'is', result
	! define what we just used above ie f and trap_integral
contains

  function f(x)
    real :: f
    real, intent(in) :: x
    !f = x ** 2
		f = sin(x)
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