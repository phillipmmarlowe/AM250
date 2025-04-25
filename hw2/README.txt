trap.f90:

assuming you have gfortran installed:

gfortran -o runme trap_x2.f90

./runme

gfortran -o runme trap_sinx.f90

./runme

This will ask for start, end, and step size points for the integral.

If you want to see a graph for step values between 2^n where n = [0,8] (will still ask for range and what func to uses) run:

gfortran -o runme trap_plot.f90

./runme

ones.f90:

To run (it will ask for size):

gfortran -o runme ones.f90

./runme

Q1:

The right answers are:

integral of x^2 from 0 to 2: 8/3
integral of sinx from 0 to pi: 2