 set title 'Integration Result'
 set xlabel 'Number of steps'
 set ylabel 'Integral value'
 plot 'coords.dat' using 1:2 with linespoints title 'f(x)'
