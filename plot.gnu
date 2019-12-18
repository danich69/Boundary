#set terminal postfile
#set output "plot.ps"
set xlabel "x"
set ylabel "y"
set key left
plot "shooting_forward.dat" u 2:3 w l lt 5 dt 2 lw 3 title 'forward shooting', "shooting_reverse.dat" u 2:3 w l lt 6 dt 3 lw 3 title 'reverse shooting', "finite_differences.dat" u 2:3 w l title 'finite differences'
pause -1 "This is your plot"
