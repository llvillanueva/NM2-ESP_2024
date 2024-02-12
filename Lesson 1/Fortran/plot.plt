# plot.plt
set term png
set output "error1.png"
set title "Errors for FT ODE solver for dy/dt = -λy"
set xlabel "Time"
plot "FT.txt" using 1:2 with lines title "dt = 0.01"