# plot.plt
set term png
set output "FT_error1.png"
set title "Errors for FT ODE solver for dy/dt = -λy"
set xlabel "Time"
plot "FT_Error01.txt" using 1:2 with lines title "dt = 0.01"

# plot.plt
set term png
set output "FT_error2.png"
set title "Errors for FT ODE solver for dy/dt = -λy"
set xlabel "Time"
plot "FT_Error02.txt" using 1:2 with lines title "dt = 15.0"

# plot.plt
set term png
set output "FT_error3.png"
set title "Errors for FT ODE solver for dy/dt = -λy"
set xlabel "Time"
plot "FT_Error03.txt" using 1:2 with lines title "dt = 25.0"

# plot.plt
set term png
set output "BT_error1.png"
set title "Errors for BT ODE solver for dy/dt = -λy"
set xlabel "Time"
plot "BT_Error01.txt" using 1:2 with lines title "dt = 0.01"

# plot.plt
set term png
set output "BT_error2.png"
set title "Errors for BT ODE solver for dy/dt = -λy"
set xlabel "Time"
plot "BT_Error02.txt" using 1:2 with lines title "dt = 15.0"

# plot.plt
set term png
set output "BT_error3.png"
set title "Errors for BT ODE solver for dy/dt = -λy"
set xlabel "Time"
plot "BT_Error03.txt" using 1:2 with lines title "dt = 25.0"
