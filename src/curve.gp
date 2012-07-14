set terminal png; set output "Graphics Errors of Linear Regression.png"
set xlabel "Iterations"
set ylabel "MSE"
plot  "curve0.csv" using 1:2 with lines , "curve1.csv" using 1:2 with lines 
