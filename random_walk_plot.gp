set term pdfcairo


#1D
set output "random_walk.pdf"
set title "Random walk"

f(x) = a+b*sqrt(x)
fit f(x) "random_walk2d.dat" via a,b

plot "random_walk1d.dat" using 1:2 w l t "1D", \
"random_walk2d.dat" using 1: 2 w l t "2D" , \
f(x) t "2D fit", "random_walk3d.dat" using 1: 2 w l t "3D"

unset output
unset term