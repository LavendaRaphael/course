colors='black red blue green cyan magenta yellow'
set term pdfcairo font "Arial,25" size 8*1,8*1
set samples 500
set key box
set key samplen 2
set key width 2
set key height 0.5
set key noautotitle
workhome0="~/university/202009_NumericalMethods/hw/exp3/"
#===============================[]
if (0==1) {
set term pdfcairo size 8*1,5*1
set xlabel "x" offset 0,0
set ylabel "u" offset 1,0
set xrange [:]
set yrange [:]
set output workhome0."3.pdf"
f(x)=exp(-pi**2)*sin(pi*x)
set key b c
p \
f(x) w l lc ''.word(colors,1) lw 3 dt 1 t 'exp(-{/Symbol p}^2)sin{/Symbol p}x',\
workhome0."3.0.1.out" u 1:2 w l lc ''.word(colors,1) lw 3 dt 2 t 'Forward,{/Symbol l}=0.1',\
workhome0."3.0.1.out" u 1:3 w l lc ''.word(colors,1) lw 3 dt 3 t 'Crank-Nicolson,{/Symbol l}=0.1',\
workhome0."3.0.5.out" u 1:2 w p lc ''.word(colors,1) lw 3 pt 14 t 'Forward,{/Symbol l}=0.5',\
workhome0."3.0.5.out" u 1:3 w p lc ''.word(colors,1) lw 3 pt 4 t 'Crank-Nicolson,{/Symbol l}=0.5'
}
#===============================[]
if (1==1) {
set term pdfcairo size 8*2,5*2
set xlabel "x" offset 0,0
set ylabel "y" offset 1,0
set xrange [:]
set yrange [:]
set view equal xyz
set key c
set output workhome0."2.pdf"
set multiplot layout 2,2
p \
workhome0."2.out" u 2:3 w l lc ''.word(colors,1) lw 3 dt 1 t 'Trapezoid'
p \
workhome0."2.out" u 4:5 w l lc ''.word(colors,1) lw 3 dt 1 t 'Euler'
p \
workhome0."2.out" u 6:7 w l lc ''.word(colors,1) lw 3 dt 1 t 'Runge-Kutta'
unset multiplot
}
#===============================[]
if (0==1) {
set term pdfcairo size 8*1,15*1
set xlabel "x" offset 0,0
set ylabel "y" offset 1,0
set xrange [:]
set yrange [:]
set view equal xyz
set key c
set output workhome0."2.pdf"
set multiplot layout 3,1
p \
workhome0."2.out" u 2:3 w l lc ''.word(colors,1) lw 3 dt 1 t 'Trapezoid'
p \
workhome0."2.out" u 4:5 w l lc ''.word(colors,1) lw 3 dt 1 t 'Euler'
p \
workhome0."2.out" u 6:7 w l lc ''.word(colors,1) lw 3 dt 1 t 'Runge-Kutta'
unset multiplot
}
#===============================[]
if (0==1) {
set term pdfcairo size 8*1,5*1
set output workhome0."1.2.pdf"
set xlabel "x" offset 0,0
set ylabel "y" offset 1,0
set xrange [0:0.3]
set yrange [0:100]
f(x)=exp(-50.0*x)*100.0
p \
f(x) w l lc ''.word(colors,1) lw 3 dt 1 t 'y=100exp(-50x)',\
workhome0."1.0.05.out" u 1:3 w l lc ''.word(colors,1) lw 3 dt 2 t 'h=0.05',\
workhome0."1.0.04.out" u 1:3 w l lc ''.word(colors,1) lw 3 dt 3 t 'h=0.04',\
workhome0."1.0.03.out" u 1:3 w l lc ''.word(colors,1) lw 3 dt 5 t 'h=0.03'
}
#===============================[]
if (0==1) {
set term pdfcairo size 8*1,7*1
set output workhome0."1.1.pdf"
set xlabel "x" offset 0,0
set ylabel "y" offset 1,0
set xrange [0:0.3]
set yrange [-150:170]
f(x)=exp(-50.0*x)*100.0
p \
f(x) w l lc ''.word(colors,1) lw 3 dt 1 t 'y=100exp(-50x)',\
workhome0."1.0.05.out" u 1:2 w l lc ''.word(colors,1) lw 3 dt 2 t 'h=0.05',\
workhome0."1.0.04.out" u 1:2 w l lc ''.word(colors,1) lw 3 dt 3 t 'h=0.04',\
workhome0."1.0.03.out" u 1:2 w l lc ''.word(colors,1) lw 3 dt 5 t 'h=0.03'
}
