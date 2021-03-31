colors='black red blue green brown'

#set term pdfcairo font "Arial,12"
set term pdfcairo font "Arial,25" size 8*1,5*1
#set term svg font "Arial-Bold,25" size 800,500

mode=1
#===============================[]
if (mode==1) {
workhome0="/home/tianff/university/202009_NumeriCalculation/hw/exp2/"
set output workhome0."1.2.pdf"
set samples 500
set key box
set key samplen 2
set key width 2
set key height 0.5
set xlabel "y" offset 0,0
set ylabel "x" offset 1,0
#set key 
set xrange [-1:1]
#set yrange [:]
f(x)=1.0/(1.0+25.0*x**2)
a=0.66942
b=-2.30554
c=1.86886
P4(x)=a+b*x**2+c*x**4
p \
f(x) w l lw 3 lc ''.word(colors,1) t 'f(x)',\
P4(x) w l lw 3 lc ''.word(colors,2) t 'P_4(x)'
}
#===============================[]
if (mode==0) {
workhome0="/home/tianff/university/202009_NumeriCalculation/hw/exp2/"
set output workhome0."1.1.pdf"
set samples 500
set key box
set key samplen 2
set key width 2
set key height 0.5
set xlabel "y" offset 0,0
set ylabel "x" offset 1,0
set key c
#set xrange [:]
#set yrange [:]
p \
workhome0.'1.1.out' u 1:2 w l lw 3 lc ''.word(colors,1) t 'f(x)',\
workhome0.'1.1.out' u 1:3 w l lw 3 lc ''.word(colors,2) t 'L_2(x)',\
workhome0.'1.1.out' u 1:4 w l lw 3 lc ''.word(colors,3) t 'L_4(x)',\
workhome0.'1.1.out' u 1:5 w l lw 3 lc ''.word(colors,4) t 'L_8(x)',\
workhome0.'1.1.out' u 1:6 w l lw 3 lc ''.word(colors,5) t 'L_{16}(x)'
}
