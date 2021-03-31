colors='black red blue green cyan magenta yellow'
set term pdfcairo font "Arial,25" size 8*1,6*1
set samples 500
set key box
set key samplen 2
set key width 0
set key height 0.5
set key noautotitle
workhome0='~/university/202009_ComputeCata/hw/'
#===============================[]
if (1==1) {
set output workhome0."hw3/C.pdf"
set xlabel "E_{ads}(C) (eV)" offset 0,0
set ylabel "E_{ads}(CH_x) (eV)" offset 1,0
set key b r
set key nobox
set xrange []
set yrange [-7.5:]
f1(x)=a1*x+b1
f2(x)=a2*x+b2
f3(x)=a3*x+b3
fit f1(x) workhome0."hw3/C.dat" using 2:3 via a1,b1
fit f2(x) workhome0."hw3/C.dat" using 2:4 via a2,b2
fit f3(x) workhome0."hw3/C.dat" using 2:5 via a3,b3
p \
workhome0.'hw3/C.dat' u 2:3:(0.05) w circle lc ''.word(colors,1) t 'CH',\
workhome0.'hw3/C.dat' u 2:4 pt 1 lc ''.word(colors,1) t 'CH_2',\
workhome0.'hw3/C.dat' u 2:5 pt 2 lc ''.word(colors,1) t 'CH_3',\
f1(x) w l lw 3 lc ''.word(colors,1) t sprintf("E_{ads}(CH)=%1.2f*E_{ads}(C)%1.2f",a1,b1),\
f2(x) w l lw 3 lc ''.word(colors,1) dt 2 t sprintf("E_{ads}(CH_2)=%1.2f*E_{ads}(C)%1.2f",a2,b2),\
f3(x) w l lw 3 lc ''.word(colors,1) dt 3 t sprintf("E_{ads}(CH_3)=%1.2f*E_{ads}(C)%1.2f",a3,b3)
}
#===============================[]
if (0==1) { 
set output workhome."2d.pdf"
set key b r
f(x)=a*x+b
fit f(x) workhome."2d.txt" using 2:3 via a,b
set xlabel "{/Symbol D}E (eV)" offset 0,0
set ylabel "E_a (eV)" offset 1,0
set xrange []
set yrange [0.4:2.2]
p workhome."2d.txt" u 2:3:(0.025) w circle lw 1 lc ''.word(colors,1) t "Data",\
f(x) w l lw 2 lc ''.word(colors,2) t sprintf("E_a=%1.2f*{/Symbol D}E+%1.2f",a,b)
}
