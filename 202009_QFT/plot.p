#workhome=''
colors='black red blue green brown'

set term x11 persist
#set term pdfcairo font "Arial,12"
set term svg font "TimesNewRoman, 30" size 8*110,5*110

mode=3

#===============================[brilliun]
if (mode==3) {
set output "kspace.svg"
set size square
set samples 500
unset xtics
unset ytics
unset key
#set xzeroaxis
#set yzeroaxis
#set xlabel "kx" offset 0,0
#set ylabel "ky" offset 0,0
set arrow from  -4,0 to  4,0 size 0.25,15
set label "k_x" at 3.5,-0.5 
set arrow from  0,-4 to  0,4 size 0.25,15
set label "k_y" at -0.5,3.5
unset border
set arrow from  0,0 to sqrt(3),3 size 0.25,15 lw 2 lc ''.word(colors,1)
set arrow from  0,0 to sqrt(3),-3 size 0.25,15 lw 2 lc ''.word(colors,1)
set object 8 polygon \
from 0,2 \
to sqrt(3),1 \
to sqrt(3),-1 \
to 0,-2 \
to -sqrt(3),-1 \
to -sqrt(3),1 \
to 0,2 lw 2 fc rgb ''.word(colors,1)
set label "M" at sqrt(3),0 point pointtype 7 pointsize 0.5
set label "K" at sqrt(3),1 point pointtype 7 pointsize 0.5
set label "b_1" at 1,3
set label "b_2" at 1,-3
set label "G" at 0,0 point pointtype 7 pointsize 0.5
p [-4:4] [-4:4] 15
}
#===============================[dos]
if (mode==2) {
set output "dos.svg"
set samples 500
set xlabel 'E/t'
set ylabel 'DOS'
set xtics offset 0, 0.0
set xlabel offset 0, 0.5
set ytics offset 0.0, 0
set ylabel offset 2.0, 0
unset key
p \
[] \
[] \
"dos.out" u 1:($2*0.5) w l lw 2 lc ''.word(colors,1),\
"dos.out" u ($1*(-1.0)):($2*0.5) w l lw 2 lc ''.word(colors,1)
}
#===============================[band]
if (mode==1) { 
set output "t.svg"
unset key
set samples 500
set ylabel 'E/t'
w1=0.0
w2=1.0/sqrt(3.0)
w3=w2+2.0/3.0
w4=w3+1.0/3.0
f1(x)=x>=w1&&x<=w2? sqrt(5.0-4.0*cos(sqrt(3.0)*pi*x)) : 1/0
f2(x)=x>=w2&&x<=w3? sqrt(3.0+2.0*cos(pi*(x-w2))+4.0*cos(3.0*pi/2.0*(x-w2))*cos(pi/2.0*(x-w2))) : 1/0
f3(x)=x>=w3&&x<=w4? sqrt(3.0+2.0*cos((1.0/3.0-(x-w3))*2.0*pi)-4.0*cos((1.0/3.0-(x-w3))*pi)) : 1/0
m1(x)=-f1(x)
m2(x)=-f2(x)
m3(x)=-f3(x)
set xtics font "TimesNewRoman, 44" 
set xtics offset 0, 0.3 
set ytics font "TimesNewRoman, 40" 
set ylabel font "TimesNewRoman, 48" 
set ylabel offset 1.0, 0 
set xtics ("M" w1, "G" w2, "K" w3, "M" w4) 
set arrow from w2,-3 to w2,3 nohead lt 0 lw 2 lc ''.word(colors,1)
set arrow from w3,-3 to w3,3 nohead lw 2 lc ''.word(colors,1)
set arrow from w1,0 to w4,0 nohead lw 2 lc ''.word(colors,1)
p \
[w1:w4] \
[] \
f1(x) lw 2 lc ''.word(colors,4),\
m1(x) lw 2 lc ''.word(colors,4),\
f2(x) lw 2 lc ''.word(colors,2),\
m2(x) lw 2 lc ''.word(colors,2),\
f3(x) lw 2 lc ''.word(colors,3),\
m3(x) lw 2 lc ''.word(colors,3)
}
