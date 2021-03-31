#workhome='~/tianff/201903/tianff/'
colors='black red blue green brown'

#set term pdfcairo font "Arial,12"
#set term svg font "Arial-Bold,25" size 8*110,5*110

#set key box
set key samplen 2

#set output "6912.svg"
set samples 500
set xlabel 'Frequency {/Symbol w}'
set ylabel 'Intensity (Arb. Units)'
w1=6.0
w2=8.0
w3=12.0
y2=1
f2(x)=x>w2&&x<=w3? 1.0/pi*1.0/sqrt((w3-w2)*(x-w1))*EllipticK(sqrt((w3-x)*(w2-w1)/(w3-w2)/(x-w1))) : 1/0
f1(x)=x>=w1&&x<w2? 1.0/pi*1.0/sqrt((w3-x)*(w2-w1))*EllipticK(sqrt((w3-w2)*(x-w1)/(w3-x)/(w2-w1))) : 1/0
if (w1 != w2) {
        set arrow from w1,0 to w1,f1(w1) nohead lw 2 lc ''.word(colors,2)
} else {
        set arrow from w1,0 to w1,y2 nohead lw 2 lc ''.word(colors,2)
}
set arrow from w3,0 to w3,f2(w3) nohead lw 2 lc ''.word(colors,2)
p [5:13] [0:y2] \
f1(x) lw 2 lc ''.word(colors,2) t "Analytical {/Symbol w}_1-{/Symbol w}_2",\
f2(x) lw 2 lc ''.word(colors,2) t "Analytical {/Symbol w}_2-{/Symbol w}_3",\
"anisotropy_6812.out" u 1:2:(0.05) w circle lw 1 lc ''.word(colors,1) t "Numerical"

#set output '2d.svg'
#the=0.25*pi
#set arrow 1 from 0,0 to sqrt((sin(the))**2+(2*cos(the))**2)*cos(the),sqrt((sin(the))**2+(2*cos(the))**2)*sin(the)
#set object 1 circle at 0,0 radius 0.2 arc[45:90] fc rgb "black"
#set polar
#set label 'a' at 2.1,0 center
#set label 'b' at 0,1.1 center
#set label '{/Symbol q}' at 0.1,0.3 center
#set label 'r' at 0.55,0.7 center
#set grid r polar 60
#set size square
#unset xtics
#unset ytics
#set border 0
#unset key
#p sqrt((sin(t))**2+(2*cos(t))**2) lw 2 lc ''.word(colors,1),\
#'ab' u 1:2 w l lw 2 lc ''.word(colors,1),\
#'ab' u ($3*pi):4 w l lw 2 lc ''.word(colors,1)

pause -1
#unset output
