#workhome=''
colors='black red blue green brown'

set term x11 persist
#set term pdfcairo font "Arial,12"
set term svg font "Arial-Bold,25" size 8*110,5*110

mode=1
#===============================[]
if (mode==3) {
set output "C.svg"
unset key
set samples 500
set xlabel "{K_B}T/{/Symbol m}_BB" offset 0,1
set ylabel "C (k_B)" offset 2,0
set xrange [0:5]
set yrange [0:0.5]
p 1.0/x**2*(1-(tanh(1.0/x))**2) lw 2 lc ''.word(colors,1)
}
#===============================[]
if (mode==2) {
set output "S.svg"
unset key
set samples 500
set xlabel "{K_B}T/{/Symbol m}_BB" offset 0,1
set ylabel "S (k_B)" offset 2,0
set xrange [0:5]
set yrange [0:0.8]
p log(2.0*cosh(1.0/x))-1.0/x*tanh(1.0/x) lw 2 lc ''.word(colors,1)
}
#===============================[]
if (mode==1) { 
set output "M.svg"
unset key
set samples 500
set xlabel "{K_B}T/{/Symbol m}_BB" offset 0,1
set ylabel "{/Symbol m}/{/Symbol m}_s" offset 2,0
set xrange [0:5]
set yrange [0:1.1]
p tanh(1.0/x) lw 2 lc ''.word(colors,1) 
}
