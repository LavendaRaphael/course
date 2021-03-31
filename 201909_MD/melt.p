reset
#set term pdfcairo font "Arial,12"
set term svg font "Arial,12"
set output 'Temperature.svg'

set xlabel 'Time t(ns)'
set ylabel 'Temperature T(K)'
set key box width 2 height 1
set key samplen 2
workdir='~/201909_MD/20191012/'
p [:] [:12000] workdir.'3000K/Cu_melt.out' every ::25::(24+2000) u ($1*0.000001):2 w l lw 0.5 lc 'blue' t "3000K",\
workdir.'6000K/Cu_melt.out' every ::25::(24+2000) u ($1*0.000001):2 w l lw 0.5 lc 'red' t '6000K',\
workdir.'9000K/Cu_melt.out' every ::25::(24+2000) u ($1*0.000001):2 w l lw 0.5 lc 'black' t '9000K'

unset output
#pause -1
