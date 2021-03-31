reset
#set term pdfcairo font "Arial,12"
set term svg font "Arial,17"
set output 'q6_1d.svg'

set xlabel 'q6'
set ylabel 'Probability distributions'
set key box width 2 height 1
set key samplen 2
workdir='~/201909_MD/20191012/'
p [0.1:0.75] [:] workdir.'3000K/q6.boo_1d' w l lw 1 lc 'blue' t "3000K",\
workdir.'6000K/q6.boo_1d' w l lw 1 lc 'red' t '6000K',\
workdir.'9000K/q6.boo_1d' w l lw 1 lc 'black' t '9000K'

unset output
pause -1
