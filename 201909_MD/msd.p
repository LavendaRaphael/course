reset
#set term pdfcairo font "Arial,12"
set term svg font "Arial,17"
set output 'msd_3000K.svg'

set xlabel 'Time t(ns)'
set ylabel 'MSD (angstrom^2)'
set key box width 2 height 1
set key samplen 2
set key top left
workdir='~/201909_MD/20191012/'
p [:] [:] workdir.'3000K/msd.out' u ($1*0.000001):2 w p ps 0.5 lc 'blue' t '3000K'
#w l lw 1 lc 'blue' t "3000K"#,\
#workdir.'6000K/msd.out' u ($1*0.000001):2 w l lw 1 lc 'red' t '6000K',\
#workdir.'9000K/msd.out' u ($1*0.000001):2 w l lw 1 lc 'black' t '9000K'

unset output
pause -1
