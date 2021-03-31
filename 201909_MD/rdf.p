reset
#set term pdfcairo font "Arial,12"
set term svg font "Arial,12"
set output 'rdf_Cu-Cu.svg'

set xlabel 'Distance r(Angstroms)'
set ylabel 'g\_Cu-Cu(r)'
set key box width 2 height 1
set key samplen 2
workdir='~/201909_MD/20191012/'
p [0:8] [:] workdir.'3000K/1_1.rdf' w l lw 1 lc 'blue' t "3000K",\
workdir.'6000K/1_1.rdf' w l lw 1 lc 'red' t '6000K',\
workdir.'9000K/1_1.rdf' w l lw 1 lc 'black' t '9000K',1

unset output
#pause -1
