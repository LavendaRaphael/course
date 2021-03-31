reset
set term pdfcairo font "Arial,12"
set output 'OO.pdf'

set xlabel 'Distance r(Angstroms)'
set ylabel 'gOO(r)'
unset title
p '1_1.rdf' w l lw 0.5 lc 'blue'

unset output
