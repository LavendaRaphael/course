colors='black red blue green cyan magenta yellow'
set term pdfcairo font "Arial,25" size 8*1,5*1
set samples 500
set key box
set key samplen 2
set key width 2
set key height 0.5
set key noautotitle
#===============================[]
if (1==1) {
workhome0="~/university/202102_EssayWriting/"
set output workhome0."hw/hw5/temp.pdf"
set xlabel "Column-1 (Unit1)" offset 0,0
set ylabel "Column-2 (Unit2)" offset 3,0
set xrange [:]
set yrange [:]
set format x "%7.0f"
set format y "%7.1f"
p \
workhome0.'slides/20210325_data.txt' u 1:2:(0.5) w lp pt 7 ps 0.5 lc ''.word(colors,1) t 'Exp.'
}
