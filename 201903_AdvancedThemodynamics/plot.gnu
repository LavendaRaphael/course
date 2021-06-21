array pic[100]
do for [i=1:100] {pic[i]=0}

pic[1]=1 # test.pdf

array colors2=['#FE7D6A', '#81B8E9']
array colors3=['#4D85BD', '#F7903D', '#59A95A']
array colors3_1=['#D22027', '#384589', '#7FA5B7']
array colors4=['#817F00','#FB7E03','#01FD01','#00FFFF']
array colors6=['#EE3624','#323293','#62BB47','#BC8EC0','#8EDAF8','#C7811F']

set samples 500
# set key box
set key samplen 2
set key width 2
set key height 0.5
set key noautotitle
set encoding iso_8859_1
set style data lines

datdir="~/university/201903_AdvancedThemodynamics/server/final/"
outdir="~/university/201903_AdvancedThemodynamics/hw/final/"
#----------------------------------------------------------------------------[]
if (pic[1]==1) {
subdir=''
outfile=outdir.subdir.'p4.pdf'

array mid=['','']
num=|mid|

array datfile[num]
do for [i=1:num] {datfile[i]=mid[i].'ising_200.dat'}
do for [i=1:num] {datfile[i]=datdir.subdir.datfile[i]}

array titl=['Magnetization','Magnetic susceptibility']

array colo[num]
if (num==2) {
    do for [i=1:num] {colo[i]=colors2[i]}
}
if (num==3) {
    do for [i=1:num] {colo[i]=colors3[i]}
}
if (num==4) {
    do for [i=1:num] {colo[i]=colors4[i]}
}
if (num==5 || num==6) {
    do for [i=1:num] {colo[i]=colors6[i]}
}

set term pdfcairo font "Arial,25" size 8*1,5*1
set output outfile
set xlabel "T ({/Symbol e}/k)" offset 0,0
set ylabel "M/M_{max}" offset 3,0
set y2label "{/Symbol c} (arb. units)" offset -1,0
set ytics nomirror 
set y2tics
set xrange [*:*]
set yrange [0:1.1]
set y2range [0:6.0]
set format x "%7.1f"
set format y "%7.1f"
set format y2 "%7.1f"

set style line 1 lw 2
p \
datfile[1] u 1:2 ls 1 lc ''.colo[1] t titl[1] axis x1y1,\
datfile[2] u 1:($3*1000) ls 1 lc ''.colo[2] t titl[2] axis x1y2,\
}
