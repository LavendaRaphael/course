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

datdir="~/university/202102_ComputatialChemistry/server/hw3/"
outdir="~/university/202102_ComputatialChemistry/hw/"
#----------------------------------------------------------------------------[]
if (pic[1]==1) {
subdir=''
outfile=outdir.subdir.'reaction.pdf'

array mid=['Pt','Pd','Ni']
num=|mid|

array datfile[num]
do for [i=1:num] {datfile[i]='Cu_'.mid[i].'_CH_vac.neb/deltaE.dat'}
do for [i=1:num] {datfile[i]=datdir.subdir.datfile[i]}

array labelfile[num]
do for [i=1:num] {labelfile[i]='Cu_'.mid[i].'_CH_vac.neb/label.dat'}
do for [i=1:num] {labelfile[i]=datdir.subdir.labelfile[i]}

array titl[num]
do for [i=1:num] {titl[i]=mid[i]}

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

set term pdfcairo font "Arial,25" size 6*1,5*1
set output outfile
set xlabel "" offset 0,0
set ylabel "Energy (eV)" offset 2,0
set xrange [*:*]
set yrange [-0.1:*]
set format y "%7.2f"
set style line 1 lw 2
set style line 2 lw 2 dt 2
set key l t
set xtics ('CH_3*'  1,'TS1'  4,'CH_2*'  7   ,'TS2'  10,'CH*'  13,'TS3'  16,'C*'  19)

p \
for [i=1:num] datfile[i] u (column(1)):(column(3)) ls 2 lc ''.colo[i],\
for [i=1:num] datfile[i] u (column(1)):(column(2)) ls 1 lc ''.colo[i] t titl[i],\
for [i=1:num] labelfile[i] u 1:($2+$3):4 w labels offset char 0,0.5 tc ''.colo[i],\
}

