head='Step Temp PotEng TotEng Press Volume'
workdir='~/tianff/20191021/'
do for [c=1:6] {
set title word(head,(c))
p workdir.'output' every ::(69-1)::(195-1) u 1:c w l,\

pause -1
}
