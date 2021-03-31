head='Timestep Temp Press Volume PotEng KinEng TotEng Enthalpy'
workdir='~/201909_MD/20191012/'
sp workdir.'3000K/q4_q6.boo_2d' w l lc 'red',\
workdir.'6000K/q4_q6.boo_2d' w l lc 'black',\
workdir.'9000K/q4_q6.boo_2d' w l lc 'blue'

pause -1

