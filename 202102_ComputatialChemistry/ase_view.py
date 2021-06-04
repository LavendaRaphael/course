#!/user/bin/env python
from ase.visualize import view
from ase.io import read
import sys

cont = []
for x in sys.argv[1:]:
    cont.append(read(str(x)))

#cont=read(str(sys.argv[1]),index=':')
view(cont,repeat=(2,2,1))
