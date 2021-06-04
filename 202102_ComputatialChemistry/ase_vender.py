#!/user/bin/env python
from ase.io import read,write
import sys

cont=read(str(sys.argv[1]))
write('vender.pov',cont*(2,2,1)).render()
