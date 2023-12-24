from z3 import *

# Real ended up being faster than Int.
x = Real('x')
y = Real('y')
z = Real('z')

dx = Real('dx')
dy = Real('dy')
dz = Real('dz')

t0 = Real('t0')
t1 = Real('t1')
t2 = Real('t2')

ax  = 380596900441035
ay  = 475034410013298
az  = 238677466991589
adx = -141
ady = -244
adz = 154

bx  = 233796913851006
by  = 262774170759556
bz  = 265925724673108
bdx = 54
bdy = 10
bdz = 23

cx  = 276006064958748
cy  = 296055609314709
cz  = 391999646036593
cdx = 14
cdy = 21
cdz = 24

solver = Solver()
solver.add(x + t0*dx == ax + t0*adx)
solver.add(y + t0*dy == ay + t0*ady)
solver.add(z + t0*dz == az + t0*adz)
solver.add(x + t1*dx == bx + t1*bdx)
solver.add(y + t1*dy == by + t1*bdy)
solver.add(z + t1*dz == bz + t1*bdz)
solver.add(x + t2*dx == cx + t2*cdx)
solver.add(y + t2*dy == cy + t2*cdy)
solver.add(z + t2*dz == cz + t2*cdz)

res = solver.check()
if res != sat:
    raise "unsolved"

print(solver.model().eval(x+y+z))

# pos 24, 13, 10 and velocity -3, 1, 2
