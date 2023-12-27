# https://theory.stanford.edu/~nikolaj/programmingz3.html#sec-intro
# %%
import z3
from z3 import *
# %%

def parseFile():
    lines = []
    for line in open("input.txt").readlines():
        a,b = line.strip().split(" @ ")
        pos = [int(x) for x in a.replace(' ','').split(",")]
        vel = [int(x) for x in b.replace(' ','').split(",")]
        lines.append((pos,vel))
    return lines
lines = parseFile()

# %%


x = z3.Int('x')
y = z3.Int('y')
z = z3.Int('z')

result = z3.Int('result')

vx = z3.Int('vx')
vy = z3.Int('vy')
vz = z3.Int('vz')

s = Solver()

s.add(result == x + y + z)
for i, ((xi,yi,zi),(vxi,vyi,vzi)) in enumerate(lines):
    ti = z3.Int(f't{i}')
    s.add(ti > 0)
    s.add(x + ti * vx == xi + ti * vxi)
    s.add(y + ti * vy == yi + ti * vyi)
    s.add(z + ti * vz == zi + ti * vzi)

# %%
# print(s.sexpr())
# %%
print('solving...')
s.check()
model = s.model()
print(model[result])

# %%
