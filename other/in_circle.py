from math import *

"""
def in_circle(i, j, (x,y), r):
    if (i-x)**2 + (j-y)**2 <= r*r:
        return 1.0
    else:
        return 0.0

for i in range(10):
    for j in range(10):
        print i, j, in_circle(2*i,2*j, (10, 10), 5)
for i in range(20):
    for j in range(20):
        print i, j, in_circle(i,j, (10, 10), 5)
"""

def min_max(i,j):
    mini = min(i,j)
    maxi = max(i,j)
    
    if ( i == mini ):
        mini = 1
    else:
        mini = 0
    
    if ( i == maxi ):
        maxi = 1
    else:
        maxi = 0
    
    return (mini,maxi)

for i in range(50):
    for j in range(50):
        (a,b) = min_max(i,j)
        print i, j, a, b