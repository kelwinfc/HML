def representacion(i):
    r = bin(i)[2:]
    r = "0"*(32-len(r)) + r
    
    rr = ""
    for i in r:
        rr += " " + i
    return rr

def es_par(i):
    if i % 2 == 0:
        return 1
    else:
        return 1

for i in range(1000):
    print representacion(i), es_par(i)
