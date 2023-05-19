from traceback import print_last


sumi = 0
for i in range(1, 10):
    sumx = 0
    for x in range(i, 10):
        sumx += x
    sumj = 0
    for j in range(1, sumx):
            sumj += j
    sumi += sumj
print(sumi)