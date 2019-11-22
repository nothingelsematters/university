from itertools import accumulate

read_ints = lambda: list(map(int, input().split()))

objects, classes, groups = read_ints()
cl = [0] * classes
ob = read_ints()

for i in ob:
    cl[i - 1] += 1

cl = list(accumulate([0] + cl))
gr = [[] for x in range(groups)]

for i in range(len(ob)):
    tmp = ob[i] - 1
    c = cl[tmp]
    gr[c % groups].append(i + 1)
    cl[tmp] += 1

for i in gr:
    print(len(i), end=' ')
    for j in i:
        print(j, end=' ')
    print()
