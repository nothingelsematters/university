def harmonic_mean(a, b):
    return 0 if a + b == 0 else 2 * a * b / (a + b)

size = int(input())
elements = [list(map(int, input().split())) for x in range(size)]

row = [sum(i) for i in elements]
column = [sum(map(lambda x: x[i], elements)) for i in range(size)]
full = sum(row)

precision = 0
recall = 0
score = 0

for i in range(size):
    local_precision = 0 if row[i] == 0 else elements[i][i] / row[i]
    local_recall = 0 if column[i] == 0 else elements[i][i] / column[i]
    weight = row[i]

    precision += local_precision * weight
    recall += local_recall * weight
    score += harmonic_mean(local_precision, local_recall) * weight

# macro
print(harmonic_mean(precision, recall) / full)
# micro
print(score / full)
