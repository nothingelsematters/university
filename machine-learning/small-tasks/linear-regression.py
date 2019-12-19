from sys import stdin, stdout
from random import random

def input_ints():
    return list(map(int, stdin.readline().split()))

def normalize(x):
    maxs = max([max(i[:-1]) for i in x])
    mins = min([min(i[:-1]) for i in x])

    for i in range(objects_amount):
        for j in range(features_amount):
            x[i][j] = (x[i][j] - mins) / (maxs - mins)

    return x, mins, maxs

def inverse_normalization(a, mins, maxs):
    for i in range(features_amount):
        a[i] /= maxs - mins
    a[-1] -= sum([mins * a[i] for i in range(len(a) - 1)])
    return a

def linear_regression(x, y, epochs=8000, lr=1e-2, gamma=0.9):
    x, mins, maxs = normalize(x)
    w = [0] * len(x[0])
    momentum = [0] * len(x[0])

    for epoch in range(epochs):
        i = int(random() * len(x))
        diff = sum([w[z] * x[i][z] for z in range(len(w))]) - y[i]
        shift = [diff * x[i][j] for j in range(len(x[0]))]

        momentum = [momentum[j] * gamma + lr * shift[j] for j in range(len(momentum))]
        w = [w[j] - momentum[j] for j in range(len(w))]

    return inverse_normalization(w, mins, maxs)

### main ###

# objects_amount, features_amount = input_ints()
features_amount = input_ints()[0]
objects_amount = input_ints()[0]

x = []
y = []

for i in range(objects_amount):
    line = input_ints()
    x.append(line[:-1] + [1])
    y.append(line[-1])

for i in linear_regression(x, y):
    stdout.write('{:.08f}\n'.format(i))
