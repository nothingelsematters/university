from math import e, pi, hypot, cos

### functions ###

def in_range_one(variable, value):
    return value if abs(variable) < 1 else 0

kernel_functions = {
    "uniform":      (lambda u: in_range_one(u, 0.5)),
    "triangular":   (lambda u: in_range_one(u, 1 - abs(u))),
    "epanechnikov": (lambda u: in_range_one(u, 0.75 * (1 - u ** 2))),
    "quartic":      (lambda u: in_range_one(u, (15 / 16) * (1 - u ** 2) ** 2)),
    "triweight":    (lambda u: in_range_one(u, (35 / 32) * (1 - u ** 2) ** 3)),
    "tricube":      (lambda u: in_range_one(u, (70 / 81) * (1 - abs(u) ** 3) ** 3)),
    "gaussian":     (lambda u: (1 / (2 * pi) ** (0.5)) * e ** (-0.5 * u ** 2)),
    "cosine":       (lambda u: in_range_one(u, (pi / 4) * cos(pi * u / 2))),
    "logistic":     (lambda u: 1 / (e ** u + 2 + e ** (-u))),
    "sigmoid":      (lambda u: 2 / (pi * (e ** u + e ** (-u))))
}

distance_functions = {
    "manhattan": (lambda f, s: sum([abs(f[i] - s[i]) for i in range(len(f))])),
    "euclidean": (lambda f, s: sum([(f[i] - s[i]) ** 2 for i in range(len(f))]) ** 0.5),
    "chebyshev": (lambda f, s: max([abs(f[i] - s[i]) for i in range(len(f))]))
}

window_type = {
    "fixed": True,
    "variable": False
}

### utils ###

def nonparametric_regression(data, request, distance, kernel, window_type, width):
    window = width if window_type else sorted([distance(request, i[:-1]) for i in data])[width]

    denominator = [kernel(distance(i[:-1], request) if window == 0 else distance(i[:-1], request) / window) for i in data]
    numerator = sum([data[i][-1] * denominator[i] for i in range(len(data))])
    denominator = sum(denominator)

    return numerator / denominator if denominator != 0 else sum([i[-1] for i in data]) / len(data)

def input_ints():
    return list(map(int, input().split()))

### reading ###

objects_amount, property_amount = input_ints()
data = [input_ints() for _ in range(objects_amount)]
request = input_ints()

df = distance_functions[input()]
kf = kernel_functions[input()]
wt = window_type[input()]
width = int(input())

### working ###

print(nonparametric_regression(data, request, df, kf, wt, width))
