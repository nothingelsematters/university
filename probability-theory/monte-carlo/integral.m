# script file

g = 0.95;

function I = interval(approx, T, y, n)
    d = T * std(y) / sqrt(n);
    I = [approx - d, approx + d];
endfunction

function I = first(f, n, bottom, up, g)
    real = quad(f, bottom, up)
    x = exprnd(1, n, 1);
    y = x .^ 4;
    approx = mean(y)
    T = expinv((1 + g) / 2, 1);
    I = interval(approx, T, y, n)
endfunction

function I = second(f, n, bottom, up, g, a, s)
    real = quad(f, bottom, up)
    x = normrnd(a, s, n, 1);
    y = 2 * sqrt(pi * abs(x));
    approx = mean(y)
    T = norminv((1 + g) / 2);
    I = interval(approx, T, y, n)
endfunction

for n = [10 ^ 4, 10 ^ 6]
    n
    first(@(x) x .^ 4 .* exp(-x), n, 0, Inf, g);
    second(@(x) sqrt(abs(x)) .* exp(-(x + 2) ^ 2 / 4), n, -Inf, Inf, g, -2, sqrt(2));
endfor
