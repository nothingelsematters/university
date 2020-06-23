# script file

pkg load statistics;

k = 5;
c = 6.85;
g = 0.85;
a = 7;
f = @(x) log(a * x + 1);

function I = interval(n, p, g)
  T = norminv((1 + g) / 2);
  d = T * sqrt(p * (1 - p) / n);
  I = [p - d, p + d];
endfunction

function run(n, k, c, g, f)
  x = rand(n, k);
  y = sum(f(x), 2);
  p = sum(y < c) / n
  I = interval(n, p, g)
endfunction

for n = [10 ^ 4, 10 ^ 6]
  n
  run(n, k, c, g, f);
endfor
