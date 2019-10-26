## Функциональный язык

Придумайте примитивный функциональный язык программирования и
реализуйте его трансляцию в императивный язык.

#### Пример:

```haskell
fac :: Integer -> Integer
fac 0 = 1
fac n | n > 0 = n * fac(n - 1)
```

#### Вывод:

```pascal
function fac(n: integer): integer;
begin
    if n = 0 then
        fac := 1
    else if n > 0 then
        fac := n * fac(n - 1);
end;
```
