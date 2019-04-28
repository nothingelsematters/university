# Лабораторная работа 2-5. Поток минимальной стоимости

## A. Максимальный поток минимальной стоимости
```
ограничение по времени на тест 2 секунды
ограничение по памяти на тест 512 мегабайт
ввод mincost.in
вывод mincost.out
```

Задан ориентированный граф, каждое ребро которого обладает пропускной способностью и стоимостью. Найдите максимальный поток минимальной стоимости из вершины с номером 1 в вершину с номером n.

### Входные данные

Первая строка входного файла содержит n и m — количество вершин и количество ребер графа (2 ≤ n ≤ 100, 1 ≤ m ≤ 1000). Следующие m строк содержат по четыре целых числа числа: номера вершин, которые соединяет соответствующее ребро графа, его пропускную способность и его стоимость. Пропускные способности и стоимости не превосходят 105.

### Выходные данные

В выходной файл выведите одно число — цену максимального потока минимальной стоимости из вершины с номером 1 в вершину с номером n. Ответ не превышает 263 - 1. Гарантируется, что в графе нет циклов отрицательной стоимости.

### Пример
#### Входные данные
```
4 5
1 2 1 2
1 3 2 2
3 2 1 1
2 4 2 1
3 4 2 3
```

#### Выходные данные
```
12
```

## B. Задача о назначениях

```
ограничение по времени на тест 2 секунды
ограничение по памяти на тест 256 мегабайт
ввод assignment.in
вывод assignment.out
```
Дана целочисленная матрица C размера n × n. Требуется выбрать n ячеек так, чтобы в каждой строке и каждом столбце была выбрана ровно одна ячейка, а сумма значений в выбранных ячейках была минимальна.

### Входные данные

Первая строка входного файла содержит n (2 ≤ n ≤ 300). Каждая из последующих n строк содержит по n чисел: Cij Все значения во входном файле неотрицательны и не превосходят 106.

### Выходные данные

В первую строку выходного файла выведите одно число — искомая минимизуруемая величина. Далее выведите n строк по два числа в каждой — номер строки и столбца клетки, участвующей в оптимальном назначении.

Пары чисел можно выводить в произвольном порядке.

### Пример
#### Входные данные
```
3
3 2 1
1 3 2
2 1 3
```

#### Выходные данные
```
3
2 1
3 2
1 3
```

## C. Costly Labels
```
time limit per test 2 seconds
memory limit per test 256 megabytes
input standard input
output standard output
```
You've got yourself an unrooted tree with N nodes — that is, a connected, undirected graph with N nodes numbered from 1 to N, and N - 1 edges. The ith edge connects nodes Ai and Bi.

You'd like to spend as little money as possible to label each node with a number from 1 to K, inclusive. It costs Ci, j dollars to label the ith node with the number j.

Additionally, after the whole tree has been labelled, you must pay P more dollars for each node which has at least one pair of neighbours that share the same label as each other. In other words, for each node u, you must pay P dollars if there exist two other nodes v and w which are both adjacent to node u, such that the labels on nodes v and w are equal (note that node u's label is irrelevant). You only pay the penalty of P dollars once for a given central node u, even if it has multiple pairs of neighbours which satisfy the above condition.

What's the minimum cost (in dollars) to label all N nodes?

### Input

Input begins with a line containing the space-separated integers N (1 ≤ N ≤ 1000), K (1 ≤ K ≤ 30), and P (0 ≤ P ≤ 106). Then, N lines follow, the ith of which contains the space-separated integers Ci, 1 through Ci, K in order (0 ≤ Ci, j ≤ 106). Then, N - 1 lines follow, the ith of which contains the space-separated integers Ai and Bi (1 ≤ Ai, Bi ≤ N).

### Output

Print the minimum cost to label all of the tree's nodes.

### Examples
#### Input
```
1 1 1
111
```

#### Output
```
111
```

---

#### Input
```
3 1 8
1
2
4
1 2
2 3
```

#### Output
```
15
```

---

#### Input
```
3 2 10
4 7
8 9
2 3
1 2
2 3
```

#### Output
```
15
```

---

#### Input
```
4 2 99
0 1
0 1
0 1
0 0
4 1
2 4
4 3
```

#### Output
```
99
```

---

#### Input
```
4 3 99
0 1 0
0 1 0
0 1 0
0 0 0
4 1
2 4
4 3
```

#### Output
```
1
```
> Note:
>
>  In the first case, there is only one node which must be painted the only possible color for 111 dollars. In the second case, there is only one color, so a penalty of 8 dollars must be paid since node 2 has two neighbors with the same color. In total we pay 1 + 2 + 4 + 8 = 15 dollars. In the third case, it's optimal to paint nodes 1 and 2 with color 1, and node 3 with color 2. The total cost is 4 + 8 + 3 = 15 dollars.

## D. Камень, ножницы, бумага — 2
```
ограничение по времени на тест 1 секунда
ограничение по памяти на тест 512 мегабайт
ввод rps2.in
вывод rps2.out
```

Год назад Ростислав с Мирославом играли в камень, ножницы, бумагу на щелбаны. За каждый выигранный раунд победитель ставил один щелбан проигравшему. В случае ничьи щелбаны не ставились. Эта игра запомнилась Мирославу как самая худшая игра в его жизни: всю следующую неделю у него болел лоб.

Воспоминания нахлынули на Мирослава, когда он нашел бумажку с шестью числами — запись с той самой игры. Прошло много времени, и теперь Мирослав может спокойно подумать, почему он проиграл так много раз. Но, к сожалению, он не может посчитать точное количество своих поражений, так как он записал только то, что Ростислав показал камень r1 раз, ножницы s1 раз и бумагу p1 раз, а сам Мирослав показал камень r2 раз, ножницы s2 раз и бумагу p2 раз.

Помогите Мирославу узнать по этим данным, какое минимальное количество щелбанов он мог получить в той самой роковой игре.

Для справки, победитель этой игры определяется по следующим правилам:

Если игроки показали одинаковый знак, то засчитывается ничья.

### Входные данные

В первой строке входных данных три целых числа r1, s1, p1. Во второй строке три целых числа r2, s2, p2.

Все числа неотрицательные и не превышают 108, r1 + s1 + p1 = r2 + s2 + p2.

### Выходные данные

Выходные данные должны содержать единственное число  — минимальное количество щелбанов, которые мог получить Мирослав.

### Примеры
#### Входные данные
```
3 0 0
0 3 0
```

#### Выходные данные
```
3
```

## E. Travelling salesmans problem
```
time limit per test 2 seconds
memory limit per test 256 megabytes
input standard input
output standard output
```

You are working in a salesmans company as a programmer.

There are n towns in your country and m directed roads between them. Each road has a cost person should spend on fuel. The company wants to sell goods in all n towns. There are infinetely many salesmans in the company. We can choose some positive number of salesmans and give a non-empty list of towns to each of them. Towns from the list are the towns to sell goods in. Each salesman will visit all the towns in his list in this particular order in cycle (after the last town he will return to the first town and so on). Salesman can visit other towns on his way but he will not sell goods in these towns. Two salesmans cannot sell goods in one town because it will attract unnecessary attention to your company. But for every town there must be a salesman who sell goods in this town. If salesman's list of towns consists of exactly 1 town then he should pay fee to stay in this town each month (each town has its own fee) or he should go for a round trip and spend money on fuel.

Your task is to calculate the minimal amount of money company must spend monthly to achieve its goals. We will assume that every salesman will spend a month to make one cycle.

### Input

The first line of input contains two integers n and m — number of towns and number of directed roads between them respectively (1 ≤ n ≤ 500, 0 ≤ m ≤ n(n - 1)).

The second line contains n numbers ai — monthly fee for i-th town (0 ≤ ai ≤ 109).

Next m lines describes roads. Each description looks like u v cost and describe a road from town u to town v with assigned cost cost (1 ≤ u, v ≤ n, u ≠ v, 0 ≤ cost ≤ 109). It is guaranteed that there are no two roads between the same pair of towns in the same direction.

### Output

Print one integer — the minimal amount of money to spend.
Scoring

n ≤ 5 — 5 points

n ≤ 16 — 25 points

n ≤ 80 — 30 points

n ≤ 500 — 40 points

### Example
#### Input
```
3 3
30 25 30
1 2 3
2 3 5
3 1 10
```

#### Output
```
18
```
