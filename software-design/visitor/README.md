# Homework 6

> Goal: to gain practical experience in applying visitor and state behavior patterns.

It is necessary to implement a calculator that is able to transform simple arithmetic expressions into reverse polish record (RWP) and calculate them.

> Example:
>
> `(23 + 10) * 5 – 3 * (32 + 5) * (10 – 4 * 5) + 8 / 2`

The expression may contain parentheses, space characters, numbers and 4 operations: +, -, *, /.

To calculate an expression, it must first be broken down into tokens:

* one token for each bracket and operation;
* token for integer numbers.

> Example:
>
> `(30 + 2) / 8 -> LEFT NUMBER(30) PLUS NUMBER(2) RIGHT DIV NUMBER(8)`

Next, the tokens are converted to EPR, which no longer contains brackets and can be easily calculated using the stack.

`LEFT NUMBER(30) PLUS NUMBER(2) RIGHT DIV NUMBER(8) -> NUMBER(30) NUMBER(2) PLUS NUMBER(8) DIV`

The scheme of work of the calculator:

* the input data set is parsed into separate Tokenizer tokens;
* ParserVisitor bypasses all received tokens and converts them to the reverse polish token entries;
* then tokens are printed by PrintVisitor;
* the value of the expression is calculated by the Calculator.

Visitors can use stacks and other data structures to accumulate in themselves intermediate results.

Tokenizer is easiest to implement as a finite machine that reads one at a time from symbols from the input stream and converts them into tokens. The machine itself is necessary to implement using the State pattern. Scheme of the machine:

Class skeleton:

```java
interface Token {
    void accept(TokenVisitor visitor);
}

interface TokenVisitor {
    void visit(NumberToken token);
    void visit(Brace token);
    void visit(Operation token);
}
```

NumberToken, Brace, Operation implement Token.

All Visitor implements TokenVisitor.

As a result, you need to implement a program that reads the input expression from the console.
and outputs to the console first the expression converted to the reverse Polish notation, a
then the calculated value of the expression. If an incorrect expression was entered,
it is necessary to print out an error.
Read more about OPZ and its conversion:
https://ru.wikipedia.org/wiki/Обратная_польская_запись
