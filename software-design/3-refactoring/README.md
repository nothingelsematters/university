# Homework 3

> Goal: to gain practical experience in applying refactoring techniques.

[Download the application](https://github.com/akirakozov/software-design/tree/master/java/refactoring)

The application is a simple web-server, which stores information about the
products and their price. Such methods are supported:

+ `http://localhost:8081/get-products` - view all products in the database
+ `http://localhost:8081/add-product?name=iphone6&price=300` - add new product
+ `http://localhost:8081/query?command=sum` - execute some query with data in

It is necessary to refactor this code (the logic of the methods must not
change), for example:

+ eliminate duplication
+ isolate a separate layer of work with the database
+ isolate a separate layer of html response
+ etc.

## Instruction

+ the task should be submitted via e-mail, in the header of the letter specify "[SD-TASK]".
+ port to `github.com`
+ first add tests (in separate commissions)
+ each individual refactoring to make a separate commission
+ without history of changes and tests, the points will be taken off.
