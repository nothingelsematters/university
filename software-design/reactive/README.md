# Homework 2 (Second semester)

> Goal: to get practical experience in implementing "reactive" applications.

To implement a web service for browsing the catalog of products.
In the service you can register new users and add products.
When registering users indicate the currency
in which they want to see the goods (dollars, euros, rubles).

## Directions

- The web service must be fully "reactive" (all interaction must be
  asynchronous and non-blocking)

- The requests may not check the authorization of users,
  but simply specify the id user

- Data can be stored in mongo and a "reactive" mongo driver can be used;
  rxnetty-http can be used as rxnetty-http can be used as an http service

- Other reactive libraries and frameworks can also be used;

- [An example from the lecture](https://github.com/akirakozov/software-design/tree/master/java/rxjava)
