# Homework 1 (Second semester)
 
> Goal: to gain practical experience with the use of actors.

To implement a search aggregator which on user's request collects
top 5 answers via known search engines API and outputs them to the user.
For example, the user makes query to Google, Yandex, Bing and returns 15
answers (it must be clear which answer is from which search engine).
You don't need to use a real API and implement a StubServer, which will return
the results in a convenient format (json, xml, protobuf, etc.).

## Application architecture

- A master-actor is created for each query, which will collect results from
search engines

- The master-actor creates a child-actor for each search engine, to which it sends the original
  "search query"

- The master-actor sets itself a receive timeout, how long it will wait
  responses from child-actors

- The child-actor makes a request to the appropriate search service and sends its
  the result to the master-actor

- The master-actor saves each answer and if it gets all 3
  answers or timeout time has passed, it sends the aggregated
  result for further processing

- The master-actor must die after returning an aggregated result

## Directions

- The stub-server should implement a delay feature so that you can
  check the scenario when the master-actor has not waited for
  responses from all searchers

- [Examples from the lecture](https://github.com/akirakozov/software-design/tree/master/java/akka)
