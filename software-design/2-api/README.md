# Homework 2

> Goal: to gain practical experience in implementing modular tests and tests
> that use mock-objects.

You need to implement a component that will calculate the frequency of tweets
from a certain hashtag over the last few hours. To perform a laboratory
you must use [twitter api](https://dev.twitter.com/rest/public/search) or any api
other social network (for example, vk does not require authorization).

At input, the component must accept:

+ hashtag to search through
+ `N` - number of hours for which a tweet chart should be drawn (`1 <= N <= 24`)

You need to output an array of `N` integers - each number in the array defines a
number of tweets at the appropriate hour.

Instructions:

+ It is recommended to use SOLID principles
+ The code must be covered by tests (including mock tests and tests with
  StubServer)

[Examples from the lecture](https://github.com/akirakozov/example-apps/tree/master/java/mock-example)
