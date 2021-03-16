# Homework 3 (Second semester)

> Goal: to gain practical experience in implementing event-sourcing systems and applying the principles of
> CQRS.

To implement an information system for a fitness center.
The clients of the fitness center can buy and prolong their membership.
When entering and leaving the fitness center, the client is obliged to attach a membership card for the passage.
The system allows you to build a variety of reports based on statistics collected in the course of the service.

The system consists of three parts:

1. Web service for a manager, allows to

  - View information about subscriptions (by number)

  - Issue and renew cards

2. Reporting service, allows you to

  - View attendance statistics by day

  - Calculate the average frequency and duration of visits

3. Input/output module

  - Lets the customer in if he has a valid season ticket

  - saves the client's entry/exit time in the database

## Directions

- The system should be based on an event-sourcing approach

- Commands and requests must be strictly separated (CQRS-principle)

- Events themselves should be stored in a persistent repository.

- The service for the manager and the input/output module do not have a separate database, and when
  The request pulls all the events of the subscription from the storage of events, aggregating them
  on the fly.

- The reporting service, on the contrary, calculates statistics on startup and stores them in memory,
  It is updated with the new events (but it is possible to store them in a separate persistent
  (but it is possible to store it in a separate persistent storage).

## References

- http://ookami86.github.io/event-sourcing-in-practice/
