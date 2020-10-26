# Homework 7. Change of data

We will assume that a student has a study debtif he or she has less than 60 points in that subject.

1. Write requests that will remove:
   1. Students who do not have debts;
   2. Students who have 3 or more debts;
   3. Groups in which no students are enrolled.
2. Working with debtors
   1. Create a `Losers` view that shows the number of `Losers` for each student who has debts;
   2. Create a `LoserT` table that contains the same information as the `Losers` view.
      This table should be automatically updated when the points table changes;
   3. Disable the automatic updating of `LoserT`;
   4. Write a query (one) that updates the `LoserT` table using data from the NewPoints table,
      which contains information about the points awarded on the last day.
3. Data Integrity
   1. Add a check that all students in the same group are studying the same set of courses;
   2. Create a trigger that does not reduce student scores in a course.
      If you try to make this change, the scores should not change.
