SELECT AVG(CAST(Mark AS DOUBLE PRECISION)) AS AvgMark
FROM Marks
WHERE StudentId = :StudentId