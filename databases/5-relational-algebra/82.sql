SELECT GroupName, AvgAvgMark
FROM Groups
LEFT OUTER JOIN
  (
    SELECT GroupId, AVG(CAST(AvgMark AS DOUBLE PRECISION)) as AvgAvgMark
    FROM
      (
        SELECT StudentId, AVG(CAST(Mark AS DOUBLE PRECISION)) AS AvgMark
        FROM Marks
        GROUP BY StudentId
      ) AvgMarks
      NATURAL JOIN Students
    GROUP BY GroupId
  ) AvgAvgMarks
    ON Groups.GroupId = AvgAvgMarks.GroupId