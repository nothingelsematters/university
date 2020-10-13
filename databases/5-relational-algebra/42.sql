select StudentId, StudentName, GroupId
from Students
    natural join Plan
    natural join Courses
where CourseName = :CourseName
except
select StudentId, StudentName, GroupId
from Students
  natural join Marks
  natural join Courses
where CourseName = :CourseName;