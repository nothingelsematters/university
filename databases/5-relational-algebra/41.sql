select * from Students
except
select StudentId, StudentName, GroupId from Students
  natural join Marks
  natural join Courses
where CourseName = :CourseName;