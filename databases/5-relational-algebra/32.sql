select Students.StudentId, StudentName, GroupId
from Marks
    natural join Students
    natural join Courses
where Mark = :Mark
  and CourseName = :CourseName;