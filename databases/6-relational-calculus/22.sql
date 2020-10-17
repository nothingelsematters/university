select StudentId, StudentName, GroupName
from Students, Groups
where Students.GroupId = Groups.GroupId
  and not exists (
    select Mark
    from Marks, Courses
    where Marks.CourseId = Courses.CourseId
      and Marks.StudentId = Students.StudentId
      and CourseName = :CourseName
  )
