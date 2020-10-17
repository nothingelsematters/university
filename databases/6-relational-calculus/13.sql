select StudentId, StudentName, GroupId
from Students
where exists (
    select Mark
    from Marks
    where Mark = :Mark
      and Marks.StudentId = Students.StudentId
      and exists (
        select CourseId
        from Courses
        where CourseName = :CourseName
          and Courses.CourseId = Marks.CourseId
      )
  )
