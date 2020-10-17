select GroupName, CourseName
from Groups, Courses
where not exists (
    select StudentId
    from Students
    where Students.GroupId = Groups.GroupId
      and not exists (
        select Mark
        from Marks
        where Marks.CourseId = Courses.CourseId
          and Marks.StudentId = Students.StudentId
      )
  )
