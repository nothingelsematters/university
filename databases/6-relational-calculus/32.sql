select StudentName, CourseName
from Students, Courses
where exists (
    select StudentId, CourseId
    from Marks
    where Marks.StudentId = Students.StudentId
      and Marks.CourseId = Courses.CourseId
    union
    select Students.StudentId, CourseId
    from Plan
    where Plan.GroupId = Students.GroupId
      and Courses.CourseId = Plan.CourseId
  )
