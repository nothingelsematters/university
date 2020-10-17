select distinct StudentId, StudentName, GroupName
from Students, Groups, Courses, Plan
where Students.GroupId = Groups.GroupId
  and Plan.CourseId = Courses.CourseId
  and Plan.GroupId = Students.GroupId
  and CourseName = :CourseName
  and not exists (
    select Mark
    from Marks
    where Marks.CourseId = Courses.CourseId
      and Marks.StudentId = Students.StudentId
  )
