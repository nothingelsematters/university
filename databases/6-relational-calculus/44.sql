select StudentId
from Students
where not exists (
    select StudentId
    from Lecturers, Plan
    where Lecturers.LecturerId = Plan.LecturerId
      and LecturerName = :LecturerName
      and Plan.GroupId = Students.GroupId
      and not exists (
          select Mark
          from Marks
          where Marks.StudentId = Students.StudentId
            and Marks.CourseId = Plan.CourseId
        )
  )
