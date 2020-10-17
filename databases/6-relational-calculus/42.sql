select StudentId
from Students
where not exists (
    select Students.StudentId
    from Marks, Plan, Lecturers
    where Students.StudentId = Marks.StudentId
      and Students.GroupId = Plan.GroupId
      and Marks.CourseId = Plan.CourseId
      and Plan.LecturerId = Lecturers.LecturerId
      and LecturerName = :LecturerName
  )
