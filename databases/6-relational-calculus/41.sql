select distinct Students.StudentId
from Students, Marks, Plan, Lecturers
where Students.StudentId = Marks.StudentId
  and Students.GroupId = Plan.GroupId
  and Marks.CourseId = Plan.CourseId
  and Plan.LecturerId = Lecturers.LecturerId
  and LecturerName = :LecturerName
