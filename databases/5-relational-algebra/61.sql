select distinct StudentId
from Students
  natural join Marks
  natural join Plan
  natural join Lecturers
where LecturerName = :LecturerName;