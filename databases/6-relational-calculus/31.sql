select StudentId, CourseId
from Marks
union
select StudentId, CourseId
from Students, Plan
where Plan.GroupId = Students.GroupId
