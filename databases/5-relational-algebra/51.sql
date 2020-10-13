select distinct StudentName, CourseName
from Students
    natural join Plan
    natural join Courses;