select StudentId, StudentName, GroupName
from Students, Groups
where Students.GroupId = Groups.GroupId
