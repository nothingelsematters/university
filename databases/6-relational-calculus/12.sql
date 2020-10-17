select StudentId, StudentName, GroupId
from Students
where exists (
    select GroupId, GroupName
    from Groups
    where Groups.GroupId = Students.GroupId
      and GroupName = :GroupName
  )
