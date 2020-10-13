select CourseId, GroupId
from Marks, Students
where Mark > 2
except
select CourseId, GroupId from
(
    select CourseId, StudentId, GroupId
    from Students,
      (select CourseId from Marks where Mark > 2) MarkCourses
    except
    select CourseId, StudentId, GroupId
    from Marks
      natural join Students
) GreatDivisionSubQuery;
