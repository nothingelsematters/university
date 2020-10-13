select CourseName, GroupName
from (select CourseName from Marks natural join Courses) CourseMarks,
     (select GroupName from Students natural join Groups) StudentGroups
except
select CourseName, GroupName from
(
    select CourseName, StudentId, GroupName
    from
      (select StudentId, GroupName from Students natural join Groups) StudentGroupsWithStudentId,
      (select CourseName from Marks natural join Courses) CourseMarks
    except
    select CourseName, StudentId, GroupName
    from
      (
        select CourseName, StudentId
        from Marks
          natural join Courses
      ) CoursesMarksWithStudentId
      natural join
        (
          select GroupName, StudentId
          from Students
            natural join Groups
        ) StudentGroupsWithStudentId
) GreatDivisionSubQuery;
