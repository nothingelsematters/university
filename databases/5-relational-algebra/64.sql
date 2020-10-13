select distinct StudentId
from
  (
    select StudentId, GroupId
    from
      (
        select StudentId
        from Lecturers
          natural join Plan
          natural join Students
          natural join Marks
        where LecturerName = :LecturerName
      ) StudentIds,
      (
        select GroupId
        from Lecturers
          natural join Plan
        where LecturerName = :LecturerName
      ) GroupIds
    except
    select StudentId, GroupId
    from
      (
        select StudentId, GroupId, CourseId
        from
          (
            select StudentId
            from Lecturers
              natural join Plan
              natural join Students
              natural join Marks
            where LecturerName = :LecturerName
          ) StudentIds,
          (
            select GroupId, CourseId
            from Lecturers
              natural join Plan
            where LecturerName = :LecturerName
          ) GroupIds
        except
        select StudentId, GroupId, CourseId
        from
          (
            select StudentId, CourseId
            from Lecturers
              natural join Plan
              natural join Students
              natural join Marks
            where LecturerName = :LecturerName
          ) StudentIds
          natural join
            (
              select GroupId, CourseId
              from Lecturers
                natural join Plan
              where LecturerName = :LecturerName
            ) GroupIds
      ) GreatDivSubSequent
) as SubSequent