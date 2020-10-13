select StudentId
from Marks
  left join Plan
    on Plan.CourseId = Marks.CourseId
except
select StudentId
from
  (
    select StudentId, Courses.CourseId
    from
      (
        select StudentId
        from Marks
          left join Plan
            on Plan.CourseId = Marks.CourseId
      ) StudentMarks,
      (
        select CourseId
        from Lecturers
          natural join Plan
        where LecturerName = :LecturerName
      ) Courses
    except
    select StudentId, Plan.CourseId
    from Marks
      left join Plan
        on Plan.CourseId = Marks.CourseId
  ) StudentsDivisionSubQuery