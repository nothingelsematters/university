# Homework 6. Relational Calculus

Compile queries in terms of Datalog and SQL languages for the "University" database, allowing to receive:

1. Information about students (StudentId, StudentName, GroupId)
    1. With the specified name (:StudentName);
    2. Students in the given group (:GroupName);
    3. With the given grade (:Mark) in the discipline (:CourseName).
2. Full information about students (StudentId, StudentName, GroupName)
    1. For all students;
    2. Students who do not have a grade in the subject :CourseName;
    3. Students who do not have a grade in a subject :CourseName; students who have a grade in a subject.
3. Students and courses such that a student has had a course (either planned or graded) :CourseName; Students who have no grade in a subject :CourseName.
    1. Identifiers;
    2. Name and title.
4. Student Identifiers by Teacher (:LecturerName)
    1. Who have at least one teacher's grade;
    2. Not having a single grade from the instructor;
    3. Those who have grades in all subjects by the instructor;
    4. Those who have grades in all subjects that the instructor has administered to the student.
5. Groups and courses such that all students in a group have completed a course.
    1. Identifiers;
    2. Titles.
