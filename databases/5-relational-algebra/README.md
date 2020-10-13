# Homework 5. Relational algebra

Structure of the database "University":

+ *Students(StudentId, StudentName, GroupId)*
+ *Groups(GroupId, GroupName)*
+ *Courses(CourseId, CourseName)*
+ *Lecturers(LecturerId, LecturerName)*
+ *Plan(GroupId, CourseId, LecturerId)*
+ *Marks(StudentId, CourseId, Mark)*

Compile relational algebra expressions and the corresponding SQL queries that allow you to get:

1. Information about students (*StudentId, StudentName, GroupId*)
   1. With specified identifier (*:StudentId*);
   2. With specified name (*:StudentName*).
2. Full information about students (*StudentId, StudentName, GroupName*)
   1. With specified identifier (*:StudentId*);
   2. With specified name (*:StudentName*).
3. Information about students with the given grade (*:Mark*) by subject (*:StudentId*);
   With the given name (*:StudentName*).
   1. With specified identifier (*:CourseId*);
   2. With preset name (*:CourseName*).
4. Information about students without a grade in the subject :CourseName
   1. Among all students;
   2. Among students who have this course.
5. For each student, name and course title
   1. Which he has on the plan;
   2. He does, but he does not have a grade;
   3. Yes, but he does not have 4 or 5;
6. Student Identifiers by Teacher (*:LecturerName*)
   1. Who have at least one teacher's grade;
   2. Not having a single grade from the instructor;
   3. Those who have grades in all subjects by the instructor;
   4. Those who have grades in all subjects that the instructor has administered to the student;
7. Groups and courses such that all students in a group have completed a course.
   1. Identifiers;
   2. Titles;

Compile SQL queries that allow you to receive:

8. Student Average Score (AvgMark) based on grades given
   1. By :StudentId
   2. For each student (specify StudentName)
9. The average grade point average (AvgAvgMark) of students in each group (GroupName).
10. For each student (StudentName): The number of courses he or she has taken (Total),
  the number of Passed courses, and the number of Failed courses.

