insert into Students
    (student_id, student_name) values
    (0, 'Иванов Иван Иванович');

insert into Students
    (student_id, student_name) values
    (1, 'Петров Петр Петрович');

insert into Students
    (student_id, student_name) values
    (2, 'Алексеев Алексей Алексеевич');

insert into Courses
    (course_id, course_name) values
    (0, 'Теория вероятности и математическая статистика');

insert into Courses
    (course_id, course_name) values
    (1, 'Теория сложности');

insert into Courses
    (course_id, course_name) values
    (2, 'Алгоритмы и структуры данных');

insert into Marks
    (student_id, course_id) values
    (0, 0);

insert into Marks
    (student_id, course_id, mark) values
    (1, 2, 'A');

insert into Marks
    (student_id, course_id, mark) values
    (2, 1, 'C');

insert into Lecturers
    (lecturer_id, lecturer_name) values
    (0, 'Андреев Андрей Андреевич');

insert into Lecturers
    (lecturer_id, lecturer_name) values
    (1, 'Игорев Игорь Игоревич');

insert into Groups
    (group_id, group_name) values
    (0, 'M3333');

insert into Groups
    (group_id, group_name) values
    (1, 'M33333');

insert into Groups
    (group_id, group_name) values
    (2, 'N3533');

insert into GroupCourses
    (group_id, course_id, lecturer_id) values
    (0, 1, 0);

insert into GroupCourses
    (group_id, course_id, lecturer_id) values
    (1, 0, 1);

insert into GroupCourses
    (group_id, course_id, lecturer_id) values
    (1, 1, 0);
