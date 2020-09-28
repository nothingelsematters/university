insert into Groups
  (group_id, name) values
  (0, 'M3437');

insert into Groups
  (group_id, name) values
  (1, 'M33335');

insert into Students
  (student_id, name, group_id) values
  (0, 'Иванов Иван Иванович', 0);

insert into Students
  (student_id, name, group_id) values
  (1, 'Петров Пётр Петрович', 1);

insert into Students
  (student_id, name, group_id) values
  (2, 'Алексеев Алексей Алексеевич', 1);


insert into Subjects
  (subject_id, title) values
  (0, 'Теория вероятности и математической статистики');

insert into Subjects
  (subject_id, title) values
  (1, 'Введение в программирование');

insert into Subjects
  (subject_id, title) values
  (2, 'Парадигмы программирования');

insert into Subjects
  (subject_id, title) values
  (3, 'Технологии Java');

insert into Subjects
  (subject_id, title) values
  (4, 'Введение в базы данных');

insert into Teachers
  (teacher_id, name) values
  (0, 'Алексеев Пётр Иванович');

insert into Teachers
  (teacher_id, name) values
  (1, 'Петров Иван Алексеевич');

insert into Teachers
  (teacher_id, name) values
  (2, 'Иванов Алексей Петрович');

insert into Marks
  (student_id, subject_id, value, date) values
  (0, 0, 'A', '2020-02-02');

insert into Marks
  (student_id, subject_id, value, date) values
  (0, 1, 'B', '2020-02-04');

insert into Marks
  (student_id, subject_id, value, date) values
  (0, 2, 'C', '2020-01-02');

insert into Marks
  (student_id, subject_id, value, date) values
  (0, 3, 'D', '2019-09-02');

insert into Marks
  (student_id, subject_id, value, date) values
  (2, 3, 'E', '2020-02-02');

insert into Marks
  (student_id, subject_id, value, date) values
  (2, 4, 'B', '2020-02-01');

insert into Marks
  (student_id, subject_id, value, date) values
  (2, 3, 'C', '2020-02-02');

insert into Marks
  (student_id, subject_id, date) values
  (1, 4, '2020-02-03');

insert into TeachersSubjects
  (teacher_id, subject_id) values
  (0, 1);

insert into TeachersSubjects
  (teacher_id, subject_id) values
  (0, 2);

insert into TeachersSubjects
  (teacher_id, subject_id) values
  (1, 3);

insert into TeachersSubjects
  (teacher_id, subject_id) values
  (1, 4);

insert into TeachersSubjects
  (teacher_id, subject_id) values
  (2, 0);

insert into TeachersSubjects
  (teacher_id, subject_id) values
  (2, 4);
