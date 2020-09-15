create table Groups (
  group_id int not null primary key,
  name     varchar(6)
);

create table Students (
  student_id int          not null primary key,
  name       varchar(100) not null,
  group_id   int          not null,
  constraint student_group_fk foreign key (group_id) references Groups (group_id)
);

create table Subjects (
  subject_id int         not null primary key,
  title      varchar(50) not null
);

create table Marks (
  student_id int                     not null,
  subject_id int                     not null,
  value      varchar(2) default 'FX' not null,
  date       date                    not null,
  constraint mark_student_fk foreign key (student_id) references Students (student_id),
  constraint mark_subject_fk foreign key (subject_id) references Subjects (subject_id)
);

create table Teachers (
  teacher_id int          not null primary key,
  name       varchar(100) not null
);

create table TeachersSubjects (
  teacher_id int not null,
  subject_id int not null,
  primary key (teacher_id, subject_id),
  constraint teachersubject_teacher_fk foreign key (teacher_id) references Teachers (teacher_id),
  constraint teachersubject_subject_fk foreign key (subject_id) references Subjects (subject_id)
);
