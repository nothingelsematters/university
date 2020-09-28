create table Students (
    student_id   int          not null primary key,
    student_name varchar(100) not null
);

create table Courses (
    course_id   int          not null primary key,
    course_name varchar(100) not null
);

create table Marks (
    student_id int        not null,
    course_id  int        not null,
    mark       varchar(2) not null default 'FX',
    primary key (student_id, course_id)
);

create table Lecturers (
    lecturer_id   int          not null primary key,
    lecturer_name varchar(100) not null
);

create table Groups (
    group_id   int        not null primary key,
    group_name varchar(6) not null
);

create table GroupCourses (
    group_id    int not null,
    course_id   int not null,
    lecturer_id int not null,
    primary key (group_id, course_id)
);

alter table GroupCourses
    add constraint group_courses_group_fk foreign key (group_id) references Groups (group_id);

alter table GroupCourses
    add constraint group_courses_course_fk foreign key (course_id) references Courses (course_id);

alter table GroupCourses
    add constraint group_courses_lecturer_fk foreign key (lecturer_id) references Lecturers (lecturer_id);

alter table Marks
    add constraint marks_student_fk foreign key (student_id) references Students (student_id);

alter table Marks
    add constraint marks_course_fk foreign key (course_id) references Courses (course_id);
