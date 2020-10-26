drop view if exists Losers;
drop table if exists Groups;
drop table if exists Students;
drop table if exists Marks;
drop table if exists Courses;
drop table if exists Lecturers;
drop table if exists Plan;
drop table if exists LoserT;
drop function if exists RefreshLoserT;
drop function if exists OnlyIncreaseMarkConstraint;

create table Groups (
  GroupId   int primary key,
  GroupName varchar(6)
);

create table Students (
  StudentId   int primary key,
  StudentName varchar(100) not null,
  GroupId     int          not null
);

create table Courses (
  CourseId   int primary key,
  CourseName varchar(50) not null
);

create table Marks (
  StudentId int not null,
  CourseId  int not null,
  Mark      int default 0 not null,
  check (Mark between 0 and 100),
  primary key (StudentId, CourseId)
);

create table Lecturers (
  LecturerId   int primary key,
  LecturerName varchar(100) not null
);

create table Plan (
  LecturerId int,
  CourseId   int,
  GroupId    int,
  primary key (LecturerId, CourseId, GroupId)
);

insert into Courses (CourseId, CourseName)
values
  (1, 'C1'),
  (2, 'C2'),
  (3, 'C3'),
  (4, 'C4');

insert into Groups (GroupId, GroupName)
values
  (1, 'G1'),
  (2, 'G2'),
  (3, 'G3'),
  (4, 'G4');

insert into Students (StudentId, StudentName, GroupId)
values
  (1, 'S1', 1),
  (2, 'S2', 1),
  (3, 'S3', 2),
  (4, 'S4', 2),
  (5, 'S5', 3),
  (6, 'S6', 3),
  (7, 'S7', 1),
  (8, 'S8', 1);

insert into Marks (StudentId, CourseId, Mark)
values
  (1, 1, 90),
  (2, 1, 75),
  (3, 1, 50),
  (2, 2, 50),
  (3, 2, 40),
  (4, 2, 90),
  (7, 1, 90),
  (8, 1, 90),
  (7, 4, 90),
  (8, 4, 90),
  (5, 4, 90),
  (6, 4, 90),
  (3, 3, 50);

insert into Plan (CourseId, GroupId, LecturerId)
values
  (1, 1, 1),
  (1, 2, 1),
  (1, 3, 1),
  (2, 2, 1),
  (3, 2, 1),
  (4, 3, 1);
