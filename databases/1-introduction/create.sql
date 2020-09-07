drop database if exists ctd;

create database ctd;

\c ctd;

create table Groups (
  group_id int,
  group_no char(5)
);

create table Students (
  student_id int,
  name varchar(30),
  group_id int
);

insert into Groups
  (group_id, group_no) values
  (1, 'M3437'),
  (2, 'M3439');

insert into Students
  (student_id, name, group_id) values
  (1, 'Ali Alekperov', 1),
  (2, 'Ildar Amirov', 2),
  (3, 'Alexandra Drozdova', 2);

insert into Groups (group_id, group_no) values (1, 'M3438');

delete from Groups where group_no = 'M3438';

alter table Groups add constraint group_id_unique unique (group_id);

update Students set group_id = 5 where student_id = 1;

update Students set group_id = 1 where student_id = 1;

alter table Students add foreign key (group_id) references Groups (group_id);
