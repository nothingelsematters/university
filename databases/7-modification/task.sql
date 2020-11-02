-- 1.1
delete from Students
where StudentId not in (
  select StudentId
  from Plan
  natural join Students
  natural left join Marks
  where Mark is null or Mark < 60
);

-- 1.2
delete from Students
where StudentId in (
  select StudentId
  from Plan
  natural join Students
  natural left join Marks
  where Mark is null or Mark < 60
  group by StudentId
  having count(StudentId) >= 3
);

-- 1.3
delete from Groups
where GroupId not in (select GroupId from Students);

-- 2.1
create view Losers as
  select StudentId, count(StudentId) as DebtAmount
  from Plan
    natural join Students
    natural left join Marks
  where Mark is null or Mark < 60
  group by StudentId;

-- 2.2
create table LoserT as (select * from Losers);

create function RefreshLoserT()
  returns TRIGGER
  language plpgsql
  as $function$
    begin
      if tg_op = 'update' or tg_op = 'delete' then
        delete from LoserT where StudentId = old.StudentId;
      else
        delete from LoserT where StudentId = new.StudentId;
      end if;

      insert into LoserT
      select * from Losers
      where StudentId = old.StudentId;

      RETURN NEW;
    END;
  $function$;

create trigger RefreshLoserT after insert or update or delete on Marks
  for each row execute procedure RefreshLoserT();

-- 2.3
drop trigger RefreshLoserT on Losers;

-- 2.4
merge into LoserT
using (
  select StudentId, CourseId, coalesce(Mark, 0) as OldPoints, Points as AdditionalPoints
  from Plan
    natural join Students
    natural left join Marks
    natural join NewPoints
) PointUpdates
on LoserT.StudentId = PointUpdates.StudentId
when matched
  and PointUpdates.OldPoints < 60
  and PointUpdates.OldPoints + PointUpdates.AdditionalPoints >= 60
  and LoserT.DebtAmount = 1
  then delete
when matched
  and PointUpdates.OldPoints < 60
  and PointUpdates.OldPoints + PointUpdates.AdditionalPoints >= 60
  then update set DebtAmount = LoserT.DebtAmount - 1
when matched
  and PointUpdates.OldPoints >= 60
  and PointUpdates.OldPoints + PointUpdates.AdditionalPoints < 60
  then update set DebtAmount = LoserT.DebtAmount + 1
when not matched
  and PointUpdates.OldPoints + PointUpdates.AdditionalPoints < 60
  then insert (StudentId, DebtAmount) values (PointUpdates.StudentId, 1);

-- 3.1
-- We don't need that. That's a redundant check.

-- 3.2
create function OnlyIncreaseMarkConstraint()
  returns trigger
  language plpgsql
  as $function$
    begin
      new.Mark := greatest(new.Mark, old.Mark);
      return new;
    end;
  $function$;

create trigger OnlyIncreaseMarkTrigger before update on Marks
for each row execute procedure OnlyIncreaseMarkConstraint();
