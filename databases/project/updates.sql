-- ShutProject: close project and all current issues and pull requests in it
-- Isolation Level: Read Committed
create or replace function shutProject(
  username varchar(50),
  repository_name varchar(50),
  project_name varchar(50)
)
returns void as
$$
declare
  current_repository_id bigint;
begin
  select repository_id into current_repository_id
  from Repositories R
  join Developers D on D.developer_id = R.developer_id
  where D.username = shutProject.username
    and R.name = repository_name;

  update Issues
  set is_opened = false
  where issue_id in (
    select issue_id
    from ProjectIssues PI
    join Issues I on PI.repository_id = I.repository_id and PI.issue_number = I.number
    where PI.project_name = shutProject.project_name
      and I.repository_id = current_repository_id
  );

  update PullRequests
  set pull_request_status = 'closed'
  where pull_request_id in (
    select pull_request_id
    from ProjectPullRequests PPR
    join PullRequests PR
      on PPR.repository_id = PR.repository_id
      and PPR.pull_request_number = PR.number
    where PPR.project_name = shutProject.project_name
      and PPR.repository_id = current_repository_id
  );

  update Projects
  set is_opened = false
  where Projects.name = shutProject.project_name
    and repository_id = repository_id;
end
$$
language 'plpgsql';

-- test example
create view ShutProjectTest as
select P.name, P.is_opened as ProjectOpened, I.is_opened as IssueOpened
from Projects P
join ProjectIssues PI
  on PI.project_name = P.name
  and Pi.repository_id = P.repository_id
join Issues I
  on I.repository_id = P.repository_id
  and PI.issue_number = I.number
join Repositories R on R.repository_id = P.repository_id
join Developers D on D.developer_id = R.developer_id
where D.username = 'nothingelsematters'
  and R.name = 'find'
  and P.name = 'Find v2.0';

select * from ShutProjectTest;

select shutProject('nothingelsematters', 'find', 'Find v2.0');

select * from ShutProjectTest;

-- Merge Pull Request Trigger: close corresponding issues and add author in
-- contributors if needed
-- Isolation Level: Read Committed
create or replace function mergePullRequestTrigger()
returns trigger as
$$
declare
begin
  if old.pull_request_status = 'merged' or new.pull_request_status <> 'merged'
  then return new;
  end if;

  update Issues
  set is_opened = false
  where issue_id in (
    select issue_id
    from Issues I
    join IssueProcessings IP
      on I.number = IP.issue_number
      and I.repository_id = IP.repository_id
      and IP.pull_request_number = new.pull_request_id
  );

  if not exists (
    select *
    from Contributors C
    where C.developer_id = new.author_id
      and C.repository_id = new.repository_id
  ) then
  insert into Contributors (developer_id, repository_id)
    values (new.author_id, new.repository_id);
  end if;

  return new;
end
$$
language 'plpgsql';

-- test example
create view MergePullRequestTest as
select
  PR.pull_request_id,
  I.issue_id,
  PR.pull_request_status,
  DPR.username,
  I.is_opened as CorrespodingIssueIsOpened,
  (count(C.developer_id) > 0) as IsContributor
from PullRequests PR
join Repositories R on R.repository_id = PR.repository_id
join Developers D on R.developer_id = D.developer_id
join Developers DPR on PR.author_id = DPR.developer_id
left join Contributors C
  on C.developer_id = DPR.developer_id
  and C.repository_id = R.repository_id
join IssueProcessings IP
  on IP.repository_id = R.repository_id
  and IP.pull_request_number = PR.number
join Issues I
  on I.number = IP.issue_number
  and I.repository_id = R.repository_id
where pull_request_id = 3
group by I.is_opened, I.issue_id, pull_request_id, pull_request_status, DPR.username;

select * from MergePullRequestTest;

create trigger merge_pull_request_trigger
before update of pull_request_status
on PullRequests
for each row
execute procedure mergePullRequestTrigger();

update PullRequests
set pull_request_status = 'merged'
where pull_request_id in (
  select pull_request_id
  from MergePullRequestTest
);

select * from MergePullRequestTest;
