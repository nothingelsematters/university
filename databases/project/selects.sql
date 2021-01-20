-- Repository Issues Agenda
select title
from Issues I
join ProjectIssues PI
  on I.repository_id = PI.repository_id
  and I.number = PI.issue_number
join Projects P
  on I.repository_id = P.repository_id
  and PI.project_name = P.name
join Repositories R
  on I.repository_id = R.repository_id
where P.name = 'Find v2.0'
  and R.name = 'find';

-- Followed Contributions: show followed users' contributions
select distinct FO.username, R.name
from Followers F
join Developers FS  on FS.developer_id = F.follower
join Developers FO  on FO.developer_id = F.followed
join Contributors C on C.developer_id = F.followed
join Repositories R on R.repository_id = C.repository_id
where FS.username = 'nothingelsematters';

-- Contributing Statistics: information that could be needed to visualize it for
-- "Activity Overview" on GitHub user page
create view ContributingStats as
select
  D.name,
  D.username,
  RepositoryAmount,
  PullRequestsAmount,
  IssuesAmount,
  ContributionsAmount,
  StarsAmount,
  FollowersAmount
from Developers D
left join (
  select R.developer_id, count(R.repository_id) as RepositoryAmount
  from Repositories R
  group by R.developer_id
) RepositoryAmountQuery
  on D.developer_id = RepositoryAmountQuery.developer_id
left join (
  select PR.author_id, count(PR.repository_id) as PullRequestsAmount
  from PullRequests PR
  group by PR.author_id
) PullRequestsAmountQuery
  on D.developer_id = PullRequestsAmountQuery.author_id
left join (
  select I.author_id, count(I.repository_id) as IssuesAmount
  from Issues I
  group by I.author_id
) IssuesAmountQuery
  on D.developer_id = IssuesAmountQuery.author_id
left join (
  select C.developer_id, count(C.repository_id) as ContributionsAmount
  from Contributors C
  group by C.developer_id
) ContributionsAmountQuery
  on D.developer_id = ContributionsAmountQuery.developer_id
left join (
  select S.developer_id, count(S.repository_id) as StarsAmount
  from Stars S
  group by S.developer_id
) StarsAmountQuery
  on D.developer_id = StarsAmountQuery.developer_id
left join (
  select F.followed, count(F.follower) as FollowersAmount
  from Followers F
  group by F.followed
) FollowersAmountQuery
  on D.developer_id = FollowersAmountQuery.followed;

select * from ContributingStats where username = 'nothingelsematters';

-- Repository Previews: information you can see in "Pinned Repositories" on
-- GitHub user page
create view RepositoryPreviews as
select D.username, R.name, R.about, R.fork_source_id, count(S.developer_id) as StarsAmount
from Developers D
join Repositories R on D.developer_id = R.developer_id
left outer join Stars S on S.repository_id = R.repository_id
where is_archived = false
  and is_opened = true
group by D.username, R.name, R.about, R.fork_source_id;

select * from RepositoryPreviews where username = 'nothingelsematters';

-- Suggestions: suggest maximum 3 repositories that does not have user's stars
-- yet and could be interesting
create or replace function getSuggestions(username varchar(50))
returns table (name varchar(50)) as
$$
declare
  user_id bigint;
begin
  select developer_id into user_id
  from Developers D
  where D.username = getSuggestions.username;

  create temporary table TmpRepos on commit drop as
  select R.name
  from Repositories R
  join Followers F
    on F.follower = user_id
    and F.followed = R.developer_id
  where R.developer_id = F.followed
    and R.repository_id not in (
      select repository_id
      from Stars S
      where S.developer_id = user_id
    )
  group by R.name
  limit 3;

  if (select count(*) from TmpRepos) > 0 then
    return query select TR.name from TmpRepos TR;
  else
    return query
    select R.name
    from Repositories R
    where R.developer_id <> user_id
    order by random()
    limit 3;
  end if;
end
$$
language 'plpgsql';

select getSuggestions('sorokin');
