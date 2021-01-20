insert into Developers
  (name, username, mail, bio, company) values
  (
    'Simon Naumov',
    'nothingelsematters',
    'daretoodefy@gmail.com',
    'ITMO CTD Student',
    'ITMO University'
  ),
  ('Dmitrii Banshchikov', 'ubique', 'me@ubique.spb.ru', null, null),
  ('Ivan Sorokin', 'sorokin', 'sorokin.ivan@itmo.ru', null, 'CPP-KT'),
  (
    'Vsevolod',
    'volekerb',
    'vsevolod.brekelov@gmail.com',
    'Software Engineer. Java, JavaScript, Golang, Groovy, Kotlin. Interested in openSource projects. Ping me to participate!',
    null
  );

insert into Followers (follower, followed) values
  (1, 2),
  (1, 3),
  (1, 4);

insert into Repositories
  (name, is_opened, is_archived, about, developer_id, fork_source_id)
  values
  (
    'university',
    true,
    false,
    'ITMO University Computer Science Department home work projects',
    1,
    null
  ),
  ('lalr-generator', true, true, 'Kotlin LALR grammar parser generator', 1, null),
  ('os-find', true, false, null, 2, null),
  ('find', true, false, 'Find utility implementation', 1, 3),
  ('testing-lectures', true, false, 'Software testing lectures for ITMO', 4, null);

insert into Gists
  (hashname, filename, description, developer_id)
  values
  ('d823cad81540610e9f8558493f316a93', 'file name', null, 1),
  ('aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa', 'another file name', null, 2),
  ('bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb', 'file name', null, 1),
  ('cccccccccccccccccccccccccccccccc', 'file name', null, 3),
  ('dddddddddddddddddddddddddddddddd', 'file name', null, 1),
  ('eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee', 'file name', null, 4);

insert into Stars (developer_id, repository_id) values
  (1, 2),
  (1, 3),
  (1, 4),
  (3, 4),
  (2, 1);

insert into Contributors (developer_id, repository_id) values
  (1, 1),
  (1, 2),
  (2, 3),
  (1, 4),
  (1, 5),
  (4, 5);

-- TODO
insert into Issues
  (title, number, is_opened, description, author_id, repository_id)
  values
  (
    'README is incorrect',
    1,
    true,
    'Some long description about why README is incorrect and correction suggestions',
    1,
    5
  ),
  (
    'Code is somehow overcomplicated, refactoring needed',
    1,
    true,
    'Your code is heavy to read, try simplifying it somehow',
    2,
    1
  ),
  (
    'Duplicate refactoring issue',
    2,
    true,
    'Your code is heavy to read, try simplifying it somehow',
    3,
    1
  ),
  (
    'Dependency updates',
    2,
    true,
    'There are security issues figured out in the libraries we are currently using!!',
    4,
    2
  ),
  ('Write MVP', 1, false, null, 2, 4),
  ('Add some features', 2, false, null, 1, 4),
  ('Make usage easier', 3, false, null, 4, 4),
  ('Add some cli flags', 4, false, null, 3, 4),
  ('Remove deprecated things', 5, true, null, 1, 4);

insert into PullRequests
  (title, number, pull_request_status,
    source_branch_name, target_branch_name,
    description, author_id, repository_id)
  values
  (
    'First part of README fixes',
    1,
    'closed',
    'bugfix/readme',
    'master',
    null,
    1,
    5
  ),
  (
    'Second part of README fixes',
    2,
    'open',
    'bugfix/readme',
    'master',
    null,
    1,
    5
  ),
  (
    'Refactoring',
    1,
    'open',
    'feature/refactoring',
    'develop',
    'Super pull request closing all the issues mentioning refactoring',
    4,
    1
  ),
  ('find feature', 1, 'merged', 'feature/find', 'master', null, 2, 4),
  ('miscellaneous updates', 1, 'open', 'features', 'master', null, 3, 2);

insert into IssueProcessings
  (repository_id, pull_request_number, issue_number)
  values
  (5, 1, 1),
  (5, 2, 1),
  (1, 1, 1),
  (1, 1, 2);

insert into Projects
  (name, is_opened, description, repository_id)
  values
  ('Find v1.0', false, 'First utility version', 4),
  ('Find v2.0', true, 'Second utility version', 4),
  ('Some milestones with generation fixes', true, null, 2);

insert into ProjectIssues
  (repository_id, project_name, issue_number)
  values
  (4, 'Find v1.0', 1),
  (4, 'Find v1.0', 2),
  (4, 'Find v1.0', 4),
  (4, 'Find v2.0', 3),
  (4, 'Find v2.0', 5),
  (2, 'Some milestones with generation fixes', 2);

insert into ProjectPullRequests
  (repository_id, project_name, pull_request_number)
  values
  (4, 'Find v1.0', 1),
  (2, 'Some milestones with generation fixes', 1);
