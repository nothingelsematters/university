create domain positive_int as int check ( value > 0 );

create type pull_request_status_type as enum ('open', 'closed', 'merged');

create table Developers (
    developer_id bigint       not null generated always as identity,
    name         varchar(255) not null,
    username     varchar(50)  not null,
    mail         varchar(50),
    bio          varchar(255),
    company      varchar(50),
    constraint developers_pk primary key (developer_id),
    constraint developers_username_unique_key unique (username)
);

create table Followers (
    follower bigint not null,
    followed bigint not null,
    constraint followers_pk primary key (follower, followed),
    constraint followers_follower_fk
        foreign key (follower) references Developers (developer_id)
            on delete cascade on update cascade,
    constraint followers_followed_fk
        foreign key (followed) references Developers (developer_id)
            on delete cascade on update cascade,
    constraint followers_self_following_check
        check ( follower <> followed )
);

create table Repositories (
    repository_id  bigint      not null generated always as identity,
    name           varchar(50) not null,
    is_opened      boolean     not null default true,
    is_archived    boolean     not null default false,
    about          varchar(255),
    developer_id   bigint      not null,
    fork_source_id bigint,
    constraint repository_pk primary key (repository_id),
    constraint repositories_fork_source_fk
        foreign key (fork_source_id) references Repositories (repository_id),
    constraint repositories_developer_fk
        foreign key (developer_id) references Developers (developer_id)
            on delete cascade on update cascade,
    constraint repositories_developer_name_unique_key unique (name, developer_id)
);

create table Gists (
    gist_id      bigint       not null generated always as identity,
    hashname     char(32)     not null,
    filename     varchar(255) not null,
    description  varchar(255),
    developer_id bigint       not null,
    constraint gists_pk primary key (gist_id),
    constraint gists_developer_fk
        foreign key (developer_id) references Developers (developer_id)
            on delete cascade on update cascade
);

create table Stars (
    developer_id  bigint not null,
    repository_id bigint not null,
    constraint stars_pk primary key (developer_id, repository_id),
    constraint stars_developer_fk
        foreign key (developer_id) references Developers (developer_id)
            on delete cascade on update cascade,
    constraint stars_repository_fk
        foreign key (repository_id) references Repositories (repository_id)
            on delete cascade on update cascade
);

create table Contributors (
    developer_id  bigint not null,
    repository_id bigint not null,
    constraint contributors_pk primary key (developer_id, repository_id),
    constraint contributors_developer_fk
        foreign key (developer_id) references Developers (developer_id)
            on delete cascade on update cascade,
    constraint contributors_repository_fk
        foreign key (repository_id) references Repositories (repository_id)
            on delete cascade on update cascade
);

create table Issues (
    issue_id      bigint                  not null generated always as identity,
    title         varchar(255)            not null,
    number        positive_int            not null,
    is_opened     boolean                 not null default true,
    description   text,
    author_id     bigint                  not null,
    repository_id bigint                  not null,
    constraint issues_pk primary key (issue_id),
    constraint issues_author_fk
        foreign key (author_id) references Developers (developer_id)
            on delete cascade on update cascade,
    constraint issues_repository_fk
        foreign key (repository_id) references Repositories (repository_id)
            on delete cascade on update cascade,
    constraint issues_repository_number_unique_key unique (repository_id, number)
);

create table PullRequests (
    pull_request_id     bigint                   not null generated always as identity,
    title               varchar(255)             not null,
    number              positive_int             not null,
    pull_request_status pull_request_status_type not null default 'open',
    source_branch_name  varchar(50)              not null,
    target_branch_name  varchar(50)              not null,
    description         text,
    author_id           bigint                   not null,
    repository_id       bigint                   not null,
    constraint pull_request_pk primary key (pull_request_id),
    constraint pull_requests_author_fk
        foreign key (author_id) references Developers (developer_id)
            on delete cascade on update cascade,
    constraint pull_requests_repository_fk
        foreign key (repository_id) references Repositories (repository_id)
            on delete cascade on update cascade,
    constraint pull_requests_repository_number_unique_key unique (repository_id, number)
);

create table IssueProcessings (
    repository_id       bigint       not null,
    pull_request_number positive_int not null,
    issue_number        int          not null,
    constraint issue_processings_pk primary key (repository_id, pull_request_number, issue_number),
    constraint issue_processings_repository_pull_request_number_fk
        foreign key (repository_id, pull_request_number)
            references PullRequests (repository_id, number)
            on delete cascade on update cascade,
    constraint issue_processings_repository_issue_number_fk
        foreign key (repository_id, issue_number) references Issues (repository_id, number)
            on delete cascade on update cascade
);

create table Projects (
    project_id    bigint      not null generated always as identity,
    name          varchar(50) not null,
    is_opened     boolean     not null default true,
    description   varchar(255),
    repository_id bigint      not null,
    constraint projects_pk primary key (project_id),
    constraint projects_repository_fk
        foreign key (repository_id) references Repositories (repository_id)
            on delete cascade on update cascade,
    constraint projects_repository_name_unique_key unique (repository_id, name)
);

create table ProjectIssues (
    repository_id bigint       not null,
    project_name  varchar(50)  not null,
    issue_number  positive_int not null,
    constraint project_issues_pk primary key (repository_id, project_name, issue_number),
    constraint column_issue_project_name_fk
        foreign key (repository_id, project_name)
            references Projects (repository_id, name)
            on delete cascade on update cascade,
    constraint column_issue_issue_number_fk
        foreign key (repository_id, issue_number) references Issues (repository_id, number)
            on delete cascade on update cascade
);

create table ProjectPullRequests (
    repository_id        bigint       not null,
    project_name         varchar(50)  not null,
    pull_request_number  positive_int not null,
    constraint project_pull_request_pk primary key (repository_id, project_name, pull_request_number),
    constraint column_pull_requests_project_name_fk
        foreign key (repository_id, project_name)
            references Projects (repository_id, name)
            on delete cascade on update cascade,
    constraint column_pull_requests_pull_request_number_fk
        foreign key (repository_id, pull_request_number)
            references PullRequests (repository_id, number)
            on delete cascade on update cascade
);


-- search gists when using GET /{username}/{hashname}
create index gists_hashname_developer_index on Gists using btree (hashname, developer_id);
create index gists_developer_hashname_index on Gists using btree (developer_id, hashname);

-- developer's repositories tab
create index repositories_developer_index on Repositories using hash (developer_id);

-- show forks of a repository
create index repositories_fork_source_index on Repositories using hash (fork_source_id);

-- projects tab
create index projects_repository_index on Projects using hash (repository_id);

-- search by author
create index issues_author_index on Issues using hash (author_id);

-- show all issues
create index issues_repository_index on Issues using hash (repository_id);

-- search by author
create index pull_requests_author_index on PullRequests using hash (author_id);

-- show all pull requests
create index pull_requests_repository_index on PullRequests using hash (repository_id);

-- Repository's stars quantity
create index stars_repository_index on Stars using hash (repository_id);

-- "Followers" line in developer's profile
create index followers_follower_index on Followers using hash (follower);

-- "Following" line in developer's profile
create index followers_followed_index on Followers using hash (followed);

-- search developers by name in search bar
create index developers_name_index on Developers using hash (name);

-- search repositories by name in search bar
create index repositories_name_index on Repositories using hash (name);

-- search gists by filename in search bar
create index gists_filename_index on Gists using hash (filename);

create index gists_developer_index on Gists using hash (developer_id);
create index starts_developer_index on Stars using hash (developer_id);
create index contributors_developer_index on Contributors using hash (developer_id);
create index contributors_repository_index on Contributors using hash (repository_id);

Create index inssue_processings_repository_pull_request_index on IssueProcessings using btree (repository_id, pull_request_number);
Create index inssue_processings_pull_request_repository_index on IssueProcessings using btree (pull_request_number, repository_id);

Create index inssue_processings_repository_issue_index on IssueProcessings using btree (repository_id, issue_number);
Create index inssue_processings_issue_repository_index on IssueProcessings using btree (issue_number, repository_id);

create index project_issues_repository_project_index on ProjectIssues using btree (repository_id, project_name);
create index project_issues_project_repository_index on ProjectIssues using btree (project_name, repository_id);
create index project_isssues_repository_issues_index on ProjectIssues using btree (repository_id, issue_number);
create index project_isssues_issues_repository_index on ProjectIssues using btree (issue_number, repository_id);

create index project_pull_requests_repository_project_index on ProjectPullRequests using btree (repository_id, project_name);
create index project_pull_requests_project_repository_index on ProjectPullRequests using btree (repository_id, project_name);

create index project_pull_requests_repository__pull_request_index on ProjectPullRequests using btree (repository_id, pull_request_number);
create index project_pull_requests__pull_request_repository_index on ProjectPullRequests using btree (pull_request_number, repository_id);
