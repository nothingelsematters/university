# Homework 3

Relationship with attributes `StudentId`, `StudentName`, `GroupId`, `GroupName`,
`CourseId`, `CourseName`, `LecturerId`, `LecturerName`, `Mark`

1. Find the functional dependencies in this relationship.

    > ```
    > StudentId -> StudentName
    > StudentId -> GroupId
    >
    > GroupId -> GroupName
    > GroupName -> GroupId
    >
    > CourseId -> CourseName
    >
    > LecturerId -> LecturerName
    >
    > StudentId, CourseId -> Mark
    > StudentId, CourseId -> LecturerId
    > ```

2. Find all the keys of this relationship.

    > `(StudentId, CourseId)`

3. Find attribute set closure:

    + GroupId, CourseId;

       > `{GroupId, GroupName, CourseId, CourseName}`

    + StudentId, CourseId;

       > `{StudentId, StudentName, GroupId, GroupName, CourseId, CourseName, LecturerId, LecturerName, Mark}`

    + StudentId, LecturerId.

       > `{StudentId, StudentName, GroupId, GroupName, LecturerId, LecturerName}`

4. Find an inreducable set of functional dependencies for this relationship.

    > `See point 1`
