package info.kgeorgiy.java.advanced.student;

/**
 * Basic student information.
 *
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public class Student implements Comparable<Student> {
    private final int id;
    private final String firstName;
    private final String lastName;
    private final String group;

    public Student(final int id, final String firstName, final String lastName, final String group) {
        this.id = id;
        this.firstName = firstName;
        this.lastName = lastName;
        this.group = group;
    }

    /** Unique student id. */
    public int getId() {
        return id;
    }

    /** Student first name. */
    public String getFirstName() {
        return firstName;
    }

    /** Student last name. */
    public String getLastName() {
        return lastName;
    }

    /** Name of the group, this students belongs to. */
    public String getGroup() {
        return group;
    }

    /** Compares students by {@link #getId() id}. */
    @Override
    public int compareTo(final Student that) {
        return Integer.compare(this.id, that.id);
    }

    @Override
    public String toString() {
        return String.format("Student(%s, %s, %s, %s)", id, firstName, lastName, group);
    }
}
