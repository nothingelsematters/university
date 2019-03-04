package ru.ifmo.rain.naumov.student;

import info.kgeorgiy.java.advanced.student.Group;
import info.kgeorgiy.java.advanced.student.Student;
import info.kgeorgiy.java.advanced.student.StudentGroupQuery;

import java.util.Map;
import java.util.Set;
import java.util.List;
import java.util.TreeSet;
import java.util.TreeMap;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.Collection;
import java.util.stream.Stream;
import java.util.stream.Collector;
import java.util.stream.Collectors;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.function.Predicate;
import java.util.function.BinaryOperator;

public class StudentDB implements StudentGroupQuery {

    private final Comparator<Student> nameStudentComparator = Comparator.comparing(Student::getLastName)
            .thenComparing(Student::getFirstName)
            .thenComparing(Student::getId);

    private final Comparator<Student> idStudentComparator = Comparator.naturalOrder();  // comparator

    private <C extends Collection<String>> C getter(List<Student> students,
                                                    Function<Student,String> mapper,
                                                    Supplier<C> collection) {

        return students.stream().map(mapper).collect(Collectors.toCollection(collection));
    }

    private List<String> listGetter(List<Student> students, Function<Student,String> mapper) {
        return getter(students, mapper, ArrayList::new);
    }

    @Override
    public List<String> getFirstNames(List<Student> students) {
        return listGetter(students, Student::getFirstName);
    }

    @Override
    public List<String> getLastNames(List<Student> students) {
        return listGetter(students, Student::getLastName);
    }

    @Override
    public List<String> getGroups(List<Student> students) {
        return listGetter(students, Student::getGroup);
    }

    @Override
    public List<String> getFullNames(List<Student> students) {
        return listGetter(students, it -> it.getFirstName() + ' ' + it.getLastName());
    }

    @Override
    public Set<String> getDistinctFirstNames(List<Student> students) {
        return getter(students, Student::getFirstName, TreeSet::new);
    }

    @Override
    public String getMinStudentFirstName(List<Student> students) {
        return students.parallelStream().min(idStudentComparator)
                .map(Student::getFirstName).orElse("");
    }

    private List<Student> sortedList(Collection<Student> students, Comparator<Student> comparator) {
        return students.parallelStream().sorted(comparator).collect(Collectors.toList());
    }

    @Override
    public List<Student> sortStudentsById(Collection<Student> students) {
        return sortedList(students, idStudentComparator);
    }

    @Override
    public List<Student> sortStudentsByName(Collection<Student> students) {
        return sortedList(students, nameStudentComparator);
    }

    private Stream<Student> sortedStudentStream(Stream<Student> students) {
        return students.sorted(nameStudentComparator);
    }

    private Stream<Student> filteredStudentStream(Stream<Student> students, Predicate<Student> predicate) {
        return students.filter(predicate);
    }

    private List<Student> findStudentsByName(Collection<Student> students, Predicate<Student> predicate) {
        return sortedStudentStream(filteredStudentStream(students.stream(), predicate))
                .collect(Collectors.toList());
    }

    private List<Student> findStudentsByName(Collection<Student> students,
                                        String attribute, Function<Student, String> function) {
        return findStudentsByName(students, it -> attribute.equals(function.apply(it)));
    }

    private Predicate<Student> groupPredicate(String groupName) {
        return it -> groupName.equals(it.getGroup());
    }

    @Override
    public List<Student> findStudentsByFirstName(Collection<Student> students, String name) { // unify search
        return findStudentsByName(students, name, Student::getFirstName);
    }

    @Override
    public List<Student> findStudentsByLastName(Collection<Student> students, String name) {
        return findStudentsByName(students, name, Student::getLastName);
    }

    @Override
    public List<Student> findStudentsByGroup(Collection<Student> students, String group) {
        return findStudentsByName(students, groupPredicate(group));
    }

    @Override
    public Map<String, String> findStudentNamesByGroup(Collection<Student> students, String group) {
        return filteredStudentStream(students.stream(), groupPredicate(group))
                .collect(Collectors.toMap(Student::getLastName, Student::getFirstName,
                    BinaryOperator.minBy(String::compareTo)));
    }

    private List<Group> getGroups(Collection<Student> students, Comparator<Student> comparator) {
        return students.stream().sorted(comparator)
                .collect(Collectors.groupingBy(Student::getGroup))
                .entrySet().stream()
                .sorted(Map.Entry.comparingByKey())
                .map(it -> new Group(it.getKey(), it.getValue()))
                .collect(Collectors.toList());
    }

    @Override
    public List<Group> getGroupsByName(Collection<Student> students) {
        return getGroups(students, nameStudentComparator);
    }

    @Override
    public List<Group> getGroupsById(Collection<Student> students) {
        return getGroups(students, idStudentComparator);
    }

    private <T> String getMaxFromGrouped(Collection<Student> students,
                                         Collector<Student,?,T> collector,
                                         Comparator<Map.Entry<String,T>> comparator) {
        return students.parallelStream().collect(Collectors.groupingBy(Student::getGroup, collector))
                    .entrySet().parallelStream()
                    .max(comparator
                        .thenComparing(Comparator.comparing(Map.Entry<String,T>::getKey).reversed()))
                    .map(Map.Entry::getKey)
                    .orElse("");
    }

    @Override
    public String getLargestGroup(Collection<Student> students) {
        return getMaxFromGrouped(students,
                    Collectors.counting(),
                    Map.Entry.comparingByValue());
    }

    @Override
    public String getLargestGroupFirstName(Collection<Student> students) {
        return getMaxFromGrouped(students,
                    Collectors.toList(),
                    Comparator.comparing(
                        it -> it.getValue().stream().map(Student::getFirstName)
                                .distinct().count()
                    ));
    }
}
