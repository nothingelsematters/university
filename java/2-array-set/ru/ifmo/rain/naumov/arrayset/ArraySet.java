package ru.ifmo.rain.naumov.arrayset;

import java.util.*;

public class ArraySet<E> extends AbstractSet<E> implements NavigableSet<E>  {

    private final List<E> elements;
    private final Comparator<? super E> comparator;

    public ArraySet() {
        comparator = null;
        elements = Collections.emptyList();
    }

    public ArraySet(Collection<? extends E> collection) {
        this(collection, null);
    }

    public ArraySet(Collection<? extends E> collection, Comparator<? super E> comparator) {
        this.comparator = comparator;
        TreeSet<E> treeSet = new TreeSet<>(comparator);
        treeSet.addAll(collection);
        elements = new ArrayList<>(treeSet);
    }

    private ArraySet(ListView listView, Comparator<? super E> comparator) {
        this.comparator = comparator;
        elements = listView;
    }

    @Override
    @SuppressWarnings("unchecked")
    public boolean contains(Object o) {
        return Collections.binarySearch(elements, (E) o, comparator) >= 0;
    }

    @Override
    public Iterator<E> iterator() {
        return Collections.unmodifiableList(elements).iterator();
    }

    @Override
    public Iterator<E> descendingIterator() {
        return descendingSet().iterator();
    }

    @Override
    public int size() {
        return elements.size();
    }

    @Override
    public E first() {
        checkNotEmpty();
        return elements.get(0);
    }

    @Override
    public E last() {
        checkNotEmpty();
        return elements.get(size() - 1);
    }

    private void checkNotEmpty() {
        if (isEmpty()) {
            throw new NoSuchElementException();
        }
    }

    @Override
    public E lower(E e) {
        return valueOrNull(lowerIndex(e));
    }

    @Override
    public E floor(E e) {
        return valueOrNull(floorIndex(e));
    }

    @Override
    public E ceiling(E e) {
        return valueOrNull(ceilingIndex(e));
    }

    @Override
    public E higher(E e) {
        return valueOrNull(higherIndex(e));
    }

    private E valueOrNull(int index) {
        return validIndex(index) ? elements.get(index) : null;
    }

    private int floorIndex(E e) {
        int index = indexSearch(e);
        return index < 0 ? -index - 2 : index;
    }

    private int lowerIndex(E e) {
        return ceilingIndex(e) - 1;
    }

    private int ceilingIndex(E e) {
        int index = indexSearch(e);
        return index < 0 ? -index - 1 : index;
    }

    private int higherIndex(E e) {
        int index = indexSearch(e);
        return index < 0 ? -index - 1 : index + 1;
    }

    private int indexSearch(E e) {
        return Collections.binarySearch(elements, e, comparator);
    }

    private boolean validIndex(int index) {
        return (index >= 0 && index < size());
    }

    @Override
    public E pollFirst() {
        throw new UnsupportedOperationException();
    }

    @Override
    public E pollLast() {
        throw new UnsupportedOperationException();
    }

    @Override
    public NavigableSet<E> descendingSet() {
        return new ArraySet<>(new ListView(elements), Collections.reverseOrder(comparator));
    }

    @Override
    public NavigableSet<E> subSet(E fromElement, boolean fromInclusive, E toElement, boolean toInclusive) {
        int fromIndex = fromInclusive ? ceilingIndex(fromElement) : higherIndex(fromElement);
        int toIndex = toInclusive ? floorIndex(toElement) : lowerIndex(toElement);

        if (fromIndex == -1 || fromIndex > toIndex + 1) {
            return new ArraySet<>(Collections.emptyList(), comparator);
        }
        return new ArraySet<>(new ListView(fromIndex, toIndex + 1, true), comparator);
    }

    @Override
    public NavigableSet<E> headSet(E toElement, boolean inclusive) {
        if (isEmpty()) {
            return new ArraySet<>(Collections.emptyList(), comparator);
        }
        return subSet(first(), true, toElement, inclusive);
    }

    @Override
    public NavigableSet<E> tailSet(E fromElement, boolean inclusive) {
        if (isEmpty()) {
            return new ArraySet<>(Collections.emptyList(), comparator);
        }
        return subSet(fromElement, inclusive, last(), true);
    }

    @Override
    public SortedSet<E> subSet(E fromElement, E toElement) {
        return subSet(fromElement, true, toElement, false);
    }

    @Override
    public SortedSet<E> headSet(E toElement) {
        return headSet(toElement, false);
    }

    @Override
    public SortedSet<E> tailSet(E fromElement) {
        return tailSet(fromElement, true);
    }

    @Override
    public Comparator<? super E> comparator() {
        return comparator;
    }

    private class ListView extends AbstractList<E> implements List<E>, RandomAccess {

        private final int fromIndex, toIndex;
        private final boolean ascending;

        ListView(List<? extends E> list) {
            fromIndex = 0;
            toIndex = list.size();
            ascending = false;
        }

        ListView(ListView listView) {
            fromIndex = listView.fromIndex;
            toIndex = listView.toIndex;
            ascending = !listView.ascending;
        }

        ListView(int fromIndex, int toIndex, boolean ascending) {
            this.fromIndex = fromIndex;
            this.toIndex = toIndex;
            this.ascending = ascending;
        }

        @Override
        public E get(int index) {
            if (!validIndex(index)) {
                throw new IndexOutOfBoundsException();
            }
            return ascending ? elements.get(fromIndex + index) : elements.get(toIndex - index - 1);
        }

        @Override
        public int size() {
            return toIndex - fromIndex;
        }
    }
}
