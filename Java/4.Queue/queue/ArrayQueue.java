package queue;

import java.lang.*;

public class ArrayQueue extends AbstractQueue implements Copiable {
private Object[] elements = new Object[16];
private int shift;

protected void enqueueImpl(Object element) {
        ensureCapacity(size + 1);
        elements[(shift + size) % elements.length] = element;
}

private void ensureCapacity(int capacity) {
        if (capacity > elements.length) {
                elements = arraying(2 * capacity);
                shift = 0;
        }
}

protected void remove() {
        shift = (shift + 1) % elements.length;
}

protected Object elementImpl() {
        return elements[shift];
}

public ArrayQueue makeCopy() {
        final ArrayQueue copy = new ArrayQueue();
        copy.size = size;
        copy.elements = arraying(size);
        return copy;
}

public void clearImpl() {
        Object[] newElements = new Object[16];
        elements = newElements;
        shift = 0;
}

private Object[] arraying(int newSize) {
        assert size > 0;
        Object[] arr = new Object[newSize];
        if (shift + size <= elements.length) {
                System.arraycopy(elements, shift, arr, 0, size);
        } else {
                System.arraycopy(elements, shift, arr, 0, elements.length - shift);
                System.arraycopy(elements, 0, arr, elements.length - shift, size + shift - elements.length);
        }
        return arr;
}
}
