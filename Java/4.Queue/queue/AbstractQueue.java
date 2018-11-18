package queue;

public abstract class AbstractQueue implements Queue {
protected int size;

public void enqueue(Object element) {
        assert element != null;
        enqueueImpl(element);
        size++;
}

protected abstract void enqueueImpl(Object element);

public Object element() {
        assert size > 0;
        return elementImpl();
}

protected abstract Object elementImpl();

public Object dequeue() {
        Object result = element();
        size--;
        remove();
        return result;
}

protected abstract void remove();

public void clear() {
        size = 0;
        clearImpl();
}

protected abstract void clearImpl();

public int size() {
        return size;
}

public Object[] toArray() {
        assert size > 0;
        Object[] arr = new Object[size];
        for (int i = 0; i < size; ++i) {
                arr[i] = dequeue();
                enqueue(arr[i]);
        }
        return arr;
}

public boolean isEmpty() {
        return size == 0;
}
}
