package queue;

public class ArrayQueue {
//inv: size >=0 && forall i=shift..shift+size-1: a[i % a.length] != null
private int size;
private int shift;
private Object[] elements = new Object[16];
//pre: element != null
//post: size = size' + 1 && forall i=head..head+size'-1 : a[i % a.length]' = a[i % a.length]
//&& shift = shift' && a[(head+size-1)%a.length] = element
public void enqueue(Object element) {
        assert element != null;
        ensureCapacity(size + 1);
        elements[(shift + size++) % elements.length] = element;
}
private void ensureCapacity(int capacity) {
        if (capacity <= elements.length) {
                return;
        }
        Object[] newElements = new Object[2 * capacity];
        arraying(newElements);
        elements = newElements;
        shift = 0;
}
//pre: size > 0
//post: R = a[shift] && size = size' && shift = shift'
//&& forall i=shift'+1..shift+size-1 : a[i%a.length]' = a[i%a.length]
public Object element() {
        assert size > 0;
        return elements[shift];
}
//pre: size > 0
//post: R = a[shift] && size = size'-1 && shift = shift' + 1
//&& forall i=shift'+1..shift+size'-1 : a[i%a.length]' = a[i%a.length]
public Object dequeue() {
        assert size > 0;
        size--;
        int temp = shift;
        shift = (shift + 1) % elements.length;
        return elements[temp];
}
//post: R = size && size = size' && shift = shift' && forall i=shift..shift+size-1 : a[i % a.length]' = a[i % a.length]
public int size() {
        return size;
}
//post: R = (size = 0) && size = size' && shift = shift'
//&& forall i=shift..shift+size-1 : a[i % a.length]' =a[i % a.length]
public boolean isEmpty() {
        return (size == 0);
}
//post: size = 0 && shift = 0 && forall i=0..a.length-1 a[i] = null && a.length = 16
public void clear() {
        size = 0;
        shift = 0;
        Object[] newElements = new Object[16];
        elements = newElements;
}
//post: R = Object[] b: forall i = shift..shift+size-1 a[i % a.length] = b[i]
//&& size = size' && shift = shift' && forall i = shift..shift+size-1 a[i % a.length]' = a[i % a.length]
public Object[] toArray() {
        assert size > 0;
        Object[] arr = new Object[size];
        arraying(arr);
        return arr;
}
private void arraying(Object[] arr) {
        assert size > 0;
        for (int i = 0; i < size; ++i) {
                arr[i] = elements[(i + shift) % elements.length];
        }
}
}
