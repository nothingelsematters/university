package queue;

public class ArrayQueueADT {
//inv: size >=0 && forall i=shift..shift+size-1: a[i % a.length] != null
private static int size;
private static int shift;
private static Object[] elements = new Object[16];
//pre: element != null
//post: size = size' + 1 && forall i=head..head+size'-1 : a[i % a.length]' = a[i % a.length]
//&& shift = shift' && a[(head+size-1)%a.length] = element
public static void enqueue(ArrayQueueADT queue, Object element) {
        assert element != null;
        ensureCapacity(queue, queue.size + 1);
        queue.elements[(queue.shift + queue.size++) % queue.elements.length] = element;
}
private static void ensureCapacity(ArrayQueueADT queue, int capacity) {
        if (capacity <= queue.elements.length) {
                return;
        }
        Object[] newElements = new Object[2 * capacity];
        arraying(queue, newElements);
        queue.elements = newElements;
        queue.shift = 0;
}
//pre: size > 0
//post: R = a[shift] && size = size' && shift = shift'
//&& forall i=shift'+1..shift+size-1 : a[i%a.length]' = a[i%a.length]
public static Object element(ArrayQueueADT queue) {
        assert queue.size > 0;
        return queue.elements[shift];
}
//pre: size > 0
//post: R = a[shift] && size = size'-1 && shift = shift' + 1
//&& forall i=shift'+1..shift+size'-1 : a[i%a.length]' = a[i%a.length]
public static Object dequeue(ArrayQueueADT queue) {
        assert queue.size > 0;
        queue.size--;
        int temp = queue.shift;
        queue.shift = (queue.shift + 1) % queue.elements.length;
        return queue.elements[temp];
}
//post: R = size && size = size' && shift = shift' && forall i=shift..shift+size-1 : a[i % a.length]' = a[i % a.length]
public static int size(ArrayQueueADT queue) {
        return queue.size;
}
//post: R = (size = 0) && size = size' && shift = shift'
//&& forall i=shift..shift+size-1 : a[i % a.length]' =a[i % a.length]
public static boolean isEmpty(ArrayQueueADT queue) {
        return (queue.size == 0);
}
//post: size = 0 && shift = 0 && forall i=0..a.length-1 a[i] = null && a.length = 16
public static void clear(ArrayQueueADT queue) {
        queue.size = 0;
        queue.shift = 0;
        Object[] newElements = new Object[16];
        queue.elements = newElements;
}
//post: R = Object[] b: forall i = shift..shift+size-1 a[i % a.length] = b[i]
//&& size = size' && shift = shift' && forall i = shift..shift+size-1 a[i % a.length]' = a[i % a.length]
public static Object[] toArray(ArrayQueueADT queue) {
        assert queue.size > 0;
        Object[] arr = new Object[size];
        arraying(queue, arr);
        return arr;
}
private static void arraying(ArrayQueueADT queue, Object[] arr) {
        assert queue.size > 0;
        for (int i = 0; i < queue.size; ++i) {
                arr[i] = queue.elements[(i + shift) % queue.elements.length];
        }
}
}
