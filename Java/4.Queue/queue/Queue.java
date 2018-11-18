package queue;

public interface Queue extends Copiable {
//pre: element != null
//post: size = size' + 1 && forall i=head..head+size'-1 : a[i % a.length]' = a[i % a.length]
//&& shift = shift' && a[(head+size-1)%a.length] = element
void enqueue(Object element);
//pre: size > 0
//post: R = a[shift] && size = size'-1 && shift = shift' + 1
//&& forall i=shift'+1..shift+size'-1 : a[i%a.length]' = a[i%a.length]
Object dequeue();
//pre: size > 0
//post: R = a[shift] && queue immutable
Object element();
//post: R = size && queue immutable
int size();
//post: R = (size = 0) && queue immutable
boolean isEmpty();
//post: size = 0 && shift = 0 && forall i=0..a.length-1 a[i] = null && a.length = 16
void clear();
//post: R = Object[] b: forall i = shift..shift+size-1 a[i % a.length] = b[i]
//&& size = size' && shift = shift' && forall i = shift..shift+size-1 a[i % a.length]' = a[i % a.length]
Object[] toArray();
//post: R = Queue
Queue makeCopy();
}
