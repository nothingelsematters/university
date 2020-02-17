package faaqueue;

import kotlinx.atomicfu.*;

import static faaqueue.FAAQueue.Node.NODE_SIZE;


public class FAAQueue<T> implements Queue<T> {
    private static final Object DONE = new Object(); // Marker for the "DONE" slot state; to avoid memory leaks

    private final AtomicRef<Node> head; // Head pointer, similarly to the Michael-Scott queue (but the first node is _not_ sentinel)
    private final AtomicRef<Node> tail; // Tail pointer, similarly to the Michael-Scott queue

    public FAAQueue() {
        Node firstNode = new Node();
        head = new AtomicRef<>(firstNode);
        tail = new AtomicRef<>(firstNode);
    }

    @Override
    public void enqueue(T x) {
        while (true) {
            final Node currentTail = this.tail.getValue();
            final int enqIdx = currentTail.enqIdx.getAndIncrement();

            if (enqIdx >= NODE_SIZE) {
                Node newTail = new Node(x);
                if (!currentTail.next.compareAndSet(null, newTail)) continue;
                if (tail.compareAndSet(currentTail, newTail)) return;
            } else if (currentTail.data.get(enqIdx).compareAndSet(null, x)) {
                return;
            }
        }
    }

    @Override
    public T dequeue() {
        while (true) {
            final Node currentHead = this.head.getValue();
            if (currentHead.isEmpty()) {
                final Node next = currentHead.next.getValue();
                if (next == null) return null;
                this.head.compareAndSet(currentHead, next);
                continue;
            }

            int deqIdx = currentHead.deqIdx.getAndIncrement();
            if (deqIdx >= NODE_SIZE) continue;

            final Object res = currentHead.data.get(deqIdx).getAndSet(DONE);
            if (res == null) continue;
            return (T) res;
        }
    }

    static class Node {
        static final int NODE_SIZE = 2; // CHANGE ME FOR BENCHMARKING ONLY

        private final AtomicRef<Node> next = new AtomicRef<Node>(null);
        private final AtomicInt enqIdx = new AtomicInt(0); // index for the next enqueue operation
        private final AtomicInt deqIdx = new AtomicInt(0); // index for the next dequeue operation
        private final AtomicArray<Object> data = new AtomicArray<Object>(NODE_SIZE);

        Node() {}

        Node(Object x) {
            enqIdx.getAndIncrement();
            data.get(0).getAndSet(x);
        }

        private boolean isEmpty() {
            final int d = deqIdx.getValue();
            final int e = enqIdx.getValue();
            return d >= e || d >= NODE_SIZE;
        }
    }
}
