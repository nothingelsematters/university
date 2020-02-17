package msqueue;

import kotlinx.atomicfu.AtomicRef;

public class MSQueue implements Queue {
    private AtomicRef<Node> head;
    private AtomicRef<Node> tail;

    public MSQueue() {
        Node dummy = new Node(0);
        this.head = new AtomicRef<>(dummy);
        this.tail = new AtomicRef<>(dummy);
    }

    @Override
    public void enqueue(int value) {
        Node newTail = new Node(value);
        while (true) {
            Node curTail = tail.getValue();
            if (curTail.next.compareAndSet(null, newTail)) {
                tail.compareAndSet(curTail, newTail);
                return;
            } else {
                tail.compareAndSet(curTail, curTail.next.getValue());
            }
        }
    }

    @Override
    public int dequeue() {
        while (true) {
            Node curHead = head.getValue();
            Node headNext = curHead.next.getValue();
            if (headNext == null) {
                return Integer.MIN_VALUE;
            }

            if (head.compareAndSet(curHead, headNext)) {
                return headNext.value;
            }
        }
    }

    @Override
    public int peek() {
        AtomicRef<Node> curHead = head;
        Node next = curHead.getValue().next.getValue();
        if (next == null) {
            return Integer.MIN_VALUE;
        }
        return next.value;
    }

    private class Node {
        final int value;
        AtomicRef<Node> next;

        Node(int value) {
            this.value = value;
            next = new AtomicRef<>(null);
        }
    }
}
