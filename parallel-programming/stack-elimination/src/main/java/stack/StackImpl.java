package stack;

import java.util.Random;
import java.util.List;
import java.util.Collections;
import kotlinx.atomicfu.AtomicRef;

public class StackImpl implements Stack {
    private static class Node {
        final AtomicRef<Node> next;
        final int x;

        Node(int x, Node next) {
            this.next = new AtomicRef<>(next);
            this.x = x;
        }
    }

    private static Random R = new Random(0);
    private static int ARRAY_SIZE = 60;
    private static int WAITING = 20;
    private static int NEIGHBOURS = 1;

    // head pointer
    private AtomicRef<Node> head = new AtomicRef<>(null);

    private List<AtomicRef<Integer>> eliminationArray = Collections.nCopies(ARRAY_SIZE, new AtomicRef<Integer>(null));

    @Override
    public void push(int x) {
        int index = R.nextInt(ARRAY_SIZE);
        for (int i = Math.max(0, index - NEIGHBOURS); i < Math.min(ARRAY_SIZE, index + NEIGHBOURS); i++) {
            Integer coatedX = x;
            if (eliminationArray.get(i).compareAndSet(null, coatedX)) {

                for (int j = 0; j < WAITING; j++) {
                    Integer ai = eliminationArray.get(i).getValue();
                    if (ai == null || ((int) ai) != x) {
                        return;
                    }
                }

                if (!eliminationArray.get(i).compareAndSet(coatedX, null)) {
                    return;
                }
                break;
            }
        }

        while (true) {
            Node localHead = head.getValue();
            AtomicRef<Node> newHead = new AtomicRef<Node>(new Node(x, localHead));
            if (head.compareAndSet(localHead, newHead.getValue())) {
                return;
            }
        }
    }

    @Override
    public int pop() {
        int index = R.nextInt(ARRAY_SIZE);
        for (int i = Math.max(0, index - NEIGHBOURS); i < Math.min(ARRAY_SIZE, index + NEIGHBOURS); i++) {
            Integer ai = eliminationArray.get(i).getValue();
            if (ai != null && eliminationArray.get(i).compareAndSet(ai, null)) {
                return ai;
            }
        }

        while (true) {
            Node localHead = head.getValue();
            if (localHead == null) {
                return Integer.MIN_VALUE;
            }
            if (head.compareAndSet(localHead, localHead.next.getValue())) {
                return localHead.x;
            }
        }
    }
}
