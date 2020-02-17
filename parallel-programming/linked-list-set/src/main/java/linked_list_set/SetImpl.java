package linked_list_set;

import kotlinx.atomicfu.AtomicRef;

public class SetImpl implements Set {
    private static interface Node {
        public boolean removed();
    }

    private static class Alive implements Node {
        int key;
        AtomicRef<Node> next;

        Alive(int key, Node next) {
            this.key = key;
            this.next = new AtomicRef<>(next);
        }

        @Override
        public boolean removed() {
            return false;
        }
    }

    private static class Dead implements Node {
        Alive reference;

        Dead(Alive reference) {
            this.reference = reference;
        }

        @Override
        public boolean removed() {
            return true;
        }
    }


    private static class Window {
        Alive left;
        Alive right;
    }

    private final AtomicRef<Alive> head = new AtomicRef<>(new Alive(Integer.MIN_VALUE, new Alive(Integer.MAX_VALUE, null)));

    /**
     * Returns the {@link Window}, where left.key < key <= right.key
     */
    private Window findWindow(int key) {
        while (true) {
            Window w = new Window();
            w.left = head.getValue();
            w.right = (Alive) w.left.next.getValue();

            boolean failure = false;
            while (w.right.key < key) {
                Node next = w.right.next.getValue();
                if (next.removed()) {
                    if (!w.left.next.compareAndSet(w.right, ((Dead) next).reference)) {
                        failure = true;
                        break;
                    }
                    w.right = ((Dead) next).reference;
                } else {
                    w.left = w.right;
                    w.right = (Alive) next;
                }
            }

            if (!failure) {
                return w;
            }
        }
    }

    @Override
    public boolean add(int key) {
        while (true) {
            Window w = findWindow(key);
            if (w.right.key == key && !w.right.next.getValue().removed()) {
                return false;
            }

            if (w.left.next.compareAndSet(w.right, new Alive(key, w.right))) {
                return true;
            }
        }
    }

    @Override
    public boolean remove(int key) {
        while (true) {
            Window w = findWindow(key);
            if (w.right.key != key) {
                return false;
            }

            Node next = w.right.next.getValue();
            if (next.removed()) {
                return false;
            }
            if (w.right.next.compareAndSet(next, new Dead((Alive) next))) {
                w.left.next.compareAndSet(w.right, next);
                return true;
            }
        }
    }

    @Override
    public boolean contains(int key) {
        Window w = findWindow(key);
        return w.right.key == key && !w.right.next.getValue().removed();
    }
}
