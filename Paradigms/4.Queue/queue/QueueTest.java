package queue;

public class QueueTest {
    public static void fill(Queue queue) {
        for (int i = 0; i < 3; i++) {
            queue.enqueue(i);
        }
    }

    public static void dump(Queue queue) {
        while (!queue.isEmpty()) {
            System.out.println(queue.size() + " " +
                queue.element() + " " + queue.dequeue());
        }
    }

    public static void test(Queue queue) {
        fill(queue);
        Queue copy = queue.makeCopy();

        dump(queue);
        System.out.println();

        dump(copy);
        System.out.println("-------");
    }

    public static void main(String[] args) {
        test(new ArrayQueue());
        test(new LinkedQueue());
    }
}
