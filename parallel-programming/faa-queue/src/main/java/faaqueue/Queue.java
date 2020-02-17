package faaqueue;

/**
 * Queue interface.
 *
 * @author Nikita Koval
 */
public interface Queue<T> {

    /**
     * Inserts the specified element into this queue
     *
     * @param x the element to add
     */
    void enqueue(T x);

    /**
     * Retrieves and removes the head of this queue,
     * or returns {@code null} if this queue is empty
     */
    T dequeue();
}
