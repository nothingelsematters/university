package dijkstra

import java.util.Random
import java.util.PriorityQueue
import java.util.Queue
import java.util.concurrent.Phaser
import java.util.concurrent.locks.ReentrantLock
import kotlin.Comparator
import kotlin.concurrent.thread
import kotlinx.atomicfu.atomic

private val NODE_DISTANCE_COMPARATOR = compareBy<Node>(Node::distance)

public class PriorityMultiQueue<T : Any>(val amount: Int, val comparator: Comparator<T>) {
    private val queues = List(amount) { PriorityQueue(amount, comparator) }
    private val locks = List(amount) { ReentrantLock(true) }
    private val R = Random(0)

    public fun <F> withLockedRandom(function: (Queue<T>) -> F): F {
        while (true) {
            val n = R.nextInt(amount)

            if (locks[n].tryLock()) {
                return try {
                    function(queues[n])
                } finally {
                    locks[n].unlock()
                }
            }
        }
    }

    public fun add(element: T): Boolean = withLockedRandom { it.offer(element) }

    public fun take(): T? =
        withLockedRandom { first ->
            withLockedRandom { second ->
                minOf(first, second, compareBy(nullsLast(comparator), Queue<T>::peek)).poll()
            }
        }
}

private val activeNodes = atomic(1)

// Returns `Integer.MAX_VALUE` if a path has not been found.
fun shortestPathParallel(start: Node) {
    val workers = Runtime.getRuntime().availableProcessors()
    start.distance = 0

    val q = PriorityMultiQueue(2 * workers, NODE_DISTANCE_COMPARATOR)
    q.add(start)

    val onFinish = Phaser(workers + 1)
    activeNodes.lazySet(1)

    repeat(workers) {
        thread {
            while (activeNodes.value > 0) {
                val cur: Node = q.take() ?: continue

                for (edge in cur.outgoingEdges) {
                    val newPath = cur.distance + edge.weight

                    while (true) {
                        val oldPath = edge.to.distance
                        if (oldPath > newPath) {
                            if (!edge.to.casDistance(oldPath, newPath)) {
                                continue
                            }
                            q.add(edge.to)
                            activeNodes.incrementAndGet()
                        }
                        break
                    }
                }
                activeNodes.decrementAndGet()
            }
            onFinish.arrive()
        }
    }
    onFinish.arriveAndAwaitAdvance()
}
