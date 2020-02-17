import kotlin.coroutines.Continuation
import kotlin.coroutines.resume
import kotlin.coroutines.suspendCoroutine
import java.util.concurrent.atomic.AtomicReference

class SynchronousQueueMS<E> : SynchronousQueue<E> {
    private val head: AtomicReference<Node>
    private val tail: AtomicReference<Node>
    init {
        val d = Dummy()
        head = AtomicReference<Node>(d)
        tail = AtomicReference<Node>(d)
    }
    
    override suspend fun send(element: E) {
        while (true) {
            val t = tail.get()
            val h = head.get()

            if (t == h || t is Sender<*>) {
                val res = suspendCoroutine<Unit?> sc@ { cont ->
                    val newNode = Sender(element, cont)

                    if (!t.next.compareAndSet(null, newNode)) {
                        cont.resume(null)
                        return@sc
                    }
                    tail.compareAndSet(t, newNode)
                }
                if (res != null) return

            } else {
                val headNext = h.next.get() as? Receiver<E> ?: continue

                if (head.compareAndSet(h, headNext)) {
                    headNext.action.resume(element)
                    return
                }
            }
        }
    }

    override suspend fun receive(): E {
        while (true) {
            val t = tail.get()
            val h = head.get()

            if (t == h || t is Receiver<*>) {
                val res = suspendCoroutine<E?> sc@ { cont ->
                    val newNode = Receiver(cont)

                    if (!t.next.compareAndSet(null, newNode)) {
                        cont.resume(null)
                        return@sc
                    }
                    tail.compareAndSet(t, newNode)
                }
                if (res != null) return res

            } else {
                val headNext = h.next.get() as? Sender<E> ?: continue

                if (head.compareAndSet(h, headNext)) {
                    headNext.action.resume(Unit)
                    return headNext.element
                }
            }
        }
    }

    private abstract class Node(val next: AtomicReference<Node>)

    private class Receiver<E>(
        val action: Continuation<E>,
        next: AtomicReference<Node> = AtomicReference<Node>()
    ) : Node(next)

    private class Sender<E>(
        val element: E,
        val action: Continuation<Unit>,
        next: AtomicReference<Node> = AtomicReference<Node>()
    ) : Node(next)

    private class Dummy() : Node(AtomicReference<Node>())
}
