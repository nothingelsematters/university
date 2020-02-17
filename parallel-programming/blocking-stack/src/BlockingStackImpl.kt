import java.util.concurrent.atomic.*
import kotlin.coroutines.Continuation
import kotlin.coroutines.suspendCoroutine
import kotlin.coroutines.resume

class BlockingStackImpl<E> : BlockingStack<E> {

    // ==========================
    // Segment Queue Synchronizer
    // ==========================

    private val enqIdx: AtomicReference<Resumer<E>>
    private val deqIdx: AtomicReference<Resumer<E>>

    init {
        val dummy = Resumer<E>(null)
        enqIdx = AtomicReference<Resumer<E>>(dummy)
        deqIdx = AtomicReference<Resumer<E>>(dummy)
    }

    private class Resumer<E>(
        val action: Continuation<E>?,
        val next: AtomicReference<Resumer<E>> = AtomicReference<Resumer<E>>()
    )

    private suspend fun suspend(): E = suspendCoroutine {
        while (true) {
            val oldTail = deqIdx.get()
            val newTail = Resumer<E>(it)

            if (oldTail.next.compareAndSet(null, newTail)) {
                deqIdx.compareAndSet(oldTail, newTail)
                break
            }
        }
    }

    private fun resume(element: E) {
        while (true) {
            val oldHead = enqIdx.get()
            val newHead = oldHead.next.get()

            if (newHead != null && oldHead != deqIdx.get() && enqIdx.compareAndSet(oldHead, newHead)) {
                newHead.action?.resume(element)
                return
            }
        }
    }

    // ==============
    // Blocking Stack
    // ==============


    private val head = AtomicReference<Node<E>?>()
    private val elements = AtomicInteger()

    override fun push(element: E) {
        if (elements.getAndIncrement() < 0) {
            resume(element)
            return
        }

        while (true) {
            val oldHead = head.get()
            
            if (oldHead?.element === SUSPENDED) {
                val newHead = oldHead.next.get()
                if (head.compareAndSet(oldHead, newHead)) {
                    resume(element)
                    return
                }
                continue
            }

            if (head.compareAndSet(oldHead, Node(element, AtomicReference<Node<E>?>(oldHead)))) break
        }
    }

    override suspend fun pop(): E {
        if (elements.getAndDecrement() <= 0) return suspend()

        while (true) {
            val oldHead = head.get()
            if (oldHead == null) {
                if (head.compareAndSet(oldHead, Node<E>(SUSPENDED))) return suspend()
                continue
            }

            if (head.compareAndSet(oldHead, oldHead.next.get())) return oldHead.element as E
        }
    }
}

private class Node<E>(val element: Any?, val next: AtomicReference<Node<E>?> = AtomicReference<Node<E>?>())

private val SUSPENDED = Any()
