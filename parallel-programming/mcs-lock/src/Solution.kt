import java.util.concurrent.atomic.*

class Solution(val env: Environment) : Lock<Solution.Node> {
    private val tail = AtomicReference<Node>(null)

    override fun lock(): Node {
        val node = Node(AtomicReference<Boolean>(true))
        tail.getAndSet(node)?.run {
            next.value = node
            while (node.locked.value) env.park()
        }
        return node
    }

    override fun unlock(node: Node) {
        if (node.next.value == null) {
            if (tail.compareAndSet(node, null)) return
            while (node.next.value == null) continue
        }
        node.next.value?.run {
            locked.value = (false)
            env.unpark(thread)
        }
    }

    public data class Node(
        val locked: AtomicReference<Boolean> = AtomicReference<Boolean>(false),
        val next: AtomicReference<Node?> = AtomicReference<Node?>(null),
        val thread: Thread = Thread.currentThread()
    )
}
