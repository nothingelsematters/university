import java.lang.ThreadLocal.withInitial


class Solution : AtomicCounter {
    private val root = Node(0)
    private val last = withInitial { -> root }

    override fun getAndAdd(x: Int): Int {
        var node: Node
        var old: Int

        do {
            old = last.get().value
            node = Node(old + x)
            last.set(last.get().next.decide(node))
        } while (last.get() != node)

        return old
    }

    private data class Node(val value: Int = 0, val next: Consensus<Node> = Consensus<Node>())
}
