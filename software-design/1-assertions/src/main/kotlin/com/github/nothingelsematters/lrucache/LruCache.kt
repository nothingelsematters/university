package com.github.nothingelsematters.lrucache

class LruCache<K, V>(capacity: Int = 256) : AbstractCache<K, V>(capacity) {

    private val containMap = HashMap<K, LinkedList.DataNode<K, V>>(capacity)

    private val recentOrderHead = LinkedList.DummyNode<K, V>()

    private val recentOrderTail = LinkedList.DummyNode<K, V>()

    init {
        recentOrderHead.next = recentOrderTail
        recentOrderTail.previous = recentOrderHead
    }

    private fun checkRecent(key: K) = check((recentOrderHead.next as? LinkedList.DataNode)?.key == key) {
        "The recent key should be on top"
    }

    override fun containsKey(key: K): Boolean = containMap.containsKey(key)

    override fun getCurrentSize(): Int = containMap.size

    override fun getElement(key: K): V? {
        val result = containMap[key]
        if (result != null) {
            removeNode(result)
            addNode(result.key, result.value)

            checkRecent(key)
        }
        return result?.value
    }

    override fun addElement(key: K, value: V) {
        containMap[key]?.run(::removeNode)
        if (containMap.size == capacity) {
            removeNode(recentOrderTail.previous as LinkedList.DataNode<K, V>)
        }
        addNode(key, value)

        checkRecent(key)
    }

    private fun addNode(key: K, value: V) {
        val headNext = recentOrderHead.next!!
        val newHead = LinkedList.DataNode(key, value, recentOrderHead, headNext)

        containMap[key] = newHead
        headNext.previous = newHead
        recentOrderHead.next = newHead
    }

    private fun removeNode(node: LinkedList.DataNode<K, V>) {
        val nextNode = node.next!!
        val previousNode = node.previous!!

        containMap.remove(node.key)
        nextNode.previous = previousNode
        previousNode.next = nextNode
    }

    private sealed class LinkedList<K, V>(var previous: LinkedList<K, V>?, var next: LinkedList<K, V>?) {

        class DummyNode<K, V>(
            previous: LinkedList<K, V>? = null,
            next: LinkedList<K, V>? = null
        ) : LinkedList<K, V>(previous, next)

        class DataNode<K, V>(
            val key: K,
            val value: V,
            previous: LinkedList<K, V>,
            next: LinkedList<K, V>
        ) : LinkedList<K, V>(previous, next)
    }
}
