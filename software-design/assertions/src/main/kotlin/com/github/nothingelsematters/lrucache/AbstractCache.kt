package com.github.nothingelsematters.lrucache

abstract class AbstractCache<K, V>(val capacity: Int) : Cache<K, V> {

    final override val size: Int
        get() {
            checkSize()
            return getCurrentSize()
        }

    init {
        require(capacity > 0) { "Cache capacity should be positive non zero" }
    }

    private fun checkSize() = check(getCurrentSize() <= capacity) { "Size should be less or equals to cache capacity" }

    final override fun get(key: K): V? {
        val oldSize = size
        val element = getElement(key)

        check(size == oldSize) { "Size should not change on read operation" }
        return element
    }

    final override fun set(key: K, value: V) {
        val oldSize = size
        val replacingValue = containsKey(key) || size == capacity

        addElement(key, value)

        check(size == if (replacingValue) oldSize else oldSize + 1) {
            "Size should increment on insertion of a new key if the capacity won't be exceeded and should not otherwise"
        }
        check(containsKey(key)) { "Element should be put" }
        checkSize()
    }

    protected abstract fun getElement(key: K): V?

    protected abstract fun addElement(key: K, value: V)

    protected abstract fun getCurrentSize(): Int

    protected abstract fun containsKey(key: K): Boolean
}
