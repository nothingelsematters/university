package com.github.nothingelsematters.lrucache

import java.util.LinkedHashMap

private const val loadFactor = 0.75f
private const val accessOrder = true

class LinkedHashMapLruCache<K, V>(val capacity: Int = 256) :
    LinkedHashMap<K, V>(capacity, loadFactor, accessOrder), Cache<K, V> {

    override fun set(key: K, value: V) {
        put(key, value)
    }

    override fun removeEldestEntry(eldest: MutableMap.MutableEntry<K, V>?) = size > capacity
}
