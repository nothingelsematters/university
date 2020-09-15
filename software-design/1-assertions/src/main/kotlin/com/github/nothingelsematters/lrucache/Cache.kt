package com.github.nothingelsematters.lrucache

interface Cache<K, V> {

    val size: Int

    operator fun get(key: K): V?

    operator fun set(key: K, value: V)
}
