package com.github.nothingelsematters.lrucache

import kotlin.random.Random
import kotlin.test.Test
import kotlin.test.assertEquals

class LruCacheTest {

    private val random = Random.Default

    private val testCount = 1000

    private fun randomInt() = random.nextInt() % 100

    @Test(expected = IllegalArgumentException::class)
    fun `IllegalArgumentException on negative capacity init attempt `() {
        LruCache<Int, Int>(-1)
    }

    @Test
    fun `access set value test`() {
        val cache = LruCache<Int, Int>()

        repeat(testCount) {
            val key = randomInt()
            val value = randomInt()
            cache[key] = value
            assertEquals(value, cache[key])
        }
    }

    @Test
    fun `mapping updates test`() {
        val cache = LruCache<Int, Int>()

        repeat(testCount) {
            val key = randomInt()
            val oldValue = randomInt()
            val newValue = randomInt()

            cache[key] = oldValue
            cache[key] = newValue

            assertEquals(newValue, cache[key])
        }
    }

    @Test
    fun `latest gets invalidated test`() {
        val cache = LruCache<Int, Int>(1)
        val map = HashMap<Int, Int>()

        repeat(testCount) {
            val key = randomInt()
            val value = randomInt()
            cache[key] = value
            map[key] = value
            map.forEach { (currentKey, currentValue) ->
                assertEquals(if (currentKey == key) currentValue else null, cache[currentKey])
            }
        }
    }

    @Test
    fun `comparing with LinkedHashMap implementation test`() {
        val cache = LruCache<Int, Int>(50)
        val comparingCache = LinkedHashMapLruCache<Int, Int>(50)
        val requests = mutableSetOf<Int>()

        repeat(testCount * 10) {
            val key = randomInt()
            val value = randomInt()
            cache[key] = value
            comparingCache[key] = value
            requests.add(key)

            assertEquals(comparingCache.size, cache.size)
            requests.forEach { currentKey ->
                assertEquals(comparingCache[currentKey], cache[currentKey])
            }

        }
    }
}
