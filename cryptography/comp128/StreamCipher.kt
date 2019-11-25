package comp128

import java.util.BitSet


class ShiftRegister(val size: Int, val xoredBits: List<Int>, val controlBit: Int) {
    private val register = BitSet(size)
    private var pointer = 0

    fun control(): Boolean = register[(pointer + controlBit) % size]
    fun output(): Boolean = register[(pointer + size - 1) % size]

    fun xorPointed(value: Boolean) {
        register[pointer] = register[pointer] != value
    }

    fun shift() {
        pointer = (pointer + size - 1) % size
        register[pointer] = xoredBits.map { register[(pointer + it + 1) % size] }.reduce { l, r -> l != r }
    }

    fun clear() {
        register.clear()
        pointer = 0
    }
}

class ShiftCluster(val registers: List<ShiftRegister>) {
    constructor(vararg registers: ShiftRegister) : this(registers.asList())

    fun launch(key: ByteArray, frame: Int) {
        registers.forEach(ShiftRegister::clear)

        key.forEach { byte ->
            repeat (Byte.SIZE_BITS) { xorRegisters(byte and (1 shl it) != 0.toByte()) }
        }
        repeat (22) { xorRegisters(frame and (1 shl it) != 0) }
        repeat(100) { getAndShift() }
    }

    private fun xorRegisters(value: Boolean) {
        registers.forEach { reg ->
            reg.shift()
            reg.xorPointed(value)
        }
    }

    private fun getAndShift(): Boolean {
        registers
            .partition(ShiftRegister::control)
            .toList()
            .maxBy(List<ShiftRegister>::size)
            ?.forEach(ShiftRegister::shift)

        return registers
            .map(ShiftRegister::output)
            .reduce { l, r -> l != r }
    }

    fun getBlock(): BooleanArray = BooleanArray(BLOCK_SIZE) { getAndShift() }
}

/*
 * A5/1 steam cipher
 */
fun streamEncrypt(key: ByteArray, message: ByteArray): ByteArray {
    lengthAssertion(key, "key", 8)

    val cluster = ShiftCluster(
        ShiftRegister(19, listOf(13, 16, 17, 18), 8),
        ShiftRegister(22, listOf(20, 21), 10),
        ShiftRegister(23, listOf(7, 20, 21, 22), 10)
    )

    val result = BooleanArray(message.size * Byte.SIZE_BITS)
    val framesCount = message.size * 8 / BLOCK_SIZE + if (message.size * 8 % BLOCK_SIZE == 0) 0 else 1

    repeat (framesCount) { i ->
        cluster.launch(key, i)
        val block = cluster.getBlock()
        val iterations = if (i == framesCount - 1) (message.size * 8) % BLOCK_SIZE else BLOCK_SIZE

        repeat (iterations) { j ->
            val index = i * BLOCK_SIZE + j
            result[index] = ((message[index / Byte.SIZE_BITS] and (1 shl (index % Byte.SIZE_BITS))) != 0.toByte()) != block[j]
        }
    }

    return result
        .toList()
        .chunked(Byte.SIZE_BITS) {
            it.foldIndexed(0) { index, result, bit -> if (bit) result or (1 shl index) else result }.toByte()
        }
        .toByteArray()
}
