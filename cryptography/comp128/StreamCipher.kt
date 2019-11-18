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
        register[pointer] = xoredBits.map { register[(pointer + it) % size] }.reduce { l, r -> l != r }
        justShift()
    }

    fun justShift() {
        pointer = (pointer + 1) % size
    }

    fun clear() {
        register.clear()
        pointer = 0
    }
}

class ShiftCluster(val registers: List<ShiftRegister>) {
    fun launch(key: ByteArray, frame: Int) {
        registers.forEach(ShiftRegister::clear)

        key.forEach { byte ->
            repeat (Byte.SIZE_BITS) { xorRegisters(byte and (1 shl it) != 0.toByte()) }
        }
        repeat (22) { xorRegisters(frame and (1 shl it) != 0) }
        repeat(100) { getAndShift() }
    }

    fun xorRegisters(value: Boolean) {
        registers.forEach { reg ->
            reg.justShift()
            reg.xorPointed(value)
        }
    }

    private fun getAndShift(): Boolean =
        registers
            .map(ShiftRegister::output)
            .reduce { l, r -> l != r }
            .also {
                registers
                    .partition(ShiftRegister::control)
                    .toList()
                    .maxBy(List<ShiftRegister>::size)
                    ?.forEach(ShiftRegister::shift)
            }

    public fun getBlock(): BooleanArray = BooleanArray(BLOCK_SIZE) { getAndShift() }

}

/*
 * A5/1 steam cipher
 */
fun streamEncrypt(key: ByteArray, message: ByteArray): ByteArray {
    lengthAssertion(key, "key", 8)

    val cluster = ShiftCluster(
        listOf(
            ShiftRegister(19, listOf(13, 16, 17, 18), 8),
            ShiftRegister(22, listOf(20, 21), 10),
            ShiftRegister(23, listOf(7, 20, 21, 22), 10)
        )
    )

    val result = BooleanArray(message.size * 8)
    val framesCount = message.size / BLOCK_SIZE + if (message.size * 8 % BLOCK_SIZE == 0) 0 else 1

    repeat (framesCount) { i ->
        cluster.launch(key, i)
        val block = cluster.getBlock()

        repeat (BLOCK_SIZE) { j ->
            val index = i * BLOCK_SIZE + j
            val messageBit = (message[index / Byte.SIZE_BITS] ushr (index % Byte.SIZE_BITS)) and 1 == 1.toByte()
            result[index] = messageBit != block[j]
        }
    }

    return result
        .toList()
        .chunked(Byte.SIZE_BITS) {
            it.foldIndexed(0) { index, result, bit -> if (bit) result or (1 shl index) else result }.toByte()
        }
        .toByteArray()
}

fun main() {
    val key = listOf(0x12, 0x23, 0x45, 0x67, 0x89, 0xAB, 0xCD, 0xEF).map(Int::toByte).toByteArray()
    val frame = 0x134
    val expected = listOf(0x53, 0x4E, 0xAA, 0x58, 0x2F, 0xE8, 0x15, 0x1A, 0xB6, 0xE1, 0x85, 0x5A, 0x72, 0x8C, 0x00,
        0x24, 0xFD, 0x35, 0xA3, 0x5D, 0x5F, 0xB6, 0x52, 0x6D, 0x32, 0xF9, 0x06, 0xDF, 0x1A, 0xC0).map(Int::toByte)

    val cluster = ShiftCluster(
        listOf(
            ShiftRegister(19, listOf(13, 16, 17, 18), 8),
            ShiftRegister(22, listOf(20, 21), 10),
            ShiftRegister(23, listOf(7, 20, 21, 22), 10)
        )
    )

    cluster.launch(key, frame)
    val result = cluster
        .getBlock()
        .toList()
        .chunked(Byte.SIZE_BITS) {
            it.foldIndexed(0) { index, result, bit -> if (bit) result or (1 shl index) else result }.toByte()
        }

    println("expected: ${expected.map { it.toBits() }.joinToString(separator = "")}")
    println("result:   ${result.map { it.toBits() }.joinToString(separator = "")}")
}

fun Byte.toBits(): String {
    var res = ""
    for (i in 0..7) {
        res += if (ushr(7 - i).and(1) == 1.toByte()) "1" else "0"
    }
    return res
}
