package cubehash

const val ROUNDS = 16
const val BLOCK_BYTES = 32

infix fun Int.rotate(n: Int) = shl(n) or ushr(Int.SIZE_BITS - n)
fun Byte.toPositiveInt(): Int = if (this >= 0) toInt() else (1 shl Byte.SIZE_BITS) + toInt()

fun xorPosition(state: IntArray, pos: Int, right: Int) {
    state[pos ushr 5] = state[pos ushr 5] xor right.shl(pos.ushr(3).and(3).shl(3))
}

private fun transform(state: IntArray) {
    val tmp = IntArray(16)

    repeat(ROUNDS) {
        // 1. Add x[0jklm] into x[1jklm] modulo 2^32, for each (j,k,l,m).
        for (i in 0 until 16) state[i + 16] += state[i]
        // 2. Rotate x[0jklm] upwards by 7 bits, for each (j,k,l,m).
        // 3. Swap x[00klm] with x[01klm], for each (k,l,m).
        // 4. Xor x[1jklm] into x[0jklm], for each (j,k,l,m).
        for (i in 0 until 16) tmp[i xor 8] = state[i]
        for (i in 0 until 16) state[i] = (tmp[i] rotate 7) xor state[i + 16]
        // 5. Swap x[1jk0m] with x[1jk1m], for each (j,k,m).
        // 6. Add x[0jklm] into x[1jklm] modulo 2^32, for each (j,k,l,m).
        for (i in 0 until 16) tmp[i xor 2] = state[i + 16]
        for (i in 0 until 16) state[i + 16] = tmp[i] + state[i]
        // 7. Rotate x[0jklm] upwards by 11 bits, for each (j,k,l,m).
        // 8. Swap x[0j0lm] with x[0j1lm], for each (j,l,m).
        for (i in 0 until 16) tmp[i xor 4] = state[i]
        for (i in 0 until 16) state[i] = (tmp[i] rotate 11) xor state[i + 16]
        // 9. Xor x[1jklm] into x[0jklm], for each (j,k,l,m).
        // 10. Swap x[1jkl0] with x[1jkl1], for each (j,k,l).
        for (i in 0 until 16) tmp[i xor 1] = state[i + 16]
        for (i in 0 until 16) state[i + 16] = tmp[i]
    }
}

fun hash(message: ByteArray, hashBitLength: Int = 512): ByteArray {
    val state = IntArray(32)
    if (hashBitLength < 8 || hashBitLength > 512 || hashBitLength and 7 != 0)
        throw IllegalArgumentException("hashBitLength must be in {8, 16, 24, ..., 512}")

    // INIT
    state[0] = hashBitLength ushr 3
    state[1] = BLOCK_BYTES
    state[2] = ROUNDS
    transform(state)

    // UPDATE
    var pos = 0
    var size = message.size * Byte.SIZE_BITS

    for (i in 0 until (size ushr 3)) {
        xorPosition(state, pos, message[i].toPositiveInt())
        pos += 8

        if (pos == BLOCK_BYTES shl 3) {
            transform(state)
            pos = 0
        }
    }

    if (size and 7 != 0) {
        val left = (size ushr 3) + 1
        xorPosition(state, pos, message[left].toPositiveInt())
        pos += left
    }

    // FINAL
    xorPosition(state, pos, 128.ushr(pos and 3))
    transform(state)
    state[31] = state[31] xor 1
    transform(state)
    transform(state)

    return ByteArray(hashBitLength ushr 3) { (state[it ushr 2] ushr (it.and(3).shl(3))).toByte() }
}
