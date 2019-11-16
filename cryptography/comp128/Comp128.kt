package comp128

class WrongLengthException(val field: String, val expected: Int, val reality: Int) :
    Exception("wrong length at $field: expected $expected, reality $reality")

fun lengthAssertion(array: ByteArray, name: String, expected: Int) {
    if (array.size != expected) throw WrongLengthException(name, expected, array.size)
}

fun Byte.cleverToInt(): Int = toInt() + if (toInt() < 0) 256 else 0
infix fun Byte.shl(shift: Int): Byte = cleverToInt().shl(shift).toByte()
infix fun Byte.ushr(shift: Int): Byte = cleverToInt().ushr(shift).toByte()
infix fun Byte.or(that: Int): Byte = cleverToInt().or(that).toByte()
infix fun Byte.or(that: Byte): Byte = cleverToInt().or(that.cleverToInt()).toByte()
infix fun Byte.and(that: Int): Byte = cleverToInt().and(that).toByte()


fun round(array: ByteArray, bits: ByteArray) {
    table.forEachIndexed { i, t ->
        for (j in 0 until (1 shl i)) {
            for (k in 0 until (1 shl (4 - i))) {
                val fromFirst = k + j * (1 shl (5 - i))
                val fromSecond = fromFirst + (1 shl (4 - i))
                val toFirst = (array[fromFirst] + (array[fromSecond] shl 1)) and ((1 shl (9 - i)) - 1)
                val toSecond = ((array[fromFirst] shl 1) + array[fromSecond]) and ((1 shl (9 - i)) - 1)
                array[fromFirst] = t[toFirst]
                array[fromSecond] = t[toSecond]
            }
        }
    }

    for (i in 0 until 32) {
        for (j in 0 until 4) {
            bits[(i shl 2) + j] = (array[i] ushr (3 - j)) and 1
        }
    }
}

/*
 * A3 / A8 initial funciton
 *
 * rand: challenge from base station
 * key: sim's key
 * return[0..3]: sres
 * return[4.11]: a5 session key
 */
fun a3a8(rand: ByteArray, key: ByteArray): ByteArray {
    lengthAssertion(rand, "rand", 16)
    lengthAssertion(key, "key", 16)

    val array = ByteArray(32)
    val bits = ByteArray(128)
    rand.copyInto(array, 16)

    repeat (7) {
        key.copyInto(array)
        round(array, bits)

        for (j in 0 until 16) {
            array[j + 16] = 0
            for (k in 0 until 8) {
                val new_bit = (((j shl 3) + k) * 17) and ((1 shl 6) - 1) // % 128
                array[j + 16] = array[j + 16] or (bits[new_bit] shl (7 - k))
            }
        }
    }
    key.copyInto(array)
    round(array, bits)

    val result = ByteArray(12)
    for (i in 0 until 4) {
        result[i] = (array[i shl 1] shl 4) or array[(i shl 1) + 1]
    }
    for (i in 0 until 6) {
        result[4 + i] = (array[(i shl 1) + 18] shl 6) or (array[(i shl 1) + 19] shl 2) or (array[(i shl 1) + 20] ushr 2)
    }
    result[10] = (array[30] shl 6) or (array[31] shl 2)
    result[11] = 0
    return result
}
