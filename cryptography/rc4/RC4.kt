package rc4

import java.io.InputStream
import java.io.OutputStream
import java.io.IOException
import java.math.BigInteger


const val SIZE = 256

fun ByteArray.swapIndices(first: Int, second: Int) {
    this[first] = this[second].also { this[second] = this[first] }
}

fun encrypt(key: ByteArray, input: InputStream, output: OutputStream) {
    // initializing
    val array = ByteArray(SIZE, Int::toByte)
    var second = 0
    array.forEachIndexed { first, element ->
        second = (second + element + key[first % key.size] + SIZE) % SIZE
        array.swapIndices(first, second)
    }

    // generating
    var first = 0
    second = 0
    loop@ while (true) {
        val read = when (val b = input.read()) {
            in 0..255 -> b.toByte()
            -1 -> break@loop
            else -> throw IOException("illegal byte")
        }

        first = (first + 1 + SIZE) % SIZE
        second = (second + array[first] + SIZE) % SIZE
        array.swapIndices(first, second)
        val write = array[(array[first] + array[second] + SIZE * 2) % SIZE]

        output.write(read.toInt() xor write.toInt())
    }
}
