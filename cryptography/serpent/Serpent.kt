package serpent

import java.math.BigInteger
import java.security.InvalidKeyException
import java.io.File // TEST

// TODO make cipher interface

// Int utilities

infix fun Int.rotateLeft(bits: Int): Int = shl(bits) or ushr(Int.SIZE_BITS - bits)

fun Int.toByteList(): List<Byte> = List<Byte>(Int.SIZE_BITS / Byte.SIZE_BITS) {
    ((this ushr (Int.SIZE_BITS - (it + 1) * Byte.SIZE_BITS)) and ((1 shl Byte.SIZE_BITS) - 1)).toByte()
}

fun List<Int>.pairsByteList(): List<Byte> = List<Byte>(size / 2) {
    ((get(it * 2) shl (Byte.SIZE_BITS / 2)) or get(it * 2 + 1)).toByte()
}

// BigInteger utilities

fun BigInteger.takeBits(begin: Int, amount: Int = 1): BigInteger = // lowest is zeroth
    (this shr (begin - amount)) and ((1.toBigInteger() shl amount) - 1.toBigInteger())

fun BigInteger.toInts(n: Int, step: Int = Int.SIZE_BITS): List<Int> = List<Int>(n) {
    takeBits((it + 1) * step, step).toInt()
}

fun List<Byte>.toBigInteger(): BigInteger = BigInteger(1, toByteArray())
fun List<Byte>.toReversedBigInteger(): BigInteger = asReversed().toBigInteger()

fun BigInteger.orBitFrom(pos: Int, otherPos: Int, from: BigInteger): BigInteger =
    or(from.takeBits(otherPos + 1) shl pos)

// algorithm utilities

fun BigInteger.permutate(permutation: List<Int>, size: Int = BLOCK_BITS): BigInteger =
    permutation.foldIndexed(0.toBigInteger()) { index, result, element ->
        result.orBitFrom(size - index - 1, size - element - 1, this)
    }

fun BigInteger.keySubstitute(sbox: List<Int>): BigInteger {
    val size = BLOCK_BITS / 4

    return List<BigInteger>(BLOCK_BITS / 4) {
        takeBits(size * 3 + 1 + it)
            .orBitFrom(1, size * 2 + it, this)
            .orBitFrom(2, size * 1 + it, this)
            .orBitFrom(3,            it, this)
    }
    .map { sbox[it.toInt()] }
    .foldIndexed(0.toBigInteger()) { index, result, byte ->
        val bigByte = byte.toBigInteger()
        result
            .orBitFrom(           index, 0, bigByte)
            .orBitFrom(size     + index, 1, bigByte)
            .orBitFrom(size * 2 + index, 2, bigByte)
            .orBitFrom(size * 3 + index, 3, bigByte)
    }
}

fun BigInteger.substitute(sbox: List<Int>): BigInteger =
    toInts(32, 4).map { sbox[it] }.asReversed().pairsByteList().toBigInteger()


fun BigInteger.linearTransformation(transform: List<List<Int>>): BigInteger =
    transform.foldIndexed(0.toBigInteger()) { index, result, bits ->
        result or (bits.fold(0.toBigInteger()) { xorBit, bit -> xorBit xor takeBits(bit + 1) } shl index)
    }


// algorithm main part

fun generateSubkeys(lb: List<Byte>): List<BigInteger> {
    val key = lb.toReversedBigInteger()
    if (key >= (1.toBigInteger() shl KEY_SIZE)) {
        throw InvalidKeyException()
    }

    val keys = ArrayList<Int>(key.toInts(PARTS_KEY_SIZE))
    for (i in PARTS_KEY_SIZE until PARTS_KEY_SIZE + KEY_AMOUNT) {
        keys.add(
            (keys[i - 8] xor keys[i - 5] xor keys[i - 3] xor keys[i - 1] xor PHI xor (i - PARTS_KEY_SIZE))
                .rotateLeft(11)
        )
    }

    return keys
        .subList(PARTS_KEY_SIZE, keys.size)
        .windowed(size = SUBKEY_INTS, step = SUBKEY_INTS)
        .mapIndexed { index, el ->
            el.flatMap(Int::toByteList)
                .toBigInteger()
                .keySubstitute(SBOXES[(KEY_AMOUNT + 7 - index) % SBOXES.size])
                .permutate(INITIAL_PERMUTATION)
        }
}

fun encrypt(
    message: List<Byte>,
    keys: List<BigInteger>,
    initialPermutation: List<Int>,
    sboxes: List<List<Int>>,
    linearTransform: List<List<Int>>,
    finalPermutation: List<Int>
): List<Byte> = message
    .windowed(size = BLOCK_SIZE, step = BLOCK_SIZE, partialWindows = true)
    .flatMap {
        keys
            .subList(0, keys.lastIndex - 1)
            .foldIndexed(it.toReversedBigInteger().permutate(initialPermutation)) { index, element, key ->
                element
                    .xor(key)
                    .substitute(sboxes[index % sboxes.size])
                    .linearTransformation(linearTransform)
            }
            .xor(keys[keys.lastIndex - 1])
            .substitute(sboxes.last())
            .xor(keys.last())
            .permutate(finalPermutation)
            .toByteArray()
            .toList()
            .asReversed()
            .subList(0, BLOCK_SIZE)
    }

fun encrypt(key: List<Byte>, message: List<Byte>): List<Byte> =
    encrypt(message, generateSubkeys(key), INITIAL_PERMUTATION, SBOXES, LINEAR_TRANSFORMATION, FINAL_PERMUTATION)

fun decrypt(key: List<Byte>, message: List<Byte>): List<Byte> =
    /* encrypt(message, generateSubkeys(key).asReversed(),
        FINAL_PERMUTATION, INVERSED_SBOXES, INVERSED_LINEAR_TRANSFORMATION, INITIAL_PERMUTATION) */

        message
            .windowed(size = BLOCK_SIZE, step = BLOCK_SIZE, partialWindows = true)
            .flatMap {
                val keys = generateSubkeys(key)
                var result = it.toReversedBigInteger().permutate(INITIAL_PERMUTATION)

                result
                    .xor(keys.last())
                    .substitute(INVERSED_SBOXES.last())
                    .xor(keys[keys.lastIndex - 1])
                for (i in keys.lastIndex - 2 downTo 0) {
                    result = result
                        .linearTransformation(INVERSED_LINEAR_TRANSFORMATION)
                        .substitute(INVERSED_SBOXES[i % INVERSED_SBOXES.size])
                        .xor(keys[i])
                }
                result
                    .permutate(FINAL_PERMUTATION)
                    .toByteArray()
                    .toList()
                    .asReversed()
                    .subList(0, BLOCK_SIZE)
            }


/* TEST */

fun fromFile(fileName: String, length: Int): List<Byte> {
    val bytes = ByteArray(length)
    val stream = File(fileName).inputStream()
    stream.read(bytes)
    stream.close()
    return bytes.toList()
}

fun main() {
    /* val inp = fromFile("../des/tests/encryption.in", 2000)
    val oup = fromFile("tests/encryption.out", 2000)
    val res = encrypt(listOf(0x01, 0x23, 0x45, 0x67, 0x89, 0xab, 0xcd, 0xef,
            0x01, 0x23, 0x45, 0x67, 0x89, 0xab, 0xcd, 0xef,
            0x01, 0x23, 0x45, 0x67, 0x89, 0xab, 0xcd, 0xef,
            0x01, 0x23, 0x45, 0x67, 0x89, 0xab, 0xcd, 0xef).map { it.toByte() },
                inp)
    if (res == oup) {
        println("SUCCESS")
    } else {
        println("FAILURE")
    } */
    /* println(res)
    println("OUTPUT")
    println(oup) */
    val test =
        listOf(0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0A, 0x0B, 0x0C, 0x0D, 0x0E, 0x0F)
            .map(Int::toByte)

    val expected = "b765b0de3d7d9f6d056080aef28e4c62".toUpperCase()

    val key = listOf(0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0A, 0x0B, 0x0C, 0x0D, 0x0E, 0x0F,
            0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x18, 0x19, 0x1A, 0x1B, 0x1C, 0x1D, 0x1E, 0x1F). map(Int::toByte)

    /* println("was: ${test.to16()}")
    println("key: ${key.to16()}") */
    println(if (BigInteger(1, decrypt(key, test).toByteArray()).toString(16).toUpperCase() == expected) "SUCCESS" else "FAILURE")
    println("now: ${BigInteger(1, decrypt(key, test).toByteArray()).toString(16).toUpperCase()}")
    println("exp: $expected")
}

/* DEBUG */

fun BigInteger.to16(): String = toString(16).toUpperCase()
fun ByteArray.to16(): String = BigInteger(1, this).to16()
fun List<Byte>.to16(): String = map { it.toString(16).toUpperCase() }.joinToString(separator = "")

fun intToString(k: Int): String {
    var n = k
    val buf = CharArray(8)
    val HEX_DIGITS = charArrayOf('0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F')
    for (i in 7 downTo 0) {
        buf[i] = HEX_DIGITS[n and 0x0F]
        n = n ushr 4;
    }
    return String(buf)
}
