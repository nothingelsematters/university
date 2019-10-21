package serpent

import java.math.BigInteger
import java.security.InvalidKeyException
import java.io.File // TEST

// TODO make cipher interface

// Int utilities

infix fun Int.rotateLeft(bits: Int): Int = shl(bits) or ushr(Int.SIZE_BITS - bits)

fun Int.takeBits(begin: Int, amount: Int): Int = (this ushr (begin - amount)) and ((1 shl amount) - 1)

fun Int.toByteList(): List<Byte> = List<Byte>(Int.SIZE_BITS / Byte.SIZE_BITS) {
    takeBits(Int.SIZE_BITS - it * Byte.SIZE_BITS, Byte.SIZE_BITS).toByte()
}

// BigInteger utilities

fun BigInteger.takeBits(begin: Int, amount: Int): BigInteger = // lowest is zeroth
    (this shr (begin - amount)) and ((1.toBigInteger() shl amount) - 1.toBigInteger())

fun BigInteger.toInts(n: Int, step: Int = Int.SIZE_BITS): List<Int> = List<Int>(n) {
    takeBits((it + 1) * step, step).toInt()
}

// algorithm utilities

fun BigInteger.permutate(permutation: List<Int>, size: Int = BLOCK_BITS): BigInteger {
    /* println("before   ${this.to16()}") */
    return permutation.foldIndexed(0.toBigInteger()) { // TODO refactor
        index, result, element ->
            if (testBit(size - 1 - element)) result.setBit(size - 1 - index) else result
    }
    /* .also { println("permuted: ${it.to16()}") } */
}

fun BigInteger.substitute(sbox: List<Int>): BigInteger {
    var result = 0.toBigInteger()
    for (i in 0..31) {
        val z = sbox[(((this and (1.toBigInteger() shl (96 + i))) shr (96 + i)) or
            (((this and (1.toBigInteger() shl (64 + i))) shr (64 + i)) shl 1) or
            (((this and (1.toBigInteger() shl (32 + i))) shr (32 + i)) shl 2) or
            (((this and (1.toBigInteger() shl i)) shr i) shl 3)).toInt()]
        result = result or
            (((z and 1)).toBigInteger() shl (i + 0) or
            ((((z and (1 shl 1))) shr 1).toBigInteger() shl (32 + i)) or
            ((((z and (1 shl 2))) shr 2).toBigInteger() shl (64 + i)) or
            ((((z and (1 shl 3))) shr 3).toBigInteger() shl (96 + i)))
    }
    return result
}

fun BigInteger.substitution(sbox: List<Int>): BigInteger {
    var result = 0.toBigInteger()
    for (i in 0..31) {
        result = result or (sbox[takeBits(4 * (i + 1), 4).toInt()].toBigInteger() shl (i * 4))
    }
    return result
}

fun BigInteger.linearTransformation(transform: List<List<Int>>): BigInteger {
    var b: Int
    var result = 0.toBigInteger()
    for (i in 0..127) {
        b = 0
        for (j in LINEAR_TRANSFORMATION[i]) {
            b = b xor takeBits(j + 1, 1).toInt()
        }
        result = result or (b.toBigInteger() shl i)
    }
    return result
}
    /* transform.foldIndexed(0.toBigInteger()) {
    index, result, element -> result or (element.fold(0.toBigInteger()) {
        resultBit, xorBit -> resultBit xor takeBits(BLOCK_BITS - 1 - xorBit, 1)
    } shl (BLOCK_BITS - 1 - index))
} */


// algorithm main part

fun generateSubkeys(lb: List<Byte>): List<BigInteger> { // TODO refactor
    val key = BigInteger(1, lb.asReversed().toByteArray())
    if (key >= (1.toBigInteger() shl KEY_SIZE)) {
        throw InvalidKeyException()
    }

    val keys: MutableList<Int> = ArrayList<Int>(key.toInts(PARTS_KEY_SIZE) + List<Int>(132) { 0 }) // TODO refactor
    for (i in PARTS_KEY_SIZE until keys.size) { // TODO mb fix
        keys[i] = (keys[i - 8] xor keys[i - 5] xor keys[i - 3] xor keys[i - 1] xor PHI xor (i - PARTS_KEY_SIZE))
            .rotateLeft(11)
    }
    /* println("pre-keys:")
    keys.forEachIndexed { index, el -> println("pre-keys[$index] = ${intToString(el)}") } */

    return keys
        .subList(PARTS_KEY_SIZE, keys.size)
        .windowed(size = SUBKEY_INTS, step = SUBKEY_INTS)
        .mapIndexed { index, el ->
            BigInteger(1, el.flatMap(Int::toByteList).toByteArray())
                .substitute(SBOXES[((3 - index) % SBOXES.size + SBOXES.size) % SBOXES.size]) // TODO refactor
        }
        /* .also {
            println("keys:")
            it.forEachIndexed { index, el -> println("keys[$index] = ${el.to16()}") }
        } */
        .map { it.permutate(INITIAL_PERMUTATION) } // TODO
        /* .also {
            println("khat:")
            it.forEachIndexed { index, el -> println("khat[$index] = ${el.to16()}") }
        } */

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
            .foldIndexed(BigInteger(1, it.asReversed().toByteArray()).permutate(initialPermutation)) { index, element, key ->
                element
                    .xor(key)
                    /* .also { println("xored[$index] = ${it.to16()}") } */
                    .substitution(sboxes[index % sboxes.size])
                    /* .also { println("subst[$index] = ${it.to16()}") } */
                    .linearTransformation(linearTransform)
                    /* .also { println("trans[$index] = ${it.to16()}") } */
                    /* .also { println("=".repeat(70))} */
            }
            .xor(keys[keys.lastIndex - 1])
            /* .also { println("xored[31] = ${it.to16()}") } */
            .substitution(sboxes.last())
            /* .also { println("subst[31] = ${it.to16()}") } */
            .xor(keys.last())
            /* .also { println("xored[32] = ${it.to16()}") } */
            .permutate(finalPermutation)
            /* .also { println("permu ${it.to16()}")} */
            .toByteArray()
            .toList()
            .asReversed() // TODO mb
            .subList(0, BLOCK_SIZE)
    }

fun encrypt(key: List<Byte>, message: List<Byte>): List<Byte> =
    encrypt(message, generateSubkeys(key), INITIAL_PERMUTATION, SBOXES, LINEAR_TRANSFORMATION, FINAL_PERMUTATION)

fun decrypt(key: List<Byte>, message: List<Byte>): List<Byte> = // TODO mb last round is the first, oops
    encrypt(message, generateSubkeys(key).asReversed(),
        FINAL_PERMUTATION, INVERSED_SBOXES, INVERSED_LINEAR_TRANSFORMATION, INITIAL_PERMUTATION)

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
    val res = encrypt(BigInteger(1, listOf(0x01, 0x23, 0x45, 0x67, 0x89, 0xab, 0xcd, 0xef,
            0x01, 0x23, 0x45, 0x67, 0x89, 0xab, 0xcd, 0xef,
            0x01, 0x23, 0x45, 0x67, 0x89, 0xab, 0xcd, 0xef,
            0x01, 0x23, 0x45, 0x67, 0x89, 0xab, 0xcd, 0xef).map { it.toByte() }.asReversed().toByteArray()),
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

    val expected = "DE269FF833E432B85B2E88D2701CE75C"

    val key = listOf(0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0A, 0x0B, 0x0C, 0x0D, 0x0E, 0x0F,
            0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x18, 0x19, 0x1A, 0x1B, 0x1C, 0x1D, 0x1E, 0x1F). map(Int::toByte)

    /* println("was: ${test.to16()}")
    println("key: ${key.to16()}") */
    println(if (BigInteger(1, encrypt(key, test).toByteArray()).toString(16).toUpperCase() == expected) "SUCCESS" else "FAILURE")
    /* println("now: ${BigInteger(1, encrypt(key, test).toByteArray()).toString(16).toUpperCase()}")
    println("exp: $expected") */
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
