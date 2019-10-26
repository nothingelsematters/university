package serpent

import java.math.BigInteger
import java.security.InvalidKeyException

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

fun List<Byte>.extend(n: Int): List<Byte> = when (size.compareTo(n)) {
    1  -> subList(0, n)
    0  -> this
    -1 -> this + List<Byte>(n - size) { 0 }
    else -> throw RuntimeException()
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

fun encrypt(key: List<Byte>, message: List<Byte>): List<Byte> =
    message
        .windowed(size = BLOCK_SIZE, step = BLOCK_SIZE, partialWindows = true)
        .flatMap {
            val keys = generateSubkeys(key)
            keys
                .subList(0, keys.lastIndex - 1)
                .foldIndexed(it.toReversedBigInteger().permutate(INITIAL_PERMUTATION)) { index, element, key ->
                    element
                        .xor(key)
                        .substitute(SBOXES[index % SBOXES.size])
                        .linearTransformation(LINEAR_TRANSFORMATION)
                }
                .xor(keys[keys.lastIndex - 1])
                .substitute(SBOXES.last())
                .xor(keys.last())
                .permutate(FINAL_PERMUTATION)
                .toByteArray()
                .toList()
                .asReversed()
                .extend(BLOCK_SIZE)
        }

fun decrypt(key: List<Byte>, message: List<Byte>): List<Byte> =
    message
        .windowed(size = BLOCK_SIZE, step = BLOCK_SIZE, partialWindows = true)
        .flatMap {
            val keys = generateSubkeys(key)
            var result = it.asReversed().toBigInteger().permutate(INITIAL_PERMUTATION)
                .xor(keys.last())
                .substitute(INVERSED_SBOXES.last())
                .xor(keys[keys.lastIndex - 1])

            keys
                .subList(0, keys.lastIndex - 1)
                .foldRightIndexed(result) { index, element, res ->
                    res
                        .linearTransformation(INVERSED_LINEAR_TRANSFORMATION)
                        .substitute(INVERSED_SBOXES[index % INVERSED_SBOXES.size])
                        .xor(element)
                }
                .permutate(FINAL_PERMUTATION)
                .toByteArray()
                .toList()
                .asReversed()
                .extend(BLOCK_SIZE)
        }
