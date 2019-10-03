package des;

class InvalidKeyException: Throwable("8th bits are not 0")

fun Long.permute(permutation: List<Int>, size: Int = Long.SIZE_BITS): Long = permutation.foldIndexed(0L) {
    index, result, element -> result or (takeBits(size - element, 1) shl (permutation.size - 1 - index))
}

fun List<Byte>.toLong(): Long = foldIndexed(0L) { index, result, element ->
    result or ((element.toLong() and 0xFFL) shl ((LONG_BYTES - index - 1) * Byte.SIZE_BITS))
}

fun Long.toByteList(): List<Byte> =
    ByteArray(LONG_BYTES).mapIndexed { index, _ -> takeBits((LONG_BYTES - index) * LONG_BYTES, LONG_BYTES).toByte() }

fun Long.takeBits(begin: Int, amount: Int): Long = (this ushr (begin - amount)) and ((1L shl amount) - 1L)

fun Long.twoParts(bits: Int): Pair<Long, Long> = Pair(takeBits(bits * 2, bits), takeBits(bits, bits))

fun List<Long>.mergeParts(bits: Int): Long = (component1() shl bits) or component2()
fun Pair<Long, Long>.mergeParts(bits: Int): Long = toList().mergeParts(bits)

fun generateKeys(key: Long): List<Long> {
    // validation
    for (i in 0 until Long.SIZE_BITS / BLOCK_SIZE) {
        if (key.takeBits(BLOCK_SIZE * i + 1, 1) != 0L) {
            throw InvalidKeyException()
        }
    }

    return KEY_SHIFT.map { shift ->
        key.permute(KEY_BEGIN_PERMUTATION)
            .twoParts(KEY_BLOCK_SIZE)
            .toList()
            .map { ((it shl shift) and KEY_MAX_BLOCK) or (it ushr (KEY_BLOCK_SIZE - shift)) }
            .mergeParts(KEY_BLOCK_SIZE)
            .permute(KEY_END_PERMUTATION, KEY_BEGIN_PERMUTATION.size)
    }
}

fun Long.sbox(sboxes: List<List<List<Int>>>): Long = sboxes.foldIndexed(0L) { index, result, box ->
    var part = takeBits(SBOX_SIZE * (sboxes.size - index), SBOX_SIZE)
    var row = Pair(part.takeBits(SBOX_SIZE, 1), part.takeBits(1, 1)).mergeParts(1)
    var column = part.takeBits(SBOX_SIZE - 1, SBOX_SIZE - 2)
    result or (box.get(row.toInt()).get(column.toInt()).toLong() shl (POST_SBOX_SIZE * (sboxes.size - index - 1)))
}

fun encryptWithKeys(keys: List<Long>, message: List<Byte>): List<Byte> =
    message
        .windowed(size = BLOCK_SIZE, step = BLOCK_SIZE, partialWindows = true)
        .map {  it.toLong().permute(INITIAL_PERMUTATION) }
        .map { block ->
            keys.fold(block) { was, key ->
                val (left, right) = was.twoParts(BLOCK_BITS)
                val sboxResult = (right.permute(EXTENSION, BLOCK_BITS) xor key).sbox(SBOXES)
                Pair(right, (left xor sboxResult.permute(POST_SBOX_PERMUTATION, BLOCK_BITS))).mergeParts(BLOCK_BITS)
            }
        }
        .map { it.twoParts(BLOCK_BITS).toList().asReversed().mergeParts(BLOCK_BITS).permute(FINAL_PERMUTATION) }
        .flatMap { it.toByteList() }

fun encrypt(key: Long, message: List<Byte>): List<Byte> = encryptWithKeys(generateKeys(key), message)
fun decrypt(key: Long, message: List<Byte>): List<Byte> = encryptWithKeys(generateKeys(key).asReversed(), message)
