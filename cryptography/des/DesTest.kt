package des;

import org.junit.Test
import org.junit.runner.JUnitCore
import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue

import java.io.File

class DesTest {
    private fun List<Int>.toListByte(): List<Byte> = map { it.toByte() }

    private fun fromFile(fileName: String, length: Int): List<Byte> {
        val bytes = ByteArray(length)
        val stream = File(fileName).inputStream()
        stream.read(bytes)
        stream.close()
        return bytes.toList()
    }

    private fun checkEquals(first: List<Byte>, second: List<Byte>) {
        assertTrue(second.size - BLOCK_SIZE <= first.size)
        assertEquals(first, second.subList(0, first.size))
        assertTrue(second.subList(first.size, second.size).all { it == 0.toByte() })
    }

    private fun testEncoding(key: Long, message: List<Byte>, encrypted: List<Byte>) =
        assertEquals(encrypted, encrypt(key, message))

    private fun testDecodeEncoded(key: Long, message: List<Byte>) =
        checkEquals(message, decrypt(key, encrypt(key, message)))

    @Test fun `test 1 simple encrypt`() = testEncoding(
        0x123456789ABCDEF0L,
        listOf(0x01, 0x23, 0x45, 0x67, 0x89, 0xAB, 0xCD, 0xEF).toListByte(),
        listOf(0x85, 0xE8, 0x13, 0x54, 0x0F, 0x0A, 0xB4, 0x05).toListByte()
    )

    @Test fun `test 2 partial window encrypt`() = testEncoding(
        0x123456789ABCDEF0L,
        listOf(0x01, 0x23, 0x45, 0x67, 0x89, 0xAB, 0xCD, 0xEF, 0xAB).toListByte(),
        listOf(0x85, 0xE8, 0x13, 0x54, 0x0F, 0x0A, 0xB4, 0x05, 0x99, 0x48, 0x80, 0x93, 0x7d, 0xef, 0x35, 0x48).toListByte()
    )

    @Test fun `test 3 2000 bytes file encrypt`() = testEncoding(
        0x0EDCBA9876543210L,
        fromFile("tests/encryption.in", 2000),
        fromFile("tests/encryption.out", 2000)
    )

    @Test fun `test 4 simple encrypt-decrypt`() = testDecodeEncoded(
        0x123456789ABCDEF0L,
        listOf(0x01, 0x23, 0x45, 0x67, 0x89, 0xAB, 0xCD, 0xEF).toListByte()
    )

    @Test fun `test 5 partial window encrypt`() = testDecodeEncoded(
        0x123456789ABCDEF0L,
        listOf(0x01, 0x23, 0x45, 0x67, 0x89, 0xAB, 0xCD, 0xEF, 0xAB).toListByte()
    )

    @Test fun `test 6 2000 bytes file encrypt`() = testDecodeEncoded(
        0x0EDCBA9876543210L,
        fromFile("tests/encryption.in", 2000)
    )

    /* @Test fun `test 7 67Mb test`() = testDecodeEncoded(
        0x0EDCBA9876543210L,
        fromFile("tests/second.in", 65536)
    ) */

    @Test fun `test 8 2Mb encryption test`() = testEncoding(
        0x0EDCBA9876543210L,
        fromFile("tests/third.in", 2048000),
        fromFile("tests/third.out", 2048000)
    )
}
