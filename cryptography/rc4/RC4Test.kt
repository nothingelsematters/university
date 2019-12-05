package rc4

import org.junit.Test
import org.junit.runner.JUnitCore
import org.junit.Assert.assertArrayEquals
import org.junit.Assert.assertTrue

import java.io.InputStream
import java.io.OutputStream
import java.io.FileInputStream
import java.io.FileOutputStream
import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream
import java.util.Random


class RC4Test {
    private val rnd = Random()

    fun byteArrayTest(key: ByteArray, input: ByteArray) {
        ByteArrayOutputStream(input.size).use { baos ->
            encrypt(key, ByteArrayInputStream(input), baos)

            ByteArrayOutputStream(input.size).use { baosTwo ->
                encrypt(key, ByteArrayInputStream(baos.toByteArray()), baosTwo)

                assertArrayEquals(input, baosTwo.toByteArray())
            }
        }
    }

    fun encoded(key: ByteArray, input: ByteArray): ByteArray {
        ByteArrayOutputStream().use { baos ->
            encrypt(key, ByteArrayInputStream(input), baos)
            return baos.toByteArray()
        }
    }

    fun encodedTest(key: ByteArray, input: String, output: ByteArray) =
        assertArrayEquals(output, encoded(key, input.toByteArray()))

    fun keyStringTest(key: String, input: String, output: ByteArray) =
        encodedTest(key.toByteArray(), input, output)

    fun randomTests(repetitions: Int, inputSize: Int, keySize: Int) {
        repeat(repetitions) {
            val input = ByteArray(inputSize)
            val key = ByteArray(keySize)
            rnd.nextBytes(key)
            rnd.nextBytes(input)

            byteArrayTest(key, input)
        }
    }

    @Test fun `test 1 easy tests`() {
        keyStringTest("Key", "Plaintext", listOf(
            0xBB, 0xF3, 0x16, 0xE8, 0xD9, 0x40, 0xAF, 0x0A, 0xD3
        ).map(Int::toByte).toByteArray())

        keyStringTest("Wiki", "pedia", listOf(
            0x10, 0x21, 0xBF, 0x04, 0x20
        ).map(Int::toByte).toByteArray())

        keyStringTest("Secret", "Attack at dawn", listOf(
            0x45, 0xA0, 0x1F, 0x64, 0x5F, 0xC3, 0x5B, 0x38, 0x35,
            0x52, 0x54, 0x4B, 0x9B, 0xF5
        ).map(Int::toByte).toByteArray())
    }



    @Test fun `test 2 random medium tests`() = randomTests(100, 1000, 128)
    @Test fun `test 3 random heavy tests`() = randomTests(1000, 1000000, 256)
}
