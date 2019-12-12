package rsa

import org.junit.Test
import org.junit.Assert.assertArrayEquals
import java.util.Random
import java.math.BigInteger


class RSATest {
    val rand = Random()

    private fun testEncodeDecoded(message: ByteArray) {
        val cipher = RSAEncryptor()
        val encrypted = cipher.encrypt(message)
        val decrypted = cipher.decrypt(encrypted)

        println("${ byteArrayString(message) } --> ${ byteArrayString(encrypted) } --> ${ byteArrayString(decrypted) }")
        assertArrayEquals(message, decrypted)
    }

    private fun byteArrayString(ar: ByteArray) = BigInteger(1, ar).toString(16)

    @Test fun `test 1 easy test`() = testEncodeDecoded(BigInteger.valueOf(123456789L).toByteArray())

    @Test fun `test 2 random big tests`() = repeat(5) {
        testEncodeDecoded(BigInteger(256, rand).toByteArray())
    }
}
