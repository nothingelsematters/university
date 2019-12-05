package comp128

import org.junit.Test
import org.junit.runner.JUnitCore
import org.junit.Assert.assertArrayEquals
import org.junit.Assert.assertTrue


class RC4Test {
    @Test fun `a5 block test`() {
        val key = listOf(0x12, 0x23, 0x45, 0x67, 0x89, 0xAB, 0xCD, 0xEF).map(Int::toByte).toByteArray()
        val frame = 0x134
        val expected = listOf(0x53, 0x4E, 0xAA, 0x58, 0x2F, 0xE8, 0x15, 0x1A, 0xB6, 0xE1, 0x85, 0x5A, 0x72, 0x8C, 0x00,
            0x24, 0xFD, 0x35, 0xA3, 0x5D, 0x5F, 0xB6, 0x52, 0x6D, 0x32, 0xF9, 0x06, 0xDF, 0x1A, 0xC0).map(Int::toByte).toByteArray()

        val cluster = ShiftCluster(
            listOf(
                ShiftRegister(19, listOf(13, 16, 17, 18), 8),
                ShiftRegister(22, listOf(20, 21), 10),
                ShiftRegister(23, listOf(7, 20, 21, 22), 10)
            )
        )


        cluster.launch(key, frame)
        val result = cluster
            .getBlock()
            .toList()
            .chunked(BLOCK_SIZE / 2)
            .map {
                it
                    .chunked(Byte.SIZE_BITS) {
                        it.foldIndexed(0) { index, result, bit -> if (bit) result or (1 shl (Byte.SIZE_BITS - 1 - index)) else result }.toByte()
                    }
            }
            .fold(listOf<Byte>()) { was, el -> was + el }
            .toByteArray()

        println(result.toList())
        println(expected.toList())
        assertArrayEquals(expected, result)
    }

    @Test fun `a5 decode encoded`() {
        val key = listOf(0x12, 0x23, 0x45, 0x67, 0x89, 0xAB, 0xCD, 0xEF).map(Int::toByte).toByteArray()
        val expected = listOf(0x53, 0x4E, 0xAA, 0x58, 0x2F, 0xE8, 0x15, 0x1A, 0xB6, 0xE1, 0x85, 0x5A, 0x72, 0x8C, 0x00,
            0x24, 0xFD, 0x35, 0xA3, 0x5D, 0x5F, 0xB6, 0x52, 0x6D, 0x32, 0xF9, 0x06, 0xDF, 0x1A, 0xC0).map(Int::toByte).toByteArray()
        println(streamEncrypt(key, streamEncrypt(key, expected)).toList())
        println(expected.toList())
        assertArrayEquals(expected, streamEncrypt(key, streamEncrypt(key, expected)))
    }

    @Test fun `a3 - a8 test`() {
        val key = listOf(0x12, 0x23, 0x45, 0x67, 0x89, 0xAB, 0xCD, 0xEF).map(Int::toByte).toByteArray()
        val frame = 0x134
        val expected = listOf(0x53, 0x4E, 0xAA, 0x58, 0x2F, 0xE8, 0x15, 0x1A, 0xB6, 0xE1, 0x85, 0x5A, 0x72, 0x8C, 0x00, 0x0C).map(Int::toByte).toByteArray()
        a3a8(expected, expected)
    }

    @Test fun `a3 a8 test`() {
        val RANDHex = "FF23456789ABCDEFFFFF456789ABCDEF"
        val KiHEX = "FEDCBA987654FFFFFEDCBA987654FFFF"

        val RAND = RANDHex.hexToByte()
        val Ki = KiHEX.hexToByte()
        val RESPONSE = (a3a8(RAND, Ki)).byteToHex()

        print("RAND:     $RANDHex\nKi:       $KiHEX\nResponse: $RESPONSE\n  where ->\nsres:           ${RESPONSE.substring(0, 8)}\n")
        print("A5_session_key: ${RESPONSE.substring(8, 24)}\n")
    }
}
