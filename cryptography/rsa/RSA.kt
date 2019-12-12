package rsa

import java.math.BigInteger
import java.math.BigInteger.ZERO
import java.math.BigInteger.ONE
import java.math.BigInteger.probablePrime
import java.util.Random
import java.util.concurrent.Executors
import java.util.concurrent.ThreadPoolExecutor
import java.util.concurrent.Future


const val PRIME_SIZE = 1024
const val ITERATIONS = 10
const val BATCH_SIZE = 8
const val CONFIDENCE_THRESHOLD = 3
const val numThreads = 8
private val rand = Random()
private val TWO = 2.toBigInteger()

private operator fun BigInteger.minus(i: Long): BigInteger = minus(BigInteger.valueOf(i))
private operator fun BigInteger.plus(i: Long): BigInteger = plus(BigInteger.valueOf(i))


public class RSAEncryptor() {
    private val secretExponent: BigInteger
    private val publicExponent: BigInteger
    private val n: BigInteger

    val publicKey
        get() = publicExponent to n

    val secretKey
        get() = secretExponent to n


    init {
        val p = generateLargePrime()
        val q = generateLargePrime()
        n = p * q

        val eiler = (p - ONE) * (q - ONE)
        var k = BigInteger(PRIME_SIZE, rand)

        while (gcd(k, eiler) != ONE) {
            k = BigInteger(PRIME_SIZE, rand)
        }

        publicExponent = k
        secretExponent = modInverse(k, eiler)
    }

    public fun generateLargePrime(): BigInteger {
        val threadPool = Executors.newFixedThreadPool(numThreads) as ThreadPoolExecutor
        threadPool.setRejectedExecutionHandler(ThreadPoolExecutor.DiscardPolicy())

        while (true) {
            val inputs = List<BigInteger>(BATCH_SIZE) { BigInteger(PRIME_SIZE / 2, rand) }
            var futures = List<Future<Boolean>>(BATCH_SIZE) { threadPool.submit<Boolean> { testPrime(inputs[it], threadPool) } }

            try {
                for (i in futures.indices) {
                    if (futures[i].get()) {
                        threadPool.shutdown()
                        threadPool.shutdownNow()
                        threadPool.getQueue().clear()

                        /* println("result ${threadPool.getTaskCount() - threadPool.getActiveCount()} ${inputs[i]}") */
                        return inputs[i]
                    }
                }
            } catch (e: Exception) {
                println("smth went wrong: ${e.message}")
            }

            /* threadPool.shutdownNow() */
            threadPool.getQueue().clear()
        }
    }

    private fun gcd(lhs: BigInteger, rhs: BigInteger): BigInteger = gcdHelper(lhs, rhs).first()

    private fun gcdHelper(lhs: BigInteger, rhs: BigInteger): List<BigInteger> {
        if (lhs == ZERO) return listOf(rhs, ZERO, ONE)
        val (result, left, right) = gcdHelper(rhs % lhs, lhs)
        return listOf(result, right - (rhs / lhs) * left, left)
    }

    private fun modMultiply(lhs: BigInteger, rhs: BigInteger, module: BigInteger): BigInteger {
        var result = ZERO
        var first = lhs
        var second = rhs

        while (second > ZERO) {
            if (second and ONE == ONE) {
                result = (result + first) % module
            }
            first = (first shl 1) % module
            second = second shr 1
        }
        return result
    }

    private fun modPow(number: BigInteger, exp: BigInteger, module: BigInteger): BigInteger {
        var result = ONE
        var power = exp
        var num = number

        while (power > ZERO) {
            if (power and ONE == ONE) {
                result = modMultiply(result, num, module)
            }
            num = modMultiply(num, num, module)
            power = power shr 1
        }
        return result
    }

    private fun modInverse(number: BigInteger, module: BigInteger): BigInteger =
        (gcdHelper(number, module)[1] % module + module) % module

    private fun testPrime(n: BigInteger, workers: ThreadPoolExecutor): Boolean {
        if (!n.testBit(0)) return false

        if (modPow(TWO, n - 1, n) != ONE) return false
        var successful = 0
        /* val threadPool = Executors.newFixedThreadPool(numThreads) */
        var futures = List<Future<Boolean>>(ITERATIONS) { workers.submit<Boolean> { millerRabinTest(n) } }

        try {
            for (result in futures) {
                if (result.get()) {
                    successful++
                    if (successful >= CONFIDENCE_THRESHOLD) {
                        workers.shutdownNow()
                        return true
                    }
                }
            }
        } catch (e: Exception) {
            println("smth went wrong: ${e.message}")
        }

        /* workers.shutdownNow() */
        return false
    }

    private fun millerRabinTest(n: BigInteger): Boolean {
        val nMinusOne = n - 1
        var randomN: BigInteger

        do {
            randomN = BigInteger(n.bitLength() - 1, rand)
        } while (randomN <= TWO)

        var x = modPow(randomN, nMinusOne, n)
        if (x != ONE) return false

        var dr = nMinusOne.divideAndRemainder(TWO)

        while (dr[1] == ZERO) {
            x = modPow(randomN, dr[0], n)
            if (x == (-1).toBigInteger() || x == nMinusOne) break
            if (x != ONE) return false
            dr = dr[0].divideAndRemainder(TWO)
        }
        return true
    }


    public fun encrypt(message: ByteArray): ByteArray = modPow(BigInteger(1, message), publicExponent, n).toByteArray()
    public fun decrypt(message: ByteArray): ByteArray = modPow(BigInteger(1, message), secretExponent, n).toByteArray()
}

/* public class StandardRSA() {
    private val secretExponent: BigInteger
    private val publicExponent: BigInteger
    private val n: BigInteger
    private val rand = Random()

    val publicKey
        get() = publicExponent to n

    val secretKey
        get() = secretExponent to n

    init {
        val p = generateLargePrime()
        val q = generateLargePrime()
        n = p * q

        val eiler = (p - ONE) * (q - ONE)
        var k = BigInteger(PRIME_SIZE, rand)

        while (k.gcd(eiler) != ONE) {
            k = BigInteger(PRIME_SIZE, rand)
        }

        publicExponent = k
        secretExponent = k.modInverse(eiler)
    }


    private fun generateLargePrime(): BigInteger = probablePrime(PRIME_SIZE, rand)

    public fun encrypt(message: ByteArray): ByteArray = BigInteger(1, message).modPow(publicExponent, n).toByteArray()
    public fun decrypt(message: ByteArray): ByteArray = BigInteger(1, message).modPow(secretExponent, n).toByteArray()

} */
