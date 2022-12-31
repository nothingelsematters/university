import java.io.File
import java.util.Scanner
import kotlin.math.max
import kotlin.math.min

fun scheduleR2CMax(p1: List<Int>, p2: List<Int>): Int {
    val maxTime = p1.sum()
    val infinity = Int.MAX_VALUE / 4

    val last = p1.indices.fold(listOf(0) + List(maxTime) { infinity}) { prev, i ->
        List(maxTime + 1) { j ->
            min(prev[j] + p2[i], prev.getOrNull(j - p1[i]) ?: infinity)
        }
    }

    return (0..maxTime).minOf { max(it, last[it]) }
}

fun main() {
    val input = Scanner(File("r2cmax.in").bufferedReader())

    val n = input.nextInt()
    val p1 = List(n) { input.nextInt() }
    val p2 = List(n) { input.nextInt() }

    File("r2cmax.out").printWriter().use { it.println(scheduleR2CMax(p1, p2)) }
}
