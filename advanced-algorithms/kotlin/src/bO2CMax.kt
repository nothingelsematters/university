import java.io.File
import java.util.Scanner

fun scheduleO2CMax(p1: List<Long>, p2: List<Long>): Pair<Long, List<List<Long>>> {
    fun fill(schedule: MutableList<Long>, times: List<Long>, indices: Sequence<Int>, init: Long = 0) =
        indices.fold(init) { sum, index ->
            schedule[index] = sum
            sum + times[index]
        }

    fun i() = p1.asSequence().withIndex().filter { (index, i) -> i <= p2[index] }.map { it.index }
    fun j() = p1.asSequence().withIndex().filter { (index, i) -> i > p2[index] }.map { it.index }

    val x = i().maxByOrNull { p1[it] }
    val y = j().maxByOrNull { p2[it] }

    return if (x == null || y != null && p1[x] < p2[y]) {
        val (cMax, schedule) = scheduleO2CMax(p2, p1)
        cMax to listOf(schedule[1], schedule[0])
    } else {
        val cMax = maxOf(p1.sum(), p2.sum(), p1.asSequence().zip(p2.asSequence()).maxOf { (a, b) -> a + b })

        val first = MutableList(p1.size) { 0L }
        fill(first, p1, i() - x)
        fill(first, p1, sequenceOf(x), cMax - p1[x])
        fill(first, p1, j(), cMax - p1[x] - j().sumOf { p1[it] })

        val second = MutableList(p2.size) { 0L }
        fill(second, p2, sequenceOf(x))
        fill(second, p2, i() - x, p2[x])
        fill(second, p2, j(), cMax - j().sumOf { p2[it] })

        cMax to listOf(first, second)
    }
}

fun main() {
    val input = Scanner(File("o2cmax.in").bufferedReader())

    val n = input.nextInt()
    val p1 = List(n) { input.nextLong() }
    val p2 = List(n) { input.nextLong() }

    val (c, schedule) = scheduleO2CMax(p1, p2)

    File("o2cmax.out").printWriter().use { output ->
        output.println(c)
        schedule.forEach { line ->
            line.forEach { output.print("$it ") }
            output.println()
        }
    }
}
