import java.io.File
import java.math.BigInteger
import java.util.Scanner

private fun scheduleP1PrecFMax(
    times: List<Int>,
    functions: List<(Int) -> BigInteger>,
    prec: Map<Int, List<Int>>,
): Pair<BigInteger, List<Int>> {
    val n = times.size
    val children = MutableList(n) { 0 }
    prec.forEach { (_, row) -> row.forEach { i -> children[i] += 1 } }

    val excluded = (0 until n).toMutableSet()
    var p = times.sum()
    val reverseSchedule = MutableList(n) { 0 }

    for (k in n - 1 downTo 0) {
        val j = excluded.asSequence().filter { children[it] == 0 }.minBy { functions[it](p) }
        excluded.remove(j)
        reverseSchedule[k] = j
        p -= times[j]

        prec[j]?.forEach { children[it] -= 1 }
    }

    val schedule = MutableList(n) { 0 }
    reverseSchedule.fold(0) { sum, it ->
        schedule[it] = sum
        sum + times[it]
    }

    val fMaxMin = (0 until n)
        .maxOfOrNull { functions[it](schedule[it] + times[it]) } ?: 0.toBigInteger()

    return fMaxMin to schedule
}

fun main() {
    val inputFile = Scanner(File("p1precfmax.in").bufferedReader())
    val n = inputFile.nextInt()
    val times = List(n) { inputFile.nextInt() }

    val functions = List(n) {
        val m = inputFile.nextInt()
        val coefficients = List(m + 1) { inputFile.nextInt() };
        { x: Int ->
            coefficients.asSequence()
                .withIndex()
                .sumOf { (i, it) -> x.toBigInteger().pow(coefficients.size - 1 - i) * it.toBigInteger() }
        }
    }

    val d = inputFile.nextInt()
    val prec = buildMap {
        for (i in 0 until d) {
            val from = inputFile.nextInt() - 1
            val to = inputFile.nextInt() - 1

            val list: MutableList<Int> = getOrPut(to) { mutableListOf() }
            list += from
        }
    }

    val (fMaxMin, schedule) = scheduleP1PrecFMax(times, functions, prec)

    File("p1precfmax.out").printWriter().use { output ->
        output.println(fMaxMin.toString())

        for (i in schedule) {
            output.print("$i ")
        }
    }
}
