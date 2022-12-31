import java.io.File
import java.util.Scanner

fun schedulePIntreeP1LMax(
    deadlines: MutableList<Int>,
    m: Int,
    fromTo: Map<Int, Int>,
    toFrom: Map<Int, List<Int>>,
): Pair<Int, List<Int>> {
    val i = deadlines.indices.find { it !in fromTo }!!

    val deque = ArrayDeque<Int>()
    deque += i

    while (deque.isNotEmpty()) {
        val j = deque.removeFirst()
        toFrom[j].orEmpty().forEach { k ->
            deadlines[k] = minOf(deadlines[k], deadlines[j] - 1)
            deque.addLast(k)
        }
    }

    var f = 0
    val r = MutableList(deadlines.size) { 0 }
    val q = MutableList(deadlines.size) { 0 }
    val x = MutableList(deadlines.size) { 0 }

    deadlines.asSequence().withIndex().sortedBy { it.value }.map { it.index }.forEach { i ->
        val t = maxOf(r[i], f)
        x[i] = t
        q[t] += 1

        if (q[t] == m) {
            f = t + 1
        }

        fromTo[i]?.let { j -> r[j] = maxOf(r[j], t + 1) }
    }

    val l = deadlines.asSequence().zip(x.asSequence()).maxOf { (d, c) -> c + 1 - d }
    return l to x
}

fun main() {
    val input = Scanner(File("pintreep1l.in").bufferedReader())

    val n = input.nextInt()
    val m = input.nextInt()
    val deadlines = MutableList(n) { input.nextInt() }
    val fromTo = mutableMapOf<Int, Int>()
    val toFrom = mutableMapOf<Int, MutableList<Int>>()
    repeat(n - 1) {
        val from = input.nextInt() - 1
        val to = input.nextInt() - 1
        fromTo[from] = to
        toFrom.getOrPut(to) { mutableListOf() }.add(from)
    }

    val (lMax, schedule) = schedulePIntreeP1LMax(deadlines, m, fromTo, toFrom)

    File("pintreep1l.out").printWriter().use { output ->
        output.println(lMax)
        schedule.forEach { output.print("$it ") }
    }
}
