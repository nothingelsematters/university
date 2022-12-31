import java.io.File
import java.util.Scanner
import kotlin.math.min

fun main() {
    val input = Scanner(File("p1p1sumu.in").bufferedReader())

    val n = input.nextInt()
    var (d1, d2) = List(2) { input.nextInt() }
    val (a, b, c, d) = List(4) { input.nextLong() }

    val num = IntArray(n + 2)
    var time = 0

    when {
        d1 - 1 >= n -> time++
        d1 > 0 -> num[d1]++
    }

    when {
        d2 - 1 >= n -> time++
        d2 > 0 -> num[d2]++
    }

    repeat(n - 2) {
        val d3 = ((a * d1 + b * d2 + c) % d).toInt()
        d1 = d2
        d2 = d3

        if (d3 - 1 >= n) {
            time++
        } else {
            num[d3]++
        }
    }

    var currentNumber = 0
    repeat(n) {
        val current = min(it + 1 - currentNumber, num[it + 1])
        currentNumber += current
        time += current
    }

    File("p1p1sumu.out").printWriter().use { it.println(time) }
}
