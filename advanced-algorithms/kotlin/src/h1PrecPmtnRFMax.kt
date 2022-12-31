import java.io.File
import java.util.Scanner

private fun <T> MutableList<T>.addAll(vararg ts: T) = addAll(ts)

data class Job(
    val time: Long,
    var release: Long,
    val index: Int,
    val f: (Long) -> Long,
    val times: MutableList<Long> = mutableListOf(),
)

private data class Block(val start: Long, var time: Long = 0, val jobs: MutableList<Job> = mutableListOf()) {
    val end: Long
        get() = start + time

    fun add(job: Job) {
        jobs += job
        time += job.time
    }
}

private fun topologicalSort(jobs: List<Job>, edges: List<List<Int>>, reverseEdges: List<List<Int>>): List<Job> {
    fun depthFirstSearch(
        edges: List<List<Int>>,
        jobs: List<Job>,
        currentVertex: Int,
        result: MutableList<Job>,
        used: MutableSet<Int>,
    ) {
        if (currentVertex in used) return
        used += currentVertex

        edges[currentVertex]
            .asSequence()
            .filter { it !in used }
            .forEach { depthFirstSearch(edges, jobs, it, result, used) }

        result += jobs[currentVertex]
    }

    val result = mutableListOf<Job>()
    val used = mutableSetOf<Int>()

    jobs.indices
        .asSequence()
        .filter { it !in used && reverseEdges[it].isEmpty() }
        .forEach { depthFirstSearch(edges, jobs, it, result, used) }

    return result
}

private fun createBlocks(jobs: List<Job>): List<Block> = buildList {
    jobs.forEach { job ->
        val block = if (lastOrNull()?.let { it.end >= job.release } == true) {
            last()
        } else {
            Block(job.release).also { add(it) }
        }

        block.add(job)
    }
}

private fun decompose(edges: List<List<Int>>, block: Block): Long {
    val end = block.end

    val used = mutableSetOf<Int>()
    val minimumJobIndex = block.jobs
        .indices
        .reversed()
        .asSequence()
        .map { it to block.jobs[it] }
        .filter { (_, job) -> edges[job.index].none { it in used }.also { used += job.index } }
        .minBy { (_, job) -> job.f(end) }
        .first

    val deleted = block.jobs[minimumJobIndex]
    block.jobs.removeAt(minimumJobIndex)
    val newBlocks = createBlocks(block.jobs)

    return if (newBlocks.isEmpty()) {
        deleted.times.addAll(block.start, block.end)
        deleted.f(end)
    } else {
        if (block.start < newBlocks.first().start) {
            deleted.times.addAll(block.start, newBlocks.first().start)
        }

        newBlocks.asSequence()
            .windowed(2)
            .map { (left, right) -> left.end to right.start }
            .filter { (start, end) -> start < end }
            .forEach { deleted.times.addAll(it.toList()) }

        if (block.end > newBlocks.last().end) {
            deleted.times.addAll(newBlocks.last().end, block.end)
        }

        maxOf(deleted.f(end), newBlocks.maxOf { decompose(edges, it) })
    }
}

fun schedule1PrecPmtnRFMax(jobs: List<Job>, edges: List<List<Int>>, reverseEdges: List<List<Int>>): Long {
    val topologicalSorted = topologicalSort(jobs, edges, reverseEdges)

    topologicalSorted.asReversed().forEach { job ->
        edges[job.index]
            .asSequence()
            .map { jobs[it] }
            .forEach { it.release = maxOf(it.release, job.release + job.time) }
    }

    return createBlocks(topologicalSorted.sortedBy { it.release }).maxOf { decompose(edges, it) }
}

fun main() {
    val scanner = Scanner(File("p1precpmtnrifmax.in").bufferedReader())

    val n = scanner.nextInt()
    val times = List(n) { scanner.nextLong() }
    val releases = List(n) { scanner.nextLong() }
    val m = scanner.nextInt()

    val edges = List(n) { mutableListOf<Int>() }
    val reverseEdges = List(n) { mutableListOf<Int>() }

    repeat(m) {
        val (from, to) = List(2) { scanner.nextInt() - 1 }
        edges[from] += to
        reverseEdges[to] += from
    }

    val jobs = List(n) {
        val (a, b, c) = List(3) { scanner.nextLong() }
        Job(times[it], releases[it], it, { time -> a * time * time + b * time + c })
    }

    val fMax = schedule1PrecPmtnRFMax(jobs, edges, reverseEdges)

    File("p1precpmtnrifmax.out").printWriter().use { output ->
        output.println(fMax)

        jobs.forEach { job ->
            output.write("${job.times.size / 2} ")
            job.times.forEach { output.print("$it ") }
            output.println()
        }
    }
}
