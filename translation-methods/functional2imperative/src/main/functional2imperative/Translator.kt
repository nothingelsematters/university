package functional2imperative

import functional2imperative.parser.*

import java.io.FileInputStream
import java.io.InputStream

fun main() {
    val parsed = functionalProgramfromStream(FileInputStream("input.txt"))
    println(parsed)
}
