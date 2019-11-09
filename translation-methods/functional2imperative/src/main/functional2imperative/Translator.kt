package functional2imperative

import functional2imperative.parser.*

fun main() {
    val functional = functionalProgramfromStream(System.`in`)
    val imperative = functional.toImperative()
    println("was:\n$functional")
    println("-".repeat(30))
    println("now:\n$imperative")
}
