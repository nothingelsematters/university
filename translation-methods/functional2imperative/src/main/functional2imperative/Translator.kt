package functional2imperative

import functional2imperative.parser.*

fun main() {
    val functional = functionalProgramfromStream(System.`in`)
    val imperative: ImperativeProgram

    try {
        imperative = functional.toImperative()
    } catch (e: Exception) {
        println("FAILED with: ${e.message}")
        return
    }
    
    println(imperative)
}
