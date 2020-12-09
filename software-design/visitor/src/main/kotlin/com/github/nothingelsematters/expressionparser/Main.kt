package com.github.nothingelsematters.expressionparser

import com.github.nothingelsematters.expressionparser.visitor.*
import com.github.nothingelsematters.expressionparser.visitor.parser.ParserVisitor
import com.github.nothingelsematters.expressionparser.visitor.parser.Tokenizer
import kotlin.system.exitProcess

fun printUsage(): Nothing {
    println("Usage: <expression>")
    exitProcess(-1)
}

fun TokenVisitor.acceptAll(tokens: List<Token>): TokenVisitor {
    tokens.forEach { it.accept(this) }
    return this
}

fun main() {
    val input = readLine() ?: printUsage()

    try {
        var tokens = Tokenizer(input).tokenize()
        println(tokens)

        val parserVisitor = ParserVisitor()
        parserVisitor.acceptAll(tokens)
        tokens = parserVisitor.finish()
        println(tokens)

        PrintVisitor().acceptAll(tokens)

        val calculationVisitor = CalculationVisitor()
        calculationVisitor.acceptAll(tokens)

        println("\n = ${calculationVisitor.finish()}")
    } catch (e: Throwable) {
        println("Caught exception: ${e.message} (${e::class})")
    }
}
