package functional2imperative

import functional2imperative.parser.*
import org.antlr.v4.runtime.CharStreams
import org.antlr.v4.runtime.CommonTokenStream
import org.antlr.v4.runtime.TokenStream

import java.io.FileInputStream
import java.io.InputStream
import java.io.IOException

fun main() {
    val parsed = fromStream(FileInputStream("input.txt"))
    println(parsed)
}

// TODO replace in 'utils':

fun fromString(input: String): FunctionalProgram = fromStream(input.byteInputStream())

fun fromStream(input: InputStream): FunctionalProgram =
    FunctionalParser(CommonTokenStream(FunctionalLexer(CharStreams.fromStream(input)))).program().fns.toProgram()
