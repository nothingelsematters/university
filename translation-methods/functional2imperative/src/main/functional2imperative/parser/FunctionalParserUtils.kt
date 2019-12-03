package functional2imperative.parser

import functional2imperative.*
import org.antlr.v4.runtime.CharStreams
import org.antlr.v4.runtime.CommonTokenStream
import java.io.InputStream


fun functionalProgramfromString(input: String): FunctionalProgram = functionalProgramfromStream(input.byteInputStream())

fun functionalProgramfromStream(input: InputStream): FunctionalProgram =
    FunctionalParser(CommonTokenStream(FunctionalLexer(CharStreams.fromStream(input)))).program().fns.toProgram()
