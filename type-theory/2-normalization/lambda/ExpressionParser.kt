package lambda

import com.github.h0tk3y.betterParse.combinators.*
import com.github.h0tk3y.betterParse.grammar.*
import com.github.h0tk3y.betterParse.parser.Parser
import lambda.*


val expressionGrammar = object : Grammar<Expression>() {
    val variable by token("[a-z]([a-z0-9'])*")
    val lambda by token("\\\\")
    val dot by token("\\.")
    val openp by token("\\(")
    val closep by token("\\)")
    val ws by token("[\\s\\n\\t ]+", ignore = true)

    val variableParser: Parser<Variable> by variable use { Variable(text) }

    val atom: Parser<Expression> by
        (-openp * parser(this::rootParser) * -closep) or
        parser(this::variableParser)

    val application: Parser<Expression> by
        (oneOrMore(atom) map { it.reduce { l, r -> Application(l, r) } }) or
        parser(this::atom)

    val lambdaParser: Parser<Expression> by
        (-lambda * parser(this::variableParser) * -dot * parser(this::rootParser)) map { (v, e) -> Lambda(v, e) }

    override val rootParser: Parser<Expression> by
        ((parser(this::application) * parser(this::lambdaParser)) map { (l, r) -> Application(l, r) }) or
        parser(this::application) or
        parser(this::lambdaParser)
}

fun Expression(str: String) = expressionGrammar.parseToEnd(str)
