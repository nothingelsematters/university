package com.github.nothingelsematters.expressionparser.visitor.parser

import com.github.nothingelsematters.expressionparser.visitor.*

abstract class State(val inputString: String, var position: Int) {

    abstract fun next(tokens: MutableList<Token>): State

    open fun checkMeaningless(): State? = when {
        position >= inputString.length -> End(inputString, position)
        Character.isWhitespace(inputString[position]) -> Whitespace(inputString, position)
        else -> null
    }
}

class Beginning(input: String, position: Int) : State(input, position) {

    override fun next(tokens: MutableList<Token>): State {
        checkMeaningless()?.let { return it }

        val token = when (val currentCharacter = inputString[position]) {
            in '0'..'9' -> return Number(inputString, position)
            in charToParenthesisType.keys -> Parenthesis(currentCharacter)
            in charToOperationType.keys -> Operation(currentCharacter)
            else -> throw TokenizerException("Invalid char '$currentCharacter'")
        }
        tokens.add(token)

        position++
        return this
    }
}

class End(input: String, position: Int) : State(input, position) {
    override fun next(tokens: MutableList<Token>) = this
}

class Number(input: String, position: Int) : State(input, position) {

    private var value = 0

    override fun next(tokens: MutableList<Token>): State {
        val checked = checkMeaningless()
        when {
            checked != null -> {
                tokens.add(NumberToken(value))
                return checked
            }
            !Character.isDigit(inputString[position]) -> {
                tokens.add(NumberToken(value))
                return Beginning(inputString, position)
            }
            else -> {
                value = value * 10 + inputString[position].toString().toInt()
                val res = Number(inputString, position + 1)
                res.value = value
                return res
            }
        }
    }
}

class Whitespace(input: String, position: Int) : State(input, position) {

    override fun next(tokens: MutableList<Token>): State = when {
        position >= inputString.length -> End(inputString, position)
        Character.isWhitespace(inputString[position]) -> {
            position++
            this
        }
        else -> if (Character.isDigit(inputString[position])) Number(inputString, position) else Beginning(inputString, position)
    }
}
