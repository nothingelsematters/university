package com.github.nothingelsematters.expressionparser.visitor

import com.github.nothingelsematters.expressionparser.visitor.parser.TokenizerException

interface Token {
    fun accept(visitor: TokenVisitor)
}

class NumberToken(val value: Int) : Token {

    override fun accept(visitor: TokenVisitor) = visitor.visit(this)

    override fun toString() = "NUMBER($value)"
}

enum class OperationType {
    PLUS, MINUS, MULTIPLICATION, DIVISION
}

val charToOperationType = mapOf(
    '+' to OperationType.PLUS,
    '-' to OperationType.MINUS,
    '*' to OperationType.MULTIPLICATION,
    '/' to OperationType.DIVISION
)

class Operation(currentCharacter: Char) : Token {
    val type = charToOperationType[currentCharacter] ?: throw TokenizerException("Invalid char '$currentCharacter'")

    val priority = when(type) {
        OperationType.PLUS, OperationType.MINUS -> 1
        OperationType.MULTIPLICATION, OperationType.DIVISION -> 2
    }

    override fun accept(visitor: TokenVisitor) = visitor.visit(this)

    override fun toString() = type.toString()
}

enum class ParenthesisType {
    OPEN, CLOSE
}

val charToParenthesisType = mapOf('(' to ParenthesisType.OPEN, ')' to ParenthesisType.CLOSE)

class Parenthesis(currentCharacter: Char) : Token {

    val type = charToParenthesisType[currentCharacter] ?:
        throw TokenizerException("Invalid char '$currentCharacter', expected parenthesis")

    override fun accept(visitor: TokenVisitor) = visitor.visit(this)

    override fun toString() = type.toString()
}
