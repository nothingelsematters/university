package com.github.nothingelsematters.expressionparser.visitor

interface TokenVisitor {

    fun visit(token: NumberToken)

    fun visit(token: Parenthesis)

    fun visit(token: Operation)
}
