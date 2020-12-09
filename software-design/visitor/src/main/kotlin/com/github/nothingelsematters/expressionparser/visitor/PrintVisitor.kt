package com.github.nothingelsematters.expressionparser.visitor

class PrintVisitor : TokenVisitor {

    private fun printToken(token: Token) = print("$token ")

    override fun visit(token: NumberToken) = printToken(token)

    override fun visit(token: Parenthesis) = printToken(token)

    override fun visit(token: Operation) = printToken(token)
}
