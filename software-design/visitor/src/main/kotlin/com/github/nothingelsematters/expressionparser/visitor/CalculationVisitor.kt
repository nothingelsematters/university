package com.github.nothingelsematters.expressionparser.visitor

class CalculationException(message: String) : RuntimeException(message)

class CalculationVisitor : TokenVisitor {

    private val stack = ArrayDeque<Double>()

    override fun visit(token: NumberToken) = stack.addFirst(token.value.toDouble())

    override fun visit(token: Parenthesis) = throw CalculationException("Invalid token PARENTHESIS")

    override fun visit(token: Operation) {
        if (stack.size < 2) {
            throw CalculationException("Invalid expression")
        }
        val y = stack.removeFirst()
        val x = stack.removeFirst()

        val operation: (Double, Double) -> Double = when (token.type) {
            OperationType.PLUS -> Double::plus
            OperationType.MULTIPLICATION -> Double::times
            OperationType.MINUS -> Double::minus
            OperationType.DIVISION -> Double::div
        }

        stack.addFirst(operation(x, y))
    }

    fun finish(): Double = stack.removeFirstOrNull() ?: throw CalculationException("Invalid expression")
}
