package lambda

interface Expression {}

data class Variable(val name: String) : Expression {
    override fun toString(): String = name
}

data class Application(val left: Expression, val right: Expression) : Expression {
    override fun toString(): String = "($left $right)"
}

data class Lambda(val variable: Variable, val expression: Expression) : Expression {
    override fun toString(): String = "(\\$variable.$expression)"
}
