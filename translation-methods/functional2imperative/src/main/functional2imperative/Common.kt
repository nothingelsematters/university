package functional2imperative

interface Expression {}

data class StringExpression(val str: String): Expression {
    override fun toString(): String = str
}

interface FunctionalType {}

interface ImperativeType {}

data class AtomicType(val name: String) : FunctionalType, ImperativeType {
    override fun toString(): String = name
}

data class FunctionType(val from: FunctionalType, val to: FunctionalType) : FunctionalType {
    override fun toString(): String = "($from -> $to)"
}

data class ImperativeFunctionType(val args: List<ImperativeType>, val returnType: ImperativeType): ImperativeType {
    override fun toString(): String = "(${args.joinToString()}) -> $returnType"
}
