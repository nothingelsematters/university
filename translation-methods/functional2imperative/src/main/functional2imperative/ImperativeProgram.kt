package functional2imperative


data class ImperativeProgram(val instructions: List<Instruction>) {
    override fun toString(): String = instructions.joinToString(separator = "\n\n")
}

interface Instruction {}

data class ImperativeFunction(
    val name: String,
    val args: List<Pair<String, ImperativeType>>,
    val returnType: ImperativeType,
    val body: List<Instruction>
): Instruction {
    override fun toString(): String =
        "fun $name(${args.map { (name, atype) -> "$name: $atype" }.joinToString()}): $returnType " +
            if ((body.firstOrNull() as? Return)?.value is When) {
                "= ${(body[0] as Return).value}"
            } else {
                "{\n${body.joinToString(separator = "\n")}\n}"
            }

}

data class Assigning(val value: String, val ftype: ImperativeType, val expression: Expression, val readOnly: Boolean = true): Instruction {
    override fun toString(): String = "${if (readOnly) "val" else "var"} $value: $ftype = $expression"
}

data class When(var body: List<Pair<Expression, Expression>>, val level: Int = 0): Expression, Instruction {
    private val indent0 = indent(level)
    private val indent1 = indent(level + 1)

    override fun toString(): String =
        if (body.firstOrNull()?.first is TrueExpression ) {
            body.first().second.toString()
        } else {
            val strings = body.map { (cond, expr) -> "$cond -> $expr" }.toMutableList()
            strings.add("else -> throw SubstitutionFailedException()")
            "when {\n${strings.joinToString(prefix = indent1, separator = "\n" + indent1)}\n$indent0}"
        }
}

data class Return(val value: Expression): Instruction {
    override fun toString(): String = "return $value"
}
