package functional2imperative


data class ImperativeProgram(val instructions: List<Instruction>) {
    override fun toString(): String = instructions.joinToString(separator = "\n")
}

interface Instruction {}

data class ImperativeFunction(
    val name: String,
    val args: List<Pair<String, ImperativeType>>,
    val returnType: ImperativeType,
    val body: List<Instruction>
): Instruction {
    override fun toString(): String = "fun $name(${args.joinToString()}): $returnType \n${body.joinToString(separator = "\n")}\n"
}

data class Assigning(val value: String, val expression: Expression, val readOnly: Boolean = true): Instruction {
    override fun toString(): String = "${if (readOnly) "val" else "var"} $value = $expression"
}


data class When(val case: String, val body: List<Pair<Expression, Expression>>): Expression, Instruction {
    override fun toString(): String = "when($case) {\n${body.map { (cond, expr) -> "$cond -> $expr" }.joinToString(separator = "\n")}\n}"
}

data class Return(val value: Expression): Instruction {
    override fun toString(): String = "return $value"
}

// conversion
/*
    functional:
        FunctionalProgram, Function, SubstitutionCase, Lines, Line, DeclaredType, Type, AtomicType, FunctionType, FunctionDefinition,
            FunctionBody, InsecureFunction, GuardedFunction, BooleanCase, Expression, Argument, Name, Literal
    imperative:
        ImperativeProgram, Type, AtomicType, FunctionType, Expression, Instruction, ImperativeFunction, Assigning, When, Return

    common:
        Type, AtomicType, FunctionType, Expression

    same:
        Expression, Type, AtomicType, FunctionType (??)
*/

/* fun FunctionalProgram.toImperative(): ImperativeProgram { // TODO lots of collisions
    ImperativeProgram (
        functions.map {
            when (it.type) {
                AtomicType   -> Assigning(it.name, toWhen(it.body))
                ImperativeFunctionType -> TODO()
            }
        }
    )
} */

/*
// fucking java collision: 
fun List<SubstitutionCase>.toWhen(): When = TODO()
fun List<BooleanCase>.toWhen(): When = TODO()
*/
