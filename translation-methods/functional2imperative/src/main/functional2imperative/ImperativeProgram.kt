package functional2imperative.imperative

import functional.*


data class ImperativeProgram(val instructions: List<Instruction>) {
    override fun toString(): String = instructions.joinToString(separator = "\n")
}

interface Type {}

data class FunctionType(val args: List<Type>, val returnType: Type): Type {
    override fun toString(): String = "(${args.joinToString()}) -> $returnType"
}

data class AtomicType(val value: String): Type {
    override fun toString(): String = value
}


data class Expression(val value: String) {
    override fun toString(): String = value
}


interface Instruction {}

data class Function(val name: String, val args: List<String, Type>, val returnType: Type, val body: List<Instruction>): Instruction {
    override fun toString(): String = "$fun $name(${args.joinToString()}): $returnType {\nbody.joinToString(separator = "\n")\n}"
}

data class Assigning(val value, val expression: Expression, val readOnly: Boolean = true): Instruction {
    override fun toString(): String = "${if (readOnly) "val" else "var"} $value = $expression"
}


data class When(val case: String, val body: List<Pair<Expression, Expression>>): Expression, Instruction {
    override fun toString(): String = "when($case) {\n${body.map { (cond, expr) -> "$cond -> $expr" }.joinToString(separator = "\n")}\n}"
}

data class Return(val value: Expression): Instruction {
    override fun toString(): String = "return $value"
}


fun FunctionalProgram.toImperative(): ImperativeProgram { // TODO lots of collisions
    ImperativeProgram (
        functions.map {
            when (it.type) {
                AtomicType   -> Assigning(it.name, toWhen(it.body))
                FunctionType -> TODO()
            }
        }
    )
}

fun toWhen(body: functional.Body): When = TODO()
