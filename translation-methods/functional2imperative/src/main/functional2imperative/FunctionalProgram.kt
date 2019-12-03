package functional2imperative

data class FunctionalProgram(val functions: List<Function>) {
    override fun toString(): String = functions.joinToString(separator = "\n\n")
}

fun Lines.toProgram(): FunctionalProgram {
    val (declaredTypes, definitions) = lines.partition { it is DeclaredType }
    val declarations = declaredTypes
        .map {
            val dt = it as DeclaredType
            Pair(dt.name, dt.ftype)
        }
        .toMap()

    return FunctionalProgram (
        definitions
            .map { it as FunctionDefinition }
            .groupBy { it.name }
            .map { (name, cases) ->
                Function (
                    name,
                    declarations[name] ?: throw NoTypeDefinitionException(name),
                    cases.map { SubstitutionCase(it.arguments, it.body) }
                )
            }
    )
}

data class Function(val name: String, val ftype: FunctionalType, val substCases: List<SubstitutionCase>) {
    override fun toString(): String = "$name :: $ftype\n${substCases.joinToString(prefix = "$name ", separator = "\n$name ")}"
}

data class SubstitutionCase(val arguments: List<Argument>, val body: FunctionBody) {
    override fun toString(): String = "${arguments.joinToString(separator = " ")} $body"
}


// parsing classes

data class Lines(val lines: List<Line>) {
    override fun toString(): String = lines.joinToString(separator = "\n")
}

interface Line {}

// type

data class DeclaredType(val name: String, val ftype: FunctionalType) : Line {
    override fun toString(): String = "$name :: $ftype"
}

// function

class FunctionDefinition(val name: String, val arguments: List<Argument>, val body: FunctionBody) : Line {
    override fun toString(): String = "$name ${arguments.joinToString(separator = " ")} $body"
}

interface FunctionBody {}

class InsecureFunction(val expression: Expression) : FunctionBody {
    override fun toString(): String = "= $expression"
}

class GuardedFunction(val boolCases: List<BooleanCase>) : FunctionBody {
    override fun toString(): String = boolCases.joinToString(prefix = "\n    | ", separator = "\n    | ")
}


data class BooleanCase(val condition: Expression, val expression: Expression) {
    override fun toString(): String = "$condition = $expression"
}

interface Argument {}

data class Name(val value: String): Argument {
    override fun toString(): String = value
}

data class Literal(val value: String): Argument {
    override fun toString(): String = value
}
