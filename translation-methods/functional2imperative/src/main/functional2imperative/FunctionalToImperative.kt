package functional2imperative


fun FunctionalProgram.toImperative(): ImperativeProgram = ImperativeProgram (
    functions.map {
        when (it.ftype) {
            is AtomicType   -> Assigning(it.name, it.ftype.toImperativeType(), it.substCases.toWhen(listOf(it.name), 0))
            is FunctionType -> it.toImperativeFunction()
            else            -> throw IllegalArgumentException()
        }
    }
)

fun Function.toImperativeFunction(): ImperativeFunction {
    val argsAmount = if (HashSet<Int>(substCases.map { it.arguments.size }).size == 1) {
        substCases.first().arguments.size
    } else {
        throw DifferentArgumentAmountException(name)
    }

    val ift: ImperativeFunctionType = ftype.toImperativeType(argsAmount) as? ImperativeFunctionType
        ?: throw IllegalArgumentException()

    val names = List<String>(argsAmount) {
        substCases
            .find { sub -> sub.arguments[it] is Name }
            ?.let { found -> (found.arguments[it] as Name).value }
        ?: "arg$it" // TODO 0 arg may be "arg1"
    }

    return ImperativeFunction (
        this.name,
        names.zip(ift.args),
        ift.returnType,
        listOf(Return(substCases.toWhen(names)))
    )
}

// type cast

fun FunctionalType.toImperativeType(amount: Int = -1): ImperativeType = when (this) {
    is AtomicType -> if (amount > 1) throw IllegalArgumentException() else this
    is FunctionType -> {
        val args = ArrayList<ImperativeType>()
        var r = this
        var l: FunctionalType
        var left = amount
        while (r is FunctionType && left != 0) {
            l = r.from
            r = r.to
            args.add(l.toImperativeType())
            left--
        }
        ImperativeFunctionType(args, when(r) {
            is FunctionType -> r.toImperativeType()
            is AtomicType -> r
            else -> throw IllegalArgumentException()
        })
    }
    else -> throw IllegalArgumentException()
}

// subst cast

fun List<SubstitutionCase>.toWhen(names: List<String>, indent: Int = 1): When = When (
    map {
        val conditionString = it
            .arguments
            .mapIndexedNotNull { i, el ->
                (el as? Literal)?.let { Pair<String, Literal>(names[i], it) }
            }
            .map { (name, lit) -> "$name == $lit" }
            .joinToString(separator = " && ")

        Pair<Expression, Expression> (
            if (conditionString.isEmpty()) TrueExpression() else StringExpression(conditionString),
            it.body.toExpression(indent)
        )
    }
)

fun List<BooleanCase>.toWhen(indent: Int): When =
    When(map { Pair<Expression, Expression> (it.condition, it.expression) }, indent)

fun FunctionBody.toExpression(indent: Int): Expression = when (this) {
    is InsecureFunction -> expression
    is GuardedFunction  -> boolCases.toWhen(indent)
    else                -> throw IllegalArgumentException()
}
