package functional2imperative

class NoTypeDefinitionException(val error: String) : Exception(error)

class DifferentArgumentAmountException(val name: String) : Exception("different argument amount in \"$name\"")
