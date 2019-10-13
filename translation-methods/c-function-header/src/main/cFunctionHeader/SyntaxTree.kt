package cFunctionHeader

data class SyntaxTree(val name: String, val children: List<SyntaxTree>) {
    constructor(name: String, vararg children: SyntaxTree) : this(name, children.asList())

    private fun toString(indent: String): String {
        val newIndent = indent.replace("├── ", "│   ").replace("└── ", "    ")

        return StringBuilder()
            .append("$indent$name\n")
            .append(children.foldIndexed(StringBuilder()) { i, sb, el ->
                sb.append(el.toString(newIndent + (if (i == children.lastIndex) "└── " else "├── ")))
            })
            .toString()
    }

    override fun toString(): String = toString("")
}
