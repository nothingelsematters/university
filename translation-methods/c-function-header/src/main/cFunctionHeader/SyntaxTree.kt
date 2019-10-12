package cFunctionHeader

data class SyntaxTree(val node: String, val children: List<SyntaxTree>) {
    constructor(node: String, vararg children: SyntaxTree) : this(node, children.asList())

    private fun toString(indent: String): String {
        val newIndent = indent.replace("├── ", "│   ").replace("└── ", "    ")

        return StringBuilder()
            .append("$indent$node\n")
            .append(children.foldIndexed(StringBuilder()) { i, sb, el ->
                sb.append(el.toString(newIndent + (if (i == children.lastIndex) "└── " else "├── ")))
            })
            .toString()
    }

    override fun toString(): String = toString("")
}
