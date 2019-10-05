package cFunctionHeader

public enum class Token {
    CHAR, SHORT, INT, LONG,
    SIGNED, UNSIGNED,
    INLINE, STATIC,
    VOID,
    CONST,
    STRUCT,
    POINTER,          // *
    COMA,             // ,
    NAME,             // [\w_][\w\d_]*
    LEFTPARENTHESIS,  // (
    RIGHTPARENTHESIS, // )
    SEMICOLON,        // ;
    END               // $ (fake)
}

val charactersMap = mapOf(
    '*' to Token.POINTER,
    ',' to Token.COMA,
    ';' to Token.SEMICOLON,
    '(' to Token.LEFTPARENTHESIS,
    ')' to Token.RIGHTPARENTHESIS
)

val wordsMap = mapOf(
    "char"     to Token.CHAR,
    "short"    to Token.SHORT,
    "int"      to Token.INT,
    "long"     to Token.LONG,
    "signed"   to Token.SIGNED,
    "unsigned" to Token.UNSIGNED,
    "inline"   to Token.INLINE,
    "static"   to Token.STATIC,
    "void"     to Token.VOID,
    "const"    to Token.CONST,
    "struct"   to Token.STRUCT
)
