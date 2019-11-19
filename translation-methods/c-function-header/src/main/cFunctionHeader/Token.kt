package cFunctionHeader

public enum class Token {
    CHAR, SHORT, INT, LONG,
    FLOAT, DOUBLE,
    BOOL,
    SIGNED, UNSIGNED,
    AMPERSAND,
    INLINE, STATIC,
    VOID,
    CONST,
    STRUCT,
    POINTER,          // *
    COMA,             // ,
    LEFTPARENTHESIS,  // (
    RIGHTPARENTHESIS, // )
    SEMICOLON,        // ;
    END,              // $ (fake)
    NAME              // [\w_][\w\d_]*
}

val charactersMap = mapOf(
    '*' to Token.POINTER,
    ',' to Token.COMA,
    ';' to Token.SEMICOLON,
    '(' to Token.LEFTPARENTHESIS,
    ')' to Token.RIGHTPARENTHESIS,
    '&' to Token.AMPERSAND
)

val tokenMap = mapOf(
    Token.POINTER to "*",
    Token.COMA to ",",
    Token.SEMICOLON to ";",
    Token.LEFTPARENTHESIS to "(",
    Token.RIGHTPARENTHESIS to ")",
    Token.AMPERSAND to "&",
    Token.CHAR to "char",
    Token.SHORT to "short",
    Token.INT to "int",
    Token.LONG to "long",
    Token.SIGNED to "signed",
    Token.UNSIGNED to "unsigned",
    Token.INLINE to "inline",
    Token.STATIC to "static",
    Token.VOID to "void",
    Token.CONST to "const",
    Token.STRUCT to "struct",
    Token.END to "end of input",
    Token.NAME to "name",
    Token.FLOAT to "float",
    Token.DOUBLE to "double",
    Token.BOOL to "bool"
)

val wordsMap = mapOf(
    "float"    to Token.FLOAT,
    "double"   to Token.DOUBLE,
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
    "struct"   to Token.STRUCT,
    "bool"     to Token.BOOL
)
