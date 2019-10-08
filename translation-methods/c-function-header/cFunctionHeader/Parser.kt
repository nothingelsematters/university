package cFunctionHeader

import java.io.InputStream
import java.text.ParseException

public class Parser {
    private lateinit var lex: LexicalAnalyzer

    public fun parse(ins: InputStream): SyntaxTree {
        lex = LexicalAnalyzer(ins)
        lex.nextToken()
        return function()
    }

    private fun epsilon(): SyntaxTree = SyntaxTree("epsilon")

    private fun checkToken(expected: Token) {
        if (lex.curToken != expected) {
            throw ParseException("${tokenMap[expected]!!} expected at ${lex.curPos}", lex.curPos)
        }
    }

    private fun unxpectedToken(unexpected: Token): SyntaxTree =
        throw ParseException("${tokenMap[unexpected]!!} is not expected at ${lex.curPos}", lex.curPos)

    private fun tokenFork(
        name: String,
        token: Token,
        ifPart: List<() -> SyntaxTree>,
        elsePart: List<() -> SyntaxTree>
    ): SyntaxTree = SyntaxTree(name,
        if (lex.curToken == token) {
            listOf(processToken(token)) + ifPart.map { it() }
        } else {
            elsePart.map { it() }
        }
    )

    private fun eitherwayTokenFork(name: String, token: Token, part: List<() -> SyntaxTree>): SyntaxTree =
        tokenFork(name, token, part, part)

    private fun processToken(token: Token): SyntaxTree {
        checkToken(token)
        lex.nextToken()
        return SyntaxTree(if (token == Token.NAME) "name: \'${lex.curWord}\'" else "\'${tokenMap[token]!!}\'")
    }

    private fun function(): SyntaxTree {
        val result = SyntaxTree("function", specifiers(), returnType(), nameAndArgList(), processToken(Token.SEMICOLON))
        checkToken(Token.END)
        return result
    }

    private fun specifiers(): SyntaxTree = SyntaxTree("specifiers",
        when (lex.curToken) {
            Token.STATIC, Token.INLINE -> SyntaxTree("specifiers", specifier(), specifiers())
            else -> epsilon()
        }
    )

    private fun specifier(): SyntaxTree = SyntaxTree("specifier",
        when (val t = lex.curToken) {
            Token.STATIC, Token.INLINE -> processToken(t)
            else -> unxpectedToken(t)
        }
    )

    private fun functionDeclaration(): SyntaxTree =
        SyntaxTree("function declaration", returnType(), name(), argsList())

    private fun returnType(): SyntaxTree =
        tokenFork("return type", Token.VOID, listOf(::epsilon), listOf(::argType))

    private fun argType(): SyntaxTree =
        eitherwayTokenFork("argument type", Token.CONST, listOf(::typeNameModifiers))

    private fun typeNameModifiers(): SyntaxTree = SyntaxTree("type name modifiers", typeName(), modifiers())

    private fun typeName(): SyntaxTree =
        tokenFork("type name", Token.STRUCT, listOf(::name), listOf(::systemTypes))

    private fun systemTypes(): SyntaxTree =
        SyntaxTree("system types", numericSpecifiers(), systemTypesPrime())

    private fun systemTypesPrime(): SyntaxTree = SyntaxTree("system type second",
        when (val t = lex.curToken) {
            Token.CHAR, Token.INT -> listOf(processToken(t))
            Token.SHORT -> listOf(processToken(t), int())
            Token.LONG -> listOf(processToken(t), long())
            else -> throw ParseException("${tokenMap[t]!!} is not expected at ${lex.curPos}", lex.curPos) // TODO fix copy paste
        }
    )

    private fun long(): SyntaxTree = eitherwayTokenFork("long", Token.LONG, listOf(::int))

    private fun numericSpecifiers(): SyntaxTree = SyntaxTree("numericSpecifiers",
        when (val t = lex.curToken) {
            Token.UNSIGNED, Token.SIGNED -> processToken(t)
            else -> epsilon()
        }
    )

    private fun name(): SyntaxTree = processToken(Token.NAME)

    private fun modifiers(): SyntaxTree = SyntaxTree("modifiers",
        when (val t = lex.curToken) {
            Token.POINTER, Token.CONST -> listOf(processToken(t), modifiers())
            else -> listOf(epsilon())
        }
    )

    private fun nameAndArgList(): SyntaxTree = SyntaxTree("name and arg list", name(), argsList())

    private fun nameAndArgs(): SyntaxTree = SyntaxTree("name and args",
        if (lex.curToken == Token.LEFTPARENTHESIS) {
            listOf(processToken(Token.LEFTPARENTHESIS), processToken(Token.POINTER), name(),
                processToken(Token.RIGHTPARENTHESIS), argsList())
        } else {
            listOf(name(), argsList())
        }
    )

    private fun int(): SyntaxTree = tokenFork("int", Token.INT, listOf(::int), listOf(::epsilon))

    private fun argument(): SyntaxTree =
        tokenFork("argument", Token.VOID, listOf(::functionArgumentNameAndArgsList), listOf(::argType, ::argRight))

    private fun argRight(): SyntaxTree = SyntaxTree("argument right part",
        if (lex.curToken == Token.LEFTPARENTHESIS) {
            functionArgumentNameAndArgsList()
        } else {
            argumentName()
        }
    )

    private fun functionArgumentNameAndArgsList(): SyntaxTree =
        SyntaxTree("function argument name and args list",
            processToken(Token.LEFTPARENTHESIS), processToken(Token.POINTER),
            name(), processToken(Token.RIGHTPARENTHESIS), argsList())

    private fun argumentName(): SyntaxTree =
        tokenFork("argument name", Token.NAME, emptyList<() -> SyntaxTree>(), listOf(::epsilon))

    private fun notEmptyArgs(): SyntaxTree = SyntaxTree("not empty arguments", argument(), restArgs())

    private fun restArgs(): SyntaxTree =
        tokenFork("rest arguments", Token.COMA, listOf(::notEmptyArgs), listOf(::epsilon))

    private fun arguments(): SyntaxTree = SyntaxTree("arguments",
        when (val t = lex.curToken) {
            Token.VOID -> processToken(t)
            Token.CONST, Token.STRUCT, Token.UNSIGNED, Token.SIGNED, Token.CHAR, Token.INT, Token.SHORT, Token.LONG ->
                notEmptyArgs()
            else -> epsilon()
        }
    )

    private fun argsList(): SyntaxTree = SyntaxTree("argument list",
        processToken(Token.LEFTPARENTHESIS), arguments(), processToken(Token.RIGHTPARENTHESIS))
}
