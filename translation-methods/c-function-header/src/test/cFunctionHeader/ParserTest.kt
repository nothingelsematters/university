package cFunctionHeader

import java.text.ParseException
import java.io.FileOutputStream
import java.io.File
import java.util.Random
import java.util.concurrent.TimeUnit

import kotlin.math.abs

import org.junit.Test
import org.junit.Assume


class IncorrectTestException(val test: String) : Exception(test)


class ParserTest {
    private val generator = TreeGenerator()

    private fun parse(str: String): SyntaxTree = Parser().parse(str.byteInputStream())
    private fun stringTest(str: String) {
        parse(str)
    }

    @Test fun `simpliest test`() = stringTest("int f();")

    @Test(expected = LexicalException::class)
    fun `weird symbol test`() = stringTest("int ?? f();")

    @Test(expected = LexicalException::class)
    fun `wrong name format test`() = stringTest("int ?_f();")

    @Test(expected = LexicalException::class)
    fun `numeric beginning name test`() = stringTest("int 1_f();")

    @Test(expected = SyntaxException::class)
    fun `no semicolon test`() = stringTest("int f()")

    @Test(expected = SyntaxException::class)
    fun `no return test`() = stringTest("f();")

    @Test fun `megapointer test`() = stringTest("const signed short int* const **** pointers(struct name* (*f)(long****));")

    @Test fun `system types test`() = stringTest("int f(short, short int, unsigned short int, signed short, long, long long, double, float);")

    @Test fun `structure argument test`() = stringTest("void _f(struct NAME);")

    @Test fun `function specifiers test`() = stringTest("static inline const unsigned long long int foo();")

    @Test fun `middle difficulty test`() =
        stringTest("inline static void A_123(const unsigned long long int * const * b, struct n4ME_ * const * (*func)(signed short* arg));")

    @Test fun `full coverage random grammar test`() = repeat (1000) {
        val test = generator.getRandom()
        val file = createTempFile(suffix = ".c")
        with (FileOutputStream(file)) {
            write(test.toByteArray())
        }

        val path = file.absolutePath
        val outputPath = path + ".o"

        val process = Runtime.getRuntime().exec("gcc -o $outputPath -c $path")
        val exited = process.waitFor(6L, TimeUnit.SECONDS)
        val valid = if (exited) process.exitValue() == 0 else null
        if (exited && valid == false) throw IncorrectTestException(test)

        file.delete()
        File(outputPath).delete()


        try {
            stringTest(test)

        } catch (e: Exception) {
            println(test)

            if (!exited) {
                println("WARNING: gcc skipped checking, this test may fail with ${e.message}: $test")
            } else {
                throw e
            }
        }
    }
}

typealias StringFunc = () -> String
typealias StringFuncList = List<StringFunc>

class TreeGenerator {
    public fun getRandom(): String = function()
    private val r = Random()

    private var argumentsDepth = 0

    private fun epsilon(): String = ""

    private fun concatenate(vararg strings: String): String = concatenate(strings.asList())
    private fun concatenate(strings: List<String>): String = strings.joinToString(separator = " ")

    private fun tokenFork(token: Token, ifPart: StringFuncList, elsePart: StringFuncList): String =
        listOf(listOf({-> processToken(token)}) + ifPart, elsePart).random().map { it() }.joinToString(separator = " ")

    private fun eitherwayTokenFork(token: Token, part: StringFuncList): String = tokenFork(token, part, part)

    private fun processToken(token: Token): String = if (token == Token.NAME) name() else tokenMap[token]!!

    private fun function(): String = concatenate(specifiers(), returnType(), nameAndArgList(), ";")

    private fun specifiers(): String = listOf(tokenMap[Token.STATIC]!!, tokenMap[Token.INLINE]!!, epsilon()).random()

    private fun specifier(): String = listOf(Token.STATIC, Token.INLINE).map { tokenMap[it]!! }.random()

    private fun functionDeclaration(): String = concatenate(returnType(), name(), argsList())

    private fun returnType(): String = tokenFork(Token.VOID, listOf(::epsilon), listOf(::argType))

    private fun argType(): String = eitherwayTokenFork(Token.CONST, listOf(::typeNameModifiers))

    private fun typeNameModifiers(): String = concatenate(typeName(), modifiers())

    private fun typeName(): String = tokenFork(Token.STRUCT, listOf(::name), listOf(::systemTypes))

    private fun systemTypes(): String = listOf(
        concatenate(numericSpecifiers(), systemTypesPrime()),
        processToken(Token.DOUBLE),
        processToken(Token.FLOAT)
    ).random()

    private fun systemTypesPrime(): String =
        listOf(processToken(Token.CHAR), processToken(Token.INT),
            concatenate(processToken(Token.SHORT), int()),
            concatenate(processToken(Token.LONG), long())).random()

    private fun long(): String = "long"

    private fun numericSpecifiers(): String = listOf(processToken(Token.UNSIGNED), processToken(Token.SIGNED), epsilon()).random()

    private fun name(): String = "n4m3${abs(r.nextInt())}"

    private fun processPointer(): String = processToken(Token.POINTER)
    private fun processConst(): String = processToken(Token.CONST)

    private fun modifiers(): String =
        concatenate(listOf(listOf(::processPointer, ::modifiers), listOf(::processConst, ::modifiers), listOf(::epsilon)).random().map { it() })

    private fun nameAndArgList(): String = concatenate(name(), argsList())

    private fun nameAndArgs(): String =
        listOf(concatenate(processToken(Token.LEFTPARENTHESIS), processToken(Token.POINTER), name(),
            processToken(Token.RIGHTPARENTHESIS), argsList()), concatenate(name(), argsList())).random()

    private fun int(): String = "int"

    private fun argument(): String =
        tokenFork(Token.VOID, listOf(::functionArgumentNameAndArgsList), listOf(::argType, ::argRight))

    private fun argRight(): String = listOf(functionArgumentNameAndArgsList(), argumentName()).random()

    private fun functionArgumentNameAndArgsList(): String =
        concatenate(processToken(Token.LEFTPARENTHESIS), processToken(Token.POINTER),
            name(), processToken(Token.RIGHTPARENTHESIS), argsList())

    private fun argumentName(): String =
        tokenFork(Token.NAME, emptyList<StringFunc>(), listOf(::epsilon))

    private fun notEmptyArgs(): String = concatenate(argument(), restArgs())

    private fun restArgs(): String =
        tokenFork(Token.COMA, listOf(::notEmptyArgs), listOf(::epsilon))

    private fun processVoid(): String = processToken(Token.VOID)

    private fun arguments(): String {
        if (argumentsDepth > 20) return epsilon()
        argumentsDepth++
        val res = (listOf(::notEmptyArgs, ::epsilon).random())()
        argumentsDepth--
        return res
    }

    private fun argsList(): String =
        concatenate(processToken(Token.LEFTPARENTHESIS), arguments(), processToken(Token.RIGHTPARENTHESIS))
}
