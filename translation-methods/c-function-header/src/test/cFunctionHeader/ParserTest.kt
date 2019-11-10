package cFunctionHeader

import java.text.ParseException
import org.junit.Test
import org.junit.Assume

class ParserTest {
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
    fun `no return test`() = stringTest("f(void);")

    @Test fun `megapointer test`() = stringTest("const signed short int* const **** pointers(struct name* (*f)(long****));")

    @Test fun `system types test`() = stringTest("int f(short, short int, unsigned short int, signed short, long, long long, double, float);")

    @Test fun `structure argument test`() = stringTest("void _f(struct NAME);")

    @Test fun `function specifiers test`() = stringTest("static inline const unsigned long long int foo(void);")

    @Test fun `middle difficulty test`() =
        stringTest("inline static void A_123(const unsigned long long int * const * b, struct n4ME_ * const * (*func)(signed short* arg));")
}
