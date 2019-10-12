package cFunctionHeader

import java.text.ParseException
import org.junit.Test
import org.junit.Assume

class ParserTest {
    private fun stringTest(str: String): SyntaxTree = Parser().parse(str.byteInputStream())
    private fun parse(str: String) {
        stringTest(str)
    }

    @Test fun `simpliest test`() = parse("int f();")

    @Test(expected = LexicalException::class)
    fun `weird symbol test`() = parse("int ?? f();")

    @Test(expected = LexicalException::class)
    fun `wrong name format test`() = parse("int 1_f();")

    @Test(expected = SyntaxException::class)
    fun `no semicolon test`() = parse("int f()")

    @Test fun `middle difficulty test`() =
        parse("inline static void A_123(const unsigned long long int * const * b, struct n4ME_ * const * (*func)(signed short*));")
}
