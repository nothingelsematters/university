package cFunctionHeader

import java.io.InputStream
import java.io.IOException
import java.text.ParseException

public class LexicalAnalyzer(val ins: InputStream) {
    private var curChar = 0

    var curPos = 0
        private set

    lateinit var curToken: Token
        private set

    lateinit var curWord: String
        private set

    init {
        nextChar()
    }

    private fun Int.isBlank(): Boolean = when (toChar()) {
        ' ', '\t', '\r', '\n' -> true
        else -> false
    }

    private fun Int.isNameChar(): Boolean {
        val ch = toChar()
        return ch.isLetterOrDigit() || ch == '_'
    }

    private fun nextChar() {
        curPos++;
        try {
            curChar = ins.read();
        } catch (e: IOException) {
            throw ParseException(e.message, curPos)
        }
    }

    public fun nextToken() {
        while (curChar.isBlank()) {
            nextChar()
        }

        curToken = when (val ch = curChar.toChar()) {
            in 'a'..'z', in 'A'..'Z', '_' -> {
                val sb = StringBuilder()
                while (curChar.isNameChar()) {
                    sb.append(curChar.toChar())
                    nextChar()
                }

                curWord = sb.toString()
                wordsMap[curWord] ?: Token.NAME
            }

            in charactersMap -> {
                nextChar()
                charactersMap[ch]!!
            }

            (-1).toChar() -> Token.END
            else          -> throw ParseException("Illegal character '${curChar.toChar()}'", curPos)
        }
    }
}
