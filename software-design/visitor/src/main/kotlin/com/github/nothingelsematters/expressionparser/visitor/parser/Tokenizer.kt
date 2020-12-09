package com.github.nothingelsematters.expressionparser.visitor.parser

import com.github.nothingelsematters.expressionparser.visitor.Token

class TokenizerException(message: String) : RuntimeException(message)

class Tokenizer(input: String) {

    private var state: State = Beginning(input, 0)

    fun tokenize(): List<Token> {
        val tokens = mutableListOf<Token>()
        while (state !is End) {
            state = state.next(tokens)
        }
        return tokens
    }
}
