package com.github.nothingelsematters.hashtags.response

interface HashtagResponseParser {

    fun parse(response: String): HashtagResponse
}
