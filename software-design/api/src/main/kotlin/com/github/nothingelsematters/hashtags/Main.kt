package com.github.nothingelsematters.hashtags

import com.github.nothingelsematters.hashtags.hashtag.HashtagRequestService
import com.github.nothingelsematters.hashtags.hashtag.HashtagRequestServiceImpl
import com.github.nothingelsematters.hashtags.http.UrlReader
import com.github.nothingelsematters.hashtags.http.UrlReaderImpl
import com.github.nothingelsematters.hashtags.response.HashtagResponseParser
import com.github.nothingelsematters.hashtags.response.HashtagResponseParserImpl
import kotlin.system.exitProcess

fun printUsage(): Nothing {
    println("Usage: <hashtag> <hour amount>")
    exitProcess(1)
}

fun main(args: Array<String>) {

    if (args.size != 2) {
        printUsage()
    }

    val hashtag = args[0]
    val hourAmount = args[1].toIntOrNull() ?: printUsage()

    val urlReader: UrlReader = UrlReaderImpl()
    val hashtagResponseParser: HashtagResponseParser = HashtagResponseParserImpl()

    val hashtagRequestService: HashtagRequestService = HashtagRequestServiceImpl(urlReader, hashtagResponseParser)

    try {
        println(hashtagRequestService.hashtagRecentActivity(hashtag, hourAmount))
    } catch (e: Throwable) {
        println("ERROR: ${e.message}")
    }
}
