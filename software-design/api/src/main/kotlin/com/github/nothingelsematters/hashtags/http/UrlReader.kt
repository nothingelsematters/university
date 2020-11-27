package com.github.nothingelsematters.hashtags.http

interface UrlReader {

    fun readAsText(sourceUrl: String): String
}
