package com.github.nothingelsematters.hashtags.http

import mu.KotlinLogging
import java.io.IOException
import java.io.UncheckedIOException
import java.net.MalformedURLException
import java.net.URL


class UrlReaderImpl : UrlReader {

    private val logger = KotlinLogging.logger {}

    override fun readAsText(sourceUrl: String): String {
        val url = sourceUrl.toUrl()
        logger.info("Performing request on '$url'")

        return try {
            url.openStream()
                .bufferedReader()
                .use { it.readText() }
                .also { logger.info("Got response: '$it'") }
        } catch (e: IOException) {
            throw UncheckedIOException(e)
        }
    }

    private fun String.toUrl(): URL =
        try {
            URL(this)
        } catch (e: MalformedURLException) {
            throw RuntimeException("Malformed url: $this")
        }
}
