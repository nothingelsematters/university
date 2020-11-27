package com.github.nothingelsematters.hashtags.response

import com.beust.klaxon.JsonObject
import com.beust.klaxon.Klaxon
import mu.KotlinLogging

class HashtagResponseParserImpl : HashtagResponseParser {

    private val logger = KotlinLogging.logger {}

    private val parser = Klaxon()

    override fun parse(response: String): HashtagResponse {

        try {
            parser
                .parse<Map<String, JsonObject>>(response)
                ?.get("response")
                ?.let { parser.parseFromJsonObject<HashtagResponse>(it) }
                ?.let { return it }
        } catch (e: Throwable) {
        }

        logger.error("Invalid endpoint output: $response")
        throw RuntimeException("Invalid API response")
    }
}