package com.github.nothingelsematters.hashtags.hashtag

import com.github.nothingelsematters.hashtags.http.UrlReader
import com.github.nothingelsematters.hashtags.http.UrlReaderImpl
import com.github.nothingelsematters.hashtags.response.HashtagResponseParser
import com.github.nothingelsematters.hashtags.response.HashtagResponseParserImpl
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertTrue

class HashtagRequestServiceIntegrationTest {

    private val urlReader: UrlReader = UrlReaderImpl()

    private val hashtagResponseParser: HashtagResponseParser = HashtagResponseParserImpl()

    private val hashtagRequestService: HashtagRequestService =
        HashtagRequestServiceImpl(urlReader, hashtagResponseParser)

    @Test
    fun `successful simple test`() {
        val hourAmount = 5
        val result = hashtagRequestService.hashtagRecentActivity("nhl", hourAmount)

        assertEquals(hourAmount, result.size)
        result.forEach { assertTrue(it in 0..10000) }
    }
}
