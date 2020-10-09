package com.github.nothingelsematters.hashtags.hashtag

import com.github.nothingelsematters.hashtags.http.UrlReader
import com.github.nothingelsematters.hashtags.response.HashtagResponse
import com.github.nothingelsematters.hashtags.response.HashtagResponseParser
import io.mockk.MockKAnnotations
import io.mockk.every
import io.mockk.impl.annotations.MockK
import io.mockk.verify
import kotlin.test.BeforeTest
import kotlin.test.Test
import kotlin.test.assertEquals

class HashtagRequestServiceTest {

    @MockK
    private lateinit var urlReader: UrlReader

    @MockK
    private lateinit var hashtagResponseParser: HashtagResponseParser

    private lateinit var hashtagRequestService: HashtagRequestService

    @BeforeTest
    fun setUp() {
        MockKAnnotations.init(this)
        hashtagRequestService = HashtagRequestServiceImpl(urlReader, hashtagResponseParser)
    }

    @Test
    fun `success simple test`() {
        val hashtag = "hashtag"
        val hourAmount = 10
        val urlResponse = "url response"
        val resultNumber = 1000

        every { urlReader.readAsText(any()) } returns urlResponse
        every { hashtagResponseParser.parse(urlResponse) } returns HashtagResponse(emptyList<Any>(), 10, resultNumber)

        assertEquals(List(hourAmount) { resultNumber }, hashtagRequestService.hashtagRecentActivity(hashtag, hourAmount))

        verify(exactly = hourAmount) { urlReader.readAsText(any()) }
        verify(exactly = hourAmount) { hashtagResponseParser.parse(urlResponse) }
    }

    @Test(expected = IllegalArgumentException::class)
    fun `hour amount should be greater than 0 test`() {
        hashtagRequestService.hashtagRecentActivity("hashtag", 0)
    }

    @Test(expected = IllegalArgumentException::class)
    fun `hour amount should be less than 25 test`() {
        hashtagRequestService.hashtagRecentActivity("hashtag", 25)
    }

    @Test(expected = IllegalArgumentException::class)
    fun `hashtag should not be blank test`() {
        hashtagRequestService.hashtagRecentActivity(" ", 10)
    }

    @Test(expected = IllegalArgumentException::class)
    fun `hashtag should be valid test`() {
        hashtagRequestService.hashtagRecentActivity("#%))", 10)
    }
}