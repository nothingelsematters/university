package com.github.nothingelsematters.hashtags.response

import kotlin.test.Test
import kotlin.test.assertEquals

class HashtagResponseParserTest {

    private val hashtagResponseParser: HashtagResponseParser = HashtagResponseParserImpl()

    @Test
    fun `successful parse simple test`() {
        val count = 10
        val totalCount = 1000

        val parsed = hashtagResponseParser.parse(
            """
            {
                "response": {
                    "items": [],
                    "count": $count,
                    "total_count": $totalCount
                }
            }
            """.trimIndent()
        )

        assertEquals(count, parsed.count)
        assertEquals(totalCount, parsed.totalCount)
    }

    @Test(expected = RuntimeException::class)
    fun `unexpected format should fail test`() {
        hashtagResponseParser.parse(""" { "response":  "ERROR" } """)
    }

    @Test(expected = RuntimeException::class)
    fun `receiving wrong types should fail test`() {
        hashtagResponseParser.parse(
            """
                {
                    "response": {
                        "items": [],
                        "count": "ERROR",
                        "total_count": {}
                    }
                }
                """.trimIndent()
        )
    }
}