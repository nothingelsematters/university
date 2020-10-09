package com.github.nothingelsematters.hashtags.hashtag

import com.github.nothingelsematters.hashtags.Configuration
import com.github.nothingelsematters.hashtags.http.UrlReader
import com.github.nothingelsematters.hashtags.response.HashtagResponseParser
import java.time.Duration

class HashtagRequestServiceImpl(
    private val urlReader: UrlReader,
    private val hashtagResponseParser: HashtagResponseParser
) : HashtagRequestService {

    override fun hashtagRecentActivity(hashtag: String, hourAmount: Int): List<Int> {
        require(hourAmount in 1..24)
        require(hashtag.isNotBlank())
        require(!hashtag.contains("[ \t\n#%$&]".toRegex()))

        return List(hourAmount) {
            hashtagResponseParser.parse(urlReader.readAsText(generateUrl(hashtag, hourAmount))).totalCount
        }
    }

    private fun generateUrl(hashtag: String, hourAmount: Int): String {
        val startTime = Duration.ofMillis(System.currentTimeMillis()) - Duration.ofHours(hourAmount - 1L)
        val endTime = startTime + Duration.ofHours(1L)

        return "https://api.vk.com/method/newsfeed.search" +
            "?count=0" +
            "&q=%23$hashtag" +
            "&start_time=${startTime.toSeconds()}" +
            "&end_time=${endTime.toSeconds()}" +
            "&access_token=${Configuration.accessToken}" +
            "&v=${Configuration.apiVersion}"
    }
}
