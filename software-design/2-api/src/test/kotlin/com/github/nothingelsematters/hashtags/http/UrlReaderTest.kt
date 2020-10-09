package com.github.nothingelsematters.hashtags.http

import com.github.nothingelsematters.hashtags.http.HostReachableRule.HostReachable
import org.junit.ClassRule
import kotlin.test.Test
import kotlin.test.assertTrue

@HostReachable("google.com")
class UrlReaderTest {

    private val urlReader: UrlReader = UrlReaderImpl()

    @Test
    fun read() = assertTrue(urlReader.readAsText("http://google.com/").isNotEmpty())

    companion object {
        @ClassRule @JvmField
        val rule = HostReachableRule()
    }
}

