package com.github.nothingelsematters.hashtags.http

import com.xebialabs.restito.builder.stub.StubHttp.whenHttp
import com.xebialabs.restito.semantics.Action.status
import com.xebialabs.restito.semantics.Action.stringContent
import com.xebialabs.restito.semantics.Condition.method
import com.xebialabs.restito.semantics.Condition.startsWithUri
import com.xebialabs.restito.server.StubServer
import org.glassfish.grizzly.http.Method
import org.glassfish.grizzly.http.util.HttpStatus
import java.io.UncheckedIOException
import kotlin.test.Test
import kotlin.test.assertEquals


class UrlReaderWithStubServerTest {

    private val PORT = 32453

    private val urlReader: UrlReader = UrlReaderImpl()

    @Test
    fun `read as text test`() = withStubServer {
        val path = "ping"
        val response = "pong"

        whenHttp(it)
            .match(method(Method.GET), startsWithUri("/$path"))
            .then(stringContent(response))

        assertEquals(response, urlReader.readAsText("http://localhost:$PORT/$path"))
    }

    @Test(expected = UncheckedIOException::class)
    fun `read as text with not found error test`() = withStubServer {
        val path = "ping"

        whenHttp(it)
            .match(method(Method.GET), startsWithUri("/$path"))
            .then(status(HttpStatus.NOT_FOUND_404))

        urlReader.readAsText("http://localhost:$PORT/$path")
    }

    private fun withStubServer(callback: (StubServer) -> Unit) {
        var stubServer: StubServer? = null

        try {
            stubServer = StubServer(PORT).run()
            callback(stubServer)
        } finally {
            stubServer?.stop()
        }
    }

}