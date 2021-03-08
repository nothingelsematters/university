package com.github.nothingelsematters.design.actors

import akka.actor.AbstractLoggingActor
import akka.japi.pf.ReceiveBuilder
import khttp.get
import khttp.responses.Response
import org.json.JSONException
import org.json.JSONObject
import java.net.URL

class RequestWorker(
    private val serviceName: String,
    private val requestTemplate: String,
) : AbstractLoggingActor() {

    override fun preStart() {
        super.preStart()
        log().info("Starting child actor")
    }

    private fun performRequest(request: Request) {
        log().info("Performing request: $request")

        val links = extractLinks(get(requestTemplate.format(request.request)))
        log().info("Result: $links")
        context.parent.tell(RequestResult(links), self)
    }

    /**
     * Dummy extract links method, should be modified in real life API requests.
     */
    private fun extractLinks(result: Response): List<Link> =
        result.jsonArray.map {
            if (it !is JSONObject) {
                throw JSONException("Invalid response structure")
            }

            Link(it.getString("header"), URL(it.getString("url")), serviceName)
        }

    override fun createReceive() =
        ReceiveBuilder()
            .match(Request::class.java, ::performRequest)
            .build()
}
