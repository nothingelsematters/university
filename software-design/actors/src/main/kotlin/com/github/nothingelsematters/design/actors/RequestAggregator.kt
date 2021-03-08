package com.github.nothingelsematters.design.actors

import akka.actor.AbstractLoggingActor
import akka.actor.Props
import akka.actor.ReceiveTimeout
import akka.japi.pf.ReceiveBuilder
import java.time.Duration

class RequestAggregator(
    childClasses: Map<String, String>,
    private val timeout: Duration = Duration.ofSeconds(1),
    private val callback: (List<Link>) -> Unit = ::println,
) : AbstractLoggingActor() {

    init {
        childClasses.forEach {
            context.actorOf(Props.create(RequestWorker::class.java, it.key, it.value), it.key)
        }
    }

    private val results = mutableListOf<Link>()

    private var resultCount = childClasses.size

    private fun propagateRequest(request: Request) {
        log().info("Starting request propagation & aggregation: $request")
        context.receiveTimeout = timeout
        context.children.forEach { it.tell(request, self) }
    }

    private fun collectResult(requestResult: RequestResult) {
        results.addAll(requestResult.links)
        if (--resultCount <= 0) {
            log().info("Stopped by result quantity satisfaction")
            returnResults()
        }
    }

    private fun returnResults() {
        log().info("Stopped by timeout exceeding")
        log().info("Results: $results")
        callback(results)
        context.children.forEach(context::stop)
        context.stop(self)
    }

    override fun createReceive() =
        ReceiveBuilder()
            .match(ReceiveTimeout::class.java) { returnResults() }
            .match(Request::class.java, ::propagateRequest)
            .match(RequestResult::class.java, ::collectResult)
            .build()
}
