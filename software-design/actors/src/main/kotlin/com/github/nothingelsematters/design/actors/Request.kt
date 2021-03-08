package com.github.nothingelsematters.design.actors

import java.net.URL

data class Link(val header: String, val url: URL, val source: String)

sealed class RequestMessage

data class Request(val request: String) : RequestMessage()

data class RequestResult(val links: List<Link>) : RequestMessage()
