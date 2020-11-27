package com.github.nothingelsematters.hashtags

import com.natpryce.konfig.ConfigurationProperties
import com.natpryce.konfig.Key
import com.natpryce.konfig.stringType

object Configuration {

    private val configuration = ConfigurationProperties.fromResource("defaults.properties")

    val accessToken = configuration[Key("api.access-token", stringType)]

    val apiVersion = configuration[Key("api.version", stringType)]
}
