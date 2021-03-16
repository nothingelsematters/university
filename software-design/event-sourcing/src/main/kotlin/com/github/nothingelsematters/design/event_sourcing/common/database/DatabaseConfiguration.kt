package com.github.nothingelsematters.design.event_sourcing.common.database

import org.jetbrains.exposed.sql.Database
import org.jetbrains.exposed.sql.SchemaUtils
import org.jetbrains.exposed.sql.StdOutSqlLogger
import org.jetbrains.exposed.sql.addLogger
import org.jetbrains.exposed.sql.transactions.transaction
import org.springframework.boot.context.properties.ConfigurationProperties
import org.springframework.context.annotation.Configuration
import javax.annotation.PostConstruct

@Configuration
@ConfigurationProperties(prefix = "database")
open class DatabaseConfiguration {

    var url: String? = null

    var username: String? = null

    var password: String? = null

    @PostConstruct
    fun init() {
        Database.connect(
            url ?: throw RuntimeException("Database url is not configured"),
            user = username ?: throw RuntimeException("Database username is not configured"),
            password = password ?: "",
        )

        transaction {
            addLogger(StdOutSqlLogger)
            SchemaUtils.create(Clients, SubscriptionExtensions, Turnstile)
        }
    }
}
