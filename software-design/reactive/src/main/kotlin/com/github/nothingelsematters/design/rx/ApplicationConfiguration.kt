package com.github.nothingelsematters.design.rx

import com.github.nothingelsematters.design.rx.Currency.*
import com.mongodb.reactivestreams.client.MongoClient
import com.mongodb.reactivestreams.client.MongoClients
import com.natpryce.konfig.ConfigurationProperties
import com.natpryce.konfig.Key
import com.natpryce.konfig.stringType
import org.springframework.context.annotation.Bean
import org.springframework.data.mongodb.config.AbstractReactiveMongoConfiguration
import org.springframework.data.mongodb.repository.config.EnableReactiveMongoRepositories

@EnableReactiveMongoRepositories
object ApplicationConfiguration : AbstractReactiveMongoConfiguration() {

    private val configuration = ConfigurationProperties.fromResource("defaults.properties")

    // TODO this information is better to be fetched
    val conversions = mapOf(
        USD to mapOf(
            RUB to 74.6405,
            EUR to 0.8421,
        ),
        EUR to mapOf(
            RUB to 88.5572,
            USD to 1.1868,
        ),
        RUB to mapOf(
            USD to 0.0134,
            EUR to 0.0112,
        ),
    )

    @Bean fun mongoClient(): MongoClient = MongoClients.create(configuration[Key("mongo.url", stringType)])

    override fun getDatabaseName() = configuration[Key("mongo.database.name", stringType)]

    override fun reactiveMongoClient() = mongoClient()
}
