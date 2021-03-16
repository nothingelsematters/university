package com.github.nothingelsematters.design.event_sourcing.administrator

import com.github.nothingelsematters.design.event_sourcing.common.database.Clients
import com.github.nothingelsematters.design.event_sourcing.common.database.SubscriptionExtensions
import com.github.nothingelsematters.design.event_sourcing.common.getSubscriptionEndTime
import org.jetbrains.exposed.sql.insert
import org.jetbrains.exposed.sql.insertAndGetId
import org.jetbrains.exposed.sql.select
import org.jetbrains.exposed.sql.transactions.transaction

interface AdministratorService {

    fun registerClient(client: Client): Int

    fun getSubscription(clientId: Int): Subscription?

    fun renewSubscription(clientId: Int, monthDuration: Int)
}


class AdministratorServiceImpl : AdministratorService {

    override fun registerClient(client: Client) = transaction {
        Clients
            .insertAndGetId {
                it[name] = client.name
                it[phone] = client.phone
                it[passport] = client.passport
            }
            .value
    }

    override fun getSubscription(clientId: Int): Subscription? = transaction {
        val client = getClient(clientId) ?: return@transaction null
        val endTime = getSubscriptionEndTime(clientId)
        Subscription(client[Clients.name], endTime)
    }

    override fun renewSubscription(clientId: Int, monthDuration: Int): Unit = transaction {
        val currentClient = getClient(clientId) ?: return@transaction

        SubscriptionExtensions.insert {
            it[client] = currentClient[Clients.id]
            it[this.monthDuration] = monthDuration
        }
    }

    private fun getClient(clientId: Int) = Clients.select { Clients.id eq clientId }.singleOrNull()
}
