package com.github.nothingelsematters.design.event_sourcing.turnstile

import com.github.nothingelsematters.design.event_sourcing.common.database.Turnstile
import com.github.nothingelsematters.design.event_sourcing.common.database.Turnstile.Direction.*
import com.github.nothingelsematters.design.event_sourcing.common.getSubscriptionEndTime
import org.jetbrains.exposed.sql.insert
import org.jetbrains.exposed.sql.max
import org.jetbrains.exposed.sql.select
import org.jetbrains.exposed.sql.transactions.transaction
import org.joda.time.DateTime

interface TurnstileService {

    fun letIn(clientId: Int)

    fun letOut(clientId: Int)
}

class TurnstileServiceImpl : TurnstileService {

    override fun letIn(clientId: Int): Unit = transaction {
        val endTime = getSubscriptionEndTime(clientId)
        if (DateTime.now() > endTime) {
            throw SubscriptionDurationExceededException(endTime)
        }

        if (getLastDirection(clientId) == IN) {
            throw RougeDetectedException()
        }

        Turnstile.insert {
            it[client] = clientId
            it[direction] = IN
        }
    }

    override fun letOut(clientId: Int): Unit = transaction {
        if (getLastDirection(clientId) == OUT) {
            throw RougeDetectedException()
        }

        Turnstile.insert {
            it[client] = clientId
            it[direction] = OUT
        }
    }

    private fun getLastDirection(clientId: Int) = Turnstile
        .slice(Turnstile.time.max(), Turnstile.direction, Turnstile.client)
        .select { Turnstile.client eq clientId }
        .groupBy(Turnstile.direction, Turnstile.client)
        .singleOrNull()
        ?.let { it[Turnstile.direction]  }
}
