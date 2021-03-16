package com.github.nothingelsematters.design.event_sourcing.turnstile

import com.github.nothingelsematters.design.event_sourcing.AbstractServiceTest
import com.github.nothingelsematters.design.event_sourcing.common.database.Clients
import com.github.nothingelsematters.design.event_sourcing.common.database.SubscriptionExtensions
import org.jetbrains.exposed.sql.insert
import org.jetbrains.exposed.sql.insertAndGetId
import org.jetbrains.exposed.sql.transactions.transaction
import org.joda.time.DateTime
import kotlin.test.Test
import kotlin.test.assertFailsWith
import kotlin.test.assertNotEquals

internal class TurnstileServiceTest : AbstractServiceTest() {

    private val turnstileService = TurnstileServiceImpl()

    @Test
    fun `client with a valid subscription going in`() =
        turnstileService.letIn(subscribe(1, DateTime.now().minusMinutes(5)))

    @Test
    fun `client with a time exceeded subscription going in`() {
        assertFailsWith<SubscriptionDurationExceededException> {
            turnstileService.letIn(subscribe(3, DateTime.now().minusMonths(5)))
        }
    }

    @Test
    fun `client without subscription going in`() {
        assertFailsWith<SubscriptionDurationExceededException> {
            turnstileService.letIn(transaction { registerClient() })
        }
    }

    @Test
    fun `client tries to go in second time`() {
        val clientId = subscribe(1, DateTime.now().minusMinutes(5))
        turnstileService.letIn(clientId)
        assertFailsWith<RougeDetectedException> { turnstileService.letIn(clientId) }
    }

    @Test
    fun `client tries to go out second time`() {
        val clientId = subscribe(1, DateTime.now().minusMinutes(5))
        turnstileService.letOut(clientId)
        assertFailsWith<RougeDetectedException> { turnstileService.letOut(clientId) }
    }

    @Test
    fun `client with a valid subscription going in & out`() {
        val clientId = subscribe(1, DateTime.now().minusMinutes(5))
        turnstileService.letIn(clientId)
        turnstileService.letOut(clientId)
    }

    private fun subscribe(monthDuration: Int, dealTime: DateTime = DateTime.now()): Int {
        var clientId: Int? = null

        transaction {
            clientId = registerClient()

            SubscriptionExtensions.insert {
                it[client] = clientId
                it[SubscriptionExtensions.monthDuration] = monthDuration
                it[SubscriptionExtensions.dealTime] = dealTime
            }
        }

        assertNotEquals(null, clientId)
        return clientId!!
    }

    private fun registerClient(): Int =
        Clients
            .insertAndGetId {
                it[name] = "name"
                it[phone] = null
                it[passport] = 40_10_123456
            }
            .value
}
