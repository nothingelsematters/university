package com.github.nothingelsematters.design.event_sourcing.administrator

import com.github.nothingelsematters.design.event_sourcing.AbstractServiceTest
import com.github.nothingelsematters.design.event_sourcing.common.database.SubscriptionExtensions
import org.jetbrains.exposed.sql.selectAll
import org.jetbrains.exposed.sql.transactions.transaction
import org.joda.time.DateTime
import org.joda.time.Duration
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertNull
import kotlin.test.assertTrue

internal class AdministratorServiceTest : AbstractServiceTest() {

    private val administratorService = AdministratorServiceImpl()

    @Test
    fun `register user correction`() {
        administratorService.registerClient(Client("Some Name"))
    }

    @Test
    fun `get unreal subscription returns null`() = assertNull(administratorService.getSubscription(-1))

    @Test
    fun `get real subscription correctness`() {
        val clientName = "Some Name"
        val monthDuration = 10

        val clientId = administratorService.registerClient(Client(clientName))
        administratorService.renewSubscription(clientId, monthDuration)

        val actual = administratorService.getSubscription(clientId)!!

        assertEquals(clientName, actual.clientName)

        val expectedLastDay = DateTime.now().plusMonths(monthDuration)
        println("Actual: ${actual.lastDay}, expected: $expectedLastDay")

        assertTrue { Duration(actual.lastDay, expectedLastDay) < Duration.standardSeconds(10) }
    }

    @Test
    fun `double renew test`() {
        val clientName = "Some Name"
        val monthDuration = 10

        val clientId = administratorService.registerClient(Client(clientName))
        administratorService.renewSubscription(clientId, monthDuration)
        administratorService.renewSubscription(clientId, monthDuration)

        assertEquals(2, transaction { SubscriptionExtensions.selectAll().count() })

        val actual = administratorService.getSubscription(clientId)!!

        assertEquals(clientName, actual.clientName)

        val expectedLastDay = DateTime.now().plusMonths(2 * monthDuration)
        println("Actual: ${actual.lastDay}, expected: $expectedLastDay")

        assertTrue {
            Duration(actual.lastDay, expectedLastDay) < Duration.standardSeconds(10)
        }
    }
}
