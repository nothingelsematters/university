package com.github.nothingelsematters.design.event_sourcing.report

import com.github.nothingelsematters.design.event_sourcing.AbstractServiceTest
import com.github.nothingelsematters.design.event_sourcing.common.database.Clients
import com.github.nothingelsematters.design.event_sourcing.common.database.SubscriptionExtensions
import com.github.nothingelsematters.design.event_sourcing.common.database.Turnstile
import com.github.nothingelsematters.design.event_sourcing.common.database.Turnstile.Direction.*
import org.jetbrains.exposed.sql.deleteAll
import org.jetbrains.exposed.sql.insert
import org.jetbrains.exposed.sql.insertAndGetId
import org.jetbrains.exposed.sql.transactions.transaction
import org.joda.time.DateTime
import org.joda.time.Duration
import kotlin.math.abs
import kotlin.test.BeforeTest
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertTrue

internal class ReportServiceTest : AbstractServiceTest() {

    private val reportService = ReportServiceImpl()

    @BeforeTest
    fun cleanUp() {
        transaction { Turnstile.deleteAll() }
    }

    @Test
    fun `average statistics test`() {
        val days = 10
        var minutes = 0

        transaction {
            val clientId = registerClient()
            subscribe(clientId, 10, DateTime.now().minusMonths(1))

            val now = DateTime.now().minusDays(days + 2)

            for (i in 0 until days) {
                minutes += i + 10
                val duration = Duration.standardMinutes(i + 10L)
                exercise(clientId, now.plusDays(i), duration)
            }
        }

        val actual = reportService.getAverageStatistics()

        assertTrue { abs(actual.averageFrequency - (days.toDouble() / (days + 3))) < 1e-7 }
        assertEquals((minutes.toDouble() / days).toInt(), actual.averageMinutesDuration)
    }

    @Test
    fun `visiting statistics test`() {
        val days = 4

        val now = DateTime.now()

        val visits = mutableListOf<DateTime>()

        transaction {
            val clientId = registerClient()
            subscribe(clientId, 10, DateTime.now().minusMonths(1))

            for (i in 1..days) {
                for (j in 0 until i) {
                    val inTime = now.plusDays(i - 1).plusHours(j)
                    val duration = Duration.standardMinutes(i + 3L)
                    println("Exercising: $inTime -> ${inTime + duration}")
                    visits.add(inTime)

                    exercise(clientId, inTime, duration)
                }
            }
        }

        val actual = reportService.getVisitingStatistics(now.minusMinutes(1), now.plusDays(days))

        assertEquals(
            visits.asSequence()
                .map { it.toLocalDate() }
                .groupBy { it }
                .mapValues { it.value.size }
                .toList()
                .asSequence()
                .sortedBy { it.first }
                .map { it.second }
                .toList()
                .run(::VisitingStatistics),
            actual,
        )
    }

    private fun exercise(clientId: Int, inTime: DateTime, duration: Duration) {
        Turnstile.insert {
            it[client] = clientId
            it[direction] = IN
            it[time] = inTime
        }

        Turnstile.insert {
            it[client] = clientId
            it[direction] = OUT
            it[time] = inTime + duration
        }
    }

    private fun subscribe(clientId: Int, monthDuration: Int, dealTime: DateTime = DateTime.now()) {
        SubscriptionExtensions.insert {
            it[client] = clientId
            it[SubscriptionExtensions.monthDuration] = monthDuration
            it[SubscriptionExtensions.dealTime] = dealTime
        }
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
