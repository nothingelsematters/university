package com.github.nothingelsematters.design.event_sourcing.report

import com.github.nothingelsematters.design.event_sourcing.common.database.Turnstile
import com.github.nothingelsematters.design.event_sourcing.common.database.Turnstile.Direction.IN
import com.github.nothingelsematters.design.event_sourcing.common.database.Turnstile.Direction.OUT
import org.jetbrains.exposed.sql.Min
import org.jetbrains.exposed.sql.Query
import org.jetbrains.exposed.sql.and
import org.jetbrains.exposed.sql.min
import org.jetbrains.exposed.sql.notExists
import org.jetbrains.exposed.sql.select
import org.jetbrains.exposed.sql.selectAll
import org.jetbrains.exposed.sql.transactions.transaction
import org.joda.time.DateTime
import org.joda.time.Duration
import java.util.*

interface ReportService {

    fun getVisitingStatistics(from: DateTime, to: DateTime): VisitingStatistics

    fun getAverageStatistics(): AverageStatistics
}

class ReportServiceImpl : ReportService {

    private val timeZero = 1614700000

    private var snapshotDay: Date = DateTime(0).toDate()

    private var openingDay: DateTime? = null

    private var visitAmount = 0L

    private var totalMinutes = 0L

    override fun getVisitingStatistics(from: DateTime, to: DateTime) = transaction {
        Turnstile
            .select { Turnstile.direction.eq(IN) and Turnstile.time.less(to) and Turnstile.time.greater(from) }
            .orderBy(Turnstile.time)
            .asSequence()
            .map { it[Turnstile.time].toLocalDate().also { println(it) } }
            .groupBy { it }
            .also { println(it) }
            .mapValues { it.value.size }
            .toList()
            .asSequence()
            .sortedBy { it.first }
            .map { it.second }
            .toList()
            .run(::VisitingStatistics)
    }

    override fun getAverageStatistics(): AverageStatistics {
        updateAverageStatistics()

        return AverageStatistics(
            if (openingDay == null) 0.0
            else visitAmount.toDouble() /
                (Duration(openingDay!!.toInstant(), DateTime.now().toInstant()).standardDays + 1),
            (totalMinutes.toDouble() / visitAmount / 60).toInt(),
        )
    }

    private fun updateAverageStatistics() = transaction {
        if (openingDay == null) {
            openingDay = getOpeningDay()
        }

        if (openingDay == null) {
            return@transaction
        }

        val nextSnapshotDay = DateTime.now().toDate()

        visitAmount += Turnstile
            .select {
                Turnstile.direction.eq(IN) and
                    Turnstile.time.greater(DateTime(snapshotDay)) and
                    Turnstile.time.less(DateTime(nextSnapshotDay))
            }
            .count()

        totalMinutes += getSeconds {
            select {
                direction.eq(OUT) and time.greater(DateTime(snapshotDay)) and time.less(DateTime(nextSnapshotDay))
            }
        }

        totalMinutes -= getSeconds {
            select { direction.eq(IN) and time.greater(DateTime(snapshotDay)) and time.less(DateTime(nextSnapshotDay)) }
        }

        snapshotDay = nextSnapshotDay
    }

    private fun getSeconds(query: Turnstile.() -> Query) =
        Turnstile.query().sumOf { (it[Turnstile.time].millis / 1000 - timeZero).also { println(it) } }

    private fun getOpeningDay() = Turnstile
        .slice(Turnstile.time.min())
        .selectAll()
        .singleOrNull()
        ?.get(Min(Turnstile.time, Turnstile.time.columnType))
}
