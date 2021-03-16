package com.github.nothingelsematters.design.event_sourcing.common.database

import org.jetbrains.exposed.dao.id.IntIdTable
import org.jetbrains.exposed.dao.id.LongIdTable
import org.jetbrains.exposed.sql.jodatime.CurrentDateTime
import org.jetbrains.exposed.sql.jodatime.datetime

object Clients : IntIdTable() {
    val name = varchar("name", 100)
    val phone = long("phone").nullable().check { it less 10_000_000_00_00 }
    val passport = long("passport").nullable().check { it less 100_00_000_000 }
}

object SubscriptionExtensions : IntIdTable() {
    val client = reference("client_id", Clients)
    val dealTime = datetime("since_time").defaultExpression(CurrentDateTime())
    val monthDuration = integer("month_duration").check { it greater 0 }
}

object Turnstile : LongIdTable() {
    val client = reference("client_id", Clients)
    val direction = enumeration("direction", Direction::class)
    val time = datetime("time").defaultExpression(CurrentDateTime())

    enum class Direction {
        IN, OUT
    }
}
