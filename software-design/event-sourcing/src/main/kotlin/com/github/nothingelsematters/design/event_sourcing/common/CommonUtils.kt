package com.github.nothingelsematters.design.event_sourcing.common

import com.github.nothingelsematters.design.event_sourcing.common.database.SubscriptionExtensions
import org.jetbrains.exposed.sql.select
import org.joda.time.DateTime

fun getSubscriptionEndTime(clientId: Int) = SubscriptionExtensions
    .select { SubscriptionExtensions.client eq clientId }
    .orderBy(SubscriptionExtensions.dealTime)
    .fold(DateTime(0)) { acc, resultRow ->
        val currentEnd = if (resultRow[SubscriptionExtensions.dealTime] > acc) {
            resultRow[SubscriptionExtensions.dealTime]
        } else {
            acc
        }

        currentEnd.plusMonths(resultRow[SubscriptionExtensions.monthDuration])
    }
