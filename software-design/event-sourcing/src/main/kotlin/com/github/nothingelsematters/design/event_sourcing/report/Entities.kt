package com.github.nothingelsematters.design.event_sourcing.report

import org.joda.time.DateTime

data class VisitingStatisticsRequest(val from: DateTime, val to: DateTime)

data class VisitingStatistics(val visitAmounts: List<Int>)

data class AverageStatistics(val averageFrequency: Double, val averageMinutesDuration: Int)
