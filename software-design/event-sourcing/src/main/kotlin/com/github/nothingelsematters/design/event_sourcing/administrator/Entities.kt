package com.github.nothingelsematters.design.event_sourcing.administrator

import org.joda.time.DateTime

data class Client(val name: String, val phone: Long? = null, val passport: Long? = null)

data class Subscription(val clientName: String, val lastDay: DateTime)

data class SubscriptionRenew(val clientId: Int, val monthDuration: Int)
