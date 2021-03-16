package com.github.nothingelsematters.design.event_sourcing.turnstile

import org.joda.time.DateTime

class SubscriptionDurationExceededException(lastDate: DateTime) :
    RuntimeException(
        if (lastDate == DateTime(0)) "Subscription has not been activated yet"
        else "Subscription duration exceeded, last date was: $lastDate"
    )

class RougeDetectedException : RuntimeException("An attempt to pass the turnstile twice in one direction was detected")
