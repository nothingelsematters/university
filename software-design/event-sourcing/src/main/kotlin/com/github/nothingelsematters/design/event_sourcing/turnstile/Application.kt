package com.github.nothingelsematters.design.event_sourcing.turnstile

import com.github.nothingelsematters.design.event_sourcing.common.database.DatabaseConfiguration
import org.springframework.boot.autoconfigure.SpringBootApplication
import org.springframework.boot.runApplication
import org.springframework.context.annotation.Bean
import org.springframework.context.annotation.Configuration
import org.springframework.context.annotation.Import

@Configuration
@Import(DatabaseConfiguration::class)
open class ApplicationConfiguration {

    @Bean
    open fun turnstileService() = TurnstileServiceImpl()
}

@SpringBootApplication
open class Application

fun main(args: Array<String>) {
    runApplication<Application>(*args)
}
