package com.github.nothingelsematters.design.event_sourcing

import org.springframework.boot.test.context.SpringBootTest
import org.springframework.test.context.DynamicPropertyRegistry
import org.springframework.test.context.DynamicPropertySource
import org.testcontainers.containers.PostgreSQLContainer
import org.testcontainers.junit.jupiter.Container
import org.testcontainers.junit.jupiter.Testcontainers

@SpringBootTest
@Testcontainers
abstract class AbstractServiceTest {

    companion object {
        @Container
        private val container = KPostgreSQLContainer().withDatabaseName("database")

        @JvmStatic
        @DynamicPropertySource
        fun properties(registry: DynamicPropertyRegistry) {
            registry.add("database.url") { container.jdbcUrl }
            registry.add("database.username") { container.username }
            registry.add("database.password") { container.password }
        }
    }

    internal class KPostgreSQLContainer : PostgreSQLContainer<KPostgreSQLContainer>("postgres")
}
