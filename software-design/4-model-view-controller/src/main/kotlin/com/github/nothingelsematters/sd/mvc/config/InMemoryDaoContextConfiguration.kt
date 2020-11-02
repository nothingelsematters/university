package com.github.nothingelsematters.sd.mvc.config

import com.github.nothingelsematters.sd.mvc.dao.ToDoListDao
import com.github.nothingelsematters.sd.mvc.dao.ToDoListInMemoryDao
import org.springframework.context.annotation.Bean
import org.springframework.context.annotation.Configuration

@Configuration
class InMemoryDaoContextConfiguration {

    @Bean
    fun toDoListDao(): ToDoListDao = ToDoListInMemoryDao()
}
