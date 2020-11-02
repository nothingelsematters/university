package com.github.nothingelsematters.sd.mvc.dao

import com.github.nothingelsematters.sd.mvc.model.Task
import com.github.nothingelsematters.sd.mvc.model.ToDoList

interface ToDoListDao {

    fun add(name: String): ToDoList

    fun getAll(): List<ToDoList>

    fun getById(id: Int): ToDoList?

    fun delete(id: Int): Boolean

    fun addTask(listId: Int, description: String): Task

    fun markCompleted(listId: Int, taskId: Int): Boolean
}
