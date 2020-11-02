package com.github.nothingelsematters.sd.mvc.model

data class Task(val id: Int, val description: String, var completed: Boolean = false)

data class ToDoList(val id: Int, val name: String, val tasks: MutableList<Task> = mutableListOf())
