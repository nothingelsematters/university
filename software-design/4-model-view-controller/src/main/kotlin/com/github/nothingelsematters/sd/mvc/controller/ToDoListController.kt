package com.github.nothingelsematters.sd.mvc.controller

import com.github.nothingelsematters.sd.mvc.dao.ToDoListDao
import com.github.nothingelsematters.sd.mvc.model.ToDoList
import org.springframework.data.repository.query.Param
import org.springframework.web.bind.annotation.*

@RestController
@RequestMapping("/api/v1")
class ToDoListController(private val toDoListDao: ToDoListDao) {

    @GetMapping
    fun getToDoLists(): List<ToDoList> = toDoListDao.getAll()

    @PostMapping
    fun addToDoList(@Param("listName") listName: String) = toDoListDao.add(listName)

    @PostMapping("/{list-id}")
    fun deleteToDoList(@PathVariable("list-id") listId: Int): Boolean = toDoListDao.delete(listId)

    @PostMapping("/{list-id}/new-task")
    fun addTask(@PathVariable("list-id") listId: Int, @Param("description") description: String) =
        toDoListDao.addTask(listId, description)

    @PostMapping("/{list-id}/{task-id}")
    fun markAsCompleted(@PathVariable("list-id") listId: Int, @PathVariable("task-id") taskId: Int) =
        toDoListDao.markCompleted(listId, taskId)
}
