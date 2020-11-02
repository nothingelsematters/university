package com.github.nothingelsematters.sd.mvc.controller

import com.github.nothingelsematters.sd.mvc.dao.ToDoListDao
import com.github.nothingelsematters.sd.mvc.model.ToDoList
import org.springframework.data.repository.query.Param
import org.springframework.web.bind.annotation.*

@RestController
@RequestMapping("/")
class ToDoListController(private val toDoListDao: ToDoListDao) {

    @GetMapping
    fun getToDoLists(): List<ToDoList> = toDoListDao.getAll()

    @PutMapping
    fun addToDoList(@RequestBody listName: String) = toDoListDao.add(listName)

    @DeleteMapping("/{list-id}")
    fun deleteToDoList(@PathVariable("list-id") listId: Int): Boolean = toDoListDao.delete(listId)

    @PutMapping("/{list-id}")
    fun addTask(@PathVariable("list-id") listId: Int, @RequestBody taskDescription: String) =
        toDoListDao.addTask(listId, taskDescription)

    @PostMapping("/{list-id}/{task-id}")
    fun markAsCompleted(@PathVariable("list-id") listId: Int, @PathVariable("task-id") taskId: Int) =
        toDoListDao.markCompleted(listId, taskId)
}
