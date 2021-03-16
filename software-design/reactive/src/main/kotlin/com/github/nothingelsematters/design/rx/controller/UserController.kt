package com.github.nothingelsematters.design.rx.controller

import com.github.nothingelsematters.design.rx.Product
import com.github.nothingelsematters.design.rx.User
import com.github.nothingelsematters.design.rx.service.UserService
import org.springframework.web.bind.annotation.DeleteMapping
import org.springframework.web.bind.annotation.GetMapping
import org.springframework.web.bind.annotation.PathVariable
import org.springframework.web.bind.annotation.PostMapping
import org.springframework.web.bind.annotation.RequestBody
import org.springframework.web.bind.annotation.RequestMapping
import org.springframework.web.bind.annotation.RestController
import reactor.core.publisher.Flux
import reactor.core.publisher.Mono

@RestController
@RequestMapping("/api/user")
class UserController(private val userService: UserService) {

    @PostMapping
    fun add(@RequestBody user: User): Mono<User> = userService.add(user)

    @GetMapping("{id}")
    fun get(@PathVariable id: Long): Mono<User?> = userService[id]

    @DeleteMapping("{id}")
    fun delete(@PathVariable id: Long) = userService.delete(id)

    @GetMapping("{id}/products")
    fun getProducts(@PathVariable id: Long): Flux<Product> = userService.getProducts(id)
}
