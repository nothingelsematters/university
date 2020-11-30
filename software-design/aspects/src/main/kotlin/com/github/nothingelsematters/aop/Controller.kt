package com.github.nothingelsematters.aop

import org.springframework.web.bind.annotation.GetMapping
import org.springframework.web.bind.annotation.RequestMapping
import org.springframework.web.bind.annotation.RestController

@RestController
@RequestMapping("/api/v1")
class Controller {

    @GetMapping("/inc")
    fun increment(): Int {
        var count = 0
        repeat(1000000000) { count++ }
        return count
    }

    @GetMapping("/dec")
    fun decrement(): Int {
        var count = 1000000000
        repeat(1000000000) { count-- }
        return count
    }
}
