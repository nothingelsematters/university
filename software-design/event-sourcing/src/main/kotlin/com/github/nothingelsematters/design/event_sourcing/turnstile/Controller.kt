package com.github.nothingelsematters.design.event_sourcing.turnstile

import org.springframework.web.bind.annotation.PathVariable
import org.springframework.web.bind.annotation.PostMapping
import org.springframework.web.bind.annotation.RequestMapping
import org.springframework.web.bind.annotation.RestController

@RestController
@RequestMapping("/api/v1/turnstile")
class Controller(private val turnstileService: TurnstileService) {

    @PostMapping("in/{clientId}") fun letIn(@PathVariable clientId: Int) = turnstileService.letIn(clientId)

    @PostMapping("out/{clientId}") fun letOut(@PathVariable clientId: Int) = turnstileService.letOut(clientId)
}

