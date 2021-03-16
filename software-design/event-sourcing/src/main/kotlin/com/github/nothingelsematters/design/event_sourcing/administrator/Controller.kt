package com.github.nothingelsematters.design.event_sourcing.administrator

import org.springframework.web.bind.annotation.GetMapping
import org.springframework.web.bind.annotation.PathVariable
import org.springframework.web.bind.annotation.PostMapping
import org.springframework.web.bind.annotation.RequestBody
import org.springframework.web.bind.annotation.RequestMapping
import org.springframework.web.bind.annotation.RestController

@RestController
@RequestMapping("/api/v1")
class Controller(private val administratorService: AdministratorService) {

    @PostMapping("client") fun register(@RequestBody client: Client) = administratorService.registerClient(client)

    @PostMapping("subscription") fun renew(@RequestBody subscriptionRenew: SubscriptionRenew) =
        administratorService.renewSubscription(subscriptionRenew.clientId, subscriptionRenew.monthDuration)

    @GetMapping("subscription/{id}") fun get(@PathVariable("id") id: Int) = administratorService.getSubscription(id)
}
