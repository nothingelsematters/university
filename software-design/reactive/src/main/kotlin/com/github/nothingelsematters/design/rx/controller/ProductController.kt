package com.github.nothingelsematters.design.rx.controller

import com.github.nothingelsematters.design.rx.Product
import com.github.nothingelsematters.design.rx.service.ProductService
import org.springframework.web.bind.annotation.DeleteMapping
import org.springframework.web.bind.annotation.GetMapping
import org.springframework.web.bind.annotation.PathVariable
import org.springframework.web.bind.annotation.PostMapping
import org.springframework.web.bind.annotation.RequestBody
import org.springframework.web.bind.annotation.RequestMapping
import org.springframework.web.bind.annotation.RestController
import reactor.core.publisher.Mono

@RestController
@RequestMapping("/api/product")
class ProductController(private val productService: ProductService) {

    @PostMapping
    fun add(@RequestBody product: Product): Mono<Product> = productService.add(product)

    @GetMapping("{id}")
    fun get(@PathVariable id: Long): Mono<Product?> = productService[id]

    @DeleteMapping("{id}")
    fun delete(@PathVariable id: Long) = productService.delete(id)
}
