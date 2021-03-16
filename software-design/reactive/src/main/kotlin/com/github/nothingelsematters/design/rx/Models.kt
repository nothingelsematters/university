package com.github.nothingelsematters.design.rx

import org.springframework.data.annotation.Id
import java.beans.ConstructorProperties
import org.springframework.data.mongodb.core.mapping.Document

enum class Currency {
    USD, EUR, RUB
}

data class Product
@ConstructorProperties("id", "name", "currency", "price")
constructor(
    @Id val id: Long,
    val name: String,
    val currency: Currency,
    val price: Double,
) {
    fun convertCurrency(neededCurrency: Currency): Product =
        if (currency == neededCurrency) this
        else copy(price = price * ApplicationConfiguration.conversions[currency]!![neededCurrency]!!)
}

@Document
data class User
@ConstructorProperties("id", "username", "currency")
constructor(
    @Id val id: Long,
    val username: String,
    val currency: Currency,
)
