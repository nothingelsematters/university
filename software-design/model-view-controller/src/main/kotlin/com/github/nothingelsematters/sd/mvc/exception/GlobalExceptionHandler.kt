package com.github.nothingelsematters.sd.mvc.exception

import mu.KLogging
import org.springframework.http.HttpHeaders
import org.springframework.http.HttpStatus
import org.springframework.http.ResponseEntity
import org.springframework.web.bind.annotation.ControllerAdvice
import org.springframework.web.bind.annotation.ExceptionHandler
import org.springframework.web.context.request.WebRequest
import org.springframework.web.servlet.mvc.method.annotation.ResponseEntityExceptionHandler

@ControllerAdvice
class GlobalExceptionHandler : ResponseEntityExceptionHandler() {

    companion object : KLogging()

    @ExceptionHandler(NotFoundException::class)
    fun handleNotFoundException(ex: Exception, request: WebRequest): ResponseEntity<Any> {
        logger.info("Not found argument exception: " + ex.message)
        return handleExceptionInternal(ex, ex.message, HttpHeaders(), HttpStatus.NOT_FOUND, request)
    }

    @ExceptionHandler(Exception::class)
    fun handleTheRest(ex: Exception, request: WebRequest): ResponseEntity<Any> {
        logger.error("Unhandled exception: " + ex.message)

        return handleExceptionInternal(
            ex,
            "Internal server error",
            HttpHeaders(),
            HttpStatus.INTERNAL_SERVER_ERROR,
            request
        )
    }
}