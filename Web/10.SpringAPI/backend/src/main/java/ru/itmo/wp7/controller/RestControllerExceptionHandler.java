package ru.itmo.wp7.controller;

import org.springframework.http.HttpStatus;
import org.springframework.validation.BindException;
import org.springframework.validation.FieldError;
import org.springframework.web.bind.MethodArgumentNotValidException;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.ResponseStatus;
import ru.itmo.wp7.exception.LoginInUseException;
import ru.itmo.wp7.exception.NoSuchResourceException;
import ru.itmo.wp7.exception.ValidationException;

@ControllerAdvice
public class RestControllerExceptionHandler {
    private Error handleValidationException(FieldError fieldError) {
        if (fieldError != null) {
            String message = fieldError.getField() + ": " + fieldError.getDefaultMessage();
            return new Error(message);
        } else {
            return new Error("Method argument not valid");
        }
    }

    @SuppressWarnings("unused")
    @ExceptionHandler
    @ResponseStatus(HttpStatus.NOT_FOUND)
    @ResponseBody
    public Error handleException(NoSuchResourceException ignored) {
        return new Error("Resource not found");
    }

    @ExceptionHandler
    @ResponseStatus(HttpStatus.BAD_REQUEST)
    @ResponseBody
    public Error handleException(MethodArgumentNotValidException e) {
        return handleValidationException(e.getBindingResult().getFieldError());
    }

    @ExceptionHandler
    @ResponseStatus(HttpStatus.BAD_REQUEST)
    @ResponseBody
    public Error handleException(BindException e) {
        return handleValidationException(e.getBindingResult().getFieldError());
    }

    @ExceptionHandler
    @ResponseStatus(HttpStatus.BAD_REQUEST)
    @ResponseBody
    public Error handleException(ValidationException e) {
        return new Error(e.getMessage());
    }

    @ExceptionHandler
    @ResponseStatus(HttpStatus.BAD_REQUEST)
    @ResponseBody
    public Error handleException(LoginInUseException ignored) {
        return new Error("Login is already in use");
    }

    public static class Error {
        private final String message;

        public Error(String message) {
            this.message = message;
        }

        @SuppressWarnings("unused")
        public String getMessage() {
            return message;
        }
    }
}
