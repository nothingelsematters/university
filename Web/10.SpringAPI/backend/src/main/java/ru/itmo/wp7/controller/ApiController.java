package ru.itmo.wp7.controller;

import org.springframework.web.bind.annotation.ModelAttribute;
import ru.itmo.wp7.domain.User;

import javax.servlet.http.HttpServletRequest;

public class ApiController {
    @ModelAttribute
    public User getUser(HttpServletRequest httpServletRequest) {
        return (User) httpServletRequest.getAttribute("user");
    }
}
