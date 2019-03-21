package ru.itmo.wp7.controller;

import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import ru.itmo.wp7.domain.User;
import ru.itmo.wp7.exception.NoSuchResourceException;
import ru.itmo.wp7.service.JwtService;
import ru.itmo.wp7.service.UserService;

@RestController
@RequestMapping("/api/1")
public class JwtController extends ApiController {
    private final JwtService jwtService;

    private final UserService userService;

    public JwtController(JwtService jwtService, UserService userService) {
        this.jwtService = jwtService;
        this.userService = userService;
    }

    @GetMapping("jwt")
    public String auth(@RequestParam String login, @RequestParam String password) {
        User user = userService.findByLoginAndPassword(login, password).orElseThrow(NoSuchResourceException::new);
        return jwtService.create(user);
    }
}
