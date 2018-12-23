package ru.itmo.wp7.controller;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;
import ru.itmo.wp7.domain.User;
import ru.itmo.wp7.exception.LoginInUseException;
import ru.itmo.wp7.service.UserService;
import ru.itmo.wp7.form.UserCredentials;

import javax.validation.Valid;
import java.util.List;
import java.util.Optional;

@RestController
@RequestMapping("/api/1")
public class UserController extends ApiController {
    @SuppressWarnings("SpringJavaAutowiredFieldsWarningInspection")
    @Autowired
    private UserService userService;

    @GetMapping("users/authenticated")
    public User getAuthenticatedUser(User user) {
        return user;
    }

    @GetMapping("user/{userId}")
    public Optional<User> getNotice(@PathVariable("userId") String userId) {
        return  userService.findById(Long.parseLong(userId));
    }

    @GetMapping("users")
    public List<User> getUsers() {
        return userService.findAll();
    }

    @PostMapping(path = "users")
    public void register(@Valid @RequestBody UserCredentials userCredentials)
            throws LoginInUseException {

        if (userService.countByLogin(userCredentials.getLogin()) != 0) {
            throw new LoginInUseException();
        }
        userService.register(userCredentials);
    }
}
