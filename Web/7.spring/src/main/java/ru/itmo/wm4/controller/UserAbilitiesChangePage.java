package ru.itmo.wm4.controller;

import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestParam;
import ru.itmo.wm4.service.UserService;

import javax.servlet.http.HttpSession;

@Controller
public class UserAbilitiesChangePage extends Page {
    private final UserService userService;

    public UserAbilitiesChangePage(UserService userService) {
        this.userService = userService;
    }

    @PostMapping(path = "/userAbilitiesChange")
    public String userAbilitiesChange(@RequestParam long userId, @RequestParam Boolean userDisabled,
                                      HttpSession httpSession) {
        if (getUser(httpSession).getDisabled()) {
            return "redirect:/";
        }
        userService.changeAbilities(userId, !userDisabled);
        return "redirect:/users";
    }
}
