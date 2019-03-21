package ru.itmo.wm4.controller;

import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import ru.itmo.wm4.domain.User;
import ru.itmo.wm4.service.UserService;

@Controller
public class UserPage extends Page{
    private final UserService userService;

    public UserPage(UserService userService) {
        this.userService = userService;
    }

    @RequestMapping(value = "/user/{userId}", method= RequestMethod.GET)
    public String main(@PathVariable("userId") String userId, Model model){
        User user = userService.findById(Long.parseLong(userId));
        if (user != null) {
            model.addAttribute("userToShow", user);
        }
        return "UserPage";
    }
}
