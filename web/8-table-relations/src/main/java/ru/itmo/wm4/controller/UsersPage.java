package ru.itmo.wm4.controller;

import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import ru.itmo.wm4.domain.Role;
import ru.itmo.wm4.security.AnyRole;

@Controller
public class UsersPage extends Page {
    @AnyRole({Role.Name.USER, Role.Name.ADMIN})
    @GetMapping(path = "/users")
    public String main(Model model) {
        model.addAttribute("users", getUserService().findAll());
        return "UsersPage";
    }
}
