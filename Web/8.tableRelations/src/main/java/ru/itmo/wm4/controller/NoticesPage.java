package ru.itmo.wm4.controller;

import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import ru.itmo.wm4.domain.Role;
import ru.itmo.wm4.domain.User;
import ru.itmo.wm4.security.AnyRole;

import javax.servlet.http.HttpSession;

@Controller
public class NoticesPage extends Page {
    @AnyRole(Role.Name.ADMIN)
    @GetMapping(path = "/notices")
    public String notices(Model model, HttpSession httpSession) {
        User user = getUser(httpSession);
        model.addAttribute("notices", user.getNotices());
        return "NoticesPage";
    }
}

