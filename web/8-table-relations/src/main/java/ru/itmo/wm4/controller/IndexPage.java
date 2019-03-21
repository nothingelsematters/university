package ru.itmo.wm4.controller;

import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import ru.itmo.wm4.security.Guest;

import javax.servlet.http.HttpSession;

@Controller
public class IndexPage extends Page {
    @Guest
    @GetMapping(path = "")
    public String index() {
        return "IndexPage";
    }

    @Guest
    @GetMapping(path = "/logout")
    public String index(HttpSession httpSession) {
        unsetUser(httpSession);
        return "redirect:/";
    }
}
