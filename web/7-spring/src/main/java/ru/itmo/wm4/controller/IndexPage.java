package ru.itmo.wm4.controller;

import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;

import javax.servlet.http.HttpSession;

@Controller
public class IndexPage extends Page {
    @GetMapping(path = "")
    public String index() {
        return "IndexPage";
    }

    @GetMapping(path = "/logout")
    public String index(HttpSession httpSession) {
        unsetUser(httpSession);
        return "redirect:/";
    }
}
