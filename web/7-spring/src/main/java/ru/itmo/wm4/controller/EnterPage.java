package ru.itmo.wm4.controller;

import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.WebDataBinder;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.InitBinder;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.PostMapping;
import ru.itmo.wm4.form.UserCredentials;
import ru.itmo.wm4.form.validator.UserCredentialsEnterValidator;
import ru.itmo.wm4.service.UserService;

import javax.servlet.http.HttpSession;
import javax.validation.Valid;

@Controller
public class EnterPage extends Page {
    private final UserService userService;
    private final UserCredentialsEnterValidator userCredentialsEnterValidator;

    public EnterPage(UserService userService, UserCredentialsEnterValidator userCredentialsEnterValidator) {
        this.userService = userService;
        this.userCredentialsEnterValidator = userCredentialsEnterValidator;
    }

    @InitBinder
    public void initRegisterFormBinder(WebDataBinder binder) {
        binder.addValidators(userCredentialsEnterValidator);
    }

    @GetMapping(path = "/enter")
    public String registerGet(Model model) {
        model.addAttribute("enterForm", new UserCredentials());
        return "EnterPage";
    }

    @PostMapping(path = "/enter")
    public String registerPost(@Valid @ModelAttribute("enterForm") UserCredentials registerForm,
                               BindingResult bindingResult, HttpSession httpSession) {
        if (bindingResult.hasErrors()) {
            return "EnterPage";
        }

        setUser(httpSession, userService.findByLoginAndPassword(registerForm.getLogin(), registerForm.getPassword()));
        return "redirect:/";
    }
}
