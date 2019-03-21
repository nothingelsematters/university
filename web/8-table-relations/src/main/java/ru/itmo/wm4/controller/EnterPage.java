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
import ru.itmo.wm4.security.Guest;

import javax.servlet.http.HttpSession;
import javax.validation.Valid;

@Controller
public class EnterPage extends Page {
    private final UserCredentialsEnterValidator userCredentialsEnterValidator;

    public EnterPage(UserCredentialsEnterValidator userCredentialsEnterValidator) {
        this.userCredentialsEnterValidator = userCredentialsEnterValidator;
    }

    @InitBinder("enterForm")
    public void initRegisterFormBinder(WebDataBinder binder) {
        binder.addValidators(userCredentialsEnterValidator);
    }

    @Guest
    @GetMapping(path = "/enter")
    public String registerGet(Model model) {
        model.addAttribute("enterForm", new UserCredentials());
        return "EnterPage";
    }

    @Guest
    @PostMapping(path = "/enter")
    public String registerPost(@Valid @ModelAttribute("enterForm") UserCredentials registerForm,
                               BindingResult bindingResult, HttpSession httpSession) {
        if (bindingResult.hasErrors()) {
            return "EnterPage";
        }

        setUser(httpSession, getUserService().findByLoginAndPassword(registerForm.getLogin(), registerForm.getPassword()));
        return "redirect:/";
    }
}
