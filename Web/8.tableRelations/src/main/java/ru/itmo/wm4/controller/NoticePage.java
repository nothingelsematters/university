package ru.itmo.wm4.controller;

import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.WebDataBinder;
import org.springframework.web.bind.annotation.*;
import ru.itmo.wm4.domain.Notice;
import ru.itmo.wm4.domain.Role;
import ru.itmo.wm4.form.NoticeCredentials;
import ru.itmo.wm4.form.validator.NoticeCredentialsAddValidator;
import ru.itmo.wm4.security.AnyRole;

import javax.servlet.http.HttpSession;
import javax.validation.Valid;

@Controller
public class NoticePage extends Page {
    private final NoticeCredentialsAddValidator noticeCredentialsAddValidator;

    public NoticePage(NoticeCredentialsAddValidator noticeCredentialsAddValidator) {
        this.noticeCredentialsAddValidator = noticeCredentialsAddValidator;
    }

    @InitBinder("noticeForm")
    public void initRegisterFormBinder(WebDataBinder binder) {
        binder.addValidators(noticeCredentialsAddValidator);
    }

    @AnyRole(Role.Name.ADMIN)
    @GetMapping(path = "/notice")
    public String noticeGet(Model model) {
        model.addAttribute("noticeForm", new NoticeCredentials());
        return "NoticePage";
    }

    @AnyRole(Role.Name.ADMIN)
    @PostMapping(path = "/notice")
    public String noticePost(@Valid @ModelAttribute("noticeForm") NoticeCredentials notice,
                               BindingResult bindingResult, HttpSession httpSession) {
        if (bindingResult.hasErrors()) {
            return "NoticePage";
        }

        getNoticeService().save(notice, getUser(httpSession));
        return "redirect:/notices";
    }
}
