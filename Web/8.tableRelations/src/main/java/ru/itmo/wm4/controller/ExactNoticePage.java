package ru.itmo.wm4.controller;

import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.annotation.*;
import ru.itmo.wm4.domain.Comment;
import ru.itmo.wm4.domain.Notice;
import ru.itmo.wm4.domain.Role;
import ru.itmo.wm4.security.AnyRole;
import ru.itmo.wm4.security.Guest;

import javax.servlet.http.HttpSession;
import javax.validation.Valid;

@Controller
public class ExactNoticePage extends Page {
    @Guest
    @RequestMapping(value = "/notice/{noticeId}", method= RequestMethod.GET)
    public String noticeIdGet(@PathVariable("noticeId") String noticeId, Model model){
        Notice notice = getNoticeService().findById(Long.parseLong(noticeId));
        if (notice != null) {
            model.addAttribute("notice", notice);
        }
        return "ExactNoticePage";
    }

    @AnyRole({Role.Name.ADMIN, Role.Name.USER})
    @RequestMapping(value = "/notice/{noticeId}", method= RequestMethod.POST)
    public String noticeIdPost(@PathVariable("noticeId") String noticeId,
                               @Valid @ModelAttribute("comment") Comment comment,
                               BindingResult bindingResult, HttpSession httpSession){
        if (bindingResult.hasErrors()) {
            return "ExactNoticePage";
        }

        getCommentService().save(comment, getNoticeService().findById(Long.parseLong(noticeId)), getUser(httpSession));
        return "redirect:/notice/" + noticeId;
    }
}
