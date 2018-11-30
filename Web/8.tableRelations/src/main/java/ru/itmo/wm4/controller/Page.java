package ru.itmo.wm4.controller;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.ModelAttribute;
import ru.itmo.wm4.domain.Notice;
import ru.itmo.wm4.domain.Tag;
import ru.itmo.wm4.domain.User;
import ru.itmo.wm4.service.CommentService;
import ru.itmo.wm4.service.NoticeService;
import ru.itmo.wm4.service.UserService;

import javax.servlet.http.HttpSession;
import java.util.List;
import java.util.Set;

@Controller
public class Page {
    private static final String USER_ID_SESSION_KEY = "userId";

    @SuppressWarnings("SpringJavaAutowiredFieldsWarningInspection")
    @Autowired
    private UserService userService;

    @SuppressWarnings("SpringJavaAutowiredFieldsWarningInspection")
    @Autowired
    private NoticeService noticeService;

    @SuppressWarnings("SpringJavaAutowiredFieldsWarningInspection")
    @Autowired
    private CommentService commentService;

    @ModelAttribute
    public User getUser(HttpSession httpSession) {
        return httpSession == null ? null : userService.findById((Long) httpSession.getAttribute(USER_ID_SESSION_KEY));
    }

    @ModelAttribute(name = "notices")
    public List<Notice> getNotices() {
        return noticeService.findByOrderByCreationTimeDesc();
    }

    UserService getUserService() {
        return userService;
    }

    NoticeService getNoticeService() {
        return noticeService;
    }

    CommentService getCommentService() {
        return commentService;
    }

    void setUser(HttpSession httpSession, User user) {
        if (user != null) {
            httpSession.setAttribute(USER_ID_SESSION_KEY, user.getId());
        } else {
            httpSession.removeAttribute(USER_ID_SESSION_KEY);
        }
    }

    void unsetUser(HttpSession httpSession) {
        httpSession.removeAttribute(USER_ID_SESSION_KEY);
    }
}
