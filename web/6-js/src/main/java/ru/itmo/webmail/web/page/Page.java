package ru.itmo.webmail.web.page;

import ru.itmo.webmail.model.domain.User;
import ru.itmo.webmail.model.service.ArticleService;
import ru.itmo.webmail.model.service.UserService;

import javax.servlet.http.HttpServletRequest;
import java.util.Map;

public class Page {
    static final String USER_ID_SESSION_KEY = "userId";

    private UserService userService = new UserService();
    private ArticleService articleService = new ArticleService();

    private User user;

    protected UserService getUserService() {
        return userService;
    }

    protected ArticleService getArticleService() {
        return articleService;
    }

    protected User getUser() {
        return user;
    }

    protected void login(HttpServletRequest request, User user) {
        request.getSession(true).setAttribute(USER_ID_SESSION_KEY, user.getId());
    }

    protected void logout(HttpServletRequest request) {
        request.getSession().removeAttribute(USER_ID_SESSION_KEY);
    }

    public void before(HttpServletRequest request, Map<String, Object> view) {
        Long userId = (Long) request.getSession().getAttribute(USER_ID_SESSION_KEY);
        if (userId != null) {
            user = userService.find(userId);
            view.put("user", user);
            if (user.isAdmin()) {
                view.put("admin", true);
            }
        }
    }

    public void after(HttpServletRequest request, Map<String, Object> view) {
        // No operations.
    }
}
