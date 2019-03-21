package ru.itmo.webmail.web.page;

import ru.itmo.webmail.model.domain.User;
import ru.itmo.webmail.model.service.NewsService;
import ru.itmo.webmail.model.service.UserService;

import javax.servlet.http.HttpServletRequest;
import java.util.Map;

public abstract class Page {
    protected UserService userService = new UserService();
    protected NewsService newsService = new NewsService();

    public void before(HttpServletRequest request, Map<String, Object> view) {

    }

    public void after(HttpServletRequest request, Map<String, Object> view) {
        view.put("userCount", userService.findCount());
        User user = (User) request.getSession().getAttribute("user");
        if (user != null) {
            view.put("authorized", true);
            view.put("username", user.getLogin());
        }
        view.put("news", newsService.findAll());
    }
}
