package ru.itmo.webmail.web.page;

import ru.itmo.webmail.model.domain.User;
import ru.itmo.webmail.model.exception.ValidationException;
import ru.itmo.webmail.web.exception.RedirectException;

import javax.servlet.http.HttpServletRequest;
import java.nio.charset.Charset;
import java.util.Map;

import static java.nio.charset.StandardCharsets.UTF_8;

public class AddNewsPage extends Page {
    private void action() {
        // No operations.
    }

    private void addNews(HttpServletRequest request, Map<String, Object> view) {
        String text = request.getParameter("text");
        User user = (User) request.getSession().getAttribute("user");

        try {
            newsService.validateNews(text, user);
        } catch (ValidationException e) {
            view.put("text", text);
            view.put("error", e.getMessage());
            return;
        }

        newsService.save(text, user.getId());
        throw new RedirectException("/index", "newsAdded");
    }
}
