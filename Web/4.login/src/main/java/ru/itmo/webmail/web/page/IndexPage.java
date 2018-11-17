package ru.itmo.webmail.web.page;

import ru.itmo.webmail.model.domain.FrontNews;
import ru.itmo.webmail.model.domain.News;
import ru.itmo.webmail.model.domain.User;

import javax.servlet.http.HttpServletRequest;
import java.util.*;

public class IndexPage extends Page {
    private void action(Map<String, Object> view) {
        List<FrontNews> result = new ArrayList<>();
        for (News n: newsService.findAll()) {
            result.add(new FrontNews(n));
        }
        view.put("news", result);
    }

    private void registrationDone(Map<String, Object> view) {
        view.put("message", "You have been registered");
        action(view);
    }

    private void enterDone(HttpServletRequest request, Map<String, Object> view) {
        view.put("message", "You have entered as " + ((User) request.getSession().getAttribute("user")).getLogin());
        action(view);
    }

    private void logout(Map<String, Object> view) {
        view.put("message", "You have been logged out");
        action(view);
    }

    private void newsAdded(Map<String, Object> view) {
        view.put("message", "News added successfully");
        action(view);
    }
}
