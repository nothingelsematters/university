package ru.itmo.webmail.web.page;

import ru.itmo.webmail.model.domain.User;
import ru.itmo.webmail.model.exception.ValidationException;
import ru.itmo.webmail.web.exception.RedirectException;

import javax.servlet.http.HttpServletRequest;
import java.util.Map;

public class EnterPage extends Page {
    private void enter(HttpServletRequest request, Map<String, Object> view) {
        String login = request.getParameter("login");
        String password = request.getParameter("password");
        User entered;

        try {
            entered = userService.validateEntry(login, password);
        } catch (ValidationException e) {
            view.put("login", login);
            view.put("password", password);
            view.put("error", e.getMessage());
            return;
        }

        request.getSession().setAttribute("user", entered);
        throw new RedirectException("/index", "enterDone");
    }

    private void action() {
        // No operations.
    }
}
