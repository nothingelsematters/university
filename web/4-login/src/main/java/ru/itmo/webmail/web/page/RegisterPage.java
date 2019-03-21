package ru.itmo.webmail.web.page;

import ru.itmo.webmail.model.domain.User;
import ru.itmo.webmail.model.exception.ValidationException;
import ru.itmo.webmail.model.service.UserService;
import ru.itmo.webmail.web.exception.RedirectException;

import javax.servlet.http.HttpServletRequest;
import java.util.Map;

public class RegisterPage extends Page {
    private void register(HttpServletRequest request, Map<String, Object> view) {
        User user = new User();
        user.setLogin(request.getParameter("login"));
        String password = request.getParameter("password");
        String confirmation = request.getParameter("password_confirmation");
        String email = request.getParameter("email");

        try {
            userService.validateRegistration(user, password, confirmation, email);
        } catch (ValidationException e) {
            view.put("login", user.getLogin());
            view.put("email", email);
            view.put("password", password);
            view.put("password_confirmation", confirmation);
            view.put("error", e.getMessage());
            return;
        }

        userService.register(user, password, email);
        throw new RedirectException("/index", "registrationDone");
    }

    private void action() {
        // No operations.
    }
}
