package ru.itmo.webmail.web.page;

import ru.itmo.webmail.web.exception.RedirectException;

import javax.servlet.http.HttpServletRequest;

public class LogoutPage extends Page {
    private void logout(HttpServletRequest request) {
        request.getSession().removeAttribute("user");
        throw new RedirectException("/index", "logout");
    }

    private void action(HttpServletRequest request) {
        logout(request);
    }
}
