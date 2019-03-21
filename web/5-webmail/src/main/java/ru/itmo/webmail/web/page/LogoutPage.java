package ru.itmo.webmail.web.page;

import ru.itmo.webmail.model.database.DatabaseUtils;
import ru.itmo.webmail.model.domain.Event;
import ru.itmo.webmail.model.exception.RepositoryException;
import ru.itmo.webmail.web.exception.RedirectException;

import javax.servlet.http.HttpServletRequest;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.Map;

public class LogoutPage extends Page {
    private void action(HttpServletRequest request, Map<String, Object> view) {
        Long userId = (Long) request.getSession().getAttribute(USER_ID_SESSION_KEY);
        request.getSession().removeAttribute(USER_ID_SESSION_KEY);
        getEventService().actionPerformed(Event.EventEnum.Logout, getUserService().findById(userId));
        throw new RedirectException("/index");
    }
}
