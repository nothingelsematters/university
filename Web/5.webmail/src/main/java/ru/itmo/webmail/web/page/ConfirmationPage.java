package ru.itmo.webmail.web.page;

import ru.itmo.webmail.model.database.DatabaseUtils;
import ru.itmo.webmail.model.exception.RepositoryException;
import ru.itmo.webmail.web.exception.RedirectException;

import javax.servlet.http.HttpServletRequest;
import java.sql.*;
import java.util.Map;

public class ConfirmationPage extends Page {
    private void action(HttpServletRequest request, Map<String, Object> view) {
        String message = (getEmailConfirmationService().confirmBySecret(request.getParameter("secret")) ?
                "confirmationDone" : "didntFindEmailtoVerify");
        throw new RedirectException("/index", message);
    }
}
