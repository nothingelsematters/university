package ru.itmo.webmail.web.page;

import javax.servlet.http.HttpServletRequest;
import java.util.Map;

public class IndexPage extends Page {
    private void action(HttpServletRequest request, Map<String, Object> view) {
        // No operations.
    }

    private void registrationDone(HttpServletRequest request, Map<String, Object> view) {
        view.put("message", "You have been registered. Now you have to verify your account by sent link");
    }

    private void didintFindEmailtoVerify(HttpServletRequest request, Map<String, Object> view) {
        view.put("message", "Didn't find email to verify");
    }

    private void confirmationDone(HttpServletRequest request, Map<String, Object> view) {
        view.put("message", "Your account is verified");
    }
}
