package ru.itmo.webmail.web.page;

import ru.itmo.webmail.model.domain.Talk;
import ru.itmo.webmail.model.exception.ValidationException;
import ru.itmo.webmail.web.exception.RedirectException;

import javax.servlet.http.HttpServletRequest;
import java.util.Map;

public class TalksPage extends Page {
    @Override
    public void before(HttpServletRequest request, Map<String, Object> view) {
        super.before(request, view);

        if (getUser() == null) {
            throw new RedirectException("/index");
        }
    }

    public void action(HttpServletRequest request, Map<String, Object> view) {
        view.put("talks", getTalkService().findAllFrontForUser(getUser().getId()));
    }

    private void talks(HttpServletRequest request, Map<String, Object> view) {
        Talk talk = new Talk();
        talk.setSourceUserId(getUser().getId());
        String target = request.getParameter("target");
        String text = request.getParameter("text");

        try {
            getTalkService().validateSending(target, text);
        } catch (ValidationException e) {
            view.put("target", target);
            view.put("text", text);
            view.put("error", e.getMessage());
            view.put("talks", getTalkService().findAllFrontForUser(getUser().getId()));
            return;
        }

        getTalkService().send(getUser().getId(), getUserService().findByLoginOrEmail(target).getId(), text);
        throw new RedirectException("/talks");
    }
}
