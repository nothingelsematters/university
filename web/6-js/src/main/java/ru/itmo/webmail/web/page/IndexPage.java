package ru.itmo.webmail.web.page;

import ru.itmo.webmail.model.domain.Article;
import ru.itmo.webmail.model.domain.User;
import ru.itmo.webmail.model.repository.impl.ArticleRepositoryImpl;

import javax.servlet.http.HttpServletRequest;
import java.util.List;
import java.util.Map;

public class IndexPage extends Page {
    private void action(HttpServletRequest request, Map<String, Object> view) {
        // No operations.
    }

    private List<ArticleRepositoryImpl.FrontArticle> find(HttpServletRequest request, Map<String, Object> view) {
        return getArticleService().findAllDisplayableFront();
    }

    private void registrationDone(HttpServletRequest request, Map<String, Object> view) {
        view.put("message", "You have been registered");
    }
}
