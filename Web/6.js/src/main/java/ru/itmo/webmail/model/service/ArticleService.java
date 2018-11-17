package ru.itmo.webmail.model.service;

import ru.itmo.webmail.model.domain.Article;
import ru.itmo.webmail.model.domain.User;
import ru.itmo.webmail.model.exception.ValidationException;
import ru.itmo.webmail.model.repository.ArticleRepository;
import ru.itmo.webmail.model.repository.impl.ArticleRepositoryImpl;

import java.util.List;

public class ArticleService {
    private ArticleRepository articleRepository = new ArticleRepositoryImpl();

    public void validateArticleAdding(String title, String text) throws ValidationException {
        if (title == null || title.isEmpty()) {
            throw new ValidationException("You have to write a title");
        }
        if (text == null || text.isEmpty()) {
            throw new ValidationException("You have to write a text");
        }
        if (text.length() > 10000) {
            throw new ValidationException("Your article is too big");
        }
    }

    public void addArticle(User user, String title, String text, String hidden) {
        articleRepository.addArticle(user, title, text, hidden);
    }

    public List<ArticleRepositoryImpl.FrontArticle> findAllFront() {
        return articleRepository.findAllFront();
    }

    public List<ArticleRepositoryImpl.FrontArticle> findAllDisplayableFront() {
        return articleRepository.findAllDisplayableFront();
    }

    public List<Article> findFromUser(User user) {
        return articleRepository.findFromUser(user);
    }

    public void changeDisplay(Long articleId, boolean show) {
        articleRepository.changeDisplay(articleId, show);
    }

    public void validateChanging(Long articleId, Long userId) throws ValidationException {
        if (articleRepository.find(articleId) == null) {
            throw new ValidationException("Article doesn't exist");
        }

        if (!articleRepository.find(articleId).getUserId().equals(userId)) {
            throw new ValidationException("You are not the author");
        }
    }
}
