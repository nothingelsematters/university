package ru.itmo.webmail.model.repository;

import ru.itmo.webmail.model.domain.Article;
import ru.itmo.webmail.model.domain.User;
import ru.itmo.webmail.model.repository.impl.ArticleRepositoryImpl;

import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.util.List;

public interface ArticleRepository {
    void addArticle(User user, String title, String text, String hidden);
    Article toArticle(ResultSetMetaData metaData, ResultSet resultSet) throws SQLException;
    List<Article> findAll();
    List<Article> findAllDisplayable();
    List<ArticleRepositoryImpl.FrontArticle> findAllFront();
    List<ArticleRepositoryImpl.FrontArticle> findAllDisplayableFront();
    List<Article> findFromUser(User user);
    void changeDisplay(Long articleId, boolean show);
    Article find(Long id);
}
