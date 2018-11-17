package ru.itmo.webmail.model.repository.impl;

import ru.itmo.webmail.model.database.DatabaseUtils;
import ru.itmo.webmail.model.domain.Article;
import ru.itmo.webmail.model.domain.User;
import ru.itmo.webmail.model.exception.RepositoryException;
import ru.itmo.webmail.model.repository.ArticleRepository;
import ru.itmo.webmail.model.service.UserService;

import javax.sql.DataSource;
import java.sql.*;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

public class ArticleRepositoryImpl implements ArticleRepository {
    private static final DataSource DATA_SOURCE = DatabaseUtils.getDataSource();

    private static final UserService userService = new UserService();

    @Override
    public void addArticle(User user, String title, String text, String hidden) {
        try (Connection connection = DATA_SOURCE.getConnection()) {
            try (PreparedStatement statement = connection.prepareStatement(
                    "INSERT INTO Article (userId, title, text, hidden, creationTime) VALUES (?, ?, ?, ?, NOW())",
                    Statement.RETURN_GENERATED_KEYS)) {
                statement.setString(1, Long.toString(user.getId()));
                statement.setString(2, title);
                statement.setString(3, text);
                statement.setString(4, hidden);
                if (statement.executeUpdate() != 1) {
                    throw new RepositoryException("Can't save User.");
                }
            }
        } catch (SQLException e) {
            throw new RepositoryException("Can't save User.", e);
        }
    }

    @Override
    public List<Article> findAll() {
        List<Article> result = new ArrayList<>();
        try (Connection connection = DATA_SOURCE.getConnection()) {
            try (PreparedStatement statement = connection.prepareStatement(
                    "SELECT * FROM Article ORDER BY CreationTime DESC",
                    Statement.RETURN_GENERATED_KEYS)) {
                try (ResultSet resultSet = statement.executeQuery()) {
                    while (resultSet.next()) {
                        result.add(toArticle(statement.getMetaData(), resultSet));
                    }
                }
            }
        } catch (SQLException e) {
            throw new RepositoryException("Can't save User.", e);
        }

        return result;
    }

    @Override
    public List<Article> findAllDisplayable() {
        List<Article> result = new ArrayList<>();
        try (Connection connection = DATA_SOURCE.getConnection()) {
            try (PreparedStatement statement = connection.prepareStatement(
                    "SELECT * FROM Article WHERE hidden=? ORDER BY CreationTime DESC",
                    Statement.RETURN_GENERATED_KEYS)) {
                statement.setString(1, Boolean.toString(false));
                try (ResultSet resultSet = statement.executeQuery()) {
                    while (resultSet.next()) {
                        result.add(toArticle(statement.getMetaData(), resultSet));
                    }
                }
            }
        } catch (SQLException e) {
            throw new RepositoryException("Can't save User.", e);
        }

        return result;
    }

    @SuppressWarnings("StatementWithEmptyBody")
    public Article toArticle(ResultSetMetaData metaData, ResultSet resultSet) throws SQLException {
        Article article = new Article();
        for (int i = 1; i <= metaData.getColumnCount(); i++) {
            String columnName = metaData.getColumnName(i);
            if ("id".equalsIgnoreCase(columnName)) {
                article.setId(resultSet.getLong(i));
            } else if ("userId".equalsIgnoreCase(columnName)) {
                article.setUserId(resultSet.getLong(i));
            } else if ("creationTime".equalsIgnoreCase(columnName)) {
                article.setCreationTime(resultSet.getTimestamp(i));
            } else if ("title".equalsIgnoreCase(columnName)) {
                article.setTitle(resultSet.getString(i));
            } else if ("text".equalsIgnoreCase(columnName)) {
                article.setText(resultSet.getString(i));
            } else if ("hidden".equalsIgnoreCase(columnName)) {
                article.setHidden(resultSet.getBoolean(i));
            } else {
                throw new RepositoryException("Unexpected column 'User." + columnName + "'.");
            }
        }
        return article;
    }

    @Override
    public List<FrontArticle> findAllFront() {
        List<FrontArticle> result = new ArrayList<>();
        for (Article article: findAll()) {
            result.add(new FrontArticle(article));
        }
        return result;
    }

    public List<FrontArticle> findAllDisplayableFront() {
        List<FrontArticle> result = new ArrayList<>();
        for (Article article: findAllDisplayable()) {
            result.add(new FrontArticle(article));
        }
        return result;
    }

    @Override
    public List<Article> findFromUser(User user) {
        List<Article> result = new ArrayList<>();
        try (Connection connection = DATA_SOURCE.getConnection()) {
            try (PreparedStatement statement = connection.prepareStatement(
                    "SELECT * FROM Article WHERE userId=? ORDER BY CreationTime DESC",
                    Statement.RETURN_GENERATED_KEYS)) {
                statement.setString(1, Long.toString(user.getId()));
                try (ResultSet resultSet = statement.executeQuery()) {
                    while (resultSet.next()) {
                        result.add(toArticle(statement.getMetaData(), resultSet));
                    }
                }
            }
        } catch (SQLException e) {
            throw new RepositoryException("Can't save User.", e);
        }

        return result;
    }

    @Override
    public void changeDisplay(Long articleId, boolean show) {
        try (Connection connection = DATA_SOURCE.getConnection()) {
            try (PreparedStatement statement = connection.prepareStatement(
                    "UPDATE Article SET hidden=? WHERE id=?",
                    Statement.RETURN_GENERATED_KEYS)) {
                statement.setString(1, Integer.toString(show ? 0 : 1));
                statement.setString(2, Long.toString(articleId));
                statement.execute();
            }
        } catch (SQLException e) {
            throw new RepositoryException("Can't save User.", e);
        }
    }

    @Override
    public Article find(Long id) {
        try (Connection connection = DATA_SOURCE.getConnection()) {
            try (PreparedStatement statement = connection.prepareStatement(
                    "SELECT * FROM Article WHERE id=?",
                    Statement.RETURN_GENERATED_KEYS)) {
                statement.setString(1, Long.toString(id));
                try (ResultSet resultSet = statement.executeQuery()) {
                    if (resultSet.next()) {
                        return toArticle(statement.getMetaData(), resultSet);
                    } else {
                        return null;
                    }
                }
            }
        } catch (SQLException e) {
            throw new RepositoryException("Can't save User.", e);
        }
    }

    public static class FrontArticle {
        private String text;
        private String title;
        private String userName;
        private Date creationTime;

        FrontArticle(Article article) {
            this.text = article.getText();
            this.title = article.getTitle();
            this.creationTime = article.getCreationTime();
            this.userName = userService.find(article.getUserId()).getLogin();
        }

        public String getText() {
            return text;
        }

        public void setText(String text) {
            this.text = text;
        }

        public String getTitle() {
            return title;
        }

        public void setTitle(String title) {
            this.title = title;
        }

        public String getUserName() {
            return userName;
        }

        public void setUserName(String userName) {
            this.userName = userName;
        }

        public Date getCreationTime() {
            return creationTime;
        }

        public void setCreationTime(Date creationTime) {
            this.creationTime = creationTime;
        }
    }
}
