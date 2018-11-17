package ru.itmo.webmail.model.repository.impl;

import ru.itmo.webmail.model.database.DatabaseUtils;
import ru.itmo.webmail.model.domain.User;
import ru.itmo.webmail.model.exception.RepositoryException;
import ru.itmo.webmail.model.repository.UserRepository;
import ru.itmo.webmail.model.service.EmailConfirmationService;

import java.sql.*;
import java.util.ArrayList;
import java.util.List;

public class UserRepositoryImpl implements UserRepository {
    private EmailConfirmationService emailConfirmationService = new EmailConfirmationService();

    @Override
    public User findById(Long id) {
        return getUserFromRequest("SELECT * FROM User WHERE id=?", Long.toString(id));
    }

    @Override
    public User findByLoginOrEmail(String loginOrEmail) {
        return getUserFromRequest("SELECT * FROM User WHERE login=? OR  email=?", loginOrEmail, loginOrEmail);
    }

    @Override
    public User findByEmail(String email) {
        return getUserFromRequest("SELECT * FROM User WHERE email=?", email);
    }

    @Override
    public User findByLoginOrEmailAndPasswordSha(String loginOrEmail, String passwordSha) {
        return getUserFromRequest("SELECT * FROM User WHERE (login=? OR email=?) AND passwordSha=?",
                loginOrEmail, loginOrEmail, passwordSha);
    }

    private User getUserFromRequest(String dbrequest, String... args) {
        return (User) DatabaseUtils.performRequest(
                dbrequest,
                (PreparedStatement statement) -> {
                    try (ResultSet resultSet = statement.executeQuery()) {
                        if (resultSet.next()) {
                            return toUser(statement.getMetaData(), resultSet);
                        }
                    } catch (SQLException e) {
                        throw new RepositoryException("Can't find User.", e);
                    }
                    return null;
                }, args);

    }

    @Override
    public List<User> findAll() {
        List<User> users = new ArrayList<>();
        DatabaseUtils.performRequest(
                "SELECT * FROM User ORDER BY id",
                (PreparedStatement statement) -> {
                    try (ResultSet result = statement.executeQuery()) {
                        while (result.next()) {
                            users.add(toUser(statement.getMetaData(), result));
                        }
                    } catch (SQLException e) {
                        throw new RepositoryException("Can't find all Users.", e);
                    }
                    return null;
                });
        return users;
    }

    @SuppressWarnings("StatementWithEmptyBody")
    private User toUser(ResultSetMetaData metaData, ResultSet resultSet) throws SQLException {
        User user = new User();
        for (int i = 1; i <= metaData.getColumnCount(); i++) {
            String columnName = metaData.getColumnName(i);
            if ("id".equalsIgnoreCase(columnName)) {
                user.setId(resultSet.getLong(i));
            } else if ("login".equalsIgnoreCase(columnName)) {
                user.setLogin(resultSet.getString(i));
            } else if ("passwordSha".equalsIgnoreCase(columnName)) {
                // No operations.
            } else if ("creationTime".equalsIgnoreCase(columnName)) {
                user.setCreationTime(resultSet.getTimestamp(i));
            } else if ("email".equalsIgnoreCase(columnName)) {
                user.setEmail(resultSet.getString(i));
            } else if ("confirmed".equalsIgnoreCase(columnName)) {
                user.setConfirmed(resultSet.getBoolean(i));
            } else {
                throw new RepositoryException("Unexpected column 'User." + columnName + "'.");
            }
        }
        return user;
    }

    @Override
    public void save(User user, String email, String passwordSha) {
        user = (User) DatabaseUtils.performRequest(
                "INSERT INTO User (login, passwordSha, email, creationTime, confirmed) VALUES (?, ?, ?, NOW(), FALSE)",
                (PreparedStatement statement) -> {
            User returnUser = null;
            try {
                if (statement.executeUpdate() == 1) {
                    ResultSet generatedIdResultSet = statement.getGeneratedKeys();
                    if (generatedIdResultSet.next()) {
                        returnUser = findByEmail(email);
                    }
                }
            } catch (SQLException e) {
                throw new RepositoryException("Can't save User.", e);
            }
            if (returnUser == null) {
                throw new RepositoryException("Can't save User.");
            } else {
                return returnUser;
            }
        }, user.getLogin(), passwordSha, email);
        emailConfirmationService.emailToConfirm(user);
    }
}
