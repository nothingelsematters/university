package ru.itmo.webmail.model.repository.impl;

import ru.itmo.webmail.model.database.DatabaseUtils;
import ru.itmo.webmail.model.domain.EmailConfirmation;
import ru.itmo.webmail.model.domain.User;
import ru.itmo.webmail.model.exception.RepositoryException;
import ru.itmo.webmail.model.repository.EmailConfirmationRepository;

import java.sql.*;
import java.util.UUID;

public class EmailConfirmationRepositoryImpl implements EmailConfirmationRepository {
    private String getRandomString() {
        return UUID.randomUUID().toString();
    }

    private EmailConfirmation toEmailConfirmation(ResultSetMetaData metaData, ResultSet resultSet) throws SQLException {
        EmailConfirmation emailConfirmation = new EmailConfirmation();
        for (int i = 1; i <= metaData.getColumnCount(); i++) {
            String columnName = metaData.getColumnName(i);
            if ("id".equalsIgnoreCase(columnName)) {
                emailConfirmation.setId(resultSet.getLong(i));
            } else if ("userId".equalsIgnoreCase(columnName)) {
                emailConfirmation.setUserId(resultSet.getLong(i));
            } else if ("creationTime".equalsIgnoreCase(columnName)) {
                emailConfirmation.setCreationTime(resultSet.getTimestamp(i));
            } else if ("secret".equalsIgnoreCase(columnName)) {
                emailConfirmation.setSecret(resultSet.getString(i));
            } else {
                throw new RepositoryException("Unexpected column 'User." + columnName + "'.");
            }
        }
        return emailConfirmation;
    }

    private EmailConfirmation getEmailConfirmation(String dbrequest, String... args) {
        return (EmailConfirmation) DatabaseUtils.performRequest(
                dbrequest,
                (PreparedStatement statement) -> {
                    try (ResultSet resultSet = statement.executeQuery()) {
                        if (resultSet.next()) {
                            return toEmailConfirmation(statement.getMetaData(), resultSet);
                        }
                    } catch (SQLException e) {
                        throw new RepositoryException("Can't find User.", e);
                    }
                    return null;
                }, args);

    }

    @Override
    public void emailToConfirm(User user) {
        DatabaseUtils.executeRequest("INSERT INTO EmailConfirmation (userId, secret, creationTime) VALUES (?, ?, NOW())",
                "Can't save User.", Long.toString(user.getId()), getRandomString());
    }

    @Override
    public boolean confirmBySecret(String secret) {
        EmailConfirmation emailConfirmation = getEmailConfirmation(
                "SELECT * FROM EmailConfirmation WHERE secret=?", secret);
        if (emailConfirmation == null) {
            return false;
        }
        String userIdString = Long.toString(emailConfirmation.getUserId());
        DatabaseUtils.executeRequest("UPDATE User SET confirmed=TRUE WHERE id=?",
                "Can't find user to verify", userIdString);
        DatabaseUtils.executeRequest("DELETE FROM EmailConfirmation WHERE userId=?",
                "Can't find email confirmation to remove", userIdString);
        return true;
    }
}
