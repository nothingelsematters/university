package ru.itmo.webmail.model.repository.impl;

import ru.itmo.webmail.model.database.DatabaseUtils;
import ru.itmo.webmail.model.domain.Talk;
import ru.itmo.webmail.model.exception.RepositoryException;
import ru.itmo.webmail.model.repository.TalkRepository;

import java.sql.*;
import java.util.ArrayList;
import java.util.List;

public class TalkRepositoryImpl implements TalkRepository {
    @Override
    public List<Talk> findAllForUser(Long id) {
        List<Talk> returnTalks = new ArrayList<>();
        DatabaseUtils.performRequest(
                "SELECT * FROM Talk WHERE sourceUserId=? OR targetUserId=? ORDER BY creationTime",
                (PreparedStatement statement) -> {
                    try (ResultSet resultSet = statement.executeQuery()) {
                        while (resultSet.next()) {
                            returnTalks.add(toTalk(statement.getMetaData(), resultSet));
                        }
                    } catch (SQLException e) {
                        throw new RepositoryException("", e);
                    }
                    return returnTalks;
                }, Long.toString(id), Long.toString(id));
        return returnTalks;
    }

    @SuppressWarnings("StatementWithEmptyBody")
    private Talk toTalk(ResultSetMetaData metaData, ResultSet resultSet) throws SQLException {
        Talk talk = new Talk();
        for (int i = 1; i <= metaData.getColumnCount(); i++) {
            String columnName = metaData.getColumnName(i);
            if ("id".equalsIgnoreCase(columnName)) {
                talk.setId(resultSet.getLong(i));
            } else if ("sourceUserId".equalsIgnoreCase(columnName)) {
                talk.setSourceUserId(resultSet.getLong(i));
            } else if ("targetUserId".equalsIgnoreCase(columnName)) {
                talk.setTargetUserId(resultSet.getLong(i));
            } else if ("creationTime".equalsIgnoreCase(columnName)) {
                talk.setCreationTime(resultSet.getTimestamp(i));
            } else if ("text".equalsIgnoreCase(columnName)) {
                talk.setText(resultSet.getString(i));
            } else {
                throw new RepositoryException("Unexpected column 'Talk." + columnName + "'.");
            }
        }
        return talk;
    }

    @Override
    public void save(Talk talk) {
        DatabaseUtils.executeRequest(
                "INSERT INTO Talk (sourceUserId, targetUserId, text, creationTime) VALUES (?, ?, ?, NOW())",
                "Can't send a message.", Long.toString(talk.getSourceUserId()),
                Long.toString(talk.getTargetUserId()), talk.getText());
    }
}
