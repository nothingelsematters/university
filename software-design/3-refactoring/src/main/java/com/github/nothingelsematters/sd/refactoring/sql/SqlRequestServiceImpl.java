package com.github.nothingelsematters.sd.refactoring.sql;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class SqlRequestServiceImpl implements SqlRequestService {

    private final String jdbcUrl;

    private Connection connection = null;

    public SqlRequestServiceImpl(String jdbcUrl) {
        this.jdbcUrl = jdbcUrl;
    }

    @Override
    public List<Map<String, Object>> executeQuery(String request) {
        checkConnection();

        try (Statement statement = connection.createStatement()) {
            try (ResultSet resultSet = statement.executeQuery(request)) {

                List<Map<String, Object>> rows = new ArrayList<>();

                ResultSetMetaData resultSetMetaData = resultSet.getMetaData();
                int columnCount = resultSetMetaData.getColumnCount();

                while (resultSet.next()) {
                    Map<String, Object> row = new HashMap<>();
                    for (int i = 1; i <= columnCount; i++) {
                        row.put(resultSetMetaData.getColumnName(i).toLowerCase(), resultSet.getObject(i));
                    }
                    rows.add(row);
                }

                return rows;
            }

        } catch (SQLException e) {
            throw processError(e);
        }
    }

    @Override
    public void executeUpdate(String request) {
        checkConnection();

        try (Statement statement = connection.createStatement()) {
            statement.executeUpdate(request);
        } catch (SQLException e) {
            throw processError(e);
        }
    }

    private void checkConnection() {
        try {
            if (connection == null || connection.isClosed()) {
                connection = DriverManager.getConnection(jdbcUrl);
            }
        } catch (SQLException e) {
            throw processError(e);
        }
    }

    private RuntimeException processError(SQLException sqlException) {
        return new RuntimeException("Failed to get an SQL connection: " + sqlException.getMessage());
    }
}
