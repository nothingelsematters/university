package ru.itmo.webmail.model.database;

import org.mariadb.jdbc.MariaDbDataSource;
import ru.itmo.webmail.model.exception.RepositoryException;

import javax.sql.DataSource;
import java.io.IOException;
import java.sql.*;
import java.util.Properties;
import java.util.function.Function;

public class DatabaseUtils {
    public static DataSource getDataSource() {
        return DataSourceHolder.INSTANCE;
    }

    public static void executeRequest(String sqlRequest, String errorMessage, String... args) {
        performRequest(sqlRequest, (PreparedStatement statement) -> {
            try {
                return statement.execute();
            } catch (SQLException e) {
                throw new RepositoryException(errorMessage, e);
            }
        }, args);
    }

    public static Object performRequest(String sqlRequest, Function<PreparedStatement, Object> func, String... args) {
        try (Connection connection = getDataSource().getConnection()) {
            try (PreparedStatement statement = connection.prepareStatement(sqlRequest, Statement.RETURN_GENERATED_KEYS)) {
                int temp = 1;
                for (String a: args) {
                    statement.setString(temp++, a);
                }
                return func.apply(statement);
            }
        } catch (SQLException e) {
            throw new RepositoryException("Can't process database request: " + sqlRequest, e);
        }
    }

    private static final class DataSourceHolder {
        private static final DataSource INSTANCE;
        private static final Properties PROPERTIES = new Properties();

        static {
            try {
                PROPERTIES.load(DataSourceHolder.class.getResourceAsStream("/application.properties"));
            } catch (IOException e) {
                throw new RuntimeException("Can't load application.properties.", e);
            }

            try {
                MariaDbDataSource dataSource = new MariaDbDataSource();
                dataSource.setUrl(PROPERTIES.getProperty("database.url"));
                dataSource.setUser(PROPERTIES.getProperty("database.user"));
                dataSource.setPassword(PROPERTIES.getProperty("database.password"));
                INSTANCE = dataSource;
            } catch (SQLException e) {
                throw new RuntimeException("Can't initialize DB.", e);
            }

            try (Connection connection = INSTANCE.getConnection()) {
                if (connection == null) {
                    throw new RuntimeException("Can't get testing connection from DB.");
                }
            } catch (SQLException e) {
                throw new RuntimeException("Can't get testing connection from DB.", e);
            }
        }
    }
}
