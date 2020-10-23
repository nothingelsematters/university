package com.github.nothingelsematters.sd.refactoring.sql;

import static java.lang.String.format;
import static org.junit.Assert.assertEquals;

import org.junit.Test;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

public class SqlRequestServiceTest {

    private final SqlRequestService sqlRequestService = new SqlRequestServiceImpl("jdbc:sqlite:test.db");

    @Test
    public void executeQueryTest() {
        String tableName = "FIRST_TABLE";
        int intValue = 1;
        String stringValue = "string";
        String intColumn = "x";
        String stringColumn = "y";

        sqlRequestService.executeUpdate(format("DROP TABLE IF EXISTS %s", tableName));

        sqlRequestService.executeUpdate(format(
            "CREATE TABLE %s (%s int primary key, %s varchar(255))",
            tableName,
            intColumn,
            stringColumn
        ));
        sqlRequestService.executeUpdate(format(
            "INSERT INTO %s (%s, %s) VALUES (%s, '%s')",
            tableName,
            intColumn,
            stringColumn,
            intValue,
            stringValue
        ));

        Map<String, Object> valuesMap = new HashMap<>();
        valuesMap.put(stringColumn, stringValue);
        valuesMap.put(intColumn, intValue);

        assertEquals(
            Collections.singletonList(valuesMap),
            sqlRequestService.executeQuery(format("SELECT * FROM %s", tableName))
        );
    }
}
