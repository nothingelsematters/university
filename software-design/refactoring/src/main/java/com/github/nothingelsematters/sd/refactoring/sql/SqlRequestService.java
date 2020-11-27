package com.github.nothingelsematters.sd.refactoring.sql;

import java.util.List;
import java.util.Map;

public interface SqlRequestService {

    List<Map<String, Object>> executeQuery(String request);

    void executeUpdate(String request);
}
