package com.github.nothingelsematters.sd.refactoring.mapper;

import java.util.Map;

public interface Mapper<T> {

    T map(Map<String, Object> stringObjectMap);
}
