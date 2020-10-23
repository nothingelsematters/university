package com.github.nothingelsematters.sd.refactoring.mapper;

import com.github.nothingelsematters.sd.refactoring.entity.Product;

import java.util.Map;

public class ProductMapper implements Mapper<Product> {

    @Override
    public Product map(Map<String, Object> stringObjectMap) {
        return new Product(
            getField(stringObjectMap, "name", String.class),
            getField(stringObjectMap, "price", Long.class)
        );
    }

    private static <T> T getField(Map<String, Object> stringObjectMap, String fieldName, Class<T> tClass) {
        Object field = stringObjectMap.get(fieldName);
        if (field == null || !field.getClass().equals(tClass)) {
            throw new MapperException(String.format("Product must contain valid \"%s\"", fieldName));
        }
        return tClass.cast(field);
    }
}
