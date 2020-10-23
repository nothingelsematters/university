package com.github.nothingelsematters.sd.refactoring.mapper;

import static org.junit.Assert.assertEquals;

import com.github.nothingelsematters.sd.refactoring.entity.Product;
import org.junit.Test;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

public class ProductMapperTest {

    private final Mapper<Product> productMapper = new ProductMapper();

    @Test
    public void correctnessTest() {
        List<Product> productList = Arrays.asList(new Product("expensive", 1000L), new Product("cheap", 1L));

        List<Map<String, Object>> stringObjectMapList = productList.stream()
            .map(product -> {
                Map<String, Object> stringObjectMap = new HashMap<>();
                stringObjectMap.put("name", product.getName());
                stringObjectMap.put("price", product.getPrice());
                return stringObjectMap;
            }).collect(Collectors.toList());

        assertEquals(productList, stringObjectMapList.stream().map(productMapper::map).collect(Collectors.toList()));
    }

    @Test(expected = MapperException.class)
    public void lostFieldShouldThrowExceptionTest() {
        productMapper.map(Collections.singletonMap("name", "name"));
    }

    @Test(expected = MapperException.class)
    public void wrongTypeShouldThrowExceptionTest() {
        Map<String, Object> stringObjectMap = new HashMap<>();
        stringObjectMap.put("name", "name");
        stringObjectMap.put("price", 10);
        productMapper.map(stringObjectMap);
    }
}
