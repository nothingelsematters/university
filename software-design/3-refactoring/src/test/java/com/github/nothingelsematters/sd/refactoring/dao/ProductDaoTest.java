package com.github.nothingelsematters.sd.refactoring.dao;

import static org.junit.Assert.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.github.nothingelsematters.sd.refactoring.entity.Product;
import com.github.nothingelsematters.sd.refactoring.mapper.ProductMapper;
import com.github.nothingelsematters.sd.refactoring.sql.SqlRequestService;
import org.junit.Before;
import org.junit.Test;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.function.Function;

public class ProductDaoTest {

    private SqlRequestService sqlRequestService;
    private ProductMapper productMapper;
    private ProductDao productDao;

    @Before
    public void setUp() {
        sqlRequestService = mock(SqlRequestService.class);
        productMapper = mock(ProductMapper.class);
        productDao = new ProductDaoImpl(sqlRequestService, productMapper);
    }

    @Test
    public void getAllCorrectnessTest() {
        Map<String, Object> firstMap = Collections.singletonMap("a", null);
        Map<String, Object> secondMap = Collections.singletonMap("b", null);

        Product expensiveProduct = new Product("expensive", 10000L);
        Product cheapProduct = new Product("cheap", 1L);

        List<Map<String, Object>> productListMap = Arrays.asList(firstMap, secondMap);
        List<Product> productList = Arrays.asList(expensiveProduct, cheapProduct);

        when(sqlRequestService.executeQuery(any())).thenReturn(productListMap);
        when(productMapper.map(firstMap)).thenReturn(expensiveProduct);
        when(productMapper.map(secondMap)).thenReturn(cheapProduct);

        assertEquals(productList, productDao.getAll());
    }

    @Test
    public void getMostExpensiveTest() {
        optionalProductGetterTest(ProductDao::getMostExpensive);
    }

    @Test
    public void getLessExpensiveTest() {
        optionalProductGetterTest(ProductDao::getLessExpensive);
    }

    @Test
    public void getMostExpensiveFromEmptyDatabaseTest() {
        optionalProductGetterEmptyTest(ProductDao::getMostExpensive);
    }

    @Test
    public void getLessExpensiveFromEmptyDatabaseTest() {
        optionalProductGetterEmptyTest(ProductDao::getLessExpensive);
    }

    @Test
    public void getSumTest() {
        singleIntegerGetterTest(ProductDao::getPriceSum);
    }

    @Test
    public void getAmountTest() {
        singleIntegerGetterTest(ProductDao::getAmount);
    }

    @Test
    public void insertQueryTest() {
        productDao.insert(new Product("name", 100));
        doAnswer(invocationOnMock -> {
            String request = invocationOnMock.getArgument(0);
            assertEqualsCaseAndSpaceInsensitive("insert into product (name, price) values ('name', 100)", request);
            return null;
        }).when(sqlRequestService).executeQuery(any());
    }

    private void optionalProductGetterTest(Function<ProductDao, Optional<Product>> optionalProductGetter) {
        Map<String, Object> firstMap = Collections.singletonMap("a", null);

        Product expensiveProduct = new Product("expensive", 10000L);

        List<Map<String, Object>> productListMap = Collections.singletonList(firstMap);

        when(sqlRequestService.executeQuery(any())).thenReturn(productListMap);
        when(productMapper.map(firstMap)).thenReturn(expensiveProduct);

        assertEquals(Optional.of(expensiveProduct), optionalProductGetter.apply(productDao));
    }

    private void optionalProductGetterEmptyTest(Function<ProductDao, Optional<Product>> optionalProductGetter) {
        when(sqlRequestService.executeQuery(any())).thenReturn(Collections.emptyList());
        assertEquals(Optional.empty(), optionalProductGetter.apply(productDao));
    }

    private void singleIntegerGetterTest(Function<ProductDao, Integer> integerGetter) {
        Integer foundInteger = 100;
        when(sqlRequestService.executeQuery(any()))
            .thenReturn(Collections.singletonList(Collections.singletonMap("some name", foundInteger)));
        assertEquals(foundInteger, integerGetter.apply(productDao));
    }

    private void assertEqualsCaseAndSpaceInsensitive(String expected, String actual) {
        assertEquals(lowSolidString(expected), lowSolidString(actual));
    }

    private String lowSolidString(String input) {
        return input.replace(" ", "").toLowerCase();
    }
}
