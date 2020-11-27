package com.github.nothingelsematters.sd.refactoring.servlet;

import static org.junit.Assert.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import com.github.nothingelsematters.sd.refactoring.dao.ProductDao;
import com.github.nothingelsematters.sd.refactoring.http.HttpFormatter;
import org.junit.Before;
import org.junit.Test;

import javax.servlet.http.HttpServletRequest;
import java.io.PrintWriter;

public class QueryServletTest {

    private HttpFormatter httpFormatter;
    private ProductDao productDao;
    private QueryServlet queryServlet;

    @Before
    public void setUp() {
        httpFormatter = mock(HttpFormatter.class);
        productDao = mock(ProductDao.class);
        queryServlet = new QueryServlet(httpFormatter, productDao);
    }

    @Test
    public void servletReturnsResultFromDaoTest() {
        String result = "result";

        HttpServletRequest httpServletRequest = mock(HttpServletRequest.class);
        when(httpServletRequest.getParameter(any())).thenReturn("sum");

        PrintWriter printWriter = mock(PrintWriter.class);
        doAnswer(invocationOnMock -> {
            assertTrue(invocationOnMock.getArgument(0).toString().contains(result));
            return null;
        }).when(printWriter).println(any(String.class));

        when(httpFormatter.contentPage(any())).thenReturn(result);

        when(productDao.getPriceSum()).thenReturn(10);

        queryServlet.doGet(httpServletRequest, printWriter);
    }
}
