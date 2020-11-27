package com.github.nothingelsematters.sd.refactoring.servlet;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import com.github.nothingelsematters.sd.refactoring.dao.ProductDao;
import com.github.nothingelsematters.sd.refactoring.http.HttpFormatter;
import org.junit.Before;
import org.junit.Test;

import javax.servlet.http.HttpServletRequest;
import java.io.PrintWriter;

public class AddProductServletTest {

    private ProductDao productDao;
    private AddProductServlet addProductServlet;

    @Before
    public void setUp() {
        HttpFormatter httpFormatter = mock(HttpFormatter.class);
        productDao = mock(ProductDao.class);
        addProductServlet = new AddProductServlet(httpFormatter, productDao);
    }

    @Test
    public void servletProcessesTest() {
        HttpServletRequest httpServletRequest = mock(HttpServletRequest.class);
        when(httpServletRequest.getParameter("name")).thenReturn("name");
        when(httpServletRequest.getParameter("price")).thenReturn("10");

        doNothing().when(productDao).insert(any());

        when(productDao.getPriceSum()).thenReturn(10);

        PrintWriter printWriter = mock(PrintWriter.class);

        addProductServlet.doGet(httpServletRequest, printWriter);
    }
}
