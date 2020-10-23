package com.github.nothingelsematters.sd.refactoring.servlet;

import com.github.nothingelsematters.sd.refactoring.dao.ProductDao;
import com.github.nothingelsematters.sd.refactoring.http.HttpFormatter;

import javax.servlet.http.HttpServletRequest;
import java.io.PrintWriter;

public class GetProductsServlet extends AbstractServlet {

    public GetProductsServlet(HttpFormatter httpFormatter, ProductDao productDao) {
        super(httpFormatter, productDao);
    }

    @Override
    protected void doGet(HttpServletRequest request, PrintWriter writer) {
        writer.println(httpFormatter.contentPage(httpFormatter.productList(productDao.getAll())));
    }
}
