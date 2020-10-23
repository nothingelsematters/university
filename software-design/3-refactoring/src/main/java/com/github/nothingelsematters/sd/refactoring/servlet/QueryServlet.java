package com.github.nothingelsematters.sd.refactoring.servlet;

import com.github.nothingelsematters.sd.refactoring.dao.ProductDao;
import com.github.nothingelsematters.sd.refactoring.entity.Product;
import com.github.nothingelsematters.sd.refactoring.http.HttpFormatter;

import javax.servlet.http.HttpServletRequest;
import java.io.PrintWriter;
import java.util.Collections;
import java.util.Optional;

public class QueryServlet extends AbstractServlet {

    public QueryServlet(HttpFormatter httpFormatter, ProductDao productDao) {
        super(httpFormatter, productDao);
    }

    @Override
    protected void doGet(HttpServletRequest request, PrintWriter writer) {
        String command = getParameter(request, writer, "command");
        String content;

        switch (command) {
            case "max":
                content = productListPage("Product with max price: ", productDao.getMostExpensive());
                break;
            case "min":
                content = productListPage("Product with min price: ", productDao.getLessExpensive());
                break;
            case "sum":
                content = httpFormatter.contentPage("Summary price: " + productDao.getPriceSum());
                break;
            case "count":
                content = httpFormatter.contentPage("Number of products: " + productDao.getAmount());
                break;
            default:
                content = httpFormatter.errorPage("Unknown command: " + command);
        }

        writer.println(content);
    }

    private String productListPage(String header, Optional<Product> productSupplier) {
        String content = productSupplier.map(product -> httpFormatter.productList(Collections.singletonList(product)))
            .orElse("No products found");

        return httpFormatter.headerContent(header, content);
    }
}
