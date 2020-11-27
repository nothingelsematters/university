package com.github.nothingelsematters.sd.refactoring.servlet;

import com.github.nothingelsematters.sd.refactoring.dao.ProductDao;
import com.github.nothingelsematters.sd.refactoring.entity.Product;
import com.github.nothingelsematters.sd.refactoring.http.HttpFormatter;

import javax.servlet.http.HttpServletRequest;
import java.io.PrintWriter;

public class AddProductServlet extends AbstractServlet {

    public AddProductServlet(HttpFormatter httpFormatter, ProductDao productDao) {
        super(httpFormatter, productDao);
    }

    @Override
    protected void doGet(HttpServletRequest request, PrintWriter writer) {
        String name = getParameter(request, writer, "name");
        String priceString = getParameter(request, writer, "price");

        long price;
        try {
            price = Long.parseLong(priceString);
        } catch (NumberFormatException e) {
            writer.println("Failed due to \"price\" parameter invalidity");
            return;
        }

        productDao.insert(new Product(name, price));
        writer.println("OK");
    }
}
