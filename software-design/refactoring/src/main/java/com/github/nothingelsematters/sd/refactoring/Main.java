package com.github.nothingelsematters.sd.refactoring;

import com.github.nothingelsematters.sd.refactoring.config.Configuration;
import com.github.nothingelsematters.sd.refactoring.config.ConfigurationImpl;
import com.github.nothingelsematters.sd.refactoring.dao.ProductDao;
import com.github.nothingelsematters.sd.refactoring.dao.ProductDaoImpl;
import com.github.nothingelsematters.sd.refactoring.http.HttpFormatter;
import com.github.nothingelsematters.sd.refactoring.http.HttpFormatterImpl;
import com.github.nothingelsematters.sd.refactoring.mapper.ProductMapper;
import com.github.nothingelsematters.sd.refactoring.servlet.AbstractServlet;
import com.github.nothingelsematters.sd.refactoring.sql.SqlRequestService;
import com.github.nothingelsematters.sd.refactoring.sql.SqlRequestServiceImpl;
import com.github.nothingelsematters.sd.refactoring.servlet.AddProductServlet;
import com.github.nothingelsematters.sd.refactoring.servlet.GetProductsServlet;
import com.github.nothingelsematters.sd.refactoring.servlet.QueryServlet;
import org.eclipse.jetty.server.Server;
import org.eclipse.jetty.servlet.ServletContextHandler;
import org.eclipse.jetty.servlet.ServletHolder;

public class Main {

    public static void main(String[] args) throws Exception {

        Configuration configuration = ConfigurationImpl.getInstance();

        SqlRequestService sqlRequestService = new SqlRequestServiceImpl(configuration.sqlUrl());

        ProductMapper productMapper = new ProductMapper();

        HttpFormatter httpFormatter = new HttpFormatterImpl();

        ProductDao productDao = new ProductDaoImpl(sqlRequestService, productMapper);
        productDao.initialize();

        Server server = new Server(8081);

        ServletContextHandler context = new ServletContextHandler(ServletContextHandler.SESSIONS);
        context.setContextPath("/");
        server.setHandler(context);

        registerServlet(context, new AddProductServlet(httpFormatter, productDao), "/add-product");
        registerServlet(context, new GetProductsServlet(httpFormatter, productDao), "/get-products");
        registerServlet(context, new QueryServlet(httpFormatter, productDao), "/query");

        server.start();
        server.join();
    }

    private static void registerServlet(ServletContextHandler contextHandler, AbstractServlet servlet, String url) {
        contextHandler.addServlet(new ServletHolder(servlet), url);
    }
}
