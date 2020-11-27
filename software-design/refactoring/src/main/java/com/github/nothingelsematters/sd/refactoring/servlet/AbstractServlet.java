package com.github.nothingelsematters.sd.refactoring.servlet;

import com.github.nothingelsematters.sd.refactoring.dao.ProductDao;
import com.github.nothingelsematters.sd.refactoring.http.HttpFormatter;

import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.io.PrintWriter;

public abstract class AbstractServlet extends HttpServlet {

    protected final HttpFormatter httpFormatter;

    protected final ProductDao productDao;

    public AbstractServlet(HttpFormatter httpFormatter, ProductDao productDao) {
        this.httpFormatter = httpFormatter;
        this.productDao = productDao;
    }

    abstract protected void doGet(HttpServletRequest request, PrintWriter writer);

    @Override
    protected final void doGet(HttpServletRequest request, HttpServletResponse response) throws IOException {
        doGet(request, response.getWriter());
        response.setContentType("text/html");
        response.setStatus(HttpServletResponse.SC_OK);
    }

    protected String getParameter(HttpServletRequest request, PrintWriter writer, String parameterName) {
        String parameter = request.getParameter(parameterName);
        if (parameter == null) {
            writer.println(httpFormatter.errorPage(String.format("No valid '%s' parameter given", parameterName)));
        }
        return parameter;
    }
}
