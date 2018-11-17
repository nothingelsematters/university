package ru.itmo.webmail.web;

import javax.servlet.FilterChain;
import javax.servlet.ServletException;
import javax.servlet.ServletOutputStream;
import javax.servlet.http.*;
import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;

public class StaticFilter extends HttpFilter {
    @Override
    protected void doFilter(HttpServletRequest request, HttpServletResponse response, FilterChain chain) throws IOException, ServletException {
        response.setCharacterEncoding(StandardCharsets.UTF_8.name());
        request.setCharacterEncoding("UTF-8");

        String uri = request.getRequestURI();
        File file = new File(getServletContext().getRealPath("."), "../../src/main/webapp" + uri);
        if (!file.isFile()) {
            file = new File(getServletContext().getRealPath(uri));
        }
        if (file.isFile()) {
            response.setContentType(getServletContext().getMimeType(file.getName()));
            ServletOutputStream outputStream = response.getOutputStream();
            Files.copy(file.toPath(), outputStream);
            outputStream.flush();
        } else {
            chain.doFilter(request, response);
        }
    }

    @Override
    public void destroy() {

    }
}
