package ru.itmo.wp.servlet;

import javax.servlet.http.*;
import java.io.*;
import java.nio.file.Files;

public class StaticServlet extends HttpServlet {
    private void writeFile(File file, HttpServletResponse response) throws IOException {
        OutputStream outputStream = response.getOutputStream();
        Files.copy(file.toPath(), outputStream);
        outputStream.flush();
    }

    private File getFile(String uri) {
        File file = new File("./src/main/webapp/static/" + uri);
        if (!file.isFile()) {
            return new File(getServletContext().getRealPath("/static" + "/" + uri));
        }
        return file;
    }

    @Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response)
            throws IOException {
        String[] fileNames = request.getRequestURI().split("\\+");
        response.setContentType(getContentTypeFromName(fileNames[0]));

        for (String fileName: fileNames) {
            File file = getFile(fileName);

            if (file.isFile()) {
                writeFile(file, response);
            } else {
                 response.sendError(HttpServletResponse.SC_NOT_FOUND);
                 break;
            }
        }
    }

    private String getContentTypeFromName(String name) {
        name = name.toLowerCase();

        if (name.endsWith(".png")) {
            return "image/png";
        }

        if (name.endsWith(".jpg")) {
            return "image/jpeg";
        }

        if (name.endsWith(".html")) {
            return "text/html";
        }

        if (name.endsWith(".css")) {
            return "text/css";
        }

        if (name.endsWith(".js")) {
            return "application/javascript";
        }

        throw new IllegalArgumentException("Can't find content type for '" + name + "'.");
    }
}
