package ru.itmo.wp.servlet;

import com.google.gson.Gson;

import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.*;
import java.util.ArrayList;

public class MessageServlet extends HttpServlet {

    private ArrayList<Message> messages = new ArrayList<>();

    private class Message {
        private String user;
        private String text;

        private Message(String user, String text) {
            this.user = user;
            this.text = text;
        }
    }

    @Override
    protected void doPost(HttpServletRequest request, HttpServletResponse response)
            throws IOException {
        OutputStream outputStream = response.getOutputStream();
        String[] parts = request.getRequestURI().split("/");

        switch (parts[parts.length - 1]) {
            case ("auth"):
                String user = request.getParameter("user");

                response.setContentType("application/json");
                if (user == null) {
                    user = "";
                } else {
                    request.getSession().setAttribute("user", user);
                }
                outputStream.write(new Gson().toJson(user).getBytes());
                break;

            case ("findAll"):
                response.setContentType("application/json");
                outputStream.write(new Gson().toJson(messages).getBytes());
                break;

            case ("add"):
                messages.add(new Message((String) request.getSession().getAttribute("user"),
                        request.getParameter("text")));
                break;

            default:
                response.sendError(HttpServletResponse.SC_BAD_REQUEST);
                return;
        }

        outputStream.flush();
    }
}
