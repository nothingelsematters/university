package ru.itmo.webmail.model.domain;

import ru.itmo.webmail.model.repository.UserRepository;
import ru.itmo.webmail.model.repository.impl.UserRepositoryImpl;
import ru.itmo.webmail.model.service.NewsService;
import ru.itmo.webmail.model.service.UserService;

public class FrontNews {
    private String text;
    private String userName;
    private static UserService userService = new UserService();

    public FrontNews(String text, String userName) {
        this.text = text;
        this.userName = userName;
    }

    public FrontNews(News news) {
        this.text = news.getText();
        this.userName = userService.findById(news.getUserId()).getLogin();
    }

    public String getText() {
        return text;
    }

    public void setText(String text) {
        this.text = text;
    }

    public String getUserName() {
        return userName;
    }

    public void setUserName(String userName) {
        this.userName = userName;
    }
}
