package ru.itmo.webmail.model.service;

import ru.itmo.webmail.model.domain.News;
import ru.itmo.webmail.model.domain.User;
import ru.itmo.webmail.model.exception.ValidationException;
import ru.itmo.webmail.model.repository.NewsRepository;
import ru.itmo.webmail.model.repository.impl.NewsRepositoryImpl;

import java.util.Deque;
import java.util.List;

public class NewsService {
    private NewsRepository newsRepository = new NewsRepositoryImpl();

    public void validateNews(String text, User user) throws ValidationException {
        if (user == null) {
            throw new ValidationException("You have to enter first");
        }
        if (text == null || text.equals("")) {
            throw new ValidationException("The form is empty");
        }
    }

    public void save(String text, int id){
        newsRepository.save(new News(id, text));
    }

    public Deque<News> findAll() {
        return newsRepository.findAll();
    }
}
