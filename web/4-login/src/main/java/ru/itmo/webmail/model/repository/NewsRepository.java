package ru.itmo.webmail.model.repository;

import ru.itmo.webmail.model.domain.News;

import java.util.Deque;

public interface NewsRepository {
    void save(News news);
    Deque<News> findAll();
}
