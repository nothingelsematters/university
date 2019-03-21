package ru.itmo.webmail.model.repository.impl;

import ru.itmo.webmail.model.domain.News;
import ru.itmo.webmail.model.repository.NewsRepository;

import java.io.*;
import java.util.Deque;
import java.util.LinkedList;

public class NewsRepositoryImpl implements NewsRepository {
    private static final File tmpDir = new File(System.getProperty("java.io.tmpdir"));
    private Deque<News> news;

    public NewsRepositoryImpl() {
        try {
            //noinspection unchecked
            news = (Deque<News>) new ObjectInputStream(
                    new FileInputStream(new File(tmpDir, getClass().getSimpleName()))).readObject();
        } catch (Exception e) {
            news = new LinkedList<>();
        }
    }

    @Override
    public Deque<News> findAll() {
        return new LinkedList<>(news);
    }

    @Override
    public void save(News fresh_news) {
        news.addFirst(fresh_news);

        try {
            ObjectOutputStream objectOutputStream = new ObjectOutputStream(
                    new FileOutputStream(new File(tmpDir, getClass().getSimpleName())));
            objectOutputStream.writeObject(news);
            objectOutputStream.close();
        } catch (Exception e) {
            throw new RuntimeException("Can't save news.", e);
        }
    }
}
