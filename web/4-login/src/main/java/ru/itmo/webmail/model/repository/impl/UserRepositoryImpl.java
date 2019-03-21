package ru.itmo.webmail.model.repository.impl;

import ru.itmo.webmail.model.domain.User;
import ru.itmo.webmail.model.repository.UserRepository;

import java.io.*;
import java.util.ArrayList;
import java.util.List;

public class UserRepositoryImpl implements UserRepository {
    private static final File tmpDir = new File(System.getProperty("java.io.tmpdir"));

    private List<User> users;

    public UserRepositoryImpl() {
        try {
            //noinspection unchecked
            users = (List<User>) new ObjectInputStream(
                    new FileInputStream(new File(tmpDir, getClass().getSimpleName()))).readObject();
        } catch (Exception e) {
            users = new ArrayList<>();
        }
    }

    @Override
    public void save(User user) {
        users.add(user);

        try {
            ObjectOutputStream objectOutputStream = new ObjectOutputStream(
                    new FileOutputStream(new File(tmpDir, getClass().getSimpleName())));
            objectOutputStream.writeObject(users);
            objectOutputStream.close();
        } catch (Exception e) {
            throw new RuntimeException("Can't save user.", e);
        }
    }

    @Override
    public User findByLogin(String login) {
        return users.stream().filter(user -> user.getLogin().equals(login)).findFirst().orElse(null);
    }

    @Override
    public User findByEmail(String email) {
        return users.stream().filter(user -> user.getEmail().equals(email)).findFirst().orElse(null);
    }

    @Override
    public User findById(int userId) {
        return users.stream().filter(user -> user.getId() == userId).findFirst().orElse(null);
    }

    @Override
    public List<User> findAll() {
        return new ArrayList<>(users);
    }

    public int findCount() {
        return users.size();
    }
}
