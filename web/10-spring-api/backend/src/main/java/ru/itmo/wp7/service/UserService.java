package ru.itmo.wp7.service;

import org.springframework.stereotype.Service;
import ru.itmo.wp7.domain.User;
import ru.itmo.wp7.form.UserCredentials;
import ru.itmo.wp7.repository.UserRepository;

import java.util.List;
import java.util.Optional;

@Service
public class UserService {
    private final UserRepository userRepository;

    public UserService(UserRepository userRepository) {
        this.userRepository = userRepository;
    }

    public int countByLogin(String login) {
        return userRepository.countByLogin(login);
    }

    public Optional<User> findByLoginAndPassword(String login, String password) {
        return userRepository.findByLoginAndPassword(login, password);
    }

    public List<User> findAll() {
        return userRepository.findAll();
    }

    public User register(UserCredentials registerFrom) {
        User user = new User(registerFrom.getLogin(), registerFrom.getName(), false);
        userRepository.save(user);
        userRepository.updatePassword(user.getId(), registerFrom.getPassword());
        return user;
    }

    public Optional<User> findById(Long id) {
        return userRepository.findById(id);
    }
}
