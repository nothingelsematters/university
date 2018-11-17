package ru.itmo.webmail.model.service;

import com.google.common.hash.Hashing;
import ru.itmo.webmail.model.domain.User;
import ru.itmo.webmail.model.exception.ValidationException;
import ru.itmo.webmail.model.repository.UserRepository;
import ru.itmo.webmail.model.repository.impl.UserRepositoryImpl;

import java.nio.charset.StandardCharsets;
import java.util.List;

public class UserService {
    private static final String USER_PASSWORD_SALT = "dc3475f2b301851b";

    private UserRepository userRepository = new UserRepositoryImpl();

    public User validateEntry(String userName, String password) throws ValidationException {
        User guest = userRepository.findByLogin(userName);
        if (guest == null) {
            guest = userRepository.findByEmail(userName);
        }

        if (guest == null) {
            throw new ValidationException("No such login/email");
        }
        if (!guest.getPasswordSha1().equals(makePassword(password))) {
            throw new ValidationException("Wrong password");
        }

        return guest;
    }

    public void validateRegistration(User user, String password,
                                     String confirmation, String email)
            throws ValidationException {

        if (user.getLogin() == null || user.getLogin().isEmpty()) {
            throw new ValidationException("Login is required");
        }
        if (!user.getLogin().matches("[a-z]+")) {
            throw new ValidationException("Login can contain only lowercase Latin letters");
        }
        if (user.getLogin().length() > 8) {
            throw new ValidationException("Login can't be longer than 8");
        }
        if (userRepository.findByLogin(user.getLogin()) != null) {
            throw new ValidationException("Login is already in use");
        }

        if (!email.contains("@") || !email.contains(".") ||
                email.charAt(0) == '@' ||
                email.lastIndexOf(".") - email.lastIndexOf("@") < 1 ||
                email.charAt(email.length() - 1) == '.' ||
                email.indexOf('@') != email.lastIndexOf('@')) {
            throw new ValidationException("Email is invalid");
        }
        if (userRepository.findByEmail(email) != null) {
            throw new ValidationException("Email is already in use");
        }

        if (password == null || password.isEmpty()) {
            throw new ValidationException("Password is required");
        }
        if (password.length() < 4) {
            throw new ValidationException("Password can't be shorter than 4");
        }
        if (password.length() > 32) {
            throw new ValidationException("Password can't be longer than 32");
        }

        if (!password.equals(confirmation)) {
            throw new ValidationException("Those passwords didn't match");
        }
    }

    private String makePassword(String password) {
        return Hashing.sha256().hashString(USER_PASSWORD_SALT + password,
                StandardCharsets.UTF_8).toString();
    }

    public void register(User user, String password, String email) {
        user.setPasswordSha1(makePassword(password));
        user.setId(findCount() + 1);
        user.setEmail(email);
        userRepository.save(user);
    }

    public List<User> findAll() {
        return userRepository.findAll();
    }

    public int findCount() {
        return userRepository.findCount();
    }

    public User findById(int id) {
        return userRepository.findById(id);
    }
}
