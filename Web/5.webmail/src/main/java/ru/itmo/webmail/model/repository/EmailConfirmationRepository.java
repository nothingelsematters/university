package ru.itmo.webmail.model.repository;

import ru.itmo.webmail.model.domain.User;

public interface EmailConfirmationRepository {
    void emailToConfirm(User user);
    boolean confirmBySecret(String secret);
}
