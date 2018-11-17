package ru.itmo.webmail.model.service;

import ru.itmo.webmail.model.domain.User;
import ru.itmo.webmail.model.repository.EmailConfirmationRepository;
import ru.itmo.webmail.model.repository.impl.EmailConfirmationRepositoryImpl;

public class EmailConfirmationService {
    public EmailConfirmationRepository emailConfirmationRepository = new EmailConfirmationRepositoryImpl();

    public void emailToConfirm(User user) {
        emailConfirmationRepository.emailToConfirm(user);
    }

    public boolean confirmBySecret(String secret) {
        return emailConfirmationRepository.confirmBySecret(secret);
    }
}
