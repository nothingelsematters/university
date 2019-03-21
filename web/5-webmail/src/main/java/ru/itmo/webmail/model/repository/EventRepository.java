package ru.itmo.webmail.model.repository;

import ru.itmo.webmail.model.domain.Event;
import ru.itmo.webmail.model.domain.User;

public interface EventRepository {
    void actionPerformed(Event.EventEnum type, User user);
}
