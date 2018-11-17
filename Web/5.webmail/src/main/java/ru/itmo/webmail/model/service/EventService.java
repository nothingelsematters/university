package ru.itmo.webmail.model.service;

import ru.itmo.webmail.model.domain.Event;
import ru.itmo.webmail.model.domain.User;
import ru.itmo.webmail.model.repository.EventRepository;
import ru.itmo.webmail.model.repository.impl.EventRepositoryImpl;

public class EventService {
    public EventRepository eventRepository = new EventRepositoryImpl();

    public void actionPerformed(Event.EventEnum type, User user) {
        eventRepository.actionPerformed(type, user);
    }
}
