package ru.itmo.webmail.model.repository;

import ru.itmo.webmail.model.domain.Talk;

import java.util.List;

public interface TalkRepository {
    List<Talk> findAllForUser(Long id);
    void save(Talk talk);
}
