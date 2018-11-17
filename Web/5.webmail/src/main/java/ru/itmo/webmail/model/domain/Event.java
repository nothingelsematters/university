package ru.itmo.webmail.model.domain;

import java.util.Date;

public class Event {
    private EventEnum type;
    private Long id;
    private Long userId;
    private Date creationTime;

    public EventEnum getType() {
        return type;
    }

    public void setType(EventEnum type) {
        this.type = type;
    }

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public Long getUserId() {
        return userId;
    }

    public void setUserId(Long userId) {
        this.userId = userId;
    }

    public Date getCreationTime() {
        return creationTime;
    }

    public void setCreationTime(Date creationTime) {
        this.creationTime = creationTime;
    }

    public static enum EventEnum {
        Enter, Logout
    }
}
