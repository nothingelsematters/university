package ru.itmo.webmail.model.domain;

import java.util.Date;

public class Talk {
    private Long id;
    private Long sourceUserId;
    private Long targetUserId;
    private String text;
    private Date creationTime;


    public Talk() {}

    public Talk(Long sourceUserId, Long targetUserId, String text) {
        this.sourceUserId = sourceUserId;
        this.targetUserId = targetUserId;
        this.text = text;
    }

    public Date getCreationTime() {
        return creationTime;
    }

    public void setCreationTime(Date creationTime) {
        this.creationTime = creationTime;
    }

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public Long getSourceUserId() {
        return sourceUserId;
    }

    public void setSourceUserId(Long sourceUserId) {
        this.sourceUserId = sourceUserId;
    }

    public Long getTargetUserId() {
        return targetUserId;
    }

    public void setTargetUserId(Long targetUserId) {
        this.targetUserId = targetUserId;
    }

    public String getText() {
        return text;
    }

    public void setText(String text) {
        this.text = text;
    }
}
