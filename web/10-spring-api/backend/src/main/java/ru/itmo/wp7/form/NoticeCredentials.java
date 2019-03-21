package ru.itmo.wp7.form;

import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.Pattern;

public class NoticeCredentials {
    @NotEmpty
    private String text;

    @NotEmpty
    private String title;

    public String getTitle() {
        return title;
    }

    public void setTitle(String title) {
        this.title = title;
    }

    public String getText() {
        return text;
    }

    public void setText(String text) {
        this.text = text;
    }
}
