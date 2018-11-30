package ru.itmo.wm4.form;

import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.Pattern;

public class NoticeCredentials {
    @NotEmpty
    private String text;

    @NotEmpty
    @Pattern(regexp = "[a-z\\s]+", message = "expected lowercase Latin letters")
    private String tags;

    public String getTags() {
        return tags;
    }

    public void setTags(String tags) {
        this.tags = tags;
    }

    public String getText() {
        return text;
    }

    public void setText(String text) {
        this.text = text;
    }
}
