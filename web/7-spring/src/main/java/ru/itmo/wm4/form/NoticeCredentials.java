package ru.itmo.wm4.form;

import javax.validation.constraints.NotNull;
import javax.validation.constraints.Size;

public class NoticeCredentials {
    @NotNull
    @Size(min = 1, max = 1000)
    private String content;

    public String getContent() {
        return content;
    }

    public void setContent(String content) {
        this.content = content;
    }
}
