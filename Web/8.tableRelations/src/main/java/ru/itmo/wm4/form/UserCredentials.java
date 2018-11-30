package ru.itmo.wm4.form;

import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.Pattern;
import javax.validation.constraints.Size;

public class UserCredentials {
    @NotEmpty
    @Size(min = 3, max = 16)
    @Pattern(regexp = "[a-z]+", message = "expected lowercase Latin letters")
    private String login;

    @NotEmpty
    @Size(min = 1, max = 64)
    private String password;

    public String getLogin() {
        return login;
    }

    public void setLogin(String login) {
        this.login = login;
    }

    public String getPassword() {
        return password;
    }

    public void setPassword(String password) {
        this.password = password;
    }
}
