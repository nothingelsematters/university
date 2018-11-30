package ru.itmo.wm4.form.validator;

import org.springframework.stereotype.Component;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;
import ru.itmo.wm4.form.UserCredentials;
import ru.itmo.wm4.service.UserService;

@Component
public class UserCredentialsEnterValidator implements Validator {
    private final UserService userService;

    public UserCredentialsEnterValidator(UserService userService) {
        this.userService = userService;
    }

    @Override
    public boolean supports(Class<?> clazz) {
        return UserCredentials.class.equals(clazz);
    }

    @Override
    public void validate(Object target, Errors errors) {
        if (!errors.hasErrors()) {
            UserCredentials registerForm = (UserCredentials) target;
            if (userService.findByLoginAndPassword(registerForm.getLogin(), registerForm.getPassword()) == null) {
                errors.rejectValue("password", "invalid.login.or.password", "invalid login or password");
            }
        }
    }
}
