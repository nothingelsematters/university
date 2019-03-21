package ru.itmo.wp7.form.validator;

import org.springframework.stereotype.Component;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;
import ru.itmo.wp7.form.NoticeCredentials;

@Component
public class NoticeCredentialsAddValidator implements Validator {
    @Override
    public boolean supports(Class<?> clazz) {
        return NoticeCredentials.class.equals(clazz);
    }

    @Override
    public void validate(Object target, Errors errors) {}
}
