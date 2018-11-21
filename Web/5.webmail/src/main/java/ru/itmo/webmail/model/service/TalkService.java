package ru.itmo.webmail.model.service;

import ru.itmo.webmail.model.domain.Talk;
import ru.itmo.webmail.model.exception.ValidationException;
import ru.itmo.webmail.model.repository.TalkRepository;
import ru.itmo.webmail.model.repository.impl.TalkRepositoryImpl;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

public class TalkService {
    private UserService userService = new UserService();
    private TalkRepository talkRepository = new TalkRepositoryImpl();

    public void validateSending(String login, String text) throws ValidationException {
        if (userService.findByLoginOrEmail(login) == null) {
            throw new ValidationException("There is no user with such login or email");
        }

        if (text == null || text.isEmpty()) {
            throw new ValidationException("You have to write something");
        }
    }

    public void send(Long source, Long target, String text) {
        talkRepository.save(new Talk(source, target, text));
    }

    public List<Talk> findAllForUser(Long id) {
        return talkRepository.findAllForUser(id);
    }

    public List<FrontTalk> findAllFrontForUser(Long id) {
        List<Talk> talks = findAllForUser(id);
        List<FrontTalk> result = new ArrayList<>();
        for (Talk talk: talks) {
            result.add(new FrontTalk(userService.findById(talk.getSourceUserId()).getLogin(),
                    userService.findById(talk.getTargetUserId()).getLogin(), talk.getText(),
                    talk.getCreationTime()));
        }
        return result;
    }

    public static class FrontTalk {
        private String source;
        private String target;
        private String text;
        private Date time;

        public Date getTime() {
            return time;
        }

        public void setTime(Date time) {
            this.time = time;
        }

        public FrontTalk(String source, String target, String text, Date time) {
            this.target = target;
            this.source = source;
            this.text = text;
            this.time = time;
        }


        public String getText() {
            return text;
        }

        public void setText(String text) {
            this.text = text;
        }

        public String getSource() {
            return source;
        }

        public void setSource(String source) {
            this.source = source;
        }

        public String getTarget() {
            return target;
        }

        public void setTarget(String target) {
            this.target = target;
        }
    }
}
