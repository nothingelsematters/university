package ru.itmo.wp7.service;

import org.springframework.stereotype.Service;
import ru.itmo.wp7.domain.Notice;
import ru.itmo.wp7.domain.User;
import ru.itmo.wp7.form.NoticeCredentials;
import ru.itmo.wp7.repository.NoticeRepository;

import java.util.List;

@Service
public class NoticeService {
    private final NoticeRepository noticeRepository;

    public NoticeService(NoticeRepository noticeRepository) {
        this.noticeRepository = noticeRepository;
    }

    public List<Notice> findByOrderByCreationTimeDesc() {
        return noticeRepository.findByOrderByCreationTimeDesc();
    }

    public Notice findById(Long noticeId) {
        return noticeId == null ? null : noticeRepository.findById(noticeId).orElse(null);
    }

    public void save(NoticeCredentials noticeCredentials, User user) {
        Notice notice = new Notice(noticeCredentials);
        user.addNotice(notice);
        notice.setUser(user);
        noticeRepository.save(notice);
    }

    public List<Notice> findFresh() {
        return noticeRepository.findFresh();
    }
}
