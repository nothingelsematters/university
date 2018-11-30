package ru.itmo.wm4.service;

import org.springframework.stereotype.Service;
import ru.itmo.wm4.domain.Notice;
import ru.itmo.wm4.domain.Tag;
import ru.itmo.wm4.domain.User;
import ru.itmo.wm4.form.NoticeCredentials;
import ru.itmo.wm4.repository.NoticeRepository;

import java.util.List;

@Service
public class NoticeService {
    private final NoticeRepository noticeRepository;

    private final TagService tagService;

    public NoticeService(NoticeRepository noticeRepository, TagService tagService) {
        this.noticeRepository = noticeRepository;
        this.tagService = tagService;
    }

    public List<Notice> findByOrderByCreationTimeDesc() {
        return noticeRepository.findByOrderByCreationTimeDesc();
    }

    public Notice findById(Long noticeId) {
        return noticeId == null ? null : noticeRepository.findById(noticeId).orElse(null);
    }

    public void save(NoticeCredentials noticeCredentials, User user) {
        Notice notice = new Notice(noticeCredentials);
        String[] parsedTags = noticeCredentials.getTags().split("\\s");
        for (String stringTag: parsedTags) {
            notice.getTags().add(tagService.get(stringTag));
        }
        user.addNotice(notice);
        noticeRepository.save(notice);
    }
}
