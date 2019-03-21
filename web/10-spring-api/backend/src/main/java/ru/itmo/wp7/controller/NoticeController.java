package ru.itmo.wp7.controller;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;
import ru.itmo.wp7.domain.Notice;
import ru.itmo.wp7.form.NoticeCredentials;
import ru.itmo.wp7.service.NoticeService;

import javax.servlet.http.HttpServletRequest;
import javax.validation.Valid;
import java.util.List;

@RestController
@RequestMapping("/api/1")
public class NoticeController extends ApiController {
    @SuppressWarnings("SpringJavaAutowiredFieldsWarningInspection")
    @Autowired
    private NoticeService noticeService;

    @GetMapping("notice/{noticeId}")
    public Notice getNotice(@PathVariable("noticeId") String noticeId) {
        return  noticeService.findById(Long.parseLong(noticeId));
    }

    @GetMapping("notices")
    public List<Notice> getFresh() {
        return noticeService.findFresh();
    }

    @PostMapping("notices")
    public void save(@Valid @RequestBody NoticeCredentials noticeCredentials, HttpServletRequest httpServletRequest) {
        noticeService.save(noticeCredentials, getUser(httpServletRequest));
    }
}
