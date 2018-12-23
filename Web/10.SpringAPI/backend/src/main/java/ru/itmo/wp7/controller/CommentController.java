package ru.itmo.wp7.controller;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;
import ru.itmo.wp7.domain.Comment;
import ru.itmo.wp7.form.CommentCredentials;
import ru.itmo.wp7.service.CommentService;
import ru.itmo.wp7.service.NoticeService;

import javax.servlet.http.HttpServletRequest;
import javax.validation.Valid;
import java.util.List;
import java.util.Optional;

@RestController
@RequestMapping("/api/1")
public class CommentController extends ApiController {
    @SuppressWarnings("SpringJavaAutowiredFieldsWarningInspection")
    @Autowired
    private CommentService commentService;

    @SuppressWarnings("SpringJavaAutowiredFieldsWarningInspection")
    @Autowired
    private NoticeService noticeService;

    @GetMapping("comment/{commentId}")
    public Optional<Comment> getComment(@PathVariable("commentId") String commentId) {
        return  commentService.findById(Long.parseLong(commentId));
    }

    @PostMapping("comments")
    public void save(@Valid @RequestBody CommentCredentials commentCredentials, HttpServletRequest httpServletRequest) {
        commentService.save(new Comment(commentCredentials), noticeService.findById(commentCredentials.getNoticeId()),
                getUser(httpServletRequest));
    }
}
