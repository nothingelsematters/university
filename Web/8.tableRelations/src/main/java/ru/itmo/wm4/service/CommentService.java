package ru.itmo.wm4.service;

import org.springframework.stereotype.Service;
import ru.itmo.wm4.domain.Comment;
import ru.itmo.wm4.domain.Notice;
import ru.itmo.wm4.domain.User;
import ru.itmo.wm4.repository.CommentRepository;

@Service
public class CommentService {
    private final CommentRepository commentRepository;


    public CommentService(CommentRepository commentRepository) {
        this.commentRepository = commentRepository;
    }

    public void save(Comment comment, Notice notice, User user) {
        comment.setUser(user);
        comment.setNotice(notice);
        user.addComment(comment);
        notice.addComment(comment);
        commentRepository.save(comment);
    }
}
