package ru.itmo.wp7.service;

import org.springframework.stereotype.Service;
import ru.itmo.wp7.domain.Comment;
import ru.itmo.wp7.domain.Notice;
import ru.itmo.wp7.domain.User;
import ru.itmo.wp7.repository.CommentRepository;

import java.util.Optional;

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

    public Optional<Comment> findById(Long id) {
        return commentRepository.findById(id);
    }
}
