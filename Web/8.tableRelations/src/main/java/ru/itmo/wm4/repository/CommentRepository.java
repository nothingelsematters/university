package ru.itmo.wm4.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import ru.itmo.wm4.domain.Comment;

public interface CommentRepository extends JpaRepository<Comment, Long>  {

}
