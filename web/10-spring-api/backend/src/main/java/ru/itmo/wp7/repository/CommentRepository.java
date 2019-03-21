package ru.itmo.wp7.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import ru.itmo.wp7.domain.Comment;

public interface CommentRepository extends JpaRepository<Comment, Long>  {

}
