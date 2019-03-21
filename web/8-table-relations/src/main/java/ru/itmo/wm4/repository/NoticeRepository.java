package ru.itmo.wm4.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import ru.itmo.wm4.domain.Notice;

import java.util.List;

public interface NoticeRepository extends JpaRepository<Notice, Long> {
    List<Notice> findByOrderByCreationTimeDesc();
}
