package ru.itmo.wp7.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import ru.itmo.wp7.domain.Notice;

import java.util.List;

public interface NoticeRepository extends JpaRepository<Notice, Long> {
    List<Notice> findByOrderByCreationTimeDesc();

    @Query(value = "SELECT * FROM notice ORDER BY creation_time LIMIT 3", nativeQuery = true)
    List<Notice> findFresh();
}
