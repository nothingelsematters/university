package ru.itmo.wm4.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.transaction.annotation.Transactional;
import ru.itmo.wm4.domain.Notice;

public interface NoticeRepository extends JpaRepository<Notice, Long> {

}
