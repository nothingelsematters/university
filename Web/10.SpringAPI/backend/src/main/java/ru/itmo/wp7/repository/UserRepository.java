package ru.itmo.wp7.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.transaction.annotation.Transactional;
import ru.itmo.wp7.domain.User;

import java.util.List;
import java.util.Optional;

public interface UserRepository extends JpaRepository<User, Long> {
    int countByLogin(String login);

    @Transactional
    @Modifying
    @Query(value = "UPDATE user SET passwordSha=SHA1(?2) WHERE id=?1", nativeQuery = true)
    void updatePassword(long id, String password);

    @Query(value = "SELECT * FROM user WHERE login=?1 AND passwordSha=SHA1(?2)", nativeQuery = true)
    Optional<User> findByLoginAndPassword(String login, String password);
}
