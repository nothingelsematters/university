package ru.itmo.wp7.domain;

import com.fasterxml.jackson.annotation.JsonIgnore;
import org.hibernate.annotations.CreationTimestamp;
import org.hibernate.annotations.LazyCollection;
import org.hibernate.annotations.LazyCollectionOption;
import org.springframework.transaction.annotation.Transactional;

import javax.persistence.*;
import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Pattern;
import javax.validation.constraints.Size;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

@Entity
@Transactional
@Table(uniqueConstraints = @UniqueConstraint(columnNames = "login"))
public class User {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private long id;

    @NotNull
    @NotEmpty
    @Size(min = 2, max = 24)
    @Pattern(regexp = "[a-z]+")
    private String login;

    @NotNull
    @NotEmpty
    @Size(min = 5, max = 50)
    @Pattern(regexp = "[A-Z][a-z]+\\s[A-Z][a-z]+")
    private String name;

    private boolean admin;

    @CreationTimestamp
    private Date creationTime;

    @JsonIgnore
    @OneToMany(fetch = FetchType.EAGER)
    @JoinColumn(name = "user_id")
    @OrderBy("creationTime DESC")
    private List<Notice> notices;

    @JsonIgnore
    @OneToMany
    @LazyCollection(LazyCollectionOption.FALSE)
    @JoinColumn(name = "user_id")
    @OrderBy("creationTime DESC")
    private List<Comment> comments;


    public User() {}

    public User(@NotNull @NotEmpty @Size(min = 2, max = 24) @Pattern(regexp = "[a-z]+") String login,
                @NotNull @NotEmpty @Size(min = 5, max = 50) @Pattern(regexp = "[A-Z][a-z]+\\s[A-Z][a-z]+") String name,
                boolean admin) {

        this.login = login;
        this.name = name;
        this.admin = admin;
    }

    public List<Notice> getNotices() {
        return notices;
    }

    public void setNotices(List<Notice> notices) {
        this.notices = notices;
    }

    public List<Comment> getComments() {
        return comments;
    }

    public void setComments(List<Comment> comments) {
        this.comments = comments;
    }

    public long getId() {
        return id;
    }

    public void setId(long id) {
        this.id = id;
    }

    public String getLogin() {
        return login;
    }

    public void setLogin(String login) {
        this.login = login;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public boolean isAdmin() {
        return admin;
    }

    public void setAdmin(boolean admin) {
        this.admin = admin;
    }

    public Date getCreationTime() {
        return creationTime;
    }

    public void setCreationTime(Date creationTime) {
        this.creationTime = creationTime;
    }

    @Transactional
    public void addNotice(Notice notice) {
        if (notices == null) {
            notices = new ArrayList<>();
        }
        notices.add(notice);
    }

    public void addComment(Comment comment) {
        if (comments == null) {
            comments = new ArrayList<>();
        }
        comments.add(comment);
    }

}
