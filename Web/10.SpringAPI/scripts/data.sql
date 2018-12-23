INSERT INTO `user` (`id`, `admin`, `creation_time`, `login`, `passwordSha`, `name`)
    VALUES
('1', b'1', NOW(), 'mike', SHA1('mike1'), 'Mike Mirzayanov');

INSERT INTO `user` (`id`, `admin`, `creation_time`, `login`, `passwordSha`, `name`)
    VALUES
('5', b'0', NOW(), 'tourist', SHA1('tourist1'), 'Gennady Korotkevich');

INSERT INTO `user` (`id`, `admin`, `creation_time`, `login`, `passwordSha`, `name`)
    VALUES
('7', b'0', NOW(), 'petr', SHA1('petr1'), 'Petr Mitrichev');

INSERT INTO `user` (`id`, `admin`, `creation_time`, `login`, `passwordSha`, `name`)
    VALUES
('11', b'0', NOW(), 'andrewzta', SHA1('andrewzta1'), 'Andrew Stankevich');
