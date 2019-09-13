use feature qw(say);

sub uniq {
    my %seen;
    grep !$seen{$_}++, @_;
}

my @links = qw();
$s = '\s*';
$spaces = '^\s*$';

while ($input = <>) {
    next if (!($input =~ /<$s[aA].+\bhref$s=$s"$s(?<link>[^"]*)$s".*>/));

    $+{link} =~ /(?<protocol>[^:\/\?#\.]+:)?(\/\/)?(\w+(:\w+)?@)?(?<site>[^\/\?#:]+)(?<port>:\d+)?([\:\/\?#].*)?/;

    $protocol = $+{protocol};
    $site = $+{site};
    $port = $+{port};

    push (@links, $site) if ($site && !(($protocol =~ $spaces) && ($port =~ $spaces)));
}

say for sort(uniq(@links));
