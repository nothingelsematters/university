$x = '[^\(\)]';

while (<>) {
    print if /\($x*(\b$x*\b)$x*\)/;
}
