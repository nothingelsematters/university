use feature qw(say);

$middle = 0;
$spaces = 0;

while (<>) {
    if (/^\s*$/) {
        $spaces = 1 if $middle;
    } else {

        if (!$middle) {
            $middle = 1;
        } elsif ($spaces) {
            say "";
            $spaces = 0;
        }

        s/^\s+|\s+$//g;
        s/\s\s+/ /g;
        say;
    }
}
