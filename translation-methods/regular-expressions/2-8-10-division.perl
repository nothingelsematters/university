while (<>) {
    s/\b(?<number>\d+)0\b/$+{number}/g;
    print;
}
