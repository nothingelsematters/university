while (<>) {
    s/\b(?<first>\w)(?<second>\w)/$+{second}$+{first}/g;
    print;
}
