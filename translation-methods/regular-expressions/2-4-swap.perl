while (<>) {
    s/\b(?<first>\w+)\b(?<middle>[^\w]+)(?<second>\w+)\b/$+{second}$+{middle}$+{first}/;
    print;
}
