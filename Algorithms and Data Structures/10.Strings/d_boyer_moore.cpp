#include <iostream>
#include <list>

std::list<int> substring_find(std::string const& main, std::string const& substring) {
    const int size = substring.size();
    int bad_chars[70];
    std::fill(bad_chars, bad_chars + 70, size);
    for (int i = 0; i < size - 1; ++i) {
        bad_chars[substring[i] - 'A'] = size - 1 - i;
    }

    int good_shifts[size];
    int f[size];
    std::fill(good_shifts, good_shifts + size, 0);
    std::fill(f, f + size, 0);
    int j = size;
    f[size - 1] = j;
    for (int i = size - 1; i >= 0; --i) {
        // if (size > 1e6) break;
        while (j <= size - 1 && substring[i] != substring[j]) {
            if (good_shifts[j] == 0) {
                good_shifts[j] = j - i;
            }
            j = f[j];
        }
        f[i - 1] = --j;
    }
    int p = f[0];
    for (j = 0; j < size - 1; ++j) {
        if (good_shifts[j] == 0) {
            good_shifts[j] = p;
        }
        if (j == p) {
            p = f[p];
        }
    }

    std::list<int> coordinates;
    for (int i = size - 1; i < main.size(); ) {
        int j = size - 1;
        while (substring[j] == main[i]) {
            if (j == 0) {
                coordinates.push_back(i);
                break;
            }
            --i;
            --j;
        }
        i += std::max(size - 1 - j + good_shifts[j], bad_chars[main[i] - 'A']);
    }

    return std::move(coordinates);
}

int main() {
    std::string main, str;
    std::cin >> str >> main;

    auto result = substring_find(main, str);
    std::cout << result.size() << '\n';
    for (auto i: result) {
        std::cout << i + 1 << ' ';
    }

    return 0;
}
