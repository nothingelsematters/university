#include <iostream>

int main() {
    std::string str;
    std::cin >> str;

    int zf[str.size()];
    std::fill(zf, zf + str.size(), 0);
    int left = 0;
    int right = 0;
    for (int i = 1; i < str.size(); ++i) {
        zf[i] = std::max(0, std::min(right - i, zf[i - left]));
        while (i + zf[i] < str.size() && str[zf[i]] == str[i + zf[i]]) {
            zf[i]++;
        }
        if (i + zf[i] > right) {
            left = i;
            right = i + zf[i];
        }
    }
    for (size_t i = 1; i < str.size(); ++i) {
        std::cout << zf[i] << ' ';
    }

    return 0;
}
