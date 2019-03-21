#include <iostream>

int main() {
    std::string str;
    std::cin >> str;
    size_t last = 0;
    size_t pf[str.size()];
    std::fill(pf, pf + str.size(), 0);

    for (size_t i = 1; i < str.size(); ++i) {
        int tmp = pf[i - 1];
		while (tmp > 0 && str[i] != str[tmp]) {
            tmp = pf[tmp - 1];
        }
		if (str[i] == str[tmp]) {
            ++tmp;
        }
		pf[i] = tmp;
    }
    size_t period = str.size() - pf[str.size() - 1];
    std::cout << (period > str.size() / 2 ? str.size() : period);

    return 0;
}
