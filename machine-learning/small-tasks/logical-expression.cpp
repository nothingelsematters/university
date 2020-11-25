#include <iostream>
#include <iomanip>
#include <algorithm>
#include <iterator>

int main() {
    unsigned short argument_amount;
    std::cin >> argument_amount;

    unsigned int exp = 1 << argument_amount;

    std::cout << std::fixed << std::setprecision(10) << 2 << '\n' << exp << ' ' << 1 << '\n';

    for (unsigned int i = 0; i < exp; ++i) {
        for (unsigned short j = 0; j < argument_amount; ++j) {
            std::cout << (i & (1 << j) ? 1.0 : -1e9) << ' ';
        }
        unsigned short ones = 0;
        for (unsigned int j = i; j > 0; ++ones, j &= j - 1);
        std::cout << 0.5 - ones << '\n';
    }

    std::copy(std::istream_iterator<double>(std::cin), std::istream_iterator<double>(), std::ostream_iterator<double>(std::cout, " "));
    std::cout << -0.5 << '\n';

    return 0;
}
