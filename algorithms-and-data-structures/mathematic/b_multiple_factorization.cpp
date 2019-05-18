#include <iostream>
#include <memory>
#include <vector>
#include <algorithm>
#include <iterator>
#include <numeric>

static const size_t LIMIT = 1E6;

inline std::unique_ptr<unsigned int[]> factorizor(size_t limit) {
    size_t size = limit + 1;
    auto factorize = std::make_unique<unsigned int[]>(size);
    std::iota(factorize.get(), factorize.get() + size, 0);

    for (unsigned int i = 2; i <= size; ++i) {
        if (factorize[i] != i) {
            continue;
        }
        for (unsigned long long j = static_cast<unsigned long long>(i) * i; j <= size; j += i) {
            factorize[j] = i;
        }
    }
    return std::move(factorize);
}

inline int facts(unsigned int num) {
    static std::unique_ptr<unsigned int[]> composite = factorizor(LIMIT);
    return composite[num];
}

inline std::vector<int> factorize(int number) {
    static std::vector<int> result;
    result.clear();
    while (number != 1) {
        int fact = facts(number);
        result.push_back(fact);
        number /= fact;
    }
    std::sort(result.begin(), result.end());
    return result;
}

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(nullptr);
    size_t amount;
    std::cin >> amount;

    for (size_t i = 0; i < amount; ++i) {
        int number;
        std::cin >> number;
        auto factorized = factorize(number);
        std::copy(factorized.cbegin(), factorized.cend(), std::ostream_iterator<int>(std::cout, " "));
        std::cout << '\n';
    }
    return 0;
}
