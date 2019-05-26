#include <iostream>
#include <vector>
#include <algorithm>
#include <iterator>

static constexpr size_t LIMIT = 2 * 1E7;

std::vector<bool> primality(size_t limit) {
    size_t size = limit;
    auto is_prime = std::vector<bool>(size, true);
    is_prime[0] = false;
    is_prime[1] = false;

    for (size_t i = 2; static_cast<unsigned long long>(i) * i < size; ++i) {
        if (is_prime[i]) {
            for (unsigned long long j = static_cast<unsigned long long>(i) * i; j < size; j += i) {
                is_prime[j] = false;
            }
        }
    }
    return is_prime;
}

bool is_prime(unsigned int num) {
    static auto composite = primality(LIMIT + 1);
    return composite[num];
}

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(nullptr);
    size_t amount;
    std::cin >> amount;

    std::transform(std::istream_iterator<unsigned int>(std::cin), std::istream_iterator<unsigned int>(),
        std::ostream_iterator<std::string>(std::cout, "\n"), [] (unsigned int number) { return is_prime(number) ? "YES" : "NO"; });
    return 0;
}
