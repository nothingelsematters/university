#include <iostream>
#include <memory>

static const size_t LIMIT = 1E6;

std::unique_ptr<bool[]> primality(size_t limit) {
    size_t size = limit + 1;
    auto is_prime = std::make_unique<bool[]>(size);
    std::fill(is_prime.get(), is_prime.get() + size, true);
    is_prime[0] = false;
    is_prime[1] = false;

    for (size_t i = 2; i <= size; ++i) {
        if (!is_prime[i]) {
            continue;
        }
        for (unsigned long long j = static_cast<unsigned long long>(i) * i; j <= size; j += i) {
            is_prime[j] = false;
        }
    }
    return std::move(is_prime);
}

bool is_prime(unsigned int num) {
    static std::unique_ptr<bool[]> composite = primality(LIMIT);
    return composite[num];
}

int main() {
    size_t amount;
    std::cin >> amount;

    for (size_t i = 0; i < amount; ++i) {
        unsigned int number;
        std::cin >> number;
        std::cout << (is_prime(number) ? "YES" : "NO") << '\n';
    }
    return 0;
}
