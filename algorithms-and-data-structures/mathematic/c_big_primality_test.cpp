#include <iostream>
#include <ctime>
#include <cstdlib>


long long mult(long long first, long long second, long long module) {
    long long result = 0;
    while (second) {
        if (second & 1) {
            result = (result + first) % module;
        }
        first = (first << 1) % module;
        second >>= 1;
    }
    return result;
}

long long binpow(long long first, long long second, long long module) {
    long long result = 1;
    while (second) {
        if (second & 1) {
            result = mult(result, first, module);
        }
        first = mult(first, first, module);
        second >>= 1;
    }
    return result;
}

// Miller-Rabin test
bool is_prime(unsigned long long num) {
    static const int ITERATIONS = 7;
    if (num == 2) {
        return true;
    }
    if (num == 1 || !(num & 1)) {
        return false;
    }

    int power = 0;
    long long remain;
    for (remain = num - 1; !(remain & 1); ++power, remain >>= 1);

    std::srand(std::time(nullptr));
    for (int i = 0; i < ITERATIONS; ++i) {
        unsigned long long x = binpow((rand() % (num - 2)) + 2, remain, num);
        if (x == 1 || x == num - 1) {
            continue;
        }

        for (int j = 0; j < power; ++j) {
            x = binpow(x, 2, num);
            if (x == num - 1) {
                break;
            }
        }

        if (x != num - 1) {
            return false;
        }
    }
    return true;
}

int main() {
    int amount;
    std::cin >> amount;

    for (int i = 0; i < amount; ++i) {
        unsigned long long number;
        std::cin >> number;
        std::cout << (is_prime(number) ? "YES" : "NO") << '\n';
    }
    return 0;
}
