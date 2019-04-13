#include <iostream>

static long long MODULE = 1e9 + 7;

void safe_inc(long long& x, long long y) {
    x = (x + y) % MODULE;
}

long long safe_mul(long long x, long long y) {
    return (x * y) % MODULE;
}

int main() {
    size_t amount, limit;
    std::cin >> amount >> limit;
    bool weight[limit + 1];
    std::fill(weight, weight + limit + 1, false);

    for (size_t i = 0; i < amount; ++i) {
        size_t tmp;
        std::cin >> tmp;
        weight[tmp] = true;
    }

    long long trees_quantity[limit + 1];
    long long prefix_sum[limit + 1];
    std::fill(prefix_sum, prefix_sum + limit + 1, 0);
    std::fill(trees_quantity, trees_quantity + limit + 1, 0);
    prefix_sum[0] = 1;
    trees_quantity[0] = 1;

    for (long long i = 1; i <= limit; ++i) {
        long long current = 0;
        for (long long j = 1; j <= i; ++j) {
            if (weight[j]) {
                safe_inc(current, prefix_sum[i - j] % MODULE);
            }
            safe_inc(prefix_sum[i], safe_mul(trees_quantity[j], trees_quantity[i - j]));
        }

        trees_quantity[i] = current;
        safe_inc(prefix_sum[i], trees_quantity[i] << 1);
    }

    for (size_t i = 1; i <= limit; ++i) {
        std::cout << trees_quantity[i] << ' ';
    }

    return 0;
}
