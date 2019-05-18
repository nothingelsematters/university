#include <iostream>

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

int supereuclid(int first, int second, int& first_coef, int& second_coef) {
    if (!first) {
        first_coef = 0;
        second_coef = 1;
        return second;
    }

    int tmp1, tmp2;
    int res = supereuclid(second % first, first, tmp1, tmp2);
    first_coef = tmp2 - (second / first) * tmp1;
    second_coef = tmp1;
    return res;
}

int module_reverse(int first, int second) {
    int useful, useless;
    supereuclid(first, second, useful, useless);
    return (useful % second + second) % second;
}

std::pair<int, int> factorization2(int num) {
    for (int i = 2; i <= num; ++i) {
        if (!(num % i)) {
            return {i, num / i};
        }
    }
}

int hack_rsa(int module, int public_key, int message) {
    auto [first, second] = factorization2(module);
    return binpow(message, module_reverse(public_key, (first - 1) * (second - 1)), module);
}

int main() {
    int module, public_key, message;
    std::cin >> module >> public_key >> message;
    std::cout << hack_rsa(module, public_key, message) << '\n';
    return 0;
}
