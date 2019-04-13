#include <iostream>
#include <algorithm>

static const long long MODULE = 104857601;

long long save(long long value) {
    for (value %= MODULE; value < 0; value += MODULE);
    return value;
}

int main() {
    size_t coef_amount, needed;
    std::cin >> coef_amount >> needed;
    long long row[coef_amount << 1];
    long long coefficients[coef_amount + 1] = {1};

    for (size_t i = 0; i < coef_amount; ++i) {
        std::cin >> row[i];
    }
    for (size_t i = 1; i <= coef_amount; ++i) {
        std::cin >> coefficients[i];
        coefficients[i] = save(MODULE - coefficients[i]);
    }

    for (--needed; needed >= coef_amount; needed >>= 1) {
        for (size_t i = coef_amount; i < (coef_amount << 1); ++i) {
            row[i] = 0;
            for (size_t j = 1; j <= coef_amount; ++j) {
                row[i] = save(row[i] - coefficients[j] * row[i - j]);
            }
        }

        for (size_t i = needed & 1; i <= (coef_amount << 1); i += 2) {
            row[i >> 1] = row[i];
        }

        long long opposite_coefficients[coef_amount + 1];
        for (size_t i = 0; i <= coef_amount; ++i) {
            opposite_coefficients[i] = i & 1 ? save(MODULE - coefficients[i]) : coefficients[i];
        }

        long long new_coefficients[coef_amount + 1];
        for (size_t i = 0; i <= coef_amount; ++i) {
            new_coefficients[i] = 0;
            for (size_t j = 0; j <= (i << 1); ++j) {
                new_coefficients[i] = save(new_coefficients[i] + (j > coef_amount ? 0 : coefficients[j]) *
                    ((i << 1) - j > coef_amount ? 0 : opposite_coefficients[(i << 1) - j]));
            }
        }
        std::copy(new_coefficients, new_coefficients + coef_amount + 1, coefficients);
    }

    std::cout << row[needed] << '\n';
    return 0;
}
