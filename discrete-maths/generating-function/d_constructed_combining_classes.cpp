#include <iostream>
#include <string>
#include <memory>

static size_t precision = 7;

std::pair<std::string, std::string> coma_split(const std::string& str) {
    size_t balance = 0;
    std::string::size_type pos;

    for (size_t i = 0; i < str.size(); ++i) {
        switch (str[i]) {
            case '(':
                ++balance;
                break;
            case ')':
                --balance;
                break;
            case ',':
                if (balance == 1) {
                    pos = i;
                }
        }
    }

    return {str.substr(2, pos - 2), str.substr(pos + 1, str.size() - pos - 2)};
}

long long fact(long long from, long long amount) {
	long long result = 1;
	for (long long i = from + 1; i <= amount; ++i) {
        result *= i;
    }
	return result;
}

std::unique_ptr<long long[]> compute(const std::string& expr) {
    auto res = std::make_unique<long long[]>(precision);

    switch (expr[0]) {
        case 'B':
            std::fill(res.get(), res.get() + precision, 0);
            res[1] = 1;
            return std::move(res);

        case 'P':
            {
                auto parts = coma_split(expr);
                auto under = compute(parts.first);
                auto under_right = compute(parts.second);
                for (size_t i = 0; i < precision; ++i) {
                    res[i] = 0;
                    for (size_t j = 0; j <= i; ++j) {
                        res[i] += under[j] * under_right[i - j];
                    }
                }
            }
            break;

        case 'S':
            {
                auto under = compute(expr.substr(2, expr.size() - 3));
                res[0] = 1;
                long long matrix[precision][precision];
                for (size_t i = 0; i < precision; ++i) {
                    std::fill(matrix[i], matrix[i] + precision, 0);
                    matrix[i][0] = 0;
                    matrix[0][i] = 1;
                }

                for (size_t i = 1; i < precision; ++i) {
                    for (size_t j = 1; j < precision; ++j) {
                        for (size_t k = 0; k <= i / j; ++k) {
                            long long m = under[j] + k - 1;
                            m = std::max(m, 0ll);
                            matrix[i][j] += fact(m - k, m) / fact(1, k) * matrix[i - j * k][j - 1];
                        }
                    }
                    res[i] = matrix[i][i];
                }
            }
            break;

        case 'L':
            {
                auto under = compute(expr.substr(2, expr.size() - 3));
                res[0] = 1;

                for (size_t i = 1; i < precision; ++i) {
                    res[i] = 0;
                    for (size_t j = 1; j <= i; ++j) {
                        res[i] += under[j] * res[i - j];
                    }
                }
            }
    }
    return res;
}

int main() {
    std::string expression;
    std::cin >> expression;

    auto result = compute(expression);
    for (size_t i = 0; i < precision; ++i) {
        std::cout << result[i] << ' ';
    }
    return 0;
}
