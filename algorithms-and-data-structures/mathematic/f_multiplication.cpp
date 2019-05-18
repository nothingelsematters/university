#include <iostream>
#include <string>
#include <vector>
#include <algorithm>
#include <iterator>
#include <complex>
#include <cmath>

static constexpr double PI = std::atan(1.0) * 4;

class pseudobigint {
public:
    using complex_t = std::complex<double>;

    pseudobigint() {}

    pseudobigint(const std::string& str) : sign(str.size() > 0 && str[0] != '-') {
        digits.reserve(str.size() - !sign);
        for (size_t i = !sign; i < str.size(); ++i) {
            digits.push_back(str[i] - '0');
        }
        std::reverse(digits.begin(), digits.end());
    }

    int& operator[](size_t index) {
        return digits[index];
    }

    friend std::ostream& operator<<(std::ostream& out, const pseudobigint& pbi) {
        const std::vector<int>& digits = pbi.digits;
        if ((digits.size() > 1 || (digits.size() && digits[0])) && !pbi.sign) {
            out << '-';
        }
        std::copy(digits.crbegin(), digits.crend(), std::ostream_iterator<int>(out));
        return out;
    }

    // fast fury theorem
    pseudobigint operator+(const pseudobigint& other) {
        pseudobigint pbi;
        pbi.sign = this->sign == other.sign;

        std::vector<complex_t> first(this->digits.begin(), this->digits.end());
        std::vector<complex_t> second(other.digits.begin(), other.digits.end());

        size_t power = 1 << (static_cast<int>(std::ceil(std::log2(std::max(this->digits.size(), other.digits.size()) * 2))));
        first.resize(power);
        second.resize(power);
        pbi.digits.resize(power);

        ndots(first);
        ndots(second);

        for (size_t i = 0; i < power; ++i) {
            first[i] *= second[i];
        }
        transforming(first);


        for (size_t i = 0; i < power; ++i) {
            pbi[i] = static_cast<int>(first[i].real() + 0.5);
        }

        int carry = 0;
        for (size_t i = 0; i < power; ++i) {
            pbi[i] += carry;
            carry = pbi[i] / 10;
            pbi[i] %= 10;
        }

        for (; power > 1 && !pbi[power - 1]; --power);
        pbi.digits.resize(power);
        return pbi;
    }

private:
    static void ndots(std::vector<complex_t>& com) {
        fft_common(com, &ndots, true);
    }

    static void transforming(std::vector<complex_t>& com) {
        if (fft_common(com, &transforming, false)) {
            return;
        }
        for (auto& i : com) {
            i /= 2;
        }
    }

    static bool fft_common(std::vector<complex_t>& com, void (*fft_func)(std::vector<complex_t>&), bool sign) {
        size_t size = com.size();
        if (size == 1) {
            return true;
        }

        std::vector<complex_t> left(size >> 1);
        std::vector<complex_t> right(size >> 1);

        for (size_t i = 0, j = 0; i < size; ++j, i += 2) {
            left[j] = com[i];
            right[j] = com[i + 1];
        }


        fft_func(left);
        fft_func(right);

        double angle = (sign ? 1 : -1) * 2 * PI / size;
        complex_t dot(1, 0);
        complex_t nth(std::cos(angle), std::sin(angle));
        for (size_t i = 0; i < (size >> 1); ++i, dot *= nth) {
            com[i] = left[i] + dot * right[i];
            com[i + (size >> 1)] = left[i] - dot * right[i];
        }

        return false;
    }

    bool sign;
    std::vector<int> digits;
};

int main() {
    std::string a, b;
    std::cin >> a >> b;
    std::cout << pseudobigint(a) + pseudobigint(b) << '\n';
    return 0;
}
