#include <iostream>

static const long long MODULE = 998244353;

long long save(long long value) {
    for (value %= MODULE; value < 0; value += MODULE);
    return value;
}

long long safe_add(long long x, long long y) {
    return save(x + y);
}

long long safe_mult(long long x, long long y) {
    return save(x * y);
}

long long safe_subtract(long long x, long long y) {
    return save(x - y);
}

class Polynom {
public:
    explicit Polynom(size_t degree) : degree(degree), coefficients(new long long[degree + 1]) {}

    ~Polynom() {
        delete[] coefficients;
    }

    long long& operator[](size_t index) {
        return coefficients[index];
    }

    long long operator[](size_t index) const {
        if (index > degree) {
            return 0;
        }
        return coefficients[index];
    }

    friend std::ostream& operator<<(std::ostream& out, const Polynom& gf) {
        for (size_t i = 0; i < gf.degree + 1; ++i) {
            out << gf.coefficients[i] << '\n';
        }
        return out;
    }

    Polynom div(const Polynom& that, size_t precision) const {
        Polynom result(precision);

        for (size_t i = 0; i < result.degree + 1; ++i) {
            long long sum = 0;
            for (size_t j = 0; j < i; ++j) {
                sum = safe_add(sum, safe_mult(result[j], that[i - j]));
            }
            result[i] = safe_subtract((*this)[i], sum) / that[0];
        }
        return result;
    }

    size_t degree;
private:
    long long* coefficients;
};

int main() {
    size_t brush, amount;
    std::cin >> brush >> amount;
    Polynom numerator(((brush - 1) >> 1) + ((brush - 1) & 1 ? 0 : -1));
    Polynom denominator((brush >> 1) + (brush & 1 ? 0 : -1));

    long long choose[brush + 1][brush + 1]; // pascal
    for (size_t i = 0; i < brush + 1; ++i) {
        choose[0][i] = 1;
        choose[i][i] = 1;
        for (size_t j = 1; j < i; ++j) {
            choose[j][i] = safe_add(choose[j][i - 1], choose[j - 1][i - 1]);
        }
    }

    for (size_t i = 0; i <= numerator.degree; ++i) {
        numerator[i] = safe_mult((i & 1 ? -1 : 1), choose[i][brush - i - 2]);
    }
    for (size_t i = 0; i <= denominator.degree; ++i) {
        denominator[i] = safe_mult((i & 1 ? -1 : 1), choose[i][brush - i - 1]);
    }

    std::cout << numerator.div(denominator, amount - 1);
    return 0;
}
