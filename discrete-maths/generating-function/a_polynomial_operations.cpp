#include <iostream>

static const int MODULE = 998244353;

int safe_add(int x, int y) {
    return x < MODULE - y ? x + y : x - (MODULE - y);
}

int safe_mult(int x, int y) {
    int acc;
    for (acc = 0; y > 0; y >>= 1) {
        if (y & 1) {
            acc = safe_add(acc, x);
        }
        x = safe_add(x, x);
    }

    return acc;
}

int safe_subtract(int x, int y) {
    int result = x - y;
    while (result < 0) {
        result = safe_add(result, MODULE);
    }
    return result;
}

class Polynom {
public:
    explicit Polynom(size_t degree) : degree(degree), coefficients(new int[degree + 1]) {}

    ~Polynom() {
        delete[] coefficients;
    }

    int& operator[](size_t index) {
        return coefficients[index];
    }

    int operator[](size_t index) const {
        if (index > degree) {
            return 0;
        }
        return coefficients[index];
    }

    friend std::istream& operator>>(std::istream& in, Polynom& gf) {
        for (size_t i = 0; i < gf.degree + 1; ++i) {
            in >> gf.coefficients[i];
        }
        return in;
    }

    friend std::ostream& operator<<(std::ostream& out, const Polynom& gf) {
        for (size_t i = 0; i < gf.degree + 1; ++i) {
            out << gf.coefficients[i] << ' ';
        }
        return out;
    }

    Polynom operator+(const Polynom& that) const {
        Polynom result(std::max(degree, that.degree));
        for (size_t i = 0; i < result.degree + 1; ++i) {
            result[i] = safe_add((*this)[i], that[i]);
        }
        result.fix_degree();
        return result;
    }

    Polynom operator*(const Polynom& that) const {
        Polynom result(degree + that.degree);

        for (size_t i = 0; i < result.degree + 1; ++i) {
            result[i] = 0;
            for (size_t j = 0; j <= i; ++j) {
                result[i] = safe_add(result[i], safe_mult((*this)[j], that[i - j]));
            }
        }
        result.fix_degree();
        return result;
    }

    // only 1000 first elements
    Polynom operator/(const Polynom& that) const {
        Polynom result(999);

        for (size_t i = 0; i < result.degree + 1; ++i) {
            int sum = 0;
            for (size_t j = 0; j < i; ++j) {
                sum = safe_add(sum, safe_mult(result[j], that[i - j]));
            }
            result[i] = safe_subtract((*this)[i], sum) / that[0];
        }
        return result;
    }

    size_t degree;
private:
    int* coefficients;

    void fix_degree() {
        while ((*this)[degree] == 0 && degree != 0) {
            --degree;
        }
    }
};

int main() {
    size_t first_degree, second_degree;
    std::cin >> first_degree >> second_degree;

    Polynom first(first_degree);
    Polynom second(second_degree);
    std::cin >> first >> second;

    Polynom sum = first + second;
    Polynom product = first * second;
    std::cout << sum.degree << '\n' << sum << '\n'
        << product.degree << '\n' << product << '\n'
        << first / second << '\n';
    return 0;
}
