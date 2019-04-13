#include <iostream>
#include <algorithm>
#include <functional>

static const long long MODULE = 998244353;

long long safe_add(long long x, long long y) {
    return x < MODULE - y ? x + y : x - (MODULE - y);
}

long long safe_mult(long long x, long long y) {
    return (x * y) % MODULE;
}

long long safe_factorial(long long x) {
    long long result = 1;
    for (long long i = 2; i <= x; ++i) {
        result = safe_mult(result, i);
    }
    return result;
}

long long gcd(long long a, long long b, long long& x, long long& y) {
    if (a == 0) {
		x = 0; y = 1;
		return b;
	}
	long long x1, y1;
	long long d = gcd(b % a, a, x1, y1);
	x = y1 - (b / a) * x1;
	y = x1;

	return d;
}

long long mod_reverse(long long a) {
    long long x, y;
    long long g = gcd(a, MODULE, x, y);
	return (x % MODULE + MODULE) % MODULE;
}

class Polynom {
public:
    explicit Polynom(size_t power) : power(power), coefficients(new long long[power + 1]) {}

    Polynom(long long zero) : power(0), coefficients(new long long[1]) {
        coefficients[0] = zero;
    }

    Polynom(const Polynom& that) {
        coefficients = nullptr;
        *this = that;
    }

    ~Polynom() {
        delete[] coefficients;
    }

    long long& operator[](size_t index) {
        return coefficients[index];
    }

    long long operator[](size_t index) const {
        if (index > power) {
            return 0;
        }
        return coefficients[index];
    }

    friend std::istream& operator>>(std::istream& in, Polynom& gf) {
        for (size_t i = 0; i < gf.power + 1; ++i) {
            in >> gf.coefficients[i];
        }
        return in;
    }

    friend std::ostream& operator<<(std::ostream& out, const Polynom& gf) {
        for (size_t i = 0; i < gf.power + 1; ++i) {
            out << gf.coefficients[i] << ' ';
        }
        return out;
    }

    Polynom& operator=(const Polynom& that) {
        power = that.power;
        delete[] coefficients;
        coefficients = new long long[power + 1];
        std::copy(that.coefficients, that.coefficients + that.power + 1, coefficients);
        return *this;
    }

    Polynom operator+(const Polynom& that) const {
        Polynom result(std::max(power, that.power));
        result.reset();
        for (size_t i = 0; i < result.power + 1; ++i) {
            result[i] = safe_add((*this)[i], that[i]);
        }
        return result;
    }

    Polynom operator*(const Polynom& that) const {
        Polynom result(power + that.power);
        result.reset();

        for (size_t i = 0; i < power + 1; ++i) {
            for (size_t j = 0; j < that.power + 1; ++j) {
                result[i + j] = safe_add(result[i + j], safe_mult((*this)[i], that[j]));
            }
        }
        return result;
    }

    Polynom operator*(long long coef) const {
        Polynom result = *this;
        for (size_t i = 0; i < power + 1; ++i) {
            result[i] = safe_mult(result[i], coef);
        }
        return result;
    }

    void reset() {
        std::fill(coefficients, coefficients + power + 1, 0);
    }

    Polynom sqrt1(size_t precision) const {
        return powers_apply(precision, [] (long long i) -> long long {
            if (i == 0) {
                return 0;
            }

            long long denominator = 1;
            long long anti2 = mod_reverse(2);
            for (long long j = 0; j < i; ++j) {
                denominator = safe_mult(denominator, anti2 - j);
            }

            return safe_mult(denominator, mod_reverse(safe_factorial(i)));
        }) + 1;
    }

    Polynom exp(size_t precision) const {
        return powers_apply(precision, [] (long long i) { return mod_reverse(safe_factorial(i)); });
    }

    Polynom ln1(size_t precision) const {
        return powers_apply(precision, [] (long long i) -> long long {
            if (i == 0) {
                return 0;
            }

            return (i & 1 ? mod_reverse(i) : MODULE - mod_reverse(i));
        });
    }

    size_t power;

private:
    long long* coefficients;

    Polynom powers_apply(size_t precision, std::function<long long (long long)> maker) const {
        Polynom result(precision);
        Polynom tmp = 1;
        result.reset();

        for (size_t i = 0; i < result.power + 1; ++i, tmp = tmp * (*this)) {
            result = result + tmp * maker(i);
            result.power = precision;
        }
        return result;
    }
};


int main() {
    size_t power, precision;
    std::cin >> power >> precision;

    Polynom pol(power);
    std::cin >> pol;
    std::cout << pol.sqrt1(--precision) << '\n'
        << pol.exp(precision) << '\n'
        << pol.ln1(precision) << '\n';

    return 0;
}
