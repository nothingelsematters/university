#include <iostream>
#include <vector>
#include <cmath>
using namespace std;

const int BASE = 9;
const int MAX = 1000000000;

class BigInteger {
private:
    vector<uint32_t> digits;
    bool sign;
    friend ostream& operator <<(ostream& wave, const BigInteger& b) {
        if (!b.sign) wave << '-';
        wave << b.digits.back();
        for (int i = b.digits.size() - 2; i >= 0; --i) {
                wave.fill('0');
                wave.width(9);
                wave << b.digits[i];
        }
        return wave;
    }
    friend istream& operator >>(istream& wave, BigInteger& b) {
        string s;
        wave >> s;
        b.sign = (s.substr(0, 1) != "-");
        if (!b.sign) s = s.substr(1, s.length() - 1);
        for (int i = BASE; i <= s.length(); i += BASE) {
            b.digits.push_back(stoi(s.substr(s.length() - i, BASE)));
        }
        if (s.length() % BASE != 0) {
            b.digits.push_back(stoi(s.substr(0, s.length() % BASE)));
        }
        return wave;
    }
public:
    friend BigInteger operator -(BigInteger b){
        b.sign = !b.sign;
        return b;
    }
    bool operator >(BigInteger const& b) {
        return ((digits.size() > b.digits.size()) || ((digits.size() == b.digits.size()) && (digits.back() > b.digits.back())));
    }
    BigInteger operator -(BigInteger b) {
        if (sign != b.sign) return ((!sign) ? (-(-*this + b)) : (*this + -b));
        if (!sign) return (-b - -*this);
        if (*this > b) return (-(b - *this));
        int carry = 0;
        for (int i = 0; i < digits.size() || carry; ++i) {
                int c = ((int) b.digits[i] - carry - (int)(i < digits.size() ? digits[i] : 0));
                carry = c < 0;
                b.digits[i] = c + (carry ? MAX : 0);
        }
        return -b;
    }
    BigInteger& operator -=(BigInteger const& b) {
        *this = *this - b;
        return *this;
    }
    BigInteger operator +(BigInteger b) {
        if (sign != b.sign) return ((!sign) ? (b - -*this) : (*this - -b));
        int carry = 0;
        for (int i = 0; i < max(b.digits.size(), digits.size()) || carry; ++i) {
                if (i == digits.size()) b.digits.push_back(0);
                b.digits[i] += carry + (i < digits.size() ? digits[i] : 0);
                carry = (b.digits[i] > MAX) || (b.digits[i] < digits[i]);
                if (carry) b.digits[i] -= MAX;
        }
        return b;
    }
    BigInteger& operator +=(BigInteger const& b) {
        *this = *this + b;
        return *this;
    }
    BigInteger () {
        sign = true;
    }
    BigInteger (int32_t b) {
        digits.push_back(abs(b));
        sign = (b > 0);
    }
};

int main() {
        BigInteger a, b;
        cin >> a >> b;
        cout << a << " + " << b << " = " << a + b << "\n";
        cout << a << " - " << b << " = " << a - b << "\n";
}
