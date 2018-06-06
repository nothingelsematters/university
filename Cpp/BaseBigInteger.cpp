#include <iostream>
#include <cmath>
#include <cstdlib>
#include <limits>
#include "BigInteger.h"

template<typename T, size_t Base>
void BigInteger<T, Base>::update_digit(size_t index, T value) {
        if (index / digits_capacity + 1 > digits.size())
                digits.resize(index / digits_capacity + 1);
        T arr = digits[index / digits_capacity];
        arr &= std::numeric_limits<T>::max() - (((1 << bits) - 1) << (bits * (index % digits_capacity)));
        arr += value << (bits * (index % digits_capacity));
        digits[index / digits_capacity] = arr;
}

template<typename T, size_t Base>
size_t BigInteger<T, Base>::size() const {
        short i = 0;
        for (T last = digits.back(); (last != 0); ++i, last >>= bits) {}
        return (digits.size() - 1) * digits_capacity + i;
}

template<typename T, size_t Base>
bool BigInteger<T, Base>::abs_more(BigInteger<T, Base> const& b) {
        if (size() > b.size()) return true;
        if (b.size() > size()) return false;
        for (ptrdiff_t i = b.size() - 1; i >= 0; --i)
                if ((*this)[i] != b[i]) return ((*this)[i] > b[i]);
        return false;
}

template<typename T, size_t Base>
BigInteger<T, Base>::BigInteger() {
        digits.push_back(0);
        sign = true;
        bool ones = false;
        size_t temp = Base;
        for (bits = 0; temp != 1; ++bits, temp >>= 1)
                if (temp & 1) ones = true;
        bits += ones;
        digits_capacity = std::numeric_limits<T>::digits / bits;
}

template<typename T, size_t Base>
template<typename NewT1, size_t NewBase>
BigInteger<T, Base>::operator BigInteger<NewT1, NewBase>() {
        BigInteger<NewT1, NewBase> b;
        BigInteger<T, Base> numerator, temp;
        numerator = *this;
        b.sign = (*this).sign;
        size_t digits_counter = 0;
        while (numerator.size() > 1 || numerator[0] != 0) {
                if (numerator.digits.back() == 0) numerator.digits.resize(numerator.digits.size() - 1);
                temp.digits.resize(numerator.digits.size());
                size_t num;
                for (ptrdiff_t i = numerator.size() - 1; i >= 0; --i) {
                        num = 0;
                        for (size_t j = i; j < numerator.size(); ++j)
                                num += numerator[j] * pow(Base, j - i);
                        if (num / NewBase) {
                                temp.update_digit(i, num / NewBase);
                                size_t temp_size = numerator.size();
                                for (size_t j = i; j < temp_size; ++j)
                                        numerator.update_digit(j, (num % NewBase) % (int) pow(Base, j - i + 1) / pow(Base, j - i));
                        }
                }
                b.update_digit(digits_counter++, num % NewBase);
                numerator.digits = temp.digits;
                temp.digits.clear();
        }
        return b;
}

template<typename T, size_t Base>
T BigInteger<T, Base>::operator [](size_t id) const {
        if (id >= size()) return 0;
        T ans = digits[id / digits_capacity];
        ans >>= bits * (id % digits_capacity);
        ans = ans & ((1 << bits) - 1);
        return ans;
}

template<typename T, size_t Base>
BigInteger<T, Base> BigInteger<T, Base>::operator -(){
        BigInteger<T, Base> b = *this;
        b.sign = !b.sign;
        return b;
}

template<typename T, size_t Base>
BigInteger<T, Base> BigInteger<T, Base>::operator +(BigInteger<T, Base> b) {
        if (sign != b.sign) return ((!sign) ? (b - -*this) : (*this - -b));
        if ((*this).abs_more(b)) return b + *this;
        unsigned int carry = 0;
        for (size_t i = 0; i < std::max(b.size(), size()) || carry; ++i) {
                unsigned short temp = carry;
                carry = ((b[i] + carry + (i < size() ? (*this)[i] : 0)) >= Base);
                b.update_digit(i, b[i] + temp + (i < size() ? (*this)[i] : 0) + (carry ? -Base : 0));
        }
        return b;
}

template<typename T, size_t Base>
BigInteger<T, Base>& BigInteger<T, Base>::operator +=(BigInteger<T, Base>  b) {
        *this = *this + b;
        return *this;
}

template<typename T, size_t Base>
BigInteger<T, Base> BigInteger<T, Base>::operator -(BigInteger<T, Base> b) {
        if (sign != b.sign) return ((!sign) ? (-(-*this + b)) : (*this + -b));
        if (!sign) return (-b - -*this);
        if (this->abs_more(b)) return (-(b - *this));
        bool carry = false;
        for (size_t i = 0; i < size() || carry; ++i) {
                ptrdiff_t c = ((ptrdiff_t) b[i] - carry - (ptrdiff_t)(i < size() ? (*this)[i] : 0));
                carry = c < 0;
                b.update_digit(i, c + (carry ? Base : 0));
        }
        size_t resize = b.digits.size();
        while (b.digits[resize - 1] == 0 && resize > 1) --resize;
        b.digits.resize(resize);
        return -b;
}

template<typename T, size_t Base>
BigInteger<T, Base>& BigInteger<T, Base>::operator -=(BigInteger<T, Base> b) {
        *this = *this - b;
        return *this;
}

template<typename T, size_t Base>
std::pair<T, T> BigInteger<T, Base>::add(T a, T b) {
        if (a >= Base - b)
                return {a - (Base - b), 1};
        return {a + b, 0};
}

template<typename T, size_t Base>
std::pair<T, T> BigInteger<T, Base>::multiply(T a, T b) {
        T ans = 0;
        T carry = 0;
        T value = 0;
        while (b) {
                if (b & 1) {
                        T sum = add(ans, a).first;
                        T c = add(ans, a).second;
                        ans = sum;
                        value += carry + c;
                }
                T sum = add(a, a).first;
                T c = add(a, a).second;
                a = sum;
                carry = (carry << 1) + c;
                b >>= 1;
        }
        return {value, ans};
}

template<typename T, size_t Base>
BigInteger<T, Base> BigInteger<T, Base>::operator *(BigInteger<T, Base> b) {
        BigInteger<T, Base> result;
        for (size_t i = 0; i < size(); ++i)
                for (size_t j = 0, carry = 0; j < b.size() || carry; ++j) {
                        std::pair<T, T> x = multiply((*this)[i], (j < b.size() ? b[j] : 0));
                        T temp = result[i + j];
                        std::pair<T, T> y = add(temp, x.second);
                        x.first += y.second;
                        temp = y.first;
                        y = add(temp, carry);
                        x.first += y.second;
                        temp = y.first;
                        result.update_digit(i + j, temp);
                        carry = x.first;
                }
        return result;
}

template<typename T, size_t Base>
BigInteger<T, Base>& BigInteger<T, Base>::operator *=(BigInteger<T, Base> b) {
        *this = *this * b;
        return *this;
}

template<typename T, size_t Base>
std::istream& operator >>(std::istream& wave, BigInteger<T, Base>& b) {
        std::string number;
        wave >> number;
        BigInteger<unsigned int, 10> temp;
        if (number[0] == '-') {
                temp.sign = false;
                number = number.substr(1, number.length() - 1);
        }
        for (size_t i = 0; i < number.length(); ++i)
                temp.update_digit(i, stoi(number.substr(number.length() - i - 1, 1)));
        b = BigInteger<T, Base>(temp);
        return wave;
}

template<typename T, size_t Base>
std::ostream& operator <<(std::ostream& wave, BigInteger<T, Base> b) {
        if (b.size() == 0) {
                wave << '0';
                return wave;
        }
        BigInteger<unsigned int, 10> temp = BigInteger<unsigned int, 10> (b);
        if (!b.sign) wave << '-';
        for (ptrdiff_t i = temp.size() - 1; i >= 0; --i) wave << temp[i];
        return wave;
}
