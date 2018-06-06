#ifndef BASE_BIGINTEGER
#define BASE_BIGINTEGER

#pragma once

#include <iostream>
#include <vector>
#include <limits>

template<typename T, size_t Base = std::numeric_limits<T>::max()>
class BigInteger;
template<typename T, size_t Base>
std ::ostream& operator << (std::ostream& wave, BigInteger<T, Base> b);
template<typename T, size_t Base>
std ::istream& operator >> (std::istream& wave, BigInteger<T, Base> & b);

template<typename T, size_t Base>
class BigInteger {
private:
            size_t digits_capacity;
            unsigned int bits;
            std::vector<T> digits;
            bool abs_more(BigInteger const&);
            BigInteger subBigInteger(BigInteger const&, size_t, size_t);
            friend std ::ostream& operator << <>(std::ostream& wave, BigInteger b);
            friend std ::istream& operator >> <>(std::istream& wave, BigInteger& b);
            std::pair<T, T> multiply(T, T);
            std::pair<T, T> add(T, T);
 public:
            bool sign;
            void update_digit(size_t, T);
            BigInteger<T, Base>();
            template<typename NewT, size_t NewBase>
            explicit operator BigInteger<NewT, NewBase>();
            size_t size() const;
            T operator[](size_t id) const;
            BigInteger operator -();
            BigInteger operator +(BigInteger);
            BigInteger& operator +=(BigInteger);
            BigInteger operator -(BigInteger);
            BigInteger& operator -=(BigInteger);
            BigInteger operator *(BigInteger);
            BigInteger& operator *=(BigInteger);

};

#include "BigInteger.cpp"
#endif
