#include "BigInteger.h"
#include <iostream>

int main() {
    BigInteger<unsigned int, 16> a, b;

    std::cout << "First number :\n";
    std::cin >> a;
    std::cout << "Number a: ";
    for (int i = a.size() - 1; i >= 0; --i) std::cout << (int) a[i] << ' ';
    std::cout << "\nOriginal a in 10: " << a << "\nOne more number :\n";

    std::cin >> b;
    std::cout << "Number b: ";
    for (int i = b.size() - 1; i >= 0; --i) std::cout << (int) b[i] << ' ';
    std::cout << "\nOriginal b in 10: " << b << '\n';
    a *= b;
    std::cout << "result in x: ";
    for (int i = a.size() - 1; i >= 0; --i) std::cout << (int) a[i] << ' ';
    std::cout << "\n\na * b = " << a << '\n';
    return 0;
}
