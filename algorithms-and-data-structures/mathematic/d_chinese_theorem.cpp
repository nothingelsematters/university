#include <iostream>

int main() {
    long long first, second, first_module, second_module;
    std::cin >> first >> second >> first_module >> second_module;
    while (first % second_module != second) {
        first += first_module;
    }
    std::cout << first << '\n';
    return 0;
}
