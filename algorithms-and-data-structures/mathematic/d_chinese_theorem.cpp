#include <iostream>

int main() {
    int first, second, first_module, second_module;
    std::cin >> first >> second >> first_module >> second_module;
    first %= first_module;
    while (first % second_module != second) {
        first += first_module;
    }
    std::cout << first << '\n';
    return 0;
}
