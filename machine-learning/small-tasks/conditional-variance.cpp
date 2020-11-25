#include <iostream>
#include <algorithm>

int main() {
    size_t values, object_amount;
    std::cin >> values >> object_amount;

    std::pair<int, int> objects[object_amount];
    for (auto &[f, s] : objects) {
        std::cin >> f >> s;
    }
    double y_second_moment = 0.0;
    for (auto [_, s] : objects) {
        y_second_moment += s / static_cast<double>(object_amount) * s;
    }

    std::pair<double, double> e_y_x[values] = {};
    std::fill(e_y_x, e_y_x + values, std::pair<double, double>{0.0, 0.0});
    for (auto [f, s] : objects) {
        e_y_x[f - 1].second += 1.0 / object_amount;
        e_y_x[f - 1].first += s / static_cast<double>(object_amount);
    }
    double condition_square_expectation = 0.0;
    for (auto [expectation_y, p] : e_y_x) {
        if (p != 0) {
            condition_square_expectation += expectation_y * expectation_y / p;
        }
    }
    std::cout << std::fixed << y_second_moment - condition_square_expectation << '\n';
    return 0;
}
