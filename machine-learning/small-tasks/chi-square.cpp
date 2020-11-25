#include <iostream>
#include <vector>
#include <map>
#include <cmath>

int main () {
    size_t x_features, y_features, object_amount;
    std::cin >> x_features >> y_features >> object_amount;

    std::vector<unsigned int> first(object_amount), second(object_amount);
    std::vector<double> f_first(x_features, 0.0), f_second(y_features, 0.0);
    std::map<std::pair<unsigned int, unsigned int>, unsigned int> cross;

    for (size_t i = 0; i < object_amount; ++i) {
        std::cin >> first[i] >> second[i];
        ++f_first[--first[i]];
        ++f_second[--second[i]];
        ++cross[{first[i], second[i]}];
    }

    for (auto &i : f_first) {
        i /= object_amount;
    }
    for (auto &i : f_second) {
        i /= object_amount;
    }

    double result = object_amount;
    for (auto [value, r] : cross) {
        auto [x, y] = value;
        double expected = 1.0 * object_amount * f_first[x] * f_second[y];
        double difference = r - expected;
        result += difference * difference / expected - expected;
    }
    std::cout << std::fixed << result << '\n';

    return 0;
}
