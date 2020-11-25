#include <iostream>
#include <iomanip>
#include <vector>
#include <map>
#include <cmath>

int main() {
    size_t x_features, y_features, object_amount;
    std::cin >> x_features >> y_features >> object_amount;

    std::vector<std::pair<unsigned int, unsigned int>> objects(object_amount);
    std::map<unsigned int, double> probability_x;
    std::map<std::pair<unsigned int, unsigned int>, double> probability_xy;

    for (auto &i : objects) {
        std::cin >> i.first >> i.second;
        probability_x[i.first] += 1.0 / object_amount;
        probability_xy[i] += 1.0 / object_amount;
    }

    double sum = 0.0;
    for (auto [xy, prob_xy] : probability_xy) {
        auto x = xy.first;
        sum += -prob_xy * (std::log(prob_xy) - std::log(probability_x[x]));
    }
    std::cout << std::fixed << std::setprecision(10) << sum << '\n';
}
