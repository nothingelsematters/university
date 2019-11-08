#include <iostream>
#include <algorithm>
#include <numeric>
#include <vector>

int main() {
    size_t object_amount, classes_amount, groups_amount;
    std::cin >> object_amount >> classes_amount >> groups_amount;

    size_t objects[object_amount];
    size_t class_object_amount[classes_amount + 1] = {};

    for (size_t i = 0; i < object_amount; ++i) {
        std::cin >> objects[i];
        ++class_object_amount[objects[i]];
    }
    std::partial_sum(class_object_amount, class_object_amount + classes_amount + 1, class_object_amount);
    std::vector<size_t> groups[groups_amount];

    for (size_t i = 0; i < object_amount; ++i) {
        size_t tmp = objects[i] - 1;
        size_t& cl = class_object_amount[tmp];
        groups[cl % groups_amount].push_back(i + 1);
        ++cl;
    }

    std::for_each(groups, groups + groups_amount, [] (auto x) {
        std::cout << x.size() << ' ';
        std::for_each(x.begin(), x.end(), [] (auto y) { std::cout << y << ' '; });
        std::cout << '\n';
    });

    return 0;
}
