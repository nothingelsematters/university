#include <iostream>
#include <vector>
#include <algorithm>

using std::vector;
using object_t = std::pair<int32_t, int32_t>;

vector<int32_t> rang(const vector<object_t> &objects, bool first) {
    size_t size = objects.size();
    vector<object_t> new_objects(size);
    for (size_t i = 0; i < size; ++i) {
        new_objects[i] = {first ? objects[i].first : objects[i].second, i};
    }
    std::sort(new_objects.begin(), new_objects.end());
    vector<int32_t> result(size, 0);

    size_t counter = 0;
    for (size_t i = 1; i < size; ++i) {
        if (new_objects[i - 1].first != new_objects[i].first) {
            ++counter;
        }
        result[new_objects[i].second] = counter;
    }
    return result;
}

double spearman(const vector<object_t> &objects) {
    size_t size = objects.size();
    if (size < 2) {
        return 0.0;
    }

    vector<int32_t> first_rang = rang(objects, true);
    vector<int32_t> second_rang = rang(objects, false);

    double sum = 0.0;
    for (size_t i = 0; i < size; ++i) {
        long long difference = first_rang[i] - second_rang[i];
        sum += difference * difference;
    }
    return 1 - 6 * sum / ((size - 1.0) * size * (size + 1.0));
}

int main() {
    size_t object_amount;
    std::cin >> object_amount;

    vector<object_t> objects(object_amount);
    for (auto &i : objects) {
        std::cin >> i.first >> i.second;
    }

    std::cout << spearman(objects) << '\n';
    return 0;
}
