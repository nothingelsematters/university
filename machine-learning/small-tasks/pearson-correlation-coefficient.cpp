#include <iostream>
#include <cmath>
#include <vector>
#include <algorithm>
#include <numeric>

using std::vector;

static constexpr double EPSILON = 1e-10;

double average(const vector<int>& v) {
    return std::accumulate(v.begin(), v.end(), 0.0) * 1.0 / v.size();
}

double dispersion(const vector<int>& v) {
    double mean = average(v);
    double sum = 0;
    for (const auto i : v) {
        sum += (i - mean) * (i - mean);
    }
    return sum;
}

double covariation(const vector<int>& a, const vector<int>& b) {
    double ma = average(a);
    double mb = average(b);
    double sum = 0;
    for (size_t i = 0; i < a.size(); ++i) {
        sum += (a[i] - ma) * (b[i] - mb);
    }
    return sum;
}

bool equals(double a, double b) {
    return std::abs(a - b) < EPSILON;
}

double pearson_coefficient(const vector<int>& a, const vector<int>& b) {
    double da = dispersion(a);
    double db = dispersion(b);
    return equals(da, 0) || equals(db, 0) ? 0 : (covariation(a, b) / std::sqrt(da * db));
}

int main() {
    unsigned int object_amount;
    std::cin >> object_amount;

    vector<int> x1(object_amount);
    vector<int> x2(object_amount);

    for (unsigned int i = 0; i < object_amount; ++i) {
        std::cin >> x1[i] >> x2[i];
    }

    std::cout << pearson_coefficient(x1, x2) << '\n';
    return 0;
}
