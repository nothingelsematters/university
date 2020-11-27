#include <iostream>
#include <vector>
#include <unordered_map>
#include <algorithm>
#include <numeric>

#define int32_t long long
#define uint32_t long long
#define size_t long long

using std::vector;
using ll = long long;
using object_t = std::pair<int32_t, uint32_t>;

ll internal(const vector<object_t> &objects) {
    std::unordered_map<int32_t, vector<uint32_t>> classes;
    for (const auto &[x, y] : objects) {
        classes[y].emplace_back(x);
    }
    for (auto &[_, v] : classes) {
        std::sort(v.begin(), v.end());
    }

    ll sum = 0;
    for (const auto &[_, v] : classes) {
        ll suffix = std::accumulate(v.begin(), v.end(), 0ll);
        ll prefix = 0;
        for (size_t i = 0; i < v.size(); ++i) {
            suffix -= v[i];
            prefix += v[i];
            ll pr = i + 1, su = v.size() - pr;
            sum += (v[i] * pr - prefix) + (suffix - v[i] * su);
        }
    }
    return sum;
}

ll external(const vector<object_t> &objects) {
    vector<object_t> copied_objects = objects;
    std::unordered_map<int32_t, ll> prefix, suffix, prefix_count, suffix_count;
    ll prefix_sum = 0, suffix_sum = 0;
    std::sort(copied_objects.begin(), copied_objects.end());

    for (const auto [x, y] : copied_objects) {
        suffix[y] += x;
        ++suffix_count[y];
        suffix_sum += x;
    }

    ll sum = 0;
    for (size_t i = 0; i < copied_objects.size(); ++i) {
        const auto &[x, y] = copied_objects[i];
        suffix_sum -= x;
        prefix_sum += x;
        suffix[y] -= x;
        prefix[y] += x;
        --suffix_count[y];
        ++prefix_count[y];

        ll tpc = i + 1, tsc = copied_objects.size() - tpc, gpc = tpc - prefix_count[y],
            gsc = tsc - suffix_count[y], gps = prefix_sum - prefix[y], gss = suffix_sum - suffix[y];

        sum += gpc * x - gps + gss - gsc * x;
    }
    return sum;
}

int main() {
    size_t value_amount, object_amount;
    std::cin >> value_amount >> object_amount;

    vector<object_t> objects(object_amount);
    for (auto &[f, s] : objects) {
        std::cin >> f >> s;
    }
    std::cout << internal(objects) << '\n' << external(objects) << '\n';
}
