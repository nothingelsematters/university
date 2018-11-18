#include <iostream>
#include <limits>
#include <cmath>

double dist(std::pair<int, int> a, std::pair<int, int> b) {
    int square1 = a.first - b.first;
    int square2 = a.second - b.second;
    return std::sqrt(square1 * square1 + square2 * square2);
}

int main() {
    int n;
    std::cin >> n;

    std::pair<int, int> point[n];
    for (int i = 0; i < n; ++i) {
        std::cin >> point[i].first >> point[i].second;
    }

    bool visited[n] = {0};
    int from[n];
    double min_edge[n];
    std::fill(min_edge, min_edge + n, std::numeric_limits<double>::max());
    std::fill(from, from + n, -1);
    min_edge[0] = 0;


    double distance = 0;
    for (int i = 0; i < n; ++i) {
        int v = -1;
        for (int j = 0; j < n; ++j) {
            if (!visited[j] && (v == -1 || min_edge[j] < min_edge[v])) {
                v = j;
            }
        }

        visited[v] = true;
        distance += (from[v] == -1 ? 0 : dist(point[v], point[from[v]]));
        for (int j = 0; j < n; ++j) {
            double temp = dist(point[v], point[j]);
            if (temp < min_edge[j]) {
                min_edge[j] = temp;
                from[j] = v;
            }
        }
    }

    std::cout << distance;
    return 0;
}
