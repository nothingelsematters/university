#include <iostream>
#include <vector>
#include <climits>

struct Graph {
    size_t size;
    std::vector<std::vector<int>> weight;

    friend std::istream& operator>>(std::istream& in, Graph& g) {
        in >> g.size;
        g.weight.resize(g.size, std::vector<int>(g.size, INT_MAX));
        for (size_t i = 0; i < g.size; ++i) {
            for (int j = 0; j < g.size; ++j) {
                in >> g.weight[i][j];
            }
        }
    }
};

std::vector<std::vector<int>> floyd(Graph const& g) {
    std::vector<std::vector<int>> d = g.weight;
    for (size_t i = 0; i < g.size; ++i) {
        for (size_t u = 0; u < g.size; ++u) {
            for (size_t v = 0; v < g.size; ++v) {
                d[u][v] = std::min(d[u][v], d[u][i] + d[i][v]);
            }
        }
    }

    return std::move(d);
}

int main() {
    Graph g;
    std::cin >> g;

    for (auto i: floyd(g)) {
        for (auto j: i) {
            std::cout << j << ' ';
        }
        std::cout << '\n';
    }
    return 0;
}
