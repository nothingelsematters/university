#include <iostream>
#include <climits>
#include <list>
#include <vector>

struct Graph {
    std::vector<long long> ford_bellman(size_t s, size_t k) {
        std::vector<std::vector<long long>> dist(k + 1, std::vector<long long>(vertex_size, INT_MAX));
        dist[0][s] = 0;

        for (size_t count = 0; count < k; ++count) {
            for (size_t i = 0; i < vertex_size; ++i) {
                for (auto j: edges[i]) {
                    if (dist[count][i] != INT_MAX) {
                        dist[count + 1][j.first] = std::min(dist[count + 1][j.first],
                            dist[count][i] + j.second);
                    }
                }
            }
        }
        return std::move(dist[k]);
    }

    friend std::istream& operator>>(std::istream& in, Graph& g) {
        g.edges = new std::list<std::pair<size_t, long long>>[g.vertex_size];
        for (size_t i = 0; i < g.edge_size; ++i) {
            size_t from, to;
            long long weight;
            in >> from >> to >> weight;
            g.edges[from - 1].emplace_back(to - 1, weight);
        }

        return in;
    }

    size_t vertex_size;
    size_t edge_size;
    std::list<std::pair<size_t, long long>>* edges;
};

int main() {
    size_t k, s;
    Graph g;
    std::cin >> g.vertex_size >> g.edge_size >> k >> s >> g;
    for (auto i: g.ford_bellman(s - 1, k)) {
        std::cout << (i == INT_MAX ? -1 : i) << '\n';
    }
    return 0;
}
