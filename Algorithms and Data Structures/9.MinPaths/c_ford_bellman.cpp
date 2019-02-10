#include <iostream>
#include <climits>
#include <list>
#include <algorithm>

struct Graph {
    std::list<size_t> ford_bellman(size_t s) {
        std::list<size_t> result;
        int* dist = new int[vertex_size];
        size_t* par = new size_t[vertex_size];
        std::fill(dist, dist + vertex_size, 1);
        std::fill(par, par + vertex_size, -1);
        dist[s] = 0;

        size_t start = -1;
        for (size_t count = 0; count < vertex_size; ++count)
        for (size_t i = 0; i < vertex_size; ++i) {
            for (auto j: edges[i]) {
                if (dist[j.first] > dist[i] + j.second) {
                    dist[j.first] = dist[i] + j.second;
                    par[j.first] = i;
                    start = j.first;
                }
            }
        }

        for (size_t i = 0; i < vertex_size; ++i) {
            for (auto j: edges[i]) {
                if (dist[j.first] > dist[i] + j.second) {
                    start = j.first;
                    for (size_t k = 0; k < vertex_size; ++k) {
                        start = par[start];
                    }
                    for (size_t temp = start; !(temp == start && result.size() > 0); temp = par[temp]) {
                        result.push_back(temp);
                    }
                    std::reverse(result.begin(), result.end());
                    return result;
                }
            }
        }

        return result;
    }

    friend std::istream& operator>>(std::istream& in, Graph& g) {
        in >> g.vertex_size;
        g.edges = new std::list<std::pair<size_t, int>>[g.vertex_size];
        for (size_t i = 0; i < g.vertex_size; ++i) {
            for (size_t j = 0; j < g.vertex_size; ++j) {
                int weight;
                in >> weight;
                if (weight != 100000)
                g.edges[i].emplace_back(j, weight);
            }
        }

        return in;
    }

    size_t vertex_size;
    std::list<std::pair<size_t, int>>* edges;
};

int main() {
    Graph g;
    std::cin >> g;
    for (size_t i = 0; i < g.vertex_size; ++i) {
        std::list<size_t> result = g.ford_bellman(i);
        if (result.size() > 0) {
            std::cout << "YES\n" << result.size() << '\n';
            for (auto i: result) {
                std::cout << i + 1 << ' ';
            }
            return 0;
        }
    }

    std::cout << "NO";
    return 0;
}
