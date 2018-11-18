#include <iostream>
#include <climits>
#include <list>
#include <set>
#include <algorithm>

struct Graph {
    int* dijkstra() {
        int* dist= new int[vertex_size];
        dist[0] = 0;
        std::fill(dist + 1, dist + vertex_size, INT_MAX);

        std::set<std::pair<size_t, int>> heap;
        for (auto i: edges[0]) {
            heap.emplace(i.second, i.first);
        }

        for (size_t i = 1; i < vertex_size; ++i) {
            auto [min, index] = *(heap.begin());
            while (dist[index] != INT_MAX) {
                heap.erase(heap.begin());
                min = heap.begin()->first;
                index = heap.begin()->second;
            }

            dist[index] = min;
            heap.erase(heap.begin());
            for (auto j: edges[index]) {
                if (dist[j.first] == INT_MAX) {
                    heap.emplace(min + j.second, j.first);
                }
            }
        }

        return dist;
    }

    friend std::istream& operator>>(std::istream& in, Graph& g) {
        in >> g.vertex_size >> g.edge_size;
        g.edges = new std::list<std::pair<size_t, int>>[g.vertex_size];
        for (size_t i = 0; i < g.edge_size; ++i) {
            size_t from, to;
            int weight;
            in >> from >> to >> weight;
            g.edges[from - 1].emplace_back(to - 1, weight);
            g.edges[to - 1].emplace_back(from - 1, weight);
        }

        return in;
    }

    size_t vertex_size;
    size_t edge_size;
    std::list<std::pair<size_t, int>>* edges;
};

int main() {
    Graph g;
    std::cin >> g;
    int* result = g.dijkstra();
    for (size_t i = 0; i < g.vertex_size; ++i) {
        std::cout << result[i] << ' ';
    }
    return 0;
}
