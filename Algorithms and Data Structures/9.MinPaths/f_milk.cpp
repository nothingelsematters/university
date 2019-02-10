#include <iostream>
#include <list>
#include <set>
#include <algorithm>

const long long MAX = 1e18;

struct Graph {
    long long* dijkstra(size_t s) {
        long long* dist = new long long[vertex_size];
        std::fill(dist, dist + vertex_size, MAX);
        dist[s] = 0;

        std::set<std::pair<long long, size_t>> heap;
        for (auto i: edges[s]) {
            heap.emplace(i.second, i.first);
        }

        for (size_t i = 1; i < vertex_size && !heap.empty(); ++i) {
            long long min = heap.begin()->first;
            size_t index = heap.begin()->second;
            while (!heap.empty() && dist[index] != MAX) {
                heap.erase(heap.begin());
                min = heap.begin()->first;
                index = heap.begin()->second;
            }

            if (heap.empty()) {
                break;
            }

            dist[index] = min;
            for (auto j: edges[index]) {
                if (dist[j.first] > min + j.second) {
                    heap.emplace(min + j.second, j.first);
                }
            }
        }

        return dist;
    }

    friend std::istream& operator>>(std::istream& in, Graph& g) {
        in >> g.vertex_size >> g.edge_size;
        g.edges = new std::list<std::pair<size_t, long long>>[g.vertex_size];
        for (size_t i = 0; i < g.edge_size; ++i) {
            size_t from, to;
            long long weight;
            in >> from >> to >> weight;
            g.edges[from - 1].emplace_back(to - 1, weight);
            g.edges[to - 1].emplace_back(from - 1, weight);
        }

        return in;
    }

    ~Graph() {
        delete[] edges;
    }

    size_t vertex_size;
    size_t edge_size;
    std::list<std::pair<size_t, long long>>* edges;
};

int main() {
    Graph g;
    long long a, b, c;
    std::cin >> g >> a >> b >> c;
    long long* result = g.dijkstra(--a);
    long long a_b = result[--b];
    long long a_c = result[--c];
    delete[] result;
    result = g.dijkstra(b);
    long long b_c = result[c];
    if ((a_c == MAX && b_c == MAX) ||
            (a_c == MAX && a_b == MAX) ||
            (a_b == MAX && b_c == MAX)) {
        std::cout << -1;
    } else {
        long long min;
        if (a_b == MAX) {
            min = b_c + a_c;
        } else if (b_c == MAX) {
            min = a_b + a_c;
        } else if (a_c == MAX) {
            min = a_b + b_c;
        } else {
            min = b_c + a_b;
            min = std::min(min, b_c + a_c);
            min = std::min(min, a_b + a_c);
        }
        std::cout << min;
    }
    return 0;
}
