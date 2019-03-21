#include <iostream>
#include <climits>
#include <list>
#include <vector>

struct Graph {
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

bool* not_minus;
Graph g;

void dfs(int s) {
    not_minus[s] = true;
    for (auto i: g.edges[s]) {
        if (!not_minus[i.first])
            dfs(i.first);
    }
}

long long* ford_bellman(size_t s) {
    long long* dist = new long long[g.vertex_size];
    size_t* par = new size_t[g.vertex_size];
    std::fill(dist, dist + g.vertex_size, LONG_LONG_MAX);
    std::fill(par, par + g.vertex_size, -1);
    dist[s] = 0;

    for (size_t count = 0; count < g.vertex_size - 1; ++count) {
        for (size_t i = 0; i < g.vertex_size; ++i) {
            if (dist[i] == LONG_LONG_MAX) {
                continue;
            }
            for (auto j: g.edges[i]) {
                if (dist[j.first] > dist[i] + j.second) {
                    dist[j.first] = dist[i] + j.second;
                    par[j.first] = i;
                }
            }
        }
    }
    long long new_dist[g.vertex_size];
    for (int i = 0; i < g.vertex_size; ++i) {
        new_dist[i] = dist[i];
    }
    for (size_t i = 0; i < g.vertex_size; ++i) {
        if (new_dist[i] == LONG_LONG_MAX) {
            continue;
        }
        for (auto j: g.edges[i]) {
            if (new_dist[j.first] > new_dist[i] + j.second) {
                new_dist[j.first] = new_dist[i] + j.second;
            }
        }
    }
    for (int i = 0; i < g.vertex_size; ++i) {
        if (dist[i] != new_dist[i]) {
            dfs(i);
        }
    }

    return dist;
}



int main() {
    size_t s;
    std::cin >> g.vertex_size >> g.edge_size >> s >> g;
    not_minus = new bool[g.vertex_size];
    std::fill(not_minus, not_minus + g.vertex_size, false);

    long long* result = ford_bellman(--s);
    for (int i = 0; i < g.vertex_size; ++i) {
        if (not_minus[i]) {
            std::cout << '-';
        } else if (result[i] == LONG_LONG_MAX) {
            std::cout << '*';
        } else {
            std::cout << result[i];
        }
        std::cout << '\n';
    }
    return 0;
}
