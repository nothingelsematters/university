#include <fstream>
#include <iostream>
#include <list>
#include <set>

struct Edge {
    Edge(size_t from,std::pair<size_t, size_t> to) : from(from), to(to) {}

    size_t from;
    std::pair<size_t, size_t> to;
};

int main() {
    std::ifstream fin("dwarf.in");
    long long* weight;
    long long* min_weight;
    std::list<Edge>* edges;
    size_t vertex_size;
    size_t edge_size;
    fin >> vertex_size >> edge_size;

    edges = new std::list<Edge>[vertex_size];
    min_weight = new long long[vertex_size];

    std::fill(min_weight, min_weight + vertex_size, 1e17);
    std::set<std::pair<long long, size_t>> dijk;

    for (size_t i = 0; i < vertex_size; ++i) {
        long long temp;
        fin >> temp;
        dijk.emplace(temp, i);
    }

    for (size_t i = 0; i < edge_size; ++i) {
        size_t from, to_first, to_second;
        fin >> from >> to_first >> to_second;
        edges[to_first - 1].push_back(Edge(from - 1, {to_first - 1, to_second - 1}));
        edges[to_second - 1].push_back(Edge(from - 1, {to_second - 1, to_first - 1}));
    }

    while (!dijk.empty()) {
        auto [min, index] = *(dijk.begin());
        while (!dijk.empty() && min_weight[index] != 1e17) {
            dijk.erase(dijk.begin());
            min = dijk.begin()->first;
            index = dijk.begin()->second;
        }
        if (dijk.empty()) {
            break;
        }

        min_weight[index] = min;
        dijk.erase(dijk.begin());
        for (auto j: edges[index]) {
            if (min_weight[j.to.second] != 1e17 && min_weight[j.from] == 1e17) {
                dijk.emplace(min_weight[j.to.second] + min_weight[j.to.first], j.from);
            }
        }
    }
    std::ofstream fout("dwarf.out");

    fout << (min_weight[0] == 1e17 ? -1 : min_weight[0]);

    return 0;
}
