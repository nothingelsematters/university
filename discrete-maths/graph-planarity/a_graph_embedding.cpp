#include <iostream>

bool bipartition(std::pair<size_t, size_t>* edges, size_t* hamilton_cycle, bool* partition, size_t edges_size,
        bool* visited, size_t current_index, bool first_part = true) {

    if (visited[current_index]) {
        return partition[current_index] == first_part;
    }
    visited[current_index] = true;
    partition[current_index] = first_part;
    auto current = edges[current_index];

    std::pair<size_t, size_t> hamilton_current({hamilton_cycle[current.first], hamilton_cycle[current.second]});
    for (size_t i = 0; i < edges_size; ++i) {
        if (!((hamilton_current.first < hamilton_cycle[edges[i].first] &&
            hamilton_cycle[edges[i].first] < hamilton_current.second &&
            hamilton_current.second < hamilton_cycle[edges[i].second]) ||
                (hamilton_current.first > hamilton_cycle[edges[i].first] &&
                hamilton_cycle[edges[i].second] > hamilton_current.first &&
                hamilton_current.second > hamilton_cycle[edges[i].second]))) {
            continue;
        }
        if (!bipartition(edges, hamilton_cycle, partition, edges_size, visited, i, !first_part)) {
            return false;
        }
    }

    return true;
}

int main() {
    size_t vertex_size;
    size_t edges_size;
    std::cin >> vertex_size >> edges_size;

    std::pair<size_t, size_t>* edges = new std::pair<size_t, size_t>[edges_size];
    size_t* hamilton_cycle = new size_t[vertex_size];
    bool* partition = new bool[edges_size];
    std::fill(partition, partition + edges_size, false);

    for (size_t i = 0; i < edges_size; ++i) {
        std::cin >> edges[i].first >> edges[i].second;
    }

    for (size_t i = 0; i < vertex_size; ++i) {
        size_t vertex;
        std::cin >> vertex;
        hamilton_cycle[vertex - 1] = i;
    }

    for (size_t i = 0; i < edges_size; ++i) {
        if (hamilton_cycle[--edges[i].first] > hamilton_cycle[--edges[i].second]) {
            std::swap(edges[i].first, edges[i].second);
        }
    }

    bool* visited = new bool[edges_size];
    std::fill(visited, visited + edges_size, false);
    for (size_t i = 0; i < edges_size; ++i) {
        if (!visited[i] && !bipartition(edges, hamilton_cycle, partition, edges_size,
                visited, i)) {
            std::cout << "NO\n";
            return 0;
        }
    }

    std::cout << "YES\n";
    for (size_t i = 0; i < vertex_size; ++i) {
        std::cout << 2 * hamilton_cycle[i] << ' ' << 0 << ' ';
    }
    std::cout << '\n';

    for (size_t i = 0; i < edges_size; ++i) {
        size_t edge_begin = hamilton_cycle[edges[i].first];
        size_t edge_end = hamilton_cycle[edges[i].second];
        std::cout << edge_begin + edge_end << ' ' <<
            (partition[i] ? 1 : -1) * abs(edge_begin - edge_end) << '\n';
    }
}
