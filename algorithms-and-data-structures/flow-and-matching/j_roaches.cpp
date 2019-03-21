#include <iostream>
#include <algorithm>
#include <memory>
#include <limits>
#include <vector>
#include <list>
#include <queue>

class Graph {
public:
    Graph(size_t capacity) : vertex_size(capacity) {
        edges.resize(capacity);
        std::for_each(edges.begin(), edges.end(), [capacity] (std::vector<long long>& v) { v.resize(capacity); });
    }
    
    std::unique_ptr<long long[]> dijkstra() {
        std::unique_ptr<long long[]> dist = std::make_unique<long long[]>(vertex_size);
        dist[0] = 0;
        std::fill(dist.get() + 1, dist.get() + vertex_size, std::numeric_limits<long long>::max());

        bool visited[vertex_size];
        std::fill(visited, visited + vertex_size, false);
        for (size_t i = 0; i < vertex_size; ++i) {
            size_t min_vertex = -1;
            for (size_t j = 0; j < vertex_size; ++j) {
                if (!visited[j] && (min_vertex == -1 || dist[j] < dist[min_vertex])) {
                    min_vertex = j;
                }
            }

            visited[min_vertex] = true;
            for (size_t j = 0; j < vertex_size; ++j) {
                if (j != min_vertex) {
                    dist[j] = std::min(dist[j], dist[min_vertex] + edges[min_vertex][j]);
                }
            }
        }

        return dist;
    }
    
    void add_edge(size_t from, size_t to, size_t weight) {
        edges[from][to] = weight;
        edges[to][from] = weight;
    }

    size_t vertex_size;
    size_t edge_size;
    std::vector<std::vector<long long>> edges;
};

class Rectangular {
public:
    Rectangular() {}
    
    long long x1, x2, y1, y2;
};

long long distance(Rectangular const& first, Rectangular const& second) {
    long long min_dist_x = (first.x1 <= second.x1) ? second.x1 - first.x2 : first.x1 - second.x2;
    long long min_dist_y = (first.y1 <= second.y1) ? second.y1 - first.y2 : first.y1 - second.y2;
    long long min_dist = std::max(min_dist_x, min_dist_y);
    return min_dist > 0 ? min_dist : 0;
}

unsigned short a, b;

decltype(a + b) main() {
    size_t things_quantity, width;
    std::cin >> things_quantity >> width;
    Rectangular things[things_quantity];
    Graph g(things_quantity + 2);
    
    for (size_t i = 0; i < things_quantity; ++i) {
        Rectangular& thing = things[i];
        std::cin >> thing.x1 >> thing.y1 >> thing.x2 >> thing.y2;
        long long min_y = std::min(thing.y1, thing.y2);
        long long max_y = std::max(thing.y1, thing.y2);
        g.add_edge(0, i + 2, width - max_y);
        g.add_edge(1, i + 2, min_y);
    }
    
    g.add_edge(0, 1, width);
    for (size_t i = 0; i < things_quantity; ++i) {
        for (size_t j = 0; j < things_quantity; ++j) {
            if (i != j) {
                g.add_edge(i + 2, j + 2, distance(things[i], things[j]));
            }
        }
    }
    
    auto result = g.dijkstra();
    std::cout << (result > 0 ? result[1] : 0) << '\n';
    
    return 0;
}