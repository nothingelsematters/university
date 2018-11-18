#include <iostream>
#include <fstream>
#include <climits>
#include <vector>
#include <list>

struct OrientedGraph {
    friend std::istream& operator>>(std::istream& in, OrientedGraph& graph) {
        in >> graph.vertex_quantity >> graph.edges_quantity;
        graph.edges.resize(graph.vertex_quantity, std::vector<long long>(
            graph.vertex_quantity, INT_MAX));

        for (long long i = 0; i < graph.edges_quantity; ++i) {
            long long start, end, weight;
            in >> start >> end >> weight;
            graph.edges[start - 1][end - 1] = std::min(weight,
                graph.edges[start - 1][end - 1]);
        }
        return in;
    }

    long long vertex_quantity;
    long long edges_quantity;
    std::vector<std::vector<long long>> edges;
};

void straight_dfs(OrientedGraph const& graph, long long v, bool* visited,
        std::list<long long>& order) {
    visited[v] = true;

    for (long long i = 0; i < graph.vertex_quantity; ++i) {
        if (graph.edges[v][i] != INT_MAX && !visited[i] && graph.edges[v][i] == 0) {
            straight_dfs(graph, i, visited, order);
        }
    }
    order.push_front(v);
}

void reversed_dfs(OrientedGraph const& graph, long long v, long long* component, long long num) {
    component[v] = num;
    for (long long i = 0; i < graph.vertex_quantity; ++i) {
        if (graph.edges[i][v] != INT_MAX && component[i] == -1 && graph.edges[i][v] == 0) {
            reversed_dfs(graph, i, component, num);
        }
    }
}

OrientedGraph condensation(OrientedGraph const& graph) {
    bool visited[graph.vertex_quantity] = {0};
    long long component[graph.vertex_quantity];
    std::fill(component, component + graph.vertex_quantity, -1);
    std::list<long long> order;
    for (long long i = 0; i < graph.vertex_quantity; ++i) {
        if (!visited[i]) {
            straight_dfs(graph, i, visited, order);
        }
    }
    long long quantity = 0;
    for (auto i: order) {
        if (component[i] == -1) {
            reversed_dfs(graph, i, component, quantity++);
        }
    }
    long long index = component[0];
    for (long long i = 0; i < graph.vertex_quantity; ++i) {
        if (component[i] == index) {
            component[i] = 0;
        } else if (component[i] == 0) {
            component[i] = index;
        }
    }

    OrientedGraph new_graph;
    new_graph.vertex_quantity = quantity;
    new_graph.edges.resize(quantity, std::vector<long long>(quantity, INT_MAX));

    for (long long i = 0; i < graph.vertex_quantity; ++i) {
        for (long long j = 0; j < graph.vertex_quantity; ++j) {
            if (component[i] != component[j]) {
                new_graph.edges[component[i]][component[j]] = std::min(
                    new_graph.edges[component[i]][component[j]],
                    graph.edges[i][j]);
            }
        }
    }
    return new_graph;
}

long long dfs_check(OrientedGraph const& graph, bool* visited,
      bool zero = true, long long v = 0) {
    if (visited[v]) {
        return 0;
    }

    long long quantity = 1;
    visited[v] = true;
    for (long long i = 0; i < graph.edges.size(); ++i) {
        if ((zero && graph.edges[v][i] == 0) ||
            (!zero && graph.edges[v][i] != INT_MAX)) {
            quantity += dfs_check(graph, visited, zero, i);
        }
    }
    return quantity;
}

long long min_mst_sum(OrientedGraph const& graph) {
    long long result = 0;

    OrientedGraph new_graph;
    new_graph.vertex_quantity = graph.vertex_quantity;
    new_graph.edges.resize(graph.vertex_quantity, std::vector<long long>(
        graph.vertex_quantity, INT_MAX));

    for (long long i = 0; i < graph.vertex_quantity; ++i) {
        long long min = INT_MAX;
        for (long long j = 0; j < graph.vertex_quantity; ++j) {
            min = std::min(min, graph.edges[j][i]);
        }
        if (min == INT_MAX) {
            continue;
        }

        for (long long j = 0; j < graph.vertex_quantity; ++j) {
            new_graph.edges[j][i] = graph.edges[j][i] - min;
        }
        if (i != 0) {
          result += min;
        }
    }

    bool visited[graph.vertex_quantity] = {0};
    if (dfs_check(new_graph, visited) == graph.vertex_quantity) {
        return result;
    }
    return result + min_mst_sum(condensation(new_graph));
}

int main() {
    // std::ifstream std::cin("k.in");
    // std::ofstream std::cout("k.out");
    OrientedGraph graph;
    std::cin >> graph;

    bool visited[graph.vertex_quantity] = {0};
    if (dfs_check(graph, visited, false) != graph.vertex_quantity) {
        std::cout << "NO";
    } else {
        std::cout << "YES\n" << min_mst_sum(graph);
    }
    return 0;
}
