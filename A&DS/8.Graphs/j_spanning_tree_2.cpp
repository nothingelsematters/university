#include <iostream>
#include <climits>
#include <list>
#include <map>
#include <set>

struct UnorientedGraph {
public:
    UnorientedGraph() {}

    friend std::istream& operator>>(std::istream& in, UnorientedGraph& graph) {
        in >> graph.vertex_quantity >> graph.edges_quantity;
        graph.edges = new std::map<long long, long long>[graph.vertex_quantity];

        for (size_t i = 0; i < graph.edges_quantity; ++i) {
            long long start, end, weight;
            in >> start >> end >> weight;
            auto starting = graph.edges[start - 1];
            if (/*start != end && */(starting.find(end - 1) == starting.end() ||
                    starting[end - 1] > weight)) {
                graph.edges[start - 1][end - 1] = weight;
                graph.edges[end - 1][start - 1] = weight;
            }
        }
        return in;
    }

// private:
    size_t vertex_quantity;
    size_t edges_quantity;
    std::map<long long, long long>* edges;
};

void dfs(long long v, std::set<long long>* edges, long long* result, long long colour, bool* visited) {
    result[v] = colour;
    visited[v] = true;
    for (auto i: edges[v]) {
        if (!visited[i]) {
            dfs(i, edges, result, colour, visited);
        }
    }
}

void findComp(std::set<long long>* edges, long long arr_size, long long* result) {
    long long colour = 0;
    bool visited[arr_size] = {0};
    for (long long i = 0; i < arr_size; ++i) {
        if (!visited[i]) {
            dfs(i, edges, result, colour++, visited);
        }
    }
}

long long size(std::set<long long>* edges, long long arr_size) {
    long long result = 0;
    for (long long i = 0; i < arr_size; ++i) {
        result += edges[i].size();
    }
    return result / 2;
}

long long mst(UnorientedGraph const& graph) {
    long long result = 0;
    std::set<long long> t[graph.vertex_quantity];

    while (size(t, graph.vertex_quantity) < graph.vertex_quantity - 1) {

        std::tuple<long long, long long, long long> minEdge[graph.vertex_quantity];
        for (long long i = 0; i < graph.vertex_quantity; ++i) {
            std::get<2>(minEdge[i]) = INT_MAX;
        }
        long long comp[graph.vertex_quantity];
        findComp(t, graph.vertex_quantity, comp);

        for (long long i = 0; i < graph.vertex_quantity; ++i) {
            for (auto j: graph.edges[i]) {
                long long v = j.first;
                long long w = j.second;

                if (comp[i] != comp[v]) {
                    if (std::get<2>(minEdge[comp[i]]) > w) {
                        minEdge[comp[i]] = std::make_tuple(i, v, w);
                    }
                    if (std::get<2>(minEdge[comp[v]]) > w) {
                        minEdge[comp[v]] = std::make_tuple(i, v, w);
                    }
                }
            }
        }

        for (long long i = 0; i < graph.vertex_quantity; ++i) {
            if (std::get<2>(minEdge[i]) == INT_MAX) {
                break;
            }
            if (t[std::get<0>(minEdge[i])].find(std::get<1>(minEdge[i])) !=
                    t[std::get<0>(minEdge[i])].end()) {
                continue;
            }
            t[std::get<0>(minEdge[i])].insert(std::get<1>(minEdge[i]));
            t[std::get<1>(minEdge[i])].insert(std::get<0>(minEdge[i]));
            result += std::get<2>(minEdge[i]);
        }
    }
    return result;
}

int main() {
    UnorientedGraph graph;
    std::cin >> graph;
    std::cout << mst(graph);
    return 0;
}
