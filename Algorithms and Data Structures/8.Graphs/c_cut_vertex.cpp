#include <iostream>
#include <list>
#include <set>
#include <vector>

struct UnorientedGraph {
public:
    UnorientedGraph() {}

    friend std::istream& operator>>(std::istream& in, UnorientedGraph& graph) {
        in >> graph.vertex_quantity >> graph.edges_quantity;
        graph.edges = new std::vector<unsigned int>[graph.vertex_quantity];

        for (size_t i = 0; i < graph.edges_quantity; ++i) {
            int start, end;
            in >> start >> end;
            graph.edges[start - 1].push_back(end - 1);
            graph.edges[end - 1].push_back(start - 1);
        }
        return in;
    }

// private:
    size_t vertex_quantity;
    size_t edges_quantity;
    std::vector<unsigned int>* edges;
};

std::set<int> result;
int* enter;
int* returning;
bool* visited;

void dfs(UnorientedGraph const& graph, int v, int from = -1) {
    if (visited[v]) return;

    static int time = 0;
    visited[v] = true;
    enter[v] = ++time;
    returning[v] = time;
    int children = 0;

    for (auto i: graph.edges[v]) {

        // reverse edges
        if (visited[i] && i != from) {
            returning[v] = std::min(returning[v], enter[i]);
        }

        // sons
        if (!visited[i]) {
            dfs(graph, i, v);
            returning[v] = std::min(returning[v], returning[i]);

            if (returning[i] >= enter[v] && from != -1) {
                if (graph.edges[v].size() > 1) {
                    result.insert(v);
                }
            }
            ++children;
        }
    }
    if (from == -1 && children > 1) {
        result.insert(v);
    }
}

void find_cut_vertexes(UnorientedGraph const& graph) {
    enter = new int[graph.vertex_quantity];
    returning = new int[graph.vertex_quantity];
    visited = new bool[graph.vertex_quantity];

    for (int i = 0; i < graph.vertex_quantity; ++i) {
        enter[i] = 0;
        returning[i] = 0;
        visited[i] = false;
    }
    for (int i = 0; i < graph.vertex_quantity; ++i) {
        dfs(graph, i);
    }
}

int main() {
    UnorientedGraph graph;
    std::cin >> graph;

    find_cut_vertexes(graph);
    std::cout << result.size() << '\n';
    for (auto i: result) {
        std::cout << i + 1 << ' ';
    }
    return 0;
}
