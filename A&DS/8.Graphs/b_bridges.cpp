#include <iostream>
#include <list>
#include <vector>
#include <algorithm>

struct UnorientedGraph {
public:
    UnorientedGraph() {}

    friend std::istream& operator>>(std::istream& in, UnorientedGraph& graph) {
        in >> graph.vertex_quantity >> graph.edges_quantity;
        graph.edges = new std::list<std::pair<int, int>>[graph.vertex_quantity];

        for (size_t i = 0; i < graph.edges_quantity; ++i) {
            int start, end;
            in >> start >> end;
            graph.edges[start - 1].push_back({end - 1, i});
            graph.edges[end - 1].push_back({start - 1, i});
        }
        return in;
    }

// private:
    size_t vertex_quantity;
    size_t edges_quantity;
    std::list<std::pair<int, int>>* edges;
};

std::vector<int> result;
int* enter;
int* return_time;
bool* visited;
bool* current;

void dfs(UnorientedGraph const& graph, int v, int from) {
    if (visited[v]) return;
    static int time = 0;
    ++time;
    visited[v] = true;
    current[v] = true;
    enter[v] = time;
    return_time[v] = time;

    for (auto i: graph.edges[v]) {
        if (current[i.first] && i.first != from) {
            return_time[v] = std::min(return_time[v], enter[i.first]);
        }

        if (!visited[i.first]) {
            dfs(graph, i.first, v);
            return_time[v] = std::min(return_time[v], return_time[i.first]);
            if (return_time[i.first] > enter[v]) {
                result.push_back(i.second);
            }
        }
    }
    current[v] = false;
}

void find_bridges(UnorientedGraph const& graph) {
    enter = new int[graph.vertex_quantity];
    return_time = new int[graph.vertex_quantity];
    visited = new bool[graph.vertex_quantity];
    current = new bool[graph.vertex_quantity];

    for (int i = 0; i < graph.vertex_quantity; ++i) {
        enter[i] = 0;
        return_time[i] = 0;
        visited[i] = false;
        current[i] = false;
    }
    for (int i = 0; i < graph.vertex_quantity; ++i) {
        dfs(graph, i, -1);
    }
}

int main() {
    UnorientedGraph graph;
    std::cin >> graph;
    find_bridges(graph);

    std::cout << result.size() << '\n';
    std::sort(result.begin(), result.end());
    for (auto i: result) {
        std::cout << (i + 1) << ' ';
    }
    return 0;
}
