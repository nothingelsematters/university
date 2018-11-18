#include <iostream>
#include <algorithm>
#include <list>
#include <set>
#include <vector>
#include <map>

struct UnorientedGraph {
public:
    UnorientedGraph() {}

    friend std::istream& operator>>(std::istream& in, UnorientedGraph& graph) {
        in >> graph.vertex_quantity >> graph.edges_quantity;
        graph.edges = new std::vector<unsigned int>[graph.vertex_quantity];
        graph.pairs = new std::pair<int, int>[graph.edges_quantity];

        for (size_t i = 0; i < graph.edges_quantity; ++i) {
            int start, end;
            in >> start >> end;
            graph.num[{start - 1, end - 1}].insert(i);
            graph.num[{end - 1, start - 1}].insert(i);
            graph.edges[start - 1].push_back(end - 1);
            graph.edges[end - 1].push_back(start - 1);
            graph.pairs[i] = {start - 1, end - 1};
        }
        return in;
    }

// private:
    size_t vertex_quantity;
    size_t edges_quantity;
    std::vector<unsigned int>* edges;
    std::map<std::pair<int, int>, std::set<int>> num;
    std::pair<int, int>* pairs;
};

int* enter;
int* returning;
bool* visited;
bool* cut;

void dfs(UnorientedGraph const& graph, int v, int from = -1) {
    if (visited[v]) return;

    static int time = 0;
    visited[v] = true;
    enter[v] = ++time;
    returning[v] = time;
    std::set<int> children;

    for (auto i: graph.edges[v]) {

        // reverse edges
        if (visited[i] && i != from) {
            returning[v] = std::min(returning[v], enter[i]);
        }

        // sons
        if (!visited[i]) {
            dfs(graph, i, v);
            returning[v] = std::min(returning[v], returning[i]);
        }
    }
}

void find_cut_vertexes(UnorientedGraph const& graph) {
    enter = new int[graph.vertex_quantity];
    returning = new int[graph.vertex_quantity];
    visited = new bool[graph.vertex_quantity];
    cut = new bool [graph.vertex_quantity];

    for (int i = 0; i < graph.vertex_quantity; ++i) {
        enter[i] = 0;
        returning[i] = 0;
        visited[i] = false;
        cut[i] = false;
    }
    for (int i = 0; i < graph.vertex_quantity; ++i) {
        dfs(graph, i);
    }
}

int answer = 0;

void paint(UnorientedGraph& graph, int v, int* result, int colour, int parent = -1) {
    visited[v] = true;
    for (auto i: graph.edges[v]) {
        if (i == parent) continue;
        if (!visited[i]) {
            if (returning[i] >= enter[v]) {
                int new_colour = ++answer;
                for (auto j: graph.num[{i, v}])
                    result[j] = new_colour;
                paint(graph, i, result, new_colour, v);
            } else {
                for (auto j: graph.num[{i, v}])
                    result[j] = colour;
                paint(graph, i, result, colour, v);
            }
        } else if (enter[i] < enter[v]) {
            for (auto j: graph.num[{i, v}])
                result[j] = colour;
        }
    }
}

int hate_this(UnorientedGraph& graph, int* result) {
    for (int i = 0; i < graph.vertex_quantity; ++i) {
        if (!visited[i]) {
            paint(graph, i, result, answer);
        }
    }
    return answer;
}

int main() {
    UnorientedGraph graph;
    std::cin >> graph;

    find_cut_vertexes(graph);
    visited = new bool[graph.vertex_quantity];
    int result[graph.edges_quantity] = {0};
    std::fill(visited, visited + graph.vertex_quantity, false);

    std::cout << hate_this(graph, result) << '\n';
    for (int i = 0; i < graph.edges_quantity; ++i) {
        std::cout << result[i] << ' ';
    }
    return 0;
}
