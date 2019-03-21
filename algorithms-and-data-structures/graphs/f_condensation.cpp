#include <iostream>
#include <list>
#include <set>

struct OrientedGraph {
public:
    OrientedGraph() {}

    friend std::istream& operator>>(std::istream& in, OrientedGraph& graph) {
        in >> graph.vertex_quantity >> graph.edges_quantity;
        graph.edges = new std::list<unsigned int>[graph.vertex_quantity];

        for (size_t i = 0; i < graph.edges_quantity; ++i) {
            int start, end;
            in >> start >> end;
            graph.edges[start - 1].push_back(end - 1);
        }
        return in;
    }

// private:
    size_t vertex_quantity;
    size_t edges_quantity;
    std::list<unsigned int>* edges;
};

OrientedGraph reverse(OrientedGraph const& graph) {
    OrientedGraph reversed;

    reversed.edges_quantity = graph.edges_quantity;
    reversed.vertex_quantity = graph.vertex_quantity;
    reversed.edges = new std::list<unsigned int>[graph.vertex_quantity];
    for (int i = 0; i < graph.vertex_quantity; ++i) {
        for (auto j: graph.edges[i]) {
            reversed.edges[j].push_back(i);
        }
    }

    return reversed;
}

void straight_dfs(OrientedGraph const& graph, int v, bool* visited,
        std::list<int>& order) {
    visited[v] = true;

    for (auto i: graph.edges[v]) {
        if (!visited[i]) {
            straight_dfs(graph, i, visited, order);
        }
    }
    order.push_front(v);
}

void reversed_dfs(OrientedGraph const& graph, int v, int* component, int num) {
    component[v] = num;
    for (auto i: graph.edges[v]) {
        if (component[i] == 0) {
            reversed_dfs(graph, i, component, num);
        }
    }
}

int condesation_edge_quantity(OrientedGraph const& graph) {
    OrientedGraph reversed = reverse(graph);

    bool visited[reversed.vertex_quantity] = {0};
    int component[graph.vertex_quantity] = {0};
    std::list<int> order;

    for (int i = 0; i < reversed.vertex_quantity; ++i) {
        if (!visited[i]) {
            straight_dfs(graph, i, visited, order);
        }
    }

    int quantity = 1;
    for (auto i: order) {
        if (component[i] == 0) {
            reversed_dfs(reversed, i, component, quantity);
            ++quantity;
        }
    }

    std::set<std::pair<int, int>> edges;
    for (int i = 0; i < graph.vertex_quantity; ++i) {
        for (auto j: graph.edges[i]) {
            int ci = component[i];
            int cj = component[j];
            if (ci != cj && edges.find({ci, cj}) == edges.end() &&
                    edges.find({cj, ci}) == edges.end()) {
                edges.emplace(ci, cj);
            }
        }
    }

    return edges.size();
}

int main() {
    OrientedGraph graph;
    std::cin >> graph;
    std::cout << condesation_edge_quantity(graph);
    return 0;
}
