#include <iostream>
#include <list>
#include <stack>

struct UnorientedGraph {
public:
    UnorientedGraph() {}

    friend std::istream& operator>>(std::istream& in, UnorientedGraph& graph) {
        in >> graph.vertex_quantity >> graph.edges_quantity;
        graph.edges = new std::list<unsigned int>[graph.vertex_quantity];

        for (size_t i = 0; i < graph.edges_quantity; ++i) {
            unsigned int start, end;
            in >> start >> end;
            graph.edges[start - 1].push_back(end - 1);
            graph.edges[end - 1].push_back(start - 1);
        }
        return in;
    }

// private:
    size_t vertex_quantity;
    size_t edges_quantity;
    std::list<unsigned int>* edges;
};

void update_component(unsigned int* result, unsigned int& quantity,
        std::stack<unsigned int>& order, int vertex) {
    ++quantity;
    int last = -1;
    while (last != vertex && !(order.empty())) {
        result[order.top()] = quantity;
        last = order.top();
        order.pop();
    }
}

void dfs(UnorientedGraph const& graph, int v, int from, unsigned int* result,
        unsigned int& quantity, unsigned int* enter, unsigned int* func,
        bool* visited, std::stack<unsigned int>& order) {

    if (visited[v]) return;

    static int time = 0;
    visited[v] = true;
    enter[v] = ++time;
    func[v] = time;
    order.push(v);

    for (auto i: graph.edges[v]) {

        if (visited[i] && i != from) {
            func[v] = std::min(func[v], enter[i]);
        } else if (!visited[i]) {

            dfs(graph, i, v, result, quantity, enter, func, visited, order);
            func[v] = std::min(func[v], func[i]);

            if (func[i] > enter[v]) {
                bool was = false;
                bool was2 = false;
                for (auto j: graph.edges[v]) {
                    if (j == i) {
                        if (was) {
                            was2 = true;
                            break;
                        }
                        was = true;
                    }
                }
                if (!was2)
                update_component(result, quantity, order, i);
            }
        }
    }
}

unsigned int edge_duality_components(UnorientedGraph const& graph,
        unsigned int* result) {
    unsigned int quantity = 0;
    unsigned int enter[graph.vertex_quantity] = {0};
    unsigned int func[graph.vertex_quantity] = {0};
    bool visited[graph.vertex_quantity] = {0};
    std::stack<unsigned int> order;

    for (size_t i = 0; i < graph.vertex_quantity; ++i) {
        dfs(graph, i, -1, result, quantity, enter, func, visited, order);
        if (!order.empty()) {
            update_component(result, quantity, order, -2);
        }
    }

    return quantity;
}

int main() {
    UnorientedGraph graph;
    std::cin >> graph;

    unsigned int result[graph.vertex_quantity] = {0};
    std::cout << edge_duality_components(graph, result) << '\n';
    for (size_t i = 0; i < graph.vertex_quantity; ++i) {
        std::cout << result[i] << ' ';
    }
    return 0;
}
