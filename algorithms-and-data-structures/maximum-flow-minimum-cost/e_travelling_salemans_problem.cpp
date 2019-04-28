#include <iostream>
#include <memory>
#include <queue>
#include <optional>
#include <limits>
#include <vector>

static constexpr long long INF = std::numeric_limits<long long>::max();

class edge {
public:
    edge(size_t from, size_t to, long long capacity, long long cost, size_t reverse)
    : from(from), to(to), capacity(capacity), cost(cost), flow(0), reverse(reverse) {}

    long long potential() const noexcept {
        return capacity - flow;
    }

    const size_t from, to;
    const long long capacity, cost;
    long long flow;
    size_t reverse;
};

class network {
public:
    network(size_t size, size_t start, size_t finish)
        : size(size), start(start), finish(finish), edges(std::make_unique<std::vector<edge>[]>(size)) {}

    network(size_t size) : network(size, 0, size - 1) {}

    void emplace_edge(size_t from, size_t to, long long capacity, long long cost) {
        edges[from].emplace_back(from, to, capacity, cost, edges[to].size());
        edges[to].emplace_back(to, from, 0, -cost, edges[from].size() - 1);
    }

    long long max_flow_min_cost() {
        long long min_cost = 0;

        for (auto lev = levit(); lev; lev = levit()) {
            auto parent = std::move(*lev);
            long long add_flow = INF;
            for (size_t i = finish; i != start; i = parent[i]->from) {
                add_flow = std::min(add_flow, parent[i]->potential());
            }

            for (size_t i = finish; i != start; i = parent[i]->from) {
                edge* e = parent[i];
                e->flow += add_flow;
                edges[e->to][e->reverse].flow -= add_flow;
                min_cost += e->cost * add_flow;
            }
        }
        return min_cost;
    }

    size_t const size, start, finish;

private:
    std::unique_ptr<std::vector<edge>[]> edges;

    std::optional<std::unique_ptr<edge*[]>> levit() const {
        auto parent = std::make_unique<edge*[]>(size);
        bool visited[size] = {};

        long long distance[size];
        distance[start] = 0;
        std::fill(distance + 1, distance + size + 1, INF);

        std::queue<size_t> q;
        q.push(start);

        while (!q.empty()) {
            size_t from = q.front();
            q.pop();
            visited[from] = false;

            for (auto& e: edges[from]) {
                if (e.potential() && distance[e.to] > distance[e.from] + e.cost) {
                    distance[e.to] = distance[e.from] + e.cost;
                    parent[e.to] = &e;

                    if (!visited[e.to]) {
                        visited[e.to] = true;
                        q.push(e.to);
                    }
                }
            }
        }

        return distance[finish] >= INF ? std::nullopt : std::make_optional(std::move(parent));
    }
};

int main() {
    size_t cities, roads;
    std::cin >> cities >> roads;
    network net(2 * cities + 2);

    for (size_t i = 1; i <= cities; ++i) {
        long long cost;
        std::cin >> cost;
        net.emplace_edge(net.start, i, 1, 0);
        net.emplace_edge(cities + i, net.finish, 1, 0);
        net.emplace_edge(i, cities + i, cities, cost);
        net.emplace_edge(cities + i, i, cities, 0);
    }

    for (size_t i = 0; i < roads; ++i) {
        size_t from, to;
        long long cost;
        std::cin >> from >> to >> cost;
        net.emplace_edge(from, cities + to, cities, cost);
    }

    std::cout << net.max_flow_min_cost();
    return 0;
}
