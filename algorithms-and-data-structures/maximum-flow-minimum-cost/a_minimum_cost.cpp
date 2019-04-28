#include <fstream>
#include <vector>
#include <memory>
#include <limits>
#include <deque>
#include <optional>

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
    const size_t reverse;
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

private:
    size_t size, start, finish;
    std::unique_ptr<std::vector<edge>[]> edges;

    std::optional<std::unique_ptr<edge*[]>> levit() const {
        enum vertex_type {
            FAR_AWAY,
            WAITING,
            NEARBY
        };

        auto parent = std::make_unique<edge*[]>(size);
        vertex_type vtype[size] = {};
        long long distance[size] = {0};
        std::fill(distance + 1, distance + size + 1, INF);

        std::deque<size_t> deq;
        deq.push_back(start);

        while (!deq.empty()) {
            size_t from = deq.front();
            deq.pop_front();
            vtype[from] = NEARBY;

            for (auto& e: edges[from]) {
                if (e.potential() && distance[e.to] > distance[e.from] + e.cost) {
                    distance[e.to] = distance[e.from] + e.cost;

                    switch (vtype[e.to]) {
                        case FAR_AWAY:
                            deq.push_back(e.to);
                            break;
                        case NEARBY:
                            deq.push_front(e.to);
                    }

                    vtype[e.to] = WAITING;
                    parent[e.to] = &e;
                }
            }
        }

        return distance[finish] >= INF ? std::nullopt : std::make_optional(std::move(parent));
    }
};


int main() {
    const std::string file_name = "mincost";
    std::ifstream fin(file_name + ".in");
    size_t size, edge_amount;
    fin >> size >> edge_amount;

    network net(size);
    for (size_t i = 0; i < edge_amount; ++i) {
        size_t from, to;
        long long capacity, cost;

        fin >> from >> to >> capacity >> cost;
        if (capacity) {
            net.emplace_edge(--from, --to, capacity, cost);
        }
    }

    std::ofstream fout(file_name + ".out");
    fout << net.max_flow_min_cost() << '\n';
    return 0;
}
