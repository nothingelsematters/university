#include <fstream>
#include <vector>
#include <memory>
#include <limits>
#include <deque>
#include <optional>
#include <iostream>
#include <set>

#define size_t long long
#define int long long

unsigned short a, b;

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

    std::pair<size_t, std::vector<edge*>> max_flow_min_cost() {
        std::vector<edge*> result;
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
                result.push_back(e);
                min_cost += e->cost * add_flow;
            }
        }

        return {min_cost, result};
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

        std::vector<vertex_type> vtype(size, FAR_AWAY);

        std::vector<long long> distance(size, INF);
        distance[0] = 0;

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
                        case WAITING:
                            break;
                    }

                    vtype[e.to] = WAITING;
                    parent[e.to] = &e;
                }
            }
        }

        return distance[finish] >= INF ? std::nullopt : std::make_optional(std::move(parent));
    }
};


decltype(a + b) main() {
    const std::string file_name = "rsumc";
    std::ifstream fin(file_name + ".in");
    size_t job_size, machine_size;
    fin >> job_size >> machine_size;

    std::vector<std::vector<int>> times;
    for (size_t i = 0; i < job_size; ++i) {
        times.push_back(std::vector<int>());

        for (size_t j = 0; j < machine_size; ++j) {
            int tmp;
            fin >> tmp;
            times[i].push_back(tmp);
        }
    }

    network net(2 + job_size + job_size * machine_size, 0, 1);
    for (size_t i = 0; i < job_size; ++i) {
        net.emplace_edge(0, 2 + i, 1, 0);
    }

    for (size_t i = 0; i < job_size * machine_size; ++i) {
        net.emplace_edge(2 + job_size + i, 1, 1, 0);
    }

    for (size_t i = 0; i < job_size; ++i) {
        for (size_t j = 0; j < machine_size; ++j) {
            for (size_t k = 0; k < job_size; ++k) {
                net.emplace_edge(2 + i, 2 + job_size * (j + 1) + k, 1, (k + 1) * times[i][j]);
            }
        }
    }

    auto [sum_c, flow] = net.max_flow_min_cost();
    std::vector<std::vector<size_t>> result(machine_size, std::vector<size_t>(job_size, -1));

    for (auto edge : flow) {
        if (edge->from == 0 || edge->to == 1 || edge->from > 2 + job_size - 1 || edge->flow == 0) continue;

        long long k = job_size - 1 - (edge->to - 2) % job_size;
        long long j = (edge->to - 2) / job_size - 1;
        long long i = edge->from - 2;
        result[j][k] = i;
    }

    std::ofstream fout(file_name + ".out");
    fout << sum_c << '\n';

    for (auto i : result) {
        size_t count = 0;
        for (auto j : i) {
            if (j != -1) ++count;
        }

        fout << count << ' ';
        for (auto j : i) {
            if (j != -1) fout << (j + 1) << ' ';
        }
        fout << '\n';
    }

    return 0;
}
