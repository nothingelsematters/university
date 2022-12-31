#include <fstream>
#include <vector>
#include <limits>
#include <deque>
#include <optional>
#include <algorithm>
#include <iomanip>

static constexpr long long INF = std::numeric_limits<long long>::max();
static constexpr double EPSILON = 1e-10;

class edge {
public:
    edge(size_t from, size_t to, double capacity, long long cost, size_t reverse)
    : from(from), to(to), capacity(capacity), cost(cost), flow(0.0), reverse(reverse) {}

    double potential() const noexcept {
        return capacity - flow;
    }

    const size_t from, to;
    const double capacity;
    const long long cost;
    double flow;
    const size_t reverse;
};

class network {
public:
    network(size_t size, size_t start, size_t finish)
        : size(size), start(start), finish(finish), edges(std::make_unique<std::vector<edge>[]>(size)) {}

    network(size_t size) : network(size, 0, size - 1) {}

    void emplace_edge(size_t from, size_t to, double capacity, long long cost) {
        edges[from].emplace_back(from, to, capacity, cost, edges[to].size());
        edges[to].emplace_back(to, from, 0, -cost, edges[from].size() - 1);
    }

    double max_flow_min_cost() {
        double flow = 0;
        for (auto lev = levit(); lev; lev = levit()) {
            auto parent = std::move(*lev);
            double add_flow = std::numeric_limits<double>::max();
            for (size_t i = finish; i != start; i = parent[i]->from) {
                add_flow = std::min(add_flow, parent[i]->potential());
            }

            flow += add_flow;

            for (size_t i = finish; i != start; i = parent[i]->from) {
                edge* e = parent[i];
                e->flow += add_flow;
                edges[e->to][e->reverse].flow -= add_flow;
            }
        }

        double added = 0;
        for (auto& e: edges[1]) {
            added -= e.flow;
        }

        return flow;
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
                if (std::abs(e.potential()) >= EPSILON && distance[e.to] > distance[e.from] + e.cost) {
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

bool is_schedulable(
    std::vector<int> const& times,
    std::vector<int> const& releases,
    std::vector<int> const& deadlines,
    int total_time,
    std::vector<int> const&  machines,
    double middle
) {
    std::vector<std::pair<double, double>> boundaries;
    for (size_t i = 0; i < times.size(); ++i) {
        boundaries.emplace_back(releases[i], deadlines[i] + middle);
    }

    std::vector<double> tmp(boundaries.size() * 2);
    for (size_t i = 0; i < boundaries.size(); ++i) {
        tmp[i] = boundaries[i].first;
        tmp[i + boundaries.size()] = boundaries[i].second;
    }
    std::sort(tmp.begin(), tmp.end());

    std::vector<double> intervals;
    intervals.push_back(tmp[0]);
    for (size_t i = 1; i < tmp.size(); ++i) {
        if (i > 0 && tmp[i] != tmp[i - 1]) {
            intervals.push_back(tmp[i]);
        }
    }

    network net(2 + intervals.size() - 1 + (intervals.size() - 1) * machines.size() + times.size(), 0, 1);

    int machine_sum = 0;
    for (size_t i = 0; i < machines.size(); ++i) {
        machine_sum += machines[i];
    }

    for (size_t i = 2; i < 1 + intervals.size(); ++i) {
        net.emplace_edge(i, 1, machine_sum * intervals[i - 1] - intervals[i - 2], 0);
    }

    for (size_t i = 1; i < intervals.size(); ++i) {
        for (size_t j = 0; j < machines.size(); ++j) {
            double capacity = (intervals[i] - intervals[i - 1]) * (j + 1);

            if (j == machines.size() - 1) {
                capacity *= machines[j];
            } else {
                capacity *= machines[j] - machines[j + 1];
            }

            net.emplace_edge((intervals.size() + 1) + j + (i - 1) * machines.size(), i + 1, capacity, 0);
        }
    }

    for (size_t i = 0; i < times.size(); ++i) {
        int from = (2 + intervals.size() - 1 + machines.size() * (intervals.size() - 1)) + i;

        net.emplace_edge(0, from, times[i], 0);
        int time_index = 0;

        while (boundaries[i].first > intervals[time_index]) {
            time_index++;
        }
        time_index++;

        while (time_index < intervals.size() && boundaries[i].second >= intervals[time_index]) {
            for (size_t j = 0; j < machines.size(); ++j) {
                double capacity = (intervals[time_index] - intervals[time_index - 1]);

                if (j == machines.size() - 1) {
                    capacity *= machines[j];
                } else {
                    capacity *= machines[j] - machines[j + 1];
                }

                net.emplace_edge(
                    from,
                    (2 + intervals.size() - 1) + (time_index - 1) * machines.size() + j,
                    capacity,
                    0
                );
            }
            time_index++;
        }
    }

    return std::abs(net.max_flow_min_cost() - total_time) <= EPSILON;
}

int main() {
    const std::string file_name = "cheese";
    std::ifstream fin(file_name + ".in");
    size_t job_size, machine_size;
    fin >> job_size >> machine_size;

    std::vector<int> times;
    std::vector<int> releases;
    std::vector<int> deadlines;
    int total_time = 0;

    for (size_t i = 0; i < job_size; ++i) {
        int t, r, d;
        fin >> t >> r >> d;

        times.push_back(t);
        releases.push_back(r);
        deadlines.push_back(d);

        total_time += t;
    }

    std::vector<int> machines;
    for (size_t i = 0; i < machine_size; ++i) {
        int s;
        fin >> s;
        machines.push_back(s);
    }
    std::sort(machines.begin(), machines.end(), std::greater<int>());

    double left = 0;
    double right = total_time;

    for (size_t i = 0; i < 100; ++i) {
        double middle = (left + right) / 2.0;
        if (is_schedulable(times, releases, deadlines, total_time, machines, middle)) {
            right = middle;
        } else {
            left = middle;
        }
    }

    std::ofstream fout(file_name + ".out");
    fout << std::setprecision(10) << std::fixed << (left + right) / 2.0 << '\n';

    return 0;
}
