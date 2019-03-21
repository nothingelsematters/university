#include <iostream>
#include <iomanip>
#include <algorithm>
#include <vector>
#include <limits>
#include <list>
#include <queue>
#include <optional>


class Edge {
public:
    Edge(int to, int maxcapacity, int num)
        : to(to), maxcapacity(maxcapacity), flow(0), num(num) {}

    int get_capacity() const {
        return maxcapacity - flow;
    }

    Edge* reverse;
    int flow;
    const int to, maxcapacity, num;
};

class Net {
private:
    class Order {
    public:
        int from, to;
        std::list<Edge>::iterator itfrom, itto;
    };

public:
    Net(int vq, int eq, int s, int t) : vq(vq), eq(eq), start(s), finish(t) {
        edges.resize(vq);
    }

    Net(int vq, int eq) : Net(vq, eq, 0, vq - 1) {}

    void add_connection(int from, int to, int maxc = 1) {
        insert(from, to, maxc);
        order.push_back(Order{from, to, --edges[from].end(), --edges[to].end()});
    }

    void remove_connection(int num) {
        Order ord = order[num];
        edges[ord.from].erase(ord.itfrom);
        edges[ord.to].erase(ord.itto);
    }

    void flush() {
        for (auto& i: edges) {
            for (auto& j: i) {
                j.flow = 0;
            }
        }
    }

    const int vq, eq, start, finish;
    std::vector<std::list<Edge>> edges;

private:
    void insert(int from, int to, int maxc) {
        edges[from].push_back(Edge(to, maxc, order.size()));
        edges[to].push_back(Edge(from, 0, order.size()));
        edges[from].back().reverse = &edges[to].back();
        edges[to].back().reverse = &edges[from].back();
    }

    std::vector<Order> order;
};


std::pair<bool, std::vector<int>> bfs(int capacity, Net& net) {
    std::vector<int> level(net.vq, std::numeric_limits<int>::max());
    level[net.start] = 0;

    std::queue<int> q;
    q.push(net.start);

    while (!q.empty()) {
        int v = q.front();
        q.pop();

        for (auto& e: net.edges[v]) {
            if (level[e.to] == std::numeric_limits<int>::max() && e.get_capacity() >= capacity) {
                level[e.to] = level[v] + 1;
                q.push(e.to);
            }
        }
    }

    return {level[net.finish] != std::numeric_limits<int>::max(), std::move(level)};
}

int dfs(int v, Net& net, std::vector<std::list<Edge>::iterator>& block,
        std::vector<int>& level, int mincapacity = std::numeric_limits<int>::max()) {
    if (v == net.finish) {
        return mincapacity;
    }
    if (mincapacity == 0) {
        return 0;
    }

    for (auto it = block[v]; it != net.edges[v].end(); ++it) {
        if (level[it->to] != level[v] + 1) {
            continue;
        }

        int flow_get;
        if (flow_get = dfs(it->to, net, block, level, std::min(it->get_capacity(), mincapacity))) {
            it->flow += flow_get;
            it->reverse->flow -= flow_get;

            return flow_get;
        }
        ++block[v];
    }

    return 0;
}


double dinic_iter(int capacity, Net& net) {
    double result = 0;
    auto [keep_going, level] = bfs(capacity, net);
    while (keep_going) {
        std::vector<std::list<Edge>::iterator> block(net.vq);
        for (int i = 0; i < net.vq; ++i) {
            block[i] = net.edges[i].begin();
        }

        double flow;
        while (flow = dfs(net.start, net, block, level)) {
            result += flow;
        }
        auto pr = bfs(capacity, net);
        keep_going = pr.first;
        level = pr.second;
    }
    return result;
}

double dinic(Net& net) {
    int maxcap = std::numeric_limits<int>::max();
    double answer = 0;
    while (maxcap) {
        answer += dinic_iter(maxcap, net);
        maxcap >>= 1;
    }
    return answer;
}


std::optional<std::list<int>> find_erase_path(Net& net) {
    if (dinic(net) == 0) {
        return std::nullopt;
    }

    std::list<int> result{net.start};
    bool visited[net.eq];
    std::fill(visited, visited + net.eq, false);

    for (int v = net.start; v != net.finish; ) {
        int temp = v;
        for (auto e: net.edges[v]) {
            if (e.flow > 0 && !visited[e.num]) {
                v = e.to;
                visited[e.num] = true;
                result.push_back(v);
                net.remove_connection(e.num);
                break;
            }
        }
        if (v == temp) {
            return std::nullopt;
        }
    }
    net.flush();


    return std::make_optional(std::move(result));
}

std::optional<std::pair<std::list<int>, std::list<int>>> two_edge_disjoin_paths(Net& net) {
    auto first = find_erase_path(net);
    if (!first) {
        return std::nullopt;
    }

    auto second = find_erase_path(net);
    if (!second) {
        return std::nullopt;
    }

    return std::make_optional<std::pair<std::list<int>, std::list<int>>>(first.value(), second.value());
}

int main() {
    int vertex_quantity, edge_quantity, start, finish;
    std::cin >> vertex_quantity >> edge_quantity >> start >> finish;
    Net net(vertex_quantity, edge_quantity, start - 1, finish - 1);

    for (int i = 0; i < edge_quantity; ++i) {
        int from, to, capacity;
        std::cin >> from >> to;
        net.add_connection(--from, --to);
    }

    auto result = two_edge_disjoin_paths(net);

    if (result) {
        std::cout << "YES\n";
        auto [first, second] = result.value();
        auto printn = [] (const int& n) { std::cout << n + 1 << ' '; };

        std::for_each(first.begin(), first.end(), printn);
        std::cout << '\n';
        std::for_each(second.begin(), second.end(), printn);
    } else {
        std::cout << "NO";
    }

    return 0;
}
