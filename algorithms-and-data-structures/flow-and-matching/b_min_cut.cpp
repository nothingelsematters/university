#include <iostream>
#include <iomanip>
#include <algorithm>
#include <vector>
#include <limits>
#include <list>
#include <queue>


class Edge {
public:
    Edge(int to, int maxcapacity, int revindex)
        : to(to), maxcapacity(maxcapacity), flow(0), revindex(revindex) {}

    int get_capacity() const {
        return maxcapacity - flow;
    }

    int flow;
    const int to, maxcapacity, revindex;
};

class Net {
public:
    Net(int vq, int eq) : vq(vq), eq(eq), start(0), finish(vq - 1) {
        edges.resize(vq);
    }

    void add_connection(int from, int to, int maxc) {
        order.emplace_back(from, edges[from].size());
        insert(from, to, maxc);
        insert(to, from, maxc);
    }

    Edge& get_reverse(int from, int index) {
        Edge e = edges[from][index];
        return edges[e.to][e.revindex];
    }

    const int vq, eq, start, finish;
    std::vector<std::vector<Edge>> edges;
    std::list<std::pair<int, int>> order;

private:
    void insert(int from, int to, int maxc) {
        edges[from].push_back(Edge(to, maxc, edges[to].size()));
        edges[to].push_back(Edge(from, 0, edges[from].size() - 1));
    }
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

int dfs(int v, Net& net, std::vector<int>& block, std::vector<int>& level,
        int mincapacity = std::numeric_limits<int>::max()) {
    if (v == net.finish) {
        return mincapacity;
    }
    if (mincapacity == 0) {
        return 0;
    }

    for (int i = block[v]; i < net.edges[v].size(); ++i) {
        Edge& e = net.edges[v][i];

        if (level[e.to] != level[v] + 1) {
            continue;
        }

        int flow_get;
        if (flow_get = dfs(e.to, net, block, level, std::min(e.get_capacity(), mincapacity))) {
            e.flow += flow_get;
            net.get_reverse(v, i).flow -= flow_get;

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
        std::vector<int> block(net.vq, 0);

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

void full_dfs(Net& net, bool* accessible, size_t v = 0) {
    if (accessible[v]) {
        return;
    }
    accessible[v] = true;

    for (auto e: net.edges[v]) {
        if (e.maxcapacity != e.flow) {
            full_dfs(net, accessible, e.to);
        }
    }
}

std::pair<int, std::list<int>> min_cut(Net& net) {
    dinic(net);

    bool accessible[net.vq];
    std::fill(accessible, accessible + net.vq, false);
    full_dfs(net, accessible, net.start);

    int answer = 0;
    std::list<int> result;
    int index = -1;
    for (auto num: net.order) {
        ++index;
        if (accessible[num.first] == accessible[net.edges[num.first][num.second].to]) {
            continue;
        }

        int curflow = net.edges[num.first][num.second].flow;
        if (curflow == 0) {
            curflow = net.edges[num.first][num.second + 1].flow;
        }

        answer += std::abs(curflow);
        result.push_back(index);
    }

    return {answer, result};
}


int main() {
    int vertex_quantity, edge_quantity;
    std::cin >> vertex_quantity >> edge_quantity;
    Net net(vertex_quantity, edge_quantity);

    for (int i = 0; i < edge_quantity; ++i) {
        int from, to, capacity;
        std::cin >> from >> to >> capacity;
        net.add_connection(--from, --to, capacity);
    }


    auto [answer, result] = min_cut(net);
    std::cout << result.size() << ' ' << answer << '\n';
    std::for_each(result.begin(), result.end(), [] (const int& n) { std::cout << n + 1 << ' '; });
    return 0;
}
