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
    Net(int vq) : vq(vq), start(0), finish(vq - 1) {
        edges.resize(vq);
    }

    void update_size(int newsize) {
        edges.resize(newsize);
        vq = newsize;
    }

    void add_connection(int from, int to, int maxc = 1) {
        order.emplace_back(from, edges[from].size());
        insert(from, to, maxc);
    }

    Edge& get_reverse(int from, int index) {
        Edge e = edges[from][index];
        return edges[e.to][e.revindex];
    }

    int vq, start, finish;
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

std::list<std::pair<int, int>> get_bipartie_matching(Net& net, int left_quantity) {
    net.update_size(net.vq + 2);
    net.start = net.vq - 2;
    net.finish = net.vq - 1;

    for (int i = 0; i < left_quantity; i++) {
        net.add_connection(net.start, i);
    }
    for (int i = left_quantity; i < net.finish; i++) {
        net.add_connection(i, net.finish);
    }

    dinic(net);

    std::list<std::pair<int, int>> result;
    for (int i = 0; i < net.start; i++) {
        for (auto e: net.edges[i]) {
            if (e.flow == 1 && e.to < net.start) {
                result.emplace_back(std::min(i, e.to), std::max(i, e.to) - left_quantity);
            }
        }
    }

    return result;
}


int main() {
    int left_quantity, right_quantity;
    std::cin >> left_quantity >> right_quantity;
    Net net(left_quantity + right_quantity);

    for (int i = 0; i < left_quantity; ++i) {
        int right;
        std::cin >> right;
        while (right != 0) {
            net.add_connection(i, left_quantity + right - 1);
            std::cin >> right;
        }
    }
    auto result = get_bipartie_matching(net, left_quantity);
    std::cout << result.size() << '\n';
    
    for (auto p: result) { 
        std::cout << p.first + 1 << ' ' << p.second + 1 << '\n';
    }
    return 0;
}
