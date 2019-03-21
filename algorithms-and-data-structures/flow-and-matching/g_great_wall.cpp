#include <iostream>
#include <iomanip>
#include <algorithm>
#include <vector>
#include <limits>
#include <list>
#include <queue>

class Net {
public:
    class Edge {
    public:
        Edge(size_t to, int maxcapacity, size_t revindex)
            : to(to), maxcapacity(maxcapacity), flow(0), revindex(revindex) {}

        int get_capacity() const {
            return maxcapacity - flow;
        }

        int flow;
        const size_t to;
        const int maxcapacity, revindex;
    };
    
    Net(size_t vq, bool oriented = true) : vq(vq), start(0), finish(vq - 1) {
        edges.resize(vq);
    }

    void add_connection(size_t from, size_t to, int maxc) {
        order.emplace_back(from, edges[from].size());
        insert(from, to, maxc);
        if (!oriented) {
            insert(to, from, maxc);
        }
    }

    Edge& get_reverse(size_t from, size_t index) {
        Edge e = edges[from][index];
        return edges[e.to][e.revindex];
    }

    bool oriented;
    const size_t vq;
    size_t start, finish;
    std::vector<std::vector<Edge>> edges;
    std::list<std::pair<size_t, size_t>> order;

private:
    void insert(size_t from, size_t to, int maxc) {
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

int dfs(size_t v, Net& net, std::vector<int>& block, std::vector<int>& level,
        int mincapacity = std::numeric_limits<int>::max()) {
    if (v == net.finish) {
        return mincapacity;
    }
    if (mincapacity == 0) {
        return 0;
    }

    for (int i = block[v]; i < net.edges[v].size(); ++i) {
        Net::Edge& e = net.edges[v][i];

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

void full_dfs(Net& net, bool* accessible, size_t v) {
    if (accessible[v]) {
        return;
    }
    accessible[v] = true;

    for (auto e: net.edges[v]) {
        if (e.maxcapacity > e.flow) {
            full_dfs(net, accessible, e.to);
        }
    }
}

std::pair<int, std::list<std::pair<size_t, size_t>>> min_cut(Net& net) {
    double flow = dinic(net);
    bool accessible[net.vq];
    std::fill(accessible, accessible + net.vq, false);
    full_dfs(net, accessible, net.start);

    int answer = 0;
    std::list<std::pair<size_t, size_t>> result;
    
    for (size_t i = 0; i < net.vq; ++i) {
        if (!accessible[i]) {
            continue;
        }
        
        for (auto e: net.edges[i]) {
            if (!accessible[e.to] && e.flow == 1) {
                result.emplace_back(i, e.to);
                break;
            }
        }       
        
    }

    return {flow, result};
}

enum Symbol: char {
    MOUNTAIN = '#',
    FREE = '.',
    CLOSED = '-',
    START = 'A',
    FINISH = 'B',
    ROAD = '+'
};

size_t index(size_t h, size_t l, size_t full_length) {
    return full_length * h + l;
}

size_t second_index(size_t h, size_t l, size_t full_height, size_t full_length) {
    return index(h, l, full_length) + full_height * full_length;
}


int main() {
    size_t height, length;
    std::cin >> height >> length;
    Net net(height * length * 2);
    std::vector<std::vector<char>> kingdom(height * 2, std::vector<char>(length));
    int local_inf = std::numeric_limits<int>::max();

    for (size_t i = 0; i < height; ++i) {
        for (size_t j = 0; j < length; ++j) {
            char c;
            std::cin >> c;
            Symbol result;
            switch (c) {
                case FREE: 
                    net.add_connection(second_index(i, j, height, length), index(i, j, length), 1);
                    break;
                    
                case CLOSED: 
                    net.add_connection(second_index(i, j, height, length), index(i, j, length), local_inf);
                    break;
                    
                case START: 
                    net.start = index(i, j, length);
                    break;
                    
                case FINISH: 
                    net.finish = second_index(i, j, height, length);
            }
            kingdom[i][j] = c;
        }
    }

    for (size_t i = 0; i < height; ++i) {
        for (size_t j = 0; j < length; ++j) {
            if (kingdom[i][j] == MOUNTAIN) {
                continue;
            }
            size_t from = index(i, j, length);
            size_t to = second_index(i, j, height, length);
            
            if (j + 1 < length && kingdom[i][j + 1] != MOUNTAIN) {
                net.add_connection(from, second_index(i, j + 1, height, length), local_inf);
                net.add_connection(index(i, j + 1, length), to, local_inf);
            }

            if (i + 1 < height && kingdom[i + 1][j] != MOUNTAIN) {
                net.add_connection(from, second_index(i + 1, j, height, length), local_inf);
                net.add_connection(index(i + 1, j, length), to, local_inf);
            }
        }
    }

    auto [answer, result] = min_cut(net);
    if (answer >= local_inf || answer < 0) {
        std::cout << "-1";
        return 0;
    }
    for (auto i: result) {
        kingdom[i.second / length][i.second % length] = ROAD;
    }

    std::cout << answer << '\n';
    for (size_t i = 0; i < height; ++i) {
        for (size_t j = 0; j < length; ++j) {
            std::cout << kingdom[i][j];
        }
        std::cout << '\n';
    }
    return 0;
}
