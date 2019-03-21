#include <iostream>
#include <limits>
#include <vector>
#include <queue>
#include <list>

class Net {
public:
    class Edge {
    public:
        Edge(size_t to, int maxcapacity) : to(to), maxcapacity(maxcapacity) {}

        int get_capacity() const {
            return maxcapacity - flow;
        }

        Edge* reverse = nullptr;
        int flow = 0;
        const int maxcapacity;
        const size_t to;
    };
    
    Net(size_t initial_size, bool oriented = true) : oriented(oriented) {
        edges.resize(initial_size);
    }
    
    std::list<Edge>& operator[](size_t index) {
        return edges[index];
    }

    void add_connection(size_t from, size_t to, int maxc = 1) {
        size_t update = std::max(from, to);
        if (update >= edges.size()) {
            edges.resize(update + 1);
        }
        insert(from, to, maxc);
        if (!oriented) {
            insert(to, from, maxc);
        }
    }
    
    size_t size() const {
        return edges.size();
    }

    const bool oriented;
    size_t start = 0;
    size_t finish;

private:
    std::vector<std::list<Edge>> edges;
    
    void insert(size_t from, size_t to, int maxc) {
        edges[from].push_back(Edge(to, maxc));
        edges[to].push_back(Edge(from, 0));
        edges[from].back().reverse = &edges[to].back();
        edges[to].back().reverse = &edges[from].back();
    }
};


std::pair<bool, std::vector<int>> bfs(Net& net, int capacity) {
    std::vector<int> level(net.size(), std::numeric_limits<int>::max());
    level[net.start] = 0;

    std::queue<int> q;
    q.push(net.start);

    while (!q.empty()) {
        int v = q.front();
        q.pop();

        for (auto& e: net[v]) {
            if (level[e.to] == std::numeric_limits<int>::max() && e.get_capacity() >= capacity) {
                level[e.to] = level[v] + 1;
                q.push(e.to);
            }
        }
    }

    return {level[net.finish] != std::numeric_limits<int>::max(), std::move(level)};
}

int dfs(int v, Net& net, std::vector<std::list<Net::Edge>::iterator>& block,
        std::vector<int>& level, int mincapacity = std::numeric_limits<int>::max()) {
    if (v == net.finish) {
        return mincapacity;
    }
    if (mincapacity == 0) {
        return 0;
    }

    for (auto it = block[v]; it != net[v].end(); ++it) {
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


double dinic_iter(Net& net, int capacity) {
    double result = 0;
    auto [keep_going, level] = bfs(net, capacity);
    while (keep_going) {
        std::vector<std::list<Net::Edge>::iterator> block(net.size());
        for (int i = 0; i < net.size(); ++i) {
            block[i] = net[i].begin();
        }

        double flow;
        while (flow = dfs(net.start, net, block, level)) {
            result += flow;
        }
        auto pr = bfs(net, capacity);
        keep_going = pr.first;
        level = pr.second;
    }
    return result;
}

double dinic(Net& net) {
    int maxcap = std::numeric_limits<int>::max();
    double answer = 0;
    while (maxcap) {
        answer += dinic_iter(net, maxcap);
        maxcap >>= 1;
    }
    return answer;
}

enum Match : char {
    NO = '#',
    WIN = 'W',
    LOSS = 'L',
    OTWIN = 'w',
    OTLOSS = 'l',
    TOPLAY = '.'
};

int main() {
    size_t teams;
    std::cin >> teams;
    char table[teams][teams];
    int rest[teams];
    int possible[teams];
    std::fill(rest, rest + teams, 0);
    std::fill(possible, possible + teams, 0);
    
    Net net(teams + 2);
    net.start = teams;
    net.finish = teams + 1;
    
    for (size_t i = 0; i < teams; ++i) {
        for (size_t j = 0; j < teams; ++j) {
            std::cin >> table[i][j];
            
            switch (table[i][j]) {
                case TOPLAY: if (j > i) {
                                net.add_connection(i, j, 3);
                                possible[i] += 3;
                             }
                             break;
                case WIN: rest[i] -= 3;
                          break;
                case OTWIN: rest[i] -= 2;
                            break;
                case OTLOSS: rest[i] -= 1;
            }
        }
    }
    
    for (size_t i = 0; i < teams; ++i) {
        int temp;
        std::cin >> temp;
        rest[i] += temp;
        net.add_connection(i, teams + 1, rest[i]);
        net.add_connection(teams, i, possible[i]);
    }
    
    dinic(net);
    for (size_t i = 0; i < net.size() - 2; ++i) {
        for (auto e: net[i]) {
            if (table[i][e.to] == TOPLAY && e.to != net.finish && e.to != net.start && i < e.to) {
                switch (e.flow) {
                    case 0: table[i][e.to] = WIN;
                            table[e.to][i] = LOSS;
                            break;
                    case 1: table[i][e.to] = OTWIN;
                            table[e.to][i] = OTLOSS;
                            break;
                    case 2: table[i][e.to] = OTLOSS;
                            table[e.to][i] = OTWIN;
                            break;
                    case 3: table[i][e.to] = LOSS;
                            table[e.to][i] = WIN;
                }
            }
        }
    }
    
    for (size_t i = 0; i < teams; ++i) {
        for (size_t j = 0; j < teams; ++j) {
            std::cout << table[i][j];
        }
        std::cout << '\n';
    }
    
    return 0;
}