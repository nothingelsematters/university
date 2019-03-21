#include <fstream>
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
    
    Net() {}
    
    std::list<Edge>& operator[](size_t index) {
        return edges[index];
    }

    void add_connection(size_t from, size_t to, int maxc = 1) {
        size_t update = std::max(from, to);
        if (update >= edges.size()) {
            edges.resize(update + 1);
        }
        insert(from, to, maxc);
    }
    
    size_t size() const {
        return edges.size();
    }

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

size_t index(size_t day, size_t max, size_t v) {
    return day * max + v;
}

std::pair<size_t, Net> transfer(size_t systq, size_t tunq, size_t comq, size_t s, size_t t, std::pair<size_t, size_t>* tunnel) {
    size_t days;
    size_t transferred = 0;
    Net net;
    net.start = s;
    
    
    for (days = 0; transferred < comq; ++days) {
        for (size_t i = 0; i < systq; ++i) {
            net.add_connection(index(days, systq, i), index(days + 1, systq, i), std::numeric_limits<int>::max());
        }
        
        for (size_t i = 0; i < tunq; ++i) {
            net.add_connection(index(days, systq, tunnel[i].first), index(days + 1, systq, tunnel[i].second));
            net.add_connection(index(days, systq, tunnel[i].second), index(days + 1, systq, tunnel[i].first));
        }
        
        net.finish = index(days + 1, systq, t);
        transferred += dinic(net);
    }
    
    return {days, std::move(net)};
}

int main() {
    std::ifstream fin("bring.in");
    size_t systems_q, tunnels_q, computers_q, start, finish;
    fin >> systems_q >> tunnels_q >> computers_q >> start >> finish; 
    
    std::pair<size_t, size_t> tunnel[tunnels_q];
    for (size_t i = 0; i < tunnels_q; ++i) {
        fin >> tunnel[i].first >> tunnel[i].second;
        --tunnel[i].first;
        --tunnel[i].second;
    }
    
    auto [days, net] = transfer(systems_q, tunnels_q, computers_q, start - 1, finish - 1, tunnel);
    std::ofstream fout("bring.out");
    fout << days << '\n';
    
    size_t position[computers_q];
    std::fill(position, position + computers_q, net.start);
    
    for (size_t i = 0; i < days; ++i) {
        std::list<std::pair<size_t, size_t>> transitions;
        
        for (size_t j = 0; j < computers_q; ++j) {
            
            for (auto& e: net[position[j]]) {
                if (e.flow >= 1) {
                    e.flow -= 1;
                    if (position[j] + systems_q != e.to) {
                        transitions.emplace_back(j, e.to % systems_q);
                    }
                    position[j] = e.to;
                    
                    break;
                }
            }
            
        }
        
        fout << transitions.size() << "  ";
        for (auto t: transitions) {
            fout << t.first + 1 << ' ' << t.second + 1 << "  ";
        }
        fout << '\n';
    }
    
    return 0;
}