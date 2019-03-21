#include <iostream>
#include <limits>
#include <vector>
#include <queue>
#include <list>


class Net {
public:
    class Edge {
    public:
        Edge(size_t to, size_t x, size_t y, int maxcapacity) : to(to), maxcapacity(maxcapacity), x(x), y(y) {}

        int get_capacity() const {
            return maxcapacity - flow;
        }

        Edge* reverse = nullptr;
        int flow = 0;
        const int maxcapacity;
        const size_t to, x, y;
    };
    
    Net(size_t initial_size = 0, bool oriented = true) : oriented(oriented) {
        edges.resize(initial_size);
    }
    
    std::list<Edge>& operator[](size_t index) {
        return edges[index];
    }

    void add_connection(size_t from, size_t to, size_t x, size_t y, int maxc = 1) {
        size_t update = std::max(from, to);
        if (update >= edges.size()) {
            edges.resize(update + 1);
        }
        insert(from, to, x, y, maxc);
        if (!oriented) {
            insert(to, from, x, y, maxc);
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
    
    void insert(size_t from, size_t to, size_t x, size_t y, int maxc) {
        edges[from].push_back(Edge(to, x, y, maxc));
        edges[to].push_back(Edge(from, x, y, 0));
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

void full_dfs(Net& net, bool* accessible, size_t v) {
    if (accessible[v]) {
        return;
    }
    accessible[v] = true;

    for (auto e: net[v]) {
        if (e.maxcapacity != e.flow) {
            full_dfs(net, accessible, e.to);
        }
    }
}

class Hatch {
public: 
    Hatch(bool ld, size_t x, size_t y) : left_down(ld), x(x), y(y) {}
    
    const bool left_down;
    const size_t x, y;
};

std::vector<Hatch> paint_table(std::vector<std::vector<bool>> const& table, size_t width, size_t length) {
    Net net((width + length) * 2);
    net.start = 0;
    net.finish = (width + length) * 2 - 1;
    size_t diagonals = width + length - 1;
    
    for (size_t i = 0; i < width; ++i) {
        for (size_t j = 0; j < length; ++j) {
            if (table[i][j]) {
                net.add_connection(i - j + length, i + j + length + width, i, j);
            }
        }
    }
    
    for (size_t i = 0; i < width + length - 1; ++i) {
        net.add_connection(net.start, i + 1, 0, 0);
        net.add_connection(i + width + length, net.finish, 0, 0);
    }
    
    dinic(net);
    bool visited[net.size()];
    std::fill(visited, visited + net.size(), false);
    full_dfs(net, visited, net.start);
    
    std::vector<Hatch> result;
    for (size_t i = 1; i < diagonals + 1; ++i) {
        if (visited[i]) {
            continue;
        }
        for (auto e: net[i]) {
            if (e.flow != 1) {
                continue;
            }
            result.emplace_back(true, e.x, e.y);
        }
    }
    
    for (size_t i = diagonals + 1; i < net.finish; ++i) {
        if (!visited[i]) {
            continue;
        }
        for (auto e: net[i]) {
            if (e.flow != -1) {
                continue;
            }
            result.emplace_back(false, e.x, e.y);
        }
    }
    
    return std::move(result);
}

void reverse_table(std::vector<std::vector<bool>>& table, size_t width, size_t length) {
    for (size_t i = 0; i < width; ++i) {
        for (size_t j = 0; j < length; ++j) {
            table[i][j] = !table[i][j];
        }
    }
}

std::pair<bool, std::vector<Hatch>> normalize_table(std::vector<std::vector<bool>>& table, size_t width, size_t length) {
    auto first = paint_table(table, width, length);
    reverse_table(table, width, length);
    auto second = paint_table(table, width, length);
    
    return first.size() < second.size() ? std::make_pair(true, std::move(first)) : std::make_pair(false, std::move(second));
}

int main() {
    size_t width, length;
    std::cin >> width >> length;
    
    std::vector<std::vector<bool>> table(width, std::vector<bool>(length));
    
    for (size_t i = 0; i < width; ++i) {
        for (size_t j = 0; j < length; ++j) {
            char c;
            std::cin >> c;
            table[i][j] = c == 'B' ? !((i + j) & 1) : (i + j) & 1;
        }
    }
    
    auto [white, result] = normalize_table(table, width, length);
    std::cout << result.size() << '\n';
    for (auto i: result) {
        bool colour = ((i.x + i.y) & 1);
        if (white) {
            colour = !colour;
        }
        std::cout << i.left_down + 1 << ' ' << i.x + 1 << ' ' << i.y + 1 << ' ' << (colour ? 'W' : 'B') << '\n';
    }
    
    return 0;
}