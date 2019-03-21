#include <iostream>
#include <iomanip>
#include <cmath>
#include <limits>
#include <vector>
#include <list>


class Net {
public:
    using Point = std::pair<int, int>;
    Net(size_t quantity) : q(quantity) {}    
    
private: 
    class StartV {
    public:
        StartV(int x, int y, int v) : x(x), y(y), v(v) {}
        
        double distance_to(Point p) const {
            return std::sqrt(std::pow((p.first - x), 2.0) + std::pow((p.second - y), 2.0));
        }
        
        double time_to(Point p) const {
            return distance_to(p) / v;
        }
        
        const int x, y, v;
    };

public:
    const size_t q;
    std::vector<StartV> start;
    std::vector<Point> finish;
};


bool try_kuhn(std::vector<std::list<size_t>>& edges, int* matching_vertex, bool* visited, size_t v) {
    if (visited[v]) {
        return false;
    }
    
    visited[v] = true;
    for (auto i: edges[v]) {
        if (matching_vertex[i] == -1 || try_kuhn(edges, matching_vertex, visited, matching_vertex[i])) {
            matching_vertex[i] = v;
            return true;
        }
    }
    
    return false;
}

bool find_matching(Net& net, double bound) {
    std::vector<std::list<size_t>> edges(net.q);
    int matching_vertex[net.q];
    std::fill(matching_vertex, matching_vertex + net.q, -1);    

    for (size_t i = 0; i < net.q; ++i) {
        for (size_t j = 0; j < net.q; ++j) {
            if (net.start[i].time_to(net.finish[j]) <= bound) {
                edges[i].push_back(j);
            }
        }
    }
    
    for (size_t i = 0; i < net.q; ++i) {
        bool visited[net.q];
        std::fill(visited, visited + net.q, false);
        if (!try_kuhn(edges, matching_vertex, visited, i)) {
            return false;
        }
    }
    return true;
}

double find_time(Net& net) {
    double l = 0.0;
    double r = std::numeric_limits<double>::max();
    double eps = 10e-10;
    
    while (r - l > eps) {
        double m = (l + r) / 2;
        bool y = find_matching(net, m);
        if (y) {
            r = m;
        } else {
            l = m;
        }
    }
    return l;
}

int main() {
    size_t quantity;
    std::cin >> quantity;
    Net net(quantity);
    
    for (size_t i = 0; i < quantity; ++i) {
        double x, y, v;
        std::cin >> x >> y >> v;
        net.start.emplace_back(x, y, v);
    }
    
    for (size_t i = 0; i < quantity; ++i) {
        double x, y;
        std::cin >> x >> y;
        net.finish.emplace_back(x, y);
    }
    
    std::cout << std::setprecision(10) << find_time(net) << '\n';
    return 0;
}