#include <fstream>
#include <iostream>
#include <vector>
#include <map>
#include <set>
#include <cmath>
#include <algorithm>


struct Point {
    Point() {}

    Point(long double x, long double y) : x(x), y(y) {}

    bool operator<(Point that) const {
        if (x < that.x) {
            return true;
        }
        return x == that.x && y < that.y;
    }

    long double x;
    long double y;
};


struct Line {
    Line() {}

    Line(Point const& first, Point const& second) {
        a = first.y - second.y;
        b = second.x - first.x;
        c = first.x * second.y - first.y * second.x;
    }

    long double a;
    long double b;
    long double c;
};

std::vector<Point> reverse_index;





int main() {
    size_t quantity;
    std::cin >> quantity;
    Line lines[quantity];

    std::map<Point, size_t> index;
    std::set<Point> intersections[quantity];
    std::vector<std::vector<size_t>> graph;

    for (size_t i = 0; i < quantity; ++i) {
        Point first, second;
        std::cin >> first.x >> first.y >> second.x >> second.y;
        lines[i] = Line(first, second);

        for (size_t j = 0; j < i; ++j) {
            long double denominator = lines[i].a * lines[j].b - lines[i].b * lines[j].a;
            if (denominator != 0) {
                Point p((lines[j].c * lines[i].b - lines[j].b * lines[i].c) / denominator,
                    (lines[j].a * lines[i].c - lines[j].c * lines[i].a) / denominator);
                intersections[i].insert(p);
                intersections[j].insert(p);
            }
        }
    }

    for (size_t i = 0; i < quantity; ++i) {
        if (intersections[i].size() < 2) {
            continue;
        }
        for (auto j = intersections[i].begin(); j != --intersections[i].end(); ) {
            Point begin = *j;
            Point end = *(++j);
            if (index.find(begin) == index.end()) {
                index[begin] = graph.size();
                reverse_index.push_back(begin);
                graph.push_back(std::vector<size_t>());
            }
            if (index.find(end) == index.end()) {
                index[end] = graph.size();
                reverse_index.push_back(end);
                graph.push_back(std::vector<size_t>());
            }

            graph[index[begin]].push_back(index[end]);
            graph[index[end]].push_back(index[begin]);
        }
    }

    for (size_t i = 0; i < graph.size(); ++i) {
        std::sort(graph[i].begin(), graph[i].end(), PolarComparator(i));
    }

    std::vector<std::vector<bool>> visited(graph.size());
    for (size_t i = 0; i < graph.size(); ++i) {
        visited[i].resize(graph[i].size(), false);
    }

    std::vector<long double> areas;
    for (size_t i = 0; i < graph.size(); ++i) {
        for (size_t j = 0; j < graph[i].size(); ++j) {
            if (!visited[i][j]) {
                visited[i][j] = true;
                size_t current = graph[i][j];
                size_t p = i;
                std::vector<size_t> face;

                while (true) {
                    face.push_back(current);
                    auto found = std::lower_bound(graph[current].begin(),
                        graph[current].end(), p, PolarComparator(current));
                    if (found == graph[current].end()) {
                        continue;
                    }
                    if (++found == graph[current].end()) {
                        found = graph[current].begin();
                    }
                    if (visited[current][found - graph[current].begin()]) {
                        break;
                    }
                    visited[current][found - graph[current].begin()] = true;
                    p = current;
                    current = *found;
                }

                long double area = 0.0;
                if (face.size() < 2) {
                    continue;
                }
                for (size_t i = 0, j = face.size() - 1; i < face.size(); j = i, ++i) {
                    area += (reverse_index[face[j]].x + reverse_index[face[i]].x) *
                        (reverse_index[face[j]].y - reverse_index[face[i]].y);
                }
                area /= 2.0;
                if (area >= 1e-8) {
                    areas.push_back(area);
                }
            }
        }
    }

    std::sort(areas.begin(), areas.end());

    std::cout << areas.size() << '\n';
    for (auto i: areas) {
        std::cout.precision(15);
        std::cout << i << '\n';
    }

    return 0;
}
