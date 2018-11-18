#include <iostream>
#include <fstream>
#include <list>
#include <map>

struct visitor {
    friend std::istream& operator>>(std::istream& in, visitor& v) {
        std::string temp;
        in >> temp;
        v.coming = (temp[0] == '+');
        v.name = temp.substr(1);
        return in;
    }

    std::string name;
    bool coming;
};

struct condition {
    friend std::istream& operator>>(std::istream& in, condition& c) {
        std::string trash;
        in >> c.visitors.first >> trash >> c.visitors.second;
        return in;
    }

    std::pair<visitor, visitor> visitors;
};

inline void reverse(std::list<int>* result, std::list<int>* graph, int vq) {
    for (int i = 0; i < vq; ++i) {
        for (auto j: graph[i]) {
            result[j].push_back(i);
        }
    }
}

void straight_dfs(std::list<int>* graph, int v, bool* visited,
        std::list<int>& order) {
    visited[v] = true;

    for (auto i: graph[v]) {
        if (!visited[i]) {
            straight_dfs(graph, i, visited, order);
        }
    }
    order.push_front(v);
}

void reversed_dfs(std::list<int>* graph, int v, int* component, int num) {
    component[v] = num;
    for (auto i: graph[v]) {
        if (component[i] == 0) {
            reversed_dfs(graph, i, component, num);
        }
    }
}

inline void condensation(std::list<int>* graph, int* component, int vq) {
    std::list<int> reversed[vq];
    reverse(reversed, graph, vq);

    bool visited[vq] = {0};
    std::list<int> order;

    for (int i = 0; i < vq; ++i) {
        if (!visited[i]) {
            straight_dfs(graph, i, visited, order);
        }
    }

    int quantity = 1;
    for (auto i: order) {
        if (component[i] == 0) {
            reversed_dfs(reversed, i, component, quantity++);
        }
    }
}

bool two_sat(std::list<int>* edges, int q, std::list<int>& guests) {
    int component[2 * q] = {0};
    condensation(edges, component, 2 * q);

    for (int i = 0; i < q; ++i) {
        if (component[i] == component[i + q]) {
            return false;
        }
        if (component[i] > component[i + q]) {
            guests.push_back(i);
        }
    }

    return true;
}

int main() {
    int f, c;
    std::cin >> f >> c;
    std::list<int> edges[2 * f];
    std::map<std::string, int> friends;
    std::string numbers[f];

    for (int i = 0; i < f; ++i) {
        std::string temp;
        std::cin >> temp;
        friends[temp] = i;
        numbers[i] = temp;
    }

    for (int i = 0; i < c; ++i) {
        condition temp;
        std::cin >> temp;
        std::pair<visitor, visitor> v = temp.visitors;
        edges[friends[v.first.name] + (v.first.coming ? 0 : f)].push_back(
            friends[v.second.name] + (v.second.coming ? 0 : f));
        edges[friends[v.second.name] + (!v.second.coming ? 0 : f)].push_back(
            friends[v.first.name] + (!v.first.coming ? 0 : f));
    }

    bool visited[2 * f] = {0};
    std::list<int> guests;
    if (!two_sat(edges, f, guests)/* || guests.size() == 0*/) {
        std::cout << -1;
    } else {
        std::cout << guests.size() << '\n';
        for (auto i: guests) {
            std::cout << numbers[i] << '\n';
        }
    }

    return 0;
}
