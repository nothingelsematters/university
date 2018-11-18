#include <iostream>
#include <list>

std::list<int>* edges;
std::list<int> result;
int vertexes_quantity, edges_quantity;
bool* visited;
bool* current;

void dfs(int v) {
    current[v] = true;
    visited[v] = true;
    for (auto i: edges[v]) {
        if (!visited[i]) {
            dfs(i);
        } else if (current[i]) {
            std::cout << -1;
            exit(0);
        }
    }
    current[v] = false;
    result.push_front(v);
}

void topsort() {
    visited = new bool[vertexes_quantity];
    current = new bool[vertexes_quantity];
    for (int i = 0; i < vertexes_quantity; ++i) {
        visited[i] = false;
        current[i] = false;
    }
    for (int i = 0; i < vertexes_quantity; ++i) {
        if (!visited[i]) {
            dfs(i);
        }
    }
}

int main() {
    std::cin >> vertexes_quantity >> edges_quantity;
    edges = new std::list<int>[vertexes_quantity];
    for (int i = 0; i < edges_quantity; ++i) {
        int start, end;
        std::cin >> start >> end;
        edges[start - 1].push_back(end - 1);
    }

    topsort();
    for (auto i: result) {
        std::cout << (i + 1) << ' ';
    }
    return 0;
}
