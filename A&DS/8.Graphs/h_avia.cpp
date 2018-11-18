#include <iostream>
#include <fstream>
#include <climits>
#include <list>
#include <vector>

std::vector<std::vector<bool>> reverse(std::vector<std::vector<bool>> const& graph,
        int n) {
    std::vector<std::vector<bool>> reversed(n, std::vector<bool>(n));

    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < n; ++j) {
            reversed[j][i] = graph[i][j];
        }
    }

    return reversed;
}

void straight_dfs(std::vector<std::vector<bool>> const& graph, int v, bool* visited,
        int& order, int n) {
    visited[v] = true;

    for (int i = 0; i < n; ++i) {
        if (graph[v][i] && !visited[i]) {
            straight_dfs(graph, i, visited, order, n);
        }
    }
    order = v;
}

void reversed_dfs(std::vector<std::vector<bool>> const& graph, int v,
        bool* visited, int& num, int n) {
    visited[v] = true;
    ++num;
    for (int i = 0; i < n; ++i) {
        if (graph[v][i] && !visited[i]) {
            reversed_dfs(graph, i, visited, num, n);
        }
    }
}

bool power_connected(std::vector<std::vector<bool>> const& edges, int n) {
    std::vector<std::vector<bool>> reversed = reverse(edges, n);

    bool visited[n] = {0};
    int order;

    for (int i = 0; i < n; ++i) {
        if (!visited[i]) {
            straight_dfs(edges, i, visited, order, n);
        }
    }

    int quantity = 0;
    bool visited2[n] = {0};
    reversed_dfs(reversed, order, visited2, quantity, n);
    return quantity == n;
}

bool check(int value, std::vector<std::vector<int>> const& weight, int n) {
    std::vector<std::vector<bool>> accepted(n, std::vector<bool>(n, false));

    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < n; ++j) {
            accepted[i][j] = weight[i][j] <= value;
        }
    }
    return power_connected(accepted, n);
}

int bin_search(std::vector<std::vector<int>> const& weight, int n) {
    int l = -1;
    int r = INT_MAX;

    while (l < r - 1) {
        if (check((l + r) / 2, weight, n)) {
            r = (l + r) / 2;
        } else {
            l = (l + r) / 2;
        }
    }

    return r;
}

int main() {
    std::ifstream fin("avia.in");
    std::ofstream fout("avia.out");

    int n;
    fin >> n;
    std::vector<std::vector<int>> weight(n, std::vector<int>(n));
    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < n; ++j) {
            fin >> weight[i][j];
        }
    }

    fout << bin_search(weight, n);
    return 0;
}
