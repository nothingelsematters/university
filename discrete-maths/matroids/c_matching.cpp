#include <fstream>
#include <vector>
#include <algorithm>

bool dfs(std::vector<std::vector<int>> const& graph, int* right, int* left,
        bool* visited, int index) {
    if (visited[index]) {
        return false;
    }
    visited[index] = true;
    for (auto i: graph[index]) {
        if (right[i] == -1 || dfs(graph, right, left, visited, right[i])) {
            right[i] = index;
            left[index] = i;
            return true;
        }
    }
    return false;
}

int main() {
    std::ifstream fin("matching.in");
    std::vector<std::vector<int>> graph;
    int quantity;
    fin >> quantity;
    int weight[quantity];

    for (int i = 0; i < quantity; ++i) {
        fin >> weight[i];
    }
    for (int i = 0; i < quantity; ++i) {
        int size;
        fin >> size;
        graph.push_back(std::vector<int>());
        for (int j = 0; j < size; ++j) {
            int element;
            fin >> element;
            graph.back().push_back(element - 1);
        }
    }
    fin.close();

    int order[quantity];
    for (int i = 0; i < quantity; ++i) {
        order[i] = i;
    }
    std::sort(order, order + quantity,
        [&weight] (int a, int b) {
        return weight[a] > weight[b];
    });

    int right[quantity];
    int left[quantity];
    std::fill(right, right + quantity, -1);
    std::fill(left, left + quantity, -1);
    for (int i = 0; i < quantity; ++i) {
        bool visited[quantity];
        std::fill(visited, visited + quantity, false);
        dfs(graph, right, left, visited, order[i]);
    }

    std::ofstream fout("matching.out");
    for (auto i: left) {
        fout << i + 1 << ' ';
    }
    fout.close();

    return 0;
}
