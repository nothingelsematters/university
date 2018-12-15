#include <fstream>
#include <algorithm>

void dfs(int set, int x_quantity, bool* visited) {
    if (visited[set]) return;
    visited[set] = true;
    for (int i = 0; i < x_quantity; ++i) {
        if (!(set & (1 << i))) {
            dfs(set | (1 << i), x_quantity, visited);
        }
    }
}

int main() {
    std::ifstream fin("cycles.in");
    int x_quantity, s_quantity;
    fin >> x_quantity >> s_quantity;
    int cycles[s_quantity];

    std::pair<int, int> weight[x_quantity];
    for (int i = 0; i < x_quantity; ++i) {
        fin >> weight[i].first;
        weight[i].second = i;
    }

    for (int i = 0; i < s_quantity; i++) {
        int size;
        fin >> size;
        cycles[i] = 0;
        for (int j = 0; j < size; ++j) {
            int temp;
            fin >> temp;
            cycles[i] |= 1 << (temp - 1);
        }
    }
    fin.close();

    std::sort(weight, weight + x_quantity,
        [] (std::pair<int, int> const& a, std::pair<int, int> const& b) {
            return a > b;
        });

    bool visited[1 << x_quantity];
    std::fill(visited, visited + (1 << x_quantity), false);
    for (auto i: cycles) {
        dfs(i, x_quantity, visited);
    }

    int result = 0;
    for (int i = 0, index = 0; i < x_quantity; ++i) {
        if (!visited[index | 1 << weight[i].second]) {
            result += weight[i].first;
            index |= (1 << weight[i].second);
        }
    }

    std::ofstream fout("cycles.out");
    fout << result;
    fout.close();

    return 0;
}
