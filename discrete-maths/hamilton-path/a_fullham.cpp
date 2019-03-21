#include <fstream>
#include <vector>
#include <deque>
#include <algorithm>

int main() {
    std::ifstream fin("fullham.in");
    std::ofstream fout("fullham.out");

    int n;
    fin >> n;
    std::vector<std::vector<bool>> graph(n, std::vector<bool>(n, false));
    std::deque<int> vertexes;

    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < i; ++j) {
            char temp;
            fin >> temp;
            if (temp == '1') {
                graph[i][j] = true;
                graph[j][i] = true;
            }
        }
        vertexes.push_back(i);
    }


    for (int i = 0; i < (n - 1) * n; ++i) {
        int temp1 = vertexes.front();
        vertexes.pop_front();
        int temp2 = vertexes.front();
        if (!graph[temp1][temp2]) {
            int j = 1;
            for (; !(graph[temp1][vertexes.at(j)] &&
                graph[temp2][vertexes.at(j + 1)]); ++j) {}
            std::reverse(vertexes.begin(), vertexes.begin() + j + 1);
        }
        vertexes.push_back(temp1);
    }
    for (auto i: vertexes) {
        fout << i + 1 << ' ';
    }

    return 0;
}
