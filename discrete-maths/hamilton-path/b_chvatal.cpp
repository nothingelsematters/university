#include <fstream>
#include <vector>
#include <deque>
#include <algorithm>

int main() {
    std::ifstream fin("chvatal.in");
    std::ofstream fout("chvatal.out");

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
        int temp1 = vertexes[0];
        int temp2 = vertexes[1];
        if (!graph[temp1][temp2]) {
            int j = 2;
            for (; !(graph[temp1][vertexes[j]] &&
                graph[temp2][vertexes[j + 1]]) && j < n - 1; ++j) {}
            if (j == n - 1) {
                for (j = 2; !graph[temp1][vertexes[j]] && j < n; ++j) {}
            }
            std::reverse(vertexes.begin() + 1, vertexes.begin() + j + 1);
        }
        vertexes.pop_front();
        vertexes.push_back(temp1);
    }
    for (auto i: vertexes) {
        fout << i + 1 << ' ';
    }

    return 0;
}
