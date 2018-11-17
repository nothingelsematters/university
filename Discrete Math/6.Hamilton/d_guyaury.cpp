#include <iostream>
#include <fstream>
#include <vector>
#include <deque>
#include <algorithm>

int n;
std::vector<int> v;
std::vector<std::vector<bool>> graph;
bool less(int a, int b) {
    return graph[a][b];
}

void mergesort(int l, int r) {
    if (l == r) return;
    if (r - l == 1) {
        if (less(v[r], v[l])) {
            std::swap(v[r], v[l]);
        }
        return;
    }
    int m = (l + r)  / 2;
    mergesort(m + 1, r);
    mergesort(l, m);
    int buf[n];
    int xl = l;
    int xr = m + 1;
    int cur = 0;
    while (r - l + 1 != cur) {
        if (m < xl) buf[cur++] = v[xr++];
        else if (r < xr) buf[cur++] = v[xl++];
        else if (less(v[xr], v[xl])) buf[cur++] = v[xr++];
        else buf[cur++] = v[xl++];
    }
    for (int i = 0; i < cur; i++)
        v[i + l] = buf[i];
}

int main() {
    std::ifstream fin("guyaury.in");
    std::ofstream fout("guyaury.out");

    fin >> n;
    graph.resize(n, std::vector<bool>(n, false));

    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < i; ++j) {
            char temp;
            fin >> temp;
            if (temp == '1') {
                graph[i][j] = true;
            } else {
                graph[j][i] = true;
            }
        }
        v.push_back(i);
    }
    mergesort(0, n - 1);
    while (!less(v[n - 1], v[0])) {
        std::random_shuffle(v.begin(), v.end());
        mergesort(0, n - 1);
    }

    for (auto i: v) {
        fout << i + 1 << ' ';
    }

    return 0;
}
