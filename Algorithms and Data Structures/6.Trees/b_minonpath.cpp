#include <fstream>
#include <cmath>
#include <set>
using namespace std;

struct vertex {
        int parent, depth, cost;
        set<int> children;
        vertex() : parent(0), depth(1), cost(1e9) {}
};

vertex tree[200010];
int dp[500000][20];
int cst[500000][20];

void depth_calculate(int v, int current) {
        tree[v].depth = current;
        for (auto i : tree[v].children)
                depth_calculate(i, current + 1);
}

int lca(int u, int v, int way) {
        int minimum = 1e9;
        if (tree[v].depth > tree[u].depth)
                swap(v, u);
        for (int i = way; i >= 0; --i)
                if (tree[u].depth - tree[v].depth >= (1 << i)) {
                        minimum = min(minimum, cst[u][i]);
                        u = dp[u][i];
                }
        if (v == u)
                return minimum;
        for (int i = way; i >= 0; --i)
                if (dp[v][i] != dp[u][i]) {
                        minimum = min(minimum, cst[v][i]);
                        minimum = min(minimum, cst[u][i]);
                        v = dp[v][i];
                        u = dp[u][i];
                }
        minimum = min(minimum, cst[u][0]);
        return min(minimum, cst[v][0]);
}

int main() {
        ifstream fin("minonpath.in");
        ofstream fout("minonpath.out");
        int n, m;
        fin >> n;
        int way = log2(n) + 1;
        for (int i = 1; i < n; ++i) {
                fin >> tree[i].parent >> tree[i].cost;
                tree[--tree[i].parent].children.insert(i);
        }
        depth_calculate(0, 1);
        for (int i = 0; i < n; ++i) {
                cst[i][0] = tree[i].cost;
                dp[i][0] = tree[i].parent;
                for (int j = 1; j <= way; ++j) {
                        dp[i][j] = dp[dp[i][j - 1]][j - 1];
                        cst[i][j] = min(cst[i][j - 1], cst[dp[i][j - 1]][j - 1]);
                }
        }
        fin >> m;
        for (int i = 0; i < m; ++i) {
                int u, v;
                fin >> u >> v;
                fout << lca(u - 1, v - 1, way) << '\n';
        }
        return 0;
}
