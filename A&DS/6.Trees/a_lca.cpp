#include <iostream>
#include <cmath>
#include <set>
using namespace std;

struct vertex {
        int parent, depth;
        set<int> children;
        vertex() : parent(0), depth(1) {}
};

vertex tree[200010];
int dp[500000][20];

void depth_calculate(int v, int current) {
        tree[v].depth = current;
        for (auto i : tree[v].children)
                depth_calculate(i, current + 1);
}

int lca(int u, int v, int way) {
        if (tree[v].depth > tree[u].depth)
                swap(v, u);
        for (int i = way; i >= 0; --i)
                if (tree[u].depth - tree[v].depth >= (1 << i))
                        u = dp[u][i];
        if (v == u)
                return v;
        for (int i = way; i >= 0; --i)
                if (dp[v][i] != dp[u][i]) {
                        v = dp[v][i];
                        u = dp[u][i];
                }
        return tree[v].parent;
}

int main() {
        ios_base::sync_with_stdio(false);
        cin.tie(0);
        cout.tie(0);
        int n, m;
        cin >> n;
        int way = log2(n) + 1;
        for (int i = 1; i < n; ++i) {
                cin >> tree[i].parent;
                tree[--tree[i].parent].children.insert(i);
        }
        depth_calculate(0, 1);
        for (int i = 0; i < n; ++i) {
                dp[i][0] = tree[i].parent;
                for (int j = 1; j <= way; ++j)
                        dp[i][j] = dp[dp[i][j - 1]][j - 1];
        }
        cin >> m;
        for (int i = 0; i < m; ++i) {
                int u, v;
                cin >> u >> v;
                cout << lca(u - 1, v - 1, way) + 1 << '\n';
        }
        return 0;
}
