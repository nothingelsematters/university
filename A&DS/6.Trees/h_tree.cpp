#include <iostream>
#include <cmath>
#include <vector>
#include <set>
using namespace std;

struct vertex {
        int parent, depth, count, colour;
        set<int> sons;
        vertex() : parent(0), depth(1), count(1) {}
};

int dp[(int) 1e6][21];
int last[(int) 1e6];
vertex tree[(int) 1e6];
int way;

void depth_calculate(int v, int current) {
        tree[v].depth = current;
        for (auto i : tree[v].sons)
                depth_calculate(i, current + 1);
}

int lca(int u, int v) {
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

void superdfs(int key) {
        for (auto i: tree[key].sons) {
                superdfs(i);
                tree[key].count += tree[i].count;
        }
        if (last[tree[key].colour] != -1)
                --tree[lca(last[tree[key].colour], key)].count;
        last[tree[key].colour] = key;
}

void dfs_dp(int key) {
        dp[key][0] = tree[key].parent;
        for (int j = 1; j <= way; ++j)
                dp[key][j] = dp[dp[key][j - 1]][j - 1];
        for (auto i : tree[key].sons)
                dfs_dp(i);
}

int main() {
        ios_base::sync_with_stdio(false);
        cin.tie(0);
        cout.tie(0);
        int n;
        cin >> n;
        way = log2(n) + 1;
        int root = 0;
        for (int i = 0; i < n; ++i) {
                last[i] = -1;
                int temp, col;
                cin >> temp >> col;
                tree[i].colour = col - 1;
                if (temp == 0) {
                        tree[i].parent = i;
                        root = i;
                } else {
                        tree[i].parent = temp - 1;
                        tree[temp - 1].sons.insert(i);
                }
        }
        depth_calculate(root, 1);
        dfs_dp(root);
        superdfs(root);
        for (int i = 0; i < n; ++i)
                cout << tree[i].count << ' ';
        return 0;
}
