#include <iostream>
#include <vector>
using namespace std;

struct vertex {
        int parent, depth;
        vertex() : parent(0), depth(1) {}
};

vertex tree[200010];
vector<vector<int>> dp;

int lca(int u, int v) {
        if (tree[v].depth > tree[u].depth)
                swap(v, u);
        for (int i = 19; i >= 0; --i)
                if (tree[u].depth - tree[v].depth >= (1 << i))
                        u = dp[u][i];
        if (v == u)
                return v;
        for (int i = 19; i >= 0; --i)
                if (dp[v][i] != dp[u][i]) {
                        v = dp[v][i];
                        u = dp[u][i];
                }
        return dp[v][0];
}

int funerals(int index) {
        if (tree[index].parent == index)
                return index;
        tree[index].parent = funerals(tree[index].parent);
        return tree[index].parent;
}

void kill(int dead, int parent) {
        dead = funerals(dead);
        parent = funerals(parent);
        if (parent == dead)
                return;
        if (tree[dead].depth < tree[parent].depth)
                tree[parent].parent = dead;
        else
                tree[dead].parent = parent;
}

int main() {
        ios_base::sync_with_stdio(false);
        cin.tie(0);
        dp.resize(200010, vector<int> (20, 0));
        int m, count = 1;
        cin >> m;
        for (int i = 0; i < m; ++i) {
                char c;
                int u, v;
                cin >> c >> u;
                switch (c) {
                        case '+' :
                                tree[count].parent = count;
                                tree[count].depth = tree[--u].depth + 1;
                                dp[count][0] = u;
                                for (int k = 1; k < 20; ++k)
                                        dp[count][k] = dp[dp[count][k - 1]][k - 1];
                                ++count;
                                break;
                        case '-' :
                                kill(u - 1, dp[u - 1][0]);
                                break;
                        case '?' :
                                cin >> v;
                                cout << funerals(lca(u - 1, v - 1)) + 1 << '\n';
                }
        }
        return 0;
}
