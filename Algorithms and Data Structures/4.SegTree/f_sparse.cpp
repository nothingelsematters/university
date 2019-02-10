#include <fstream>
using namespace std;

int pp(int const& x){
        if (x == 1) return 0;
        return pp(x / 2) + 1;
}

long long sparse[100000][17], p[100000];

void build(int const& n) {
        for (int i = 1; i < n; ++i) {
                p[i] = pp(i);
                sparse[i][0] = (23 * sparse[i - 1][0] + 21563) % 16714589;
        }
        for (int j = 1; (1 << j) < n; ++j)
                for (int i = 0; i + (1 << j) < n; ++i)
                        sparse[i][j] = min(sparse[i][j - 1], sparse[i + (1 << (j - 1))][j - 1]);
}

int answer(int const& u, int const& v){
        if (v > u) {
                int k = p[v - u];
                return min(sparse[u][k], min(sparse[v][0], sparse[v - (1 << k)][k]));
        }
        if (u > v) {
                int k = p[u - v];
                return min(sparse[v][k], min(sparse[u][0], sparse[u - (1 << k)][k]));
        }
        return sparse[u][0];
}

int main(){
        ifstream fin("sparse.in");
        ofstream fout("sparse.out");
        int n, m, k;
        fin >> n >> m;
        fin >> sparse[0][0];
        build(n);
        long long u, v, ans;
        fin >> u >> v;
        --u; --v;
        for (int i = 1; i < m; ++i) {
                ans = answer(u, v);
                u = ((17 * (u + 1) + 751 + ans + 2 * i) % n);
                v = ((13 * (v + 1) + 593 + ans + 5 * i) % n);
        }
        fout << u + 1 << ' ' << v + 1 << ' ' << answer(u, v);
        fin.close();
        fout.close();
}
