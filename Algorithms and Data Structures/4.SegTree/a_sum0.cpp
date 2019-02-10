#include <fstream>
#include <ctime>
unsigned long long a[10000001];
unsigned int b[20000002];
int main() {
        unsigned int clocky = clock();
        std::ifstream fin("sum0.in");
        std::ofstream fout("sum0.out");
        unsigned int n, x, y, m, z;
        int t;
        fin >> n >> x >> y >> a[1] >> m >> z >> t >> b[0];
        unsigned int const q = (1 << 16) - 1, k = (1 << 30) - 1;
        unsigned long long ans = 0;
        unsigned int done = 2;
        for (unsigned int i = 1; i <= 2 * m; i += 2) {
                b[i + 1] = (z * (b[i] = (z * b[i - 1] + t) & k) + t) & k;
                if ((b[i] %= n) > (b[i - 1] %= n)) std::swap(b[i], b[i - 1]);
                for (; done <= b[i - 1] + 2; ++done) {
                        a[done] = (x * a[done - 1] + y) & q;
                        a[done - 1] += a[done - 2];
                }
                ans += a[b[i - 1] + 1] - a[b[i]];
        }
        fout << ans << "\n" << (float) (clock() - clocky) / 1000000;
}
