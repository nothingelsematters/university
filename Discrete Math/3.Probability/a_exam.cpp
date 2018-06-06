#include <fstream>

int main() {
        std::ios_base::sync_with_stdio(false);
        std::ifstream fin("exam.in");
        std::ofstream fout("exam.out");
        int k, n;
        double ans = 0;
        fin >> k >> n;
        for (int i = 0; i < k; ++i) {
                int a, b;
                fin >> a >> b;
                ans += ((double) a * (double) b) / 100 / n;
        }
        fout.precision(5);
        fout << ans;
}
