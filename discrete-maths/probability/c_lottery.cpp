#include <fstream>

using namespace std;

int main() {
        ifstream fin("lottery.in");
        ofstream fout("lottery.out");
        int n, m, a, b;
        double ans = 0;
        fin >> n >> m >> a >> b;
        double prev = 1 /((double) a);
        ans += n * (((double) a - 1) / (double) a);
        for (int i = 1; i < m; ++i) {
                fin >> a;
                ans += ((double)(n - b)) * prev * (((double) a - 1) / ((double) a));
                prev *= 1 / ((double) a);
                fin >> b;
        }
        ans += ((double)(n - b)) * prev;
        fout.precision(m * 4);
        fout << ans;
        fin.close();
        fout.close();
}
