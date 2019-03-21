#include <fstream>
#include <algorithm>

int main() {
        std::ifstream fin("cobbler.in");
        std::ofstream fout("cobbler.out");
        int n, m;
        fin >> n >> m;
        int a[m], q = 0;
        for (int i = 0; i < m; ++i)
                fin >> a[i];
        std::sort(a, a + m);
        for (; (q <= m) && (n > 0); ++q)
                n -= a[q];
        fout << (n == 0 ? q : q - 1);
        fin.close();
        fout.close();
        return 0;
}
