#include <fstream>

int main() {
        std::ifstream fin("sqroot.in");
        std::ofstream fout("sqroot.out");
        int a[4][4], sum;
        const int bound = 1 << 16;
        bool c;
        for (int i = 0; i < 4; ++i)
                for (int j = 0; j < 4; ++j)
                        fin >> a[i][j];
        for (int i = 0; i < bound; ++i, c = true) {
                for (int j = 0; c && j < 4; ++j) {
                        for (int k = 0; c && k < 4; ++k, sum = 0) {
                                for (int l = 0; l < 4; ++l)
                                        sum += ((i >> (j * 4 + l)) & 1) * ((i >> (k + 4 * l)) & 1);
                                if ((sum & 1) != a[j][k])
                                        c = false;
                        }
                }
                if (c) {
                        for (int j = 0; j < 4; ++j) {
                                for (int k = 0; k < 4; ++k)
                                        fout << ((i >> (4 * j + k)) & 1) << ' ';
                                fout << '\n';
                        }
                        return 0;
                }
        }
        fout << "NO SOLUTION";
        return 0;
}
