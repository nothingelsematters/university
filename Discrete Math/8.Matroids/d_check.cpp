#include <fstream>
#include <algorithm>
#include <vector>
#include <set>

int main() {
    std::ifstream fin("check.in");
    size_t n, m;
    fin >> n >> m;
    std::set<unsigned short> subset;

    bool zero = false;
    for (size_t i = 0; i < m; ++i) {
        unsigned short tmp = 0;
        size_t size;
        fin >> size;
        if (size == 0) {
            zero = true;
        }
        for (size_t j = 0; j < size; ++j) {
            size_t index;
            fin >> index;
            tmp |= (1 << (index - 1));
        }
        subset.insert(tmp);
    }
    fin.close();

    std::ofstream fout("check.out");
    if (!zero) {
        fout << "NO";
        return 0;
    }

    for (auto i: subset) {
        for (size_t j = 0; j <= n; ++j) {
            unsigned short to_find = (~(1 << j)) & i;
            if (subset.find(to_find) == subset.end()) {
                fout << "NO";
                return 0;
            }
        }
    }

    for (auto i: subset) {
        for (auto j: subset) {
            size_t size_i = 0;
            size_t size_j = 0;
            for (unsigned short l = i; l > 0; size_i += (l & 1), l >>= 1);
            for (unsigned short l = j; l > 0; size_j += (l & 1), l >>= 1);
            if (size_i <= size_j) continue;
            bool exists = false;
            for (size_t k = 0; k < n; ++k) {
                if (!(i & (1 << k))) continue;
                if (j & (1 << k)) continue;
                unsigned short to_find = j | (1 << k);
                if (subset.find(to_find) != subset.end()) {
                    exists = true;
                    break;
                }
            }
            if (!exists) {
                fout << "NO";
                return 0;
            }
        }
    }

    fout << "YES";
    return 0;
}
