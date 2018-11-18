#include <fstream>

int main() {
    std::ifstream fin("dowry.in");
    int quantity;
    long long L, R;
    fin >> quantity >> L >> R;
    std::pair<long long, long long> diamonds[count];
    for (int i = 0; i < count; ++i)
        fin >> diamonds[quantity].first >> diamonds[quantity].second;
    fin.close();
    long long maxP = 0;
    unsigned int mask = 0;
    for (unsigned int i = 1; i <= (1 << quantity) - 1; ++i) {
        long long price = 0, weight = 0;
        unsigned int j = i;
        for (int count = 0; j > 0 && weight <= R; ++count, j >>= 1)
            if (j & 1) {
                weight += diamonds[count].first;
                price += diamonds[count].second;
            }
        if (weight >= L && weight <= R && price > maxP) {
            maxP = price;
            mask = i;
        }
    }
    std::ofstream fout("dowry.out");
    if (!maxP)
        fout << 0;
    else {
        int c = 0;
        for (unsigned int i = mask; i > 0; i >>= 1)
            if (i & 1)
                ++c;
        fout << c << '\n';
        int count = 1;
        for (unsigned int i = mask; i > 0; ++count, i >>= 1)
            if (i & 1)
                fout << count << ' ';
    }
    fout.close();
    return 0;
}
