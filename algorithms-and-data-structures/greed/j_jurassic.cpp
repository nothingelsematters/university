#include <fstream>

int main() {
    std::ifstream fin("jurassic.in");
    int quantity;
    fin >> quantity;
    int bones[quantity];
    for (int i = 0; i < quantity; ++i) {
        std::string s;
        fin >> s;
        int mask = 0;
        for (char j: s)
            mask += 1 << (s[j] - 'A');
        bones[i] = mask;
    }
    fin.close();
    int max_quantity = 0, max_mask = 0;
    for (int i = 1; i < 1 << quantity; ++i) {
        int j = i, mask = 0, many = 0;
        for (int count = 0; j > 0; ++count, j >>= 1)
            if (j & 1) {
                ++many;
                mask ^= bones[count];
            }
        if (!mask && many > max_quantity) {
            max_quantity = many;
            max_mask = i;
        }
    }
    std::ofstream fout("jurassic.out");
    fout << max_quantity << '\n';
    for (int i = 1; max_mask > 0; ++i, max_mask >>= 1)
        if (max_mask & 1)
            fout << i << ' ';
    fout.close();
    return 0;
}
