#include <iostream>
#include <fstream>
#include <list>
#include <vector>

int main() {
    std::ifstream fin("temp.in");
    std::string str, main;
    fin >> str >> main;

    int zf[str.size()];
    std::fill(zf, zf + str.size(), 0);
    int left = 0;
    int right = 0;
    for (int i = 1; i < str.size(); ++i) {
        zf[i] = std::max(0, std::min(right - i, zf[i - left]));
        while (i + zf[i] < str.size() && str[zf[i]] == str[i + zf[i]]) {
            zf[i]++;
        }
        if (i + zf[i] > right) {
            left = i;
            right = i + zf[i];
        }
    }

    const int size = str.size();
    left = 0;
    right = 0;
    std::vector<int> z_cache(size, 0);
    std::list<int> coordinates;

    for (int i = 0; i < main.size(); ++i) {
        int cur = i % size;
        z_cache[cur] = std::max(0, std::min(right - i, zf[i - left]));
        while (i < main.size() && z_cache[cur] < size &&
               i + z_cache[cur] < main.size() &&
               main[i + z_cache[cur]] ==
                    str[z_cache[cur]]) {
            z_cache[cur]++;
        }
        if (i + z_cache[cur] > right) {
            left = i;
            right = i + z_cache[cur];
        }
        if (z_cache[cur] == size) {
            coordinates.push_back(i + 1);
        }
    }

    std::cout << coordinates.size() << '\n';
    for (auto i: coordinates) {
        // std::cout << i << ' ';
    }
}
