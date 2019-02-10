#include <fstream>
#include <iostream>
#include <algorithm>

long long count = 0;
std::pair<int, int>* cards;

void merge(int l, int r) {
    if (l >= r)
        return;
    merge(l, (l + r) / 2);
    merge((l + r) / 2 + 1, r);
    std::pair<int, int> backup[r - l + 1];
    for (int i = l; i <= r; ++i)
        backup[i - l] = cards[i];
    int index1 = 0, index2 = (l + r) / 2 + 1 - l;
    for (int i = l; i <= r; ++i) {
        if (index2 > r - l || (index1 < (l + r) / 2 + 1 - l && backup[index1].second <= backup[index2].second))
            cards[i] = backup[index1++];
        else {
            cards[i] = backup[index2++];
            if (index1 < (l + r) / 2 + 1 - l)
                count += (l + r) / 2 + 1 - l - index1;
        }
    }
}

int main() {
    std::ifstream fin("john.in");
    int quantity;
    fin >> quantity;
    cards = new std::pair<int, int> [quantity];
    for (int i = 0; i < quantity; ++i)
        fin >> cards[i].first >> cards[i].second;
    fin.close();
    std::sort(cards, cards + quantity);
    merge(0, quantity - 1);
    std::ofstream fout("john.out");
    fout << count;
    fout.close();
    return 0;
}
