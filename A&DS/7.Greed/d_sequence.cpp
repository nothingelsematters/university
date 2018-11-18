#include <fstream>
#include <algorithm>
#include <vector>
using namespace std;

int main() {
    ifstream fin("sequence.in");
    ofstream fout("sequence.out");
    int quantity, sum = 0;
    fin >> quantity;
    pair<int, int> numbers[quantity];
    for (int i = 0; i < quantity; ++i) {
        fin >> numbers[i].first;
        sum += numbers[i].first;
        numbers[i].second = i;
    }
    fin.close();
    sort(numbers, numbers + quantity, [](pair<int, int> a, pair<int, int> b) {
        return a.first > b.first;
    });
    vector<pair<int, int>> one, two;
    int sum1 = 0, sum2 = 0;
    for (int i = 0; i < quantity; ++i) {
        if (sum1 <= sum2) {
            one.push_back(numbers[i]);
            sum1 += numbers[i].first;
        } else {
            two.push_back(numbers[i]);
            sum2 += numbers[i].first;
        }
    }
    if (sum1 != sum2) {
        fout << -1;
        return 0;
    }
    sort(one.begin(), one.end(), [](pair<int, int> a, pair<int, int> b) {
        return a.second < b.second;
    });
    fout << one.size() << '\n';
    for (auto i: one)
        fout << i.second + 1 << ' ';
    fout.close();
    return 0;
}
