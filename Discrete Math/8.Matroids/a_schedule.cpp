#include <fstream>
#include <iostream>
#include <algorithm>
#include <vector>
#include <set>

int main() {
    std::ifstream fin("schedule.in");
    size_t quantity;
    fin >> quantity;
    std::vector<std::pair<long long, long long>> task;
    for (size_t i = 0; i < quantity; ++i) {
        long long a, b;
        fin >> a >> b;
        task.emplace_back(a, b);
    }
    fin.close();

    std::sort(task.begin(), task.end());
    long long penalty = 0;
    long long time = 0;
    std::multiset<long long> punish;
    for (long long i = 0; i < quantity; ++i) {
        // std::cout << last << ' ' << task[i].first << '\n';
        if (time >= task[i].first) {
            if (!punish.empty() && *(punish.begin()) < task[i].second) {
                penalty += *(punish.begin());
                punish.erase(punish.begin());
                punish.insert(task[i].second);
            } else {
                penalty += task[i].second;
            }
        } else {
            // std::cout << task[i].first << ' ';
            punish.insert(task[i].second);
            ++time;
        }
    }
    std::ofstream fout("schedule.out");
    fout << penalty;
}
