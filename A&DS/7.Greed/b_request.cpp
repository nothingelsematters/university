#include <fstream>
#include <algorithm>

int main() {
    std::ifstream fin("request.in");
    int quantity;
    fin >> quantity;
    std::pair<int, int> requests[quantity];
    for (int i = 0; i < quantity; ++i)
        fin >> requests[i].second >> requests[i].first;
    fin.close();
    std::sort(requests, requests + quantity);
    int answer = 0, current = 0;
    for (int i = 0; i < quantity; ++i)
        if (requests[i].second >= current) {
            ++answer;
            current = requests[i].first;
        }
    std::ofstream fout("request.out");
    fout << answer;
    fout.close();
    return 0;
}
