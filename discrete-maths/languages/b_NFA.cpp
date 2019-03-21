#include <fstream>
#include <map>
#include <set>
using namespace std;

ifstream fin("problem2.in");
ofstream fout("problem2.out");

struct state {
        bool term;
        map<char, set<int>> trans;
        state() : term(false) {}
};

int main() {
        ios_base::sync_with_stdio(false);
        string word;
        int n, m, k, first, second;
        char c;
        fin >> word >> n >> m >> k;
        state states[n + 1];
        for (int i = 0; i < k; ++i) {
                fin >> n;
                states[n].term = true;
        }
        for (int i = 0; i < m; ++i) {
                fin >> first >> second >> c;
                states[first].trans[c].insert(second);
        }
        set<int> prev, cur;
        prev.insert(1);
        for (int i = 0; i < word.length(); ++i) {
                cur.clear();
                for (auto j : prev)
                        cur.insert(states[j].trans[word[i]].begin(),
                                states[j].trans[word[i]].end());
                prev = cur;
        }
        for (auto i : cur)
                if (states[i].term) {
                        fout << "Accepts";
                        return 0;
                }
        fout << "Rejects";
}
