#include <fstream>
#include <map>
using namespace std;

struct knot {
        bool term;
        map<char, int> trans;
        knot() : term(false) {}
};

int main() {
        ifstream fin("problem1.in");
        ofstream fout("problem1.out");
        string word;
        int n, m, k, cur, cur1;
        char c;
        fin >> word >> n >> m >> k;
        knot arr[n + 1];
        for (int i = 0; i < k; ++i) {
                fin >> cur;
                arr[cur].term = true;
        }
        for (int i = 0; i < m; ++i) {
                fin >> cur >> cur1 >> c;
                arr[cur].trans[c] = cur1;
        }
        cur = 1;
        for (int i = 0; i < word.length(); ++i) {
                if (arr[cur].trans.count(word[i]) == 0) {
                        fout << "Rejects";
                        return 0;
                }
                cur = arr[cur].trans[word[i]];
        }
        fout << (arr[cur].term ? "Accepts" : "Rejects");
}
