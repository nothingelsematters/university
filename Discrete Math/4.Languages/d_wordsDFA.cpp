#include <fstream>
#include <vector>
#include <queue>
#include <set>
using namespace std;

const int MODULE = 1e9 + 7;
int length;
vector<vector<int>> arr;
vector<vector<bool>> was;
multiset<int> states[100];

ifstream fin("problem4.in");
ofstream fout("problem4.out");

int main() {
        int n, m, k, l;
        char c;
        fin >> n >> m >> k >> length;
        arr.resize(n, vector<int> (length + 1, 0));
        was.resize(n, vector<bool> (length + 1, false));
        queue<pair<int, int>> q;
        for (int i = 0; i < k; ++i) {
                fin >> l;
                q.push({l - 1, 0});
                arr[l - 1][0] = 1;
        }
        for (int i = 0; i < m; ++i) {
                fin >> n >> l >> c;
                states[l - 1].insert(n - 1);
        }
        while (!q.empty()) {
                pair<int, int> temp = q.front();
                q.pop();
                if (!was[temp.first][temp.second] && temp.second != length) {
                        was[temp.first][temp.second] = true;
                        for (auto i : states[temp.first]) {
                                arr[i][temp.second + 1] = (arr[i][temp.second + 1] + arr[temp.first][temp.second]) % MODULE;
                                q.push({i, temp.second + 1});
                        }
                }
        }
        fout << arr[0][length];
        fin.close();
        fout.close();
        return 0;
}
