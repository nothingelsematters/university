#include <fstream>
#include <queue>
#include <vector>
using namespace std;

ifstream fin("isomorphism.in");
ofstream fout("isomorphism.out");

void read(int k, int m, vector<bool>& terminals, vector<vector<int>>& transitions) {
        for (int i = 0; i < k; ++i) {
                int temp;
                fin >> temp;
                terminals[temp - 1] = true;
        }
        for (int i = 0; i < m; ++i) {
                int temp, temp2;
                char c;
                fin >> temp >> temp2 >> c;
                transitions[temp - 1][c - 'a'] = temp2 - 1;
        }
}

bool equivalent(vector<bool> const& first_terms, vector<bool> const& second_terms,
                vector<vector<int>> const& first, vector<vector<int>> const& second) {
        vector<bool> used1(first.size(), false), used2(second.size(), false);
        queue<pair<int, int>> q({{0, 0}});
        while (!q.empty()) {
                int u = q.front().first, v = q.front().second;
                q.pop();
                if (first_terms[u] != second_terms[v])
                        return false;
                used1[u] = true;
                used2[v] = true;
                for (int i = 0; i < 26; ++i) {
                        if (first[u][i] != -1 && second[v][i] != -1) {
                                if (!used1[first[u][i]] || !used2[second[v][i]])
                                        q.push({first[u][i], second[v][i]});
                        } else if ((first[u][i] == -1) != (second[v][i] == -1))
                                return false;
                }
        }
        return true;
}

int main() {
        int n, m, k;
        fin >> n >> m >> k;
        vector<bool> first_terms(n, false);
        vector<vector<int>> first(n, vector<int> (26, -1));
        read(k, m, first_terms, first);
        fin >> n >> m >> k;
        vector<bool> second_terms(n, false);
        vector<vector<int>> second(n, vector<int> (26, -1));
        read(k, m, second_terms, second);
        fout << (equivalent(first_terms, second_terms, first, second) ? "YES" : "NO");
        fin.close();
        fout.close();
        return 0;
}
