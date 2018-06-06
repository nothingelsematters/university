#include <iostream>
#include <fstream>
#include <queue>
#include <vector>
#include <set>
using namespace std;

ifstream fin;
ofstream fout("equivalence.out");

void read(int const& k, int const& m, vector<bool>& terminals, vector<vector<int>>& transitions,
        vector<multiset<int>>& back) {
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
                back[temp2 - 1].insert(temp - 1);
        }
}

bool equivalent(vector<bool> const& first_terms, vector<bool> const& second_terms,
                vector<vector<int>> const& first, vector<vector<int>> const& second,
                vector<bool> const& reach1, vector<bool> const& reach2) {
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
                                if ((first[u][i] != -1 && reach1[first[u][i]]) || (second[v][i] != -1 && reach2[second[v][i]])) {
                                        return false;
                                }
                }
        }
        return true;
}

void reachable(vector<bool>& reach, vector<multiset<int>> const& back, int temp) {
        if (!reach[temp]) {
                reach[temp] = true;
                for (auto i : back[temp])
                        reachable(reach, back, i);
        }
}

int main() {
        int n, m, k;
        fin = ifstream("minimization.out");
        fin >> n >> m >> k;
        vector<bool> first_terms(n, false);
        vector<vector<int>> first(n, vector<int> (26, -1));
        vector<multiset<int>> back1(n);
        vector<bool> reach1(n, false);
        read(k, m, first_terms, first, back1);
        for (int i = 0; i < n; ++i)
                if (first_terms[i])
                        reachable(reach1, back1, i);
        fin = ifstream("minimization2.out");
        fin >> n >> m >> k;
        vector<bool> second_terms(n, false);
        vector<vector<int>> second(n, vector<int> (26, -1));
        vector<multiset<int>> back2(n);
        vector<bool> reach2(n, false);
        read(k, m, second_terms, second, back2);
        for (int i = 0; i < n; ++i)
                if (second_terms[i])
                        reachable(reach2, back2, i);
        cout << (equivalent(first_terms, second_terms, first, second, reach1, reach2) ? "YES" : "NO") << '\n';
        fin.close();
        fout.close();
        return 0;
}
