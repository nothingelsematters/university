#include <fstream>
#include <vector>
#include <queue>
#include <map>
#include <set>
using namespace std;

struct vertex {
        bool accept;
        vertex() : accept(false) {}
        vertex(bool acc) : accept(acc) {}
};

struct vertexDFA : vertex {
        multiset<int> transitions;
        vertexDFA() : vertex() {}
        vertexDFA(bool acc) : vertex(acc) {}
};

struct vertexNFA : vertex {
        map<int, set<int>> transitions;
        vertexNFA() : vertex() {}
};

inline int wordsQuantity(vector<vertexDFA> const& dfa, int length) {
        const int MODULE = 1e9 + 7;
        vector<vector<int>> quantity(dfa.size(), vector<int> (length + 1, 0));
        vector<vector<bool>> visited(dfa.size(), vector<bool> (length + 1, false));
        queue<pair<int, int>> q;
        for (int i = 0; i < dfa.size(); ++i)
                if (dfa[i].accept) {
                        q.push({i, 0});
                        quantity[i][0] = 1;
                }
        while (!q.empty()) {
                pair<int, int> temp = q.front();
                q.pop();
                if (!visited[temp.first][temp.second] && temp.second != length) {
                        visited[temp.first][temp.second] = true;
                        for (auto i : dfa[temp.first].transitions) {
                                quantity[i][temp.second + 1] = (quantity[i][temp.second + 1] + quantity[temp.first][temp.second]) % MODULE;
                                q.push({i, temp.second + 1});
                        }
                }
        }
        return quantity[0][length];
}

inline vector<vertexDFA> getDFAbyNFA(vector<vertexNFA>& nfa) {
        queue<pair<int, set<int>>> q;
        q.push({0, set<int>({0})});
        vector<pair<bool, set<int>>> DFAstates({{nfa[0].accept, set<int>({0})}});
        vector<vertexDFA> dfa({vertexDFA(nfa[0].accept)});
        while (!q.empty()) {
                set<int> pop = q.front().second;
                int num = q.front().first;
                q.pop();
                for (int i = 0; i < 26; ++i) {
                        set<int> newState;
                        bool term = false;
                        vector<bool> came(2000, false);
                        for (auto j : pop)
                                for (int k : nfa[j].transitions[i]) {
                                        if (nfa[k].accept)
                                                term = true;
                                        if (!came[k]) {
                                                newState.insert(k);
                                                came[k] = true;
                                        }
                                }
                        if (!newState.empty()) {
                                bool found = false;
                                int index = 0;
                                for (int j = 0; j < DFAstates.size(); ++j)
                                        if (DFAstates[j].second == newState) {
                                                index = j;
                                                found = true;
                                                break;
                                        }
                                if (!found) {
                                        DFAstates.push_back({term, newState});
                                        q.push({DFAstates.size() - 1, newState});
                                        dfa.push_back(vertexDFA(term));
                                        dfa[DFAstates.size() - 1].transitions.insert(num);
                                } else
                                        dfa[index].transitions.insert(num);
                        }
                }
        }
        return dfa;
}

int main() {
        ifstream fin("problem5.in");
        ofstream fout("problem5.out");
        int n, m, k, length;
        char c;
        fin >> n >> m >> k >> length;
        vector<vertexNFA> nfa(n);
        for (int i = 0; i < k; ++i) {
                int temp;
                fin >> temp;
                nfa[temp - 1].accept = true;
        }
        for (int i = 0; i < m; ++i) {
                int from, to;
                char c;
                fin >> from >> to >> c;
                nfa[from - 1].transitions[c - 'a'].insert(to - 1);
        }
        fout << wordsQuantity(getDFAbyNFA(nfa), length);
        fin.close();
        fout.close();
        return 0;
}
