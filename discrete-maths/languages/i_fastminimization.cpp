#include <fstream>
#include <vector>
#include <queue>
#include <map>
#include <unordered_set>
using namespace std;

struct vertex {
        int index;
        bool accept, reach, reachable;
        int transitions[26];
        vector<int> back[26];
        vertex() {
                for (int i = 0; i < 26; ++i)
                        transitions[i] = -1;
        }
};

vector<pair<int, int>>* tr;
vector<int>* b;
vertex* dka;

int main() {
        ifstream fin("fastminimization.in");
        int n, m, k, from, to;
        char c;
        fin >> n >> m >> k;
        dka = new vertex[n];
        tr = new vector<pair<int, int>>[n];
        b = new vector<int>[n];
        queue<int> reach;
        for (int i = 0; i < k; ++i) {
                fin >> from;
                dka[from - 1].accept = true;
                reach.push(from - 1);
        }
        for (int i = 0; i < m; ++i) {
                fin >> from >> to >> c;
                tr[from - 1].push_back({c - 97, to - 1});
                b[to - 1].push_back(from - 1);
        }
        while (!reach.empty()) {
                int pop = reach.front();
                reach.pop();
                if (!dka[pop].reach) {
                        dka[pop].reach = true;
                        for (int i : b[pop])
                                reach.push(i);
                }
        }
        reach.push(0);
        while (!reach.empty()) {
                int pop = reach.front();
                reach.pop();
                if (dka[pop].reach && !dka[pop].reachable) {
                        dka[pop].reachable = true;
                        for (pair<int, int> i : tr[pop])
                                reach.push(i.second);
                }
        }
        unordered_set<int> terms, other;
        for (int i = 0; i < n; ++i)
                if (dka[i].reachable) {
                        (dka[i].accept ? terms.insert(i) : other.insert(i));
                        dka[i].index = !dka[i].accept;
                        for (pair<int, int> j : tr[i])
                                if (dka[j.second].reachable) {
                                        dka[i].transitions[j.first] = j.second;
                                        dka[j.second].back[j.first].push_back(i);
                                }
                }
        vector<unordered_set<int>> new_dka({terms});
        if (!other.empty())
                new_dka.push_back(other);
        queue<pair<int, int>> q;
        for (int i = 0; i < 26; ++i) {
                q.push({0, i});
                if (!other.empty())
                        q.push({1, i});
        }
        while (!q.empty()) {
                pair<int, int> pop = q.front();
                q.pop();
                map<int, vector<int>> involved;
                for (int i : new_dka[pop.first])
                        for (int r : dka[i].back[pop.second])
                                involved[dka[r].index].push_back(r);
                for (pair<int, vector<int>> i : involved)
                        if (i.second.size() < new_dka[i.first].size()) {
                                new_dka.push_back(unordered_set<int>());
                                int j = new_dka.size() - 1;
                                for (int r : i.second) {
                                        new_dka[i.first].erase(r);
                                        new_dka[j].insert(r);
                                        dka[r].index = j;
                                }
                                for (int k = 0; k < 26; ++k)
                                        q.push({j, k});
                        }
        }
        vector<int> new_terms;
        map<pair<int, int>, int> new_trans;
        for (int j : new_dka[0])
                dka[j].index = dka[0].index;
        for (int j : new_dka[dka[0].index])
                dka[j].index = 0;
        for (int i = 0; i < new_dka.size(); ++i)
                for (int j : new_dka[i]) {
                        if (dka[j].accept)
                                new_terms.push_back(i);
                        for (int k = 0; k < 26; ++k)
                                if (dka[j].transitions[k] != -1)
                                        new_trans[{dka[j].index, k}] = dka[dka[j].transitions[k]].index;
                        break;
                }
        ofstream fout("fastminimization.out");
        fout << new_dka.size() << ' ' << new_trans.size() << ' ' << new_terms.size() << '\n';
        for (int i : new_terms)
                fout << i + 1 << ' ';
        for (pair<pair<int, int>, int> i : new_trans)
                fout << '\n' << i.first.first + 1 << ' ' << i.second + 1 << ' ' << char(i.first.second + 97);
}
