#include <fstream>
#include <vector>
#include <queue>
#include <map>
#include <set>
using namespace std;

struct vertex {
        int index;
        bool accept, reach, second_reach;
        map<char, int> transitions;
        map<char, set<int>> back;
        vertex() : accept(false), reach(false), second_reach(false), index(0) {}
};

vector<pair<bool, set<int>>> find_equivalent_classes(vertex* states, int n) {
        set<int> terms, other;
        for (int i = 0; i < n; ++i)
                if (states[i].reach && states[i].second_reach) {
                        (states[i].accept ? terms.insert(i) : other.insert(i));
                        states[i].index = (int) !states[i].accept;
                }
        vector<pair<bool, set<int>>> result;
        result.push_back({true, terms});
        if (!other.empty())
                result.push_back({false, other});
        queue<pair<set<int>, char>> q;
        for (char i = 'a'; i <= 'z'; ++i) {
                q.push({terms, i});
                if (!other.empty())
                        q.push({other, i});
        }
        map<int, set<int>> involved;
        while (!q.empty()) {
                pair<set<int>, char> pop = q.front();
                q.pop();
                involved.clear();
                for (auto i : pop.first) {
                        if (states[i].back.find(pop.second) != states[i].back.end())
                                for (auto r : states[i].back[pop.second])
                                                involved[states[r].index].insert(r);
                }
                for (auto i : involved) {
                        if (i.second.size() < result[i.first].second.size()) {
                                result.push_back({result[i.first].first, set<int>()});
                                int j = result.size() - 1;
                                for (auto r : i.second)
                                        if (result[i.first].second.find(r) != result[i.first].second.end()) {
                                                result[i.first].second.erase(r);
                                                result[j].second.insert(r);
                                                states[r].index = j;
                                        }
                                for (char k = 'a'; k <= 'z'; ++k)
                                        q.push({result[j].second, k});
                                if (result[j].second.size() > result[i.first].second.size()) {
                                        swap(result[i.first], result[j]);
                                        for (auto k : result[i.first].second)
                                                states[k].index = i.first;
                                        for (auto k : result[j].second)
                                                states[k].index = j;
                                }
                        }
                }
        }
        return result;
}

void print(ofstream fout, vertex* states, vector<pair<bool, set<int>>> new_dka) {
        int cur = 0;
        while (cur < new_dka.size()) {
                if (new_dka[cur].second.empty())
                        new_dka.erase(new_dka.begin() + cur);
                else
                        ++cur;
        }
        for (int i = 0; i < new_dka.size(); ++i) {
                for (auto j : new_dka[i].second)
                        states[j].index = i;
        }
        for (int i = 0; i < new_dka.size(); ++i)
                if (new_dka[i].second.find(0) != new_dka[i].second.end()) {
                        for (auto j : new_dka[0].second)
                                states[j].index = i;
                        for (auto j : new_dka[i].second)
                                states[j].index = 0;
                        swap(new_dka[i].first, new_dka[0].first);
                        break;
                }
        set<int> new_terms;
        for (int i = 0; i < new_dka.size(); ++i)
                if (new_dka[i].first)
                        new_terms.insert(i);
        map<pair<int, char>, int> new_transitions;
        for (auto i : new_dka)
                for (auto j : i.second)
                        for (auto k : states[j].transitions)
                                if (states[k.second].reach && states[k.second].second_reach)
                                        new_transitions[{states[j].index, k.first}] = states[k.second].index;
        fout << new_dka.size() << ' ' << new_transitions.size() << ' ' << new_terms.size() << '\n';
        for (auto i : new_terms)
                fout << i + 1 << ' ';
        for (auto i : new_transitions)
                fout << '\n' << i.first.first + 1 << ' ' << i.second + 1 << ' ' << i.first.second;
        fout.close();
}

void reachfrom(vertex* dka, int current) {
        if (!dka[current].reach) {
                dka[current].reach = true;
                for (auto j : dka[current].back)
                        for (auto i : j.second)
                                reachfrom(dka, i);
        }
}

void reachto(vertex* dka, int current) {
        if (!dka[current].second_reach) {
                dka[current].second_reach = true;
                for (auto i : dka[current].transitions)
                                reachto(dka, i.second);
        }
}

int main() {
        ifstream fin("minimization.in");
        int n, m, k;
        fin >> n >> m >> k;
        vertex dka[n];
        set<int> terms;
        for (int i = 0; i < k; ++i) {
                int temp;
                fin >> temp;
                dka[temp - 1].accept = true;
                terms.insert(temp - 1);
        }
        for (int i = 0; i < m; ++i) {
                int from, to;
                char c;
                fin >> from >> to >> c;
                dka[from - 1].transitions[c] = to - 1;
                dka[to - 1].back[c].insert(from - 1);
        }
        for (auto i : terms)
                reachfrom(dka, i);
        reachto(dka, 0);
        fin.close();
        print(ofstream("minimization.out"), dka, find_equivalent_classes(dka, n));
        return 0;
}
