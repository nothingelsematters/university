#include <fstream>
#include <set>
using namespace std;

struct state {
        char colour;
        bool reachable;
        long long ways;
        multiset<int> from, to;
        state() : colour(0), ways(0), reachable(false) {}
};

const int MODULE = 1e9 + 7;

ifstream fin("problem3.in");
ofstream fout("problem3.out");

state states[100000];

void mission_reachable(int temp) {
        if (!states[temp].reachable) {
                states[temp].reachable = true;
                for (auto i : states[temp].from)
                        mission_reachable(i);
        }
}

long long superdfs(int temp) {
        switch (states[temp].colour) {
                case 0 :
                        states[temp].colour = 1;
                        for (auto i : states[temp].to)
                                if (states[i].reachable)
                                        states[temp].ways = (states[temp].ways + superdfs(i)) % MODULE;
                        states[temp].colour = 2;
                        return states[temp].ways;
                case 1 :
                        fout << -1;
                        exit(0);
                case 2 :
                        return states[temp].ways;
        }
}

int main() {
        int n, m, k, l;
        char c;
        fin >> n >> m >> k;
        int terminals[k];
        for (int i = 0; i < k; ++i) {
                fin >> l;
                terminals[i] = l - 1;
                states[l - 1].ways = 1;
        }
        for (int i = 0; i < m; ++i) {
                fin >> n >> l >> c;
                states[n - 1].to.insert(l - 1);
                states[l - 1].from.insert(n - 1);
        }
        for (auto i : terminals)
                mission_reachable(i);
        fout << superdfs(0) % MODULE;
        fin.close();
        fout.close();
        return 0;
}
