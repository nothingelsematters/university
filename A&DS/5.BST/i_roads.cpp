#include <fstream>
#include <vector>
using namespace std;

ifstream fin("roads.in");
ofstream fout("roads.out");

struct tree {
        bool cycle;
        int count, y, left, right, parent;
        tree() : cycle(false), count(1), y(rand()), left(-1), right(-1), parent(-1) {}
};

vector<tree> forest;

int heighter(int t) {
        if (t != -1)
                forest[t].count = (forest[t].left != -1 ? forest[forest[t].left].count : 0) +
                        (forest[t].right != -1 ? forest[forest[t].right].count : 0) + 1;
}

pair<int, int> split(int t, int key) {
        if (t == -1)
                return {-1, -1};
        int u = (forest[t].left != -1 ? forest[forest[t].left].count + 1 : 1);
        if (key > u) {
                pair<int, int> res = split(forest[t].right, key - u);
                forest[t].right = res.first;
                if (forest[t].right != -1)
                        forest[forest[t].right].parent = t;
                if (res.second != -1)
                        forest[res.second].parent = -1;
                heighter(forest[t].right);
                heighter(t);
                heighter(res.second);
                return {t, res.second};
        }
        pair<int, int> res = split(forest[t].left, key);
        forest[t].left = res.second;
        if (forest[t].left != -1)
                forest[forest[t].left].parent = t;
        if (res.first != -1)
                forest[res.first].parent = -1;
        heighter(forest[t].left);
        heighter(t);
        heighter(res.first);
        return {res.first, t};
}

int merge(int t1, int t2) {
        if (t2 == -1) return t1;
        if (t1 == -1) return t2;
        if (forest[t1].y > forest[t2].y) {
                forest[t1].right = merge(forest[t1].right, t2);
                if (forest[t1].right != -1)
                        forest[forest[t1].right].parent = t1;
                forest[t1].parent = -1;
                heighter(forest[t1].right);
                heighter(t1);
                return t1;
        }
        forest[t2].left = merge(t1, forest[t2].left);
        if (forest[t2].left != -1)
                forest[forest[t2].left].parent = t2;
        forest[t2].parent = -1;
        heighter(forest[t2].left);
        heighter(t2);
        return t2;
}

inline pair<int, int> fly(int index) {
        int count = (forest[index].left != -1 ? forest[forest[index].left].count : 0);
        while (forest[index].parent != -1) {
                if (forest[forest[index].parent].right == index)
                        count += (forest[forest[index].parent].left != -1 ?
                                forest[forest[forest[index].parent].left].count + 1 : 1);
                index = forest[index].parent;
        }
        return {index, count};
}

void swap(int t) {
        if (t == -1)
                return;
        swap(forest[t].left, forest[t].right);
        swap(forest[t].left);
        swap(forest[t].right);
}

inline void add(int f, int s) {
        pair<int, int> one = fly(f), two = fly(s);
        if (one.first == two.first) {
                forest[one.first].cycle = true;
                return;
        }
        if (!one.second && two.second)
                swap(one.first, two.first);
        else if (one.second && two.second)
                swap(two.first);
        else if (!one.second && !two.second)
                swap(one.first);
        merge(one.first, two.first);
}

inline void remove(int f, int s) {
        pair<int, int> one = fly(f), two = fly(s);
        if (forest[one.first].cycle) {
                forest[one.first].cycle = false;
                if (abs(one.second - two.second) == 1) {
                        pair<int, int> temp = split(one.first, min(one.second, two.second) + 2);
                        merge(temp.second, temp.first);
                }
        } else
                split(one.first, min(one.second, two.second) + 2);
}

inline int query(int f, int s) {
        pair<int, int> one = fly(f), two = fly(s);
        if (one.first != two.first)
                return -1;
        if (f == s)
                return 0;
        return (forest[one.first].cycle ? min(abs(one.second - two.second),
                forest[one.first].count - abs(one.second - two.second)):
                abs(one.second - two.second)) - 1;
}

int main() {
        int n, m, q, k, l;
        char c;
        fin >> n >> m >> q;
        forest.resize(n);
        for (int i = 0; i < m; ++i) {
                fin >> k >> l;
                add(k - 1, l - 1);
        }
        for (int i = 0; i < q; ++i) {
                fin >> c >> k >> l;
                switch (c) {
                        case '+':
                                add(k - 1, l - 1);
                                break;
                        case '-':
                                remove(k - 1, l - 1);
                                break;
                        case '?':
                                fout << query(k - 1, l - 1) << '\n';
                }
        }
}
