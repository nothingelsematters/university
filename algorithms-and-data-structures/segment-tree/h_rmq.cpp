#include <fstream>
#include <limits.h>
using namespace std;

ifstream fin("rmq.in");
ofstream fout("rmq.out");
struct oak {
        int l;
        int r;
        bool set = false;
        long long v = LONG_MAX;
        long long push = 0;
        bool is_push = false;
        oak operator = (oak t) {
                push = (is_push ? max(t.push, push) : t.push);
                is_push = true;
        }
};
oak elm[1000000];
int index[1000000];
struct request {
        int l, r;
        long long q;
};

void up(int x){
        while (x != 0) {
                x = (x - 1) / 2;
                elm[x].v = min(elm[2 * x + 1].v, elm[2 * x + 2].v);
        }
}

void push(int x){
        if (elm[x].is_push) {
                elm[x].v = (elm[x].set ? max(elm[x].v, elm[x].push) : elm[x].push);
                elm[x].set = true;
                if (elm[x].l != elm[x].r){
                  elm[2 * x + 1] = elm[x];
                  elm[2 * x + 2]  = elm[x];
                }
                elm[x].is_push = false;
        }
}

void ultimatep_push(int x){
        push(x);
        if (elm[x].l != elm[x].r) {
                ultimatep_push(2 * x + 1);
                ultimatep_push(2 * x + 2);
        }
}

void set(int x, int l, int r, int newx) {
        push(x);
        push(2 * x + 1);
        push(2 * x + 2);
        if ((l > elm[x].r) || (r < elm[x].l)) return;
        if ((elm[x].l >= l) && ( (elm[x].r <= r))) {
                elm[x].is_push = true;
                elm[x].push = newx;
                push(x);
                push(2 * x + 1);
                push(2 * x + 2);
                up(x);
                return;
        }
        set(2 * x + 1, l, r, newx);
        set(2 * x + 2, l, r, newx);
}

void build (int x, int l, int r) {
        elm[x].l = l;
        elm[x].r = r;
        if (l == r) {
          index[l] = x;
          return;
        }
        build(2 * x + 1, l, (l + r) / 2);
        build(2 * x + 2, (l + r) / 2 + 1, r);
}

long long min_search(int l, int r, int x){
        if ((l > elm[x].r) || (r < elm[x].l)) return LONG_MAX;
        if ((elm[x].l >= l) && ( (elm[x].r <= r))) return elm[x].v;
        return min(min_search(l, r, 2 * x + 1), min_search(l, r, 2 * x + 2));
}

int main() {
        unsigned int n, m;
        fin >> n >> m;
        build(0, 0, n - 1);
        request req[m];
        for (unsigned int i = 0; i < m; ++i) {
                unsigned int l, r;
                long long q;
                fin >> l >> r >> q;
                req[i].l = --l;
                req[i].r = --r;
                req[i].q = q;
                set(0, l, r, q);
        }
        ultimatep_push(0);
        for (unsigned int i = 0; i < m; ++i) {
                if (min_search(req[i].l, req[i].r, 0) != req[i].q) {
                        fout << "inconsistent";
                        return 0;
                }
        }
        fout << "consistent\n";
        for (unsigned int i = 0; i < n; ++i)
                fout << elm[index[i]].v << ' ';
}
