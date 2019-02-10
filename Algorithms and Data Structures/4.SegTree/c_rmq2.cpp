#include <fstream>

const unsigned long long limit = 1000000000000000000;
const long long antilimit = -1000000000000000000;
struct MST {
        int l, r;
        long long value, push_new, push_add;
        bool is_push;
        MST() {
                value = limit;
                push_new = antilimit;
                push_add = 0;
        }
        MST operator = (MST x) {
                is_push = true;
                if (x.push_new == antilimit) {
                      if (push_new == antilimit) push_add += x.push_add;
                      else push_new += x.push_add;
                } else {
                      push_add = 0;
                      push_new = x.push_new;
                }
        }
};
MST tree[10000000];
long long source[1000000];

void up(unsigned int x){
        while (x != 0) {
                x = (x - 1) / 2;
                tree[x].value = std::min(tree[x * 2 + 1].value, tree[x * 2 + 2].value);
        }
}

void push(unsigned int x){
        if (tree[x].is_push) {
                if (tree[x].push_new != antilimit) tree[x].value = tree[x].push_new;
                if (tree[x].push_add != 0) tree[x].value += tree[x].push_add;
                tree[2 * x + 1] = tree[x];
                tree[2 * x + 2] = tree[x];
                tree[x].push_add = 0;
                tree[x].push_new = antilimit;
                tree[x].is_push = false;
        }
}

void edit(unsigned int l, unsigned int r, int new_x, unsigned int x, bool flag_set) {
        push(x);
        push(2 * x + 1);
        push(2 * x + 2);
        if ((l > tree[x].r) || (r < tree[x].l)) return;
        if ((tree[x].l >= l) && ( (tree[x].r <= r))) {
                tree[x].is_push = true;
                (flag_set ? tree[x].push_new = new_x : tree[x].push_add = new_x);
                push(x);
                up(x);
                return;
        }
        edit(l, r, new_x, 2 * x + 1, flag_set);
        edit(l, r, new_x, 2 * x + 2, flag_set);
}

long long min_search(unsigned int l, unsigned int r, unsigned int x) {
        push(x);
        if ((l > tree[x].r) || (r < tree[x].l)) return limit;
        if ((tree[x].l >= l) && ( (tree[x].r <= r))) return tree[x].value;
        return std::min(min_search(l, r, 2 * x + 1), min_search(l, r, 2 * x + 2));
}

long long build (unsigned int x, unsigned int l, unsigned int r) {
        tree[x].r = r;
        tree[x].l = l;
        if (l == r) return (tree[x].value = source[l]);
        return (tree[x].value = std::min(build(2 * x + 1, l, (l + r) / 2), build(2 * x + 2, (l + r) / 2 + 1, r)));
}

int main() {
        std::ifstream fin("rmq2.in");
        std::ofstream fout("rmq2.out");
        unsigned int quantity;
        fin >> quantity;
        for (unsigned int i = 0; i < quantity; ++i) fin >> source[i];
        build(0, 0, quantity - 1);
        std::string request;
        while (fin >> request) {
                if (request == "min") {
                        unsigned int l, r;
                        fin >> l >> r;
                        fout << min_search(l - 1, r - 1, 0) << "\n";
                } else {
                        unsigned int l, r;
                        long long new_x;
                        fin >> l >> r >> new_x;
                        edit(l - 1, r - 1, new_x, 0, (request == "set" ? true : false));
                }
        }
}
