#include <fstream>
#include <cstdlib>
using namespace std;

ifstream fin("log.in");
ofstream fout("log.out");

struct tree {
        int subcount, count, letter, y;
        uint32_t mask;
        tree *left, *right;
        tree(int l, int a) : letter(l), mask(1 << l), y(rand()), count(a), subcount(a), left(nullptr), right(nullptr) {
        }
};

tree *root;

int heighter(tree* t) {
        if (t) {
                t->subcount = (t->left ? t->left->subcount : 0) + (t->right ? t->right->subcount : 0) + t->count;
                t->mask = (t->left ? t->left->mask : 0) | (t->right ? t->right->mask : 0) | (1 << t->letter);
        }
}

tree* merge(tree* t1, tree* t2) {
        if (!t2) return t1;
        if (!t1) return t2;
        if (t1->y > t2->y) {
                t1->right = merge(t1->right, t2);
                heighter(t1->right);
                heighter(t1);
                return t1;
        }
        t2->left = merge(t1, t2->left);
        heighter(t2->left);
        heighter(t2);
        return t2;
}

pair<tree*, tree*> split(tree* t, int key) {
        if (!t)
                return {nullptr, nullptr};
        int u = (t->left ? t->left->subcount + 1 : 1);
        if (u < key && key <= u + t->count) {
                int z = key - u;
                pair<tree*, tree*> res = {new tree(t->letter, z), new tree(t->letter, t->count - z)};
                (res.first->count ? res.first->left = t->left : res.first = t->left);
                (res.second->count ? res.second->right = t->right : res.second = t->right);
                heighter(res.first);
                heighter(res.second);
                return res;
        }
        if (key > u + t->count) {
                pair<tree*, tree*> res = split(t->right, key - u + 1 - t->count);
                t->right = res.first;
                heighter(t->right);
                heighter(t);
                heighter(res.second);
                return {t, res.second};
        }
        pair<tree*, tree*> res = split(t->left, key);
        t->left = res.second;
        heighter(t->left);
        heighter(t);
        heighter(res.first);
        return {res.first, t};
}

tree* insert(tree* t, char c, int l, int r) {
        if (!t) return new tree(c, r - l + 1);
        pair<tree*, tree*> temp = split(t, l);
        return merge(merge(temp.first, new tree(c, r - l + 1)), temp.second);
}

tree* remove(tree *t, int l, int r) {
        if (!t || r <= 0 || l > t->subcount) return t;
        if (l <= 1 && r >= t->subcount) return nullptr;
        int u = (t->left ? t->left->subcount : 0) + t->count;
        t->left = remove(t->left, l, r);
        t->right = remove(t->right, l - u, r - u);
        heighter(t->left);
        heighter(t->right);
        if (l <= u && r > u - t->count) {
                if (u - t->count + 1 >= l && r >= u)
                        t = merge(t->left, t->right);
                else
                        t->count = max(l - u + t->count - 1, 0) + (u > r ? u - r : 0);
        }
        heighter(t);
        return t;
}

int query(tree* t, int l, int r) {
        if (!t || r <= 0 || l > t->subcount) return 0;
        if (l <= 1 && r >= t->subcount)
                return t->mask;
        int u = (t->left ? t->left->subcount : 0) + t->count;
        return (l <= u && r > u - t->count ? (1 << t->letter) : 0) | query(t->left, l, r) | query(t->right, l - u, r - u);
}

void print (tree* t) {
        if (!t) return;
        print(t->left);
        fout << t->count << "'" << t->subcount << "'" << (char) (t->letter + 97) << ' ';
        print(t->right);
}

int main() {
        std::ios_base::sync_with_stdio(false);
        int n, m, k;
        char c, d;
        fin >> n;
        for (int i = 0; i < n; ++i) {
                fin >> c >> m >> k;
                switch (c) {
                case '+':
                        fin >> d;
                        root = insert(root, d - 97, m, m + k - 1);
                        break;
                case '-':
                        root = remove(root, m, m + k - 1);
                        break;
                case '?':
                        int temp = query(root, m, k);
                        int res = 0;
                        for (; temp > 0; temp >>= 1)
                                res += temp & 1;
                        fout << res << '\n';
                }
        }
}
