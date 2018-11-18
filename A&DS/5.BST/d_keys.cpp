#include <iostream>
using namespace std;

struct tree {
        int count, value, zero, y;
        tree *left, *right;
        tree(int a) : count(1), value(a), y(rand()), zero(a == 0 ? 1 : 0), left(nullptr), right(nullptr) {
        }
};

tree *root;

int heighter(tree* t) {
        if (t) {
                t->count = (t->left ? t->left->count : 0) + (t->right ? t->right->count : 0) + 1;
                t->zero = (t->left ? t->left->zero : 0) + (t->right ? t->right->zero : 0) + (t->value == 0 ? 1 : 0);
        }
}

pair<tree*, tree*> split(tree* t, int key) {
        if (!t)
                return {nullptr, nullptr};
        int u = (t->left ? t->left->count + 1 : 1);
        if (key > u) {
                pair<tree*, tree*> res = split(t->right, key - u);
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

tree* remove0(tree* t) {
        if (t->left && t->left->zero)
                t->left = remove0(t->left);
        else if (t->value == 0)
                return merge(t->left, t->right);
        else
                t->right = remove0(t->right);
        heighter(t);
        return t;
}

void print (tree* t) {
        if (!t) return;
        print(t->left);
        cout << t->value << ' ';
        print(t->right);
}
int res;
bool flag;
void count0 (tree* t) {
        if (!t || !flag) return;
        count0(t->right);
        if (t->value != 0) {
                flag = false;
                return;
        }
        if (flag) ++res;
        count0(t->left);
}

int main() {
        std::ios_base::sync_with_stdio(false);
        cin.tie(0);
        cout.tie(0);
        int n, m, k;
        cin >> n >> m;
        for (int i = 0; i < m; ++i)
                root = merge(root, new tree(0));
        for (int i = 0; i < n; ++i) {
                cin >> k;
                pair<tree*, tree*> temp = split(root, k);
                root = merge(merge(temp.first, new tree(i + 1)), (temp.second && temp.second->zero ? remove0(temp.second) : temp.second));
        }
        tree* cur = root;
        res = 0; flag = true;
        count0(root);
        root = split(root, root->count - res + 1).first;
        cout << root->count << '\n';
        print(root);
}
