#include <iostream>
using namespace std;

struct tree {
        int data, h, more, less;
        long long sum;
        tree *right, *left, *parent;
        tree(int y) : data(y), more(y), less(y), sum(y), right(nullptr), left(nullptr), parent(nullptr), h(1) {
        }
};

tree* root = nullptr;

inline void heighter(tree* p) {
        p->h = max((p->left ? p->left->h : 0), (p->right ? p->right->h : 0)) + 1;
        p->sum = (p->left ? p->left->sum : 0) + (p->right ? p->right->sum : 0) + p->data;
        p->less = (p->left ? p->left->less : p->data);
        p->more = (p->right ? p->right->more : p->data);
}

inline tree* rotate_right(tree* cur){
        tree* temp = cur->left;
        cur->left = temp->right;
        if (temp->right) temp->right->parent = cur;
        temp->right = cur;
        cur->parent = temp;
        heighter(cur);
        heighter(temp);
        return temp;
}

inline tree* rotate_left(tree* cur){
        tree* temp = cur->right;
        cur->right = temp->left;
        if (temp->left) temp->left->parent = cur;
        temp->left = cur;
        cur->parent = temp;
        heighter(cur);
        heighter(temp);
        return temp;
}

inline int balance(tree* p) {
        return (p->right ? p->right->h : 0) - (p->left ? p->left->h : 0);
}

tree* balancing(tree* p){
        heighter(p);
        if (balance(p) >= 2) {
                if (balance(p->right) < 0)
                        p->right = rotate_right(p->right);
                return rotate_left(p);
        }
        if (balance(p) <= -2) {
                if (balance(p->left) > 0)
                        p->left = rotate_left(p->left);
                return rotate_right(p);
        }
        return p;
}

inline tree* insert(tree* t, int key) {
        if (!t)
                return new tree(key);
        if (t->data > key) {
                t->left = insert(t->left, key);
                t->left->parent = t;
        } else if (t->data < key) {
                t->right = insert(t->right, key);
                t->right->parent = t;
        }
        return balancing(t);
}

inline long long sum(tree* x, int l, int r) {
        if (!x)
                return 0;
        if (r < x->less || x->more < l)
                return 0;
        if (l <= x->less && x->more <= r)
                return x->sum;
        return (l <= x->data && x->data <= r ? x->data : 0) + sum(x->left, l, r) + sum (x->right, l, r);
}

int main() {
        int n, m, k;
        long long y;
        char c;
        char p = ' ';
        cin >> n;
        for (int i = 0; i < n; ++i) {
                cin >> c >> m;
                if (c == '+') {
                        if (p == '?') root = insert(root, (y + m) % 1000000000);
                        else root = insert(root, m);
                        if (root && root->parent)
                                root->parent = nullptr;
                } else {
                        y = 0;
                        cin >> k;
                        y = sum(root, m, k);
                        cout << y << '\n';
                }
                p = c;
        }
}
