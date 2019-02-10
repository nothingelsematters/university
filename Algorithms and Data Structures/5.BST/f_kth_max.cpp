#include <iostream>
using namespace std;

struct tree {
        int data, count, h;
        tree *left, *right, *parent;
        tree(int key) : data(key), count(1), h(1), left(nullptr), parent(nullptr), right(nullptr) {
        }
};

tree* root = nullptr;

tree* search(int key) {
        tree* temp = root, *dad = nullptr;
        while (temp) {
                dad = temp;
                temp = (temp->data > key ? temp->left : temp->right);
        }
        return dad;
}

int kth(int key) {
        tree* temp = root;
        while (key != (temp->right ? temp->right->count + 1 : 1)) {
                if (temp->right && temp->right->count >= key)
                        temp = temp->right;
                else {
                        key -= (temp->right ? temp->right->count + 1 : 1);
                        temp = temp->left;
                }
        }
        return temp->data;
}

inline void heighter(tree* p) {
        p->h = max((p->left ? p->left->h : 0), (p->right ? p->right->h : 0)) + 1;
}
inline void counter(tree* p) {
        p->count = (p->left ? p->left->count : 0) + (p->right ? p->right->count : 0) + 1;
}

inline tree* rotate_right(tree* cur) {
        tree* temp = cur->left;
        cur->left = temp->right;
        if (temp->right) temp->right->parent = cur;
        temp->right = cur;
        cur->parent = temp;
        heighter(cur);
        counter(cur);
        heighter(temp);
        counter(temp);
        return temp;
}

inline tree* rotate_left(tree* cur) {
        tree* temp = cur->right;
        cur->right = temp->left;
        if (temp->left) temp->left->parent = cur;
        temp->left = cur;
        cur->parent = temp;
        heighter(cur);
        counter(cur);
        heighter(temp);
        counter(temp);
        return temp;
}

inline int balance(tree* p) {
        return (p->right ? p->right->h : 0) - (p->left ? p->left->h : 0);
}

tree* balancing(tree* p){
        heighter(p);
        counter(p);
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

tree* insert(tree* t, int key) {
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

tree* remove(tree* cur, int key){
        if (!cur)
                return nullptr;
        if (cur->data > key) {
                cur->left = remove(cur->left, key);
                if (cur->left) cur->left->parent = cur;
        } else if (cur->data < key) {
                cur->right = remove(cur->right, key);
                if (cur->right) cur->right->parent = cur;
        } else {
                tree* temp = cur;
                if (cur->right) {
                        temp = cur->right;
                        while (temp->left)
                                temp = temp->left;
                        cur->data = temp->data;
                        cur->right = remove(cur->right, temp->data);
                        if (cur->right) cur->right->parent = cur;
                } else if (cur->left) {
                        temp = cur->left;
                        while (temp->right)
                                temp = temp->right;
                        cur->data = temp->data;
                        cur->left = remove(cur->left, temp->data);
                        if (cur->left) cur->left->parent = cur;
                } else {
                        if (cur->parent) {
                                cur = cur->parent;
                                (cur->left == temp ? cur->left = nullptr : cur->right = nullptr);
                                delete temp;
                        }
                        return nullptr;
                }
        }
        return balancing(cur);
}

int main() {
        int n, m;
        string c;
        cin >> n;
        for (int i = 0; i < n; ++i) {
                cin >> c >> m;
                switch (c[0]) {
                        case '+':
                        case '1':
                                root = insert(root, m);
                                break;
                        case '-':
                                root = remove(root, m);
                                break;
                        case '0':
                                cout << kth(m) << '\n';
                }
        }
}
