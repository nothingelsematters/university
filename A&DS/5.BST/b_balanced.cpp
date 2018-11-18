#include <iostream>
using namespace std;

struct tree {
        int data, h;
        tree *right, *left, *parent;
        tree(int y) : data(y), right(nullptr), left(nullptr), parent(nullptr), h(1) {
        }
};

tree* root = nullptr;

inline void heighter(tree* p) {
        p->h = max((p->left ? p->left->h : 0), (p->right ? p->right->h : 0)) + 1;
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

inline pair<bool, tree*> find(int key) {
        tree* cur = root, *dad = nullptr;
        while (cur && cur->data != key) {
                dad = cur;
                cur = (cur->data > key ? cur->left : cur->right);
        }
        return (cur ? pair<bool, tree*> (true, cur) : pair<bool, tree*> (false, dad));
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

tree* deleter(tree* cur, int key){
        if (!cur)
                return nullptr;
        if (cur->data > key) {
                cur->left = deleter(cur->left, key);
                if (cur->left) cur->left->parent = cur;
        } else if (cur->data < key) {
                cur->right = deleter(cur->right, key);
                if (cur->right) cur->right->parent = cur;
        } else {
                tree* temp = cur;
                if (cur->right) {
                        temp = cur->right;
                        while (temp->left)
                                temp = temp->left;
                        cur->data = temp->data;
                        cur->right = deleter(cur->right, temp->data);
                        if (cur->right) cur->right->parent = cur;
                } else if (cur->left) {
                        temp = cur->left;
                        while (temp->right)
                                temp = temp->right;
                        cur->data = temp->data;
                        cur->left = deleter(cur->left, temp->data);
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

inline void next(int key) {
        if (root) {
                tree* temp = find(key).second;
                if (temp->data > key)
                        cout << temp->data << '\n';
                else if (temp->right) {
                        temp = temp->right;
                        while (temp->left)
                                temp = temp->left;
                        cout << temp->data << '\n';
                } else {
                        tree *t = temp->parent;
                        while (t && (temp == t->right)) {
                                temp = t;
                                t = t->parent;
                        }
                        if (!t)
                                cout << "none\n";
                        else
                                cout << t->data << '\n';
                }
        } else cout << "none\n";
}

inline void prev(int key) {
        if (root) {
                tree* temp = find(key).second;
                if (temp->data < key)
                        cout << temp->data << '\n';
                else if (temp->left) {
                        temp = temp->left;
                        while (temp->right)
                                temp = temp->right;
                        cout << temp->data << '\n';
                } else {
                        tree *t = temp->parent;
                        while (t && (temp == t->left)) {
                                temp = t;
                                t = t->parent;
                        }
                        if (!t)
                                cout << "none\n";
                        else
                                cout << t->data << '\n';
                }
        } else cout << "none\n";
}

int main() {
        string request;
        int num;
        while (cin>> request) {
                cin>> num;
                switch (request[0]) {
                case 'i':
                        root = insert(root, num);
                        if (root && root->parent)
                                root->parent = nullptr;
                        break;
                case 'd':
                        root = deleter(root, num);
                        if (root && root->parent)
                                root->parent = nullptr;
                        break;
                case 'e':
                        cout << (find(num).first ? "true" : "false") << '\n';
                        break;
                case 'n':
                        next(num);
                        break;
                case 'p':
                        prev(num);
                        break;
                case 'r':
                        cout << root->data << ' ' << root->right->left->right->data <<'\n';
                }
        }
}
