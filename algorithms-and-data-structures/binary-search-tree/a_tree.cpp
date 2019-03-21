#include <fstream>
#include <iostream>
#include <ctime>
using namespace std;

struct tree {
        int data;
        tree *right, *left, *parent;
        tree() : right(nullptr), left(nullptr), parent(nullptr) {
        }
};

ifstream fin("tree.in");
ofstream fout("tree2.out");

tree *root = nullptr;
tree *temp;

bool exists(tree* t, int key) {
        if (root == nullptr){
                return false;
        }
        temp = t;
        while (key != temp->data) {
                if ((temp->left == nullptr && key < temp->data) ||
                    (temp->right == nullptr && key > temp->data)) {
                            return false;
                    }
                ((key < temp->data) ? temp = temp->left : temp = temp->right);
        }
        return true;
}

void insert(int key) {
        if (root == nullptr) {
                root = new tree;
                root->data = key;
        } else if (!exists(root, key)) {
                tree *son = new tree;
                son->data = key;
                son->parent = temp;
                (temp->data > key ? temp->left = son : temp->right = son);
        }
}

void deleter(tree* t, int key){
        if (exists(t, key)) {
                if (temp->right != nullptr) {
                        tree* u = temp;
                        temp = temp->right;
                        while (temp->left != nullptr)
                                temp = temp->left;
                        u->data = temp->data;
                        deleter(temp, temp->data);
                } else if (temp->left != nullptr) {
                        tree* u = temp;
                        temp = temp->left;
                        while (temp->right != nullptr)
                                temp = temp->right;
                        u->data = temp->data;
                        deleter(temp, temp->data);
                } else {
                        if (temp->parent == nullptr)
                                root = nullptr;
                        else {
                                if (temp->parent->left == temp)
                                        temp->parent->left = nullptr;
                                else temp->parent->right = nullptr;
                                delete temp;
                        }
                }
        }
}

void next(int key) {
        if (root != nullptr) {
                exists(root, key);
                if (temp->data > key)
                        fout << temp->data << '\n';
                else if (temp->right != nullptr) {
                        temp = temp->right;
                        while (temp->left != nullptr)
                                temp = temp->left;
                        fout << temp->data << '\n';
                } else {
                        tree *t = temp->parent;
                        while ((t != nullptr) && (temp == t->right)) {
                                temp = t;
                                t = t->parent;
                        }
                        if (t == nullptr) fout << "none\n";
                        else fout << t->data << '\n';
                }
        } else fout << "none\n";
}

void prev(int key) {
        if (root != nullptr) {
                exists(root, key);
                if (temp->data < key)
                        fout << temp->data << '\n';
                else if (temp->left != nullptr) {
                        temp = temp->left;
                        while (temp->right != nullptr)
                                temp = temp->right;
                        fout << temp->data << '\n';
                } else {
                        tree *t = temp->parent;
                        while ((t != nullptr) && (temp == t->left)) {
                                temp = t;
                                t = t->parent;
                        }
                        if (t == nullptr) fout << "none\n";
                        else fout << t->data << '\n';
                }
        } else fout << "none\n";
}

int main() {
        int c = clock();
        string request;
        int num;
        while (fin >> request) {
                fin >> num;
                switch (request[0]) {
                case 'i':
                        insert(num);
                        break;
                case 'd':
                        temp = root;
                        deleter(root, num);
                        break;
                case 'e':
                        fout << (exists(root, num) ? "true" : "false") << '\n';
                        break;
                case 'n':
                        next(num);
                        break;
                case 'p':
                        prev(num);
                }
        }
        cout << ((double) (clock() - c)) / 1000000 << '\n';
}
