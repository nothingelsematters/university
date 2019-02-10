#include <iostream>
#include <vector>
using namespace std;

int limit = -1000000010;
struct phantom_tree {
        int sum, height, left_son, right_son, daddy, push;
        phantom_tree (int d) {
                sum = 0;
                push = limit;
                height = 0;
                left_son = -1;
                right_son = -1;
                daddy = d;
        }
};
vector<phantom_tree> tree;

void push(int x, int ltb, int rtb) {
        if ((tree[x].left_son == -1) && (ltb != rtb)) {
                tree.push_back(phantom_tree(x));
                tree[x].left_son = tree.size() - 1;
                tree.push_back(phantom_tree(x));
                tree[x].right_son = tree.size() - 1;
        }
        if (tree[x].push != limit) {
                tree[x].height = tree[x].push * (rtb - ltb + 1);
                tree[x].sum = tree[x].push * (rtb - ltb + 1);
                if (ltb != rtb) {
                        tree[tree[x].left_son].push = tree[x].push;
                        tree[tree[x].right_son].push = tree[x].push;
                }
                tree[x].push = limit;
        }
}

void update(int x, int ltb, int rtb, int l, int r, int h) {
        push(x, ltb, rtb);
        if (ltb != rtb) {
                push(tree[x].left_son, ltb, (ltb + rtb) >> 1);
                push(tree[x].right_son, ((ltb + rtb) >> 1) + 1, rtb);
        }
        if (ltb > r || rtb < l) return;
        if (ltb >= l && rtb <= r) {
                tree[x].push = h;
                push(x, ltb, rtb);
                while (x != 0) {
                        x = tree[x].daddy;
                        tree[x].height = max(tree[tree[x].left_son].height,
                                             tree[tree[x].left_son].sum + tree[tree[x].right_son].height);
                        tree[x].sum = tree[tree[x].left_son].sum + tree[tree[x].right_son].sum;
                }
                return;
        }
        update(tree[x].left_son, ltb, (ltb + rtb) >> 1, l, r, h);
        update(tree[x].right_son, ((ltb + rtb) >> 1) + 1, rtb, l, r, h);
}

int search(int x, int l, int r, int h) {
        push(x, l, r);
        if (l != r) {
                push(tree[x].left_son, l, (l + r) >> 1);
                push(tree[x].right_son, ((l + r) >> 1) + 1, r);
        } else return (tree[x].height > h ? l : l + 1);
        if (tree[tree[x].left_son].height > h) return search(tree[x].left_son, l, (l + r) >> 1, h);
        return search(tree[x].right_son, ((l + r) >> 1) + 1, r, h - tree[tree[x].left_son].sum);
}

int main(){
        ios_base::sync_with_stdio(false);
        tree.push_back(phantom_tree(-1));
        int n;
        char c;
        cin >> n;
        while (cin >> c) {
                if (c == 'E') return 0;
                if (c == 'Q') {
                        int h;
                        cin >> h;
                        cout << search(0, 0, n - 1, h) << '\n';
                } else if (c == 'I') {
                        int a, b, h;
                        cin >> a >> b >> h;
                        update(0, 0, n - 1, a - 1, b - 1, h);
                }
        }
}
