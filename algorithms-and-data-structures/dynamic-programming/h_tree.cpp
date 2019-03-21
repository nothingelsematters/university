#include <iostream>
#include <vector>
using namespace std;

class ans{
public:
    ans(){
        y=0;
        m=0;
        e=false;
    }
    int y,m;
    bool e;
};

vector<int> tree;
ans count (int l){
    vector<int> children;
    for (int i=0; i<tree.size(); i++) if (tree[i]==l+1) children.push_back(i);
    if (children.size()==0) {
        ans t;
        t.y=1;
        t.m=1;
        t.e=true;
        return t;
    }
    ans t;
    for (auto i: children) {
        ans u=count(i);
        t.m+=u.m;
        if (!u.e) t.y+=u.y;
    }
    ans out;
    out.y=t.m;
    if (out.y>1+t.y) out.m=out.y;
    else out.m=1+t.y;
    return out;
}

int main() {
    int n, root;
    cin >> n;
    tree.resize(n);
    for (int i=0; i<n; i++) {
        cin >> tree[i];
        if (tree[i]==0) root=i;
    }
    cout << count(root).m;
}