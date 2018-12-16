#include <iostream>
#include <map>
#include <vector>

struct Vertex {
    int length;
    int suf_link;
    int counted = -1;
    std::string res;
    std::map<char, int> trans;
};

struct SufTree {
    SufTree() {
        vertexes.push_back(Vertex());
        vertexes.back().length = 0;
        vertexes.back().suf_link = -1;
    }

    void add(char c) {
        int index = size();
        vertexes.push_back(Vertex());
        vertexes.back().length = vertexes[last].length + 1;
        int parent;
        for (parent = last; parent != -1 && vertexes[parent].trans.find(c) ==
                    vertexes[parent].trans.end(); parent = vertexes[parent].suf_link) {
            vertexes[parent].trans[c] = index;
        }
        if (parent == -1) {
            vertexes[index].suf_link = 0;
        } else {
            int transition = vertexes[parent].trans[c];
            if (vertexes[parent].length + 1 == vertexes[transition].length) {
                vertexes[index].suf_link = transition;
            } else {
                int copy = size();
                vertexes.push_back(Vertex());
                vertexes.back().length = vertexes[parent].length + 1;
                vertexes.back().trans = vertexes[transition].trans;
                vertexes.back().suf_link = vertexes[transition].suf_link;
                while (parent != -1 && vertexes[parent].trans[c] == transition) {
                    vertexes[parent].trans[c] = copy;
                    parent = vertexes[parent].suf_link;
                }
                vertexes[transition].suf_link = vertexes[index].suf_link = copy;
            }
        }
        last = index;
    }

    size_t size() const { return vertexes.size(); }
    Vertex& operator[](size_t i) { return vertexes[i]; }
    int last = 0;

private:
    std::vector<Vertex> vertexes;
};

std::pair<size_t, std::string> st_lcs_dfs(SufTree& st, size_t v, size_t quantity, size_t depth = 0) {
    size_t accept = 0;
    size_t const full = (1 << quantity) - 1;
    std::string loc_max;
    char transition;
    for (auto i: st[v].trans) {
        if (i.first < quantity) {
            accept |= 1 << i.first;
        } else {
            size_t q;
            std::string s;
            if (st[i.second].counted == -1) {
                auto tmp = st_lcs_dfs(st, i.second, quantity, depth + 1);
                q = tmp.first;
                s = std::move(tmp.second);
            } else {
                q = st[i.second].counted;
                s = st[i.second].res;
            }
            if (q == full) {
                if (s.size() >= loc_max.size()) {
                    transition = i.first;
                    loc_max = std::move(s);
                    loc_max.push_back(i.first);
                }
            } else {
                accept |= q;
            }
        }
    }
    if (loc_max.size() > 0) {
        st[v].counted = full;
        st[v].res = loc_max;
        return {full, std::move(loc_max)};
    }
    st[v].counted = accept;
    return {accept, ""};
}

int main() {
    size_t quantity;
    std::cin >> quantity;
    SufTree st;
    char divider[quantity];
    for (int i = 0; i < quantity; ++i) {
        std::string str;
        std::cin >> str;
        for (auto c: str) {
            st.add(c);
        }
        divider[i] = i;
        st.add(i);
    }
    std::string str = st_lcs_dfs(st, 0, quantity).second;
    for (auto i = str.rbegin(); i != str.rend(); ++i) {
        std::cout << *i;
    }

    return 0;
}
