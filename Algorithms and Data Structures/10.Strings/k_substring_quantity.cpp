#include <fstream>
#include <map>
#include <vector>

struct Vertex {
    int length;
    int suf_link;
    long long counted = -1;
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

long long st_sq_dfs(SufTree& st, size_t v = 0) {
    if (st[v].counted != -1) {
        return st[v].counted;
    }
    long long result = 1;
    for (auto i: st[v].trans) {
        result += st_sq_dfs(st, i.second);
    }
    st[v].counted = result;
    return result;
}

int main() {
    std::ifstream fin("count.in");
    SufTree st;
    std::string str;
    fin >> str;
    for (auto c: str) {
        st.add(c);
    }
    fin.close();

    std::ofstream fout("count.out");
    fout << st_sq_dfs(st) - 1;

    return 0;
}
