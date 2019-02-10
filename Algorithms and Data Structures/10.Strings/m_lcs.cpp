#include <fstream>
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

int main() {
    std::ifstream fin("common.in");
    SufTree st;
    std::string first, second;
    fin >> first;
    for (auto c: first) {
        st.add(c);
    }
    fin >> second;
    fin.close();

    int vertex = 0;
    int length = 0;
    int longest = 0;
    int position = 0;
    for (int i = 0; i < second.size(); ++i) {
    	while (vertex && st[vertex].trans.find(second[i]) == st[vertex].trans.end()) {
    		vertex = st[vertex].suf_link;
    		length = st[vertex].length;
    	}
    	if (st[vertex].trans.find(second[i]) != st[vertex].trans.end()) {
    		vertex = st[vertex].trans[second[i]];
    		++length;
    	}
    	if (length > longest) {
            longest = length;
            position = i;
        }
    }
    std::ofstream fout("common.out");
    fout << second.substr(position - longest + 1, longest);
    fout.close();

    return 0;
}
