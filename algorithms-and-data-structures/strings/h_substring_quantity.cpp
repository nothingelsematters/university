#include <fstream>
#include <iostream>
#include <list>
#include <vector>
#include <map>
#include <algorithm>

struct Vertex {
    Vertex(size_t parent, char c) : parent(parent), parent_char(c) {}

    size_t operator[](char c) const {
        auto it = travel.find(c);
        if (it == travel.end()) {
            return -1;
        }
        return it->second;
    }

    size_t parent;
    char parent_char;
    size_t suf_link = -1;
    size_t com_link = -1;
    int visited = 0;
    std::map<char, int> travel;
    std::map<char, int> son;
};

struct Forest {
    Forest() {
        vertex.push_back(Vertex(-1, '\0'));
    }

    void add(std::string const& str) {
        size_t current = 0;
        for (auto c: str) {
            size_t to = vertex[current][c];
            if (to != -1) {
                current = to;
                continue;
            }

            vertex[current].travel[c] = vertex.size();
            vertex[current].son[c] = vertex.size();
            vertex.push_back(Vertex(current, c));
            current = vertex.size() - 1;
        }
        terminals.push_back(current);
    }

    int* quantity(std::string const& text) {
        size_t current = 0;
        for (auto c: text) {
            current = get_link(current, c);
            vertex[current].visited++;
        }

        for (auto i: order_bfs()) {
            vertex[get_suf_link(i)].visited += vertex[i].visited;
        }

        int* result = new int[terminals.size()];
        std::fill(result, result + terminals.size(), 0);
        for (int i = 0; i < terminals.size(); ++i) {
            result[i] = vertex[terminals[i]].visited;
        }

        return result;
    }

private:
    size_t get_suf_link(size_t index) {
        if (vertex[index].suf_link != -1) {
            return vertex[index].suf_link;
        }

        if (vertex[index].parent == -1 || vertex[index].parent == 0) {
            vertex[index].suf_link = 0;
        } else {
            vertex[index].suf_link = get_link(get_suf_link(vertex[index].parent),
                vertex[index].parent_char);
        }
        return vertex[index].suf_link;
    }

    size_t get_link(size_t index, char c) {
        size_t res = vertex[index][c];
        if (res != -1) {
            return res;
        }

        size_t trans = vertex[index][c];
        if (index == 0) {
            vertex[index].travel[c] = 0;
        } else {
            vertex[index].travel[c] = get_link(get_suf_link(index), c);
        }
        return vertex[index].travel[c];
    }

    std::list<size_t> order_bfs() {
        std::list<size_t> result = {0};
        for (auto i = result.begin(); i != result.end(); ++i) {
            for (auto j: vertex[*i].son) {
                result.push_back(j.second);
            }
        }
        std::reverse(result.begin(), result.end());
        return std::move(result);
    }

    std::vector<Vertex> vertex;
    std::vector<size_t> terminals;
};


int main() {
    std::ifstream fin("search5.in");
    Forest f;
    size_t size;
    fin >> size;

    for (size_t i = 0; i < size; ++i) {
        std::string str;
        fin >> str;
        f.add(std::move(str));
    }

    std::string text;
    fin >> text;
    fin.close();
    int* quantity = f.quantity(std::move(text));

    std::ofstream fout("search5.out");
    for (size_t i = 0; i < size; ++i) {
        fout << quantity[i] << '\n';
    }
    fout.close();

    delete[] quantity;
    return 0;
}
