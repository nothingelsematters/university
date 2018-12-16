#include <fstream>
#include <iostream>
#include <list>
#include <vector>
#include <map>

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
    bool visited = false;
    std::map<char, int> travel;
    std::list<size_t> terminator;
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
            vertex.push_back(Vertex(current, c));
            current = vertex.size() - 1;
        }

        vertex[current].terminator.push_back(string_quantity++);
    }

    bool* check(std::string const& text) {
        bool* result = new bool[string_quantity];
        std::fill(result, result + string_quantity, false);

        size_t current = 0;
        vertex[current].visited = true;
        for (auto c: text) {

            current = get_link(current, c);
            for (size_t local = current; local != 0 && !vertex[local].visited; local = get_compressed(local)) {
                vertex[local].visited = true;
                for (auto j: vertex[local].terminator) {
                    result[j] = true;
                }
            }
            vertex[current].visited = true;
        }

        return result;
    }

private:
    size_t get_compressed(size_t index) {
        if (vertex[index].com_link != -1) {
            return vertex[index].com_link;
        }

        size_t suf = get_suf_link(index);
        if (get_suf_link(index) == 0) {
            vertex[index].com_link = 0;
        } else if (vertex[suf].terminator.size() > 0) {
            vertex[index].com_link = suf;
        } else {
            vertex[index].com_link = get_compressed(suf);
        }
        return vertex[index].com_link;
    }

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


    size_t string_quantity = 0;
    std::vector<Vertex> vertex;
};


int main() {
    std::ifstream fin("search4.in");
    Forest f;
    size_t quantity;
    fin >> quantity;

    for (size_t i = 0; i < quantity; ++i) {
        std::string str;
        fin >> str;
        f.add(str);
    }

    std::string text;
    fin >> text;
    fin.close();
    bool* check = f.check(text);

    std::ofstream fout("search4.out");
    for (size_t i = 0; i < quantity; ++i) {
        fout << (check[i] ? "YES" : "NO") << '\n';
    }
    fout.close();

    delete[] check;
    return 0;
}
