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
    bool visited = false;
    std::map<char, int> travel;
    std::list<std::pair<size_t, size_t>> terminator;
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

        vertex[current].terminator.emplace_back(string_quantity++, str.size());
    }

    int* check(std::string const& text) {
        int* result = new int[string_quantity];
        std::fill(result, result + string_quantity, -1);

        size_t current = 0;
        vertex[current].visited = true;
        int index = 0;
        for (auto c: text) {

            current = get_link(current, c);
            for (size_t local = current; local != 0 && !vertex[local].visited; local = get_compressed(local)) {
                vertex[local].visited = true;
                for (auto j: vertex[local].terminator) {
                    if (flag) result[j.first] = index - j.second + 1;
                    else result[j.first] = text.size() - index - 1;
                }
            }
            vertex[current].visited = true;
            ++index;
        }

        return result;
    }

bool flag = true;
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
    std::ifstream fin("search6.in");
    Forest f;
    Forest rev;
    rev.flag = false;
    size_t quantity;
    fin >> quantity;

    for (size_t i = 0; i < quantity; ++i) {
        std::string str;
        fin >> str;
        f.add(str);
        std::reverse(str.begin(), str.end());
        rev.add(str);
    }

    std::string text;
    fin >> text;
    fin.close();
    int* check = f.check(text);
    std::reverse(text.begin(), text.end());
    int* check2 = rev.check(text);

    std::ofstream fout("search6.out");
    for (size_t i = 0; i < quantity; ++i) {
        fout << check[i] << ' ' << check2[i]  << ' ' << '\n';
    }
    fout.close();

    delete[] check;
    delete[] check2;
    return 0;
}
