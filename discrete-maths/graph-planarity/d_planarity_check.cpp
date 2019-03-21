#include <fstream>
#include <iostream>
#include <vector>
#include <list>
#include <set>
#include <map>
#include <algorithm>
#include <cmath>
#include <climits>


struct Graph {
    Graph(size_t size_) : size_(size_) {
        edges.resize(size_, std::vector<bool>(size_, false));
    }

    Graph(std::string triangle) : Graph(0.5 + std::sqrt(0.25 + 2 * triangle.size())) {
        size_t index = 0;
        for (size_t i = 0; i < size_; ++i) {
            for (size_t j = 0; j < i; ++j) {
                if (triangle[index++] == '1') {
                    edges[i][j] = true;
                    edges[j][i] = true;
                }
            }
        }
    }

    std::vector<bool>& operator[](size_t index) {
        return edges[index];
    }

    std::vector<bool> operator[](size_t index) const {
        return edges[index];
    }

    size_t size() const {
        return size_;
    }

    bool operator==(Graph const& second) const {
        if (size_ != second.size_) {
            return false;
        }

        return edges == second.edges;
    }

    bool operator!=(Graph const& second) const {
        return !(*this == second);
    }

private:
    std::vector<std::vector<bool>> edges;
    size_t size_;
};

struct GraphSegment {
    void add_edge(size_t from, size_t to) {
        if (std::find(edges[from].begin(), edges[from].end(), to) == edges[from].end()){
            edges[from].push_back(to);
            edges[to].push_back(from);
            vertexes.insert(from);
            vertexes.insert(to);
        }
    }

    std::set<size_t> vertexes;
    std::map<size_t, std::list<size_t>> edges;
};

struct Face {
    Face() {}

    Face(std::list<size_t> const& l) {
        for (auto i: l) {
            vertexes_set.insert(i);
            vertexes.push_back(i);
        }
    }

    Face operator=(Face const& from) {
        vertexes_set = from.vertexes_set;
        vertexes = from.vertexes;
        return *this;
    }

    Face(Face const& from) {
        vertexes_set = from.vertexes_set;
        vertexes = from.vertexes;
    }

    std::set<size_t> vertexes_set;
    std::vector<size_t> vertexes;
};

void dfs(Graph const& g, size_t v, bool* visited, std::vector<size_t>& component) {
    component.push_back(v);
    visited[v] = true;
    for (size_t i = 0; i < g.size(); ++i) {
        if (g[v][i] && !visited[i]) {
            dfs(g, i, visited, component);
        }
    }
}

std::pair<bool, std::list<std::vector<size_t>>> connected_components(Graph const& g) {
    bool* visited = new bool[g.size()];
    std::fill(visited, visited + g.size(), false);
    std::list<std::vector<size_t>> components;

    for (size_t i = 0; i < g.size(); ++i) {
        if (!visited[i]) {
            std::vector<size_t> new_component;
            dfs(g, i, visited, new_component);
            components.push_back(new_component);
        }
    }
    delete[] visited;

    return {components.size() == 1, components};
}

std::pair<size_t, std::list<size_t>> cycle_dfs(Graph const& g, size_t v, bool* visited, size_t from) {
    visited[v] = true;
    for (size_t i = 0; i < g.size(); ++i) {
        if (g[v][i]) {
            if (!visited[i]) {
                auto result = cycle_dfs(g, i, visited, v);
                if (result.second.size() > 0) {
                    if (std::find(result.second.begin(), result.second.end(), result.first) == result.second.end()) {
                        result.second.push_back(v);
                    }
                    return result;
                }
            } else if (from != i) {
                return {i, {v}};
            }
        }
    }
    visited[v] = false;
    return {0, std::list<size_t>()};
}

std::list<size_t> find_cycle(Graph const& g) {
    bool* visited = new bool[g.size()];
    std::fill(visited, visited + g.size(), false);
    auto result = cycle_dfs(g, 0, visited, 0);
    delete[] visited;
    return result.second;
}

void dfs_segment(Graph const& g, size_t v, bool* visited, GraphSegment& gs,
        Graph const& g_plane, std::set<size_t> const& g_plane_vertexes) {

    visited[v] = true;
    for (size_t i = 0; i < g.size(); ++i) {
        if (g[v][i]) {
            if (!g_plane[v][i]) {
                gs.add_edge(v, i);
            }
            if (g_plane_vertexes.find(i) == g_plane_vertexes.end() && !visited[i]) {
                dfs_segment(g, i, visited, gs, g_plane, g_plane_vertexes);
            }
        }
    }
}

std::vector<size_t> dfs_sequence(GraphSegment& gs, size_t v, bool* visited, Graph const& g,
        std::set<size_t> const& g_plane_vertexes, size_t from = -1, bool check = false) {

    visited[v] = true;
    for (auto i: gs.edges[v]) {
        if (check && visited[i]) {
            continue;
        }
        if (g_plane_vertexes.find(i) != g_plane_vertexes.end() && i != from) {
            return {i, v};
        }
        if (!visited[i]) {
            auto result = dfs_sequence(gs, i, visited, g, g_plane_vertexes, v, check);
            if (result.size() > 0) {
                result.push_back(v);
                return result;
            }
        }
    }
    return std::vector<size_t>();
}

void dfs_bridge(Graph& g, size_t v, int* enter, int* return_time, bool* visited,
        bool* current, size_t from = -1) {

    if (visited[v]) {
        return;
    }

    static int time = 0;
    ++time;
    visited[v] = true;
    current[v] = true;
    enter[v] = time;
    return_time[v] = time;

    for (size_t i = 0; i < g.size(); ++i) {
        if (!g[v][i]) {
            continue;
        }

        if (current[i] && i != from) {
            return_time[v] = std::min(return_time[v], enter[i]);
        }

        if (!visited[i]) {
            dfs_bridge(g, i, enter, return_time, visited, current, v);
            return_time[v] = std::min(return_time[v], return_time[i]);
            if (return_time[i] > enter[v]) {
                g[v][i] = false;
                g[i][v] = false;
            }
        }
    }
    current[v] = false;
    return;
}

Graph find_and_remove_bridges(Graph g) {
    int* enter = new int[g.size()];
    int* return_time = new int[g.size()];
    bool* visited = new bool[g.size()];
    bool* current = new bool[g.size()];

    for (int i = 0; i < g.size(); ++i) {
        enter[i] = 0;
        return_time[i] = 0;
        visited[i] = false;
        current[i] = false;
    }

    for (int i = 0; i < g.size(); ++i) {
        dfs_bridge(g, i, enter, return_time, visited, current);
    }
    delete[] enter;
    delete[] return_time;
    delete[] visited;
    delete[] current;

    return g;
}

bool planarity_check(Graph const& g, bool check = true) {
    if (check) {
        auto con_com = connected_components(g);
        if (!con_com.first) {
            for (auto subg: con_com.second) {
                Graph new_g(subg.size());
                for (size_t i = 0; i < subg.size(); ++i) {
                    for (size_t j = 0; j < i; ++j) {
                        if (g[subg[i]][subg[j]]) {
                            new_g[i][j] = true;
                            new_g[j][i] = true;
                        }
                    }
                }
                if (!planarity_check(new_g, false)) {
                    return false;
                }
            }

            return true;
        }
    }

    auto cycle = find_cycle(g);
    if (cycle.size() == 0) {
        return true;
    }

    Graph g_wob = find_and_remove_bridges(g);
    if (g_wob != g) {
        return planarity_check(g_wob);
    }

    Graph g_plane(g.size());
    std::set<size_t> g_plane_vertexes;
    for (auto i = cycle.begin(); i != --cycle.end(); ) {
        g_plane_vertexes.insert(*i);
        auto j = *(i++);
        g_plane[*i][j] = true;
        g_plane[j][*i] = true;
    }
    g_plane_vertexes.insert(*(--cycle.end()));
    g_plane[*cycle.begin()][*(--cycle.end())] = true;
    g_plane[*(--cycle.end())][*cycle.begin()] = true;

    std::vector<Face> faces({Face(cycle), Face(cycle)});
    std::vector<GraphSegment> segments;

    for (size_t i = 0; i < g.size(); ++i) {
        for (size_t j = 0; j < i; ++j) {
            if (g[i][j] && !g_plane[i][j] && g_plane_vertexes.find(i) != g_plane_vertexes.end() &&
                    g_plane_vertexes.find(j) != g_plane_vertexes.end()) {
                GraphSegment gs;
                gs.add_edge(i, j);
                segments.push_back(gs);
            }
        }
    }

    bool* visited = new bool[g.size()];
    std::fill(visited, visited + g.size(), false);
    for (size_t i = 0; i < g.size(); ++i) {
        if (g_plane_vertexes.find(i) == g_plane_vertexes.end() && !visited[i]) {
            GraphSegment gs;
            dfs_segment(g, i, visited, gs, g_plane, g_plane_vertexes);
            segments.push_back(gs);
        }
    }

    size_t out_face_index = 0;
    while (segments.size() > 0) {
        int min_power = INT_MAX;
        size_t index = -1;
        size_t face_index = -1;
        for (size_t i = 0; i < segments.size(); ++i) {
            int current_power = 0;
            size_t current_face_index = -1;
            for (size_t j = 0; j < faces.size(); ++j) {
                std::vector<size_t> intersection(g.size());
                auto it = std::set_intersection(segments[i].vertexes.begin(), segments[i].vertexes.end(),
                    g_plane_vertexes.begin(), g_plane_vertexes.end(), intersection.begin());
                intersection.resize(it - intersection.begin());

                if (std::includes(faces[j].vertexes_set.begin(), faces[j].vertexes_set.end(),
                        intersection.begin(), intersection.end())) {
                    ++current_power;
                    current_face_index = j;
                }
            }
            if (current_power == 0) {
                delete[] visited;
                return false;
            }
            if (current_power <= min_power) {
                min_power = current_power;
                index = i;
                face_index = current_face_index;
            }
        }

        GraphSegment current_gs = segments[index];
        std::fill(visited, visited + g.size(), false);
        std::vector<size_t> sequence;
        for (auto i: current_gs.vertexes) {
            if (g_plane_vertexes.find(i) != g_plane_vertexes.end()) {
                sequence = dfs_sequence(current_gs, i, visited, g, g_plane_vertexes, -1, true);
                break;
            }
        }

        std::fill(visited, visited + g.size(), false);
        if (sequence.size() == 0) {
            for (auto i: current_gs.vertexes) {
                if (g_plane_vertexes.find(i) != g_plane_vertexes.end()) {
                    sequence = dfs_sequence(current_gs, i, visited, g, g_plane_vertexes);
                    break;
                }
            }
        }
        for (int i = 0; i < sequence.size() - 1; ++i) {
            g_plane[sequence[i]][sequence[i + 1]] = true;
            g_plane[sequence[i + 1]][sequence[i]] = true;
        }


        for (auto i: current_gs.edges) {
            for (auto j: i.second) {
                size_t start_index = std::find(sequence.begin(), sequence.end(),
                        j) - sequence.begin();

                if (start_index < sequence.size()) {
                    if (!(start_index < sequence.size() - 1 && sequence[start_index + 1] == i.first) &&
                            !(start_index > 0 && sequence[start_index - 1] == i.first) &&
                            g_plane_vertexes.find(j) == g_plane_vertexes.end()) {
                        GraphSegment new_gs;
                        new_gs.add_edge(i.first, j);
                        segments.push_back(new_gs);
                    }
                }
            }
        }

        for (auto i: sequence) {
            g_plane_vertexes.insert(i);
        }

        std::fill(visited, visited + g.size(), false);
        for (auto i: current_gs.vertexes) {
            if (g_plane_vertexes.find(i) == g_plane_vertexes.end() && !visited[i]) {
                GraphSegment new_gs;
                dfs_segment(g, i, visited, new_gs, g_plane, g_plane_vertexes);
                segments.push_back(new_gs);
            }
        }

        bool not_reversed = true;
        size_t division_start_index = std::find(faces[face_index].vertexes.begin(),
            faces[face_index].vertexes.end(), sequence.front()) - faces[face_index].vertexes.begin();
        size_t division_end_index = std::find(faces[face_index].vertexes.begin(),
            faces[face_index].vertexes.end(), sequence.back()) - faces[face_index].vertexes.begin();

        if (division_start_index > division_end_index) {
            std::swap(division_end_index, division_start_index);
            not_reversed = false;
        }

        Face first_face(faces[face_index]);
        for (size_t i = division_start_index + 1; i < division_end_index; ++i) {
            first_face.vertexes_set.erase(first_face.vertexes[i]);
        }

        if (division_end_index != division_start_index) {
            first_face.vertexes.erase(first_face.vertexes.begin() + division_start_index + 1,
                first_face.vertexes.begin() + division_end_index);
        } else {
            first_face.vertexes.insert(first_face.vertexes.begin() + division_start_index + 1, sequence.back());
        }
        if (not_reversed) {
            first_face.vertexes.insert(first_face.vertexes.begin() + division_start_index + 1, sequence.begin() + 1,
                sequence.end() - 1);
        } else {
            first_face.vertexes.insert(first_face.vertexes.begin() + division_start_index + 1, sequence.rbegin() + 1,
                sequence.rend() - 1);
        }

        bool was = false;
        Face second_face(faces[face_index]);
        if (division_end_index == division_start_index && out_face_index == face_index) {
            Face third_face;
            for (auto i: sequence) {
                third_face.vertexes.push_back(i);
                third_face.vertexes_set.insert(i);
            }
            second_face = third_face;
            was = true;
            out_face_index = faces.size() - 1;

        } else {
            for (size_t i = 0; i < division_start_index; ++i) {
                second_face.vertexes_set.erase(second_face.vertexes[i]);
            }
            for (size_t i = division_end_index + 1; i < faces[face_index].vertexes.size(); ++i) {
                second_face.vertexes_set.erase(second_face.vertexes[i]);
            }
            second_face.vertexes.erase(second_face.vertexes.begin(),
                second_face.vertexes.begin() + division_start_index);
            second_face.vertexes.erase(second_face.vertexes.begin() + division_end_index + 1 - division_start_index,
                second_face.vertexes.end());

            if (not_reversed) {
                second_face.vertexes.insert(second_face.vertexes.end(), sequence.rbegin() + 1,
                    sequence.rend() - 1);
            } else {
                second_face.vertexes.insert(second_face.vertexes.end(), sequence.begin() + 1,
                sequence.end() - 1);
            }
        }

        for (auto i: sequence) {
            first_face.vertexes_set.insert(i);
            second_face.vertexes_set.insert(i);
        }

        faces.erase(faces.begin() + face_index);
        faces.push_back(first_face);
        faces.push_back(second_face);
        segments.erase(segments.begin() + index);
    }

    delete[] visited;

    return true;
}

int main() {
    std::ifstream fin("planaritycheck.in");
    std::ofstream fout("planaritycheck.out");
    int quantity;
    fin >> quantity;

    std::string triangle;
    std::getline(fin, triangle);
    for (int i = 0; i < quantity; ++i) {
        std::getline(fin, triangle);
        Graph g(triangle);
        fout << (planarity_check(g) ? "YES" : "NO") << '\n';
    }

    return 0;
}
