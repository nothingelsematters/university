#include <iostream>
#include <vector>
#include <limits>
#include <iterator>
#include <algorithm>

using graph = std::vector<std::vector<int>>;
static constexpr int INF = std::numeric_limits<int>::max();

template <typename T>
void make_min(T& current, T check) {
    current = std::min(current, check);
}

int hungarian(const graph& m, size_t length, size_t width) {
    unsigned int line_potential[length] = {};
    unsigned int column_potential[width] = {};
    size_t min_location[width];
    size_t result[width] = {};

    for (size_t i = 1; i < length; ++i) {
        result[0] = i;
        size_t starting_column = 0;

        bool visited[width] = {};
        size_t min_vertex[width];
        std::fill(min_vertex, min_vertex + width, std::numeric_limits<size_t>::max());

        do {
            visited[starting_column] = true;
            size_t starting_line = result[starting_column];
            unsigned int diff = INF;
            size_t new_column;

            for (size_t j = 1; j < width; ++j) {
                if (!visited[j]) {
                    unsigned int current = m[starting_line][j] - line_potential[starting_line] - column_potential[j];
                    if (current < min_vertex[j]) {
                        min_vertex[j] = current;
                        min_location[j] = starting_column;
                    }
                    if (min_vertex[j] < diff) {
                        diff = min_vertex[j];
                        new_column = j;
                    }
                }
            }

            for (size_t j = 0; j < width; ++j) {
                if (visited[j]) {
                    line_potential[result[j]] += diff;
                    column_potential[j] -= diff;
                } else {
                    min_vertex[j] -= diff;
                }
            }

            starting_column = new_column;
        } while (result[starting_column] != 0);

        do {
            size_t new_column = min_location[starting_column];
            result[starting_column] = result[new_column];
            starting_column = new_column;
        } while (starting_column);
    }

    return -column_potential[0];
}

class pseudotree {
public:
    pseudotree(size_t vertexes, size_t numbers, int fine)
        : vertexes(vertexes + 1), numbers(numbers), fine(fine), edges(vertexes + 1), cost(vertexes + 1, std::vector<int>(numbers + 1)) {}

    void add_edge(size_t from, size_t to) {
        edges[from].push_back(to);
        edges[to].push_back(from);
    }

    void add_cost(size_t i, size_t j, int new_cost) {
        cost[i][j] = new_cost;
    }

    int min_paint_cost() {
        bool visited[vertexes] = {};
        sorted.resize(vertexes);
        dp.assign(vertexes, graph(numbers + 1, std::vector<int>(numbers + 1)));
        tree_preprocess(visited);

        dpdfs();
        return get_min(1, 1);
    }

private:
    const size_t vertexes, numbers;
    const int fine;
    graph edges, cost, sorted;
    std::vector<graph> dp;

    void tree_preprocess(bool* visited, size_t v = 1) {
        visited[v] = true;
        for (auto i: edges[v]) {
            if (!visited[i]) {
                sorted[v].push_back(i);
                tree_preprocess(visited, i);
            }
        }

        if (sorted[v].empty()) {
            std::transform(cost[v].begin(), cost[v].end(), dp[v].begin(), [this](int in){ return std::vector<int>(numbers + 1, in); });
        }
    }

    int get_min(size_t index, size_t comp_index) const {
        return (*std::min_element(std::next(dp[index].begin()), dp[index].end(),
            [comp_index](const auto& first, const auto& second) { return first[comp_index] < second[comp_index]; }))[comp_index];
    }

    void dpdfs(size_t v = 1) {
        for (auto child: sorted[v]) {
            dpdfs(child);
        }

        for (size_t i = 1; i <= numbers; ++i) {
            for (size_t j = 1; j <= numbers; ++j) {
                if (!sorted[v].empty()) {
                    dp[v][j][i] = fine + cost[v][j];
                }

                for (auto child: sorted[v]) {
                    dp[v][j][i] += get_min(child, j);
                }

                if ((v == 1 && sorted[v].size() <= numbers) || (!sorted[v].empty() && sorted[v].size() < numbers)) {
                    graph matrix(sorted[v].size() + 1, std::vector<int>(numbers + 1));
                    for (size_t k = 0; k < sorted[v].size(); ++k) {
                        for (size_t l = 1; l <= numbers; ++l) {
                            matrix[k + 1][l] = (v != 1 && l == i) ? INF : dp[sorted[v][k]][l][j];
                        }
                    }
                    make_min(dp[v][j][i], hungarian(matrix, sorted[v].size() + 1, numbers + 1) + cost[v][j]);
                }
            }
        }
    }
};



int main() {
    size_t vertexes, numbers;
    int fine;
    std::cin >> vertexes >> numbers >> fine;
    pseudotree pt(vertexes, numbers, fine);

    for (size_t i = 1; i <= vertexes; ++i) {
        for (size_t j = 1; j <= numbers; ++j) {
            int cost;
            std::cin >> cost;
            pt.add_cost(i, j, cost);
        }
    }

    for (size_t i = 0; i < vertexes - 1; ++i) {
        size_t from, to;
        std::cin >> from >> to;
        pt.add_edge(from, to);
    }

    std::cout << pt.min_paint_cost() << '\n';
    return 0;
}
