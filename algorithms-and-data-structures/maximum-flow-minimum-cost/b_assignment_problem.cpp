#include <fstream>
#include <memory>
#include <vector>
#include <limits>

using matrix = std::vector<std::vector<int>>;

std::pair<int, std::unique_ptr<size_t[]>> hungarian(const matrix& m) {
    const size_t size = m.size();
    const int INF = std::numeric_limits<int>::max();

    unsigned int line_potential[size] = {};
    unsigned int column_potential[size] = {};
    size_t min_location[size];
    auto result = std::make_unique<size_t[]>(size);

    std::fill(result.get(), result.get() + size, 0);

    for (size_t i = 1; i < size; ++i) {
        result[0] = i;
        size_t starting_column = 0;

        size_t min_vertex[size];
        bool visited[size];
        std::fill(min_vertex, min_vertex + size, std::numeric_limits<size_t>::max());
        std::fill(visited, visited + size, false);

        do {
            visited[starting_column] = true;
            size_t starting_line = result[starting_column];
            unsigned int diff = INF;
            size_t new_column;

            for (size_t j = 1; j < size; ++j) {
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

            for (size_t j = 0; j < size; ++j) {
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

    return {-column_potential[0], std::move(result)};
}

int main() {
    const std::string file_name = "assignment";
    std::ifstream fin(file_name + ".in");
    size_t size;
    fin >> size;
    matrix m(size + 1, std::vector<int>(size + 1));

    for (size_t i = 1; i <= size; ++i) {
        for (size_t j = 1; j <= size; ++j) {
            fin >> m[i][j];
        }
    }

    auto [cost, result] = hungarian(m);
    std::ofstream fout(file_name + ".out");
    fout << cost << '\n';
    for (size_t i = 1; i <= size; ++i) {
        fout << result[i] << ' ' << i << '\n';
    }
    return 0;
}
