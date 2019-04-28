#include <fstream>
#include <iterator>
#include <vector>
#include <algorithm>

int main() {
    std::string file_name = "rps2";
    std::ifstream fin(file_name + ".in");
    std::ofstream fout(file_name + ".out");

    std::vector<int> args;
    std::copy(std::istream_iterator<int>(fin), std::istream_iterator<int>(), std::back_insert_iterator<std::vector<int>>(args));
    fout << std::max({0, args[1] - args[4] - args[3], args[2] - args[5] - args[4], args[0] - args[3] - args[5]});
    return 0;
}
