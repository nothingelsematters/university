#include <fstream>
#include <vector>

std::ofstream fout;
std::vector<int> v({0});

void write(int num, int quantity) {
        if (num - 1 == quantity) {
                for (int i = 0; i < quantity; ++i)
                        fout << v[i + 1];
                fout << '\n';
                return;
        }
        v[num] = 0;
        write(num + 1, quantity);
        if (v[num - 1] == 0) {
                v[num] = 1;
                write(num + 1, quantity);
        }
}

int main () {
        std::ifstream fin("vectors2.in");
        fout.open("vectors2.out");
        int quantity, one = 1;
        fin >> quantity;
        for (int i = 0; i < quantity; one += v[i++])
                v.push_back(one);
        fout << v[quantity] + one << '\n';
        write(1, quantity);
        fin.close();
        fout.close();
        return 0;
}
