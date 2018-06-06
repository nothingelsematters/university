#include <fstream>
#include <cmath>

int main() {
        std::ios_base::sync_with_stdio(false);
        std::ifstream fin("shooter.in");
        std::ofstream fout("shooter.out");
        int a, b, c;
        double divide, sum;
        fin >> a >> b >> c;
        for (int i = 0; i < a; ++i) {
                double d;
                fin >> d;
                if (i + 1 == c) divide = pow(1-d, b);
                sum += pow(1-d, b);
        }
        sum = divide / sum;
        fout.precision(13);
        fout << sum;
}
