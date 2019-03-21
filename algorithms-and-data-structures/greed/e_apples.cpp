#include<fstream>
#include <algorithm>

struct apple {
    int down, up, index;
};

int main() {
    std::ifstream fin("apples.in");
    std::ofstream fout("apples.out");
    int q, height;
    fin >> q >> height;
    apple a[q];
    for (int i = 0; i < q; ++i) {
        a[i].index = i + 1;
        fin >> a[i].down >> a[i].up;
    }
    fin.close();
    std::sort(a, a + q, [](apple one, apple two) {
        int d1 = one.up - one.down;
        int d2 = two.up - two.down;
        if (d1 < 0) {
            if (d2 < 0)
                return one.up > two.up;
            return false;
        }
        if (d2 < 0)
            return true;
        return one.down < two.down;
    });
    bool fuck = false;
    for (int i = 0; i < q; ++i) {
        if (height - a[i].down <= 0) {
            fuck = true;
            break;
        }
        height += a[i].up - a[i].down;
    }
    if (fuck)
        fout << -1;
    else {
        for (int i = 0; i < q; ++i)
            fout << a[i].index << ' ';
    }
    fout.close();
    return 0;
}
