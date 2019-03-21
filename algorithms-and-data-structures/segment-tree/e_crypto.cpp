#include <fstream>
using namespace std;

int module;
struct matrix {
        int a, b, c, d;
        matrix(){
                a = 1;
                b = 0;
                c = 0;
                d = 1;
        }
};
struct part {
        matrix m;
        int l, r;
};

matrix multiply (matrix x, matrix y) {
          matrix res;
          res.a = (x.a * y.a + x.b * y.c) % module;
          res.b = (x.a * y.b + x.b * y.d) % module;
          res.c = (x.c * y.a + x.d * y.c) % module;
          res.d = (x.c * y.b + x.d * y.d) % module;
          return res;
}

matrix count(part Neo[], int l, int r, int x) {
        if ((l > Neo[x].r) || (r < Neo[x].l)) return matrix();
        if ((Neo[x].l >= l) && ( (Neo[x].r <= r))) return Neo[x].m;
        return multiply(count(Neo, l, r, 2 * x + 1), count(Neo, l, r, 2 * x + 2));
}

matrix build(part Neo[], matrix Mor[], int l, int r, int x){
          Neo[x].l = l;
          Neo[x].r = r;
          if (l == r) return (Neo[x].m = Mor[l]);
          return (Neo[x].m = multiply(build(Neo, Mor, l, (l + r) / 2, 2 * x + 1), build(Neo, Mor, (l + r) / 2 + 1, r, 2 * x + 2)));
}

int main() {
        ifstream fin("crypto.in");
        ofstream fout("crypto.out");
        int quantity, requests_quantity;
        fin >> module >> quantity >> requests_quantity;
        part Neo[quantity * 4];
        matrix Mor[quantity];
        for (int i = 0; i < quantity; ++i)
                fin >> Mor[i].a >> Mor[i].b >> Mor[i].c >> Mor[i].d;
        build(Neo, Mor, 0, quantity - 1, 0);
        for (int i = 0; i < requests_quantity; ++i) {
                int l, r;
                fin >> l >> r;
                matrix ans = count(Neo, --l, --r, 0);
                fout << ans.a << ' ' << ans.b << "\n" << ans.c << ' ' << ans.d << "\n";
        }
}
