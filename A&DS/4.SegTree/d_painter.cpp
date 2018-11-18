#include <fstream>

struct line {
        int l, r;
        bool b = false;
        line(){}
        line(char w, int q, int c){
                b = (w == 'B');
                l = q;
                r = q + c;
        }
};
struct picture {
        unsigned int left, right, count, length;
        bool push_black, is_push, lblack, rblack;
};
picture pic[10000000];

void build(unsigned int x, unsigned int l, unsigned int r){
          pic[x].left = l;
          pic[x].right = r;
          pic[x].lblack = false;
          pic[x].rblack = false;
          pic[x].is_push = false;
          pic[x].count = 0;
          pic[x].length = 0;
          if (l == r) return;
          build(2 * x + 1, l, (l + r) / 2);
          build(2 * x + 2, (l + r) / 2 + 1, r);
}

void push(unsigned int x){
        if (pic[x].is_push) {
                pic[x].length = pic[x].push_black * (pic[x].right - pic[x].left + 1);
                pic[x].lblack = pic[x].push_black;
                pic[x].rblack = pic[x].push_black;
                pic[x].count = pic[x].push_black;
                pic[2 * x + 1].is_push = true;
                pic[2 * x + 1] .push_black = pic[x].push_black;
                pic[2 * x + 2].is_push = true;
                pic[2 * x + 2] .push_black = pic[x].push_black;
                pic[x].is_push = false;
        }
}

void up(unsigned int x){
        while (x != 0) {
                x = (x - 1) / 2;
                pic[x].length = pic[2 * x + 1].length + pic[2 * x + 2].length;
                pic[x].count = pic[2 * x + 1].count + pic[2 * x + 2].count;
                if (pic[2 * x + 1].rblack && pic[2 * x + 2].lblack) --pic[x].count;
                pic[x].lblack = pic[2 * x + 1].lblack;
                pic[x].rblack = pic[2 * x + 2].rblack;
        }
}

void update(unsigned int x, unsigned int l, unsigned int r, bool black){
          push(x);
          push(2 * x + 1);
          push(2 * x + 2);
          if ((l > pic[x].right) || (r < pic[x].left)) return;
          if ((pic[x].left >= l) && ( (pic[x].right <= r))) {
                  pic[x].push_black = black;
                  pic[x].is_push = true;
                  push(x);
                  push(2 * x + 1);
                  push(2 * x + 2);
                  up(x);
                  return;
          }
          update(2 * x + 1, l, r, black);
          update(2 * x + 2, l, r, black);
}

int main() {
        std::ifstream fin("painter.in");
        std::ofstream fout("painter.out");
        unsigned int quantity;
        int mini, maxi;
        fin >> quantity;
        line requests[quantity];
        maxi = -50000000;
        mini = 50000000;
        for (unsigned int i = 0; i < quantity; ++i) {
                char q; int l, r;
                fin >> q >> l >> r;
                maxi = std::max(maxi, l + r);
                mini = std::min(mini, l);
                requests[i] = line(q, l, r);
        }
        unsigned int mod = maxi - mini + 1;
        build (0, 0, mod - 2);
        for (unsigned int i = 0; i < quantity; ++i) {
                update(0, requests[i].l - mini, requests[i].r - mini - 1, requests[i].b);
                fout << pic[0].count << ' ' << pic[0].length << "\n";
        }
}
