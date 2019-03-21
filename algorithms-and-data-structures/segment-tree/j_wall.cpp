#include <iostream>
struct w {
        unsigned int l, r, v, push;
};
w arr[10000000];

void build (unsigned int x, unsigned int l, unsigned int r) {
        arr[x].v = 0;
        arr[x].r = r;
        arr[x].l = l;
        arr[x].push = 0;
        if (l == r) return;
        build(2 * x + 1, l, (l + r) / 2);
        build(2 * x + 2, (l + r) / 2 + 1, r);
}

void push(unsigned int x){
        if (arr[x].push != 0) {
                if (arr[x].push > arr[x].v) {
                        arr[x].v = arr[x].push;
                        arr[2 * x + 1].push = arr[x].push;
                        arr[2 * x + 2].push = arr[x].push;
                }
                arr[x].push = 0;
        }
}

void up(unsigned int x){
        while (x != 0) {
                x = (x - 1) / 2;
                arr[x].v = std::min(arr[x * 2 + 1].v, arr[x * 2 + 2].v);
        }
}

void addto(unsigned int x, unsigned int l, unsigned int r, unsigned int y){
        push(x);
        push(2 * x + 1);
        push(2 * x + 2);
        if ((l > arr[x].r) || (r < arr[x].l)) return;
        if ((arr[x].l >= l) && ( (arr[x].r <= r))) {
                arr[x].push = y;
                push(x);
                push(2 * x + 1);
                push(2 * x + 2);
                up(x);
                return;
        }
        addto(2 * x + 1, l, r, y);
        addto(2 * x + 2, l, r, y);
}

unsigned int minsearch(unsigned int x){
        push(x);
        push(2 * x + 1);
        push(2 * x + 2);
        if (arr[x].r == arr[x].l) return arr[x].l;
        if (arr[2 * x + 1].v != arr[x].v) return minsearch(2 * x + 2);
        return minsearch(2 * x + 1);
}

std::pair<unsigned int, unsigned int> mini(unsigned int x, unsigned int l, unsigned int r) {
        push(x);
        push(2 * x + 1);
        push(2 * x + 2);
        if ((l > arr[x].r) || (r < arr[x].l)) return std::pair<unsigned int, unsigned int> (-1, 100000000);
        if ((arr[x].l >= l) && (arr[x].r <= r)) {
                return std::pair<unsigned int, unsigned int> (x, arr[x].v);
        }
        std::pair<unsigned int, unsigned int> a = mini(2 * x + 1, l, r);
        std::pair<unsigned int, unsigned int> b = mini(2 * x + 2, l, r);
        return (a.second < b.second ? a : b);
}

int main() {
        unsigned int len, qn;
        std::ios_base::sync_with_stdio(false);
        std::cin >> len >> qn;
        build(0, 0, len - 1);
        for (unsigned int i = 0; i < qn; ++i) {
                std::string request;
                std::cin >> request;
                if (request == "defend") {
                        unsigned int a, b, c;
                        std::cin >> a >> b >> c;
                        addto(0, --a, --b, c);
                } else {
                        unsigned int a, b;
                        std::cin >> a >> b;
                        std::pair<unsigned int, unsigned int> ans = mini(0, --a, --b);
                        std::cout << ans.second << ' ' << minsearch(ans.first) + 1 << "\n";
                }
        }
}
