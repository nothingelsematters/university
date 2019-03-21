#include <iostream>
#include <algorithm>
using namespace std;

struct dot {
        int x, y_up, y_down;
        bool start;
        bool operator < (dot b) {
          return ((x < b.x) || (x == b.x && start && !b.start));
        }
};
struct mega {
        int value, push;
};
dot field[2000000];
mega super[3000000];
int mini = 100000;
int maxi = -100000;
unsigned int d = 0;

void push(int x, int l, int r){
        super[x].value += super[x].push;
        if (l != r) {
                super[(x << 1) + 1].push += super[x].push;
                super[(x << 1) + 2].push += super[x].push;
        }
        super[x].push = 0;
}

void update(int x, int l, int r){
        if (super[x].push != 0) push(x, l, r);
        if ((field[d].y_down - mini > r) || (field[d].y_up - mini < l)) return;
        if (super[(x << 1) + 1].push != 0) push((x << 1) + 1, l, (l + r) >> 1);
        if (super[(x << 1) + 2].push != 0) push((x << 1) + 2, ((l + r) >> 1) + 1, r);
        if ((l >= field[d].y_down - mini) && ((r <= field[d].y_up - mini))) {
                super[x].value += (field[d].start ? 1 : -1);
                if (l != r) {
                        super[(x << 1) + 1].push += (field[d].start ? 1 : -1);
                        super[(x << 1) + 2].push += (field[d].start ? 1 : -1);
                }
                super[x].push = 0;
                while (x != 0) {
                        x = (x - 1) >> 1;
                        super[x].value = max(super[(x << 1) + 1].value, super[(x << 1) + 2].value);
                }
                return;
        }
        update((x << 1) + 1, l, (l + r) >> 1);
        update((x << 1) + 2, ((l + r) >> 1) + 1, r);
}

int answer (int x, int l, int r) {
        if (super[x].push != 0) push(x, l, r);
        if (l == r) return l;
        if (super[(x << 1) + 1].push != 0) push((x << 1) + 1, l, (l + r) >> 1);
        if (super[(x << 1) + 2].push != 0) push((x << 1) + 2, ((l + r) >> 1) + 1, r);
        if (super[(x << 1) + 1].value == super[x].value) return answer((x << 1) + 1, l, (l + r) >> 1);
        return answer((x << 1) + 2, ((l + r) >> 1) + 1, r);
}

int main() {
        ios_base::sync_with_stdio(false);
        unsigned int quantity;
        cin >> quantity;
        quantity = (quantity << 1) - 1;
        for (unsigned int i = 0; i < quantity; i += 2) {
                int y1, y2;
                cin >> field[i].x >> y1 >> field[i + 1].x >> y2;
                if (y1 < mini) mini = y1;
                if (y2 > maxi) maxi = y2;
                field[i].y_up = y2;
                field[i].y_down = y1;
                field[i].start = true;
                field[i + 1].start = false;
                field[i + 1].y_up = y2;
                field[i + 1].y_down = y1;
        }
        maxi -= mini;
        sort(field, field + quantity + 1);
        // for (int i = 0; i < quantity + 2; ++i) cout << field[i].x << ' ' << field[i].start << ' ' << field[i].y_down << ' ' << field[i].y_up << '\n';
        int maxmax = 0;
        pair<int,int> maxmaxi;
        while(d < quantity) {
                update(0, 0, maxi);
                if (super[0].value > maxmax) {
                        maxmax = super[0].value;
                        maxmaxi.first = field[d].x;
                        maxmaxi.second = answer(0, 0, maxi);
                }
                ++d;
        }
        cout << maxmax << '\n' << maxmaxi.first << ' ' << maxmaxi.second + mini;
}
