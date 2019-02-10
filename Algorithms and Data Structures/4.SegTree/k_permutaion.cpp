#include <fstream>
#include <cstdlib>
#include <cmath>

struct tree {
      unsigned int l, r;
      uint64_t sum, hash_sum;
};
unsigned int num[1000000];
unsigned int index[1000000];
uint64_t prefix_sum[1000000];
uint64_t prefix_hash_sum[1000000];
tree oak[2000020];
unsigned long long s, h;
std::ifstream fin("permutation.in");
std::ofstream fout("permutation.out");

void change(unsigned int x, unsigned int new_x) {
      x = index[x - 1];
      oak[x].sum = new_x;
      oak[x].hash_sum = ((long long)(pow(new_x, 3))) % 3571;
      while (x != 0) {
            x = (x - 1) / 2;
            oak[x].sum = oak[2 * x + 1].sum + oak[2* x + 2].sum;
            oak[x].hash_sum = oak[2 * x + 1].hash_sum + oak[2* x + 2].hash_sum;
      }
}

void build(unsigned int x, unsigned int l, unsigned int r) {
      oak[x].l = l;
      oak[x].r = r;
      if (l != r) {
            build(2 * x + 1, l, (l + r) / 2);
            build(2 * x + 2, (l + r) / 2 + 1, r);
            oak[x].sum = oak[2 * x + 1].sum + oak[2* x + 2].sum;
            oak[x].hash_sum = oak[2 * x + 1].hash_sum + oak[2* x + 2].hash_sum;
      } else {
            oak[x].sum = num[l];
            oak[x].hash_sum =((long long) (pow(num[l], 3))) % 3571;
            index[l] = x;
      }
}

void summarise(unsigned int x, unsigned int l, unsigned int r){
      if ((l > oak[x].r) || (r < oak[x].l)) return;
      if ((oak[x].l >= l) && ( (oak[x].r <= r))) {
            s += oak[x].sum;
            h += oak[x].hash_sum;
            return;
      }
      summarise(2 * x + 1, l, r);
      summarise(2 * x + 2, l, r);

}

void check(unsigned int l, unsigned int r) {
      s = 0;
      h = 0;
      summarise(0, l, r);
      fout << (s == prefix_sum[r - l + 1] && h == prefix_hash_sum[r - l + 1] ? "YES" : "NO") << '\n';
}

int main() {
      unsigned int capacity, requests;
      fin >> capacity;
      prefix_sum[0] = 0;
      prefix_hash_sum[0] = 0;
      for (unsigned int i = 0; i < capacity; ++i) {
            fin >> num[i];
            prefix_sum[i + 1] = prefix_sum[i] + (i + 1);
            prefix_hash_sum[i + 1] = prefix_hash_sum[i] + (((long long)pow((i + 1), 3)) % 3571);
      }
      build(0, 0, capacity - 1);
      fin >> requests;
      for (unsigned int i = 0; i < requests; ++i) {
            unsigned int type, x, y;
            fin >> type >> x >> y;
            if (type == 1) change(x, y);
            else check(--x, --y);
      }
}
