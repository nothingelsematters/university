#include <vector>
#include <fstream>
#include <cmath>
using namespace std;

struct oak{
  long long l, r, s;
  oak(){
    s = 0;
  }
};
vector<oak> arr;
long long true_array;
long long loglog(long long const& a) {
        long long b;
        for (b = a; !(b & 1); b >>= 1) {}
        return log2(a) + (b != 1);
}
long long sum (long long const& l, long long const& r, long long x){
    if ((l > arr[x].r) || (r < arr[x].l)) return 0;
    if ((arr[x].l >= l) && ( (arr[x].r <= r))) return arr[x].s;
    return sum(l, r, 2 * x + 1) + sum(l, r, 2 * x + 2);
}
void set (long long a, long long x){
        long long iter = true_array + a;
        arr[iter].s = x;
        while (iter != 0) {
                (iter -= (iter & 1 ? 1 : 2)) /= 2;
                arr[iter].s = arr[iter * 2 + 1].s + arr[iter * 2 + 2].s;
        }
}

int main(){
        ifstream fin("rsq.in");
        ofstream fout("rsq.out");
        long long capacity;
        fin >> capacity;
        true_array = (1 << loglog(capacity)) - 1;
        arr.resize(true_array * 2 + 1);
        for (long long i = true_array; i < true_array + capacity; ++i){
                fin >> arr[i].s;
                arr[i].l = i - true_array;
                arr[i].r = arr[i].l;
        }
        for (long long i = true_array - 1; i >= 0; --i){
                arr[i].s = arr[2 * i + 1].s + arr[2 * i + 2].s;
                long long length = (1 << (long long)((log2(arr.size() + 1)) - log2(i + 2)));
                arr[i].l = length * (i + 1) - 1 - true_array;
                arr[i].r = length * (i + 2) - 2 - true_array;
        }
        string request;
        while (fin >> request) {
                if (request == "sum") {
                        long long l, r;
                        fin >> l >> r;
                        fout << sum(l - 1, r - 1, 0) << "\n";
                } else if (request == "set") {
                        long long a, x;
                        fin >> a >> x;
                        set(a - 1, x);
                }
        }
        fin.close();
        fout.close();
}
