#include <vector>
#include <cmath>
#include <fstream>
using namespace std;

int true_array;
vector<int> lots;

long long loglog(long long const& a) {
        long long b;
        for (b = a; !(b & 1); b >>= 1) {}
        return log2(a) + (b != 1);
}

pair<bool, int> search(int l, int r, int x){
  int len = (1 << (int)((log2(lots.size() + 1)) - log2(x + 2)));
  int left = len * (x + 1) - 1 - true_array;
  int right = len * (x + 2) - 2 - true_array;
  if ((left >= l) && ( (right <= r)) && (lots[x] == 0)) return pair<bool, int> (true, left);
  if ((lots[x] == len) || (right < l) || (left > r) || ((x >= true_array) && (lots[x]))) return pair<bool, int> (false, 0);
  pair<bool, int> pa = search(l, r, 2 * x + 1);
  if (pa.first) return pa;
  return search(l, r, 2 * x + 2);
}

void set(int x, bool b){
  x += true_array;
  lots[x] = b;
  while (x != 0) {
          (x -= (x & 1 ? 1 : 2)) /= 2;
          lots[x] = lots[x * 2 + 1] + lots[x * 2 + 2];
  }
}

int main(){
        ifstream fin("parking.in");
        ofstream fout("parking.out");
        int size, requests_size;
        fin >> size >> requests_size;
        true_array = (1 << loglog(size)) - 1;
        lots.resize(true_array * 2 + 1, 0);
        for (int i = 0; i < requests_size; ++i) {
                string request;
                fin >> request;
                if (request == "enter") {
                  int x;
                  fin >> x;
                  pair<bool, int> s = search(x - 1, size - 1, 0);
                  if (!s.first) s = search(0, x - 2, 0);
                  set(s.second, true);
                  fout << s.second + 1 << "\n";
                } else if (request == "exit") {
                  int x;
                  fin >> x;
                  set(x - 1, false);
                }
        }
        fin.close();
        fout.close();
}
