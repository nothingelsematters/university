#include <iostream>
#include <algorithm>
#include <vector>
#include <set>

std::vector<int> v;
std::set<std::pair<int, int>> e;
int n;

bool less(int a, int b) {
    if (e.find({a, b}) != e.end()) return true;
    if (e.find({b, a}) != e.end()) return false;
    std::cout << "1 " << a + 1 << ' ' << b + 1 << '\n';
    std::cout.flush();
    std::string s;
    std::cin >> s;
    e.insert(s == "YES" ? std::make_pair(a, b) : std::make_pair(b, a));
    return s == "YES";
}

void mergesort(int l, int r) {
    if (l == r) return;
    if (r - l == 1) {
        if (less(v[r], v[l])) {
            std::swap(v[r], v[l]);
        }
        return;
    }
    int m = (l + r)  / 2;
    mergesort(m + 1, r);
    mergesort(l, m);
    int buf[n];
    int xl = l;
    int xr = m + 1;
    int cur = 0;
    while (r - l + 1 != cur) {
        if (m < xl) buf[cur++] = v[xr++];
        else if (r < xr) buf[cur++] = v[xl++];
        else if (less(v[xr], v[xl])) buf[cur++] = v[xr++];
        else buf[cur++] = v[xl++];
    }
    for (int i = 0; i < cur; i++)
        v[i + l] = buf[i];
}

int main() {
    std::cin >> n;
    for (int i = 0; i < n; ++i) {
        v.push_back(i);
    }
    mergesort(0, n - 1);
    bool cool = true;
    // for (int i = 0; i < n - 1; ++i) {
    //     if (!less(v[i], v[i + 1])) {
    //         cool = false;
    //         break;
    //     }
    // }
    std::cout << "0 ";
    if (cool) {
        for (int i = 0; i < n; ++i) {
            std::cout << v[i] + 1 << ' ';
        }
    } else {
        for (int i = 0; i < n; ++i) {
            std::cout << "0 ";
        }
    }
    return 0;
}
